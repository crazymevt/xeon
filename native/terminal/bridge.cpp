// OS-specific terminal transport and libvterm ABI adapter. No GUI dependencies.
// All exports except the IO thread are called serially by the owning Lisp thread.
#include <vterm.h>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstring>
#include <deque>
#include <mutex>
#include <string>
#include <thread>
#include <vector>
#include <cstdint>
#include <cstdlib>
#ifdef _WIN32
#include <windows.h>
#define API extern "C" __declspec(dllexport)
#else
#include <cerrno>
#include <csignal>
#include <fcntl.h>
#include <poll.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#endif
#ifdef __APPLE__
#include <util.h>
#include <crt_externs.h>
#else
#include <pty.h>
extern char **environ;
#endif
#define API extern "C" __attribute__((visibility("default")))
#endif

namespace {
constexpr size_t OUTPUT_LIMIT=1024*1024, INPUT_LIMIT=256*1024, HISTORY_LIMIT=1000;
struct Style { uint32_t fg,bg,flags; };
struct Terminal {
  VTerm *vt=nullptr;
  VTermScreen *screen=nullptr;
  int rows=24,cols=80;
  bool alternate=false, cursor_visible=true;
  bool mouse=false;
  std::deque<std::vector<VTermScreenCell>> history;
  std::string text, styles, title;
  std::vector<Style> palette;
  int cursor=0;
  std::mutex mutex;
  std::deque<char> input,output;
  std::thread worker;
#ifdef _WIN32
  std::thread writer;
#endif
  std::atomic<bool> stopping{false},exited{false};
  std::atomic<int> exit_code{-1};
  bool started=false, overflow=false;
#ifdef _WIN32
  HPCON console=nullptr;
  HANDLE read_pipe=nullptr,write_pipe=nullptr,process=nullptr,job=nullptr;
#else
  int master=-1;
  pid_t pid=-1;
#endif
};
thread_local std::string last_error;
void fail(const std::string& s) { last_error=s; }
void enqueue(const char *bytes,size_t n,void *data) {
  auto &t=*static_cast<Terminal*>(data);
  std::lock_guard<std::mutex> lock(t.mutex);
  if(n>INPUT_LIMIT-t.input.size()) { t.overflow=true; return; }
  t.input.insert(t.input.end(),bytes,bytes+n);
}
int property(VTermProp prop,VTermValue *value,void *data) {
  auto &t=*static_cast<Terminal*>(data);
  if(prop==VTERM_PROP_ALTSCREEN) t.alternate=value->boolean;
  if(prop==VTERM_PROP_CURSORVISIBLE) t.cursor_visible=value->boolean;
  if(prop==VTERM_PROP_MOUSE) t.mouse=value->number!=0;
  // OSC title fragments are intentionally not used as UI markup or commands.
  return 1;
}
int pushline(int cols,const VTermScreenCell *cells,void *data) {
  auto &t=*static_cast<Terminal*>(data);
  if(t.history.size()==HISTORY_LIMIT) t.history.pop_front();
  t.history.emplace_back(cells,cells+cols);
  return 1;
}
int popline(int cols,VTermScreenCell *cells,void *data) {
  auto &t=*static_cast<Terminal*>(data);
  if(t.history.empty()) return 0;
  // Blank padding retains valid default colours when a resized line is shorter.
  VTermScreenCell blank{}; blank.width=1;
  vterm_state_get_default_colors(vterm_obtain_state(t.vt),&blank.fg,&blank.bg);
  std::fill(cells,cells+cols,blank);
  auto &line=t.history.back();
  std::copy_n(line.begin(),std::min(size_t(cols),line.size()),cells);
  t.history.pop_back(); return 1;
}
int clearhistory(void *data) { static_cast<Terminal*>(data)->history.clear(); return 1; }
const VTermScreenCallbacks callbacks={nullptr,nullptr,nullptr,property,nullptr,nullptr,pushline,popline,clearhistory};
uint32_t rgb(Terminal &t,VTermColor c) {
  vterm_screen_convert_color_to_rgb(t.screen,&c);
  return (uint32_t(c.rgb.red)<<16)|(uint32_t(c.rgb.green)<<8)|c.rgb.blue;
}
unsigned style(Terminal &t,const VTermScreenCell &cell) {
  Style s{rgb(t,cell.fg),rgb(t,cell.bg),
    unsigned(cell.attrs.bold)|(unsigned(cell.attrs.italic)<<1)|(unsigned(cell.attrs.underline!=0)<<2)};
  if(cell.attrs.reverse) std::swap(s.fg,s.bg);
  if(cell.attrs.conceal) s.fg=s.bg;
  for(unsigned i=0;i<t.palette.size();++i)
    if(t.palette[i].fg==s.fg && t.palette[i].bg==s.bg && t.palette[i].flags==s.flags) return i;
  // Scintilla reserves styles 32..39; use 0..31 and 40..255 (248 styles).
  if(t.palette.size()<248) { t.palette.push_back(s); return unsigned(t.palette.size()-1); }
  return 0; // Bounded colour diversity; never overwrite another visible style.
}
unsigned style_id(unsigned n) { return n<32?n:n+8; }
void append_utf8(Terminal &t,uint32_t cp,unsigned st) {
  if(cp>0x10ffff || (cp>=0xd800&&cp<=0xdfff)) cp=0xfffd;
  size_t before=t.text.size();
  if(cp<128) t.text+=char(cp);
  else if(cp<2048) { t.text+=char(0xc0|(cp>>6)); t.text+=char(0x80|(cp&63)); }
  else if(cp<65536) { t.text+=char(0xe0|(cp>>12));t.text+=char(0x80|((cp>>6)&63));t.text+=char(0x80|(cp&63)); }
  else { t.text+=char(0xf0|(cp>>18));t.text+=char(0x80|((cp>>12)&63));t.text+=char(0x80|((cp>>6)&63));t.text+=char(0x80|(cp&63)); }
  t.styles.append(t.text.size()-before,char(style_id(st)));
}
void append_cell(Terminal &t,const VTermScreenCell &cell) {
  if(cell.chars[0]==uint32_t(-1)) return; // continuation of a wide glyph
  unsigned st=style(t,cell);
  if(!cell.chars[0]) append_utf8(t,' ',st);
  else for(uint32_t cp:cell.chars) { if(!cp) break; append_utf8(t,cp,st); }
}

#ifdef _WIN32
std::wstring wide(const char *s) {
  int n=MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,s,-1,nullptr,0);
  if(!n) return {};
  std::wstring w(size_t(n),0); MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,s,-1,w.data(),n);
  w.pop_back(); return w;
}
bool spawn(Terminal &t,const char *cwd,const char *shell) {
  HANDLE in=nullptr,out=nullptr;
  if(!CreatePipe(&in,&t.write_pipe,nullptr,0) || !CreatePipe(&t.read_pipe,&out,nullptr,0)) {
    if(in) CloseHandle(in); if(out) CloseHandle(out); fail("Cannot create ConPTY pipes."); return false;
  }
  HRESULT hr=CreatePseudoConsole(COORD{SHORT(t.cols),SHORT(t.rows)},in,out,0,&t.console);
  CloseHandle(in);CloseHandle(out);
  if(FAILED(hr)) { fail("CreatePseudoConsole failed; Windows 10 1809 or newer is required."); return false; }
  STARTUPINFOEXW si{};si.StartupInfo.cb=sizeof(si);
  // Null standard handles let ConPTY attach its own console streams even
  // when Xeon runs with redirected input or output.
  si.StartupInfo.dwFlags=STARTF_USESTDHANDLES;
  SIZE_T size=0; InitializeProcThreadAttributeList(nullptr,1,0,&size);
  std::vector<unsigned char> attributes(size);
  si.lpAttributeList=reinterpret_cast<LPPROC_THREAD_ATTRIBUTE_LIST>(attributes.data());
  if(!InitializeProcThreadAttributeList(si.lpAttributeList,1,0,&size)) { fail("Cannot allocate ConPTY process attributes.");return false; }
  bool ok=UpdateProcThreadAttribute(si.lpAttributeList,0,PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,t.console,sizeof(t.console),nullptr,nullptr)!=0;
  std::wstring command=wide(shell && *shell?shell:"powershell.exe");
  // A configured shell is an executable path, never a shell command string.
  command=L"\""+command+L"\"";
  auto directory=wide(cwd);
  PROCESS_INFORMATION pi{};
  t.job=CreateJobObjectW(nullptr,nullptr);
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits{};
  limits.BasicLimitInformation.LimitFlags=JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
  ok=ok && t.job && SetInformationJobObject(t.job,JobObjectExtendedLimitInformation,&limits,sizeof(limits));
  ok=ok && CreateProcessW(nullptr,command.data(),nullptr,nullptr,FALSE,
    EXTENDED_STARTUPINFO_PRESENT|CREATE_UNICODE_ENVIRONMENT|CREATE_SUSPENDED,
    nullptr,directory.c_str(),&si.StartupInfo,&pi);
  DeleteProcThreadAttributeList(si.lpAttributeList);
  if(!ok) { fail("Cannot launch terminal shell (Windows error "+std::to_string(GetLastError())+").");return false; }
  if(!AssignProcessToJobObject(t.job,pi.hProcess)) {
    TerminateProcess(pi.hProcess,1);CloseHandle(pi.hProcess);CloseHandle(pi.hThread);
    fail("Cannot assign terminal shell to its lifetime job.");return false;
  }
  t.process=pi.hProcess; ResumeThread(pi.hThread);CloseHandle(pi.hThread);return true;
}
#else
bool spawn(Terminal &t,const char *cwd,const char *shell) {
  std::string executable=shell?shell:"";
  if(executable.empty()) {
    const char *env=std::getenv("SHELL");
    if(env && *env) executable=env;
    else { struct passwd *pw=getpwuid(getuid()); executable=pw&&pw->pw_shell&&*pw->pw_shell?pw->pw_shell:"/bin/sh"; }
  }
  if(executable[0]!='/' || access(executable.c_str(),X_OK)!=0) { fail("Terminal shell must be an executable absolute path.");return false; }
  std::vector<std::string> environment;
#ifdef __APPLE__
  char **env=*_NSGetEnviron();
#else
  char **env=environ;
#endif
  for(;*env;++env) if(std::strncmp(*env,"TERM=",5)&&std::strncmp(*env,"COLORTERM=",10)) environment.emplace_back(*env);
  environment.emplace_back("TERM=xterm-256color");environment.emplace_back("COLORTERM=truecolor");
  std::vector<char*> envp; for(auto &s:environment) envp.push_back(s.data());envp.push_back(nullptr);
  char *argv[]={executable.data(),const_cast<char*>("-i"),nullptr};
  long fd_limit=sysconf(_SC_OPEN_MAX);
  struct winsize size{};size.ws_row=t.rows;size.ws_col=t.cols;
  // Everything used in the child is prepared before fork. No Lisp, malloc,
  // stdio, environment mutation or C++ destructors run between fork and exec.
  t.pid=forkpty(&t.master,nullptr,nullptr,&size);
  if(t.pid==0) {
#if defined(SYS_close_range)
    if(syscall(SYS_close_range,3u,~0u,0)!=0)
      for(int fd=3;fd<fd_limit;++fd) close(fd);
#else
    for(int fd=3;fd<fd_limit;++fd) close(fd);
#endif
    if(chdir(cwd)!=0) _exit(126); execve(argv[0],argv,envp.data());_exit(127);
  }
  if(t.pid<0) {fail("Cannot allocate PTY: "+std::string(std::strerror(errno)));return false;}
  fcntl(t.master,F_SETFL,fcntl(t.master,F_GETFL)|O_NONBLOCK);
  fcntl(t.master,F_SETFD,FD_CLOEXEC);return true;
}
#endif

void io_loop(Terminal *p) {
  Terminal &t=*p;
  char buffer[16384];
  bool eof=false;
  while(!t.stopping) {
    size_t room; std::string outgoing;
    { std::lock_guard<std::mutex> lock(t.mutex);
      room=std::min(sizeof(buffer),OUTPUT_LIMIT-t.output.size());
#ifndef _WIN32
      size_t n=std::min(size_t(4096),t.input.size());
      outgoing.assign(t.input.begin(),t.input.begin()+n);
#endif
    }
    int got=0,written=0;
#ifdef _WIN32
    DWORD available=0,n=0;
    if(room&&!eof) {
      if(PeekNamedPipe(t.read_pipe,nullptr,0,nullptr,&available,nullptr)) {
        if(available && ReadFile(t.read_pipe,buffer,DWORD(std::min(room,size_t(available))),&n,nullptr)) got=int(n);
      } else eof=true;
    }
    // A separate writer keeps output draining even when ConPTY input blocks.
    DWORD code=STILL_ACTIVE;
    if(WaitForSingleObject(t.process,0)==WAIT_OBJECT_0 && GetExitCodeProcess(t.process,&code)) { t.exit_code=int(code); t.exited=true; }
#else
    struct pollfd fd{t.master,short(POLLIN|(!outgoing.empty()?POLLOUT:0)),0};
    poll(&fd,1,5);
    if(room && (fd.revents&(POLLIN|POLLHUP))) {
      ssize_t n=read(t.master,buffer,room);
      if(n>0) got=int(n);else if(n==0||(n<0&&errno==EIO)) eof=true;
    }
    if(!outgoing.empty()&&(fd.revents&POLLOUT)) { ssize_t n=write(t.master,outgoing.data(),outgoing.size());if(n>0)written=int(n); }
    if(!t.exited) { int status=0;if(waitpid(t.pid,&status,WNOHANG)==t.pid) {
      t.exit_code=WIFEXITED(status)?WEXITSTATUS(status):128+WTERMSIG(status);t.exited=true;
    }}
#endif
    { std::lock_guard<std::mutex> lock(t.mutex);
      if(got) t.output.insert(t.output.end(),buffer,buffer+got);
      if(written) t.input.erase(t.input.begin(),t.input.begin()+written);
    }
    if(eof&&t.exited) break;
    std::this_thread::sleep_for(std::chrono::milliseconds(3));
  }
}
void shutdown(Terminal &t) {
  t.stopping=true;
#ifdef _WIN32
  if(t.job) TerminateJobObject(t.job,1);
  if(t.writer.joinable()) { CancelSynchronousIo(t.writer.native_handle());t.writer.join(); }
  if(t.worker.joinable()) { CancelSynchronousIo(t.worker.native_handle()); t.worker.join(); }
  // Close the output pipe before closing the pseudoconsole, so its final flush
  // cannot wait forever for a reader which has already stopped.
  if(t.read_pipe) {CloseHandle(t.read_pipe);t.read_pipe=nullptr;}
  if(t.console) {ClosePseudoConsole(t.console);t.console=nullptr;}
  if(t.process) CloseHandle(t.process);
  if(t.job) CloseHandle(t.job);
  if(t.write_pipe) CloseHandle(t.write_pipe);
#else
  if(t.pid>0) {
    pid_t foreground=t.master>=0?tcgetpgrp(t.master):-1;
    if(foreground>0 && foreground!=getpgrp()) kill(-foreground,SIGHUP);
    if(!t.exited) kill(-t.pid,SIGHUP);
  }
  if(t.worker.joinable()) t.worker.join();
  if(t.master>=0) close(t.master);
  if(t.pid>0 && !t.exited) {
    int status;
    for(int i=0;i<20;++i) { if(waitpid(t.pid,&status,WNOHANG)==t.pid) {t.exited=true;break;}std::this_thread::sleep_for(std::chrono::milliseconds(5)); }
    if(!t.exited) { kill(-t.pid,SIGKILL);kill(t.pid,SIGKILL);waitpid(t.pid,&status,0); }
  }
#endif
}
} // namespace

API int kte_abi() { return 2; }
// Query the transport, not terminal output/prompt text. Unknown means busy so
// a failed OS query never silently discards a potentially running command.
API int kte_command_running(void *p) {
  auto &t=*static_cast<Terminal*>(p);
  if(!t.started || t.exited) return 0;
#ifdef _WIN32
  if(WaitForSingleObject(t.process,0)==WAIT_OBJECT_0) return 0;
  JOBOBJECT_BASIC_ACCOUNTING_INFORMATION info{};
  if(!QueryInformationJobObject(t.job,JobObjectBasicAccountingInformation,
                               &info,sizeof(info),nullptr)) return 1;
  return info.ActiveProcesses>1;
#else
  pid_t foreground=tcgetpgrp(t.master);
  if(foreground<0) return t.exited?0:1;
  return foreground!=t.pid;
#endif
}
API const char *kte_error() { return last_error.c_str(); }
API void *kte_create(int rows,int cols) {
  if(rows<2||rows>100||cols<10||cols>300) {fail("Terminal size outside 2..100 rows / 10..300 columns.");return nullptr;}
  auto *t=new Terminal;t->rows=rows;t->cols=cols;
  t->vt=vterm_new(rows,cols);
  if(!t->vt){delete t;fail("Cannot allocate terminal screen.");return nullptr;}
  vterm_set_utf8(t->vt,1);t->screen=vterm_obtain_screen(t->vt);
  vterm_screen_enable_altscreen(t->screen,1);
  vterm_screen_set_callbacks(t->screen,&callbacks,t);
  vterm_output_set_callback(t->vt,enqueue,t);
  vterm_screen_reset(t->screen,1);return t;
}
API int kte_start(void *p,const char *cwd,const char *shell) {
  auto &t=*static_cast<Terminal*>(p);
  if(t.started) {fail("Terminal already started.");return 0;}
  if(!spawn(t,cwd,shell)) return 0;
  t.started=true;t.worker=std::thread(io_loop,&t);
#ifdef _WIN32
  t.writer=std::thread([&t]() {
    while(!t.stopping && !t.exited) {
      std::string bytes;
      { std::lock_guard<std::mutex> lock(t.mutex);
        size_t n=std::min(size_t(4096),t.input.size());bytes.assign(t.input.begin(),t.input.begin()+n);
      }
      if(!bytes.empty()) {
        DWORD n=0;
        if(!WriteFile(t.write_pipe,bytes.data(),DWORD(bytes.size()),&n,nullptr)) break;
        std::lock_guard<std::mutex> lock(t.mutex);t.input.erase(t.input.begin(),t.input.begin()+n);
      } else std::this_thread::sleep_for(std::chrono::milliseconds(3));
    }
  });
#endif
  return 1;
}
API void kte_destroy(void *p) { if(!p)return;auto *t=static_cast<Terminal*>(p);shutdown(*t);vterm_free(t->vt);delete t; }
API int kte_poll(void *p) {
  auto &t=*static_cast<Terminal*>(p);std::string bytes;
  { std::lock_guard<std::mutex> lock(t.mutex);
    size_t n=std::min(size_t(65536),t.output.size());bytes.assign(t.output.begin(),t.output.begin()+n);t.output.erase(t.output.begin(),t.output.begin()+n);
  }
  if(!bytes.empty()) {vterm_input_write(t.vt,bytes.data(),bytes.size());vterm_screen_flush_damage(t.screen);}
  return int(bytes.size());
}
API void kte_feed(void *p,const char *bytes,int n) {auto &t=*static_cast<Terminal*>(p);if(n>0){vterm_input_write(t.vt,bytes,size_t(n));vterm_screen_flush_damage(t.screen);}}
API int kte_status(void *p) {auto &t=*static_cast<Terminal*>(p);return t.exited?t.exit_code.load():-1;}
API int kte_resize(void *p,int rows,int cols) {
  auto &t=*static_cast<Terminal*>(p);if(rows<2||rows>100||cols<10||cols>300)return 0;
  if(t.started && !t.exited) {
#ifdef _WIN32
    if(FAILED(ResizePseudoConsole(t.console,COORD{SHORT(cols),SHORT(rows)})))return 0;
#else
    struct winsize size{};size.ws_row=rows;size.ws_col=cols;
    if(ioctl(t.master,TIOCSWINSZ,&size)!=0)return 0;
#endif
  }
  t.rows=rows;t.cols=cols;vterm_set_size(t.vt,rows,cols);vterm_screen_flush_damage(t.screen);return 1;
}
API int kte_key(void *p,int key,int unicode,int mods) {
  auto &t=*static_cast<Terminal*>(p);if(t.exited)return 0;
  t.overflow=false;
  if(key) vterm_keyboard_key(t.vt,VTermKey(key),VTermModifier(mods&7));
  else if(unicode>0) vterm_keyboard_unichar(t.vt,uint32_t(unicode),VTermModifier(mods&7));
  return !t.overflow;
}
API int kte_paste(void *p,const char *bytes,int n) {
  auto &t=*static_cast<Terminal*>(p);if(n<0||n>65536||t.exited)return 0;
  // Reserve framing space, and reject atomically rather than truncate a command.
  {std::lock_guard<std::mutex> lock(t.mutex);if(size_t(n)+32>INPUT_LIMIT-t.input.size())return 0;}
  vterm_keyboard_start_paste(t.vt);enqueue(bytes,size_t(n),&t);vterm_keyboard_end_paste(t.vt);return 1;
}
API void kte_colours(void *p,unsigned fg,unsigned bg) {
  auto &t=*static_cast<Terminal*>(p);VTermColor f,b;
  vterm_color_rgb(&f,(fg>>16)&255,(fg>>8)&255,fg&255);vterm_color_rgb(&b,(bg>>16)&255,(bg>>8)&255,bg&255);
  vterm_state_set_default_colors(vterm_obtain_state(t.vt),&f,&b);
}
API const char *kte_snapshot(void *p) {
  auto &t=*static_cast<Terminal*>(p);t.text.clear();t.styles.clear();t.palette.clear();
  VTermPos cursor;vterm_state_get_cursorpos(vterm_obtain_state(t.vt),&cursor);
  if(!t.alternate) for(const auto &line:t.history) {
    for(const auto &cell:line) append_cell(t,cell);
    append_utf8(t,'\n',0);
  }
  for(int row=0;row<t.rows;++row) {
    for(int col=0;col<t.cols;++col) {
      if(row==cursor.row&&col==cursor.col)t.cursor=int(t.text.size());
      VTermScreenCell cell{};vterm_screen_get_cell(t.screen,VTermPos{row,col},&cell);append_cell(t,cell);
    }
    if(row+1<t.rows)append_utf8(t,'\n',0);
  }
  return t.text.c_str();
}
API int kte_length(void *p) {return int(static_cast<Terminal*>(p)->text.size());}
API const char *kte_styles(void *p) {return static_cast<Terminal*>(p)->styles.data();}
API int kte_cursor(void *p) {return static_cast<Terminal*>(p)->cursor;}
API int kte_cursor_visible(void *p) {return static_cast<Terminal*>(p)->cursor_visible;}
API int kte_mouse_mode(void *p) {return static_cast<Terminal*>(p)->mouse;}
API int kte_history_lines(void *p) {auto &t=*static_cast<Terminal*>(p);return t.alternate?0:int(t.history.size());}
API void kte_mouse(void *p,int row,int col,int button,int pressed,int mods) {
  auto &t=*static_cast<Terminal*>(p);
  if(t.exited || !t.mouse || row<0 || row>=t.rows || col<0 || col>=t.cols)return;
  vterm_mouse_move(t.vt,row,col,VTermModifier(mods&7));
  if(button) vterm_mouse_button(t.vt,button,pressed!=0,VTermModifier(mods&7));
}
API int kte_palette_count(void *p) {return int(static_cast<Terminal*>(p)->palette.size());}
API unsigned kte_palette(void *p,int index,int field) {
  auto &t=*static_cast<Terminal*>(p);if(index<0||size_t(index)>=t.palette.size())return 0;
  auto &s=t.palette[index];return field==0?s.fg:field==1?s.bg:s.flags;
}
