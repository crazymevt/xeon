(in-package #:xeon.chat-render)

(defun render-message (message provider register-code)
  (let ((role (getf message :role)) (text (getf message :text)))
    (with-output-to-string (out)
      (case role
        (:divider (format out "<div class='divider'><span>~A</span></div>" (html-escape (string-trim '(#\Space #\Newline #\Return #\─) text))))
        (:notice
         (let* ((clean (string-trim '(#\Space #\Newline #\Return) (strip-ansi text)))
                (summary (subseq clean 0 (min 96 (or (position #\Newline clean) (length clean))))))
           (format out "<details class='activity'><summary>~A</summary><pre>~A</pre></details>" (html-escape summary) (html-escape clean))))
        (otherwise
         (format out "<article class='message ~A'><div class='avatar' aria-hidden='true'>~A</div><div class='bubble'><div class='message-meta'><span>~A</span><button class='copy-message' data-message-copy='~D' aria-label='Copy message'>Copy</button></div>"
           (if (eq role :user) "user" "assistant") (if (eq role :user) "Y" (if (eq provider :codex) "Cx" "Cl"))
           (if (eq role :user) "You" (string-capitalize provider)) (getf message :id))
         (when (getf message :truncated) (write-string "<p class='clipped'>Earlier text in this message was clipped.</p>" out))
         (write-string (render-markdown text register-code) out)
         (write-string "</div></article>" out))))))

(defun chat-page ()
  "Trusted local shell. All conversation content arrives as escaped, inert HTML.
The page has no external assets, raw Markdown HTML, or ambient network access."
  (identity "<!doctype html><html><head><meta charset='utf-8'>
<meta name='viewport' content='width=device-width,initial-scale=1'>
<meta http-equiv='Content-Security-Policy' content=\"default-src 'none'; script-src 'nonce-xeon-chat'; style-src 'unsafe-inline'; img-src data:; connect-src 'none'; frame-src 'none'; object-src 'none'; base-uri 'none'; form-action 'none'\">
<title>Xeon Chat</title><style>
:root{color-scheme:dark;--bg:#17191e;--fg:#e4e7ee;--muted:#a2a8b5;--surface:#21252c;--user:#23354d;--border:#343a45;--accent:#79b8ff;--code:#11151b;--kw:#cba6f7;--str:#a6da95;--comment:#7f8c8d;--number:#f5b97f;--symbol:#8bd5ca;--function:#f2d680;--parameter:#f0a6c5}
*{box-sizing:border-box}html{background:var(--bg);color:var(--fg)}body{margin:0;font:14px/1.65 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;overflow-wrap:anywhere}button{font:inherit;cursor:pointer;color:var(--muted);border:1px solid transparent;background:transparent;border-radius:6px;padding:3px 9px;font-size:11px}button:hover{color:var(--fg);background:var(--border)}button:focus-visible,a:focus-visible{outline:2px solid var(--accent);outline-offset:2px}main{max-width:960px;padding:28px 26px 22px;margin:0 auto;min-height:100vh}.message{display:flex;gap:11px;margin:0 0 22px;align-items:flex-start}.avatar{flex:0 0 29px;width:29px;height:29px;display:grid;place-items:center;border-radius:10px;background:var(--surface);color:var(--accent);font-size:11px;font-weight:650;margin-top:5px;border:1px solid var(--border)}.bubble{min-width:0;max-width:calc(100% - 42px);padding:14px 18px 16px;background:var(--surface);border:1px solid var(--border);border-radius:4px 17px 17px 17px;box-shadow:0 3px 16px rgba(0,0,0,.07)}.user{flex-direction:row-reverse;margin-left:12%}.user .bubble{background:var(--user);border-color:transparent;border-radius:17px 4px 17px 17px}.user .avatar{background:var(--user);border-color:transparent}.message-meta{display:flex;align-items:center;justify-content:space-between;gap:24px;margin:0 0 7px;font-size:11px;font-weight:600;color:var(--muted)}.message-meta .copy-message{opacity:0;font-weight:400}.bubble:hover .copy-message,.copy-message:focus{opacity:1}.bubble>:last-child{margin-bottom:0}p{margin:0 0 13px;white-space:pre-wrap}h1,h2,h3,h4,h5,h6{line-height:1.35;margin:22px 0 11px;color:var(--fg)}h1{font-size:23px}h2{font-size:20px}h3{font-size:17px}h4,h5,h6{font-size:15px}.bubble>h1:first-of-type,.bubble>h2:first-of-type,.bubble>h3:first-of-type{margin-top:8px}strong{font-weight:650}ul,ol{padding-left:23px;margin:0 0 14px}li{padding-left:3px;margin:5px 0}blockquote{border-left:3px solid var(--accent);margin:14px 0;padding:4px 13px;color:var(--muted);white-space:pre-wrap}a{color:var(--accent);text-decoration:none}a:hover{text-decoration:underline}code{font:12.5px/1.65 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;background:var(--code);border-radius:4px;padding:2px 5px}pre{margin:0;tab-size:4}.code-block{background:var(--code);border:1px solid var(--border);border-radius:10px;margin:14px 0 16px;overflow:hidden}.code-block header{display:flex;align-items:center;justify-content:space-between;gap:12px;padding:6px 11px;border-bottom:1px solid var(--border);color:var(--muted);font:11px/1.5 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}.code-block pre{padding:13px 15px;overflow-x:auto;white-space:pre;overflow-wrap:normal}.code-block pre code{padding:0;background:none;border-radius:0}.tok-1{color:var(--kw)}.tok-2{color:var(--str)}.tok-3{color:var(--comment)}.tok-4{color:var(--number)}.tok-5{color:var(--symbol)}.tok-6{color:var(--function)}.tok-7{color:var(--parameter)}hr{border:0;border-top:1px solid var(--border);margin:20px 0}.table-wrap{overflow-x:auto;margin:15px 0}table{border-collapse:collapse;min-width:100%;font-size:13px}td,th{padding:8px 12px;border:1px solid var(--border);text-align:left}th{font-weight:600;background:var(--code)}.activity{font-size:11px;color:var(--muted);margin:0 0 15px;padding-left:40px}.activity summary{cursor:pointer;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}.activity pre{white-space:pre-wrap;background:var(--code);padding:12px;border-radius:8px;margin-top:7px;font:11px/1.6 ui-monospace,monospace;max-height:220px;overflow:auto}.divider{display:flex;align-items:center;gap:15px;color:var(--muted);font-size:11px;margin:25px 0}.divider:before,.divider:after{content:'';height:1px;flex:1;background:var(--border)}.clipped{color:var(--muted);font-size:11px}.empty{min-height:65vh;display:flex;align-items:center;justify-content:center;flex-direction:column;text-align:center;color:var(--muted)}.empty .mark{font-size:27px;width:62px;height:62px;display:grid;place-items:center;background:var(--surface);border:1px solid var(--border);border-radius:20px;margin-bottom:17px;color:var(--accent)}.empty h2{font-size:20px;font-weight:600;margin:0 0 8px;color:var(--fg)}.empty p{font-size:13px;max-width:330px}.working{padding:0 0 12px 41px;color:var(--muted);font-size:12px}.working:before{content:'●';display:inline-block;color:var(--accent);font-size:9px;margin-right:9px;animation:pulse 1.5s infinite}@keyframes pulse{50%{opacity:.25}}#jump{display:none;position:fixed;bottom:18px;left:50%;transform:translateX(-50%);background:var(--surface);color:var(--fg);border:1px solid var(--border);box-shadow:0 6px 22px #0003;font-size:12px;padding:7px 14px;border-radius:20px}@media(max-width:560px){main{padding:18px 12px}.avatar{display:none}.bubble{max-width:100%;padding:12px 14px}.user{margin-left:8%}.activity{padding-left:0}}@media(prefers-reduced-motion:reduce){.working:before{animation:none}}
</style></head><body><main id='chat'><div class='empty'><div class='mark'>✦</div><h2>Your assistant is ready</h2><p>Ask a question, explore an idea, or work on your project.</p></div></main><button id='jump'>↓ Latest message</button>
<script nonce='xeon-chat'>(function(){'use strict';
const root=document.getElementById('chat'),jump=document.getElementById('jump');let chunks=[],version=0,pending=null,selectionTimer;
function send(x){window.dispatchEvent(new CustomEvent('xeon-message',{detail:x}))}
// Only explicit drop/paste gestures read images. Never navigate to dropped files.
let imageQueue=Promise.resolve();
const input=document.getElementById('chat-input');let submitting=false;
function submitInput(){if(!input||submitting)return;submitting=true;input.readOnly=true;
 imageQueue.then(()=>send({type:'submit-input',text:input.value}));}
window.xeonInput={submit:submitInput,ack(ok){if(ok)input.value='';submitting=false;input.readOnly=false;input.focus()}};
if(input)input.addEventListener('keydown',e=>{if(e.key==='Enter'&&!e.shiftKey&&!e.isComposing){e.preventDefault();submitInput()}});
function attachFiles(files){if(submitting)return;for(const file of [...files]){
 imageQueue=imageQueue.then(async()=>{
  if(!['image/png','image/jpeg','image/webp'].includes(file.type))throw Error('Use PNG, JPEG or WebP screenshots.');
  if(file.size>20*1024*1024)throw Error('Screenshot exceeds 20 MiB.');
  const url=await new Promise((ok,no)=>{const r=new FileReader();r.onload=()=>ok(r.result);r.onerror=no;r.readAsDataURL(file)});
  const image=await new Promise((ok,no)=>{const i=new Image();i.onload=()=>ok(i);i.onerror=()=>no(Error('Cannot decode screenshot.'));i.src=url});
  const scale=Math.min(1,1600/Math.max(image.width,image.height));
  const canvas=document.createElement('canvas');canvas.width=Math.max(1,Math.round(image.width*scale));canvas.height=Math.max(1,Math.round(image.height*scale));
  const ctx=canvas.getContext('2d');ctx.fillStyle='white';ctx.fillRect(0,0,canvas.width,canvas.height);ctx.drawImage(image,0,0,canvas.width,canvas.height);
  let result=canvas.toDataURL('image/png');if(result.length>680000)result=canvas.toDataURL('image/jpeg',0.8);
  if(result.length>680000)throw Error('Screenshot is too detailed; crop it before attaching.');
  send({type:'attach-image',url:result});
 }).catch(e=>send({type:'attachment-error',text:e.message||'Cannot read screenshot.'}));
}}
document.addEventListener('dragover',e=>{e.preventDefault();if(e.dataTransfer)e.dataTransfer.dropEffect='copy'});
document.addEventListener('drop',e=>{e.preventDefault();attachFiles(e.dataTransfer.files)});
document.addEventListener('paste',e=>{let files=[...(e.clipboardData?.items||[])].filter(i=>i.kind==='file').map(i=>i.getAsFile()).filter(Boolean);if(!files.length)files=[...(e.clipboardData?.files||[])];if(files.length){e.preventDefault();attachFiles(files)}});
// WebViews can consume native menu accelerators; forward the sidebar chord.
document.addEventListener('keydown',function(e){if(e.code==='KeyB'&&(e.metaKey||e.ctrlKey)&&!e.shiftKey){e.preventDefault();e.stopPropagation();send({type:'sidebar-shortcut',id:e.altKey?'right':'left'})}});
function atBottom(){return document.documentElement.scrollHeight-window.innerHeight-window.scrollY<100}
function hasSelection(){const s=window.getSelection();return s&&!s.isCollapsed&&s.toString().length>0}
function scrollEnd(){window.scrollTo(0,document.documentElement.scrollHeight);jump.style.display='none'}
function apply(data){if(hasSelection()){pending=data;return}const follow=atBottom(),oldY=window.scrollY;const keep=new Set();
 if(!data.messages.length){root.innerHTML='<div class=\"empty\"><div class=\"mark\">✦</div><h2>Chat with '+(data.provider==='codex'?'Codex':'Claude')+'</h2><p>Ask a question, explore an idea, or work on your project.</p></div>'}
 else{const empty=root.querySelector('.empty');if(empty)empty.remove();for(const m of data.messages){const id='message-'+m.id;keep.add(id);let el=document.getElementById(id);if(!el){el=document.createElement('div');el.id=id;root.appendChild(el)}if(el.dataset.version!==String(m.version)){el.innerHTML=m.html;el.dataset.version=String(m.version)}}for(const el of [...root.children]){if(el.id&&el.id!=='working'&&!keep.has(el.id))el.remove()}}
 let work=document.getElementById('working');if(data.busy){if(!work){work=document.createElement('div');work.id='working';work.className='working';root.appendChild(work)}work.textContent=data.waiting?'Waiting for your response below':'Thinking…';root.appendChild(work)}else if(work)work.remove();
 if(follow)requestAnimationFrame(scrollEnd);else{window.scrollTo(0,oldY);jump.style.display='block'}send({type:'rendered',version:data.version,count:data.messages.length})}
window.xeonChat={reset(){pending=null;chunks=[];root.replaceChildren();window.getSelection()?.removeAllRanges()},begin(v){version=v;chunks=[]},chunk(v,s){if(v===version)chunks.push(s)},end(v){if(v===version){const data=JSON.parse(chunks.join(''));chunks=[];apply(data)}},theme(vars){for(const k of Object.keys(vars))document.documentElement.style.setProperty('--'+k,vars[k]);document.documentElement.style.colorScheme=vars.scheme||'dark'},copied(id){for(const b of document.querySelectorAll('[data-code-id]'))if(b.dataset.codeId===id){b.textContent='Copied';setTimeout(()=>{if(b.isConnected)b.textContent='Copy'},1400)}}};
document.addEventListener('click',e=>{const copy=e.target.closest('[data-code-id]'),message=e.target.closest('[data-message-copy]'),link=e.target.closest('[data-url]');if(copy){e.preventDefault();send({type:'copy-code',id:copy.dataset.codeId})}else if(message){e.preventDefault();send({type:'copy-message',id:Number(message.dataset.messageCopy)})}else if(link){e.preventDefault();send({type:'open-link',url:link.dataset.url})}});
document.addEventListener('selectionchange',()=>{clearTimeout(selectionTimer);selectionTimer=setTimeout(()=>{send({type:'selection',text:window.getSelection().toString()});if(!hasSelection()&&pending){const data=pending;pending=null;apply(data)}},120)});window.addEventListener('scroll',()=>{jump.style.display=atBottom()?'none':'block'},{passive:true});jump.addEventListener('click',scrollEnd);send({type:'ready'});
})();</script></body></html>"))

(defun chat-input-page ()
  "Use the same trusted paste/drop pipeline as the transcript, with a text composer.
No clipboard polling or native, OS-specific clipboard commands are needed."
  (let* ((page (chat-page)) (script (search "<script nonce=" page)))
    (concatenate 'string (subseq page 0 script)
      "<style>html,body{height:100%;overflow:hidden}main,#jump{display:none!important}textarea{box-sizing:border-box;width:100%;height:100%;resize:none;background:var(--surface);color:var(--fg);border:1px solid var(--border);padding:9px;font:14px/1.5 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}textarea:focus{outline:1px solid var(--accent);outline-offset:-1px}</style><textarea id='chat-input' aria-label='Message' placeholder='Message — paste a screenshot or type here'></textarea>"
      (subseq page script))))
