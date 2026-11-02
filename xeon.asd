(asdf:defsystem "xeon"
  :description "Xeon headless editor backend"
  :version "0.1.0"
  :depends-on ("xeon/core" "xeon/search" "xeon/themes" "xeon/folders" "xeon/syntax")
  :in-order-to ((test-op (test-op "xeon/tests"))))

(asdf:defsystem "xeon/ui"
  :description "Native LGT editor shell; widgets and layout only, behavior stays in the backend"
  :depends-on ("xeon/core" "xeon/search" "xeon/themes" "xeon/folders" "xeon/syntax" "xeon/repl"
               "xeon/assistants" "xeon/chat-render" "xeon/databases" "xeon/http" "lgt")
  :serial t
  :components ((:file "src/ui/package") (:file "src/ui/theme") (:file "src/ui/icons")
               (:file "src/ui/state") (:file "src/ui/document") (:file "src/ui/editor")
               (:file "src/ui/explorer") (:file "src/ui/tools") (:file "src/ui/documents")
               (:file "src/ui/documentation") (:file "src/ui/preview") (:file "src/ui/chat-tools") (:file "src/ui/chat") (:file "src/ui/workbench") (:file "src/ui/menus") (:file "src/ui/projects") (:file "src/ui/source-control") (:file "src/ui/databases") (:file "src/ui/api") (:file "src/ui/api-sidebar") (:file "src/ui/settings")
               (:file "src/ui/main")
               (:static-file "assets/chat/chat.css") (:static-file "assets/chat/chat.js")
               (:static-file "assets/fonts/material-icons/MaterialIcons-Regular.ttf")
               (:static-file "assets/fonts/material-icons/LICENSE")
               (:static-file "assets/fonts/material-icons/PROVENANCE.md"))
  :in-order-to ((test-op (test-op "xeon/ui-tests"))))

(asdf:defsystem "xeon/ui-tests"
  :description "Headless tests of the editor shell"
  :depends-on ("xeon/ui")
  :components ((:file "tests/ui"))
  :perform (test-op (op system)
             (declare (ignore op system))
             (uiop:symbol-call :xeon.ui.tests :run-tests)))

(asdf:defsystem "xeon/core"
  :description "UI-independent buffer and editing model"
  :version "0.1.0"
  :depends-on ("xeon/docset")
  :serial t
  :components ((:file "src/core/package")
               (:file "src/core/text-storage")
               (:file "src/core/buffer")
               (:file "src/core/cursor")
               (:file "src/core/editing")
               (:file "src/core/filesystem")
               (:file "src/core/files")
               (:file "src/core/buffer-manager")
               (:file "src/core/hyperspec")
               (:file "src/core/documentation-render"))
  :in-order-to ((test-op (test-op "xeon/tests"))))

(asdf:defsystem "xeon/docset"
  :description "Vendored portable reader for Cadre-compatible local docsets"
  :serial t
  :components ((:file "vendor/docset/package") (:file "vendor/docset/docset")))

(asdf:defsystem "xeon/repl"
  :description "Headless SBCL socket/process adapters for Swank, nREPL and Racket"
  :depends-on ("sb-bsd-sockets") :serial t
  :components ((:file "src/repl/package") (:file "src/repl/base") (:file "src/repl/sexp")
               (:file "src/repl/swank") (:file "src/repl/bencode") (:file "src/repl/nrepl")
               (:file "src/repl/adapters") (:file "src/repl/session") (:file "src/repl/problems") (:file "src/repl/function-help")))

(asdf:defsystem "xeon/terminal"
  :description "Headless libvterm terminal with native PTY / Windows ConPTY transport"
  :depends-on ("cffi" "babel") :serial t
  :components ((:file "src/terminal/package") (:file "src/terminal/native")
               (:file "src/terminal/session")))

(asdf:defsystem "xeon/assistants"
  :description "Headless Codex and Claude chat, approvals and runtime tool protocol"
  :depends-on ("xeon/core") :serial t
  :components ((:file "src/assistants/package") (:file "src/assistants/json")
               (:file "src/assistants/process") (:file "src/assistants/session") (:file "src/assistants/history") (:file "src/assistants/limits")
               (:file "src/assistants/providers") (:file "src/assistants/requests")))

(asdf:defsystem "xeon/chat-render"
  :description "Headless, escaped Markdown presentation for assistant chats"
  :depends-on ("xeon/assistants" "xeon/syntax") :serial t
  :components ((:file "src/chat-render/chat-render-package") (:file "src/chat-render/chat-markdown") (:file "src/chat-render/chat-html")))

(asdf:defsystem "xeon/http"
  :description "Headless HTTP(S) requests through native libcurl; no curl executable"
  :depends-on ("xeon/core" "xeon/themes" "cffi" "babel") :serial t
  :components ((:file "src/http/package") (:file "src/http/request")
               (:file "src/http/collection") (:file "src/http/builder") (:file "src/http/libcurl")
               (:file "src/http/jobs") (:file "src/http/pretty") (:file "src/http/environments")))

(asdf:defsystem "xeon/databases"
  :description "Headless SQLite driver, connection workers and stored connections (requires platform SQLite shared library)"
  :depends-on ("cffi" "xeon/themes") :serial t
  :components ((:file "src/databases/package") (:file "src/databases/sqlite")
               (:file "src/databases/worker") (:file "src/databases/connections")))

(asdf:defsystem "xeon/search"
  :description "UI-independent literal and CL-PPCRE search/replace"
  :version "0.1.0"
  :depends-on ("xeon/core" (:version "cl-ppcre" "2.1.2"))
  :components ((:file "src/core/search")))

(asdf:defsystem "xeon/themes"
  :description "UI-independent theme palettes, safe data files and preferences"
  :depends-on ("xeon/core")
  :serial t
  :components ((:file "src/themes/package") (:file "src/themes/configuration")
               (:file "src/themes/themes") (:file "src/themes/history")))

(asdf:defsystem "xeon/folders"
  :description "Headless lazy folder listing, Git ignore filtering and background reader"
  :depends-on ("xeon/core" "xeon/search") :serial t
  :components ((:file "src/folders/package") (:file "src/folders/filesystem")
               (:file "src/folders/project") (:file "src/folders/git-status") (:file "src/folders/mutations") (:file "src/folders/copy-worker") (:file "src/folders/systems") (:file "src/folders/worker") (:file "src/folders/search")))

(asdf:defsystem "xeon/syntax"
  :description "Headless incremental lexical highlighting for project languages"
  :depends-on ("xeon/core" "xeon/folders" "cl-ppcre") :serial t
  :components ((:file "src/syntax/package") (:file "src/syntax/languages")
               (:file "src/syntax/completion") (:file "src/syntax/lexer") (:file "src/syntax/session") (:file "src/syntax/indentation-rules") (:file "src/syntax/structure") (:file "src/syntax/function-help") (:file "src/syntax/outline") (:file "src/syntax/pretty-layout") (:file "src/syntax/format")))

(asdf:defsystem "xeon/tests"
  :description "Headless backend regression suite"
  :depends-on ("xeon/core" "xeon/search" "xeon/themes" "xeon/folders" "xeon/syntax" "xeon/repl" "xeon/assistants" "xeon/chat-render")
  :serial t
  :components ((:file "tests/core")
               (:file "tests/files")
               (:file "tests/buffer-manager")
               (:file "tests/search")
               (:file "tests/themes")
               (:file "tests/folders") (:file "tests/git") (:file "tests/workspace-search") (:file "tests/repl") (:file "tests/assistants") (:file "tests/chat-render")
               (:file "tests/syntax") (:file "tests/outline") (:file "tests/format") (:file "tests/repl-problems"))
  :perform (test-op (op system)
             (declare (ignore op system))
             (uiop:symbol-call :xeon.tests :run-tests)))
