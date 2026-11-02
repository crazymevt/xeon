# AI chat

Choose **File › New AI Chat** (Option+Command+N) or the chat icon on the activity
rail. Each chat is a tab beside your documents and works in the open folder.

## Setup

Install and sign in to the **Codex** or **Claude Code** CLI. Xeon uses the CLI's
existing account; there is no API key field. A new chat uses Codex when it is
installed, otherwise Claude. Choose the other in the **AI** menu of the chat's
header to switch; switching starts a separate conversation.

## The header

- **AI, Model, Effort, Mode** come from the connected provider. Unsupported effort
  levels are not offered. Settings change while the assistant is idle.
- **Agent** mode lets the assistant run commands, edit files and use the editor,
  REPL and debugger, asking first. **Chat only** discusses, and refuses actions.
- **Approve for me** (Agent mode, this chat only) allows commands, file edits and
  Xeon's editor, REPL and debugger actions without asking. Questions, forms and
  permission requests still ask.
- **New Chat** starts another conversation; **Clear Context** keeps the transcript
  but starts the provider afresh. Both confirm first. **Reconnect** appears when the
  connection is lost and resumes the conversation.
- The second row shows the connection, account **usage limits** (hover for each
  window's remaining share and local reset time), the **context** in use against
  the model's window with a meter, the session's token counts, and the folder.
  Values the provider has not reported say so rather than guessing.

## Conversations and requests

Return sends; Shift+Return inserts a new line. Paste or drop up to four
screenshots while idle. While the assistant works, the composer steers the same
turn, and **Stop** interrupts it. A chat waiting for you shows an orange dot in its
tab.

Requests appear as cards above the composer:

- **Approvals** show the command or action with its details and the decisions the
  provider offers (for example Allow once, Allow for this session, Decline).
- **Questions** offer choices, multiple selection, free text and masked secrets.
- **Forms** (elicitation) show simple schemas as fields, link requests as a button,
  and anything else as an explicit JSON editor with the full schema.

Links open in your browser. Message text is rendered as escaped Markdown; the page
loads nothing from the network.

## What the assistant can do in Xeon

Besides its own tools, the assistant has Xeon's editor and runtime:

| Tool | What it does | Asks first |
| --- | --- | --- |
| `editor_state` | Open documents, unsaved changes, the active caret and selection | No |
| `editor_read` | A document's current text, including unsaved changes | No |
| `editor_open`, `editor_select`, `editor_new` | Open a file, move the caret, start a document | No |
| `editor_replace`, `editor_write` | Edit an open document (undoable in the editor, not saved) | Yes |
| `editor_save` | Save a document to its file | Yes |
| `runtime_state` | REPL status, output, results, problems, debugger, inspector | No |
| `repl_start`, `repl_evaluate`, `repl_interrupt` | Start, use and interrupt the editor's REPL | Yes |
| `debugger_frame`, `inspector_part` | Read debugger frames and inspector parts | No |
| `debugger_restart`, `debugger_abort`, `inspector_evaluate` | Act in the debugger or inspector | Yes |

Actions that ask first are allowed automatically with **Approve for me**. The tools
use the editor's own REPL session, so you see everything in the REPL, Debugger and
Inspector panes. A runtime action refuses to run if the REPL, debugger or inspector
changed since the request.

## History

Chat records and provider preferences are kept in `chats/` in Xeon's configuration
folder (`~/Library/Application Support/Xeon/` on macOS), outside the project.
Opening another folder starts a new conversation there. Pending requests and
credentials are never saved.

## Tests

`make ui-test` drives chats with an offline protocol peer
(`tests/assistant-fixture.lisp`) for both providers: replies, usage, approvals,
questions, Approve for me with a REPL tool call, and the editor tools. It never
contacts an AI account.
