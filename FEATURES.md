Since I started supporting multiple interprets I have learned that some features just don't work well across them or even in swank. 
This is a list of features that are supported.

| Legend |                                        |
|--------|----------------------------------------|
| ✅️     | Implemented                            |
| ❓      | Not implemented but might be in future |
| ❎      | Is not supported                       |

Unsupported and will not be supported implementations:

* CLISP - does not work, maybe with threads, but single threaded it's useless and crashes on debug attempt

| Features / Lisp   | SBCL | ABCL | CCL |
|-------------------|------|------|-----|
| REPL              | ✅️   | ✅️   | ✅️  |
| Buffer Evaluation | ✅️   | ✅️   | ✅️  |
| Documentation     | ✅    | ✅    | ✅️  |
| Macroexpand       | ✅    | ✅    | ✅️  |
| Debugger          | ✅    | ✅    | ️   |
| Frame Eval        | ✅    | ❎    | ️   |
| Stepping Debugger | ❎    | ❎    | ️   |
| References        | ✅    | ❎    | ✅️  |
| Inspector         | ✅¹   | ✅²   | ️   |
| Autocomplete      | ✅    | ✅    |     |
| Find References   | ✅    | ❎    | ️   |

¹Only read-only inspector available

²Only read-only inspector available, also no history support