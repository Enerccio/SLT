Since I started supporting multiple interprets I have learned that some features just don't work well across them or even in swank. 
This is a list of features that are supported.

| Legend |                                        |
|--------|----------------------------------------|
| ✅️     | Implemented                            |
| ❓      | Not implemented but might be in future |
| ❎      | Is not supported                       |

| Features / Lisp   | SBCL | ABCL |
|-------------------|------|------|
| REPL              | ✅️   | ✅️   |
| Buffer Evaluation | ✅️   | ✅️   |
| Documentation     | ✅    | ✅    |
| Macroexpand       | ✅    | ✅    |
| Debugger          | ✅    | ✅    |
| Frame Eval        | ✅    | ❎    |
| Stepping Debugger | ❎    | ❎    |
| References        | ✅    | ❎    |
| Inspector         | ✅¹   | ✅²   |
| Autocomplete      | ✅    | ✅    |
| Find References   | ✅    | ❎    |

1 - Only read-only inspector available

2 - Only read-only inspector available, also no history support