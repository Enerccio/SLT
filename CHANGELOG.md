## 0.2.2 - tentative

### Fixes

- Fixed action threading
- Fixed repl not closing
- Fixed templates
- Fixed lexer issue with comments

## 0.2.1 - 230120

### Fixes

- Attempting to fix windows path error

## 0.2.0 - 230120

### Added

- Added first version of inspector - so far only read-only. 
To access, start interactive debugging and then click on any local variable.
- Macro expand. When you hover over a symbol that is a macro call in a form, 
it will macro expand it in the documentation. Due to async notion, 
you need to hover again to see it.
- Basic completion suggestion working

### Fixes

- Changed internal environment to be more decoupled 
- Fixed code highlight for methods
- Fixed bad package when package does not exist
- Fixed lisp parser, `REFERENCE_LABEL` requiring `datum`, now it is stand alone
- Fixed line comment highlight color
- Fixed highlight on braces 
  - no longer using standard BracePair but instead use internal brace matcher to prevent auto brace inserting

## 0.1.1 - 230115

### Fixes

- Fixed bug in build without certificates (oops!)
- Fixed package detector to work with `#:` packages
- Fixed settings to correctly restart on change
- Fixed settings to save quicklisp path

## 0.1.0 - 230115

Initial release

### Added

- Basic language parsing and lexing
- REPL
  - Click on `+` icon to create REPL  
- Interactive debugger
- Eval actions - right click menu in editor
  - Eval in region
  - Eval previous/next/current form
  - Eval file of current editor
- Eval file action
- Some basic `.cl`, `.lisp` and `.asdf` templates