
## 0.2.0

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