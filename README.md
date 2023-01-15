# SLT - A Common Lisp Language Plugin for Jetbrains IDE lineup

[![License: APACHE](https://badgen.net/github/license/enerccio/SLT?color=green)](LICENSE)
[![0.1.0](https://badgen.net/github/milestones/enerccio/SLT/1)](https://github.com/enerccio/SLT/milestone/1)
[![0.2.0](https://badgen.net/github/milestones/enerccio/SLT/2)](https://github.com/enerccio/SLT/milestone/2)

**THIS PLUGIN IS EXPERIMENTAL and can crash at any time! Please report all bugs!**

This plugin is providing support for Common Lisp for JetBrains IDEs. 
Using modified SLIME/Swank protocol to commmunicate with SBCL providing 
IDE capabilities for Common Lisp.

![Image](https://i.imgur.com/xbDscTJ.png "Interactive Debugger")

## Requirements

1) Intellij based IDE - tested on `Intellij Idea Community/Ultimate` but should work on all major IDEs
   1) Versions supported are from 2022.2 and upwards 
2) [Steel Bank Common Lisp](https://www.sbcl.org/) installed
3) [Quicklisp](https://www.quicklisp.org/beta/)

## Getting started

Download plugin for your IDE from releases and install it via file.

_ie_ File->Settings->Plugin, click on gear icon and then 'Install plugin from disk'

To find out which release applies to you check this table:

| Jetbrains IDE Variant |         Plugin name pattern |
|-----------------------|----------------------------:|
| CLion                 | slt-_version_-signed-CL.zip |
| GoLand                | slt-_version_-signed-GO.zip |
| Intellij Community    | slt-_version_-signed-IC.zip |
| Intellij Ultimate     | slt-_version_-signed-IU.zip |
| PyCharm               | slt-_version_-signed-PY.zip |
| PyCharm Community     | slt-_version_-signed-PC.zip |
| Rider                 | slt-_version_-signed-RD.zip |

PhpStorm is coming when I read how to build it correctly since just swapping
the type does not work. 

For the first time use, I recommend installing swank and eclector dependencies manually 
from sbcl because there is 10 seconds timeout to see if swank has initialized,and it might 
terminate before quicklisp has initialized.

Start sbcl and run this script:

On linux and default quicklisp path: 

```common lisp
(load "~/quicklisp/setup.lisp")
(ql:quickload :swank)
(ql:quickload :eclector)
```

## Plugin options

Plugin has 2 options right now.
These are accessible in Settings>SLT Configuration

- SBCL executable: Full path to sbcl or if sbcl is in path just `sbcl` is fine. 
  - on windows, you might need .exe or even full path to .exe
- Quicklisp path: path to the `setup.lisp` file of quicklisp. Defaults to `~/quicklisp/setup.lisp`

## Compiling source

Clone the repository and change gradle.properties for your IDE. 
Then use gradle to build the plugin. 
You can also open this as a project in Intellij Idea.

## Planned features / goals

* [ ] Upload to marketplace when it has enough features
* [x] REPL
* [x] Interactive debugging
* [ ] Inspection
  * [x] Basic inspection
  * [ ] Actions
  * [ ] Inspection eval
* [ ] Walkable debugger without actions 
* [ ] Breakpoints
* [x] Documentation 
* [x] Macro expand in documentation
  * Macro expand requires you to hover element twice for now
* [x] Find function by symbol name
* [ ] Search for symbols
* [ ] Back references 
* [ ] Refactoring
* [ ] List of quicklisp installed packages / ASDF packages
* [ ] List of modified top level forms that are yet to be evaluated
* [ ] Actually make an IDE, ie just plugin with dependencies as one application, not a plugin

### Far futures / possible goals 

* [ ] Virtual Environment à la pycharm so you can specify which interpret instance you want
* [ ] Automatic download of lisp interpret and quicklisp
* [ ] Different lisp interpreter support 
* [ ] Remote connections to interpreters

## License

This project is licensed under [Apache License v2](LICENSE.txt).

### What does SLT even mean?

SLT - Speech Language Therapy. Only cure for LISP!