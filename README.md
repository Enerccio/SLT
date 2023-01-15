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

1) Intellij based IDE - tested on `Intellij Idea Community/Ultimate` but should workd on all major IDEs
2) [Steel Bank Common Lisp](https://www.sbcl.org/) installed
3) [Quicklisp](https://www.quicklisp.org/beta/)

## Getting started

Download plugin for your IDE from releases and install it via file.
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

## Compiling source

Clone the repository and change gradle.properties for your IDE. 
Then use gradle to build the plugin. 
You can also open this as a project in Intellij Idea.

## Planned features / goals

* [ ] Upload to marketplace when it has enough features
* [x] REPL
* [x] Interactive debugging
* [ ] Walkable debugger without actions 
* [ ] Breakpoints
* [x] Documentation 
* [ ] Macro expand in documentation
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