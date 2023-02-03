# SLT - A Common Lisp Language Plugin for Jetbrains IDE lineup

![GitHub release (latest by date)](https://img.shields.io/github/v/release/Enerccio/SLT)
![GitHub](https://img.shields.io/github/license/Enerccio/SLT)
![GitHub lisp counter](https://img.shields.io/github/search/Enerccio/SLT/lisp)
[![0.1.0](https://badgen.net/github/milestones/enerccio/SLT/1)](https://github.com/enerccio/SLT/milestone/1)
[![0.2.0](https://badgen.net/github/milestones/enerccio/SLT/2)](https://github.com/enerccio/SLT/milestone/2)
[![0.3.0](https://badgen.net/github/milestones/enerccio/SLT/4)](https://github.com/enerccio/SLT/milestone/4)
[![0.3.0](https://badgen.net/github/milestones/enerccio/SLT/5)](https://github.com/enerccio/SLT/milestone/5)
![GitHub all releases](https://img.shields.io/github/downloads/Enerccio/SLT/total)
![GitHub last commit](https://img.shields.io/github/last-commit/Enerccio/SLT)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/Enerccio/SLT)
[![(want-to-support-me? T NIL)](https://img.shields.io/liberapay/receives/Enerccio.svg?logo=liberapay)](https://liberapay.com/Enerccio)
[![Join the chat at https://gitter.im/SLT-Plugin/community](https://badges.gitter.im/SLT-Plugin/Lobby.svg)](https://gitter.im/SLT-Plugin/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

![Image](src/main/resources/logo/logo.svg)

**THIS PLUGIN IS EXPERIMENTAL and can crash at any time! Please report all bugs!**

This plugin is providing support for Common Lisp for JetBrains IDEs. 
Using modified SLIME/Swank protocol to commmunicate with SBCL providing 
IDE capabilities for Common Lisp.

# (Somewhat)Detailed Installation and Usage Guide

https://github.com/Enerccio/SLT/wiki/User-Guide

## Requirements

1) Intellij based IDE - tested on `Intellij Idea Community/Ultimate` but should work on all major IDEs
   1) Versions supported are from 2022.2 and upwards 

Optionally (see more - guide):

1) [Steel Bank Common Lisp](https://www.sbcl.org/) installed
2) [Quicklisp](https://www.quicklisp.org/beta/) installed

## Getting started

See https://github.com/Enerccio/SLT/wiki/User-Guide#plugin-installation

## Plugin options

- See guide above for SDK
- For changing colors https://github.com/Enerccio/SLT/wiki/User-Guide#change-colors-of-elements
- For changing indent https://github.com/Enerccio/SLT/wiki/User-Guide#change-indentation-settings

## Compiling source

Clone the repository and change gradle.properties for your IDE. 
Then use gradle to build the plugin. 
You can also open this as a project in Intellij Idea.

## Planned features / goals

* [ ] Upload to marketplace when it has enough features
* [x] Automatic indentation
* [x] REPL
* [x] Interactive debugging
* [x] Argument help (Ctrl+P)
* [ ] Inspection
  * [x] Basic inspection
  * [ ] Actions
  * [ ] Inspection eval
* [ ] Walkable debugger without actions 
  * [ ] Breakpoints
  * Currently impossible to do correctly with sbcl, investigating other options
* [x] Documentation 
* [x] Macro expand in documentation
  * Macro expand requires you to hover element twice for now
* [x] Find function by symbol name
* [x] Search for symbols
* [x] Back references 
* [ ] Refactoring
* [ ] List of quicklisp installed packages / ASDF packages
* [ ] List of modified top level forms that are yet to be evaluated
* [ ] Actually make an IDE, ie just plugin with dependencies as one application, not a plugin

### Far futures / possible goals 

* [x] SDK Support 
  * not a true SDK because that is only available in Intellij and not in  (for instance) PyCharm, thus
this is implemented manually.
  * [x] Download SBCL and quicklisp for user
* [x] Automatic download of lisp interpret and quicklisp
* [ ] Different lisp interpreter support 
* [ ] Remote connections to interpreters
* [ ] Rewrite everything into ABCL just for purity’s sake lol

## License

This project is licensed under [Apache License v2](LICENSE.txt).

### What does SLT even mean?

SLT - Speech Language Therapy. Only cure for LISP!

Also, backronym for Superior Lisp Tooling!