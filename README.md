# SLT - A Common Lisp Language Plugin for Jetbrains IDE lineup

![GitHub release (latest by date)](https://img.shields.io/github/v/release/Enerccio/SLT)
[![Version](https://img.shields.io/jetbrains/plugin/v/21132-slt.svg)](https://plugins.jetbrains.com/plugin/21132-slt)
![JetBrains plugins](https://img.shields.io/jetbrains/plugin/d/21132)
![GitHub Release Date](https://img.shields.io/github/release-date/Enerccio/SLT)
![OSS Lifecycle](https://img.shields.io/osslifecycle/Enerccio/SLT)
![GitHub](https://img.shields.io/github/license/Enerccio/SLT)
![GitHub lisp counter](https://img.shields.io/github/search/Enerccio/SLT/lisp)
[![0.1.0](https://badgen.net/github/milestones/enerccio/SLT/1)](https://github.com/enerccio/SLT/milestone/1)
[![0.2.0](https://badgen.net/github/milestones/enerccio/SLT/2)](https://github.com/enerccio/SLT/milestone/2)
[![0.3.0](https://badgen.net/github/milestones/enerccio/SLT/4)](https://github.com/enerccio/SLT/milestone/4)
[![0.4.0](https://badgen.net/github/milestones/enerccio/SLT/5)](https://github.com/enerccio/SLT/milestone/5)
[![0.5.0](https://badgen.net/github/milestones/enerccio/SLT/6)](https://github.com/enerccio/SLT/milestone/6)
![GitHub all releases](https://img.shields.io/github/downloads/Enerccio/SLT/total)
![GitHub last commit](https://img.shields.io/github/last-commit/Enerccio/SLT)
![GitHub commit activity](https://img.shields.io/github/commit-activity/m/Enerccio/SLT)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/Enerccio/SLT)
![Lines of code](https://img.shields.io/tokei/lines/github/Enerccio/SLT)
![GitHub top language](https://img.shields.io/github/languages/top/Enerccio/SLT)
[![(want-to-support-me? T NIL)](https://img.shields.io/liberapay/receives/Enerccio.svg?logo=liberapay)](https://liberapay.com/Enerccio)
![Liberapay patrons](https://img.shields.io/liberapay/patrons/Enerccio)
[![Support me on Patreon](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Fshieldsio-patreon.vercel.app%2Fapi%3Fusername%3Denerccio%26type%3Dpledges&style=flat)](https://patreon.com/enerccio)[![Support me on Patreon](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Fshieldsio-patreon.vercel.app%2Fapi%3Fusername%3Denerccio%26type%3Dpatrons&style=flat)](https://patreon.com/enerccio)
![Maintenance](https://img.shields.io/maintenance/yes/2023)
![GitHub issues](https://img.shields.io/github/issues/Enerccio/SLT)
![GitHub branch checks state](https://img.shields.io/github/checks-status/Enerccio/SLT/master)

[![Subreddit subscribers](https://img.shields.io/reddit/subreddit-subscribers/SLT_IDE?style=social)](https://old.reddit.com/r/SLT_IDE/)
![GitHub forks](https://img.shields.io/github/forks/Enerccio/SLT?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/Enerccio/SLT?style=social)
![GitHub watchers](https://img.shields.io/github/watchers/Enerccio/SLT?style=social)
[![Join the chat at https://gitter.im/SLT-Plugin/community](https://badges.gitter.im/SLT-Plugin/Lobby.svg)](https://gitter.im/SLT-Plugin/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
![GitHub Discussions](https://img.shields.io/github/discussions/Enerccio/SLT)

![Image](src/main/resources/logo/logo.svg)

**THIS PLUGIN IS EXPERIMENTAL and can crash at any time! Please report all bugs!**

This plugin is providing support for Common Lisp for JetBrains IDEs. 
Using modified SLIME/Swank protocol to communicate with lisp interpret providing 
IDE capabilities for Common Lisp.

# (Somewhat) Detailed Installation and Usage Guide

You can now download the plugin from marketplace! https://plugins.jetbrains.com/plugin/21132-slt

https://github.com/Enerccio/SLT/wiki/User-Guide

## Requirements

1) Intellij based IDE - tested on `Intellij Idea Community/Ultimate` but should work on all major IDEs
   1) Versions supported are from 2022.2 and upwards 

Optionally (see more - guide):

1) One of the supported LISP Interprets installed:
   * [Steel Bank Common Lisp](https://www.sbcl.org/)
   * [Armed Bear Common Lisp](https://armedbear.common-lisp.dev/)
   * [Clozure Common Lisp](https://ccl.clozure.com/)
   * [Allegro CL](https://franz.com/products/allegro-common-lisp/)
   * [CMUCL](https://www.cons.org/cmucl/)
2) [Quicklisp](https://www.quicklisp.org/beta/) installed

(Not all features work with all interprets, see [FEATURES.md](FEATURES.md) for more info!)

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

* [x] Upload to marketplace when it has enough features
* [x] Automatic indentation
* [x] REPL
  * [x] REPL return inspection
* [x] Interactive debugging
* [x] Argument help (Ctrl+P)
* [x] Inspection
  * [x] Basic inspection
  * [x] Actions
  * [x] Symbol inspector
  * [ ] Inspection eval
* [x] Breakpoints
* [x] Documentation 
  * [x] Hyperspec intergration
* [x] Macro expand (all, 1, normal)
* [x] Find function by symbol name
* [x] Search for symbols
* [x] Back references 
* [x] Rainbow braces
* [ ] Refactoring
  * [x] Extracting lambda to function
* [ ] List of quicklisp installed packages / ASDF packages
* [ ] List of modified top level forms that are yet to be evaluated
* [ ] Actually make an IDE, ie just plugin with dependencies as one application, not a plugin
* [x] SDK Support
    * not a true SDK because that is only available in Intellij and not in  (for instance) PyCharm, thus
      this is implemented manually.
    * [x] Download SBCL and quicklisp for user
* [x] Automatic download of lisp interpret and quicklisp
* [x] Different lisp interpreter support

## License

This project is licensed under [Apache License v2](LICENSE.txt).

# How to help?

* Please install the plugin and try it out!
* Report all bugs so I can catch them all. Click [here](https://github.com/enerccio/SLT/issues/new) to report.
* If you like the project, consider starring it or spreading info about it
* This project is OSS so if you have knowledge and want to implement something you can fork the repo and then create PR!

## Donations

If you feel like it, you can donate to support my work via [Liberapay](https://liberapay.com/Enerccio/donate) or 
[Patreon](https://www.patreon.com/enerccio).

# What does SLT even mean?

SLT - Speech Language Therapy. Only cure for LISP!

Also, backronym for Superior Lisp Tooling!