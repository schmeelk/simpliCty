# simpliCty - (C Lite)
## Overview
SimpliCty started as a project for [S4115 Programming Languages & Translators](http://www.cs.columbia.edu/~sedwards/classes/2016/4115-summer/index.html) at Columbia University.

The project propsoal can be read [here](http://www.cs.columbia.edu/~sedwards/classes/2016/4115-summer/proposals/simpliCty.pdf).

The simpliCty language is a simplified version of C which was developed by Kernighan and Ritchie. The simpliCty language contains a subset of C grammar but with a strict type system. The language uses LLVM as the backend to produce bytecode.

The simpliCty language operates best with programs that need to be Turing complete and can be defined using strict type casting and only stack-based memory management, which decreases runtime errors. The C language domain is for number crunching and embedded systems. SimpliCty programs will operate in the same domain as C with the exception that our compiler supports only a limited features of C.


### Authors
- Adam Hadar (anh2130@columbia.edu)
- Rui Gu (ruigu@cs.columbia.edu)
- Suzanna Schmeelk (schmeelk@cs.columbia.edu)
- Zachary Moffitt (z.moffitt@columbia.edu)

