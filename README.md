# Overview

This is a simple library consisting of scheme code to compute elementary 
functions.

The installation is simple, just load it by calling
`(load "calculator.scm")`. There are no dependencies. 

It's coded for the MIT-Scheme, so some of it might not be platform independent 
(mostly the `infinite?` and `:+inf.*` functions might cause problems).

This is released under the MIT License. There's nothing novel or exciting about 
the calculator, it's just written from scratch.

## Known Issues

I need some way to handle infinity as a number, but in a "platform independent"
way.

Right now, `:pi` is too unwieldy (although good to, what, 65 digits). If
used in any computation, it slows things down too much. Is there any
better approximation to pi?

## Future Directions

Well, since we haven't considered special functions, that might be a direction.

A pipe dream would be to turn this into a full-blown computer algebra system.
That would probable consume my life.
