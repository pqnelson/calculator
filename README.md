# Overview

This is a simple library consisting of scheme code to compute elementary 
functions.

The installation is simple, just load it. There are no dependencies.

It's coded for the MIT-Scheme, so some of it might not be platform independent 
(mostly the `infinite?` and `:+inf.*` functions might cause problems).

This is released under the MIT License. There's nothing novel or exciting about 
the calculator, it's just written from scratch.

## Known Issues

It takes a minute to initialize, because I'm using rational approximations for 
`:pi`, `:e`, etc.

On the other hand, these constants are precise to 50+ digits.

## Future Directions

It would be nice to refactor the code into self-contained files. This would
be the first thing on my "to do" list.

Well, since we haven't considered special functions, that might be a direction.

It would be nice to speed up the initial load.

A pipe dream would be to turn this into a full-blown computer algebra system.
That would probable consume my life.
