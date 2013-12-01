# Overview

This is a simple library consisting of scheme code to compute elementary functions.

The installation is simple, just load it. There are no dependencies.

It's coded for the MIT-Scheme, so some of it might not be platform independent (mostly the `infinite?` and `:+inf.*` functions might cause problems).

This is released under the MIT License. There's nothing novel or exciting about the calculator, it's just written from scratch.

## Known Issues

It takes a minute to initialize, because I'm using rational approximations for pi, e, etc.

On the other hand, these constants are precise to 50+ digits.
