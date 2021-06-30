# Core Library

**This folder contains Hasky (`*.ha`) files that contain Hasky standard library
and are available to every Hasky program.**

```rs
use core:io as io;
```

Hasky build system (invoked with `hasky build`) is going to compile files
within the following folders:

- `src`
- `lib`
- `test`\*

> \*`test` folder may be supported in the future, not straight away.
