# RunTime System

**This folder contains Hasky runtime.**

Here is Hasky's compilation pipeline:

```
*-------------------*
| HASKY CODE (*.ha) |-------------------------------------*
*-------------------*                                     |
                                                          |
*-------------------*<-- ( Hasky Compiler ) --------------*
| C++ CODE (*.cpp)  |
*-------------------*-- ( Wrap Into Runtime ) ------------*
                                                          |
*---------------------------------*<-- ( C++ Compiler ) --*
| BINARY FILE (*.out/*.exe/*.app) |
*---------------------------------*
```

RTS comes in at the `Wrap Into Runtime` step.
