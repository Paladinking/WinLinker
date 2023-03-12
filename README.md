# WinLinker
Exploring creating and inspecting windows executables

Todo:
- 64-bit immediate values can cause compilation failure if no register is available since mov instruction
    cannot move 64-bit values directly to memory.  
- Immediate values should be downscaled as much as possible to save space.