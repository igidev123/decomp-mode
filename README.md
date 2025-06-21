= decomp-mode
A minor mode for emacs to help with decompilation projects. Inspired by [decomp.me](https://decomp.me/).

This is work in progress

== Usage
- Open `diff.el` and `decomp-mode.el`.
- Eval both files by calling `eval-buffer`

- Place a source file (e.g. `base.c`) and a target assembly file (called `target.s`) in your working directory.
- Write a `compile.sh` script that will compile your source file when called as `./compile.sh <source.c> -o <out.o> -g`
- The disassembly is done by calling `arm-none-eabi-objdump -l --no-show-raw-insn -d <out.o> > <out.s>`. Edit `decomp-mode.el` if you use something else.

Once the setup is done, you can compile the source file wih `c-assembly-compile-and-show` (bound to `C-c C-a`), and jump to the assembly corresponding to the currently selected line with `c-assembly-jump-to-line` (bound to `C-c C-j`).
