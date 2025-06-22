# Why?
- decomp.me doesn't work offline
- It sometimes struggles with large functions

- A shell-script/makefile based workflow isn't convenient
  - Doesn't link source file and diff output
  
- I already use emacs for code editing

# Desired features
- Quickly recompile and compare compiled and target assembly
  - Partial: diff output, but ugly
    - Should use two windows, to allow resizing and horizontal scrolling
  - The diff is too slow (about 5s for 1500 lines of asm)
  
- Jump between source and compiled assembly
  - Partial: Only jumps from C to asm, not the reverse
    - Could be improved with highlighting
    
- Others
  - Follow register allocation and jumps. (not implemented)
  
  
# Things I tried
## Diff optimizations
The `shortest-edit` function is clearly the biggest source of slowness (~80% of
the execution time)
- Using singly-linked lists
  - Requires retracing the chain for each iteration. ~50% of the execution is 
    spent in calls to 'nth
- Using doubly-linked lists
  - Should require less seeking through the chains, since neighbouring cells in 
    v usually have close x values.
  - Disappointing result. Saved ~10% in the execution time. The execution time
    is a lot more scattered within the loop.
    
- Compiling `diff.el` and `dll.el`
  - Saved ~40% of the execution time compared to the interpreted version.
