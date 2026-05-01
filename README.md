# cs6868-project
**[This is the link to our video presentation](https://drive.google.com/file/d/1XzJTMnCCx8zRpE2Y319rWFGdfUBQl78U/view?usp=drive_link)**
## Repository Structure
- The source code is present in the src directory
- The `src/test` directory contains various tests(lin,stm,concurrent,sequential).
- The `src/lib/benchmarks` directory has benchmarking and plotting scripts.
- The `src/lib` directory has the implementations of both the universal constructions, consensus object, node, mutex-protected sequential implementations and purpose-built lock-free implementations of stack and queue
- The `src/lib/sequential` directory contains the sequential versions of BST,Stack,Queue, Skip List, Sorted List which can be passed to the universal constructions.

## Configuration required to run the code
- Opam version 5.4.0 or above
- Dune version 3.21 or above

## Steps to run the code
- Use 