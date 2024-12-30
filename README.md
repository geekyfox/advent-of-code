Advent of Code
--------------

This repository contains my solutions for
https://adventofcode.com/ puzzles.

Self-imposed rules to be aware of:

1. **Haskell.**
  
Professionally, I use Rust, Python and (very occasionally) JavaScript.
That's why my go-to languages for hobby programming are C and
Haskell... And that actually makes a lot of sense: in my spare time, I
want to do something that expands my consciousness and helps me see
programming from unusual angles rather than "more of the same, but now
on a private computer." I dabbled with using C for AoC 2024... But then
I decided that solving one of those puzzles every morning is a big
enough commitment by itself, without an additional handicap of DYI-ing
containers and hunting segfaults, although I might still re-solve them
all in C at a later time.

2. **Vanilla GHC, no third-party packages.**

No need to explain here.
   
3. ***"Works for my dataset."***

A significant part of the fun with AoC problems is that input data
often contains "undocumented regularities." This means that according
to the data, you can make certain assumptions and take certain
convenient shortcuts, but the problem statement doesn't explicitly
confirm (or deny) that you can. So, I didn't intentionally overfit the
solutions to pass for my personal input datasets, but I'm also not 110%
sure I didn't take a "one shortcut too many" somewhere. If I did,
fixing that is left as an exercise for the reader.

4. **Performance should be decent.**

The threshold is that every solution should complete in less than a
second on my crappy old laptop. The only allowed exception is if I
defatted the algorithm down to matrix crunching, and it's still above
one second. In this case, accept that GHC-compiled Haskell is not the
greatest matrix cruncher out there and move on.

5. **The code should be decently clean.**

The threshold of "decently clean" is that it should be readable for a
competent Rust/Java/C++ developer curious about what this Haskell thing
is all about. Convincing crowds such as "static typing and immutability
are bureaucratic hurdles for my unrestrained creativity," "everything
since COBOL-74 was a mistake," and "I'm married to NodeJS, why would I
ever bother to look at anything else" is out of my intentions.

