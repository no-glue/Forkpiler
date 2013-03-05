# Read Me!

## Installing the Haskell Platform
You can find it [here](http://www.haskell.org/platform/).
It should be easy to install and run. To make sure it's installed run the command ghci. If you have issues add /Library/Haskell/bin to your path. 

## Installing Rexeg.PCRE
To run lexer you need the better regex. To install the better regex run: 
	cabal update
	cabal install regex-pcre-builtin
You might get a bunch of warnings but it should install

## Compiling the Compiler :)
Navigate to the directory in which all the .hs files are and run ghc --make Main.hs. Shouldn't take more than a couple seconds.

Or! To be super cool just run cabal install in the root directory. This
will build everything cleanly and make it so you can run Forkpiler <filename>
instead of having to use ./Main in the right folder and all that stuff.

## Running the Compiler
If you used ghc --make ./Main <file to compile> in the directory you ran ghc. Make sure the file is in the same folder.

If you used cabal install run it as Forkpiler <file to compiler> instead. This
can be done in any folder on your computer. 

# Assignment 1
All of the test files and the test writeup are in the assignment 1 folder. All of the working code is currently on branch assignment1. Just pull the repo and checkout assignment1 and everything will show up.
