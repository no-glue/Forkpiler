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

## Running the Compiler
Just ./Main <file to compile> in the directory you ran ghc. Make sure the file is in the same folder.

# Assignment 1
All of the test files and the test writeup are in the assignment 1 folder. All of the working code is currently on branch master.
