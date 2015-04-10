## ** Modula-2 Revision 2010 ** ##

### M2 R10 Repository ###

Welcome to the design and development repository of Modula-2 Revision 2010,
its language report, reference compiler and standard library.

Latest news:

* 2015-04-05: Work on a bootstrap compiler has begun.
* 2015-03-31: The language design has been finalised.
* 2015-02-15: Fund raising efforts have begun.

Quick links:

** o  Finalised Grammar (up-to-date) **

https://bitbucket.org/trijezdci/m2r10/downloads/SyntaxDiagrams.pdf

** o  Technical Specification (needs updating) **

https://bitbucket.org/trijezdci/m2r10/downloads/M2R10.2014-01-31.tracked.pdf

** o  Source Tree (needs updating) **

https://bitbucket.org/trijezdci/m2r10/src

** o  Library Definition Modules **

https://bitbucket.org/trijezdci/m2r10/src/tip/_STANDARD_LIBRARY

** o  Summary of Changes **

https://bitbucket.org/trijezdci/m2r10/downloads/M2R10.Changes.Info.txt

** o  Design Journal **

http://knightsoftype.blogspot.com


### Donations ###

Please help funding the Modula-2 R10 development work by making a donation.

Our [donation page](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=QA4WRY9TW7GT4) is hosted at Paypal.com.


### About Classic Modula-2 ###

Modula-2 is a strongly typed, modular, imperative programming language derived from
Xerox' Mesa language. It was published in 1978 at the Swiss Federal Institute of Technology
in Zurich (ETHZ) by Prof. Niklaus Wirth as a successor to his earlier language Pascal. 
More information about the classic Modula-2 language and its history can be found at:

http://en.wikipedia.org/wiki/Modula-2


### About Modula-2 Revision 2010 ###

Modula-2 R10 is a modern revision of N.Wirth's Modula-2 language undertaken by
B.Kowarsch  and R.Sutcliffe  in 2009  and 2010.  A pragma system  was added in
in 2011 and 2012.  The Design was  refined and polished in 2013.  Extensive work
was done on blueprints and templates for the standard library in 2014 and 2015.
The language report is under editorial review and will be published in a book in 2016.

The primary design goals of the revision were type safety,  utmost readability
and consistency,  and  suitability  as a  core language   for domain  specific
supersets.   Targeted  areas   of  application   are  systems  implementation,
engineering and mathematics.  Some inspiration was taken from Ada and Oberon.

A particular strength  of the design  is  a set of facilities  to make library
defined abstract data types  practically indistinguishable from built-in types
and thereby eliminate one of the major causes of feature growth.


### Compiler Support ###

A reference compiler  for Modula-2 R10 has been  under development  since 2010
but work had been suspended until the design is finalised. Initially, the compiler will
generate C99 source code,  and eventually it will generate LLVM IR.

Work on a bootstrap compiler has begun in April 2015.

The developer of GNU Modula-2 has pledged to add support for Modula-2 R10 in
GM2. The GM2 compiler is a Modula-2 front-end for GCC.


### Development Schedule ###

This project is a  *private*  and  *self-funded*  effort by the authors who are
doing this work in their own *spare* *time*.  Further, the authors believe that
quality design  and proper specification are prerequisites for a quality
implementation and cannot be rushed.  Work on a bootstrap compiler
is now under way but there is *no fixed schedule* for the completion of the compiler.

Please consider [making a donation](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=QA4WRY9TW7GT4) to speed up the development work.


[Status: April 10, 2015]