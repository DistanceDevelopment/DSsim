DSsim
=====

Distance Sampling Simulation R Package

DSsim is written using S4 R programming. 

FILE STRUCTURE
--------------

There are 2 special .R files. “Class.Constructors.R” and “generic.functions.R”.

“Class.Constructors.R” contains the user interface methods to be used for constructing classes. 

“generic.functions.R” uses setGeneric to create the generic function definitions to be used for one or more classes in DSsim. Generic function names are not capitalized. Plot, show, summary and print are only defined if they do not currently exist in the R environment.

CLASSES

Class names are capitalized. Classes are stored in their appropriately named .R file. E.g. the class Region would be defined in “Region.R”. In addition to the class definition these files also contain any class validation methods and the definition of the generic functions

[currently accessor functions are created as generic functions and specified in the relevant class file (there are only a few). However, this does not make sense and it would be better to move these to a “generic.accessor.functions.R” file.]
 
FUNCTIONS

Ordinary (non-generic) functions are contained within the appropriately named .R file. E.g. the method calc.area would be written in the file “calc.area.R”. These are not capitalized.


DOCUMENTATION
-------------

The documentation can be generated using roxygen2 but depending on the version of roxygen2 it may be necessary to manually remove the \alias{plot}, \alias{show}, \alias{summary} and \alias{print} lines from the .Rd files for the classes. Apparently the developers version does not include these in the first place but the version currently on CRAN does. 

The classes defined in DSsim all have their own .Rd files named “Class-class.Rd”.

The generic functions defined in DSsim all give function descriptions stored in the relevant .Rd file. E.g. the generic function generate.population documentation is found in “generate.population-methods.Rd”. Each time this method is defined for a new class it should point to this file using the @rdname in the roxygen code. 

The exceptions to the above are the generic plot, show, print and summary methods as we wish these to look for the documentation written when their definitions were first created. The documentation for these is therefore store in the class is it defined for. In this case the @rdname should be “Class-class.Rd”. It is in this case that you have to be careful that the \alias{show} etc. is not included in the “Class-class.Rd”  file.

Each standard (non-generic) function has a .Rd file of the same name as the function if it is documented. E.g. the documentation for make.region is found in “make.region.Rd”.
