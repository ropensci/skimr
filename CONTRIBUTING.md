# Contributing

Contributions to `skimr` whether in the form of bug fixes, issue reports, new code or documentation improvement
are welcome.  Please use the github issue tracker. For any pull request please like to or open a corresponding issue 
in the issue tracker.   Please ensure that you have notifications turned and respond to questions, comments or 
needed changes promptly.

## Understanding the scope of skimr

Skimr solves a very specific set of problems focused on the compact, flexible and useful display of summary data
in the console.  By itself it is not intended as a replacement for packages that create publication ready tables.
The basic concept is that of "skimming" a data frame or tibble to get an overview of the data it contains. One
intended group of users is students in a first semester statistics class.  As such, the package is focused on 
data types that are widely used.   Fortunately, the `skim()` function is a generic which means that rather than
add to the core package you can write your own implementation for specialized classes of data and for objects 
that are not dataframes.  Please read the vignette "Supporting additional objects" that provides documentation
of how to do this. 

##  Tests

Skimr uses testthat for testing. Please try to provide 100% test coverage for any submitted code and always check 
that existing tests continue to pass.  If you are a beginner and need help with writing a test, mention this
in the issue and we will try to help.

## Pull requests

Pull requests should be against the _develop_ branch not the master branch.  You can set this when creating 
your pull request.   Please make a separately named branch to submit.  Keep each branch for a complete specific
issue.  If you create a pull request by editing in the github web editor and you end up with multiple pull
requests, note that in your issue comments.

## Code style

Please use snake case (such as skim_v)  for function names.  Besides that, in general follow the Google code 
style for R.



## Code of Conduct

When contributing to `skimr` you must follow the code of conduct defined in CONDUCT.md
