# Contributing

Contributions to `skimr` whether in the form of bug fixes, issue reports, new
code or documentation improvement are welcome. Please use the github issue
tracker. For any pull request please link to or open a corresponding issue in
the issue tracker. Please ensure that you have notifications turned on and
respond to questions, comments or needed changes promptly.

## Understanding the scope of skimr

`skimr` solves a very specific set of problems focused on the compact, flexible
and useful display of summary data in the console. By itself it is not intended
as a replacement for packages that create publication ready tables. The basic
concept is that of "skimming" a data frame or tibble to get an overview of the
data it contains.

One intended group of users is students in a first semester statistics class. As
such, the package is focused on data types that are widely used. One general
guideline is that if a data type is not found in the `datasets` package it will
not be directly supported in `skimr`. Fortunately, `skim()` has a generic
internal function for handling a variety of data types `get_skimmers()`. See the
documentation for that function or the vignette "Supporting additional objects"
for documentation on how to do this.

Similarly, `skimr` is deeply tied to the `tidyverse` and `dplyr` in particular.
The comes with a lot of benefits, but some constraints too. Most importantly,
data processed by `skim()` needs to be an object that inherits from a data frame
or in a form that can be coerced to a data frame.

## Tests

`skimr` uses `testthat` for testing. Please try to provide 100% test coverage
for any submitted code and always check that existing tests continue to pass. If
you are a beginner and need help with writing a test, mention this in the issue
and we will try to help.

## Pull requests

Pull requests should be against the _develop_ branch not the master branch. You
can set this when creating your pull request. Please make a separately named
branch to submit. Keep each branch for a complete specific issue. If you create
a pull request by editing in the GitHub web editor and you end up with multiple
pull requests, note that in your issue comments.

## Code style

We follow the [tidyverse style guide](http://style.tidyverse.org/).

## Pre commits

To enforce coding style and support development, we rely on [pre-commit.com],
and the [R precommit package](https://github.com/lorenzwalthert/precommit). This
tool runs a series of additional checks for your code before `git commit`
completes.

To install the package and enable precommits, run the following:

```
# once on your system
remotes::install_github("lorenzwalthert/precommit")
precommit::install_precommit()

# once in every git repo either
# * after cloning a repo that already uses pre-commit or
# * if you want introduce pre-commit to this repo
precommit::use_precommit()
```

The checks will run automatically from there.

## Code of Conduct

When contributing to `skimr` you must follow the [code of conduct defined by rOpenSci](https://ropensci.org/code-of-conduct/).
