# Contributing to qtkit development

The goal of this guide is to help you get up and contributing to qtkit as
quickly as possible. The guide is divided into two main pieces:

1. Filing a bug report or feature request in an issue.
1. Suggesting a change via a pull request.

Please note that qtkit is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Issues

When filing an issue, the most important thing is to include a minimal
reproducible example so that we can quickly verify the problem, and then figure
out how to fix it. There are three things you need to include to make your
example reproducible: required packages, data, code.

1.  **Packages** should be loaded at the top of the script, so it's easy to
    see which ones the example needs.

1.  The easiest way to include **data** is to use `dput()` to generate the R code
    to recreate it. For example, to recreate the `mtcars` dataset in R,
    I'd perform the following steps:

    1.  Run `dput(mtcars)` in R
    2.  Copy the output
    3.  In my reproducible script, type `mtcars <- ` then paste.

    But even better is if you can create a `data.frame()` with just a handful
    of rows and columns that still illustrates the problem.

1.  Spend a little bit of time ensuring that your **code** is easy for others to
    read:

    - make sure you've used spaces and your variable names are concise, but
      informative

    - use comments to indicate where your problem lies

    - do your best to remove everything that is not related to the problem.
      The shorter your code is, the easier it is to understand.

You can check you have actually made a reproducible example by starting up a
fresh R session and pasting your script in.

(Unless you've been specifically asked for it, please don't include the output
of `sessionInfo()`.)

## Pull requests

To contribute a change to qtkit, you follow these steps:

1. Create a branch in git and make your changes.
1. Push branch to github and issue pull request (PR).
1. Discuss the pull request.
1. Iterate until either we accept the PR or decide that it's not
   a good fit for qtkit.

Each of these steps are described in more detail below. If you're not familiar
with git or github, please start by reading:
<https://r-pkgs.org/software-development-practices.html>

Pull requests will be evaluated against a seven point checklist:

1.  **Motivation**. Your pull request should clearly and concisely motivate the
    need for change. Unfortunately neither Winston nor I have much time to
    work on qtkit these days, so you need to describe the problem and show
    how your pull request solves it as concisely as possible.

    Also include this motivation in `NEWS` so that when a new release of
    qtkit comes out it's easy for users to see what's changed. Add your
    item at the top of the file and use markdown for formatting. The
    news item should end with `(@yourGithubUsername, #the_issue_number)`.

1.  **Only related changes**. Before you submit your pull request, please
    check to make sure that you haven't accidentally included any unrelated
    changes. These make it harder to see exactly what's changed, and to
    evaluate any unexpected side effects.

    Each PR corresponds to a git branch, so if you expect to submit
    multiple changes make sure to create multiple branches. If you have
    multiple changes that depend on each other, start with the first one
    and don't submit any others until the first one has been processed.

1.  **Use tidyverse coding style**. Please follow the
    [official tidyverse style](https://style.tidyverse.org). Maintaining
    a consistent style across the whole code base makes it much easier to
    jump into the code. If you're modifying existing qtkit code that
    doesn't follow the style guide, a separate pull request to fix the
    style would be greatly appreciated.

1.  If you're adding new parameters or a new function, you'll also need
    to document them with [roxygen2](https://github.com/r-lib/roxygen2).
    Make sure to re-run `devtools::document()` on the code before submitting.

1.  If fixing a bug or adding a new feature to a non-graphical function,
    please add a [testthat](https://github.com/r-lib/testthat) unit test.

1.  If fixing a bug in the visual output, please add a visual test.
    (Instructions to follow soon)

1.  If you're adding a new graphical feature, please add a short example
    to the appropriate function.

This seems like a lot of work but don't worry if your pull request isn't perfect.
It's a learning process and members of the qtkit team will be on hand to help you
out. A pull request ("PR") is a process, and unless you've submitted a few in the
past it's unlikely that your pull request will be accepted as is. All PRs require
review and approval from at least one member of the qtkit development team
before merge.
