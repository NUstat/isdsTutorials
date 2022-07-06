# ISDStutorials

The ISDStutorials package contains a series of interactive tutorials that teach alongside chapters of the Introduction to Statistics and Data Science webbook. The tutorials are written using a package called learnr. Once a tutorial is running it's a bit like reading a book but with places where you can practice the R code that you have just been taught. The ISDStutorials package is free (as are all things -related) and offered to support teachers and students using the textbook who want to learn R.

# Installation

Install the latest version of ISDStutorials from GitHub with the remotes package.

```{r}
# install.packages("remotes")
# remotes::install_github("rstudio/gradethis")
remotes::install_github("NUstat/ISDStutorials", dependencies = TRUE)
```

# Contents of ISDStutorials

The tutorials are named to correspond to the relevant chapter of the book. For example, lesson_03 would be a good tutorial to run alongside teaching related to chapter 3, and so on.

**lesson_01:** Introduction

**lesson_02a:** Scatterplots

**lesson_02b:** Line

**lesson_03a:** Data wrangling

**lesson_302:** Quick intro to R for experienced programmers. (not covered in the book)

# Running tutorials

In  Version 1.3 onwards there is a tutorial pane. Having executed

library(ISDStutorials)

A list of tutorials appears in this pane: ....

# Additional features

- Print option
- Grade option
- Question short answer
- Question drop down

To use ISDStutorials custom functions in a learnr tutorial, start by loading ISDStutorials after learnr and gradethis in the setup chunk of your tutorial:

```{r setup}
library(learnr)
library(gradethis)
library(ISDStutorials)
```
