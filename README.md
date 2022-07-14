# ISDStutorials

The ISDStutorials package contains a series of interactive tutorials that teach alongside chapters of the free Introduction to Statistics and Data Science [textbook](https://nustat.github.io/intro-stat-ds/). The tutorials are written using a package called learnr. Once a tutorial is running it's a bit like reading a book but with places where you can practice the R code that you have just been taught along with multiple choice comprehension check questions. The ISDStutorials package is free and offered to support teachers and students using the textbook who want to learn R.

# Installation

Install the latest version of ISDStutorials from GitHub with the remotes package.

```{r}
# install.packages("remotes")
# remotes::install_github("rstudio/gradethis")
remotes::install_github("NUstat/ISDStutorials", dependencies = TRUE)
```

# Contents of ISDStutorials

The tutorials are named sequentially and correspond to the recommended material covered in a single class. The relevant sections of the Introduction to Statistics and Data Science [textbook](https://nustat.github.io/intro-stat-ds/) are listed alongside each tutorial.

**01_intro:** Preface & Chapter 1

**02_ggplot1:** Chapter 2.0 - 2.3

**03_ggplot2:** Chapter 2.4 - 2.6

**04_ggplot3:** Chapter 2.7 - 2.9
**05_wrangling1:** Chapter 3.0 - 3.3
**06_wrangling2:** Chapter 3.0 - 3.3

# Running tutorials

There are two ways to run the tutorials. The recommended way to run a tutorial is to type the following line in the R console:

learnr::run_tutorial("01_intro", package = "ISDStutorials")

This should bring up a tutorial in your default web browser. You can see the full list of tutorials by running:

learnr::run_tutorial(package = "ISDStutorials")

Alternatively, in Version 1.3 onwards after having executed `library(ISDStutorials)`, a list of tutorials appears in a tutorial tab (by default it will be in the upper-right pane). However, the print option is not executable if the tutorial is run through the tutorial tab.

# Submitting tutorials

After completing each tutorial, students can obtain their grade and download the full tutorial to a pdf. Students can then upload these PDFs to a learning management system like Canvas or Gradescope.

# Additional features of ISDStutorials

- Print option
- Grade option
- Question short answer
- Question drop down
- Question multiple choice

# ISDS setup

To use ISDStutorials custom grade and print functions in one of your own learnr tutorials, start by loading ISDStutorials after learnr and gradethis in the setup chunk of your tutorial and adding the `isds_recorder` to the options:

```{r setup}
library(learnr)
library(gradethis)
library(ISDStutorials)

gradethis_setup()

options(tutorial.event_recorder = ISDStutorials::isds_recorder)
```

Next, add the `grade_server` function to a code chunk of type `context = "server"`
```{r, context = "server"}
# must supply a data.frame consisting of "question" and "points_possible" to rubric_list.
# the question/exercise name is the code chunk name followed by a number, ignoring all spaces and/or "-" symbols
rubric_list <- data.frame(question = c("Ex1", "Q1", "Q2", "Q3"),
                          points_possible = rep(1, 4))

grade_server("grade_out", 
            rubric_list = rubric_list, 
            num_try = 3, deduction = 0.1, display = c("percent", "itemize") )
```

Finally add your desired header components. The following adds a name, grade button, print button, and grade output.
```{r isds-header}
# student name
question_blank("Name: ___",
               answer_fn(function(value){
                              if (length(value) >= 1 ) { return(mark_as(TRUE)) }
                              return(mark_as(FALSE) )
                              }), allow_retry = FALSE )
# grade button and print button on same line as name
bootstrapPage(
     div(style="display:inline-block",
         grade_button_ui("grade_out") ),
     div(style="display:inline-block", print_ui("Print") )
)

# can have the grade output appear anywhere in your tutorial
grade_output_ui("grade_out")
```
