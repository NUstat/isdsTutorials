# ISDStutorials

The ISDStutorials package contains a series of interactive tutorials that teach alongside chapters of the free Introduction to Statistics and Data Science [textbook](https://nustat.github.io/intro-stat-ds/). The tutorials are written using a package called learnr. Once a tutorial is running it's a bit like reading a book but with places where you can practice the R code that you have just been taught along with multiple choice comprehension check questions. The ISDStutorials package is free and offered to support teachers and students using the textbook who want to learn R.

# Installation

Install the latest version of ISDStutorials from GitHub with the remotes package.

```{r}
# install.packages("remotes")
# remotes::install_github("rstudio/gradethis")
remotes::install_github("NUstat/ISDStutorials", dependencies = TRUE)
```

# List of tutorials

The tutorials are named sequentially and correspond to the recommended material covered in a single class. The relevant sections of the Introduction to Statistics and Data Science [textbook](https://nustat.github.io/intro-stat-ds/) are listed alongside each tutorial.

| name     | content |     | name     | content |
| ---      | ---       | ---   | ---      | ---       |
| **01_intro** | Preface & Chapter 1 |     |**11_regression4** | Chapter 6.2 - 6.4 |
| **02_ggplot1** | Chapter 2.0 - 2.3 |     |**12_randomization** | Chapter 7 |
| **03_ggplot2** | Chapter 2.4 - 2.6 |     |**13_generalizability** | Chapter 8 |
| **04_ggplot3** | Chapter 2.7 - 2.9 |     |**14_sampling1** | Chapter 9.0 - 9.1 |
| **05_wrangling1** | Chapter 3.0 - 3.3 |     |**15_sampling2** | Chapter 9.2 - 9.3 |
| **06_wrangling2** | Chapter 3.4 - 3.6 |     |**16_sampling3** | Chapter 9.4 - 9.6 |
| **07_tidy** | Chapter 4 |     |**17_ci** | Chapter 10 |
| **08_regression1** | Chapter 5.0 - 5.1 |     |**18_pvalues** | Chapter 11 |
| **09_regression2** | Chapter 5.2 - 5.4 |     |**19_hypothesis** | Chapter 12 |
| **10_regression3** | Chapter 6.2 - 6.4 |     |**20_conclusion** | Chapter 13 |


# Running tutorials

There are two ways to run the tutorials. The recommended way to run a tutorial is to type the following line in the R console:

`learnr::run_tutorial("01_intro", package = "ISDStutorials")`

This should bring up a tutorial in your default web browser. You can see the full list of tutorials by running:

`learnr::run_tutorial(package = "ISDStutorials")`

Alternatively, in Version 1.3 onwards after having executed `library(ISDStutorials)`, a list of tutorials appears in a tutorial tab (by default it will be in the upper-right pane). However, the print option is not executable if the tutorial is run through the tutorial tab.

# Submitting tutorials

After completing each tutorial, students can obtain their grade and download the full tutorial to a pdf. Students can then upload these PDFs to a learning management system like Canvas or Gradescope.

# Additional features of ISDStutorials

- Print option: `print_ui`
- Grade option: `grade_server`, `grade_button_ui`, `grade_output_ui`
- Question fill in the blanks: `question_blank`
- Question wordbank: `question_wordbank`
- Question reorder matching: `question_matching`
- Question multiple drop downs: `question_multidrop`

# ISDS setup

To use ISDStutorials custom grade and print functions in one of your own learnr tutorials, start by loading ISDStutorials after learnr and gradethis in the setup chunk of your tutorial and adding the `isds_recorder` to the options:

```{r}
library(learnr)
library(gradethis)
library(ISDStutorials)

gradethis_setup()

options(tutorial.event_recorder = ISDStutorials::isds_recorder)
```

Next, add the `grade_server` function to a code chunk of type `context = "server"`

```{r}
# must supply a data.frame consisting of "question" and "points_possible" to rubric_list.
# the question/exercise name is the code chunk name followed by a number, ignoring all spaces and/or "-" symbols
rubric_list <- data.frame(question = c("Ex1", "Q1", "Q2", "Q3"),
                          points_possible = rep(1, 4))

grade_server("grade_out", 
            rubric_list = rubric_list, 
            num_try = 3, deduction = 0.1, display = c("scaled", "itemize") )
```

Finally add your desired header components. The following adds a name, grade button, print button, and grade output.

```{r}
# student name
question_blank("Name: ___",
               answer_fn(function(value){
                              if (length(value) >= 1 ) { return(mark_as(TRUE)) }
                              return(mark_as(FALSE) )
                              }), 
                style = "notes_question",
                correct = paste0(fontawesome::fa("check") ),
                incorrect = paste0(fontawesome::fa("xmark") ),
                allow_retry = FALSE )
# grade button and print button on same line as name
bootstrapPage(
     div(style="display:inline-block",
         grade_button_ui("grade_out") ),
     div(style="display:inline-block", print_ui("Print") )
)

# can have the grade output appear anywhere in your tutorial
grade_output_ui("grade_out")
```

# Acknowledgments

This work was made possible through funding from the Alumnae of Northwestern University grant and the Open Educational Resources (OER) grant.
