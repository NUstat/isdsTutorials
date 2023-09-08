# isdsTutorials

The isdsTutorials package contains a series of interactive tutorials that teach alongside chapters of the free Introduction to Statistics and Data Science [textbook](https://nustat.github.io/intro-stat-ds/). The tutorials are written using a package called learnr. Once a tutorial is running it's a bit like reading a book but with places where you can practice the R code that you have just been taught along with multiple choice comprehension check questions. The isdsTutorials package is free and offered to support teachers and students using the textbook who want to learn R.

# Installation

Install the latest version of isdsTutorials from GitHub with the remotes package.

```{r}
# install.packages("remotes")
remotes::install_github("NUstat/isdsTutorials", dependencies = TRUE)
```

# List of tutorials

The tutorials are named sequentially and correspond to the recommended material covered in a single class. The relevant sections of the Introduction to Statistics and Data Science [textbook](https://nustat.github.io/intro-stat-ds/) are listed alongside each tutorial.

| name               | content             |     | name                    | content           |                    |                   |     |     |     |     |     |     |     |     |     |     |
|------------|------------|------------|------------|------------|------------|------------|------------|------------|------------|------------|------------|------------|------------|------------|--|--|
| **01_intro**       | Preface & Chapter 1 |     | **11_regression4**      | Chapter 6.2 - 6.4 |                    |                   |     |     |     |     |     |     |     |     |     |     |
| **02_ggplot1**     | Chapter 2.0 - 2.3   |     | **12_randomization**    | Chapter 7         |                    |                   |     |     |     |     |     |     |     |     |     |     |
| **03_ggplot2**     | Chapter 2.4 - 2.6   |     | **13_generalizability** | Chapter 8         |                    |                   |     |     |     |     |     |     |     |     |     |     |
| **04_ggplot3**     | Chapter 2.7 - 2.9   |     | **sample_exam2**        | Sample Exam 2     |                    |                   |     |     |     |     |     |     |     |     |     |     |
| **05_wrangling1**  | Chapter 3.0 - 3.3   |     | **14_sampling1**        | Chapter 9.0 - 9.1 | **15_sampling2**   | Chapter 9.2 - 9.3 |     |     |     |     |     |     |     |     |     |     |
| **06_wrangling2**  | Chapter 3.4 - 3.6   |     | **15_sampling2**        | Chapter 9.2 - 9.3 | **16_sampling3**   | Chapter 9.4 - 9.6 |     |     |     |     |     |     |     |     |     |     |
| **07_tidy**        | Chapter 4           |     | **16_sampling3**        | Chapter 9.4 - 9.6 | **17_ci**          | Chapter 10        |     |     |     |     |     |     |     |     |     |     |
| **sample_exam1**   | Sample Exam 1       |     | **17_ci**               | Chapter 10        |                    |                   |     |     |     |     |     |     |     |     |     |     |
| **08_regression1** | Chapter 5.0 - 5.1   |     | **18_pvalues**          | Chapter 11        |                    |                   |     |     |     |     |     |     |     |     |     |     |
| **09_regression2** | Chapter 5.2 - 5.4   |     | **19_hypothesis**       | Chapter 12        |                    |                   |     |     |     |     |     |     |     |     |     |     |
| **10_regression3** | Chapter 6.2 - 6.4   |     | **sample_exam3**        | Sample Exam 3     | **isds_functions** | Educator Resource |     |     |     |     |     |     |     |     |     |     |

# Running tutorials

There are two ways to run the tutorials. The recommended way to run a tutorial is to type the following line in the R console:

`learnr::run_tutorial("01_intro", package = "isdsTutorials")`

This should bring up a tutorial in your default web browser. You can see the full list of tutorials by running:

`learnr::run_tutorial(package = "isdsTutorials")`

Alternatively, in Version 1.3 onwards after having executed `library(isdsTutorials)`, a list of tutorials appears in a tutorial tab (by default it will be in the upper-right pane). However, the print option is not executable if the tutorial is run through the tutorial tab.

# Submitting tutorials

After completing each tutorial, students can obtain their grade in html format. Students can then upload the html grade to a learning management system like Canvas or Gradescope.

# Acknowledgments

This work was made possible through funding from the Alumnae of Northwestern University grant and the Open Educational Resources (OER) grant.

# Citations

Aden-Buie G, Chen D, Grolemund G, Rossell Hayes A, Schloerke B (2023). *gradethis: Automated Feedback for Student Exercises in 'learnr' Tutorials*. <https://pkgs.rstudio.com/gradethis/,> <https://rstudio.github.io/learnr/,> <https://github.com/rstudio/gradethis.>

Aden-Buie G, Schloerke B, Allaire J, Rossell Hayes A (2023). *learnr: Interactive Tutorials for R*. https://rstudio.github.io/learnr/, https://github.com/rstudio/learnr.

Sass D (2023). *tutorialExtras: Custom questions and exam functions for learnr tutorials*. R package version 0.0.0.9000, [https://NUstat.github.io/tutorialExtras/](https://nustat.github.io/tutorialExtras/).

Tipton, E., Kuyper, A. M., Sass, D. K., Fitzgerald, K. G., Ismay, C., & Kim, A. (2020). \"Introduction to Statistics and Data Science\". Northwestern Libraries Digital Publishing, <https://nustat.github.io/intro-stat-data-sci>
