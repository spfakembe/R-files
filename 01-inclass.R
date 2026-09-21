#--------------------------------------------------
# Week 1: In-class assignment
#--------------------------------------------------

# There is no one correct way to write the code to answer the questions
# But your code needs to 
# a. answer the question
# b. be fully reproducible

# For this assignment, we will use 
# the `yrbss` data 
# in the `openintro` package 

install.packages("openintro")
library(openintro)

# Other useful packages
install.packages("tidyverse")
library(tidyverse)

# Read the documentation for `yrbss` to learn about all the variables.
?yrbss

# The code below uses the `flextable` package to create a table of summary characteristics of
# Grade and Gender
# Modify the code below such that the grade shows in increasing order
# and all category labels start with a capital letter

install.packages("flextable")
library(flextable)

yrbss$Grade <- yrbss$grade
yrbss$Gender <- yrbss$gender

# Capitalize Gender labels
 yrbss$Gender <- tools::toTitleCase(yrbss$Gender)

# Build ordered Grade factor
grade_f <- yrbss$Grade

# Extract numeric grades
grade_num <- sort(unique(suppressWarnings(as.numeric(grade_f))))
grade_num <- grade_num[!is.na(grade_num)]

# Define order: numeric grades, then "other", then "missing"
grade_levels <- c(as.character(grade_num), "other", "missing")

yrbss$Grade <- factor(grade_f, levels = grade_levels, ordered = TRUE)

z <- summarizor(
  yrbss[c("Grade", "Gender")],
  overall_label = NULL
)

ft_1 <- as_flextable(z) 
ft_1


# To understand the pattern of physical activity by grade and gender,
# 1) aggregate  `physically_active_7d` by calculating its mean within each grade and gender
# 2) create a plot showing the average number of physically active days
#      x-axis: grade
#      y-axis: Mean of `physcially_active_7d`
#      Distinguish gender using different colors, symbols, or lines
# *** I would use the following functions: aggregate(), ggplot(), geom_line() but there is 
# no one correct way to do this
# Ensure that the figure is clearly labeled and includes an appropriate legend

activity_summary <- aggregate(
  physically_active_7d ~ Grade + Gender,
  data = yrbss,
  FUN = mean,
  na.rm = TRUE
)

library(ggplot2)

ggplot(activity_summary,
       aes(x = Grade,
           y = physically_active_7d,
           color = Gender,
           group = Gender)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  
  labs(
    title = "Average Number of Physically Active Days \n by Grade and Gender",
    x = "Grade",
    y = "Mean Physically Active Days (7 Days)",
    color = "Gender"
  ) +
  
  theme_minimal(base_size = 14)

#aggregate(xxx) |>
 # ggplot(aes(xxx)) + 
  #geom_line()
#...


# Create a plot that shows the relationship betwen physical activity and bmi
# among female students in grade 12 
# Ensure that the figure is clearly labeled and includes an appropriate legend



# Push your completed code to your GitHub repository
