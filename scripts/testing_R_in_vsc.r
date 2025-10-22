# On X1 the path for settings is: C:\Users\henri\AppData\Roaming\Code\User\settings.json

library(tidyverse)

df <- mtcars

head(df)
summary(df)
ggplot(df, aes(x = wt, y = mpg)) +
    geom_point() +
    geom_smooth(method = "lm")

help(package = "plot")

View(df)



a=1                  # = for assignment -> assignment_linter
if (T) print("bad")  # use TRUE/FALSE -> T_and_F_symbol_linter
x <- 1:length(iris$Sepal.Length)  # prefer seq_along() -> seq_linter
y <- 2                # (add a few spaces at end of this line) -> trailing_whitespace_linter
# Make this a very very very very very very very very very very very very very very very very long line over 120 chars

library(lintr)
