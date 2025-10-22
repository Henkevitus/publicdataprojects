# Creator: Henrik Vitus Bering Laursen
# Purpose: Quarto blog
# Content: Functions to source


# Package installer and loader --------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}