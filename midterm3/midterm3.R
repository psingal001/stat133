# Midterm 3

# Write a function called numDotElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly 
#     with the "." symbol
#
# and return the following
#   <num.dot>: an integer indicating how many elements of <chvec> contain the "."
#     symbol. For example: numDotElements(c('USA', 'U.S.A', '...')) should return 2

numDotElements <- function(chvec){
  num.vec <- grep(".", chvec, fixed = TRUE)
  num.dot <- length(num.vec)
  return(num.dot)
}


# Write a function called sumDigits that compute the sum of all the digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the sum of all the digits in chvec)

sumDigits <- function(chvec){
  digit.vec <- gsub("[^[:digit:]]", "", chvec)
  digit.vec <- unlist(strsplit(digit.vec, ""))
  total <- sum(as.numeric(digit.vec))
  return(total)
}

# Some test cases:
all.equal(sumDigits("1z3p ! 21"), 7)
all.equal(sumDigits("abcdefg"), 0)



# Write a function called hisToHer that converts every instance of 
# him in a string to her; every instance of he to she and every instance 
# of his to her. You can assume everything is lower case. Be careful not 
# to replace words that contain him/he/his (eg you don't want to
# replace the with ther). Your function should take the argument
#   <chvec>: A character vector
#
# and return
#   <herchvec>: The same character vector with the required substitutions.

hisToHers <- function(chvec){
  herchvec <- gsub(" he ", " she ", chvec)
  herchvec <- gsub(" his ", " her ", chvec)
  herchvec <- gsub(" him ", " her ", chvec)
  return(herchvec)
}


# A test case
all.equal(
  hisToHers("he went to the store his mother gave him"), 
  "she went to the store her mother gave her"
)


# Write a function called mostCommonLetter that finds the most common 
# letter in a string. If there is a tie for most common letter return 
# all of the letters that were most common. Your function should 
# convert all letters in the string to *lower case* and you should 
# remove  everything other than letters. 
# Your function has the argument
#  <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and it should return
#  <letter> The most common letter or letters in the string.
# For example mostCommonLetter("aabbccccdddd") should return 
# [1] "c" "d"

mostCommonLetter <- function(chvec){
  new.chvec <- gsub("[^[:alpha:]]", "", chvec)
  new.chvec <- tolower(new.chvec)
  letter.vec <- sort(unlist(strsplit(new.chvec, "")))
  letter <- unique(letter.vec[table(letter.vec) == max(table(letter.vec))])
  return(letter)
}