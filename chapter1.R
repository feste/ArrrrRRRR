# install.packages(c("rpart", "chron", "Hmisc", "Design",
#                    "Matrix", "lme4", "coda", "e1071",
#                    "zipfR", "ape", "languageR"),
#                  repos = "http://cran.r-project.org")

library(languageR)

#---------------Section 1.1 R as a calculator------------

#basic maths
1 + 2
2 * 3
6 / 3
2 ^ 3
9 ^ 0.5

9 ^ 0.5 ^ 3
(9 ^ 0.5) ^ 3
9 ^ (0.5 ^ 3)

#basic variables
x = 1 + 2
x
x <- 1 + 2
1 + 2 -> x
x = x + 1
x = 3
x + 1
x
4 ^ 3
x = 4
x ^ 3
x
sqrt(9)

#-----------Section 1.2 Getting data into and out of R-----

#switchboard and wallstreet journal parsed corpus == dative
#subset of this corpus == verbs

head(verbs, n=10) #this is a data frame
exp(verbs[1,5])
write.table(verbs, file = "/home/feste/Documents/etc/ArrrrRRRR/dativeS.txt")
verbs =
  read.table("/home/feste/Documents/etc/ArrrrRRRR/dativeS.txt", header=TRUE)
#sas.get
#read.csv
#read.spss

help(verbs)
example(verbs)

#------Section 1.3 Accessing information in data frames -----

verbs[1,5]
#verbs[ ,5] #this is a vector
#verbs$LengthOfTheme
verbs[1, ] #this is a row vector

row1 = verbs[1,]
col5 = verbs[,5]
head(col5, n=5)

row1[1]
col5[1]
row1["RealizationOfRec"]

rs = c(638, 799, 390, 569, 567) #the combination operator
rs

verbs[rs, ] #index some elements
1:5 #the colon operator
5:1
verbs[rs, 1:3]
verbs[rs, c("RealizationOfRec", "Verb", "AnimacyOfRec")]

verbs[verbs$AnimacyOfTheme == "animate", ]
subset(verbs, AnimacyOfTheme == "animate")
verbs[verbs$AnimacyOfTheme == "animate" & verbs$LengthOfTheme > 2, ]

head(rownames(verbs))
colnames(verbs)

verbs.rs = verbs[rs,]
verbs.rs[1, ]
verbs.rs["638",]
verbs[638,]

verbs.rs$AnimacyOfRec
verbs.rs$AnimacyOfRec = as.character(verbs.rs$AnimacyOfRec)
verbs.rs$AnimacyOfRec
verbs.rs$AnimacyOfRec = as.factor(verbs.rs$AnimacyOfRec)
verbs.rs$AnimacyOfRec

verbs.rs2 = verbs[c(638, 390),]
verbs.rs2
verbs.rs2$AnimacyOfRec
as.factor(as.character(verbs.rs2$AnimacyOfRec))
verbs.rs2$AnimacyOfRec[drop=T]

# sort dataframe by column
verbs.rs[order(verbs.rs$RealizationOfRec),]
# nested order-by
#  sorts by alphabetical or numerical depending on type of column
verbs.rs[order(verbs.rs$Verb, verbs.rs$LengthOfTheme),]
order(verbs.rs$Verb) # book has result of order(v) instead

# sorting elements of a vector is the same
v = c("pay","sell","lend","sell","send","sell","give","give","pay","cost")
order(v) # the result of this is what the book has as the result of line 105

# to sort a vector:
sort(v)

# 1.4.2 changing information in a dataframe

# if you wanted to change a cell you'd do this:
verbs.rs["638",]$RealizationOfRec = "NP"
verbs.rs["638",]$RealizationOfRec # NP
verbs.rs["638",]$RealizationOfRec = "PP"
verbs.rs["638",]$RealizationOfRec # PP
verbs.rs["638","RealizationOfRec"] = "NP"
verbs.rs["638","RealizationOfRec"] # NP
verbs.rs["638","RealizationOfRec"] = "PP"
verbs.rs["638","RealizationOfRec"] # PP

verbs.rs$LengthOfTheme
exp(verbs.rs$LengthOfTheme) # 2 4 2 5 4

# length of a string or vector of strings
nchar("antidisestablishmentarianism") # 28
nchar("a") # 1

# adding a column, converting the factor $Verb to a string first
verbs.rs$Length = nchar(as.character(verbs.rs$Verb))
# displaying first 4 rows, only "verb" and "length" columns
verbs.rs[1:4, c("Verb", "Length")]

# 1.4.3 Extracting contingency tables from data frames

# how many observations are characterized by animate
#  recipients realized as an NP?
levels(verbs$RealizationOfRec) # "NP" "PP"
levels(verbs$AnimacyOfRec) # "animate" "inanimate"

xtabs( ~ RealizationOfRec + AnimacyOfRec, data = verbs)

#general form is: dependent variable ~ predictor1 + predictor2 ...

verbs.xtabs = xtabs( ~ AnimacyOfRec + AnimacyOfTheme + RealizationOfRec,
                     data = verbs)
verbs.xtabs

verbs.xtabs = xtabs( ~ AnimacyOfRec + RealizationOfRec,
                     data = verbs, subset = AnimacyOfTheme != "animate")
verbs.xtabs
sum(verbs.xtabs)
sum(verbs.xtabs)==nrow(verbs[verbs$AnimacyOfTheme!="animate",])
verbs.xtabs / sum(verbs.xtabs) * 100

prop.table(verbs.xtabs, 1) # rows sum to 1
prop.table(verbs.xtabs, 2) # cols sum to 1
prop.table(verbs.xtabs) # table cells sum to 1

# 1.4.4 calculations on data frames
# covarying
mean(1:5) # 3
mean(verbs[verbs$AnimacyOfRec == "animate", ]$LengthOfTheme)
mean(verbs[verbs$AnimacyOfRec != "animate", ]$LengthOfTheme)
# do the previous 2 lines simultaneously:
tapply(verbs$LengthOfTheme, verbs$AnimacyOfRec, mean)

with(verbs, tapply(LengthOfTheme, list(AnimacyOfRec, AnimacyOfTheme), mean))

heid[1:5,]
# the shortest reaction time (log)
sort(heid$RT)[1]
# the longest reaction time (log)
sort(heid$RT)[length(heid$RT)]

sort(heid$BaseFrequency)[1]
sort(heid$BaseFrequency)[length(heid$BaseFrequency)]

# aggregate does not preserve column names.  averages over subjects
# first arg is thing you want to work on
# second arg is groups you want to group on
# last arg is thing you want to do to that group
heid2 = aggregate(heid$RT, list(heid$Word), mean)
heid2[1:5,]

# aggregate doesn't know what to call the new column, so you have to tell it
colnames(heid2) = c("Word", "MeanRT")
heid2[1:5]
