#' ---
#' title: "Class 5 Introduction to R graphics"
#' author: "Barry Grant"
#' date: "May 3rd, 2014"
#' output: github_document
#' ---

#' Class 05 R graphics intro
#' This is some test and I can have **bold** and *italic* and `code`


# My first boxplot
x <- rnorm(1000,0)
boxplot(x)

summary(x)
hist(x)

boxplot(x, horizontal = TRUE)

#' I have generated x and it has `r length(x)`




# Hands on session 2

weight <- read.table("bimm143_05_rstats/weight_chart.txt", header = TRUE)

plot(weight[,1], weight[,2], typ="o")

plot(weight$Age, weight$Weight, typ="o", main="some main title", 
     pch=15, cex=2, col="red",
     lwd=3, lty=3)


# Try a barplot
barplot(VADeaths, beside = TRUE)      
barplot(VADeaths, beside = FALSE)      


## Input our feature count data
mouse <- read.table("bimm143_05_rstats/feature_counts.txt", header=TRUE, sep="\t")

barplot(mouse$Count, horiz=TRUE, names.arg = mouse$Feature, las=2)

# Change margin so we can see the labels
par(mar=c(5.1, 11.1, 4.1, 2.1))
barplot(mouse$Count, horiz=TRUE, names.arg = mouse$Feature, las=2)

# add some color
barplot(mouse$Count, horiz=TRUE, names.arg = mouse$Feature, las=2, col=rainbow(11))


# Section 3
# Using color

mf <- read.table("bimm143_05_rstats/male_female_counts.txt", sep="\t", header=TRUE)

barplot(mf$Count, names.arg = mf$Sample, col=c("red","blue"), las=2)

# Expresion data

e <- read.table("bimm143_05_rstats/up_down_expression.txt", header=TRUE)

# how many genes
nrow(e)

# How many up, down and all around?
table( e$State )

plot(e$Condition1, e$Condition2, col=e$State)

# Play
palette(c("red","lightgray","blue"))
plot(e$Condition1, e$Condition2, col=e$State)




