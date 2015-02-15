#basic examples
1+2
2*3
3^4

#concatenate function
c(1,2,3,4)

#assigning objects
x <- 2
y <- 3
x+y
x <- 3
x+y

#vector behavior in R
x1 <- c(1,2)
x2 <- c(1,2)
x3 <- c(1,2,3)
x1-x2
x3-x2
x1*x2
x1*x3
x1/x2
x1/x3

#matrix multiplication
x1%*%x2

setwd('/Users/alanjenn/Dropbox/CarnegieMellon/Blog/15-2-15')

#importing student data
data <- read.table(file='input/anonymizedData.csv',header=TRUE,sep=',')

#data frame example
df <- data.frame('col1'=1:5,'col2'=rnorm(5))

#cleaning the student data
data <- na.omit(data)
data$gender <- as.factor(toupper(data$gender))
data$eyeColor <- as.factor(tolower(data$eyeColor))
data <- data[data$age<100,]

#other data structures
matrix1 <- matrix(1,nrow=3,ncol=3)
matrix2 <- matrix(2,nrow=3,ncol=3)
matrix3 <- matrix(3,nrow=3,ncol=3)

#inserting matrices into an array
arrayExample <- array(data=c(matrix1,matrix2,matrix3),dim=c(3,3,3))

#inserting objects into a list
listExample <- list(matrix1,matrix2,matrix3,data)

#custom function to generate GPA based on an age input
gpaFunction <- function(age) {
	randomGPA <- rnorm(1,mean=3,sd=.5)
	ageWeight <-rnorm(1,mean=((age-27.5)/100),sd=.5)
	weightedGPA <- randomGPA+ageWeight
	if(weightedGPA > 4) {finalGPA <- 4}
	else if(weightedGPA < 0) {finalGPA <- 0}
	else {finalGPA <- weightedGPA}
	return(finalGPA)
}

#an example of "applying" the custom function
apply(data['age'],1,gpaFunction)

#an alternative example of "applying" the custom function
data$gpa <- sapply(data$age,gpaFunction)

#timing custom function with a "for" loop
ptm <- proc.time()
for(row in 1:nrow(data)) {
	gpaFunction(data$age[row])
}
proc.time()-ptm

#timing custom function with an "apply" function
ptm <- proc.time()
sapply(data$age,gpaFunction)
proc.time()-ptm

#plotting age vs gpa
pdf(file='figures/basic_age_vs_gpa.pdf')
plot(x=data[,'age'],y=data[,'gpa'])
dev.off()

#plotting age vs gpa
pdf('figures/age_vs_gpa.pdf')
plot(x=data[,'age'],y=data[,'gpa'],xlab='Age',ylab='GPA',ylim=c(0,4),cex.lab=1.5,cex.axis=1.5)
dev.off()

#histogram example
pdf('figures/birthMonth_histogram.pdf')
hist(data[,'birthMonth'],main=NULL,xlab='Birth Month',ylab='Count',cex.lab=1.5,cex.axis=1.5)
dev.off()

#barplot example
eyeColorCounts <- table(data[,'eyeColor'])
pdf('figures/eyeColor_barplot.pdf')
barplot(eyeColorCounts,names.arg=names(eyeColorCounts),cex.names=1.2,cex.axis=1.5)
dev.off()

#running a regression model
model <- lm(gpa~age,data=data)
summary(model)

#plotting age vs gpa with fitted line
pdf('figures/age_vs_gpa_wFit.pdf')
plot(x=data[,'age'],y=data[,'gpa'],xlab='Age',ylab='GPA',ylim=c(0,4),cex.lab=1.5,cex.axis=1.5)
lines(x=data[,'age'],y=predict(model),col='red')
dev.off()

#plotting age vs residuals
pdf('figures/age_vs_resid.pdf')
plot(x=data[,'age'],y=model$residuals,xlab='Age',ylab='Residuals',cex.lab=1.5,cex.axis=1.5)
dev.off()



