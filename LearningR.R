install.packages("devtools")
install.packages("knitr")
library(knitr)
library(devtools)
devtools::install_github("hilaryparker/explainr")
library(explainr)

#Let's try and run a proportions test
prop.test(x = 500, n = 1008) # Cool, but what if we want to keep those results for later?
ptest <- prop.test(x = 500, n = 1008) #We have stored the results of our 1-sample proprotions test in the R Environment
ptest #So when we call the results of that test, they're stored in memory
explain(ptest) # And now we get an explanation of what we just did
#YOU MAY NEVER NEED A GSI AGAIN!

#Let's actually load some data and play around in R
library(foreign)
MIdf<-read.csv("http://michaelbrown.work/wp-content/uploads/2016/05/MIdf.csv", header=T, sep=",") # This downloads the resources from my website. 
#In the Environments Panel you should now have a file named MIdf with 212 observations of 36 variables. 

##Example using your computer (if you have a Mac)
#setwd("~/Downloads")
#Example<-read.csv("exampleData.csv")

summary(MIdf) #This provides quick descriptives for each variable in the dataset. 

#What is the Median and Mean tuition revenue per full time equivalent student?
summary(MIdf$school.tuition_revenue_per_fte)

#In R, we call the dataset and then specify the variable. You have to do this because you might have multiple datasets open at once. 
#You can 'attach' the data so R knows this is the dataset you're calling
attach(MIdf)
summary(school.tuition_revenue_per_fte)

#Annotate the mean and median value for each variable using the hashtag 
summary(MIdf$X2013.cost.attendance.academic_year)
#Mean                Median
summary(MIdf$X2013.admissions.admission_rate.overall)
#Mean                Median
summary(MIdf$school.instructional_expenditure_per_fte)
#Mean                Median
summary(MIdf$X2013.admissions.sat_scores.average.overall)
#Mean                Median
summary(MIdf$X2013.student.share_firstgeneration)
#Mean                Median

#Let's visualize some of this stuff and compare it to the mean and median
hist(MIdf$school.tuition_revenue_per_fte)

#Let's visualize the data
hist(MIdf$X2013.student.size)
hist(MIdf$school.instructional_expenditure_per_fte)

#Let's look at these two together
par(mfrow=c(1,2),pty="s")
hist(MIdf$X2013.student.size)
hist(MIdf$school.instructional_expenditure_per_fte)
dev.off() # We run this to reset the Plots window. 


#I wonder how revenue compares to expenditure
plot(MIdf$school.instructional_expenditure_per_fte, MIdf$school.tuition_revenue_per_fte, main="Expenditure vs Tuition Revenue 2013",
     xlab= "Instructional Expenditure", ylab="Tuition Revenue")
#And student costs compares to expenditure
plot(MIdf$school.instructional_expenditure_per_fte,MIdf$X2013.cost.tuition.program_year, main="Expenditure vs Cost of Tuition 2013",
     xlab="Instructional Expenditure", ylab="Cost of Tuition")


pdf("Comparing Graphs.pdf")
par(mfrow=c(2,1),pty="s") #This puts two graphs in one window
#I wonder how revenue compares to expenditure
plot(MIdf$school.instructional_expenditure_per_fte, MIdf$school.tuition_revenue_per_fte, main="Expenditure vs Tuition Revenue 2013",
     xlab= "Instructional Expenditure", ylab="Tuition Revenue")
#And student costs compares to expenditure
plot(MIdf$school.instructional_expenditure_per_fte,MIdf$X2013.cost.tuition.program_year, main="Expenditure vs Cost of Tuition 2013",
     xlab="Instructional Expenditure", ylab="Cost of Tuition")
dev.off()

#I want to know what some of these schools are!! It looks like there's one school that spends a lot per student- way more than it costs to go there

#The basic plotting functions in R are good, but I want to try and I identify that one weird school
library(ggplot2) #This is a very adept graphics package

plot(MIdf$X2013.cost.tuition.program_year,MIdf$school.instructional_expenditure_per_fte)

p <- ggplot(MIdf, aes(X2013.cost.tuition.program_year, MIdf$school.instructional_expenditure_per_fte, label = school.name))
p<-p + geom_point(aes(colour=MIdf$X2013.aid.pell_grant_rate))+ scale_color_gradient(high="red", low="white", "% of Pell Grant Recipients")+
  geom_text(aes(label=school.name), size=3)+labs(title = "Cost of Tuition by Instructional Expenditure") +
  ylab("Instructional Expenditure in $") +
  xlab("Cost of Tuition in $")
pdf("Comparing Graphs Labels.pdf")
p
dev.off()

#Looks like a lot of for profit schools in here! Let's just look at schools with an admission rate

MIdfRate <- subset(MIdf, is.na(MIdf$X2013.admissions.admission_rate.overall))

#Now let's look at that again

p2 <- ggplot(MIdfRate, aes(X2013.cost.tuition.program_year, school.instructional_expenditure_per_fte, label = school.name))
p2 + geom_point(aes(colour=MIdfRate$X2013.aid.pell_grant_rate))+ scale_color_gradient(high="red")+
  geom_text(aes(label=school.name), size=3)+labs(title = "Cost of Tuition by Instructional Expenditure") +
  ylab("Instructional Expenditure in $") +
  xlab("Cost of Tuition in $")
pdf("Comparing Graphs Admissions Labels.pdf")
p2
dev.off()

#Getting descriptive statistics
install.packages("pastecs")
library(pastecs)
basicdesc<-stat.desc(MIdf, basic=T, norm=F)
#Let's look at those descriptives!
summary(basicdesc)
#Oh, that's the same as summary. So why did we do that?
#BECAUSE WE CAN WRITE THIS OBJECT TO A CSV TO MAKE TABLES
write.csv(basicdesc, file="LearningRdescriptives.csv")

#AND NOW-Regression
#DV: school.instructional_expenditure_per_fte (Instructional Expenditure per student)
#IV: X2013.cost.tuition.in_state, X2013.admissions.admission_rate.overall, X2013.admissions.sat_scores.average.overall, X2013.student.size
CostModel <- lm(school.instructional_expenditure_per_fte  ~ X2013.cost.tuition.in_state + X2013.admissions.admission_rate.overall + 
                  X2013.admissions.sat_scores.average.overall + X2013.student.size, data=MIdf, x=T)
summary(CostModel)


#Let's do some diagnostics on this really spurious model that I made: 

par(mfrow=c(2,2))
pdf("Regression Diagnostic Plots.pdf")
plot(CostModel)
dev.off()


