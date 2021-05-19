
# sex = 1 is male 
# sex = 0 is female 

#One sample testing, for mean age of male geting heart attack  

library(dplyr) # so that we can use filter 

male_data <- filter(Heart_Attack_Dataset,sex=="1")  # filter dataset to have only Male
x<- male_data$age
n = length(x)
print(n) # get sample size value
standard_deviation = sd(x)
print(standard_deviation)
xbar <- mean(x) 
print(xbar) # get the sample mean value
mu = 44 
alpha = 0.05

#calculate z test 
z=(xbar-mu)/(standard_deviation/sqrt(n))


print(z)


#calculate critical value 
z.alpha = qnorm(1-alpha)

print(z.alpha)

#################################################
#one sample testing , mean age of female geting heart attack 

female_data <- filter(Heart_Attack_Dataset,sex=="0")  # filter dataset to have only Female
y<-female_data$age


n = length(y)
print(n)
standard_deviation = sd(y)
print(standard_deviation)
xbar <- mean(y) 
print(xbar) # get the sample mean value 
mu =  54
alpha = 0.05
#calculate z test 
z=(xbar-mu)/(standard_deviation/sqrt(n))

print(z)

#calculate p-value of z 
pval = pnorm(z) 
print(pval)



#Correlation 

x <-Heart_Attack_Dataset$chol
y <-Heart_Attack_Dataset$trestbps

plot(x,y,main ="Resting blood pressure against serum cholesterol",xlab = "serum cholesterol in mg/dl" , ylab = "resting blood pressure in mm Hg")

cor(x,y)

cor.test(x, y, method = "pearson")


##################################################
# regression 
x <-Heart_Attack_Dataset$trestbps
y <-Heart_Attack_Dataset$thalach


relation <- lm(y~x)

print(relation)

print(summary(relation))

plot(y,x,col = "blue",main = "Maximum heart rate achieved against Resting blood pressure",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Resting blood pressure",ylab = "Maximum heart rate achieved")

#################################################
#Chi-Square test 
library(MASS)

#get the contigency table
tbl = table (Heart_Attack_Dataset$cp,Heart_Attack_Dataset$restecg)
print(tbl)
# perform chi-square test on the data table
chisq.test(tbl, correct=FALSE)

# create a new table
tbl2 = cbind(tbl[,'0'], tbl[,'1']+ tbl[,'2'])
print(tbl2)
# perform chi-square test on the data table
chisq.test(tbl2, correct=FALSE)

# create a new table
tbl3 = cbind(tbl[,0], tbl[,1]+ tbl[,2])
print(tbl3)
# perform chi-square test on the data table
chisq.test(tbl3, correct=FALSE)



#critical value
alpha <- 0.05
x2.alpha <- qchisq(alpha, df=3,lower.tail=FALSE)
print(x2.alpha)

