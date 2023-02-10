## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

#Code R:
#Admission probability
#FORLER Sitraka Matthew
#data avaiable on Kaggle: https://www.kaggle.com/mohansacharya/graduate-admissions

library(ggplot2)


M=read.csv("C:/Users/Forler S. Matthieu/Desktop/Admission/Admission_Predict.csv", sep=",", header=T)
M
M=M[,-1]
# Search the dimension of out matrix (400 rows and 8 columns).
dim(M)
#[1] 400   8
# Harvest some information about our dataset:
summary(M)

#####################Fun facts:
N=M[M[,1]>330,]
nrow(N) #There are 70 students who got more than 330 at the GRE Score

N=M[M[,2]>109,]
nrow(N) #157 students have level C1 in English (according to the TOEFL assessment).

N=M[M[,6]>9,]
nrow(N) #113 candidates had more than 9 in their CGPA

N=M[M[,8]>0.9,] 
nrow(N) #49 have chances be accepted in the master program they want.

################# HISTOGRAMS:
 par(mfrow=c(3,3))
 hist(M[,1],main="GRE Score")
 hist(M[,2],main="TOEFL score")
 hist(M[,3],main="University rating")
 hist(M[,4],main="SOP")
 hist(M[,5],main="LOR")
 hist(M[,6],main="CGPA")
 hist(M[,7],main="Research")
 hist(M[,8],main="Chance of admit")
#We can see that many data have a normal distribution likelihood resemblance.
 
#######GRE Score Bell Curve:#######
par(mfrow=c(3,3))
H1= M[,1]
hist(H1,main="GRE Score",prob=TRUE, col="grey")
lines(density(H1), col="blue", lwd=2)
lines(density(H1, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
#We tried to show that there's a "bell like" curve, of course it's not perfect but there's a resemblance
#We do it for each
H2= M[,2]
hist(H2,main="TOEFL Score",prob=TRUE, col="grey");lines(density(H2), col="blue", lwd=2);lines(density(H2, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

H3= M[,3]
hist(H3,main="University rating",prob=TRUE, col="grey"); lines(density(H3), col="blue", lwd=2) ;lines(density(H3, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
#it works a little bite less for The ranks of Universities
H4= M[,4]
hist(H4,main="SOP",prob=TRUE, col="grey");lines(density(H4), col="blue", lwd=2);lines(density(H4, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
H5= M[,5]
hist(H5,main="LOR",prob=TRUE, col="grey");lines(density(H5), col="blue", lwd=2);lines(density(H5, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
H6=M[,6]
hist(H6,main="CGPA",prob=TRUE, col="grey");lines(density(H6), col="blue", lwd=2);lines(density(H6, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
H7=M[,7]
hist(H7,main="Research",prob=TRUE, col="grey");lines(density(H7), col="blue", lwd=2);lines(density(H7, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
#since it's a dichotomic value it's logical it doesn't follow a Normal distribution.
H8=M[,8]
hist(H8,main="Chance of admit",prob=TRUE, col="grey");lines(density(H8), col="blue", lwd=2);lines(density(H8, adjust=2), lty="dotted", col="darkgreen", lwd=2) 


############################################################################
####################################Step 1: Correlations ? #################
############################################################################
library(corrplot)


#An Overview of the possible relationships ?
pairs(M)
#it's kind of messy so we will proced 4 at a time. 


#possible relationships among the four variables ?
pairs(M[,1:4])  #it's ugly.

pairs(M[, 1:4], col=1:4, oma=c(3,3,3,15))
par(xpd = TRUE)
legend("bottomright", fill = unique(M[,1:4]), legend = c( levels(M[,1:4])))
#We can "try" to see that there are some possible relations between GRE and TOEFL, but not for University and SOP.


pairs(M[, 4:8], col=1:4, oma=c(3,3,3,15))
par(xpd = TRUE)
legend("bottomright", fill = unique(M[,4:8]), legend = c( levels(M[,4:8])))
#Possible positive relations beweent LOR and CGPA
#CGPA and Chance of Admit. 
#CGPA and SOP
#CGPA and LOR
#LOR and Chance of Admit.

#Details: 
#CGPA and GRE: Correlation ? Yes.
ggplot(M, aes(x=H1, y=H6))+ geom_point() +  geom_smooth()
#we can see that there's a correlation between The TOEFL Score and the CGPA
#Thus, students with good grades tends to have good scores at the TOEFL
ggplot(M, aes(x=H1, y=H6))+ geom_point() +    geom_smooth(method=lm)

# "Loess" Method thus it'll allow to produce smoothed curves, adjusted to a point cloud.
ggplot(M, aes(x=H1, y=H6))+ geom_point() +  geom_smooth()

#LOR and CGPA,
#Better Grades tends to have better Letters
ggplot(M, aes(x=H5, y=H6)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="blue") 

#Ugly but gives us the trend
plot(M[,-7], pch=21, cex=1.5,
     col=c("black", "blue", "green", "yellow", "orange", "purple", "pink"),
     panel = panel.smooth, main = "Admissions") 



#########################################################
####################  TEST  #############################
#########################################################
par(mfrow=c(1,1))
# Boxplot 
boxplot(H8~H1, main="Boxplot GREScore vs Chances of Admission", 
        xlab="GRE Score", ylab="Chances of Admission")
par(mfrow=c(3,3))
boxplot(H1, main="GRE Score")
boxplot(H2,main="TOEFL score")
boxplot(H3,main="University rating")
boxplot(H4 ,main="SOP")
boxplot(H5 ,main="LOR")
boxplot(H6 ,main="CGPA")
boxplot(H7 ,main="Research")
boxplot(H8,main="Chance of admit")


########## t-test#############

t.test(M, mu=0)

cor.test(~H8+H1, data=M)
#There's a correlations between GRE Score and Chance of admit.
        
#SHAPIRO TEST    
M = rnorm(400, mean=5, sd=3)
shapiro.test(M)
#so yes we are on a normal distribution 
     
cor(M) 
#yes there are many correlations thus we are going to do Linear Reg



#################################################################
##########################   LINEAR REGRESSIONS  ################
#################################################################
#First simple regression

lm (M[,1] ~ M[,2]) 

par(mfrow=c(2,2))
lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating + SOP + LOR + CGPA + Research)
reg1 = lm(H8 ~ H1+ H2+ H3+ H4+ H5+H6+ H7)
reg1
plot(reg1)



#Without University Rank 

par(mfrow=c(2,2))
lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP + LOR + CGPA + Research)
reg2 = lm(H8 ~ H1+ H2+ H4+ H5+H6+ H7)
reg2
plot(reg2)


par(mfrow=c(2,2))
lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating + SOP + LOR + CGPA)
reg3 = lm(H8 ~ H1+ H2+ H3+ H4+ H5+H6)
reg3
plot(reg3)

par(mfrow=c(2,2))
lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + CGPA)
reg4 = lm(H8 ~ H1+ H2+H6)
reg4
plot(reg4)



##################  Cool Stuff #############
M=read.csv("C:/Users/Forler S. Matthieu/Desktop/Admission/Admission_Predict.csv", sep=",", header=T) 
#so we have the num of students
a= M[,1]

b=sample(a,1)
cat ("You are student",b)


c= M[b,2]
cat("You have a GRE Score of",c )
#if(c >330 {print("You can try to candidate to a Ivy's Master Program")})

if(c >330){print("You can apply to the Master program of the MIT")} else {print("Your chances to join the MIT are low")}
  

#############################Pb set 5 like##########################
P=cor(M)
s=0.7
n=nrow(P)
for(i in 1:n){
  for(j in 1:n){
    if(abs(P[i,j])>s){
      print(colnames(P)[i])
      print(colnames(P)[j])
    }}}
#
# We wanted to create a function with a loop, 
#with which we obtain all the elements of the correlation matrix of our sample, 
#having an absolute value greater than 0.7, i.e. having a relatively high degree of correlation. 
#We observe that many variables have a strong correlation with each other. 
#Thus, the results of the function are printed in pairs: 
#we observe for example that "GRE.score" and "TOEFL.score" respect the condition, 
#their names will be printed one after the other to explain that their correlation is higher than 0.7,
#i.e. a strong linear relationship.

P=cor(M)
s=0.9
n=nrow(P)
for(i in 1:n){
  for(j in 1:n){
    if(abs(P[i,j])>s){
      print(colnames(P)[i])
      print(colnames(P)[j])
    }}}

#We also see there are no different variables who have a correlation over 90%
#Post Tenebras Lux
#Feel free to improve it (it's not really the best code or analyse you will find here but at least we coded few words lol)
#Bonsoir !
