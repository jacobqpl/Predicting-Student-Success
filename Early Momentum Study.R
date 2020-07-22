#### early momentum data
remove(list = ls()) 

library(dplyr)
library(MatchIt)
# install.packages("cramer")
library(cramer)
library(DescTools)## calculate CramerV
# install.packages("nnet")
library(nnet)

### data created from Cumulative credits script
setwd("C:\\Peilin Qiu\\Early Momentum Study\\Data from Lan")

mydata3 = read.csv("mydata3.csv", header = T)
mydata3$SemGPA_Re = ifelse(mydata3$SemGPA>=3,1,0) 

mydata4 = mydata3[which(mydata3$SummTran==0),] ## exclude students transfer before first year ends # cases 3090

mydata4 = mydata4[which(mydata4$WinterTrans==0),] ## exclude students transfer in Winter # cases 2958

### mydata4 used for modelling first year momentum variables
mydata4$SemGPA_Re = ifelse(mydata4$SemGPA>=3,1,0)
mydata4$SemGPA_Re = factor(mydata4$SemGPA_Re, labels = c("<3.0", ">=3.0"))




## descriptive statistics
y = group_by(mydata4,FTIAC,SemCrAtt_Re)%>%summarise(tot = n(),Bach = sum(MaxOfDeg =="Bachelor"), Assc = sum(MaxOfDeg == "Assc"), Cert = sum(MaxOfDeg == "Cert"),
                                                    perBach = Bach/tot, perAssc = Assc/tot, perCert = Cert/tot)


#### calculate cramerV

t = table(mydata3$SemCrAtt_Re,mydata3$FallCollCredits_Re)
CramerV(t,method = "fisheradj", conf.level = T)

#################################################
# r = cor(mydata2$FYrCollCredit_Re,mydata2$FYrCollCredit)
#################################################

# install.packages("VGAM")
library(VGAM)
library(nnet)



### multinomial response is maxofdeg


m2 = vglm(MaxOfDeg~ SemCrAtt + SemGPA + SEX + Agex + White + PELL + FTIAC, family = multinomial(), data = mydata3)
summary(m2)


m3 = multinom(MaxOfDeg2 ~ FallCollCredits_Re + winterEnl_new + summEnl_new + FYrCollCredit_Re + EngCollPass + MathCollPass + SemGPA + SEX + Agex + 
                White + PELL + FTIAC, data = mydata4)
summary(m3)

z = summary(m3)$coefficients/summary(m3)$standard.errors
p = (1-pnorm(abs(z),0,1))*2
p





plot(mydata4$MaxOfDeg2,fitted.values(m3))
x = as.data.frame(m3$fitted.values)
###########################################
#df = mydata3[,c("SemCrAtt_Re","SemGPA_Re")]
df = mydata4[,c("SemCrAtt_Re","SemGPA_Re")]
df1 = cbind(df,x)


#mydata3 = within(mydata3, MaxOfDeg <- relevel(MaxOfDeg, ref = "NoDeg"))

library(car)
m1 = vglm(MaxOfDeg~FYrCollCredit_Re + winterEnl_new + summEnl_new + MathCollPass + EngCollPass + SemGPA, family = multinomial, data = mydata3)
summary(m1)

vif(m1)
VIF()

pred = predict(m1,type = "response")
pred = as.data.frame(pred)
df = mydata3[,c("FYrCollCredit")]
df2 = cbind(df,pred)
df2 = as.data.frame(df2)


x = group_by(df2, FYrCollCredit,summerEnl_new)%>%summarise(Nodeg = mean(NoDeg), Cert = mean(Cert), Assc = mean(Assc), Bach = mean(Bachelor))

x = group_by(df2, df)%>%summarise(NoDeg = first(NoDeg), Cert = first(Cert), Assc = first(Assc),Bach = first(Bachelor))

boxplot()

x1 = x[which(x$summerEnl_new==1),]
x2 = x[which(x$summerEnl_new ==0),]

par(mfrow =c(1,1))
plot(x$df,x$Cert,col = "red", xlim = c(1,80), ylim = c(0,1), type = "l", xlab = "College-level credits earned", ylab = "Predicted probability of a degree completion")
lines(x$Assc,col = "blue")
lines(x$Bach,col = "black")
legend(0, 0.8, c("Certificate","Associate","Bachelor"), lty = c(1,1,1), col = c("red","blue","black"))


plot(x2$FYrCollCredit,x2$Nodeg,col = "red", xlim = c(1,80), ylim = c(0,1), type = "l")
lines(x2$Cert,col = "green")
lines(x2$Assc,col = "blue")
lines(x2$Bach,col = "black")


prop.table(table(mydata3$FYrCollCredit_Re,mydata3$MaxOfDeg),2)


#### binary model 

bm = glm(BachAssc ~ FYrCollCredit + Agex + , data = mydata3, family = binomial)
summary(bm)
pred = predict(bm,type = "response")
pred = as.data.frame(pred)
df = mydata3[,c("FYrCollCredit")]
df2 = cbind(df,pred)
df2 = as.data.frame(df2)




## propotional model### parrallel asumption is not satisfied


mydata3$MaxOfDeg2[mydata3$degmax_2==1]=4 ## no deg
mydata3$MaxOfDeg2[mydata3$degmax_2==5]=1 ## bach
mydata3$MaxOfDeg2[mydata3$degmax_2==4]=2 ## assc
mydata3$MaxOfDeg2[mydata3$degmax_2==3]=3 ## cert

mydata3$MaxOfDeg2 = factor(mydata3$MaxOfDeg2)

mydata3$MaxOfDeg = ordered(mydata3$MaxOfDeg)




m3 = vglm(MaxOfDeg ~ FYrCollCredit , family = cumulative(parallel = TRUE), data = mydata3)

pred = predict(m3, type = "response")

df = mydata3[,c("FYrCollCredit")]
df2 = cbind(df,pred)
df2 = as.data.frame(df2)


x = group_by(df2, df)%>%summarise(Mean1 = mean(V2), Mean2 = mean(V3), Mean3 = mean(V4), Mean4 = mean(V5))

x = group_by(df2, df)%>%summarise(Nodeg = mean(NoDeg), Cert = mean(Cert), Assc = mean(Assc), Bach = mean(Bachelor))## cumulative


plot(x$df,x$Mean1,col = "red", xlim = c(1,60), ylim = c(0,0.8), type = "l")
lines(x$Mean2,col = "green")
lines(x$Mean3,col = "blue")
lines(x$Mean4,col = "black")


plot(x$df,x$Nodeg,col = "black", xlim = c(1,60), ylim = c(0,0.8), type = "l")
lines(x$Cert,col = "blue")
lines(x$Assc,col = "green")
lines(x$Bach,col = "red")

## test for parallel 
m4 = vglm(MaxOfDeg ~ FYrCollCredit , family = cumulative(parallel = FALSE), data = mydata3)



legend(5,.6, c("bachelor","Associate Deg","Cert.", "No Deg"))



y = group_by(df1, SemCrAtt_Re,SemGPA_Re)%>%summarise(NoDeg = mean(NoDeg), Bachelor = mean(Bachelor),
                                                     Associate = mean(Assc), Certificate = mean(Cert) )

plot(y$FYrCollCredit,y$NoDeg,col = "black", xlim = c(1,60), ylim = c(0,0.8), type = "l")


lines(y$Certificate,col = "blue")
lines(y$Associate,col = "green")
lines(y$Bachelor,col = "red")



##### graphing


newdf = select(mydata4,PIDM,White, Agex, FTIAC, PELL, SemCrAtt_Re, SEX, SemGPA, FallCollCredits_Re,winterEnl_new,summEnl_new,
               EngCollPass,MathCollPass,FYrCollCredit_Re,MaxOfDeg2)


newdf2 = filter(newdf,White == 1, Agex == "<19", FTIAC ==1, PELL ==0, SEX == "F")
newdf2$MeanGPA = mean(newdf2$SemGPA)

newdf2 = newdf2[,-8]

colnames(newdf2)[colnames(newdf2) =="MeanGPA"]="SemGPA"

m3 = multinom(MaxOfDeg2 ~  FallCollCredits_Re  + SemGPA + SEX + Agex + 
                White + PELL + FTIAC, data = mydata4)
summary(m3)

pred = predict(m3, newdata = newdf2, "probs")

pp.w = cbind(newdf2,pred)

newdf2$SemCrAtt_Re = factor(newdf2$SemCrAtt_Re, c("<=6","6-12",">12"))


par(mfrow =c(1,3))
plot(pp.w$FYrCollCredit_Re,pp.w$Bachelor,ylim = c(0,0.6), main = "Bachelor Attainment")
plot(pp.w$FYrCollCredit_Re,pp.w$Assc,ylim = c(0,0.6), main = "Associate Attainment")
plot(pp.w$FYrCollCredit_Re,pp.w$Cert, ylim = c(0,0.6), main = "Certificate Attainment")


par(mfrow =c(1,3))
plot(pp.w$MathCollPass,pp.w$Bachelor,ylim = c(0,0.5), main = "Bachelor Attainment")
plot(pp.w$MathCollPass,pp.w$Assc,ylim = c(0,0.5), main = "Associate Attainment")
plot(pp.w$MathCollPass,pp.w$Cert, ylim = c(0,0.5), main = "Certificate Attainment")


par(mfrow =c(1,3))
plot(pp.w$EngCollPass,pp.w$Bachelor,ylim = c(0,0.5), main = "Bachelor Attainment")
plot(pp.w$EngCollPass,pp.w$Assc,ylim = c(0,0.5), main = "Associate Attainment")
plot(pp.w$EngCollPass,pp.w$Cert, ylim = c(0,0.5), main = "Certificate Attainment")

par(mfrow =c(1,3))
plot(pp.w$winterEnl_new,pp.w$Bachelor,ylim = c(0,0.5), main = "Bachelor Attainment")
plot(pp.w$winterEnl_new,pp.w$Assc,ylim = c(0,0.5), main = "Associate Attainment")
plot(pp.w$winterEnl_new,pp.w$Cert, ylim = c(0,0.5), main = "Certificate Attainment")

par(mfrow =c(1,3))
plot(pp.w$summEnl_new,pp.w$Bachelor,ylim = c(0,0.5), main = "Bachelor Attainment")
plot(pp.w$summEnl_new,pp.w$Assc,ylim = c(0,0.5), main = "Associate Attainment")
plot(pp.w$summEnl_new,pp.w$Cert, ylim = c(0,0.5), main = "Certificate Attainment")


par(mfrow =c(1,3))
plot(pp.w$SemCrAtt_Re,pp.w$Bachelor,ylim = c(0,0.5), main = "Bachelor Attainment")
plot(pp.w$SemCrAtt_Re,pp.w$Assc,ylim = c(0,0.5), main = "Associate Attainment")
plot(pp.w$SemCrAtt_Re,pp.w$Cert, ylim = c(0,0.5), main = "Certificate Attainment")

par(mfrow =c(1,3))
plot(pp.w$FallCollCredits_Re,pp.w$Bachelor,ylim = c(0,0.5), main = "Bachelor Attainment")
plot(pp.w$FallCollCredits_Re,pp.w$Assc,ylim = c(0,0.5), main = "Associate Attainment")
plot(pp.w$FallCollCredits_Re,pp.w$Cert, ylim = c(0,0.5), main = "Certificate Attainment")


