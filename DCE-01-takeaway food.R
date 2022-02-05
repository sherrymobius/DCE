library(idefix)
library(reshape2)
library(dplyr)
library(survival)
library(mlogit)
library(support.CEs)
library(Rchoice)



#1 Design-CEA
library(idefix)
options(max.print=1000000)

#1.1 orthogonal design
levels<- c(2,3,3)
coding = c("E", "E", "E")
Profiles (lvls=levels, coding=coding)

#1.2 CEA D-efficient design
priors <- c(0, 0, 0, 0, 0, 0)
s <- diag(length(priors))
sim <- MASS::mvrnorm(n = 1000, mu = priors, Sigma = s)
sim <- list(sim[, 1:1], sim[, 2:6])
alt.cte <- c(0, 0, 1)
d<-CEA(lvls=levels, coding=coding, n.alts=3, n.sets=6, alt.cte=alt.cte, par.draws=sim, no.choice=TRUE, best=TRUE)
design <- d$design
d
design

#2 Reshaping data
library(reshape2)

data<-melt(TF577,  #name of the data file
           id.vars = c("personid"))
data<-rbind(data,data,data)
data <- data[order(data$personid, data$variable),]
x <- nrow(data)/3
alt <- rep(1:3, x)
data <- cbind(data, alt)
cs <- rep(1:x, each= 3)
cs <- sort(cs)
data <- cbind(data,cs)

library(dplyr)
data <- mutate(data, choice=ifelse (value == "1" & alt=="1" | value== "2" & alt=="2"| value== "3" & alt=="3",1,0))

finalEEC<- cbind(data, designEEC)

#3 Data analysis
#3.1 mixed logit model
library(mlogit)
DEEC <- dfidx(finalEEC, choice="choice", idx = list(c("cs", "personid"), "alt"),idnames= c("cs", "alt" ))
resultsXLMEEC<-mlogit(choice~alt3.cte+Var11 + Var21 + Var22 + Var3|0,data=D,rpar = c(Var11="n",  Var21="n",Var22="n",Var3="n"), R=100,halton=NA, panel=TRUE)
summary(resultsXLMEEC)

#3.2 MWTP(marginal willingness to pay)
mwtp(output = resultsXLMEEC, monetary.variables = c("Var3"),
     nonmonetary.variables =c("Var11", "Var21", "Var22"),
     confidence.level = 0.95, seed = 987)

#3.3 Conditional logit
resultsCLMEEC<- clogit(choice~alt3.cte +Var11+Var21+Var22+Var3+strata(cs),data = finalEEC)
summary(resultsCLMEEC)

#3.31 MWTP
mwtp(output = resultsCLMEEC, monetary.variables = c("Var3"),
       nonmonetary.variables =c("Var11", "Var21", "Var22"),
       confidence.level = 0.95, seed = 987)
#3.32 Interactions
#3.32.1 var11
resultsv11<-clogit(choice~alt3.cte++Var11+Var21+Var22+Var3+Var11:fre+Var11:pri+Var11:tas+Var11:nqt+Var11:nqo+Var11:kno+Var11:hea+Var11:spr+Var11:sex+Var11:age+Var11:edu+Var11:inc+Var11:bmi+strata(cs),data = finalEEC)
summary(resultsv11)

#3.32.2 Var21
resultsv21<-clogit(choice~alt3.cte++Var11+Var21+Var22+Var3+Var21:fre+Var21:pri+Var21:tas+Var21:nqt+Var21:nqo+Var21:kno+Var21:hea+Var21:spr+Var21:sex+Var21:age+Var21:edu+Var21:inc+Var21:bmi+strata(cs),data = finalEEC)
summary(resultsv21)

#3.32.3 Var22
resultsv22<-clogit(choice~alt3.cte++Var11+Var21+Var22+Var3+Var22:fre+Var22:pri+Var22:tas+Var22:nqt+Var22:nqo+Var22:kno+Var22:hea+Var22:spr+Var22:sex+Var22:age+Var22:edu+Var22:inc+Var22:bmi+strata(cs),data = finalEEC)
summary(resultsv22)

#3.32.4 Var3
resultsv3<-clogit(choice~alt3.cte++Var11+Var21+Var22+Var3+Var3:fre+Var3:pri+Var3:tas+Var3:nqt+Var3:nqo+Var3:kno+Var3:hea+Var3:spr+Var3:sex+Var3:age+Var3:edu+Var3:inc+Var3:bmi+strata(cs),data = finalEEC)
summary(resultsv3)









