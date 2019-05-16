setwd("~/Documents/Academic/BA with R/Problem set")
install.packages("DBI")
install.packages("RSQLite")
install.packages("ggplot2")
install.packages("data.table")
install.packages("broom")
rm(list=ls())
library(data.table)
library(ggplot2)
library(broom)
library(forecast)
library(plm)

wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}

#Question 1:
hprice1 <- wpull('hprice1')
summary(lm(price~assess, data=hprice1))
summary(lm(price~assess + bdrms + lotsize + sqrft + colonial, data=hprice1))
summary(lm(price~bdrms + lotsize + sqrft + colonial, data=hprice1))

model1 <- step(lm(price~(bdrms + lotsize + sqrft + colonial)^2 + I(sqrft^2)+ I(bdrms^2) + I(lotsize^2), data=hprice1), k=log(nrow(hprice1)))
BIC(model1)
summary(model1)

model2 <- step(lm(price~(bdrms + lotsize + sqrft + colonial)^3 + I(sqrft^2)+ I(bdrms^2) + I(lotsize^2), data=hprice1), k=log(nrow(hprice1)))
BIC(model2)
summary(model2)

model3 <- lm(price ~ bdrms + lotsize + sqrft + colonial + I(lotsize^2) + bdrms:lotsize + bdrms:colonial, data=hprice1)
summary(model3)
BIC(model3)  #The best model

#Question 2:
gpa2 <- wpull('gpa2')
step(lm(colgpa ~ sat+ tothrs+ athlete+ verbmath+ hsize+ hsrank + hsperc+ female+ white+ black, data=gpa2))
step(lm(colgpa ~ (sat+ tothrs+ athlete+ verbmath+ hsize+ hsrank + hsperc+ female+ white+ black)^2 + sat+ tothrs+ athlete+ verbmath+ hsize+ hsrank + hsperc+ female+ white+ black, data=gpa2))
step(lm(colgpa ~ (sat+ tothrs+ athlete+ verbmath+ hsize+ hsrank + hsperc+ female+ white+ black)^2 + I(sat^2)+ I(tothrs^2) + I(athlete^2) + I(verbmath^2) + I(hsize^2) + I(hsrank^2) + I(hsperc^2) + female+ white+ black, data=gpa2))
step(lm(colgpa ~ (sat+ tothrs+ athlete+ verbmath+ hsize+ hsrank + hsperc+ female+ white+ black)^3 + I(sat^2)+ I(tothrs^2) + I(athlete^2) + I(verbmath^2) + I(hsize^2) + I(hsrank^2) + I(hsperc^2) + female+ white+ black, data=gpa2))
model4 <- lm(colgpa ~ sat + tothrs + athlete + verbmath + hsize + hsrank + hsperc + female + white + black + I(sat^2) + I(hsrank^2) + I(hsperc^2) + sat:tothrs + sat:hsperc + sat:white + tothrs:hsperc + athlete:verbmath + athlete:hsize + athlete:hsrank + athlete:hsperc + athlete:black + verbmath:white + hsize:hsrank + hsrank:hsperc + hsrank:white + female:white + female:black, data = gpa2)
summary(model4)
BIC(model4) # The best model

# Question 3:
mlb1 <- wpull('mlb1')
mlb2 <- na.omit(mlb1)
step(lm(salary~teamsal+ nl+ years+ games+ atbats+ runs+ hits+ doubles+ triples+ hruns+ rbis+ bavg+ bb+  so+ sbases+ fldperc+ frstbase+ scndbase+ shrtstop+ thrdbase+ outfield+ catcher+ yrsallst+ hispan+ black+ whitepop+ blackpop+ hisppop+ pcinc+ gamesyr+ hrunsyr+ atbatsyr+ allstar+ slugavg+ rbisyr+ sbasesyr+ runsyr+ percwhte+ percblck+ perchisp+ blckpb+ hispph+ whtepw+ blckph+ hisppb, data=mlb2))
step(lm(salary~(teamsal+ nl+ years+ games)^2+teamsal+ nl+ years+ games+ atbats+ runs+ hits+ doubles+ triples+ hruns+ rbis+ bavg+ bb+  so+ sbases+ fldperc+ frstbase+ scndbase+ shrtstop+ thrdbase+ outfield+ catcher+ yrsallst+ hispan+ black+ whitepop+ blackpop+ hisppop+ pcinc+ gamesyr+ hrunsyr+ atbatsyr+ allstar+ slugavg+ rbisyr+ sbasesyr+ runsyr+ percwhte+ percblck+ perchisp+ blckpb+ hispph+ whtepw+ blckph+ hisppb, data=mlb2))
model5 <- step(lm(salary~(teamsal+ nl+ years+ games)^2+I(teamsal^2)+ I(nl^2)+ I(years^2)+ I(games^2)+ atbats+ runs+ hits+ doubles+ triples+ hruns+ rbis+ bavg+ bb+  so+ sbases+ fldperc+ frstbase+ scndbase+ shrtstop+ thrdbase+ outfield+ catcher+ yrsallst+ hispan+ black+ whitepop+ blackpop+ hisppop+ pcinc+ gamesyr+ hrunsyr+ atbatsyr+ allstar+ slugavg+ rbisyr+ sbasesyr+ runsyr+ percwhte+ percblck+ perchisp+ blckpb+ hispph+ whtepw+ blckph+ hisppb, data=mlb2))
BIC(model5)
model6 <- step(lm(salary~teamsal+ nl+ years+ games+ I(teamsal^2)+ I(nl^2)+ I(years^2)+ I(games^2)+ (atbats+ runs+ hits+ doubles+ triples+ hruns+ rbis+ bavg+ bb+  so+ sbases+ fldperc)^2+ frstbase+ scndbase+ shrtstop+ thrdbase+ outfield+ catcher+ yrsallst+ hispan+ black+ whitepop+ blackpop+ hisppop+ pcinc+ gamesyr+ hrunsyr+ atbatsyr+ allstar+ slugavg+ rbisyr+ sbasesyr+ runsyr+ percwhte+ percblck+ perchisp+ blckpb+ hispph+ whtepw+ blckph+ hisppb + atbats+ runs+ hits+ doubles+ triples+ hruns+ rbis+ bavg+ bb+  so+ sbases+ fldperc+ frstbase+ scndbase+ shrtstop+ thrdbase+ outfield+ catcher+ yrsallst+ hispan+ black+ whitepop+ blackpop+ hisppop+ pcinc+ gamesyr+ hrunsyr+ atbatsyr+ allstar+ slugavg+ rbisyr+ sbasesyr+ runsyr+ percwhte+ percblck+ perchisp+ blckpb+ hispph+ whtepw+ blckph+ hisppb, data=mlb2))
BIC(model6)
model7 <- step(lm(salary~teamsal+ nl+ years+ games+ I(teamsal^2)+ I(nl^2)+ I(years^2)+ I(games^2)+ atbats+ runs+ hits+ doubles+ triples+ hruns+ rbis+ bavg+ bb+  so+ sbases+ fldperc+ (frstbase+ scndbase+ shrtstop+ thrdbase+ outfield+ catcher+ hispan+ black)^2 + yrsallst+ whitepop+ blackpop+ hisppop+ pcinc+ gamesyr+ hrunsyr+ atbatsyr+ allstar+ slugavg+ rbisyr+ sbasesyr+ runsyr+ percwhte+ percblck+ perchisp+ blckpb+ hispph+ whtepw+ blckph+ hisppb + atbats+ runs+ hits+ doubles+ triples+ hruns+ rbis+ bavg+ bb+  so+ sbases+ fldperc+ frstbase+ scndbase+ shrtstop+ thrdbase+ outfield+ catcher+ yrsallst+ hispan+ black+ whitepop+ blackpop+ hisppop+ pcinc+ gamesyr+ hrunsyr+ atbatsyr+ allstar+ slugavg+ rbisyr+ sbasesyr+ runsyr+ percwhte+ percblck+ perchisp+ blckpb+ hispph+ whtepw+ blckph+ hisppb, data=mlb2))
BIC(model7)
model8 <- lm(salary ~ nl + games + I(teamsal^2) + I(years^2) + I(games^2) + 
               atbats + runs + hits + doubles + triples + hruns + rbis + 
               bavg + bb + so + sbases + fldperc + frstbase + scndbase + 
               thrdbase + outfield + yrsallst + whitepop + hisppop + hrunsyr + 
               slugavg + blckpb + atbats:runs + atbats:hits + atbats:doubles + 
               atbats:triples + atbats:bb + atbats:sbases + runs:doubles + 
               runs:triples + runs:bavg + runs:fldperc + hits:doubles + 
               hits:triples + hits:bavg + hits:bb + hits:so + hits:fldperc + 
               doubles:bavg + doubles:sbases + doubles:fldperc + triples:hruns + 
               triples:rbis + triples:bb + triples:so + triples:sbases + 
               hruns:rbis + hruns:bavg + hruns:bb + rbis:bavg + rbis:bb + 
               rbis:so + rbis:sbases + bavg:bb + bavg:sbases + bavg:fldperc + 
               bb:so + bb:fldperc + so:sbases + so:fldperc, data=mlb2)
summary(model8)
BIC(model8) #The best model

# Question 4:
rental <- wpull('rental')
y90<-ifelse(rental$year == 90, 1, 0)
pctstu <- rental$enroll/rental$pop
rental <- transform(rental, rent1 = ave(log(rent), city, FUN = function(x) c(NA, diff(x))))
rental <- transform(rental, pop1 = ave(log(pop), city, FUN = function(x) c(NA, diff(x))))
rental <- transform(rental, avginc1 = ave(log(avginc), city, FUN = function(x) c(NA, diff(x))))
rental <- transform(rental, pctstu1 = ave(pctstu, city, FUN = function(x) c(NA, diff(x))))
pdrental <- pdata.frame(rental,index=c('city', 'year'))

model6 <- plm(log(rent)~y90+log(pop)+log(avginc)+pctstu, data=pdrental, model="pooling")
summary(model6)

#Q3
data <- subset(pdrental, year != 80)
model66 <- plm(rent1~pop1+avginc1+pctstu1, data=pdrental, model="pooling")
summary(model66)

#Q4
model666 <- plm(log(rent)~y90+log(pop)+log(avginc)+pctstu, data=pdrental, model="within")
summary(model666)

# Question 5:
#2. 
murder <- wpull('murder')

murder <- transform(murder, mrdrte1 = ave(mrdrte, state, FUN = function(x) c(NA, diff(x))))
murder <- transform(murder, exec1 = ave(exec, state, FUN = function(x) c(NA, diff(x))))
murder <- transform(murder, unem1 = ave(unem, state, FUN = function(x) c(NA, diff(x))))
#create new variables cuz cannot run diff(log) in the model

data <- subset(murder, year != 87)
d93<-ifelse(data$year == 93, 1, 0)
pdmurder <- pdata.frame(data,index=c('state', 'year'))
model7 <- plm(mrdrte~exec + unem + d93,model="pooling",data=pdmurder)
summary(model7)

#3 ******Ask prof****
model8 <- plm(mrdrte1 ~ exec1 + unem1, data=pdmurder, model="pooling")
summary(model8)

model8 <- plm(mrdrte ~ exec + unem, data = pdmurder, model = "within")
summary(model8)

model8 <- plm(mrdrte ~ exec + unem + d93, data = pdmurder, model = "within")
summary(model8)

#Q4
install.packages("sandwich")
library(sandwich)
library(lmtest)
coeftest(model8, vcov = vcovHC(model8))

#Q5
pdmurder
max(pdmurder$exec)

#Q6
dataNoTX <- subset(pdmurder, state != "TX")
d93v1<-ifelse(dataNoTX$year == 93, 1, 0)
model77 <- plm(mrdrte ~ exec + unem + d93v1,model="within",data=dataNoTX)
summary(model77)
coeftest(model77, vcov = vcovHC(model77))

#Q7
d93<-ifelse(murder$year == 93, 1, 0)
d90<-ifelse(murder$year == 90, 1, 0)
pdmurder1 <- pdata.frame(murder,index=c('state', 'year'))
model88 <- plm(mrdrte ~ exec + unem + d93 + d90, data=pdmurder1, model="within")
summary(model88)

# Question 6:
# 1
airfare <- wpull('airfare')
pairfare <- pdata.frame(airfare,index=c('id', 'year'))
summary(plm(log(fare)~bmktshr + log(dist) + I(log(dist)^2),model="pooling",data=pairfare))

#2
model9 <- lm(log(fare)~bmktshr + log(dist) + I(log(dist)^2),data=airfare)
confint(model9) 
install.packages("robustbase")
library(robustbase)
model10 <- lmrob(log(fare)~bmktshr + log(dist) + I(log(dist)^2), data = pairfare)
confint(model10)

#3
0.902 / (2*(0.103))
exp(0.902 / (2*(0.103)))

#4
model11 <- plm(log(fare)~bmktshr + log(dist) + I(log(dist)^2),model="within",data=pairfare)
summary(model11)

# Question 7:
#Q1
loanapp <- wpull('loanapp')
model12 <- glm(approve ~ white,family=binomial,data=loanapp)
summary(model12)
model13 <- lm(approve ~ white,data=loanapp)
summary(model13)
predict(model12, data.frame(white = 1), type="response") # probability for white
predict(model12, data.frame(white = 0), type="response") # probability for non-white

#Q2
model14 <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + + vr,family=binomial,data=loanapp)
summary(model14)


# Question 8: 
# Q1 
alcohol <- wpull('alcohol')
nrow(alcohol[alcohol$employ == 1,])/9822
nrow(alcohol[alcohol$abuse == 1,])/9822

#Q2
model15 <- lm(employ ~ abuse, data = alcohol)
summary(model15)

#Q3
model16 <- glm(employ ~ abuse, family=binomial, data = alcohol)
summary(model16)

#Q4
fitted(model15)
fitted(model16)

#Q5
model17 <- lm(employ ~ abuse + age + I(age^2) + educ + I(educ^2) + married + famsize + white + northeast + midwest + south + centcity + outercity + qrt1 + qrt2 + qrt3, data = alcohol)
summary(model17)

#Q6
model18 <- glm(employ ~ abuse + age + I(age^2) + educ + I(educ^2) + married + famsize + white + northeast + midwest + south + centcity + outercity + qrt1 + qrt2 + qrt3, family=binomial, data = alcohol)
summary(model18)

#Q8
model19 <- lm(employ ~ abuse + age + I(age^2) + educ + I(educ^2) + married + famsize + white + northeast + midwest + south + centcity + outercity + qrt1 + qrt2 + qrt3 + mothalc + fathalc, data = alcohol)
summary(model19)

# Question 9
# Q1,2 
fertil1 <- wpull('fertil1')
y74<-ifelse(fertil1$year == 74, 1, 0)
y76<-ifelse(fertil1$year == 76, 1, 0)
y78<-ifelse(fertil1$year == 78, 1, 0)
y80<-ifelse(fertil1$year == 80, 1, 0)
y82<-ifelse(fertil1$year == 82, 1, 0)
y84<-ifelse(fertil1$year == 84, 1, 0)
model20 <- glm(kids ~ educ + age + I(age^2) + black + east + northcen + west + farm + othrural + town + smcity + y74 + y76 + y78 + y80 + y82 + y84 , family=poisson,data=fertil1)
summary(model20)

#Q3
fit <- fitted(model20)
cor(fit,fertil1$kids)^2 
model21 <- lm(kids ~ educ + age + I(age^2) + black + east + northcen + west + farm + othrural + town + smcity + y74 + y76 + y78 + y80 + y82 + y84 , data=fertil1)
summary(model21)
