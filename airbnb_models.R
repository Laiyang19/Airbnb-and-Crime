library(tidyverse)
library(plm)
library(lmtest)
library(panelView)

final <- read.csv("airbnb_final.csv")

##plot Airbnb's presence in Boston
panelView(socdis~udensity, data = final,
          index = c("CT_ID_10", "year"), axis.lab = "time", 
          main = "Airbnb density", background = "white")

panelView(socdis~parcelpct, data = final,
          index = c("CT_ID_10", "year"), axis.lab = "time", 
          main = "Airbnb penetration", background = "white")

panelView(socdis~rperunit, data = final,
          index = c("CT_ID_10", "year"), axis.lab = "time", 
          main = "Airbnb usage", background = "white")

##baseline models
m1 <- plm(privateconflict~udensity+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m1, vcov=vcovHC(m1, type="sss", cluster="group"))

m2 <- plm(socdis~udensity+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m2, vcov=vcovHC(m2, type="sss", cluster="group"))

m3 <- plm(violence~udensity+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m3, vcov=vcovHC(m3, type="sss", cluster="group"))

##Robustness check 1
#alternative measures: penetration
m1 <- plm(privateconflict~parcelpct+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m1, vcov=vcovHC(m1, type="sss", cluster="group"))

m2 <- plm(socdis~parcelpct+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m2, vcov=vcovHC(m2, type="sss", cluster="group"))

m3 <- plm(violence~parcelpct+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m3, vcov=vcovHC(m3, type="sss", cluster="group"))

#alternative measures: usage
m1 <- plm(privateconflict~rperunit+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m1, vcov=vcovHC(m1, type="sss", cluster="group"))

m2 <- plm(socdis~rperunit+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m2, vcov=vcovHC(m2, type="sss", cluster="group"))

m3 <- plm(violence~rperunit+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m3, vcov=vcovHC(m3, type="sss", cluster="group"))


##Robustness check 2
#lagged independent variables: one-year lag
m1 <- plm(privateconflict~udensity_lag1+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m1, vcov=vcovHC(m1, type="sss", cluster="group"))

m2 <- plm(socdis~udensity_lag1+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m2, vcov=vcovHC(m2, type="sss", cluster="group"))

m3 <- plm(violence~udensity_lag1+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m3, vcov=vcovHC(m3, type="sss", cluster="group"))

m1 <- plm(privateconflict~parcelpct_lag1+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m1, vcov=vcovHC(m1, type="sss", cluster="group"))

m2 <- plm(socdis~parcelpct_lag1+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m2, vcov=vcovHC(m2, type="sss", cluster="group"))

m3 <- plm(violence~parcelpct_lag1+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m3, vcov=vcovHC(m3, type="sss", cluster="group"))

m1 <- plm(privateconflict~rperunit_lag1+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m1, vcov=vcovHC(m1, type="sss", cluster="group"))

m2 <- plm(socdis~rperunit_lag1+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m2, vcov=vcovHC(m2, type="sss", cluster="group"))

m3 <- plm(violence~rperunit_lag1+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m3, vcov=vcovHC(m3, type="sss", cluster="group"))


#lagged independent variables: one-year lag
m1 <- plm(privateconflict~udensity_lag2+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m1, vcov=vcovHC(m1, type="sss", cluster="group"))

m2 <- plm(socdis~udensity_lag2+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m2, vcov=vcovHC(m2, type="sss", cluster="group"))

m3 <- plm(violence~udensity_lag2+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m3, vcov=vcovHC(m3, type="sss", cluster="group"))

m1 <- plm(privateconflict~parcelpct_lag2+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m1, vcov=vcovHC(m1, type="sss", cluster="group"))

m2 <- plm(socdis~parcelpct_lag2+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m2, vcov=vcovHC(m2, type="sss", cluster="group"))

m3 <- plm(violence~parcelpct_lag2+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m3, vcov=vcovHC(m3, type="sss", cluster="group"))

m1 <- plm(privateconflict~rperunit_lag2+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m1, vcov=vcovHC(m1, type="sss", cluster="group"))

m2 <- plm(socdis~rperunit_lag2+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m2, vcov=vcovHC(m2, type="sss", cluster="group"))

m3 <- plm(violence~rperunit_lag2+medincome, 
          data = final, model = "within", 
          effect = "twoways", index = c("CT_ID_10", "year"))
coeftest(m3, vcov=vcovHC(m3, type="sss", cluster="group"))
