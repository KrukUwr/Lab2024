########################################################  Start ####
rm(list=ls())

setwd("")
source(".Rprofile")

load("KrukUWr2024.RData")
#install.packages("polars", repos = "https://rpolars.r-universe.dev")
library(data.table)
library(ggplot2)
library(dplyr)
library(scales)
#library(polars)
#polars_info()

set.seed(1)

# Cases = pl$DataFrame(cases)
# Events = pl$DataFrame(events)
Cases = data.table(cases)
Events = data.table(events)




# sampling

n = Cases[,.N]
Cases <- copy(Cases[sample(1:n, 0.5*n),])



######################################################## Zadanie 1 - data #### 

# A glance at the data

summary(cases)
summary(events)


# Decoding variables

Cases[,CreditCard := ifelse(Product=="Credit card",1,0)]
Cases[,Female := ifelse(Gender=="FEMALE",1,0)]


# Handling missing data

Variables = c(         "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary",
                       "CreditCard",
                       "Female",
                       "Bailiff",
                       "ClosedExecution"
                       )

nullCounts <- lapply(Cases[,.SD,.SDcols=Variables], function(x) sum(is.na(x)))



# Imputation with avg

variables = c(        "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary"
                       )
                                    
for (variable in Variables) {      ## variable = 'Age'
    if (eval(parse(text=paste("nullCounts$",variable,sep=""))) > 0) {
          avg <- eval(parse(text=paste("mean(Cases[,",variable,"],na.rm=TRUE)",sep="")))
          eval(parse(text=paste("Cases[is.na(",variable,"), ",variable,":=avg]",sep="")))
    }           
}



# Other imputations

Cases[is.na(Female),Female:= ifelse(runif(nullCounts$Female,0,1)<Cases[,mean(Female,na.rm=TRUE)],1,0)]

Cases[is.na(Bailiff),Bailiff:= ifelse(runif(nullCounts$Bailiff,0,1)<Cases[,mean(Bailiff,na.rm=TRUE)],1,0)]

Cases[is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0]
Cases[is.na(ClosedExecution), ClosedExecution:= ifelse(runif(nullCounts$ClosedExecution,0,1)<Cases[,mean(ClosedExecution,na.rm=TRUE)],1,0)]



# Adding payment data from events

setkey(Cases,CaseId)
setkey(Events,CaseId)

Payments = Events[Month <= 6,.(Payments6M = sum(ifelse(is.na(PaymentAmount),0,PaymentAmount))),by=.(CaseId)]
setkey(Payments,CaseId)

Cases = Cases[Payments[,.(CaseId,Payments6M)],nomatch=0][,SR6M := Payments6M/TOA]



# Proportion of tail data to be removed from the dataset

summary(Cases)

Proportion = 0.001

Cases = Cases[LoanAmount<quantile(Cases[,LoanAmount], probs=1-Proportion),]
Cases = Cases[DPD<quantile(Cases[,DPD], probs=1-Proportion),]
Cases = Cases[LastPaymentAmount<quantile(Cases[,LastPaymentAmount], probs=1-Proportion),]
Cases = Cases[SR6M<quantile(Cases[,SR6M], probs=1-Proportion),]

Cases = Cases[SR6M >= 0,]



# Standardization of variables

Variables = c(         "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "Other",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary"
                       #"CreditCard",
                       #"Female",
                       #"Bailiff",
                       #"ClosedExecution"
                       )


CasesStd <- data.table(cbind(CaseId=Cases[,CaseId], SR6M=Cases[,SR6M], scale(Cases[,.SD,.SDcols = Variables])))



summary(CasesStd)
summary(Events)


errorsSummary=data.table()
errorsSummary[,k:=c(1,2,3,4,5,10,20,30,40,50,100,150,200,250,300,1000)]
errorsSummary[,Complexity:=1/k]




######################################################## Zadanie 2 - TRN=TST #### 

k <- 5
Variables = c("TOA","M_LastPaymentToImportDate")
Variables = c(        "LoanAmount",
                       "TOA",
                       #"Principal",
                       #"Interest",
                       #"Other",
                       "D_ContractDateToImportDate",
                       #"DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita"
                       #"MeanSalary"
                       #"CreditCard",
                       #"Female",
                       #"Bailiff",
                       #"ClosedExecution"
                       )

kNearest = FNN::knn.reg(
  train=CasesStd[, .SD, .SDcols=Variables],
  test=CasesStd[, .SD, .SDcols=Variables],
  y=CasesStd$SR6M, # train
  k=k, algorithm="kd_tree")

error = sum((kNearest$pred-CasesStd[,SR6M])^2)/dim(CasesStd)[1]


knnError = data.table()
for (k in c(1,2,3,4,5,10,20,30,40,50,100,150,200,250,300,1000)) {      # seq(from=1, to=29, by=2)
  kNearest = FNN::knn.reg(
    train=CasesStd[, .SD, .SDcols=Variables],
    test=CasesStd[, .SD, .SDcols=Variables],
    y=CasesStd$SR6M,
    k=k, algorithm="kd_tree")

  knnError = rbindlist(list(knnError,
    data.table(K=k, Error=sum((kNearest$pred-CasesStd[,SR6M])^2)/dim(CasesStd)[1])))
  
  print(k)
}

knnError[,Complexity:=1/K]


error_graph = ggplot(data=knnError, aes(x=Complexity, y=Error)) + geom_point()
error_graph + geom_hline(yintercept=min(knnError$Error), linetype="dashed", color = "red")


# fill errorsSummary
errorsSummary[,TRNequalsTST:=knnError[,Error]]


# Identical cases

temp = Cases[,.SD,.SDcols=c("CaseId",Variables)]
setkey(temp,TOA, M_LastPaymentToImportDate)
temp[temp, nomatch=0][,.N,by=CaseId][N>1,]

Cases[CaseId==8388230,.SD,.SDcols=c(Variables)]
Cases[TOA==0.02 & M_LastPaymentToImportDate == 50,]





######################################################## Zadanie 3 - TRN/VAL/TST sets #### 

n = CasesStd[, .N]
CasesStd[, Set:=sample(1:3, n, replace=TRUE, prob=c(0.5,0.25,0.25))] 


# 1 - train
# 2 - valid
# 3 - test

# Choice of k

knnError = data.table()
for (k in c(1,2,3,4,5,10,20,30,40,50,100,150,200,250,300,1000)) { # to=29 by=2
  kNearest = FNN::knn.reg(
    train=CasesStd[Set == 1, .SD, .SDcols=Variables],
    test=CasesStd[Set == 2, .SD, .SDcols=Variables],
    y=CasesStd[Set == 1, SR6M],
    k=k, algorithm="kd_tree")


  knnError = rbindlist(list(knnError,
    data.table(K=k, Error=sum((kNearest$pred-CasesStd[Set == 2,SR6M])^2)/dim(CasesStd[Set == 2,])[1])))
  print(k)
}

knnError[,Complexity:=1/K]

error_graph = ggplot(data=knnError, aes(x=Complexity, y=Error)) + geom_point()
error_graph + geom_hline(yintercept=min(knnError$Error), linetype="dashed", color = "red")


# fill errorsSummary
errorsSummary[,TRN_VAL:=knnError[,Error]]



# Estimating generalisation error with a test sample

kNearest = FNN::knn.reg(
  train=CasesStd[Set < 3, .SD, .SDcols=Variables],
  test=CasesStd[Set == 3, .SD, .SDcols=Variables],
  y=CasesStd[Set < 3,SR6M],
  k=100, algorithm="kd_tree")

error = sum((kNearest$pred-CasesStd[Set == 3,SR6M])^2)/dim(CasesStd[Set == 3,])[1]
(error)



######################################################## Zadanie 4  - crossvalidation ####

# 5-fold cross-validation
n = CasesStd[, .N]
f = 5
CasesStd[, Fold:=sample(1:f, n, replace=TRUE)]

Error = c()
for (i in 1:f) {
    kNearest = FNN::knn.reg(
      train=CasesStd[Fold != i, .SD, .SDcols=Variables],
      test=CasesStd[Fold == i, .SD, .SDcols=Variables],
      y=CasesStd[Fold != i, SR6M],
      k=100, algorithm="kd_tree")

      Error = c(Error,sum((kNearest$pred-CasesStd[Fold == i,SR6M])^2)) 
  }

generalization_error = sum(Error)/n



#Choice of k

knnError = data.table()
for (k in c(1,2,3,4,5,10,20,30,40,50,100,150,200,250,300,1000)) {
  Error = c()
  for (i in 1:f) {
      kNearest = FNN::knn.reg(
        train=CasesStd[Fold != i, .SD, .SDcols=Variables],
        test=CasesStd[Fold == i, .SD, .SDcols=Variables],
        y=CasesStd[Fold != i, SR6M],
        k=k, algorithm="kd_tree")
  
        Error = c(Error,sum((kNearest$pred-CasesStd[Fold == i,SR6M])^2)) 
    }
    knnError = rbindlist(list(knnError,
    data.table(K=k, Error = sum(Error)/n)))
    print(k)
}

knnError[,Complexity:=1/K]

error_graph = ggplot(data=knnError, aes(x=Complexity, y=Error)) + geom_point()
error_graph + geom_hline(yintercept=min(knnError$Error), linetype="dashed", color = "red")


# fill errorsSummary
errorsSummary[,cross_fiveFolds:=knnError[,Error]]




######################################################## Summary ####


graph = errorsSummary[k>0,] %>%
  ggplot(aes(Complexity)) +
  geom_line(aes(y = TRNequalsTST, colour = 'TRN = TST')) + 
  geom_line(aes(y = TRN_VAL, colour = 'TRN/VAL/TST')) + 
  geom_line(aes(y = cross_fiveFolds, colour = 'Crossvalidation')) + 
  ggtitle("Forecast error estimate") +
  labs(colour = "") +
  ylab("Error") +
  scale_x_continuous(labels = ~paste(., round(1/.,digits=2), sep = "\n"), name = "Complexity\nk") +
  geom_hline(yintercept=min(errorsSummary$cross_fiveFolds), linetype="dashed", color = "orange")
graph




######################################################## Zadanie 5 - bootstrap ####

n = CasesStd[, .N]
b = 30

pb = winProgressBar(title = "progress bar", min = 0,
                     max = 1, width = 300)

Error = c()
for (i in 1:b) {

    Boot = sample(1:n, n, replace=TRUE)
    kNearest = FNN::knn.reg(
      train=CasesStd[Boot, .SD, .SDcols=Variables],
      test=CasesStd[, .SD, .SDcols=Variables],
      y=CasesStd[Boot, SR6M],
      k=100, algorithm="kd_tree")

      Error = c(Error,sum((kNearest$pred-CasesStd[,SR6M])^2)) 
      setWinProgressBar(pb, i/b, title=paste( round(i*1.0/b*100, 0), "% done"))      

  }

close(pb)
generalization_error = sum(Error)/(n*b)
(generalization_error)

# Leave one-out bootstrap

n = CasesStd[, .N]
CasesStd[,Element:=1:n]

b = 100

pb = winProgressBar(title = "progress bar", min = 0,
                     max = 1, width = 300)

Error = c()
NoOfCases = data.frame()
for (i in 1:b) {

    Boot = sample(1:n, n, replace=TRUE)
    kNearest = FNN::knn.reg(
      train=CasesStd[Boot, .SD, .SDcols=Variables],
      test=CasesStd[, .SD, .SDcols=Variables],
      y=CasesStd[Boot, SR6M],
      k=100, algorithm="kd_tree")
      Rows = CasesStd[is.element(Element, Boot)==FALSE,Element]
      Error = c(Error,sum((kNearest$pred[Rows]-CasesStd[Rows,SR6M])^2)/length(Rows)) 
      
      setWinProgressBar(pb, i/b, title=paste( round(i*1.0/b*100, 0), "% done"))      

  }
close(pb)
generalization_error = sum(Error)/(b)




############################################### Checking the avg predictor ####
options(scipen=999)


# podejście niepoprawne

Avg = Cases[,mean(SR6M)]

# średni błąd kwadratowy per case: mean((w-p)^2)
error1 = sum((Avg-Cases[,SR6M])^2)/dim(Cases)[1] 
(error1)
# średni błąd względny per case: mean((w-p)/p)    # w mianowniku p, żeby nie pojawiło się dzielenie przez 0
error2 = sum(abs(Cases[,SR6M]-Avg)/Avg)/dim(Cases)[1]
(error2)
# średni błąd względny per całość: ((sum(w)-sum(p))/sum(w)
error3 = abs(sum(Cases[,SR6M*TOA])-sum(Cases[,Avg*TOA]))/sum(Cases[,SR6M*TOA])
(error3)
error3 = Cases[,.(abs(sum(SR6M*TOA)-sum(Avg*TOA))/sum(SR6M*TOA))]    # to samo, tylko z użyciem notacji data.table
(error3)




# podejście poprawne

# Train, test and validation datasets

n = Cases[, .N]
Cases[, Set:=sample(1:3, n, replace=TRUE, prob=c(0.5,0.25,0.25))] 


#1 - train
#2 - valid
#3 - test

AvgNonTest = mean(Cases[Set == 1 | Set == 2,SR6M])

# średni błąd kwadratowy per case: mean((w-p)^2)
error1 = sum((AvgNonTest-Cases[Set == 3,SR6M])^2)/dim(Cases[Set == 3,])[1] 
(error1)
# średni błąd względny per case: mean((w-p)/p)    # w mianowniku p, żeby nie pojawiło się dzielenie przez 0
error2 = sum(abs(Cases[Set == 3,SR6M]-AvgNonTest)/AvgNonTest)/dim(Cases[Set == 3,])[1]
(error2)
# średni błąd względny per całość: ((sum(w)-sum(p))/sum(w)
error3 = abs(sum(Cases[Set == 3,SR6M*TOA])-sum(Cases[Set == 3,AvgNonTest*TOA]))/sum(Cases[Set == 3,SR6M*TOA])
(error3)
error3 = Cases[Set == 3,.(abs(sum(SR6M*TOA)-sum(AvgNonTest*TOA))/sum(SR6M*TOA))]    # to samo, tylko z użyciem notacji data.table
(error3)

