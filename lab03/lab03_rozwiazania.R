library(data.table)
library(ggplot2)
library(caret)
library(Metrics)

source("lab03/lab3-data-preparation.R") # load & prepare data
source("lab03/lab03_funkcje.R") # script with own functions

# Zad. 1 ------------------------------------------------------------------


## Sposob ekspercki

### funkcja setExpertSegment ze skryptu lab03/lab03_funkcje.R
cases_train[, SegmentExpert := setExpertSegment(LastPaymentAmount)]
cases_valid[, SegmentExpert := setExpertSegment(LastPaymentAmount)]
cases_test[, SegmentExpert := setExpertSegment(LastPaymentAmount)]

cases_train[,.N, .(SegmentExpert)]
cases_valid[,.N, .(SegmentExpert)]
cases_test[,.N, .(SegmentExpert)]



## za pomocą modelu k-means

### preprocessing

set.seed(123)

features <- c("TOA", "DPD")
n_groups <- 3

cases_model <- cases_train[,.SD, .SDcols=features]

preprocess <- caret::preProcess(cases_model, method = c("medianImpute", "range"), na.remove=TRUE)

cases_model <- predict(preprocess, newdata=cases_model)


### model

model <- kmeans(x = cases_model, centers = n_groups)


### predykcje

cases_train[,SegmentModel := predictKMeans(model = model, newdata = cases_train, preprocess = preprocess)]
cases_valid[,SegmentModel := predictKMeans(model = model, newdata = cases_valid, preprocess = preprocess)]
cases_test[,SegmentModel := predictKMeans(model = model, newdata = cases_test, preprocess = preprocess)]

#### licznosci segmentow w poszczegolnych zbiorach
cases_train[,.N, SegmentModel]
cases_valid[,.N, SegmentModel]
cases_test[,.N, SegmentModel]

#### wykres na przykładzie zbioru test
ggplot(data = cases_test, aes(x = TOA, y = DPD, color = factor(SegmentModel, levels = c("1", "2", "3")))) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "bottom")



# Zad. 2 ------------------------------------------------------------------

## segmentacja ekspercka

ncases_in_segments <- cases_valid[,.(NCasesSegment=.N), SegmentExpert]

ncases_in_groups <- cases_valid[targets_valid][IfPayment==1, .(NCases=.N), .(SegmentExpert)]

proportions <- ncases_in_groups[ncases_in_segments, on = "SegmentExpert"][
  ,.(SegmentExpert, PropPayingCases=NCases/NCasesSegment)]

setorder(proportions, SegmentExpert)
print(proportions)


ggplot(data = proportions) +
  geom_bar(aes(x = SegmentExpert, y = PropPayingCases), stat="identity")

# Coefficient of Variation
sd(proportions$PropPayingCases) / mean(proportions$PropPayingCases)


## segmentacja z modelu

ncases_in_segments <- cases_valid[,.(NCasesSegment=.N), SegmentModel]

ncases_in_groups <- cases_valid[targets_valid][IfPayment==1, .(NCases=.N), .(SegmentModel)]

proportions <- ncases_in_groups[ncases_in_segments, on = "SegmentModel"][
  ,.(SegmentModel, PropCases=NCases/NCasesSegment)]

setorder(proportions, SegmentModel)

print(proportions)

ggplot(data = proportions) +
  geom_bar(aes(x = SegmentModel, y = PropCases), stat="identity") +
  ylim(c(0, 0.5))

# Coefficient of Variation
sd(proportions$PropCases) / mean(proportions$PropCases)


# Lepsza segmentacja: Segmentacja ekspercka. Większe zróżnicowanie danych 
# (patrz: współczynnik zmienności) pod kątem udziału spraw płacących. 
# Potwierdza to również ocena wizualna przy użyciu wykresów.



# Zad. 3 ------------------------------------------------------------------

## przygotowaine danych


# usunięcie spacji z nazw produktu
cases_train[,Product:=gsub("\\s", "", Product)]
cases_valid[,Product:=gsub("\\s", "", Product)]
cases_test[,Product:=gsub("\\s", "", Product)]

reference <- rbindlist(list(cases_train, cases_valid))
reference_targets <- rbindlist(list(targets_train, targets_valid))
test_set <- copy(cases_test)

# Przekształcenie zmiennych nienumerycznych w numeryczne
dummy <- caret::dummyVars(~ Gender + Product, data=reference, levelsOnly=FALSE, fullRank=TRUE)
dummy_vars_reference <- data.table(predict(dummy, newdata=reference))
dummy_vars_test <- data.table(predict(dummy, newdata=test_set))

reference <- cbind(reference, dummy_vars_reference)
test_set <- cbind(test_set, dummy_vars_test)

reference[,`:=`(Product=NULL, Gender=NULL, SegmentModel=NULL)]
test_set[,`:=`(Product=NULL, Gender=NULL, SegmentModel=NULL)]


# uzupełnienie NA i transfomracja zmiennych do tej samej skali

cols_to_preprocess <- setdiff(names(reference), c("CaseId", "SegmentExpert"))

preprocess <- preProcess(reference[,.SD,.SDcols=cols_to_preprocess], method = c("medianImpute", "range"))

reference <- predict(preprocess, newdata=reference)
test_set <- predict(preprocess, newdata=test_set)

summary(reference)
summary(test_set)


## Wyznaczenie ważności zmiennych do modelu knn:

### korelacje z jedną ze zmiennych objaśnianych


abscorr_with_target <- abs(cor(
  x = reference[,.SD,.SDcols=cols_to_preprocess], 
  y = reference_targets[,IfPayment]
))


variables_cor <- data.table(
  Name = rownames(abscorr_with_target),
  Cor = abscorr_with_target[,1]
)

setorder(variables_cor, -Cor)

print(variables_cor)

### Grupy zmiennych wg korelacji. Szukamy grup zmiennych skorelowanych między sobą.

cors_between_variables <- cor(reference[,.SD,.SDcols=cols_to_preprocess])

dist_vars <- dist(cors_between_variables, method="euclidean")

hcl_model <- hclust(dist_vars, method = "centroid")

plot(hcl_model)

k <- 5

variable_groups <- cutree(hcl_model, k=k)

variable_groups <- data.table(
  Name = names(variable_groups),
  Group = variable_groups
)

variable_groups


### Zebranie powyższych danych w całość:

variables <- variables_cor[variable_groups, on="Name"]
setorder(variables, Group, -Cor)
variables


## Model knn w segmentach. Referencją w modelowaniu segmentu NoSegment jest cały zbiór referencyjny.

segments <- reference[,unique(SegmentExpert)]

# bierzemy pierwszą ("najlepsza") zmienną z każdej grupy
selected_variables <- variables[,.(Variables = head(Name, 1)), .(Group) ]
selected_variables <- selected_variables$Variables

reference_model <- reference[reference_targets[,.(CaseId, SumOfPayments)], on = c("CaseId" = "CaseId")]
test_set_model <- test_set[targets_test[,.(CaseId, SumOfPayments)], on = c("CaseId" = "CaseId")]

predictions <- data.table()

for(seg in segments) {
  
  # Test set in segment
  seg_test_caseids <- test_set_model[SegmentExpert==seg, CaseId]
  seg_test <- test_set_model[SegmentExpert==seg, .SD, .SDcols=selected_variables]
  seg_test_targets <- test_set_model[SegmentExpert==seg, SumOfPayments]
  
  # Reference in segment
  if(seg == "NoSegment") {
    
    seg_ref <- reference_model[, .SD, .SDcols=selected_variables]
    seg_ref_targets <- reference_model[, SumOfPayments]
    
  } else {
    
    seg_ref <- reference_model[SegmentExpert==seg, .SD, .SDcols=selected_variables]
    seg_ref_targets <- reference_model[SegmentExpert==seg, SumOfPayments]
    
  }
  
  # Model
  knn_model <- caret::knnregTrain(
    train=seg_ref, 
    test=seg_test, 
    y = seg_ref_targets, 
    k = 5)
  
  predictions_in_segment <- data.table(
    CaseId = seg_test_caseids, 
    Segment=seg, 
    Real = seg_test_targets, 
    Pred = knn_model
  )
  
  predictions <- rbindlist(list(predictions, predictions_in_segment))
  
}

# Errors:
Metrics::mae(predictions$Real, predictions$Pred)
Metrics::rmse(predictions$Real, predictions$Pred)






