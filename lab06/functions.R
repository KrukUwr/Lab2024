

preprocess <- function(train_data) {
  
  train_data <- copy(train_data)
  
  dv_product_converter <- caret::dummyVars(~ Product, data = train_data, fullRank  = TRUE)
  dv_gender_converter <- caret::dummyVars(~ Gender, data = train_data, fullRank  = FALSE)
  
  product_dummy_vars <- data.table(
    predict(dv_product_converter, newdata=train_data)
  )
  gender_dummy_vars <- data.table(
    predict(dv_gender_converter, newdata=train_data)
  )
  gender_dummy_vars[is.na(GenderFEMALE), `:=`(
    GenderFEMALE = 0L, 
    GenderMALE = 0L
    )]
  
  train_data <- cbind(train_data, product_dummy_vars)
  train_data <- cbind(train_data, gender_dummy_vars)
  
  train_data[,`:=`(Gender=NULL, Product=NULL)]
  
  imp <- caret::preProcess(x = train_data, method = "medianImpute")
  
  
  models <- list(
    ProductDummyVars = dv_product_converter, 
    GenderDummyVars = dv_gender_converter,
    MedianImpute = imp
  )
  
  return(models)
  
}


fitPreprocessing <- function(models, newdata) {
  
  newdata <- copy(newdata)
  
  product_dummy_vars <- data.table(
    predict(models$ProductDummyVars, newdata=newdata)
  )
  gender_dummy_vars <- data.table(
    predict(models$GenderDummyVars, newdata=newdata)
  )
  gender_dummy_vars[is.na(GenderFEMALE), `:=`(
    GenderFEMALE = 0L, 
    GenderMALE = 0L
  )]
  
  newdata <- cbind(newdata, product_dummy_vars)
  newdata <- cbind(newdata, gender_dummy_vars)
  
  newdata[,`:=`(CaseId=NULL, Gender=NULL, Product=NULL)]
  
  newdata <- predict(models$MedianImpute, newdata)
  
  return(newdata)
  
  
  
}

