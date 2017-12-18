#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
  wle = read.csv("pml-training.csv", stringsAsFactors = FALSE)
  library(caret)
  library(ggplot2)
  library(foreach)
  library(randomForest)
  features = colnames(wle)
  mv = rep(NA,ncol(wle))
  for (i in 1:ncol(wle)) {
    nan = sum(is.na(wle[,i]) | wle[,i]=='' | wle[,i]==' ')
    mv[i] = nan/nrow(wle)
  }
  
  names(mv) = features  # match the header with the missing value percentage
  to_drop = names(mv[mv>0.9]) # will remove the features with more than 90% missing value
  ind = match(to_drop, features)  
  wle_new = wle[,-ind]
  
  ### Remove the unnecessary features
  wle_new = wle_new[,7:60]
 
   ### correlation analasis
  corr.matrix = cor(wle_new[, 1:53], use = "pairwise.complete.obs")
  corr.matrix[is.na(corr.matrix)] = 0
  corr_features = foreach(i = 1:nrow(corr.matrix))  %do% {
    rownames(corr.matrix[corr.matrix[,i] > 0.9,])
  }
  corr_features = corr_features[sapply(corr_features, function(x) length(x) > 0 )] ## remove empty rows
  corr_features = unique(corr_features) ## remove duplicates
  # create new features
  new_gyro = wle_new$gyros_dumbbell_z * wle_new$gyros_forearm_z
  new_belt = wle_new$total_accel_belt - wle_new$accel_belt_y - wle_new$roll_belt
  wle_new$new_gyro = new_gyro
  wle_new$new_belt = new_belt
  # remove some features
  cor_f = c(corr_features[[1]], corr_features[[2]])
  cor_ind = match(cor_f, colnames(wle_new))
  wle_new = wle_new[,-cor_ind]
  # Split the data
  training = wle_new
  training$classe = as.factor(training$classe)
  set.seed(12345)
  inTrain = createDataPartition(y = training$classe, p=0.5, list = FALSE)
  training = training[inTrain,]
  inTrain = createDataPartition(y = training$classe, p=0.5, list = FALSE)
  training = training[inTrain,]
  inTrain = createDataPartition(y = training$classe, p=0.5, list = FALSE)
  training_train = training[inTrain,]
  training_cv = training[-inTrain,]
  
  
  output$plot2 <- renderPlot({
    set.seed(12345)
    ntree = input$numeric
    mtry = input$sliderx
    nodesize = input$sliderY
    rf_fit = randomForest(classe ~., data = training_train, ntree=ntree, mtry=mtry, nodesize = nodesize)
    ## Confustion matrix
    confusion = rf_fit$confusion
    #### Plot Confusion matrix
    TClass <- factor(c(rep('A',5), rep('B',5), rep('C',5), rep('D', 5), rep('E', 5)))
    PClass <- factor(c(rep(LETTERS[1:5],5)))
    Y <- c(confusion[5:1,1], confusion[5:1,2],confusion[5:1,3],confusion[5:1,4],confusion[5:1,5])
    df <- data.frame(TClass, PClass, Y)
    ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
      geom_tile(aes(fill = Y), colour = "white") +
      geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
      scale_fill_gradient(low = "green", high = "orange") +
      theme_bw() + theme(legend.position = "none") + 
      ggtitle("Confustion Matrix for Random Forest") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = 'True Activity', y = 'Predicted Activity')
   })
   
   output$plot1 <- renderPlot({
     set.seed(12345)
     ntree = input$numeric
     mtry = input$sliderx
     nodesize = input$sliderY
     rf_fit = randomForest(classe ~., data = training_train, ntree=ntree, mtry=mtry, nodesize = nodesize)
     # Feature importance
     var_importance = rf_fit$importance
     barchart(var_importance)
   })
   
   
   model_Train = reactive({
     set.seed(12345)
     ntree = input$numeric
     mtry = input$sliderx
     nodesize = input$sliderY
     rf_fit = randomForest(classe ~., data = training_train, ntree=ntree, mtry=mtry, nodesize = nodesize)
     OOB_Error = mean(rf_fit$err.rate)
     1-OOB_Error
   })
   
   model_test = reactive({
     set.seed(12345)
     ntree = input$numeric
     mtry = input$sliderx
     nodesize = input$sliderY
     rf_fit = randomForest(classe ~., data = training_train, ntree=ntree, mtry=mtry, nodesize = nodesize)
     pred_rf = predict(rf_fit, newdata = training_cv, type = 'response')
     table(prediction = pred_rf, actual = training_cv$classe)
     Pred_Acuracy = sum(diag(table(prediction = pred_rf, actual = training_cv$classe)))/length(training_cv$classe)
     Pred_Acuracy
   })
   
   output$pred1 = renderText({
     model_Train()
   })
   output$pred2 = renderText({
     model_test()
   })
   
})



