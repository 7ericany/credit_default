# calculate confusion matrix
conf_matrix <- confusionMatrix(predicted_class, actual_class)

# extract precision and recall from the confusion matrix 
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
