part_ord_reg = function(data, matrix, response_index, strata){
  hasse(apply(matrix, 2, as.logical), parameters = list(arrow = "backward"))
  
  trained = train_models(matrix, data, response_index, strata)
  final_predictions = predict_por(data, matrix, response_index, strata, trained)
  
  table(y = data[,response_index], Predictions = final_predictions)
}

part_ord_reg_tt = function(training_data, test_data, matrix, response_index, train_strata, test_strata){
  hasse(apply(matrix, 2, as.logical), parameters = list(arrow = "backward"))
  
  trained = train_models(matrix, training_data, response_index, train_strata)
  
  final_predictions = predict_por(training_data, matrix, response_index, train_strata, trained)
  train_table = table(y = training_data[,response_index], "Training Predictions" = final_predictions)
  
  final_predictions = predict_por(test_data, matrix, response_index, test_strata, trained)
  test_table = table(y = test_data[,response_index], "Testing Predictions" = final_predictions)
  
  print(train_table)
  
  print(test_table)
}

indv_part_ord_reg_graph = function(full_data, patient_data, matrix, response_index, strata, patient_strata){
  trained = train_models(matrix, full_data, response_index, strata)
  predict = predict_new_individual(trained, patient_data, matrix, patient_strata)
  full_graph(matrix, predict$step_0_probs, predict$step_1_probs, predict$step_2_probs, predict$final_probs)
}