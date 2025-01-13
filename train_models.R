unique_check = function(dataset, response_index) {
  removed_list = character(0)
  
  unique_counts = sapply(dataset[-response_index], function(col) length(unique(col)))
  
  colnames_to_remove = colnames(dataset[-response_index])[unique_counts == 1]
  
  if (length(colnames_to_remove) == 0){
    colnames_to_remove = "None"
  }
  
  removed_list = colnames_to_remove
  
  new_dataset = dataset[!colnames(dataset) %in% removed_list]
  
  return(list(new_data = new_dataset, removed_vars = removed_list))
}

# Train all the models needed to predict the data
train_models = function(matrix, dataset, response_index, strata_var){
  models = list()
  removed_variables = list()
  
  unique_check_1 = unique_check(dataset, response_index)
  
  dataset_1 = unique_check_1$new_data
  removed_1 = unique_check_1$removed_vars
  
  removed_variables = append(removed_variables, removed_1)
  
  step_0_output = step_0_total(matrix, dataset_1, response_index, strata_var)
  
  models[[length(models)+1]] = step_0_output$model
  
  step_1_models = list()
  step_2_models = list()
  
  step_1_removed = list()
  step_2_removed = list()
  
  for (i in 1:length(step_0_output$matrices)){
    step_0_original = step_0_output$original_data[[i]]
    step_0_new = step_0_output$new_data[[i]]
    
    new_matrix = step_0_output$matrices[[i]]
    
    step_1_check = unique_check(step_0_original, response_index)
    step_1_original_checked = step_1_check$new_data
    step_1_removed[[length(step_1_removed)+1]] = step_1_check$removed_vars
    step_1_output = step_1_total(new_matrix, step_1_original_checked, step_0_new, response_index)
    
    step_1_models[[length(step_1_models)+1]] = step_1_output$model
    
    if(length(step_1_output) != 1){
      for (j in 1:length(step_1_output$matrices)){
        step_1_original = step_1_output$original_data[[j]]
        step_1_new = step_1_output$new_data[[j]]
        
        new_matrix_1 = step_1_output$matrices[[j]]
        step_2_antichain = step_1_output$antichain_parts[[j]]
        
        step_2_check = unique_check(step_1_original, response_index)
        step_2_original_checked = step_2_check$new_data
        step_2_removed[[length(step_2_removed)+1]] = step_2_check$removed_vars
        
        step_2_output = step_2_total(new_matrix_1, step_2_original_checked, step_1_new, step_2_antichain, response_index)
        step_2_models[[length(step_2_models)+1]] = step_2_output
      }
    }
  }
  
  models[[length(models)+1]] = step_1_models
  models[[length(models)+1]] = step_2_models
  
  removed_variables[[length(removed_variables)+1]] = step_1_removed
  removed_variables[[length(removed_variables)+1]] = step_2_removed
  
  
  return(list(models = models, removed_variables = removed_variables))
}