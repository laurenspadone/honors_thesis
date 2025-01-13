# Predict the values using the created models
predict_por = function(data, matrix, response_index, strata_var_list){
  
  trained = train_models(matrix, data, response_index, strata_var_list)
  models = trained$models
  removed_vars = trained$removed_variables
  
  final_predictions = rep(-1, nrow(data))
  step_0_model = models[[1]]
  step_0_removed = removed_vars[[1]]
  
  components = split_components(matrix)
  
  for (i in 1:nrow(data)){
    row = data[i,-response_index, drop=FALSE]
    row = row[, !colnames(row) %in% step_0_removed, drop=FALSE]
    
    if (length(step_0_model) == 1){
      prediction = 1
    }else{
      prediction = predict(step_0_model, newdata = row)
      
      if (is.factor(prediction) == FALSE){
        if (prediction < 0){
          prediction = 1
        }else{
          prediction = 2
        }
      }}
    
    component_num = prediction
    component = components[[prediction]]
    
    if (length(component) == 1){
      final_predictions[i] = component
    }else{
      new_matrix = split_matrix(component_num, matrix, components)
      antichain_list = appending_chains(new_matrix)
      
      
      step_1_models = models[[2]]
      step_1_total_removed = removed_vars[[2]]
      
      step_1_model = step_1_models[[component_num]]
      step_1_removed = step_1_total_removed[[component_num]]
      
      row = row[, !colnames(row) %in% step_1_removed, drop=FALSE]
      
      if (length(antichain_list) == 2){
        row$strata_var_list = strata_var_list[i]
        
        pred_values = data.frame(probs = predict(step_1_model, newdata = row, type="risk"))
        final_pred = pred_values %>% mutate(pred = ifelse(probs < 1, "1","2"))
        
        step_1_prediction = as.numeric(final_pred$pred)
        
        row = row[-which(colnames(row) == "strata_var_list")]
        
      }else{
        step_1_prediction = predict(step_1_model, newdata = row)
      }
      
      antichain = antichain_list[[step_1_prediction]]
      
      if (length(antichain) == 1){
        final_predictions[i] = sort(component)[antichain]
      }else{
        step_2_models = models[[3]]
        step_2_total_removed = removed_vars[[2]]
        
        step_2_model = step_2_models[[component_num]]
        
        step_2_removed = step_2_total_removed[[component_num]]
        
        row = row[, !colnames(row) %in% step_2_removed, drop=FALSE]
        
        prediction = predict(step_2_model, newdata = row)
        
        if (length(antichain) == 2){
          if (prediction < 0){
            prediction = 1
          }else{
            prediction = 2
          }
        }
        part = antichain[prediction]
        final_predictions[i] = sort(component)[part]
      }}
  }
  
  return(final_predictions)
}

predict_new_individual = function(train_model_output, new_data, matrix, strata_var){
  step_0_model = train_model_output$models[[1]]
  step_1_models = train_model_output$models[[2]]
  step_2_models = train_model_output$models[[3]]
  
  removed_variables = train_model_output$removed_variables
  removed_variables_0 = removed_variables[[1]]
  removed_variables_1 = removed_variables[[2]]
  removed_variables_2 = removed_variables[[3]]
  
  components = split_components(matrix)
  
  split_matrices = list()
  antichain_list = list()
  
  for (component_num in 1:length(components)){
    if (length(components[[component_num]]) > 1){
      new_matrix = split_matrix(component_num, matrix, components)
      split_matrices[[length(split_matrices)+1]] = new_matrix
      
      antichain = appending_chains(new_matrix)
      antichain_list[[length(antichain_list)+1]] = antichain
    }
  }
  
  if (length(antichain_list) > 1){
    for (i in 2:length(antichain_list)){
      antichain = antichain_list[[i]]
      
      for (part in 1:length(antichain)){
        antichain_list[[i]][[part]] = components[[i]][part]
      }
    }
  }
  
  new_data = new_data[, !colnames(new_data) %in% removed_variables_0, drop=FALSE]
  
  # Pull Probabilities
  if (length(components) == 2){
    step_0_probs1 = round(predict(step_0_model, newdata = new_data, type = "response"), 6)
    step_0_probs2 = 1 - step_0_probs1[[1]]
    
    step_0_probs = c(step_0_probs2, step_0_probs1)
    names(step_0_probs) = 1:2
  }else if (length(components) > 2){
    step_0_probs = round(predict(step_0_model, newdata = new_data, type = "probs"), 6)
  }else{
    step_0_probs = 1
  }
  
  step_1_probs_total = list()
  step_2_probs_total = list()
  
  for (i in 1:length(step_1_models)){
    antichain = antichain_list[[i]]
    
    new_data1 = new_data[, !colnames(new_data) %in% removed_variables_1[[i]], drop=FALSE]
    
    if (length(antichain) == 2){
      new_data1$strata_var_list = strata_var
      
      step_1_probs0 = round(predict(step_1_models[[i]], newdata = new_data1, type = "lp"), 6)
      
      step_1_probs1 = exp(step_1_probs0)/(1+exp(step_1_probs0))
      step_1_probs2 = 1 - round(step_1_probs1[[1]], 6)
      step_1_probs = c(step_1_probs2, step_1_probs1)
      names(step_1_probs) = 1:2
      
    }else{
      step_1_probs = round(predict(step_1_models[[i]], newdata = new_data1, type = "probs"), 6)
    }
    
    step_1_probs_total[[length(step_1_probs_total) + 1]] = step_1_probs
  }
  
  count = 1
  for (chain in 1:length(antichain_list)){
    antichain = antichain_list[[chain]]
    
    for (j in 1:length(antichain)){
      
      if (length(antichain[[j]]) > 1){
        
        new_data2 = new_data[, !colnames(new_data) %in% removed_variables_1[[count]], drop=FALSE]
        new_data2 = new_data2[, !colnames(new_data2) %in% removed_variables_2[[count]], drop=FALSE]
        
        if (length(antichain[[j]]) == 2){
          step_2_probs1 = round(predict(step_2_models[[count]], newdata = new_data2, type = "response"), 6)
          step_2_probs2 = 1 - step_2_probs1[[1]]
          
          step_2_probs = c(step_2_probs2, step_2_probs1)
          names(step_2_probs) = 1:2
          
          count = count + 1
        }else{
          step_2_probs = round(predict(step_2_models[[count]], newdata = new_data2, type = "probs"), 6)
          count = count + 1
        }
        step_2_probs_total[[length(step_2_probs_total) + 1]] = step_2_probs
      }
    }
  }
  
  # Assign Final Probabilities
  final_probs = rep(-1, ncol(matrix))
  
  for (i in 1:length(components)){
    index = sort(components[[i]])
    
    if (length(index) == 1){
      final_probs[index] = step_0_probs[[i]]
    }else{
      step_0_prob = step_0_probs[[i]]
      
      for (j in 1:length(antichain_list)){
        antichain = antichain_list[[j]]
        
        for (k in 1:length(antichain)){
          antichain_part = sort(antichain[[k]])
          step_1_prob = step_1_probs_total[[j]]
          
          if (length(antichain_part) == 1){
            final_probs[index[antichain_part]] = step_0_prob*step_1_prob[[k]]
          }else{
            step_2_prob = step_2_probs_total[[j]]
            
            for (l in 1:length(antichain_part)){
              final_probs[index[antichain_part[l]]] = step_0_prob*step_1_prob[[k]]*step_2_prob[[l]]
            }
          }
        }
      }
    }
  }
  
  names(final_probs) = 1:nrow(matrix)
  
  return(list("step_0_probs" = step_0_probs, "step_1_probs" = step_1_probs_total, "step_2_probs" = step_2_probs_total, "final_probs" = final_probs))
}