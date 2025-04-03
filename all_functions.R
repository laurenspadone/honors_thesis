
library(hasseDiagram)
library(nnet)
library(survival)
library(dplyr)
library(MASS)
library(DiagrammeR)
library(ordinal)

# This function takes a list of lists and combines lists with similar entries
intersections = function(components_list_primary){
  # List to append our new lists to
  new_list = c()
  
  # Loop over all of the inputted list 
  for (i in 1:length(components_list_primary)){
    
    # Set a counter (j) and indicator value (updated)
    updated = 0
    j = i
    
    if (length(components_list_primary[[i]]) != 1){
      # Loop over all the lists again to compare list i with list j
      while (j <= length(components_list_primary)){
        
        # using the intersect function we can find common elements. Intersect returns a list of such values
        intersection = intersect(components_list_primary[[i]], components_list_primary[[j]])
        
        # If there were common elements and the returned list is not empty then we will create a new input 
        # for new_list which includes the union of the common lists
        if (length(intersection) != 0){
          union_list = union(components_list_primary[[i]], components_list_primary[[j]])
          new_list[length(new_list)+1] = list(union_list)
          
          updated = updated + 1
        }
        
        j = j + 1
        # If we loop over all of our lists and find that list i didn't have any common elements with the other lists then 
        # we will add list i to new_list
        if (updated == 0){
          new_list[length(new_list)+1] = list(components_list_primary[[i]])
          updated = updated + 1
        }}}else{
          new_list[length(new_list)+1] = list(components_list_primary[[i]])
        }}
  return(new_list)
}

split_components = function(matrix){
  # new list to append values to
  split_lists = c()
  added_items = c()
  
  # Sum each of the columns
  col_sums = colSums(matrix)
  
  # loop over each of the column sums
  for (i in 1:length(col_sums)){
    
    # If the sum is equal to zero we know that it is unrelated to all other elements or it is at the top of the hasse diagram
    if (col_sums[i] == 0){
      # Create a list with the column with a sum of 0
      # We will later add the related items to the list
      component_list = c(i)
      added_items = append(added_items, i)
      
      # By looking at the related column (ie the sum of column 4 is 0 so we look at row 4) we can see all of the things 
      # that are below 4 in the hasse diagram
      for (j in 1:length(matrix[i,])){
        # If item i and j are related (there is a one in the matrix) we append j to the component_list
        if (matrix[i,j] == 1){
          component_list = append(component_list, j)
          added_items = append(added_items, j)
        }
      }
      # Add our list to split_lists
      split_lists[length(split_lists)+1] = list(component_list)
    }
  }
  
  if (length(added_items) != nrow(matrix)){
    needed_items = c()
    
    for (k in 1:nrow(matrix)){
      if (!(k %in% added_items))
        needed_items = append(needed_items, k)
    }
    
    for (l in 1:length(needed_items)){
      updated = FALSE
      item = 1
      while (updated == FALSE){
        num = matrix[item,needed_items[l]]
        
        if (num == 1){
          for (p in 1:length(split_lists)){
            split_lists_new = split_lists[[p]]
            if (item %in% split_lists_new){
              split_lists_new = append(split_lists_new, needed_items[item])
              
              split_lists[[p]] = split_lists_new
            }
          }
          updated = TRUE
        }else{
          item = item + 1
        }
      }
    }
  }
  
  
  # Call the intersection function to merge lists with similar components
  #split_lists = intersections(split_lists)
  return(split_lists)
}

# Creates an list of the zeros that mimics the antichain we will finish with
empty_antichain = function(matrix){
  
  # Sum all of the columns in the matrix
  column_sums = colSums(matrix)
  
  
  # Find how many of each sum we have (ie 1 col with a sum of 4 or 2 cols with a sum of 0)
  col_data_frame = as.data.frame(table(column_sums))
  
  lists = c()
  
  # For each distinct sum we will create a list of zeros with a length that is number of columns with that particular sum
  for (column in col_data_frame[2]){
    
    # The index for each of the lists is each sum
    column_index = col_data_frame[1]
    
    # Create each individual list of zeros
    for (i in 1:length(column)){
      addition_list = list(rep(0, col_data_frame[i,2]))
      lists[as.character(col_data_frame[i,1,])] = addition_list
    }
  }
  # return the empty lists
  return(lists)
}

# This function will add the column numbers to the zeros lists we made to complete the antichain
appending_chains = function(matrix){
  
  # Find the empty lists with the matrix given
  empty_lists = empty_antichain(matrix)
  
  # For each column in the given matrix append the column number to the correct lists (if col 4 has a sum of 2 add it to the list labelled 2)
  for (i in 1:ncol(matrix)){
    column_sum = sum(matrix[,i])
    list_index = as.character(column_sum)
    
    # Select the correct list and replace the zero with the the column number
    # If the first item in the list is already be appended loop through the remaining values until you find the next open slot
    if (empty_lists[[list_index]][1] == 0){
      empty_lists[[list_index]][1] = i
    } else {
      count = 0
      for (j in 2:length(empty_lists[[list_index]])){
        if (count == 0){
          if (empty_lists[[list_index]][j] == 0){
            empty_lists[[list_index]][j] = i
            count = 1
          }
        }
      }
    }
  }
  # rename the lists as they are no longer empty
  full_lists = empty_lists
  return(full_lists)
}

seperate_multi_antichains = function(antichains){
  multi_size_antichain = list()
  
  for (chain in antichains){
    
    if (length(chain) != 1){
      multi_size_antichain[[length(multi_size_antichain) + 1 ]] = chain
    }
  }
  return(multi_size_antichain)
}

labeling_step_0 = function(hasse_matrix, dataset, response_index){
  # Find the response column
  response_values = dataset[response_index]
  
  # Find the components
  components = split_components(hasse_matrix)
  
  # Create an empty labels list
  step_0_labels = c()
  
  # For each row of the dataset, add a value to the list that represents the component each data point is in 
  for (i in 1:nrow(dataset)){
    for (j in 1:length(components)){
      if (response_values[i,] %in% components[[j]]){
        step_0_labels = append(step_0_labels, j)
      }}}
  
  # Return the list of labels
  return(step_0_labels)
}

labeling_step_1 = function(hasse_matrix, dataset, response_index){
  response_values = dataset[response_index]
  
  response_unique = sort(as.list(unique(response_values))[[1]])
  
  if (response_unique[1] != 1){
    for (i in 1:length(response_unique)){
      response_values[response_values == response_unique[i]] = i
    }
  }
  
  # Find the anti-chains list
  antichains = appending_chains(hasse_matrix)
  
  step_1_labels = c()
  
  # For each row of the dataset, add a value to the list that represents the anti-chain each data point is in
  for (i in 1:nrow(dataset)){
    for (j in 1:length(antichains)){
      if (response_values[i,] %in% antichains[[j]]){
        step_1_labels = append(step_1_labels, j)
      }}}
  
  # Return the list of labels
  return(step_1_labels)
}

labeling_step_2 = function(hasse_matrix, dataset, antichains, response_index){
  response_values = dataset[response_index]
  step_2_labels = rep(-1, nrow(dataset))
  
  response_unique = sort(as.list(unique(response_values))[[1]])
  
  if (response_unique[1] != 1){
    for (i in 1:length(response_unique)){
      response_values[response_values == response_unique[i]] = i
    }
  }
  
  # For each row to the dataframe, change the existing -1 to the corresponding value from each component
  for (i in 1:length(antichains)){
    antichain_component = antichains[[i]]
    
    for (j in 1:length(antichain_component)){
      working_num = antichain_component[j]
      
      for (row in 1:nrow(dataset)){
        if (as.numeric(response_values[row,1]) == working_num){
          step_2_labels[row] = working_num
        }}}}
  
  return(step_2_labels)
}

regression_step_0 = function(components_list, original_data_file, new_data_file, response_index){
  
  # Find the labels that were made earlier and use this as the response variable
  new_response = which(colnames(new_data_file) == "step_0_labels")
  
  # Create a new data frame the only includes the predictors and then new response labels
  new_file = original_data_file[,-response_index, drop=FALSE]
  
  input_file = cbind("step_0_labels" = new_data_file[,new_response], new_file)
  
  # If the number of components is 2, run logisitc regression
  # If the number of components is more than 2, run multinomial regression
  if (length(components_list) == 2){
    values = sort(unique(input_file$step_0_labels))
    
    if (values[1] != 0 & values[2] != 1){
      input_file$step_0_labels = replace(input_file$step_0_labels, input_file$step_0_labels == values[1], 0)
      input_file$step_0_labels = replace(input_file$step_0_labels, input_file$step_0_labels == values[2], 1)
    }
    
    step_0_model = glm(step_0_labels~., family=binomial(link="logit"), data = input_file)
  }else{
    step_0_model = multinom(step_0_labels~., data = input_file, trace = FALSE)
  }
  return(step_0_model)
}

regression_step_1 = function(antichains, original_data_file, new_data_file, strata_var_list, response_index){
  
  # Find the labels that were made earlier and use this as the response variable
  new_response = which(colnames(new_data_file) == "step_1_labels")
  
  new_file = original_data_file[,-response_index, drop=FALSE]
  
  input_file = cbind("step_1_labels" = new_data_file[,new_response], new_file)
  
  # If the number of components is 2, run conditional logistic regression
  # If the number of components is more than 2, run proportional odds regression
  if (length(antichains) == 2){
    step_1_model = clogit(step_1_labels~. + strata(strata_var_list), data = input_file, method = "efron")
  }else{
    input_file$step_1_labels = factor(input_file$step_1_labels, ordered = TRUE)
    step_1_model = clm(step_1_labels~., data = input_file)
  }
  
  return(step_1_model)
}

regression_step_2 = function(antichain_component, original_data_file, new_data_file, response_index){
  
  # Find the labels that were made earlier and use this as the response variable
  new_response = which(colnames(new_data_file) == "step_2_labels")
  
  # Create a new data frame the only includes the predictors and then new response labels
  new_file = original_data_file[,-response_index, drop=FALSE]
  
  input_file = cbind("step_2_labels" = new_data_file[,new_response], new_file)
  
  # If the number of components is 2, run logistic regression
  # If the number of components is more than 2, run multinational regression
  
  if (length(antichain_component) == 2){
    values = sort(unique(input_file$step_2_labels))
    if (-1 %in% values){
      input_file = subset(input_file, step_2_labels != -1)
    }
    
    values = sort(unique(input_file$step_2_labels))
    
    if (values[1] != 0 & values[2] != 1){
      input_file$step_2_labels = replace(input_file$step_2_labels, input_file$step_2_labels == values[1], 0)
      input_file$step_2_labels = replace(input_file$step_2_labels, input_file$step_2_labels == values[2], 1)
    }
    
    step_2_model = glm(step_2_labels~., family=binomial(link="logit"), data = input_file)
  }else{
    step_2_model = multinom(step_2_labels~., data = input_file, trace = FALSE)
  }
  return(step_2_model)
}

# This function sees how many components have multiple items and thus need regression step 1 performed
find_subset = function(components){
  subsets = c()
  
  for (i in 1:length(components)){
    if (length(components[[i]]) > 1){
      subsets = append(subsets, i)
    }
  }
  return(subsets)
}

# Split the matrix so that the output matrix only contains the rows of each subset
split_matrix = function(subsets_num, matrix, components){
  component = sort(components[[subsets_num]])
  new_matrix = matrix[,component]
  
  return(new_matrix)
}

# Run the entirety of regression step 0
step_0_total = function(matrix, original_data, response_index, strata_var){
  
  new_data = cbind(original_data, strata_var)
  
  # Find the components
  components = split_components(matrix)
  
  if (length(components) == 1){
    step_0_model = 1
    
    matrix_list = list()
    matrix_list[[1]] = matrix
    
    original_data_list = list()
    original_data_list[[1]] = original_data
    
    new_data_list = list()
    new_data_list[[1]] = new_data
    
    return(list(model = step_0_model, matrices = matrix_list, original_data = original_data_list, new_data = new_data_list))
  }else{
    # Label the data and add the step 0 label to the new data
    labels = labeling_step_0(matrix, original_data, response_index)
    new_data = cbind(new_data, step_0_labels = labels)
    
    # Create the step 0 model
    step_0_model = regression_step_0(components, original_data, new_data, response_index)
    
    # Find the number of subsets
    subsets = find_subset(components)
    
    # Empty list for things that need to be returned
    matrices = list()
    data_sets = list()
    new_original_data_sets = list()
    
    # For each subset needed, find the matrix, and the new and original data for only that component
    for (i in 1:length(subsets)){
      value = subsets[i]
      
      sep_matrix = split_matrix(subsets[value], matrix, components)
      
      matrices[[length(matrices)+1]] = sep_matrix
      
      step_0_subset = subset(original_data, labels == value)
      data_sets[[length(data_sets)+1]] = step_0_subset
      
      new_data_subset = subset(new_data, labels == value)
      new_original_data_sets[[length(new_original_data_sets)+1]] = new_data_subset
    }
    # Return all of the individual matrices and data sets
    return(list(model = step_0_model, matrices = matrices, original_data = data_sets, new_data = new_original_data_sets))
  }
  
}

# Run the entirety of regression step 1
step_1_total = function(matrix, original_data, new_data, response_index){
  
  # Label the data and add the step 1 label to the new data
  labels = labeling_step_1(matrix, new_data, response_index)
  
  new_data_step_1 = cbind(new_data, "step_1_labels" = labels)
  
  # Find the anti-chains
  antichain_list = appending_chains(matrix)
  
  # Create strata variable
  strata_var = new_data$strata_var 
  
  # Create the step 1 model
  step_1_model = regression_step_1(antichain_list, original_data, new_data_step_1, strata_var, response_index)
  
  # Find the number of subsets
  subsets = find_subset(antichain_list)
  
  if (is.null(subsets) == FALSE){
    
    # Empty list for things that need to be returned
    antichain_parts = list()
    matrices = list()
    data_sets = list()
    new_original_data_sets = list()
    
    # For each subset needed, find the matrix, and the new and original data for only that component
    for (i in 1:length(subsets)){
      value = subsets[i]
      
      antichain_parts[[length(matrices)+1]] = antichain_list[[value]]
      
      sep_matrix = split_matrix(value, matrix, antichain_list)
      matrices[[length(matrices)+1]] = sep_matrix
      
      step_1_subset = subset(original_data, labels == value)
      data_sets[[length(data_sets)+1]] = step_1_subset
      
      new_data_subset = subset(new_data, labels == value)
      new_original_data_sets[[length(new_original_data_sets)+1]] = new_data_subset
    }
    
    # Return all of the individual matrices and data sets
    return(list(model = step_1_model, matrices = matrices, original_data = data_sets, new_data = new_original_data_sets, antichain_parts = antichain_parts))
    
  }else{
    return(list(model = step_1_model))
  }
}

# Run the entirety of regression step 2
step_2_total = function(matrix, original_data, new_data, antichain_part, response_index){
  
  # Label the data and add the step 2 label to the new data
  labels = labeling_step_2(matrix, original_data, antichain_part, response_index)
  
  new_data = cbind(original_data, step_2_labels = labels)
  
  # Find the components
  components = antichain_part
  
  # Create the step 2 model
  step_2_model = regression_step_2(components, original_data, new_data, response_index)
  
  return(step_2_model)
}

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

# Predict the classification of a test set (required input of trained models)
predict_por = function(data, matrix, response_index, strata_var_list, trained, tuning_parameter = 1){
  
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
        #print(pred_values$probs)
        
        
        final_pred = pred_values %>% mutate(pred = ifelse(probs < tuning_parameter, "1","2"))
        
        step_1_prediction = as.numeric(final_pred$pred)
        
        row = row[-which(colnames(row) == "strata_var_list")]
        
      }else{
        step_1_prediction = predict(step_1_model, newdata = row, type = "class")
        step_1_prediction = as.numeric(step_1_prediction$fit)
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
      step_1_probs = round(predict(step_1_models[[i]], newdata = new_data1)$fit, 6)
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

build_decision_tree <- function(start_node, components, antichains, step_0_probs, step_1_probs, step_2_probs, final_probs) {
  
  # Create the label for the start node based on the input number
  start_label <- paste0("{", paste(1:start_node, collapse = ", "), "}")
  
  # Initialize the graph as a dot string
  tree_dot <- "digraph decision_tree {\n"
  
  # Define the node style
  tree_dot <- paste0(tree_dot, "  node [shape=Box, fontname=Helvetica, fontsize=8, style=filled, fillcolor=lightblue];\n")
  
  # Start node
  tree_dot <- paste0(tree_dot, "  1 [label='", start_label, "' shape=Rectangle fillcolor=lightgrey];\n")
  
  node_count <- 2
  
  create_branch <- function(parent_node, branch_label, line_label) {
    tree_dot <<- paste0(tree_dot, "  ", node_count, " [label='{", paste(sort(branch_label), collapse = ", "), "}'];\n")
    tree_dot <<- paste0(tree_dot, "  ", parent_node, " -> ", node_count, " [label= ' ", round(line_label, 6), "', fontsize=7];\n")
    node_count <<- node_count + 1
  }
  
  create_branch_final <- function(parent_node, branch_label, line_label, probability){
    tree_dot <<- paste0(tree_dot, "  ", node_count, " [label='", branch_label, "\\nFinal Probability: ", round(probability, 4), "'];\n")
    tree_dot <<- paste0(tree_dot, "  ", parent_node, " -> ", node_count, " [label= ' ", round(line_label, 6), "', fontsize=7];\n")
    node_count <<- node_count + 1
  }
  
  parent <- 1
  
  for (i in 1:length(components)){
    create_branch(1,components[[i]], step_0_probs[[i]])
    
    parent <- 2
    step_2_count <- 1
    if (length(components[[i]]) > 1){
      antichain <- antichains[[i]]
      step_1_prob = step_1_probs[[i]]
      
      parent <- node_count - 1
      for (j in 1:length(antichain)){
        
        create_branch(parent,sort(antichain[[j]]), step_1_prob[[j]])
        
        parent_level_2 <- node_count - 1
        
        if (length(antichain[[j]]) > 1){
          part <- antichain[[j]]
          
          step_2_prob = step_2_probs[[step_2_count]]
          
          for (k in 1:length(part)){
            #create_branch_final(parent_level_2, part[k], step_2_prob[[k]], final_probs[part[k]])
            create_branch(parent_level_2, part[k], step_2_prob[[k]])
          }
          step_2_count = step_2_count + 1
        }
        
      }
    }
  }
  
  # Close the graph definition
  tree_dot <- paste0(tree_dot, "}\n")
  
  # Render the graph (decision tree)
  grViz(tree_dot)
  
}

full_graph = function(matrix, step_0_probs, step_1_probs, step_2_probs, final_probs){
  start_node = nrow(matrix)
  
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
  
  build_decision_tree(start_node, components, antichain_list, step_0_probs, step_1_probs, step_2_probs, final_probs)
}

part_ord_reg = function(data, matrix, response_index, strata, tuning_parameter){
  trained = train_models(matrix, data, response_index, strata)
  final_predictions = predict_por(data, matrix, response_index, strata, trained, tuning_parameter)
  
  train_table = table(y = data[[response_index]], Predictions = final_predictions)
  return(train_table)
}

part_ord_reg_tt = function(training_data, test_data, matrix, response_index, train_strata, test_strata){
  hasse(apply(matrix, 2, as.logical), parameters = list(arrow = "backward"))
  
  trained = train_models(matrix, training_data, response_index, train_strata)
  
  final_predictions = predict_por(training_data, matrix, response_index, train_strata, trained)
  train_table = table(y = training_data[[response_index]], "Training Predictions" = final_predictions)
  
  final_predictions = predict_por(test_data, matrix, response_index, test_strata, trained)
  test_table = table(y = test_data[[response_index]], "Testing Predictions" = final_predictions)
  
  print(train_table)
  
  print(test_table)
}

indv_part_ord_reg_graph = function(full_data, patient_data, matrix, response_index, strata, patient_strata){
  trained = train_models(matrix, full_data, response_index, strata)
  predict = predict_new_individual(trained, patient_data, matrix, patient_strata)
  full_graph(matrix, predict$step_0_probs, predict$step_1_probs, predict$step_2_probs, predict$final_probs)
}