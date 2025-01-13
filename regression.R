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
    step_1_model = polr(factor(step_1_labels)~., data = input_file)
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