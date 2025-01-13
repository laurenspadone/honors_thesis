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