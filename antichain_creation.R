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