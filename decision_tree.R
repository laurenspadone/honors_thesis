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
    
    #if (length(components[[i]]) == 1){
    #create_branch_final(1,components[[i]], step_0_probs[[i]], final_probs[components[[i]]])
    #}else{
    create_branch(1,components[[i]], step_0_probs[[i]])
    #}
    
    
    parent <- 2
    step_2_count <- 1
    if (length(components[[i]]) > 1){
      antichain <- antichains[[i]]
      step_1_prob = step_1_probs[[i]]
      
      parent <- node_count - 1
      for (j in 1:length(antichain)){
        
        #if (length(antichain[[j]]) == 1){
        #create_branch_final(parent,antichain[[j]], step_1_prob[[j]], final_probs[antichain[[j]]])
        #}else{
        create_branch(parent,sort(antichain[[j]]), step_1_prob[[j]])
        #}
        
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