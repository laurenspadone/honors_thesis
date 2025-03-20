library(shiny)
library(hasseDiagram)
library(knitr)  
library(readr)  
library(DiagrammeR)
library(here)
source(here("all_functions.R"))

title <- "Partially Ordered Logistic Regression for Disease Classification"
subtitle <- "Text area for description"

ui <- fluidPage(
  
  titlePanel(title, windowTitle = title),
  p(subtitle),  # Add the subtitle
  
  sidebarLayout(
    sidebarPanel(
      # Text input for matrix values
      textAreaInput("matrix_input", "Enter Matrix (comma-separated)", value = "0,0,0,0,0,0,1,1,0", rows = 4),
      
      # File input for training dataset
      fileInput("file1", "Upload Training Data (CSV)", accept = ".csv"),
      
      # Numeric input for the response column index
      numericInput("response_index", "Response Column Index", value = 1, min = 1),
      
      # Numeric input for the strata column index
      numericInput("strata_index", "Strata Column Index", value = 2, min = 1),
      
      # Numeric input for tuning parameter
      numericInput("tuning_parameter", "Tuning Parameter", value = 1, min = 0, step = 0.1),
      
      # File input for patient dataset
      fileInput("patient_data", "Upload Patient Data (CSV)", accept = ".csv"),
      
      # Numeric input for row selection in the patient dataset
      numericInput("patient_row", "Select Patient Row", value = 1, min = 1, step = 1)
    ),
    
    mainPanel(
      uiOutput("matrix_output"),
      plotOutput("hasse"),
      verbatimTextOutput("frequency_table"),
      grVizOutput("decision_tree")  # Use this for DiagrammeR output
    )
  )
)


# Server function
server <- function(input, output) {
  
  # Read the uploaded training dataset
  dataset <- reactive({
    req(input$file1)  
    read_csv(input$file1$datapath)
  })
  
  # Read the uploaded patient dataset
  patient_dataset <- reactive({
    req(input$patient_data)  
    read_csv(input$patient_data$datapath)
  })
  
  # Process matrix input (comma-separated)
  input_matrix <- reactive({
    req(input$matrix_input)
    matrix_values <- as.numeric(unlist(strsplit(input$matrix_input, ",")))
    matrix(matrix_values, nrow = sqrt(length(matrix_values)), byrow = TRUE)
  })
  
  # Adjust response index if needed
  processed_data <- reactive({
    data <- dataset()
    if (input$response_index != 1) {
      data <- data[, c(input$response_index, setdiff(1:ncol(data), input$response_index))]
    }
    return(data)
  })
  
  adjusted_response_index <- reactive({
    if (input$response_index != 1) return(1) else return(input$response_index)
  })
  
  # Render matrix
  output$matrix_output <- renderUI({
    matrix_latex <- paste(
      "\\[ \\begin{pmatrix}",
      paste(apply(input_matrix(), 1, function(row) paste(row, collapse = " & ")), collapse = " \\\\ "),
      "\\end{pmatrix} \\]"
    )
    withMathJax(HTML(matrix_latex))
  })
  
  # Render Hasse diagram
  output$hasse <- renderPlot({
    label_vals <- nrow(input_matrix())
    hasse_diagram <- hasse(apply(input_matrix(), 2, as.logical), labels = 1:label_vals, parameters = list(arrow = "backward"))
    print(hasse_diagram)
  })
  
  # Compute and display the frequency table
  output$frequency_table <- renderPrint({
    example_data <- processed_data()
    strata_var <- example_data[, input$strata_index, drop = TRUE]
    example_data <- example_data[, -input$strata_index]
    
    part_ord_reg(example_data, input_matrix(), adjusted_response_index(), strata_var, input$tuning_parameter)
  })
  
  # Generate decision tree graph for each patient
  output$decision_tree <- renderGrViz({
    req(input$patient_data)
    
    example_data <- processed_data()
    strata_var <- example_data[, input$strata_index, drop = TRUE]
    example_data <- example_data[, -input$strata_index]
    
    patient_data <- patient_dataset()
    
    # Ensure the selected row is within range
    validate(need(input$patient_row > 0 && input$patient_row <= nrow(patient_data), 
                  "Selected row is out of range"))
    
    # Select the specified row from the patient dataset
    selected_row <- as.integer(input$patient_row)  # Ensure it's an integer
    patient_strata_var <- patient_data[selected_row, input$strata_index, drop = TRUE]
    patient_data <- patient_data[selected_row, -input$strata_index, drop = FALSE]
    
    # Capture graph output
    graph_output <- indv_part_ord_reg_graph(
      example_data, 
      patient_data, 
      input_matrix(), 
      adjusted_response_index(), 
      strata_var, 
      patient_strata_var
    )
    
    return(graph_output)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

