library(shiny)
library(ggplot2)

attrition = read.csv(choose.files(), header = TRUE)


# Preprocess the data
attrition$Education <- as.factor(attrition$Education)
attrition$EnvironmentSatisfaction <- as.factor(attrition$EnvironmentSatisfaction)
attrition$JobInvolvement <- as.factor(attrition$JobInvolvement)
attrition$JobLevel <- as.factor(attrition$JobLevel)
attrition$JobSatisfaction <- as.factor(attrition$JobSatisfaction)
attrition$PerformanceRating <- as.factor(attrition$PerformanceRating)
attrition$RelationshipSatisfaction <- as.factor(attrition$RelationshipSatisfaction)
attrition$StockOptionLevel <- as.factor(attrition$StockOptionLevel)
attrition$WorkLifeBalance <- as.factor(attrition$WorkLifeBalance)
attrition <- attrition[, !names(attrition) %in% "EmployeeCount"]
attrition <- attrition[, !names(attrition) %in% "EmployeeNumber"]
# Identify character columns
char_columns <- sapply(attrition, is.character)
# Convert character columns to factors
attrition[char_columns] <- lapply(attrition[char_columns], as.factor)



# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploratory Data Analysis for Attrition"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Select variable for histogram
      selectInput(inputId = "variable",
                  label = "Select Variable:",
                  choices = c("Age", "JobSatisfaction", "MonthlyIncome", "JobRole", 
                              "YearsAtCompany"),
                  selected = "Age")
      
    ),
    
    # Main panel for displaying plots
    mainPanel(
      
      # Plot output
      plotOutput(outputId = "histogram")
      
    )
  )
)

# Server Logic Follows:

# Define server logic
server <- function(input, output) {
  
  # Render histogram based on user input
  output$histogram <- renderPlot({
    
    # Select variable chosen by user
    var <- input$variable
    
    # Plot histogram or box plot based on variable
    if (var == "Age") {
      ggplot(attrition, aes(x = Age, fill = Attrition)) +
        geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
        labs(title = "Distribution of Age by Attrition Status")
      # Avg age for for attrition looks to be around the 30s.
    } else if (var == "JobSatisfaction") {
      ggplot(attrition, aes(x = JobSatisfaction, fill = Attrition)) +
        geom_bar(position = "fill") +
        labs(title = "Job Satisfaction by Attrition Status")
    } else if (var == "MonthlyIncome") {
      mean_income <- attrition %>%
        group_by(JobRole) %>%
        summarise(mean_income = mean(MonthlyIncome))
      # Create a histogram of MonthlyIncome facet wrapped by JobRole
      ggplot(attrition, aes(x = MonthlyIncome)) +
        geom_histogram(binwidth = 500, fill = "skyblue", color = "black", alpha = 0.7) +
        facet_wrap(~ JobRole, scales = "free_y") +
        labs(title = "Distribution of Monthly Income by Job Role",
             x = "Monthly Income",
             y = "Frequency") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        geom_vline(data = mean_income, aes(xintercept = mean_income), color = "red", linetype = "dashed") +
        geom_text(data = mean_income, aes(x = mean_income, y = 5, label = paste("Mean:", round(mean_income, 2))), vjust = -0.5, color = "red", size = 3)
    } else if (var == "YearsAtCompany") {
      ggplot(attrition, aes(x = Attrition, y = YearsAtCompany, fill = Attrition)) +
        geom_boxplot() +
        labs(title = "Years at Company by Attrition",
             x = "Attrition",
             y = "Years at Company") +
        theme_minimal()
    } else if (var == "JobRole") {
      attrition_rates <- attrition %>%
        group_by(JobRole, Attrition) %>%
        summarise(count = n()) %>%
        group_by(JobRole) %>%
        mutate(attrition_rate = count / sum(count) * 100) %>%
        filter(Attrition == "Yes")
      
      ggplot(attrition_rates, aes(x = reorder(JobRole, -attrition_rate), y = attrition_rate, fill = JobRole)) +
        geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
        labs(title = "Attrition Rates by Job Role",
             x = "Job Role",
             y = "Attrition Rate (%)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)


