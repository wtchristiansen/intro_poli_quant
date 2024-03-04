# Load the libraries
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(here)

# Assuming the Standard QoG dataset is saved as "qog.csv" in the working directory
example_dataset_path <- here("qog.csv") # Using 'here' to build the path
example_dataset <- read.csv(example_dataset_path)

# Convert specified variables to factors
variables_to_convert <- c("bmr_dem", "ht_colonial", "p_polity2")
example_dataset <- example_dataset %>%
  mutate(across(all_of(variables_to_convert), as.factor))

# UI definition
ui <- fluidPage(
  titlePanel("Introduction to Quantitative Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose your CSV File', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      actionButton("loadUserFile", "Load Uploaded Data"),
      actionButton("loadExample", "Load Example Dataset"),
      hr(),
      uiOutput("varSelectDesc"),
      actionButton("generateDesc", "Generate Descriptive Stats"),
      hr(),
      uiOutput("varSelectCorr1"),
      uiOutput("varSelectCorr2"),
      actionButton("generateCorr", "Generate Correlational Analysis")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data View", DTOutput('dataTable')),
                  tabPanel("Descriptive Stats", verbatimTextOutput("descStatsOutput")),
                  tabPanel("Correlational Analysis Results", verbatimTextOutput("corrAnalysisOutput"))
      ),
      tabsetPanel(
        tabPanel("Resources", 
                 HTML(paste0(
                   "<h4>William Christiansen, Ph.D.</h4>",
                   "<p> Assistant Professor - Political Science and International Studies</p>",
                   "<p> Mount St. Mary's University</p>",
                   "<h4>Statistical Analysis Resources</h4>",
                   "<p>Understanding variable types is crucial for selecting the appropriate statistical analysis method. Variables can be broadly categorized into quantitative and qualitative types. Quantitative variables represent numerical values, while qualitative variables represent categories or groups.</p>",
                   "<h5>Variable Explanations for the Example Dataset</h5>",
                   "<ul>",
                   "<li><strong>cname:</strong> Country name.</li>",
                   "<li><strong>bmr_dem:</strong> Binary measure of democracy, where 1 indicates a democratic regime and 0 indicates a non-democratic regime.</li>",
                   "<li><strong>ht_colonial:</strong> Indicates whether the country was a colonial power or not; 1 for yes, 0 for no.</li>",
                   "<li><strong>nunn_desert:</strong> Proportion of the country covered by desert.</li>",
                   "<li><strong>nunn_dist_coast:</strong> The country's average distance to the coastline, measuring accessibility to sea routes.</li>",
                   "<li><strong>nunn_tropical:</strong> Indicates whether a country is located in the tropical zone; 1 for yes, 0 for no.</li>",
                   "<li><strong>p_durable:</strong> Polity durability, measuring the stability and endurance of its political institutions.</li>",
                   "<li><strong>p_polity2:</strong> Polity score combining democracy and autocracy ratings for nuanced governance assessment.</li>",
                   "<li><strong>pwt_hci:</strong> Human Capital Index from the Penn World Table, indicating the quality of labor force.</li>",
                   "<li><strong>pwt_pop:</strong> Population size from the Penn World Table.</li>",
                   "<li><strong>top_top10_income_share:</strong> Share of income held by the top 10% of earners, a measure of income inequality.</li>",
                   "<li><strong>undp_hdi:</strong> United Nations Development Programme's Human Development Index, a composite statistic of life expectancy, education, and per capita income.</li>",
                   "<li><strong>van_index:</strong> Vanhanen's Index of Democracy, measuring the level of democratization based on competition and participation.</li>",
                   "<li><strong>vdem_delibdem:</strong> V-Dem's measure of deliberative democracy, assessing the degree of public reasoning in decision-making.</li>",
                   "<li><strong>vdem_egaldem:</strong> V-Dem's egalitarian democracy index, reflecting equal protection and equality under the law.</li>",
                   "<li><strong>wbgi_cce:</strong> World Bank Governance Indicators - Control of Corruption Estimate.</li>",
                   "<li><strong>wdi_gdpagr:</strong> World Development Indicators - GDP from agriculture.</li>",
                   "<li><strong>wdi_pop:</strong> World Development Indicators - Total population.</li>",
                   "<li><strong>who_sanittot:</strong> WHO data on total access to sanitation, percentage of the population with access to improved sanitation facilities.</li>",
                   "<li><strong>whr_hap:</strong> World Happiness Report's happiness score, a national average of self-reported well-being.</li>",
                   "<li><strong>wjp_rule_of_law:</strong> World Justice Project's Rule of Law Index, measuring the extent to which countries adhere to the rule of law.</li>",
                   "</ul>",
                   "<p>Download and explore the <a href='https://www.qogdata.pol.gu.se/data/codebook_bas_jan24.pdf' target=''https://www.qogdata.pol.gu.se/data/codebook_bas_jan24.pdf'>Quality of Governance dataset codebook</a> to learn more.</p>",
                   "<h5>Descriptive Statistics</h5>",
                   "<ul>",
                   "<li><strong>Mean:</strong> The average of all numbers in a set.</li>",
                   "<li><strong>Median:</strong> The middle value in a set of numbers.</li>",
                   "<li><strong>Mode:</strong> The most frequently occurring value in a set of numbers.</li>",
                   "<li><strong>Variance:</strong> A measure of how much values in a set differ from the mean.</li>",
                   "<li><strong>Standard Deviation:</strong> A measure of the amount of variation or dispersion of a set of values.</li>",
                   "</ul>",
                   "<h5>Correlational Analysis</h5>",
                   "<p>Correlational analysis helps to identify relationships between two quantitative variables. Pearson's correlation coefficient is a common measure for linear relationships.</p>",
                   "<h5>Crosstabs</h5>",
                   "<p>Crosstabs (Cross-tabulation) analyze the relationship between two or more categorical variables.</p>",
                   "<p><h5>Example Code</h5>
<p>Below are examples of R code snippets for various operations that can be performed within the app:</p>

<h6>Loading Data</h6>
<pre>
<code>
# To load data from a CSV file
data <- read.csv(file.csv)
</code>
</pre>

<h6>Generating Descriptive Statistics</h6>
<pre>
<code>
# For numeric data
mean_value <- mean(data$numericVariable, na.rm = TRUE)
median_value <- median(data$numericVariable, na.rm = TRUE)
variance_value <- var(data$numericVariable, na.rm = TRUE)
sd_value <- sd(data$numericVariable, na.rm = TRUE)

# For categorical data
table(data$categoricalVariable)
</code>
</pre>

<h6>Performing Correlational Analysis</h6>
<pre>
<code>
# For two numeric variables
correlation_result <- cor.test(data$numericVariable1, data$numericVariable2)

# Display correlation coefficient and p-value
correlation_coefficient <- correlation_result$estimate
p_value <- correlation_result$p.value
</code>
</pre>

<h6>Creating Crosstabs for Categorical Variables</h6>
<pre>
<code>
# To analyze the relationship between two categorical variables
crosstab_result <- table(data$categoricalVariable1, data$categoricalVariable2)

# To calculate proportions
proportion_result <- prop.table(crosstab_result)
</code>
</pre>

<h6>Visualizing Data with ggplot2</h6>
<pre>
<code>
# Basic example to create a scatter plot
library(ggplot2)
ggplot(data, aes(x = numericVariable1, y = numericVariable2)) +
  geom_point()
</code>
</pre>

<p>These code snippets provide a starting point for performing data analysis within the app. Adjust the variable names and data frame names as per your specific dataset.</p>
"
                 )))
      )
    )
  )
)

# Server logic remains as previously defined

# Run the app remains as previously defined


# Server logic
server <- function(input, output, session) {
  # Reactive value for storing the dataset
  datasetInput <- reactiveVal()
  
  observeEvent(input$loadUserFile, {
    req(input$file1)
    datasetInput(read.csv(input$file1$datapath))
  })
  
  observeEvent(input$loadExample, {
    datasetInput(example_dataset)
  })
  
  output$dataTable <- renderDT({
    req(datasetInput())
    datatable(datasetInput(), options = list(pageLength = 5))
  })
  
  output$varSelectDesc <- renderUI({
    req(datasetInput())
    selectInput('selectedVarDesc', 'Select Variable for Descriptive Stats:', choices = names(datasetInput()))
  })
  
  output$varSelectCorr1 <- renderUI({
    req(datasetInput())
    selectInput('selectedVarCorr1', 'Variable 1 for Correlational Analysis:', choices = names(datasetInput()))
  })
  
  output$varSelectCorr2 <- renderUI({
    req(datasetInput())
    selectInput('selectedVarCorr2', 'Variable 2 for Correlational Analysis:', choices = names(datasetInput()))
  })
  
  output$descStatsOutput <- renderPrint({
    req(input$generateDesc, datasetInput(), input$selectedVarDesc)
    var <- input$selectedVarDesc
    dataset <- datasetInput()
    data <- dataset[[var]]
    if(is.numeric(data)) {
      descriptives <- c(
        Mean = mean(data, na.rm = TRUE),
        Median = median(data, na.rm = TRUE),
        Mode = as.numeric(names(sort(table(data), decreasing = TRUE)[1])),
        Range = paste(range(data, na.rm = TRUE), collapse = "-"),
        Variance = var(data, na.rm = TRUE),
        SD = sd(data, na.rm = TRUE)
      )
      print(descriptives)
    } else {
      print(table(data))
    }
  })
  
  output$corrAnalysisOutput <- renderPrint({
    req(input$generateCorr, datasetInput(), input$selectedVarCorr1, input$selectedVarCorr2)
    var1 <- input$selectedVarCorr1
    var2 <- input$selectedVarCorr2
    dataset <- datasetInput()
    if(is.numeric(dataset[[var1]]) && is.numeric(dataset[[var2]])) {
      result <- cor.test(dataset[[var1]], dataset[[var2]], use = "complete.obs")
      print(result)
    } else {
      tableResult <- table(dataset[[var1]], dataset[[var2]])
      print(tableResult)
      print(prop.table(tableResult))
    }
  })
}

# Run the app
shinyApp(ui, server)
