library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(shinythemes)
library(RColorBrewer)
library(tidyr)
library(reshape2)
library(broom)

# Read the data
data <- read.csv('JumpData.csv')

# Mutate the data with appropriate types
jump_data <- data %>%
  mutate(PlayerId = as.integer(PlayerId),
         Date = as.Date(Date, format = "%m/%d/%Y"),
         Field.Position = as.factor(Field.Position),
         Test.Type = as.factor(Test.Type),
         Weight..kg. = as.numeric(Weight..kg.),
         Trial = as.integer(Trial),
         Countermovement.Depth..cm. = as.numeric(Countermovement.Depth..cm.),
         Flight.Time..s. = as.numeric(Flight.Time..s.),
         Jump.Height..Imp.Mom..in.Inches = as.numeric(Jump.Height..Imp.Mom..in.Inches),
         Concentric.Mean.Force..N. = as.numeric(Concentric.Mean.Force..N.),
         RSI.Modified..m.s. = as.numeric(RSI.Modified..m.s.),
         Velocity.at.Peak.Power..m.s. = as.numeric(Velocity.at.Peak.Power..m.s.),
         Eccentric.Braking.Impulse = as.numeric(Eccentric.Braking.Impulse),
         Eccentric.Braking.Impulse..Left. = as.numeric(Eccentric.Braking.Impulse..Left.),
         Eccentric.Braking.Impulse..Right. = as.numeric(Eccentric.Braking.Impulse..Right.),
         Eccentric.Braking.Impulse..Asym. = as.numeric(Eccentric.Braking.Impulse..Asym.),
         Concentric.Impulse..Ns. = as.numeric(Concentric.Impulse..Ns.),
         Concentric.Impulse..Left...Ns. = as.numeric(Concentric.Impulse..Left...Ns.),
         Concentric.Impulse..Right...Ns. = as.numeric(Concentric.Impulse..Right...Ns.),
         Concentric.Impulse..Asym...Ns. = as.numeric(Concentric.Impulse..Asym...Ns.))

# Define the UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  # Background and text styles
  tags$style(HTML("
    body {
      background-image: url('https://raw.githubusercontent.com/IDBach16/BachAnalytics-Pitcher_Comparison_Dashboard/main/newgym-7-scaled-1.jpg');
      background-attachment: fixed;
      background-size: cover;
    }
    .tab-content .active h3, .tab-content .active p {
      color: #E0E0E0;
    }
    .tab-content .active {
      padding: 15px;
      border-radius: 5px;
    }
    .custom-border {
      border: 2px solid #E0E0E0;
      padding: 15px;
      border-radius: 5px;
      background-color: rgba(0, 0, 0, 0.9); /* Dark background */
      color: #FFFFFF; /* Light text color */
    }
  ")),
  titlePanel("Force Plate Dashboard"),
  navbarPage(
    title = "Force Plate Dashboard",
    tabPanel("Introduction",
             h3("Welcome to the Force PlateDashboard"),
             class = "custom-border",
             p("The Force Plate Dashboard is designed to provide detailed insights into the biomechanical performance of athletes, 
               specifically focusing on metrics derived from force plate data."),
             p("This dashboard aims to help coaches, trainers, and analysts monitor, 
               analyze, and optimize athletic performance through various key metrics"),
             h3("Key Features:"),
             p(tags$u("Weight vs Date:")),
             p("Track changes in athletes' weight over time with a regression line to visualize trends."),
             p(tags$u("Summary Metrics:")),
             p("View summary statistics of key performance metrics for different field positions."),
             p(tags$u("Correlation Matrix:")),
             p("Analyze the relationships between various performance metrics to understand how they influence each other."),
             p(tags$u("Asymmetry Analysis:")),
             p("Examine the impact of left-right asymmetry on performance metrics like jump height and RSI."),
             p(tags$u("Time Series Analysis:")),
             p("Monitor performance changes over time for individual players, focusing on jump height, RSI, and velocity at peak power."),
             
             tableOutput("dataDictTable") # Render the data dictionary as a table
    ),
    tabPanel("Force Plate Terms",
             mainPanel(
               h3("Force Plate Data"),
               class = "custom-border",  # Apply the custom dark background style
               h3("Definitions"),
               p(tags$u("PlayerId:")),
               p("A unique identifier for each player."),
               p(tags$u("Date:")),
               p("The date when the measurement was taken."),
               p(tags$u("Field.Position:")),
               p("The position of the player on the field (e.g., Pitcher, In-field)."),
               p(tags$u("Test.Type:")),
               p("The type of test performed (e.g., CMJ - Countermovement Jump)."),
               p(tags$u("Weight..kg.:")),
               p("The weight of the player in kilograms."),
               p(tags$u("Trial:")),
               p("The trial number of the test (e.g., 1, 2, 3)."),
               
               h3("Kinematic Terms"),
               p(tags$u("Countermovement.Depth..cm.:")),
               p("The depth of the countermovement during the jump in centimeters."),
               p(tags$u("Flight.Time..s.:")),
               p("The time the player spends in the air during the jump in seconds."),
               p(tags$u("Jump.Height..Imp.Mom..in.Inches: ")),
               p("The jump height calculated using impulse momentum in inches."),
               p(tags$u("Time.to.Take.Off..s.: ")),
               p("The time taken to take off for the jump in seconds."),
               p(tags$u("Time.to.Peak.Force..s.:")),
               p("The time taken to reach peak force in seconds."),
               p(tags$u("RSI..m.s.:")),
               p("Reactive Strength Index, a measure of explosive strength, calculated as jump height divided by ground contact time."),
               p(tags$u("RSI.Modified..m.s.:")),
               p("A modified version of the Reactive Strength Index."),
               p(tags$u("Eccentric.Braking.Impulse:")),
               p("The total impulse generated during the braking phase of the eccentric movement."),
               p(tags$u("Eccentric.Braking.Impulse..Left.:")),
               p("The braking impulse generated by the left leg during the eccentric phase."),
               p(tags$u("Eccentric.Braking.Impulse..Right.:")),
               p("The braking impulse generated by the right leg during the eccentric phase."),
               p(tags$u("Eccentric.Braking.Impulse..Asym.:")),
               p("The asymmetry in braking impulse between the left and right legs."),
               p(tags$u("Concentric.Impulse..Ns.:")),
               p("The total impulse generated during the concentric phase in Newton-seconds."),
               p(tags$u("Concentric.Impulse..Left...Ns.:")),
               p("The concentric impulse generated by the left leg in Newton-seconds."),
               p(tags$u("Concentric.Impulse..Right...Ns.:")),
               p("The concentric impulse generated by the right leg in Newton-seconds."),
               p(tags$u("Concentric.Impulse..Asym...Ns.:")),
               p("The asymmetry in concentric impulse between the left and right legs in Newton-seconds."),
               
               h3("Kinetic Terms"),
               p(tags$u("Concentric.Mean.Force..N.:")),
               p("The average force generated during the concentric (upward) phase of the jump in Newtons."),
               p(tags$u("Concentric.Peak.Force..N.:")),
               p("The peak force generated during the concentric phase of the jump in Newtons."),
               p(tags$u("Concentric.Mean.Power..W.:")),
               p("The average power generated during the concentric phase in watts."),
               p(tags$u("Concentric.Peak.Power..W.:")),
               p("The peak power generated during the concentric phase in watts."),
               p(tags$u("Concentric.Peak.Velocity..m.s.:")),
               p("The peak velocity during the concentric phase in meters per second."),
               p(tags$u("Eccentric.Mean.Force..N.:")),
               p("The average force generated during the eccentric (downward) phase of the jump in Newtons."),
               p(tags$u("Eccentric.Peak.Force..N.:")),
               p("The peak force generated during the eccentric phase of the jump in Newtons."),
               p(tags$u("Eccentric.Mean.Power..W.:")),
               p("Eccentric.Mean.Power..W.: The average power during the eccentric phase in watts."),
               p(tags$u("Eccentric.Peak.Power..W.:")),
               p("The peak power generated during the eccentric phase in watts."),
               p(tags$u("Eccentric.Peak.Velocity..m.s.:")),
               p("Eccentric.Peak.Velocity..m.s.: The peak velocity during the eccentric phase in meters per second."),
               p(tags$u("Velocity.at.Peak.Power..m.s.:")),
               p("The velocity at the point of peak power in meters per second.")
             )
    ),
    tabPanel("Weight vs Date",
             sidebarPanel(
               selectInput("pitcherTeam1", "Select Field Position:", choices = unique(jump_data$Field.Position)),
               uiOutput("pitcherUI")
             ),
             mainPanel(
               h3(tags$u("Purpose:")),
               div(class = "custom-border",
                   p("The regression line in these charts represents a linear relationship between the dates and the weights for each 
                   Player ID. By fitting a straight line through the data points, the regression line provides a visual indication of 
                   how the weight of the player changes over time. The slope of the line indicates the direction and strength of the relationship; 
                   a positive slope suggests that the player's weight tends to increase over time, while a negative slope indicates a decrease in weight."),
                   
                   p("The regression line helps in identifying trends and patterns in the data, making it easier to understand long-term changes in the player's weight, 
                   and can be useful for coaches and analysts to monitor the effectiveness of training and dietary programs. The confidence interval around the line, 
                   represented by the shaded area (if included), indicates the reliability of the prediction, with narrower intervals suggesting more precise predictions.")
               ),
               tags$h3("Weight vs Date"),
               plotlyOutput("weightPlot")
             )),
    tabPanel("Summary Metrics",
             mainPanel(
               h3(tags$u("All Positions:")),
               tableOutput("Key_metrics"),
               uiOutput("positionMetrics")
             )),
    tabPanel("Correlation Matrix",
             sidebarPanel(
               selectInput("position", "Select Field Position:", choices = unique(jump_data$Field.Position)),
               uiOutput("playerUI")
             ),
             mainPanel(
               h3("Correlation Matrix"),
               plotlyOutput("correlationPlot"),
               h3("Interpretation of Correlation Matrix"),
               
               class = "custom-border",  # Apply the custom dark background style
               p(tags$u("Jump Height and Concentric Mean Force:")),
               p("Expected Relationship: Positive correlation. Higher jump heights are generally achieved by 
                 generating more force during the concentric phase of the jump."),
               p("Analysis: If the correlation coefficient is around 0.75, it suggests a strong positive relationship, 
                 confirming that players who can generate more force during the concentric phase tend to jump higher."),
               p(tags$u("Jump Height and Eccentric Braking Impulse:")),
               p("Expected Relationship: This can vary. A balanced braking impulse is necessary for optimal jump performance."),
               p("Analysis: If the correlation coefficient is around -0.2, it suggests a weak negative relationship. 
                 This could imply that while eccentric braking is important, excessive braking may slightly reduce jump height."),
               p(tags$u("Jump Height and RSI Modified:")),
               p("Expected Relationship: Positive correlation. RSI is a measure of explosive strength, 
                 which should correlate with jump height."),
               p("Analysis: If the correlation coefficient is around 0.6, it suggests a moderate positive relationship. 
               This indicates that players with higher RSI values, which reflect better explosive strength, 
               tend to achieve higher jump heights."),
               
               p(tags$u("Concentric Mean Force and Eccentric Braking Impulse:")),
               p("Expected Relationship: Positive correlation. Effective eccentric braking can contribute 
                 to higher concentric forces."),
               p("Analysis: If the correlation coefficient is around 0.4, it suggests a moderate positive 
                 relationship. This means players who can effectively brake during the eccentric phase also 
                 tend to generate higher forces during the concentric phase."),
               p(tags$u("Concentric Mean Force and RSI Modified:")),
               p("Expected Relationship: Positive correlation. Higher concentric forces 
                 should correlate with better explosive strength."),
               p("Analysis: If the correlation coefficient is around 0.7, it suggests a strong positive relationship, 
                 confirming that higher concentric forces are associated with better RSI values."),
               p(tags$u("Eccentric Braking Impulse and RSI Modified:")),
               p("Expected Relationship: This can vary. Proper eccentric braking is important for effective explosive strength."),
               p("Analysis: If the correlation coefficient is around -0.3, it suggests a weak negative relationship. This might 
                 indicate that while eccentric braking is crucial, excessive focus on braking can reduce explosive strength as measured by RSI."),
               
               h3("Practical Implications"),
               p(tags$u("Training Adjustments:")),
               p("Training programs should emphasize concentric strength development to improve both jump height and RSI. 
               Exercises that enhance concentric force generation are likely to yield better performance outcomes. Coaches should monitor 
                 eccentric braking impulses to ensure they are balanced. Too much focus on braking could detract from overall jump performance 
                 and explosive strength."),
               p(tags$u("Position-Specific Insights:")),
               p("Different positions might show varying correlation patterns. For example, pitchers might have different strength and 
                 explosive requirements compared to in-field players. Understanding these position-specific relationships can help tailor 
                 training programs more effectively."),
               p(tags$u("Injury Prevention:")),
               p("Ensuring balanced training that does not overly emphasize one phase of movement 
                 (e.g., eccentric braking) over others can help prevent injuries. Balanced development 
                 is crucial for maintaining overall athletic performance and health.")
             )
    ),
    tabPanel("Asymmetry Analysis",
             sidebarPanel(
               selectInput("asymPosition", "Select Field Position:", choices = unique(jump_data$Field.Position)),
               uiOutput("asymPlayerUI")
             ),
             mainPanel(
               h3("Asymmetry Analysis Charts"),
               h3("Linear Model Statistics"),
               class = "custom-border",
               p(tags$u("R-Squared")),
               p("The fit of the linear model can be evaluated using the R-squared value, which 
                 indicates how well the model explains the variability of the response data around its mean.
                 An R-squared value closer to 1 implies a better fit."),
               p(tags$u("p-values")),
               p("The p-values for the slope coefficients help in understanding the statistical significance of the relationships.
                 A low p-value (typically < 0.05) suggests that the relationship is statistically significant."),
               tableOutput("lmStatsTable"),
               h3("Asymmetry in Eccentric Braking Impulse vs. Jump Height "),
               plotlyOutput("asymJumpHeightPlot"),
               p(tags$u("Intercept:")),
               p("Represents the expected jump height when the asymmetry is zero."),
               p(tags$u("Slope:")),
               p("Indicates the change in jump height for a one-unit change in eccentric braking impulse asymmetry. 
                 A negative slope suggests that higher asymmetry is associated with lower jump heights, while a positive slope would suggest the opposite."),
               h3("Asymmetry in Eccentric Braking Impulse vs. RSI"),
               plotlyOutput("asymRsiPlot"),
               p(tags$u("Intercept:")),
               p("The expected RSI when the asymmetry is zero."),
               p(tags$u("Slope:")),
               p("Indicates how much RSI changes for a unit change in asymmetry. A negative slope would suggest that higher asymmetry reduces RSI, 
                 impacting the athlete’s explosive strength."),
               h3("Asymmetry in Concentric Impulse vs. RSI"),
               plotlyOutput("Con1Plot"),
               p(tags$u("Intercept:")),
               p("The expected RSI when concentric impulse asymmetry is zero."),
               p(tags$u("Slope:")),
               p("Shows the change in RSI for a one-unit change in concentric impulse asymmetry. 
                 A negative slope implies that greater asymmetry in concentric force production negatively affects RSI."),
               h3("Asymmetry in Concentric Impulse vs. Jump Height"),
               plotlyOutput("Con2Plot"),
               p(tags$u("Intercept:")),
               p("The jump height when the concentric impulse asymmetry is zero."),
               p(tags$u("Slope:")),
               p("Indicates the change in jump height for a one-unit change in concentric impulse asymmetry.
                 A negative slope suggests that higher asymmetry in concentric force production correlates with lower jump heights.")
             )),
    tabPanel("Time Series Analysis",  # New tab panel
             sidebarPanel(
               selectInput("timeSeriesFieldPosition", "Select Field Position:", choices = unique(jump_data$Field.Position)),
               uiOutput("timeSeriesPlayerUI")
             ),
             mainPanel(
               class = "custom-border",
               h3("Performance Changes Over Time"),
               h3("Jump Height"),
               p(tags$u("Biomechanical Importance:")),
               p("Jump height is a critical measure of an athlete's explosive power, reflecting their ability 
                 to generate force quickly and effectively. It is influenced by both neuromuscular coordination 
                 and muscular strength."),
               p(tags$u("Time Series:")),
               p("Tracking jump height over time allows coaches and athletes to observe trends in explosive power. 
                 Consistent improvements in jump height can indicate successful training and conditioning programs, 
                 while declines might signal fatigue, overtraining, or the need for adjustments in training intensity."),
               plotlyOutput("jumpHeightTimeSeriesPlot"),
               h3("Reactive Strength Index (RSI):"),
               p(tags$u("Biomechanical Importance:")),
               p("RSI measures explosive strength by calculating the ratio of jump height to ground contact time. 
                 It reflects an athlete's ability to utilize the stretch-shortening cycle effectively, which is crucial 
                 for sports involving rapid, explosive movements."),
               p(tags$u("Time Series:")),
               p("Monitoring RSI over time helps in understanding an athlete’s explosive strength and their ability to perform quick, 
                 powerful movements. Fluctuations in RSI can indicate changes in neuromuscular efficiency and the athlete's response to plyometric training."),
               plotlyOutput("rsiTimeSeriesPlot"),
               h3("Velocity at Peak Power:"),
               p(tags$u("Biomechanical Importance:")),
               p("This metric represents the speed at which an athlete achieves their peak power during a jump. It provides insights into the rate of force development
                 and the efficiency of power generation."),
               p(tags$u("Time Series:")),
               p("By tracking velocity at peak power over time, coaches can evaluate the effectiveness of strength and conditioning programs aimed at improving power output. 
                 Changes in this metric can highlight improvements in the athlete's ability to generate force rapidly."),
               plotlyOutput("veloTimeSeriesPlot")
             )),
    tabPanel("Clustering",
             sidebarLayout(
               sidebarPanel(
                 h3("Clustering Analysis"),
                 p("This tab displays the results of the clustering analysis performed on the jump data."),
                 p("Select the number of clusters for k-means clustering:"),
                 sliderInput("numClusters", "Number of Clusters:", min = 2, max = 10, value = 3)
               ),
               mainPanel(
                 h3("Cluster Centroids"),
                 tableOutput("centroidsTable1"),
                 tableOutput("centroidsTable2"),
                 tableOutput("centroidsTable3"),
                 p("The centroid of a cluster is the point that represents the average position of all the data 
                   points within that cluster. It is calculated by taking the mean of the coordinates of all the points 
                   in the cluster. This centroid acts as a central point that best represents the overall location of the 
                   cluster in the dataset."),
                 #################
                 
                 h3("Elbow Plot"),
                 class = "custom-border",
                 plotOutput('elbowPlot'),
                 
                 h3("Elbow Method and Choosing 3 Clusters"),
                 p("The elbow method is a technique used to determine the optimal number of clusters (k) in a dataset. 
                The idea is to run the k-means clustering algorithm for a range of k values (from 1 to some large number), 
                and for each k, calculate the total within-cluster sum of squares (WSS). This metric quantifies the variance within each cluster.
                The WSS generally decreases as the number of clusters increases, because more clusters will naturally reduce the variance within each cluster 
                (each cluster becomes tighter). However, there is a point where the rate of decrease sharply slows down and forms an 'elbow' shape in the plot. 
                This point is considered the optimal number of clusters because adding more clusters beyond this point provides diminishing returns in terms of reducing WSS."),
                 
                 
                 h3("Jump Height Vs Concentric Peak Force"),
                 plotlyOutput("jumpHeightVsConcentricPeakForce"),
                 h3("Eccentric Peak Force Vs Concentric Peak Force"),
                 plotlyOutput("eccentricPeakForceVsConcentricPeakForce"),
                 h3("RSI Vs Contraction Time"),
                 plotlyOutput("rsiVsContractionTime"),
                 h3("Mean Take off Velocity Vs JumpHeight"),
                 plotlyOutput("meanTakeoffVelocityVsJumpHeight"),
                 h3("Force At Peak Power Vs Peak Power"),
                 plotlyOutput("forceAtPeakPowerVsPeakPower"),
                 h3("Eccentric Duration Vs Concentric Duration"),
                 plotlyOutput("eccentricDurationVsConcentricDuration"),
                 h3("Braking Phase Duration Vs Eccentric Braking Impulse"),
                 plotlyOutput("brakingPhaseDurationVsEccentricBrakingImpulse"),
                 h3("Concentric Mean Power Vs Eccentric Mean Power"),
                 plotlyOutput("concentricMeanPowerVsEccentricMeanPower")
                 
               )
             )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  output$dataDictTable <- renderTable({
    # Create data dictionary here or load it if defined externally
    data_dictionary <- data.frame(
      Column = c(
        "PlayerId",
        "Date",
        "Field.Position",
        "Test.Type",
        "Weight..kg.",
        "Trial",
        "Countermovement.Depth..cm.",
        "Flight.Time..s.",
        "Jump.Height..Imp.Mom..in.Inches",
        "Concentric.Mean.Force..N.",
        "RSI.Modified..m.s.",
        "Velocity.at.Peak.Power..m.s.",
        "Eccentric.Braking.Impulse",
        "Eccentric.Braking.Impulse..Left.",
        "Eccentric.Braking.Impulse..Right.",
        "Eccentric.Braking.Impulse..Asym.",
        "Concentric.Impulse..Ns.",
        "Concentric.Impulse..Left...Ns.",
        "Concentric.Impulse..Right...Ns.",
        "Concentric.Impulse..Asym...Ns."
      ),
      Description = c(
        "Player ID as an integer",
        "Date in mm/dd/yyyy format",
        "Field Position as a factor",
        "Test Type as a factor",
        "Weight in kilograms",
        "Trial number as an integer",
        "Countermovement Depth in centimeters",
        "Flight Time in seconds",
        "Jump Height in inches using Impulse Momentum method",
        "Concentric Mean Force in Newtons",
        "Modified Reactive Strength Index in meters/second",
        "Velocity at Peak Power in meters/second",
        "Total Eccentric Braking Impulse",
        "Eccentric Braking Impulse for the left leg",
        "Eccentric Braking Impulse for the right leg",
        "Asymmetry in Eccentric Braking Impulse",
        "Total Concentric Impulse in Newton-seconds",
        "Concentric Impulse for the left leg in Newton-seconds",
        "Concentric Impulse for the right leg in Newton-seconds",
        "Asymmetry in Concentric Impulse in Newton-seconds"
      ),
      DataType = c(
        "integer",
        "Date",
        "factor",
        "factor",
        "numeric",
        "integer",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric"
      )
    )
    data_dictionary
  })
  #################################
  teamData <- reactive({
    jump_data %>%
      filter(Field.Position == input$pitcherTeam1)
  })
  
  # Update pitcher choices based on selected team
  output$pitcherUI <- renderUI({
    selectInput("pitcher1", "Select Player ID:", choices = unique(teamData()$PlayerId))
  })
  
  filteredData <- reactive({
    req(input$pitcher1)  # Ensure pitcher is selected before filtering data
    jump_data %>%
      filter(Field.Position == input$pitcherTeam1, PlayerId == input$pitcher1)
  })
  
  # Generate ggplot and convert to plotly output for weight distribution by date
  output$weightPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Date, y = Weight..kg.)) +
      geom_point(size = 3) + # Increase point size
      geom_smooth(method = "lm", se = FALSE, col = "blue") + # Add regression line
      labs(title = "Weight Distribution by Date for Player ID",
           x = "Date",
           y = "Weight (kg)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Filter out rows with empty Field.Position
  jump_data <- jump_data %>% 
    filter(Field.Position != "")
  
  # Calculate summary statistics for key metrics
  key_metrics <- jump_data %>% 
    select(PlayerId, Field.Position, Jump.Height..Imp.Mom..in.Inches, Concentric.Mean.Force..N., Eccentric.Braking.Impulse) %>% 
    group_by(Field.Position) %>% 
    summarise(
      mean_jump_height = mean(Jump.Height..Imp.Mom..in.Inches, na.rm = TRUE),
      median_jump_height = median(Jump.Height..Imp.Mom..in.Inches, na.rm = TRUE),
      sd_jump_height = sd(Jump.Height..Imp.Mom..in.Inches, na.rm = TRUE),
      mean_concentric_force = mean(Concentric.Mean.Force..N., na.rm = TRUE),
      median_concentric_force = median(Concentric.Mean.Force..N., na.rm = TRUE),
      sd_concentric_force = sd(Concentric.Mean.Force..N., na.rm = TRUE),
      mean_eccentric_impulse = mean(Eccentric.Braking.Impulse, na.rm = TRUE),
      median_eccentric_impulse = median(Eccentric.Braking.Impulse, na.rm = TRUE),
      sd_eccentric_impulse = sd(Eccentric.Braking.Impulse, na.rm = TRUE)
    )
  
  output$Key_metrics <- renderTable({
    key_metrics
  })
  
  output$positionMetrics <- renderUI({
    metrics_list <- lapply(unique(jump_data$Field.Position), function(position) {
      key_metricsP <- jump_data %>% 
        filter(Field.Position == position) %>% 
        select(PlayerId, Jump.Height..Imp.Mom..in.Inches, Concentric.Mean.Force..N., Eccentric.Braking.Impulse) %>% 
        group_by(PlayerId) %>% 
        summarise(
          mean_jump_height = mean(Jump.Height..Imp.Mom..in.Inches, na.rm = TRUE),
          median_jump_height = median(Jump.Height..Imp.Mom..in.Inches, na.rm = TRUE),
          sd_jump_height = sd(Jump.Height..Imp.Mom..in.Inches, na.rm = TRUE),
          mean_concentric_force = mean(Concentric.Mean.Force..N., na.rm = TRUE),
          median_concentric_force = median(Concentric.Mean.Force..N., na.rm = TRUE),
          sd_concentric_force = sd(Concentric.Mean.Force..N., na.rm = TRUE),
          mean_eccentric_impulse = mean(Eccentric.Braking.Impulse, na.rm = TRUE),
          median_eccentric_impulse = median(Eccentric.Braking.Impulse, na.rm = TRUE),
          sd_eccentric_impulse = sd(Eccentric.Braking.Impulse, na.rm = TRUE)
        )
      
      tagList(
        h4(paste("Position:", position)),
        tableOutput(paste0("metrics_", position))
      )
    })
    
    do.call(tagList, metrics_list)
  })
  
  lapply(unique(jump_data$Field.Position), function(position) {
    output[[paste0("metrics_", position)]] <- renderTable({
      jump_data %>% 
        filter(Field.Position == position) %>% 
        select(PlayerId, Jump.Height..Imp.Mom..in.Inches, Concentric.Mean.Force..N., Eccentric.Braking.Impulse) %>% 
        group_by(PlayerId) %>% 
        summarise(
          mean_jump_height = mean(Jump.Height..Imp.Mom..in.Inches, na.rm = TRUE),
          median_jump_height = median(Jump.Height..Imp.Mom..in.Inches, na.rm = TRUE),
          sd_jump_height = sd(Jump.Height..Imp.Mom..in.Inches, na.rm = TRUE),
          mean_concentric_force = mean(Concentric.Mean.Force..N., na.rm = TRUE),
          median_concentric_force = median(Concentric.Mean.Force..N., na.rm = TRUE),
          sd_concentric_force = sd(Concentric.Mean.Force..N., na.rm = TRUE),
          mean_eccentric_impulse = mean(Eccentric.Braking.Impulse, na.rm = TRUE),
          median_eccentric_impulse = median(Eccentric.Braking.Impulse, na.rm = TRUE),
          sd_eccentric_impulse = sd(Eccentric.Braking.Impulse, na.rm = TRUE)
        )
    })
  })
  
  # Reactive data for the correlation matrix based on selected field position
  # Update player choices based on selected field position
  output$playerUI <- renderUI({
    selectInput("player", "Select Player ID:", choices = unique(jump_data$PlayerId[jump_data$Field.Position == input$position]))
  })
  
  # Reactive data for the correlation matrix based on selected player
  correlationData <- reactive({
    req(input$player)  # Ensure player is selected before filtering data
    jump_data %>%
      filter(PlayerId == input$player) %>%
      select(Jump.Height..Imp.Mom..in.Inches, Concentric.Mean.Force..N., Eccentric.Braking.Impulse, RSI.Modified..m.s.)
  })
  
  # Generate the interactive correlation plot
  output$correlationPlot <- renderPlotly({
    req(correlationData())  # Ensure data is available before plotting
    cor_matrix <- cor(correlationData(), use = "complete.obs")
    melted_cor_matrix <- reshape2::melt(cor_matrix)
    
    plot_ly(
      data = melted_cor_matrix, 
      x = ~Var1, 
      y = ~Var2, 
      z = ~value, 
      type = "heatmap", 
      colorscale = "RdBu", 
      zmin = -1, 
      zmax = 1,
      hovertemplate = "Correlation: %{z}<extra></extra>"
    ) %>%
      layout(
        title = "Correlation Matrix",
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        colorbar = list(title = "Correlation")
      )
  })
  
  #################################################### 
  asymFilteredData <- reactive({
    req(input$asymPosition)
    jump_data %>%
      filter(Field.Position == input$asymPosition)
  })
  
  output$asymPlayerUI <- renderUI({
    selectInput("asymPlayer", "Select Player ID:", choices = unique(asymFilteredData()$PlayerId))
  })
  
  asymFilteredPlayerData <- reactive({
    req(input$asymPlayer)
    asymFilteredData() %>%
      filter(PlayerId == input$asymPlayer)
  })
  
  get_model_stats <- function(model) {
    tidy_model <- tidy(model)
    glance_model <- glance(model)
    list(
      p_value = tidy_model$p.value[2],
      r_squared = glance_model$r.squared
    )
  }
  
  output$asymJumpHeightPlot <- renderPlotly({
    asymData <- asymFilteredPlayerData() %>%
      mutate(SI_Eccentric = ((Eccentric.Braking.Impulse..Left. - Eccentric.Braking.Impulse..Right.) / 
                               (0.5 * (Eccentric.Braking.Impulse..Left. + Eccentric.Braking.Impulse..Right.))) * 100)
    
    lm_model <- lm(Jump.Height..Imp.Mom..in.Inches ~ SI_Eccentric, data = asymData)
    model_stats <- get_model_stats(lm_model)
    
    p <- ggplot(asymData, aes(x = SI_Eccentric, y = Jump.Height..Imp.Mom..in.Inches)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = sprintf("Asymmetry in Eccentric Braking Impulse vs Jump Height\np-value: %.3f, R-squared: %.3f", model_stats$p_value, model_stats$r_squared), 
           x = "Eccentric Braking Impulse Asymmetry (%)", 
           y = "Jump Height (Inches)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$asymRsiPlot <- renderPlotly({
    asymData <- asymFilteredPlayerData() %>%
      mutate(SI_Eccentric = ((Eccentric.Braking.Impulse..Left. - Eccentric.Braking.Impulse..Right.) / 
                               (0.5 * (Eccentric.Braking.Impulse..Left. + Eccentric.Braking.Impulse..Right.))) * 100)
    
    lm_model <- lm(RSI.Modified..m.s. ~ SI_Eccentric, data = asymData)
    model_stats <- get_model_stats(lm_model)
    
    p <- ggplot(asymData, aes(x = SI_Eccentric, y = RSI.Modified..m.s.)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = sprintf("Asymmetry in Eccentric Braking Impulse vs RSI\np-value: %.3f, R-squared: %.3f", model_stats$p_value, model_stats$r_squared), 
           x = "Eccentric Braking Impulse Asymmetry (%)", 
           y = "RSI (m/s)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$Con1Plot <- renderPlotly({
    asymData <- asymFilteredPlayerData() %>%
      mutate(SI_Concentric = ((Concentric.Impulse..Left...Ns. - Concentric.Impulse..Right...Ns.) / 
                                (0.5 * (Concentric.Impulse..Left...Ns. + Concentric.Impulse..Right...Ns.))) * 100)
    
    lm_model <- lm(RSI.Modified..m.s. ~ SI_Concentric, data = asymData)
    model_stats <- get_model_stats(lm_model)
    
    p <- ggplot(asymData, aes(x = SI_Concentric, y = RSI.Modified..m.s.)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = sprintf("Asymmetry in Concentric Impulse vs RSI\np-value: %.3f, R-squared: %.3f", model_stats$p_value, model_stats$r_squared), 
           x = "Concentric Impulse Asymmetry (%)", 
           y = "RSI (m/s)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$Con2Plot <- renderPlotly({
    asymData <- asymFilteredPlayerData() %>%
      mutate(SI_Concentric = ((Concentric.Impulse..Left...Ns. - Concentric.Impulse..Right...Ns.) / 
                                (0.5 * (Concentric.Impulse..Left...Ns. + Concentric.Impulse..Right...Ns.))) * 100)
    
    lm_model <- lm(Jump.Height..Imp.Mom..in.Inches ~ SI_Concentric, data = asymData)
    model_stats <- get_model_stats(lm_model)
    
    p <- ggplot(asymData, aes(x = SI_Concentric, y = Jump.Height..Imp.Mom..in.Inches)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = sprintf("Asymmetry in Concentric Impulse vs Jump Height\np-value: %.3f, R-squared: %.3f", model_stats$p_value, model_stats$r_squared), 
           x = "Concentric Impulse Asymmetry (%)", 
           y = "Jump Height (Inches)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$lmStatsTable <- renderTable({
    asymData <- asymFilteredPlayerData() %>%
      mutate(
        SI_Eccentric = ((Eccentric.Braking.Impulse..Left. - Eccentric.Braking.Impulse..Right.) / 
                          (0.5 * (Eccentric.Braking.Impulse..Left. + Eccentric.Braking.Impulse..Right.))) * 100,
        SI_Concentric = ((Concentric.Impulse..Left...Ns. - Concentric.Impulse..Right...Ns.) / 
                           (0.5 * (Concentric.Impulse..Left...Ns. + Concentric.Impulse..Right...Ns.))) * 100
      )
    
    models <- list(
      "Eccentric Braking Impulse vs Jump Height" = lm(Jump.Height..Imp.Mom..in.Inches ~ SI_Eccentric, data = asymData),
      "Eccentric Braking Impulse vs RSI" = lm(RSI.Modified..m.s. ~ SI_Eccentric, data = asymData),
      "Concentric Impulse vs RSI" = lm(RSI.Modified..m.s. ~ SI_Concentric, data = asymData),
      "Concentric Impulse vs Jump Height" = lm(Jump.Height..Imp.Mom..in.Inches ~ SI_Concentric, data = asymData)
    )
    
    lm_stats <- lapply(names(models), function(model_name) {
      model <- models[[model_name]]
      stats <- get_model_stats(model)
      data.frame(
        Model = model_name,
        `p-value` = stats$p_value,
        `R-squared` = stats$r_squared
      )
    })
    
    do.call(rbind, lm_stats)
  })
  
  # Reactive expression for time series data based on selected field position
  timeSeriesData <- reactive({
    jump_data %>%
      filter(Field.Position == input$timeSeriesFieldPosition)
  })
  
  # Update player choices based on selected field position for time series analysis
  output$timeSeriesPlayerUI <- renderUI({
    selectInput("timeSeriesPlayer", "Select Player ID:", choices = unique(timeSeriesData()$PlayerId))
  })
  
  timeSeriesFilteredData <- reactive({
    req(input$timeSeriesPlayer)
    timeSeriesData() %>%
      filter(PlayerId == input$timeSeriesPlayer)
  })
  
  # Calculate summary statistics for jump height and RSI over time for individual players
  time_series_analysis <- reactive({
    jump_data %>%
      group_by(PlayerId, Date) %>%
      summarise(
        mean_jump_height = mean(Jump.Height..Imp.Mom..in.Inches, na.rm = TRUE),
        mean_rsi = mean(RSI.Modified..m.s., na.rm = TRUE),
        mean_velo = mean(Velocity.at.Peak.Power..m.s., na.rm = TRUE)
      )
  })
  
  # Visualize the time series for individual players - Jump Height
  output$jumpHeightTimeSeriesPlot <- renderPlotly({
    req(timeSeriesFilteredData())
    player_data <- time_series_analysis() %>%
      filter(PlayerId == input$timeSeriesPlayer)
    
    p <- ggplot(player_data, aes(x = Date, y = mean_jump_height, color = as.factor(PlayerId))) +
      geom_line() +
      labs(title = "Jump Height Over Time for Individual Players", x = "Date", y = "Mean Jump Height") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Visualize the time series for individual players - RSI
  output$rsiTimeSeriesPlot <- renderPlotly({
    req(timeSeriesFilteredData())
    player_data <- time_series_analysis() %>%
      filter(PlayerId == input$timeSeriesPlayer)
    
    p <- ggplot(player_data, aes(x = Date, y = mean_rsi, color = as.factor(PlayerId))) +
      geom_line() +
      labs(title = "RSI Over Time for Individual Players", x = "Date", y = "Mean RSI") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Visualize the time series for individual players - Velocity
  output$veloTimeSeriesPlot <- renderPlotly({
    req(timeSeriesFilteredData())
    player_data <- time_series_analysis() %>%
      filter(PlayerId == input$timeSeriesPlayer)
    
    p <- ggplot(player_data, aes(x = Date, y = mean_velo, color = as.factor(PlayerId))) +
      geom_line() +
      labs(title = "Velocity at Peak Power Over Time for Individual Players", x = "Date", y = "Mean Velocity at Peak Power") +
      theme_minimal()
    
    ggplotly(p)
  })
  ########################################
  output$elbowPlot <- renderPlot({
    set.seed(123)
    
    # Select relevant columns and remove rows with missing values
    cluster_data <- jump_data %>%
      select(Jump.Height..Imp.Mom..in.Inches, Concentric.Peak.Force..N., Eccentric.Peak.Force..N., RSI.Modified..m.s., Contraction.Time..s., Mean.Takeoff.Velocity..m.s., Force.at.Peak.Power..N., Peak.Power..W., Eccentric.Duration..s., Concentric.Duration..s., Braking.Phase.Duration, Eccentric.Braking.Impulse, Concentric.Mean.Power..W., Eccentric.Mean.Power..W.) %>%
      na.omit()
    
    # Compute the WSS for different numbers of clusters
    wss <- sapply(1:15, function(k) {
      kmeans(cluster_data, centers = k, nstart = 10)$tot.withinss
    })
    
    # Plot the WSS against the number of clusters
    plot(1:15, wss, type = "b", pch = 19, frame = FALSE, 
         xlab = "Number of Clusters",
         ylab = "Total Within-Clusters Sum of Squares",
         main = "Elbow Plot for Optimal Number of Clusters")
  })
  #######################
  # Reactive clustering analysis based on the number of clusters selected by the user
  reactiveClustering <- reactive({
    set.seed(123)
    
    # Select relevant columns and remove rows with missing values
    cluster_data <- jump_data %>%
      select(Jump.Height..Imp.Mom..in.Inches, Concentric.Peak.Force..N., Eccentric.Peak.Force..N., RSI.Modified..m.s., Contraction.Time..s., Mean.Takeoff.Velocity..m.s., Force.at.Peak.Power..N., Peak.Power..W., Eccentric.Duration..s., Concentric.Duration..s., Braking.Phase.Duration, Eccentric.Braking.Impulse, Concentric.Mean.Power..W., Eccentric.Mean.Power..W.) %>%
      na.omit()
    
    # Filter original dataset to only include rows present in cluster_data
    filtered_jump_data <- jump_data %>%
      filter(complete.cases(select(jump_data, Jump.Height..Imp.Mom..in.Inches, Concentric.Peak.Force..N., Eccentric.Peak.Force..N., RSI.Modified..m.s., Contraction.Time..s., Mean.Takeoff.Velocity..m.s., Force.at.Peak.Power..N., Peak.Power..W., Eccentric.Duration..s., Concentric.Duration..s., Braking.Phase.Duration, Eccentric.Braking.Impulse, Concentric.Mean.Power..W., Eccentric.Mean.Power..W.)))
    
    # Perform k-means clustering with user-selected number of clusters
    clusters <- kmeans(cluster_data, centers = input$numClusters)
    
    # Add cluster labels to the filtered dataset
    filtered_jump_data$Cluster <- clusters$cluster
    
    list(cluster_data = cluster_data, filtered_jump_data = filtered_jump_data, clusters = clusters)
  })
  
  # Calculate and output cluster centroids
  reactiveClustering <- reactive({
    set.seed(123)
    
    # Select relevant columns and remove rows with missing values
    cluster_data <- jump_data %>%
      select(Jump.Height..Imp.Mom..in.Inches, Concentric.Peak.Force..N., Eccentric.Peak.Force..N., RSI.Modified..m.s., Contraction.Time..s., Mean.Takeoff.Velocity..m.s., Force.at.Peak.Power..N., Peak.Power..W., Eccentric.Duration..s., Concentric.Duration..s., Braking.Phase.Duration, Eccentric.Braking.Impulse, Concentric.Mean.Power..W., Eccentric.Mean.Power..W.) %>%
      na.omit()
    
    # Filter original dataset to only include rows present in cluster_data
    filtered_jump_data <- jump_data %>%
      filter(complete.cases(select(jump_data, Jump.Height..Imp.Mom..in.Inches, Concentric.Peak.Force..N., Eccentric.Peak.Force..N., RSI.Modified..m.s., Contraction.Time..s., Mean.Takeoff.Velocity..m.s., Force.at.Peak.Power..N., Peak.Power..W., Eccentric.Duration..s., Concentric.Duration..s., Braking.Phase.Duration, Eccentric.Braking.Impulse, Concentric.Mean.Power..W., Eccentric.Mean.Power..W.)))
    
    # Perform k-means clustering with user-selected number of clusters
    clusters <- kmeans(cluster_data, centers = input$numClusters)
    
    # Add cluster labels to the filtered dataset
    filtered_jump_data$Cluster <- clusters$cluster
    
    list(cluster_data = cluster_data, filtered_jump_data = filtered_jump_data, clusters = clusters)
  })
  
  # Calculate and output cluster centroids, split into three tables
  output$centroidsTable1 <- renderTable({
    clustering <- reactiveClustering()
    centroids <- aggregate(clustering$cluster_data, by = list(clustering$clusters$cluster), FUN = mean)
    colnames(centroids)[1] <- "Cluster"
    centroids_part1 <- centroids[, c("Cluster", "Jump.Height..Imp.Mom..in.Inches", "Concentric.Peak.Force..N.", "Eccentric.Peak.Force..N.", "RSI.Modified..m.s.")]
    centroids_part1
  })
  
  output$centroidsTable2 <- renderTable({
    clustering <- reactiveClustering()
    centroids <- aggregate(clustering$cluster_data, by = list(clustering$clusters$cluster), FUN = mean)
    colnames(centroids)[1] <- "Cluster"
    centroids_part2 <- centroids[, c("Cluster", "Contraction.Time..s.", "Mean.Takeoff.Velocity..m.s.", "Force.at.Peak.Power..N.", "Peak.Power..W.")]
    centroids_part2
  })
  
  output$centroidsTable3 <- renderTable({
    clustering <- reactiveClustering()
    centroids <- aggregate(clustering$cluster_data, by = list(clustering$clusters$cluster), FUN = mean)
    colnames(centroids)[1] <- "Cluster"
    centroids_part3 <- centroids[, c("Cluster", "Eccentric.Duration..s.", "Concentric.Duration..s.", "Braking.Phase.Duration", "Eccentric.Braking.Impulse", "Concentric.Mean.Power..W.", "Eccentric.Mean.Power..W.")]
    centroids_part3
  })
  
  # Plot visualizations for the clustering results using plotly for interactivity
  output$jumpHeightVsConcentricPeakForce <- renderPlotly({
    clustering <- reactiveClustering()
    plot_ly(
      clustering$filtered_jump_data,
      x = ~Jump.Height..Imp.Mom..in.Inches,
      y = ~Concentric.Peak.Force..N.,
      color = ~as.factor(Cluster),
      type = 'scatter',
      mode = 'markers',
      text = ~paste('Player ID:', PlayerId, '<br>Position:', Field.Position),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Jump Height vs. Concentric Peak Force",
        xaxis = list(title = "Jump Height (Inches)"),
        yaxis = list(title = "Concentric Peak Force (N)")
      )
  })
  
  output$eccentricPeakForceVsConcentricPeakForce <- renderPlotly({
    clustering <- reactiveClustering()
    plot_ly(
      clustering$filtered_jump_data,
      x = ~Eccentric.Peak.Force..N.,
      y = ~Concentric.Peak.Force..N.,
      color = ~as.factor(Cluster),
      type = 'scatter',
      mode = 'markers',
      text = ~paste('Player ID:', PlayerId, '<br>Position:', Field.Position),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Eccentric Peak Force vs. Concentric Peak Force",
        xaxis = list(title = "Eccentric Peak Force (N)"),
        yaxis = list(title = "Concentric Peak Force (N)")
      )
  })
  
  output$rsiVsContractionTime <- renderPlotly({
    clustering <- reactiveClustering()
    plot_ly(
      clustering$filtered_jump_data,
      x = ~RSI.Modified..m.s.,
      y = ~Contraction.Time..s.,
      color = ~as.factor(Cluster),
      type = 'scatter',
      mode = 'markers',
      text = ~paste('Player ID:', PlayerId, '<br>Position:', Field.Position),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "RSI vs. Contraction Time",
        xaxis = list(title = "RSI (m/s)"),
        yaxis = list(title = "Contraction Time (s)")
      )
  })
  
  output$meanTakeoffVelocityVsJumpHeight <- renderPlotly({
    clustering <- reactiveClustering()
    plot_ly(
      clustering$filtered_jump_data,
      x = ~Mean.Takeoff.Velocity..m.s.,
      y = ~Jump.Height..Imp.Mom..in.Inches,
      color = ~as.factor(Cluster),
      type = 'scatter',
      mode = 'markers',
      text = ~paste('Player ID:', PlayerId, '<br>Position:', Field.Position),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Mean Takeoff Velocity vs. Jump Height",
        xaxis = list(title = "Mean Takeoff Velocity (m/s)"),
        yaxis = list(title = "Jump Height (Inches)")
      )
  })
  
  output$forceAtPeakPowerVsPeakPower <- renderPlotly({
    clustering <- reactiveClustering()
    plot_ly(
      clustering$filtered_jump_data,
      x = ~Force.at.Peak.Power..N.,
      y = ~Peak.Power..W.,
      color = ~as.factor(Cluster),
      type = 'scatter',
      mode = 'markers',
      text = ~paste('Player ID:', PlayerId, '<br>Position:', Field.Position),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Force at Peak Power vs. Peak Power",
        xaxis = list(title = "Force at Peak Power (N)"),
        yaxis = list(title = "Peak Power (W)")
      )
  })
  
  output$eccentricDurationVsConcentricDuration <- renderPlotly({
    clustering <- reactiveClustering()
    plot_ly(
      clustering$filtered_jump_data,
      x = ~Eccentric.Duration..s.,
      y = ~Concentric.Duration..s.,
      color = ~as.factor(Cluster),
      type = 'scatter',
      mode = 'markers',
      text = ~paste('Player ID:', PlayerId, '<br>Position:', Field.Position),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Eccentric Duration vs. Concentric Duration",
        xaxis = list(title = "Eccentric Duration (s)"),
        yaxis = list(title = "Concentric Duration (s)")
      )
  })
  
  output$brakingPhaseDurationVsEccentricBrakingImpulse <- renderPlotly({
    clustering <- reactiveClustering()
    plot_ly(
      clustering$filtered_jump_data,
      x = ~Braking.Phase.Duration,
      y = ~Eccentric.Braking.Impulse,
      color = ~as.factor(Cluster),
      type = 'scatter',
      mode = 'markers',
      text = ~paste('Player ID:', PlayerId, '<br>Position:', Field.Position),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Braking Phase Duration vs. Eccentric Braking Impulse",
        xaxis = list(title = "Braking Phase Duration"),
        yaxis = list(title = "Eccentric Braking Impulse")
      )
  })
  
  output$concentricMeanPowerVsEccentricMeanPower <- renderPlotly({
    clustering <- reactiveClustering()
    plot_ly(
      clustering$filtered_jump_data,
      x = ~Concentric.Mean.Power..W.,
      y = ~Eccentric.Mean.Power..W.,
      color = ~as.factor(Cluster),
      type = 'scatter',
      mode = 'markers',
      text = ~paste('Player ID:', PlayerId, '<br>Position:', Field.Position),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Concentric Mean Power vs. Eccentric Mean Power",
        xaxis = list(title = "Concentric Mean Power (W)"),
        yaxis = list(title = "Eccentric Mean Power (W)")
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
