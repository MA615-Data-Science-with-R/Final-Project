# Part 1: Load Libraries, Datasets, and Define UI Layout
# Load necessary libraries
library(shiny)
library(ggplot2)
library(reshape2)
library(leaflet)
library(shinythemes)

# Load datasets
cleaned_data <- read.csv("Cleaned_South_Asian_Data.csv")
tourism_data <- read.csv("Cleaned_Tourism_Data.csv")
suicides_data <- read.csv("suicides_cleaned.csv")
opportunities_data <- read.csv("cleaned_opportunities.csv")
threat_data <- read.csv("consolidated_threat_data.csv")

# Clean column names to ensure consistency
colnames(suicides_data) <- gsub("\\s", ".", colnames(suicides_data))
colnames(opportunities_data) <- gsub("\\s", ".", colnames(opportunities_data))
colnames(threat_data) <- gsub("\\s", ".", colnames(threat_data))

# Define the UI layout
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Sri Lanka Insights"),
  sidebarLayout(
    sidebarPanel(
      h4("Navigation"),
      actionButton("btn_general", "General Description"),
      actionButton("btn_demographics", "Key Demographics"),
      actionButton("btn_analysis", "Comparative Analysis"),
      actionButton("btn_swot", "SWOT Analysis"),
      actionButton("btn_references", "References")
    ),
    mainPanel(
      uiOutput("tabs_ui"),  # Dynamic tabs
      uiOutput("content")   # Dynamic content
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to track the selected main section
  section <- reactiveVal("welcome")
  
  # Update section when buttons are clicked
  observeEvent(input$btn_general, { section("general") })
  observeEvent(input$btn_demographics, { section("demographics") })
  observeEvent(input$btn_analysis, { section("analysis") })
  observeEvent(input$btn_swot, { section("swot") })
  observeEvent(input$btn_references, { section("references") })
  
  # Dynamically generate tabs based on the selected section
  output$tabs_ui <- renderUI({
    if (section() == "general") {
      tabsetPanel(
        tabPanel("Global Location", value = "global"),
        tabPanel("Key Facts", value = "key_facts"),
        tabPanel("General Description", value = "desc"),
        id = "tabs_ui"  # Assign ID to the tabsetPanel
      )
    } else if (section() == "demographics") {
      tabsetPanel(
        tabPanel("Ethnic Groups", value = "ethnic"),
        tabPanel("Languages Spoken", value = "languages"),
        tabPanel("Population", value = "population"),
        id = "tabs_ui"  # Assign ID to the tabsetPanel
      )
    } else if (section() == "analysis") {
      tabsetPanel(
        tabPanel("GDP Comparison", value = "gdp_comparison"),
        tabPanel("Population Comparison", value = "population_comparison"),
        tabPanel("GDP vs Population", value = "gdp_vs_population"),
        id = "tabs_ui"  # Assign ID to the tabsetPanel
      )
    } else if (section() == "swot") {
      tabsetPanel(
        tabPanel("Strength", value = "strength"),
        tabPanel("Weakness", value = "weakness"),
        tabPanel("Opportunity", value = "opportunity"),
        tabPanel("Threat", value = "threat"),
        id = "tabs_ui"
      )
    } else {
      NULL
    }
  })
  
  # Render content for General Information
  output$content <- renderUI({
    if (section() == "general") {
      current_tab <- if (!is.null(input$tabs_ui)) input$tabs_ui else "global"
      
      switch(
        current_tab,
        "global" = tagList(
          h3("Global Location"),
          p("Sri Lanka is an island nation in South Asia, located in the Indian Ocean, southwest of the Bay of Bengal, 
             and southeast of the Arabian Sea. It lies to the southeast of India, separated by the Gulf of Mannar and the Palk Strait. 
             Sri Lanka shares maritime borders with the Maldives to the southwest and India to the northwest."),
          leafletOutput("sriLankaMap", height = "400px")
        ),
        "key_facts" = tagList(
          h3("Key Facts"),
          tags$ul(
            tags$li("It is nicknamed Pearl of the Indian Ocean and teardrop of India."),
            tags$li("It's packed with a large variety of animals."),
            tags$li("Tea is a big business in Sri Lanka."),
            tags$li("It is home of cinnamon."),
            tags$li("Adam's Peak is the most sacred mountain in the country."),
            tags$li("The Sri Lankan national flag is one of the oldest in the world."),
            tags$li("Volleyball is the national sport of Sri Lanka."),
            tags$li("It is home to the world's oldest human-planted tree.")
          )
        ),
        "desc" = tagList(
          h3("General Description"),
          p("Sri Lanka, historically known as Ceylon, and officially the Democratic Socialist Republic of Sri Lanka, 
             is an island country in South Asia. It lies in the Indian Ocean, southwest of the Bay of Bengal, 
             separated from the Indian peninsula by the Gulf of Mannar and the Palk Strait."),
          p("Sri Lanka has a population of approximately 22 million and is home to several cultures, languages, and ethnicities. 
             The Sinhalese people form the majority of the population, followed by the Sri Lankan Tamils, who are the largest minority group."),
          tags$img(src = "image1.jpeg", height = "300px", width = "400px", alt = "Sri Lanka Image 1"),
          tags$img(src = "images.jpeg", height = "300px", width = "400px", alt = "Sri Lanka Image 2")
        )
      )
    } else if (section() == "demographics") {
      current_tab <- if (!is.null(input$tabs_ui)) input$tabs_ui else "ethnic"
      
      switch(
        current_tab,
        "ethnic" = tagList(
          h3("Ethnic Groups"),
          p("Sri Lanka's ethnic groups include:"),
          tags$ul(
            tags$li("Sinhalese: The majority ethnic group, making up about 75% of the population."),
            tags$li("Sri Lankan Tamils: A predominantly Hindu ethnic group, making up about 15% of the population."),
            tags$li("Indian Tamils: Descendants of plantation workers brought from Tamil Nadu."),
            tags$li("Sri Lankan Moors: Predominantly Muslim, with ancestry tracing back to Arab traders."),
            tags$li("Burghers: A small Eurasian ethnic group descended from Europeans."),
            tags$li("Sri Lankan Malays: Of mixed Javanese, Malay, and Arabic origins."),
            tags$li("Sri Lanka Kaffirs: Descendants of Portuguese traders and Bantu slaves.")
          )
        ),
        "languages" = tagList(
          h3("Languages Spoken"),
          p("Sri Lanka has two official languages, Sinhala and Tamil:"),
          tags$ul(
            tags$li("Sinhala: Widely spoken in the southern, western, and central parts of the island."),
            tags$li("Tamil: Predominantly spoken in northern and eastern parts of the island."),
            tags$li("English: Widely used as a second language in business and education.")
          )
        ),
        "population" = tagList(
          h3("Population"),
          p("Sri Lanka has a population of approximately 22 million people."),
          p("The population is distributed across diverse ethnic groups, with Sinhalese forming the majority, followed by Sri Lankan Tamils, Indian Tamils, Moors, and other smaller communities.")
        )
      )
    } else if (section() == "analysis") {
      current_tab <- if (!is.null(input$tabs_ui)) input$tabs_ui else "gdp_comparison"
      
      switch(
        current_tab,
        "gdp_comparison" = tagList(
          h3("GDP Comparison"),
          plotOutput("gdp_plot"),
          p("India's GDP significantly outpaces all other South Asian countries, highlighting its dominant economic position in the region. Sri Lanka's GDP, while smaller, reflects a developing economy with opportunities for growth compared to its neighbors like Nepal and Bhutan.")
        ),
        "population_comparison" = tagList(
          h3("Population Comparison"),
          plotOutput("population_plot"),
          p("India and Pakistan have notably larger populations compared to other South Asian countries, which could be both a resource and a challenge. Sri Lanka's smaller population may allow for better resource allocation and focused development efforts.")
        ),
        "gdp_vs_population" = tagList(
          h3("GDP vs Population"),
          plotOutput("gdp_vs_population_plot"),
          p("India's position on the graph indicates a strong correlation between its large population and high GDP. Sri Lanka, with a smaller population and moderate GDP, stands out as having potential to improve economic output without demographic strain.")
        )
      )
    } else if (section() == "swot") {
      current_tab <- if (!is.null(input$tabs_ui)) input$tabs_ui else "strength"
      
      switch(
        current_tab,
        "strength" = tagList(
          h3("Strength: Tourism Industry"),
          p("Sri Lanka's tourism industry is a key strength, contributing significantly to its economy."),
          plotOutput("tourism_gdp_plot"),
          plotOutput("tourism_per_capita_plot"),
          p("Tourism has been a consistent strength for Sri Lanka, with GDP contributions showing significant growth until 2018. However, the sharp decline in recent years suggests disruptions, possibly due to global events like the pandemic.")
        ),
        "weakness" = tagList(
          h3("Weakness: Mental Health Challenges"),
          p("Sri Lanka faces significant challenges with mental health, as indicated by the high suicide rates across age groups."),
          plotOutput("suicide_plot"),
          ("Suicide rates across different age groups remain a concerning issue in Sri Lanka. The high rates in young and middle-aged groups emphasize the need for better mental health resources and interventions.")
        ),
        "opportunity" = tagList(
          h3("Opportunity: Agricultural Potential"),
          p("Sri Lanka's districts have significant potential for agricultural production, as shown by their total production values."),
          plotOutput("opportunity_plot"),
          p("Sri Lanka's agricultural production is highly concentrated in certain districts like Polonnaruwa and Anuradhapura, with significant contributions. This highlights an opportunity to diversify and develop underutilized regions to boost overall production.")
        ),
        "threat" = tagList(
          h3("Threat: Natural Disasters"),
          p("Sri Lanka faces various natural disasters, causing significant human and economic losses."),
          plotOutput("threat_plot"),
          p("Natural disasters such as floods, landslides, and cyclones have caused considerable human and economic losses in Sri Lanka. Floods, in particular, stand out as the most devastating event, underlining the need for effective disaster management and mitigation strategies.")
        )
      )
    }
    else if (section() == "references") {
      tagList(
        h3("References"),
        p("https://www.indiaodysseytours.com/knows/facts-about-sri-lanka.html"),
        p("https://www.maiglobetravels.com/language-in-sri-lanka"),
        p("https://www.kaggle.com"),
        p("https://www.imf.org/external/datamapper/profile/LKA"),
        p("Google AI & ChatGPT (For content + Report)")
      )
    } else {
      h3("Welcome to Sri Lanka Insights!")
    }
  })
  
  # Render map
  output$sriLankaMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = 80.7718, lat = 7.8731, popup = "Sri Lanka")
  })
  
  # Generate plots for Comparative Analysis
  output$gdp_plot <- renderPlot({
    ggplot(cleaned_data, aes(x = reorder(Country, Avg_GDP), y = Avg_GDP)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Average GDP by Country", x = "Country", y = "Average GDP (USD)") +
      theme_minimal()
  })
  
  output$population_plot <- renderPlot({
    ggplot(cleaned_data, aes(x = reorder(Country, Avg_Population), y = Avg_Population)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      labs(title = "Average Population by Country", x = "Country", y = "Average Population") +
      theme_minimal()
  })
  
  output$gdp_vs_population_plot <- renderPlot({
    ggplot(cleaned_data, aes(x = Avg_Population, y = Avg_GDP, label = Country)) +
      geom_point(color = "purple", size = 3) +
      geom_text(vjust = -1, hjust = 1) +
      labs(title = "GDP vs Population", x = "Average Population", y = "Average GDP (USD)") +
      theme_minimal()
  })
  
  # Render plots for SWOT Analysis
  output$tourism_gdp_plot <- renderPlot({
    ggplot(tourism_data, aes(x = Year, y = GDP_Billions)) +
      geom_line(color = "blue", size = 1) +
      labs(title = "Tourism GDP Over Years", x = "Year", y = "GDP (Billions)") +
      theme_minimal()
  })
  
  output$tourism_per_capita_plot <- renderPlot({
    ggplot(tourism_data, aes(x = Year, y = Per_Capita)) +
      geom_line(color = "red", size = 1) +
      labs(title = "Per Capita Tourism Income Over Years", x = "Year", y = "Per Capita Income") +
      theme_minimal()
  })
  
  output$suicide_plot <- renderPlot({
    suicides_long <- reshape2::melt(
      suicides_data[-nrow(suicides_data), ],
      id.vars = "Age.group",
      variable.name = "Year",
      value.name = "Suicides"
    )
    suicides_long$Year <- gsub("_Total", "", suicides_long$Year)
    suicides_long$Year <- gsub("X", "", suicides_long$Year)
    
    ggplot(suicides_long, aes(x = Age.group, y = Suicides, color = Year, group = Year)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Suicide Trends by Age Group and Year",
        x = "Age Group",
        y = "Number of Suicides",
        color = "Year"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$opportunity_plot <- renderPlot({
    ggplot(opportunities_data, aes(x = reorder(District, -Total_Production), y = Total_Production)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      labs(
        title = "Agricultural Production by District",
        x = "District",
        y = "Total Production"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$threat_plot <- renderPlot({
    threat_long <- reshape2::melt(
      threat_data,
      id.vars = "Event",
      variable.name = "Metric",
      value.name = "Value"
    )
    ggplot(threat_long, aes(x = reorder(Event, -Value), y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = "Impact of Natural Disasters",
        x = "Event",
        y = "Impact (Deaths, Injuries, Losses)",
        fill = "Metric"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }    )
}

shinyApp(ui, server)
