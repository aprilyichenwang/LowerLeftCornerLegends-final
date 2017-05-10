list.of.packages <- c("shiny", 'dplyr', 'plyr', 'tidyr', 'reshape2', 'ggplot2', 'scales', 'grid', 'gridExtra',
                      'RColorBrewer', 'networkD3', 'plotly', 'maps', 'mapproj', 'cluster', 'gplots', 'reshape',
                      'igraph', 'shinythemes')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(networkD3)
library(plotly)
library(maps)
library(mapproj)
library(cluster)
library(gplots)
library(reshape)
library(igraph)
library(shinythemes)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

######################
#### Prepare Data ####
######################

colonies <- read.csv('clean_data/colonies.csv', stringsAsFactors = FALSE)
honey <- read.csv('clean_data/honey.csv', stringsAsFactors = FALSE)
pollination <- read.csv('clean_data/pollination.csv', stringsAsFactors = FALSE)
stressors <- read.csv('clean_data/stressors.csv', stringsAsFactors = FALSE)

# get map data
map <- ggplot2::map_data("state") %>% select(long, lat, group, order, region)

# cleaning
# make all first letters of state names capitalized for merging
map$region <- sapply(map$region, simpleCap)
# remove all rows with year = 2016 in colonies and stressors data
colonies <- colonies[colonies$year != 2016,]
stressors <- stressors[stressors$year != 2016,]

# merge colonies with stressors data
colonies_stressors <- merge(colonies, stressors, by=c("state", "time_period", "year"))

poll_data <- read.csv("clean_data/pollination.csv")

levels(poll_data$region) <- c(levels(poll_data$region), "6")
poll_data$region <- replace(poll_data$region, poll_data$region=="6 & 7", "6")
poll_data$region <- droplevels(poll_data$region)

levels(poll_data$crop) <- c(levels(poll_data$crop), "Other Veg") 
poll_data$crop <- replace(poll_data$crop, poll_data$crop == "Other vegetables", "Other Veg")
poll_data$crop <- droplevels(poll_data$crop)
levels(poll_data$region) <- c("Region 1", "Region 2", "Region 3", "Region 4",
                              "Region 5", "Region 6")
totals <- poll_data %>%
  group_by(region, year) %>%
  summarise(total_col = sum(colonies_used),
            total_val = sum(total_value_of_pollination_1000USD),
            total_acr = sum(paid_pollinated_acres))

poll_data <- merge(poll_data, totals, sort = FALSE, by = c("region", "year"))
sankey_2015 <- poll_data %>% 
  filter(year == 2015) %>%
  select(region, crop, colonies_used)
sankey_2015$region <- as.character(sankey_2015$region)
sankey_2015$crop <- as.character(sankey_2015$crop)
colnames(sankey_2015) <- c("region", "crop", "colonies.used")

## map data ##
map_data <- ggplot2::map_data("state")
map_data <- map_data %>% select(-subregion)
colnames(map_data) <- c("long", "lat", "group", "order", "states") 

Region1 <- c("Connecticut", "Illinois", "Indiana", "Iowa", "Kansas", "Massachusetts", "Maine", "Michigan", "Nebraska", "New Hampshire", "New Jersey", "New York", "Ohio", "Pennsylvania", "Rhode Island", "Vermont", "Wisconsin")
Region2 <- c("Alabama", "Delaware", "Georgia", "Kentucky", "Maryland", "North Carolina", "South Carolina", "Tennessee", "Virginia", "West Virginia")
Region3 <- c("Arkansas", "Florida", "Louisiana", "Missouri", "Mississippi", "New Mexico", "Oklahoma", "Texas")
Region4 <- c("Colorado", "Minnesota", "Montana", "Nevada", "North Dakota", "South Dakota", "Utah", "Wyoming")
Region5 <- c("Idaho", "Oregon", "Washington")
Region6 <- c("Arizona", "California")
states <- c(Region1, Region2, Region3, Region4, Region5, Region6)
states <- data.frame(states)

states$region <- "Region 1"

rows2 <- states$states %in% Region2
states$region[rows2] <- "Region 2"

rows3 <- states$states %in% Region3
states$region[rows3] <- "Region 3"

rows4 <- states$states %in% Region4
states$region[rows4] <- "Region 4"

rows5 <- states$states %in% Region5
states$region[rows5] <- "Region 5"

rows6 <- states$states %in% Region6
states$region[rows6] <- "Region 6"

states$states <- tolower(states$states)

map_regions <- merge(map_data, states, sort = FALSE, by = "states")
map_regions <- map_regions %>% select(long, lat, group, order, region)
map_regions <- map_regions[order(map_regions$order),]

appCSS <- 
  "#color ~ .selectize-control.single .selectize-dropdown [data-value=\"Region 1\"] { color: #ff9900 }
#color ~ .selectize-control.single .selectize-dropdown [data-value=\"Region 2\"] { color: #3366cc }
#color ~ .selectize-control.single .selectize-dropdown [data-value=\"Region 3\"] { color: #FF1493 }
#color ~ .selectize-control.single .selectize-dropdown [data-value=\"Region 4\"] { color: #228B22 }
#color ~ .selectize-control.single .selectize-dropdown [data-value=\"Region 5\"] { color: #B22222 }
#color ~ .selectize-control.single .selectize-dropdown [data-value=\"Region 6\"] { color: #40E0D0 }
"
values <- c("Region 1" = "#ff9900", "Region 2" = "#3366cc", "Region 3" = "#FF1493",
            "Region 4" = "#228B22", "Region 5" = "#B22222", "Region 6" = "#40E0D0")

# remove Almonds for the map plots
poll_data_A <- poll_data %>% filter(crop != "Almond") 
poll_data_A$crop <- droplevels(poll_data_A$crop)

# total per crop category for map plots
crop_cat_total <- poll_data_A %>%
  group_by(year, crop_category) %>%
  summarise(total_col = sum(colonies_used),
            total_val = sum(total_value_of_pollination_1000USD),
            total_acr = sum(paid_pollinated_acres))

# total per crop category per region for map plots
region_cat_total <- poll_data_A %>%
  group_by(region, year, crop_category) %>%
  summarise(total_col = sum(colonies_used),
            total_val = sum(total_value_of_pollination_1000USD),
            total_acr = sum(paid_pollinated_acres))

# limit to 2015 for Sankey
poll_data_S <- poll_data %>%
  filter(year == 2015)
colnames(poll_data_S) <- c("region", "year", "crop", "Pollinated.Acres",
                           "Price.per.Acre.USD", "Colonies", "Price.per.Colony.USD",
                           "Total.Pollination.Value.USD", "Crop.category", "total_col", 
                           "total_val", "total_acr")

# for labeling nodes and links in Sankey
a <- c(0:5)
b <- rep(6, 25)
c <- c(a, b) # to make node colors
d <- rep(22, 61) # to make link colors

###################
#### Shiny App ####
###################

ui <- fluidPage(theme = shinytheme("paper"),
  tabsetPanel(
    tabPanel(title="Data Description",
             tags$h3(tags$b("A Study of the Pollination, Honey Production, and Risk Factors for Bees in the US")),
             "Bee populations are declining at unusually high rates. Much of this is due to climate change, parasites, 
             diseases, and industrial agriculture. This is an alarming phenomenon as bees are essential for food production. 
             In these visualizations, we provide insight into this phenomenon in order to help users understand this issue 
             better.", tags$hr(), tags$h4(tags$b(tags$u("Dataset"))), "The public data set we worked with is from the United States Department of Agriculture (USDA). 
             The USDA collects data on stressors to bee colonies, changes in numbers of bee colonies, honey production, and 
             pollination costs in order to help track risk factors and mortality for our nation’s main pollinators. 
             We specifically looked at 3 csv files spanning 2015-2016. The files analyzed consisted of numerical, categorical, 
             as well as temporal data. Our analysis focuses attention on the current decline of honey bees as well as their 
             importance in the production of the crops we consume daily.", tags$br(), tags$br(), 
             "We gathered data from the following sources:", 
             tags$ol(
               tags$li(tags$a(href="https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Bee_and_Honey/", 
                                    "https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Bee_and_Honey/")),
               tags$li(tags$a(href="http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=1191",
                              "http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=1191")),
               tags$li(tags$a(href="http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=2008",
                              "http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=2008"))), tags$hr(), 
             tags$h4(tags$b(tags$u("Description of Visualizations"))), tags$h5("Overview"), "This visualization provides a general 
             overview of the datasets. It provides a high level of interactivity to the user to understand and 
             explore all different aspects of the data.", tags$h5("Bee Colony Numbers & Stressors"), 
             "This visualization provides the user with a geographical interface, as well as with more detailed 
             information about bee colony numbers and stressors to bee colonies in each state.", 
             tags$h5("Pollination in US Agriculture"), "This visualization focuses attention 
             on the importance of honey bees in US agriculture. The geographical visualization focuses on 6 agricultural 
             regions throughout the US.",
             tags$h5("Flow of Resources Per Crop"), "This visualization provides a 
             closer look at the dependency of agriculture on honey bees. The Sankey diagram visualizes the flow of 
             resources per crop in each agricultural region in the US for the year 2015."),
    tabPanel(title="Overview",
             sidebarPanel(selectInput("overview_dataset", "Step 1. Choose a Dataset", c("Bee Colony Numbers", "Stressors to Bee Colonies", "Pollination", "Honey Production")),
                          uiOutput("overview_step2"),
                          uiOutput("overview_step3"),
                          uiOutput("overview_step4"),
                          uiOutput("overview_step5"),
                          tags$div(class="header", style="font-size:80%", checked=NA,
                                   "All of the data in this visualization is for the year 2015."),
                          width=3),
             mainPanel(tags$style(type="text/css", # to suppress error messages that pop up very shortly when reacting to user input
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       conditionalPanel(condition = "input.overview_dataset == 'Bee Colony Numbers'",
                                        plotOutput("overview1", height=600, width=900)),
                       conditionalPanel(condition = "input.overview_dataset == 'Stressors to Bee Colonies'",
                                        plotOutput("overview2", height=600, width=900)),
                       conditionalPanel(condition = "input.overview_dataset == 'Pollination'",
                                        diagonalNetworkOutput("overview3")),
                       conditionalPanel(condition = "input.overview_dataset == 'Honey Production'",
                                        plotOutput("overview4", height=600, width=900)))),
    tabPanel(title="Bee Colony Numbers & Stressors",
             mainPanel(fluidRow(column(width=6, offset=0,
                                       selectInput("map_choropleth", "Color By:", c("Maximum Number of Colonies" = "max_colonies", 
                                                                                    "Number of Colonies Lost" = "lost_colonies",
                                                                                    "Percentage of Colonies Lost" = "percent_lost_colonies",
                                                                                    "Number of Colonies Added" = "added_colonies",
                                                                                    "Number of Renovated Colonies" = "renovated_colonies",
                                                                                    "Percentage of Colonies Renovated" = "percent_renovated_colonies",
                                                                                    "Percentage of Colonies Affected by Varroa Mites" = "varroa_mites",
                                                                                    "Percentage of Colonies Affected by Other Pests or Parasites" = "other_pests_parasites",
                                                                                    "Percentage of Colonies Affected by Disease" = "diseases",
                                                                                    "Percentage of Colonies Affected by Pesticides" = "pesticides",
                                                                                    "Percentage of Colonies Affected by Other Factors" = "other",
                                                                                    "Percentage of Colonies Affected by Unknown Factors" = "unknown"),
                                                   selected="percent_lost_colonies")),
                                column(width=6, offset=0,
                                       selectInput("map_time_period", "Time Period:", c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")))),
                       plotlyOutput("map_bar"), width=7
             ),
             sidebarPanel(tags$style(".well {background-color:white;}"), 
                          tags$b("Plot Summary:"), tags$br(),
                          tags$div(class="header", style="font-size:90%", checked=NA,
                              tags$i("Bee populations are declining at unusually high rates 
                                  due to climate change, parasites, 
                                 diseases, and industrial agriculture. All data in 
                                this visualization is for the year 2015. 
                                 White states have no data.")), 
                          # tags$div(class="header", style="font-size:80%", checked=NA,
                          #          "All of the data in these visualizations are for the year 2015. 
                          #          White states have no data."),
                          tags$div(class="header", checked=NA, style="text-align:center",
                                   tags$i("Click on a state for more detailed info about that specific state.")),
                          plotOutput("map_click_plot", click = "map_click_vals"), width=5),
             plotOutput("map_bar_plot", height="200px")
    ),
    tabPanel("Pollination in US Agriculture", tags$head(tags$style(HTML(appCSS))),
             sidebarPanel(tags$b("Plot Summary:"), tags$br(),
                          tags$i("Bees play an important role in US agricultural production,
                                         pollinating many types of crops throughout the country. 
                                         They pollinate thousands of acres every year, providing farmers
                                         with an invaluable resource and consumers with numerous fruits and 
                                         vegetables."), tags$br(), tags$br(),
                          selectInput("year", "Select Year:", c("2015", "2016")),
                          selectInput("color", "Select Agricultural Region:",
                                      c("Region 1", "Region 2", "Region 3", "Region 4",
                                        "Region 5", "Region 6"), selected = "Region 5"),
                          width=3),
             mainPanel(tags$head(tags$style(HTML(appCSS))),
             fluidRow(
               column(8, offset = 2, plotOutput("States", width = "100%", height = "300px", 
                                                hover = hoverOpts(id = "plot_hover", delay = 100, 
                                                                  delayType = "debounce")), 
                      uiOutput("hover_info"))),
             fluidRow(
               splitLayout(cellWidths = c("50%", "50%"), plotOutput("Crops1"), plotOutput("Crops2")
               )), value = 1)
    ),
    tabPanel("Flow of Resources Per Crop",
             sidebarPanel(tags$b("Plot Summary:"), tags$br(),
                          tags$i("Different crops apply different demands on
                                         agricultural and pollination resources. 
                                         Certain crops are particularly dependent 
                                         on bee pollination and could suffer greatly from
                                         declining bee populations. All of the data in 
                                        this visualization is for the year 2015."), tags$br(), tags$br(),
                          selectInput("sankey_variable", "Select Variable for Analysis:",
                                      c("Bee Colonies" = "Colonies", 
                                        "Pollinated Acres" = "Pollinated.Acres",
                                        "Price Per Colony" = "Price.per.Colony.USD", 
                                        "Price Per Acre" = "Price.per.Acre.USD")),
                          checkboxInput("check", "Colored Links", value = FALSE),
                          tags$b("Agricultural Regions Legend:"),
                          img(src="usa.png", height = 260, width = 380)),
             mainPanel(tags$head(tags$style(HTML(appCSS))),
                       fluidRow(
                         column(8, offset = 2, plotOutput("blank", width = "100%", height = "60px"))),
                       fluidRow(
                         column(8, offset = 2, sankeyNetworkOutput("sankey", width = "100%"))
                       )), value = 2)
  )
)

server <- function(input, output) {
  
  #######################
  #### Overview Plot ####
  #######################
  
  output$overview_step2 <- renderUI({
    if (is.null(input$overview_dataset)) {
      return(NULL)
    } else if (input$overview_dataset == "Bee Colony Numbers") {
      return(selectInput("overview_colonies_facet", "Step 2: Choose a Grouping", c("Bee Colony Numbers Fields", "Time Period")))
    } else if (input$overview_dataset == "Stressors to Bee Colonies") {
      return(selectInput("overview_colonies_facet", "Step 2: Choose a Grouping", c("Stressors Fields", "Time Period")))
    } else if (input$overview_dataset == "Pollination") {
      return(selectInput("overview_colonies_facet", "Step 2: Choose a Grouping", c("Crop Category", "Region")))
    } else if (input$overview_dataset == "Honey Production") {
      return(checkboxGroupInput("overview_colonies_facet", "Step 2: Choose Groups to Visualize", c("# of Honey Producing Colonies (1,000 Colonies)",
                                                                                                   "Honey Yield per Colony (lbs)", "Honey Production (1,000 lbs)",
                                                                                                   "Stocks Held by Honey Producers (1,000 lbs)", 
                                                                                                   "Average Price Per Pound of Honey (cents)", 
                                                                                                   "Value of Honey Production (1,000 USD)")))
    }
  })
  
  output$overview_step3 <- renderUI({
    if (input$overview_dataset == "Honey Production") {
      state_names <- unique(honey$state)
      len_state_names <- length(state_names)
      state_names <- state_names[c(-(length(state_names)-1), -length(state_names))]
      return(selectInput("overview_colonies_states", "Step 3: Choose States to Highlight", state_names, multiple=TRUE, selected=NULL))
    }
    
    if (is.null(input$overview_colonies_facet)) {
      return(NULL)
    } else if ((input$overview_colonies_facet == "Bee Colony Numbers Fields") & (input$overview_dataset == "Bee Colony Numbers")) {
      return(checkboxGroupInput("overview_colonies_fields", "Step 3: Choose Groups to Visualize", c("Maximum Number of Colonies", "Number of Colonies Lost", 
                                                                                                    "Percent of Colonies Lost", "Number of Colonies Added", 
                                                                                                    "Number of Colonies Renovated", "Percent of Colonies Renovated")))
    } else if ((input$overview_colonies_facet == "Time Period") & (input$overview_dataset == "Bee Colony Numbers")) {
      return(selectInput("overview_colonies_fields", "Step 3: Choose a Category", c("Maximum Number of Colonies", "Number of Colonies Lost", 
                                                                                    "Percent of Colonies Lost", "Number of Colonies Added", 
                                                                                    "Number of Colonies Renovated", "Percent of Colonies Renovated")))
    } else if ((input$overview_colonies_facet == "Stressors Fields") & (input$overview_dataset == "Stressors to Bee Colonies")) {
      return(checkboxGroupInput("overview_colonies_fields", "Step 3: Choose Groups to Visualize", c("% Colonies Affected by Varroa Mites", "% Colonies Affected by Other Pests/Parasites",
                                                                                                    "% Colonies Affected by Diseases", "% Colonies Affected by Pesticides",
                                                                                                    "% Colonies Affected by Other Factors", "% Colonies Affected by Unknown Factors")))
    } else if ((input$overview_colonies_facet == "Time Period") & (input$overview_dataset == "Stressors to Bee Colonies")) {
      return(selectInput("overview_colonies_fields", "Step 3: Choose a Category", c("% Colonies Affected by Varroa Mites", "% Colonies Affected by Other Pests/Parasites",
                                                                                    "% Colonies Affected by Diseases", "% Colonies Affected by Pesticides",
                                                                                    "% Colonies Affected by Other Factors", "% Colonies Affected by Unknown Factors")))
    } else if (input$overview_colonies_facet == "Crop Category") {
      return(selectInput("overview_colonies_fields", "Step 3: Choose a Region", c("1", "2", "3", "4", "5", "6 & 7")))
    } else if (input$overview_colonies_facet == "Region") {
      return(selectInput("overview_colonies_fields", "Step 3: Choose a Crop Category", c("Tree fruit", "Melons", "Berries",
                                                                                         "Vegetables", "Other",
                                                                                         "Tree nuts", "Other fruit")))
    }
  })
  
  output$overview_step4 <- renderUI({
    if (is.null(input$overview_colonies_facet)) {
      return(NULL)
    } else if (input$overview_colonies_facet %in% c("Bee Colony Numbers Fields", "Stressors Fields")) {
      return(selectInput("overview_colonies_time_period", "Step 4: Choose a Time Period", c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")))
    } else if (input$overview_colonies_facet == "Time Period") {
      state_names <- unique(honey$state)
      len_state_names <- length(state_names)
      state_names <- state_names[c(-(length(state_names)-1), -length(state_names))]
      return(selectInput("overview_colonies_states", "Step 4: Choose States to Highlight", state_names, multiple=TRUE, selected=NULL))
    } else if (input$overview_colonies_facet %in% c("Crop Category", "Region")) {
      return(selectInput("overview_colonies_category", "Step 4: Choose a Category", c("Number of Paid Pollinated Acres", "Pollination Price Per Acre (USD)", "Number of Colonies Used for Pollination",
                                                                                      "Pollination Price Per Colony (USD)", "Total Value of Pollination (1,000 USD)")))
    }
  })
  
  output$overview_step5 <- renderUI({
    if (is.null(input$overview_colonies_facet)) {
      return(NULL)
    } else if (input$overview_colonies_facet %in% c("Bee Colony Numbers Fields", "Stressors Fields")) {
      state_names <- unique(honey$state)
      state_names <- state_names[c(-(length(state_names)-1), -length(state_names))]
      return(selectInput("overview_colonies_state", "Step 5: Choose States to Highlight", state_names, multiple=TRUE, selected=NULL))
    } else if (input$overview_colonies_facet == "Time Period") {
      return(NULL)
    } else if (input$overview_colonies_facet %in% c("Crop Category", "Region")) {
      return(tags$div(class="header", style="font-size:70%", checked=NA, tags$b("Region 1"), ": Connecticut, Illinois, Indiana, Iowa, Kansas, Massachusetts, Maine,
                      Michigan, Nebraska, New Hampshire, New Jersey, New York, Ohio, Pennsylvania, Rhode Island, Vermont, Wisconsin", tags$br(),
                      tags$b("Region 2"), ": Alabama, Delaware, Georgia, Kentucky, Maryland, North Carolina, South Carolina, Tennessee, Virginia, West Virginia", tags$br(),
                      tags$b("Region 3"), ": Arkansas, Florida, Louisiana, Missouri, Mississippi, New Mexico, Oklahoma, Texas", tags$br(),
                      tags$b("Region 4"), ": Colorado, Minnesota, Montana, Nevada, North Dakota, South Dakota, Utah, Wyoming", tags$br(),
                      tags$b("Region 5"), ": Alaska, Idaho, Oregon, Washington", tags$br(),
                      tags$b("Region 6 & 7"), ": Arizona, California, Hawaii"))
    }
  })
  
  #### Bee Colony Number Plot ####
  output$overview1 <- renderPlot({
    if (is.null(input$overview_colonies_facet)) {
      return(NULL)
    } else if (input$overview_colonies_facet == "Bee Colony Numbers Fields") {
      df <- colonies
      names(df) <- c("State", "Number of Colonies", "Maximum Number of Colonies", "Number of Colonies Lost", "Percent of Colonies Lost", 
                     "Number of Colonies Added", "Number of Colonies Renovated", "Percent of Colonies Renovated", "Time Period", "Year")
      df <- melt(df, id.vars = c("State", "Time Period"), measure.vars = c("Maximum Number of Colonies", "Number of Colonies Lost", 
                                                                           "Percent of Colonies Lost", "Number of Colonies Added", 
                                                                           "Number of Colonies Renovated", "Percent of Colonies Renovated"))
      df["state_abb"] <- state.abb[match(df$State,state.name)]
      df <- df %>% drop_na
      
      if (length(input$overview_colonies_fields) == 0) {
        df_subset <- df[df[["Time Period"]]==input$overview_colonies_time_period,]
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_state))
        names(col_palette) <- input$overview_colonies_state
        colonies_facet_plots <- lapply(split(df_subset,df_subset$variable), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_state,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x$variable))
          ggplotGrob(p)
        })
        
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
        
      } else {
        df_subset <- df[(df[["Time Period"]]==input$overview_colonies_time_period) & (df$variable %in% input$overview_colonies_fields),]
        df_subset$variable <- factor(df_subset$variable)
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_state))
        names(col_palette) <- input$overview_colonies_state
        colonies_facet_plots <- lapply(split(df_subset,df_subset$variable), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_state,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x$variable))
          ggplotGrob(p)
        })
        
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
      }
      
    } else { # if facet is by time period
      if (is.null(input$overview_colonies_fields)) {
        return(NULL)
      } else {
        df <- colonies
        names(df) <- c("State", "Number of Colonies", "Maximum Number of Colonies", "Number of Colonies Lost", "Percent of Colonies Lost", 
                       "Number of Colonies Added", "Number of Colonies Renovated", "Percent of Colonies Renovated", "Time Period", "Year")
        df["state_abb"] <- state.abb[match(df$State,state.name)]
        df <- df %>% drop_na
        
        df_subset <- df[,c(input$overview_colonies_fields, "Time Period", "state_abb", "State")]
        df_subset[["Time Period"]] <- factor(df_subset[["Time Period"]], levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_states))
        names(col_palette) <- input$overview_colonies_states
        colonies_facet_plots <- lapply(split(df_subset,df_subset[["Time Period"]]), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x[[input$overview_colonies_fields]],decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes_string(x="state_abb", y=paste0('`', input$overview_colonies_fields, '`')), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_states,], aes_string(x="state_abb", y=paste0('`', input$overview_colonies_fields, '`'), fill="State"), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x[["Time Period"]]))
          ggplotGrob(p)
        })
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
      }
    }
  })
  
  #### Stressors to Bee Colonies Plot ####
  output$overview2 <- renderPlot({
    if (is.null(input$overview_colonies_facet)) {
      return(NULL)
    } else if (input$overview_colonies_facet == "Stressors Fields") {
      df <- stressors
      names(df) <- c("State", "% Colonies Affected by Varroa Mites", "% Colonies Affected by Other Pests/Parasites",
                     "% Colonies Affected by Diseases", "% Colonies Affected by Pesticides",
                     "% Colonies Affected by Other Factors", "% Colonies Affected by Unknown Factors",
                     "Time Period", "Year")
      df <- melt(df, id.vars = c("State", "Time Period"), measure.vars = c("% Colonies Affected by Varroa Mites", "% Colonies Affected by Other Pests/Parasites",
                                                                           "% Colonies Affected by Diseases", "% Colonies Affected by Pesticides",
                                                                           "% Colonies Affected by Other Factors", "% Colonies Affected by Unknown Factors"))
      df["state_abb"] <- state.abb[match(df$State,state.name)]
      df <- df %>% drop_na
      
      if (length(input$overview_colonies_fields) == 0) {
        df_subset <- df[df[["Time Period"]]==input$overview_colonies_time_period,]
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_state))
        names(col_palette) <- input$overview_colonies_state
        colonies_facet_plots <- lapply(split(df_subset,df_subset$variable), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_state,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x$variable))
          ggplotGrob(p)
        })
        
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
        
      } else {
        df_subset <- df[(df[["Time Period"]]==input$overview_colonies_time_period) & (df$variable %in% input$overview_colonies_fields),]
        df_subset$variable <- factor(df_subset$variable)
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_state))
        names(col_palette) <- input$overview_colonies_state
        colonies_facet_plots <- lapply(split(df_subset,df_subset$variable), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_state,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x$variable))
          ggplotGrob(p)
        })
        
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
      }
      
    } else { # if facet is by time period
      if (is.null(input$overview_colonies_fields)) {
        return(NULL)
      } else {
        df <- stressors
        names(df) <- c("State", "% Colonies Affected by Varroa Mites", "% Colonies Affected by Other Pests/Parasites",
                       "% Colonies Affected by Diseases", "% Colonies Affected by Pesticides",
                       "% Colonies Affected by Other Factors", "% Colonies Affected by Unknown Factors",
                       "Time Period", "Year")
        df["state_abb"] <- state.abb[match(df$State,state.name)]
        df <- df %>% drop_na
        
        df_subset <- df[,c(input$overview_colonies_fields, "Time Period", "state_abb", "State")]
        df_subset[["Time Period"]] <- factor(df_subset[["Time Period"]], levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_states))
        names(col_palette) <- input$overview_colonies_states
        colonies_facet_plots <- lapply(split(df_subset,df_subset[["Time Period"]]), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x[[input$overview_colonies_fields]],decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes_string(x="state_abb", y=paste0('`', input$overview_colonies_fields, '`')), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_states,], aes_string(x="state_abb", y=paste0('`', input$overview_colonies_fields, '`'), fill="State"), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x[["Time Period"]]))
          ggplotGrob(p)
        })
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
      }
    }
  })
  
  #### Pollination Plot ####
  output$overview3 <- renderDiagonalNetwork({
    
    df <- pollination[(pollination$year==2015),]
    names(df) <- c("Crop", "Number of Paid Pollinated Acres", "Pollination Price Per Acre (USD)", "Number of Colonies Used for Pollination",
                   "Pollination Price Per Colony (USD)", "Total Value of Pollination (1,000 USD)", "Crop Category", "Region", "Year")
    
    # function to transform data frame into list format needed for radialNetwork function
    maketreelist <- function(df, root = df[1, 1]) { # http://stackoverflow.com/questions/23839142/transform-a-dataframe-into-a-tree-structure-list-of-lists
      if(is.factor(root)) root <- as.character(root)
      r <- list(name = root)
      children = df[df[, 1] == root, 2]
      if(is.factor(children)) children <- as.character(children)
      if(length(children) > 0) {
        r$children <- lapply(children, maketreelist, df = df)
      }
      r
    }
    if ((is.null(input$overview_colonies_facet)) | (is.null(input$overview_colonies_fields) | (is.null(input$overview_colonies_category)))) {
      return(NULL)
    } else if (input$overview_colonies_facet=="Crop Category") {
      df_subset <- df[df[["Region"]]==input$overview_colonies_fields,]
      df_subset$crop_val <- paste(df_subset$Crop, df_subset[[input$overview_colonies_category]], sep=" - ")
      df_subset <- df_subset[order(-df_subset[[input$overview_colonies_category]]),]
      df_subset <- df_subset[,c("Crop Category", "crop_val")]
      temp <- lapply(split(df_subset, df_subset[["Crop Category"]]), maketreelist)
      temp <- list(name="Crop", children=temp)
      names(temp$children) <- NULL
      diagonalNetwork(temp, fontSize = 20, linkColour = "#d4b9da", nodeColour = "#88419d", nodeStroke = "#88419d", textColour = "#081d58")
      
    } else if (input$overview_colonies_facet=="Region") {
      df_subset <- df[df[["Crop Category"]]==input$overview_colonies_fields,]
      df_subset$crop_val <- paste(df_subset$Crop, df_subset[[input$overview_colonies_category]], sep=" - ")
      df_subset$Region <- paste("Region", df_subset$Region)
      df_subset <- df_subset[order(-df_subset[[input$overview_colonies_category]]),]
      df_subset <- df_subset[,c("Region", "crop_val")]
      temp <- lapply(split(df_subset, df_subset[["Region"]]), maketreelist)
      temp <- list(name="Crop", children=temp)
      names(temp$children) <- NULL
      diagonalNetwork(temp, fontSize = 20, linkColour = "#d4b9da", nodeColour = "#88419d", nodeStroke = "#88419d", textColour = "#081d58")
    }
  })
  
  #### Honey Production Plot ####
  output$overview4 <- renderPlot({
    df <- honey
    names(df) <- c("State", "# of Honey Producing Colonies (1,000 Colonies)", "Honey Yield per Colony (lbs)", "Honey Production (1,000 lbs)",
                   "Stocks Held by Honey Producers (1,000 lbs)", "Average Price Per Pound of Honey (cents)", 
                   "Value of Honey Production (1,000 USD)", "Year")
    df <- melt(df, id.vars = c("State"), measure.vars = c("# of Honey Producing Colonies (1,000 Colonies)", "Honey Yield per Colony (lbs)", "Honey Production (1,000 lbs)",
                                                          "Stocks Held by Honey Producers (1,000 lbs)", "Average Price Per Pound of Honey (cents)", 
                                                          "Value of Honey Production (1,000 USD)"))
    df["state_abb"] <- state.abb[match(df$State,state.name)]
    df <- df %>% drop_na
    
    if (length(input$overview_colonies_facet) == 0) {
      col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_states))
      names(col_palette) <- input$overview_colonies_states
      colonies_facet_plots <- lapply(split(df,df$variable), function(x){
        x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
        p <- ggplot(x) +
          geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
          geom_bar(data=x[x$State %in% input$overview_colonies_states,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
          scale_fill_manual(values=col_palette) + 
          theme_bw() + 
          theme(legend.position="none", axis.title = element_blank(),
                plot.title = element_text(hjust=0.5, face="bold"),
                panel.grid = element_blank()) + 
          scale_y_continuous(labels = comma) + 
          ggtitle(unique(x$variable))
        ggplotGrob(p)
      })
      
      grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
      
    } else {
      df_subset <- df[(df$variable %in% input$overview_colonies_facet),]
      df_subset$variable <- factor(df_subset$variable)
      col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_states))
      names(col_palette) <- input$overview_colonies_states
      colonies_facet_plots <- lapply(split(df_subset,df_subset$variable), function(x){
        x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
        p <- ggplot(x) +
          geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
          geom_bar(data=x[x$State %in% input$overview_colonies_states,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
          scale_fill_manual(values=col_palette) + 
          theme_bw() + 
          theme(legend.position="none", axis.title = element_blank(),
                plot.title = element_text(hjust=0.5, face="bold"),
                panel.grid = element_blank()) + 
          scale_y_continuous(labels = comma) + 
          ggtitle(unique(x$variable))
        ggplotGrob(p)
      })
      
      grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
    }
  })
  
  #############################
  #### Map with Bar Charts ####
  #############################
  
  map_choropleth <- reactive({
    switch(input$map_choropleth, 
           "max_colonies" = "Maximum Number\nof Colonies", 
           "lost_colonies" = "Number of\nColonies Lost",
           "percent_lost_colonies" = "Percentage of\nColonies Lost (%)",
           "added_colonies" = "Number of\nColonies Added",
           "renovated_colonies" = "Number of\nRenovated Colonies",
           "percent_renovated_colonies" = "Percentage of\nColonies Renovated (%)",
           "varroa_mites" = "Percentage of\nColonies Affected\nby Varroa Mites (%)",
           "other_pests_parasites" = "Percentage of Colonies\nAffected by Other\nPests or Parasites (%)",
           "diseases" = "Percentage of Colonies\nAffected by Disease (%)",
           "pesticides" = "Percentage of Colonies\nAffected by Pesticides (%)",
           "other" = "Percentage of\nColonies Affected\nby Other Factors (%)",
           "unknown" = "Percentage of\nColonies Affected by\nUnknown Factors (%)")
  })
  
  colonies_stressors_subset <- reactive({
    temp <- colonies_stressors %>% filter(time_period == input$map_time_period)
    temp$hover <- with(temp, paste(state))
    names(temp)[names(temp)==input$map_choropleth] <- 'color_col'
    temp["state_abb"] <- state.abb[match(temp$state,state.name)]
    temp <- temp %>% drop_na
    temp["id"] <- 0:(nrow(temp)-1)
    temp
  })
  
  output$map_bar <- renderPlotly({
    
    
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    plot_geo(colonies_stressors_subset(), locationmode = 'USA-states') %>%
      add_trace(z=~color_col, text=~hover, locations=~state_abb, color=~color_col, colors='Reds') %>%
      colorbar(title="") %>%
      layout(geo=g)
  })
  
  output$map_click_plot <- renderPlot({
    map_click_vals <- event_data("plotly_click")
    map_click_vals$state <- colonies_stressors_subset()[colonies_stressors_subset()$id == map_click_vals[["pointNumber"]],"state"]
    
    if (is.null(map_click_vals[["pointNumber"]])) {
      return(NULL)
    } else {
      df1 <- colonies_stressors[colonies_stressors$state == map_click_vals$state, ] %>%
        select_("time_period", "state", input$map_choropleth) %>% drop_na
      p1 <- ggplot(df1) +
        geom_bar(aes_string("time_period", input$map_choropleth), stat="identity", fill="#969696", color="#969696") +
        geom_text(aes_string("time_period", input$map_choropleth, label=input$map_choropleth), vjust=-.25) +
        theme(axis.title.x = element_blank(), panel.background = element_blank(),
              panel.grid = element_blank(), axis.line = element_line(colour = "black"),
              plot.title=element_text(hjust = 0.5, face="bold", size=18)) +
        ylab(map_choropleth()) +
        ggtitle(unique(df1$state)[1]) +
        scale_y_continuous(labels = comma, expand = c(0.2,0))
      
      df2 <- colonies_stressors[(colonies_stressors$state == map_click_vals$state) & (colonies_stressors$time_period == input$map_time_period), ] %>% drop_na
      colnames(df2) <- c("state", "time_period", "year", "num_colonies", "Max #\nColonies", "# Colonies\nLost",
                         "percent_lost_colonies", "# Colonies\nAdded", "# Colonies\nRenovated", "percent_renovated_colonies",
                         "Varroa Mites", "Other Pests/\nParasites", "Diseases", "Pesticides", "Other", "Unknown")
      p2 <- ggplot(melt(df2, measure.vars = c("Max #\nColonies", "# Colonies\nLost", "# Colonies\nAdded", "# Colonies\nRenovated"))) +
        geom_bar(aes_string("variable", "value"), stat="identity", fill="#969696", color="#969696") +
        geom_text(aes_string("variable", "value", label="value"), vjust=-.25) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank(), panel.background = element_blank(),
              axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5),
              panel.grid = element_blank(), axis.line = element_line(colour = "black")) +
        ggtitle("Changes in Number of Colonies") +
        scale_y_continuous(labels = comma, expand = c(0.28,0))
      p3 <- ggplot(melt(df2, measure.vars = c("Varroa Mites", "Other Pests/\nParasites", "Diseases", "Pesticides", "Other", "Unknown"))) +
        geom_bar(aes_string("variable", "value"), stat="identity", fill="#969696", color="#969696") +
        geom_text(aes_string("variable", "value", label="value"), vjust=-.25) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.background = element_blank(), panel.grid = element_blank(),
              axis.line = element_line(colour = "black"),
              plot.title = element_text(hjust = 0.5), axis.title.x = element_blank()) +
        ggtitle("Percent of Colonies Affected by Indicated Stressors") + ylab("(%)") +
        scale_y_continuous(labels = comma, expand = c(0.28,0))
      
      grid.arrange(do.call(rbind, list(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3))), ncol=1)
    }
  })
  
  output$map_bar_plot <- renderPlot({
    map_click_vals <- event_data("plotly_click")
    map_click_vals$state <- colonies_stressors_subset()[colonies_stressors_subset()$id == map_click_vals[["pointNumber"]],"state"]
    
    df1 <- colonies_stressors[colonies_stressors$time_period == input$map_time_period,]
    df1 <- df1[order(-df1[[input$map_choropleth]]),]
    df2 <- colonies_stressors[(colonies_stressors$state == map_click_vals$state) & (colonies_stressors$time_period == input$map_time_period), ] %>% drop_na
    
    ggplot(df1) +
      geom_bar(aes_string("state", input$map_choropleth), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
      scale_x_discrete(limits = df1$state) +
      geom_bar(data=df2, aes_string("state", input$map_choropleth), stat="identity", fill="#990000", color="#990000") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank(), panel.background = element_blank(),
            panel.grid = element_blank(), axis.line = element_line(colour = "black")) +
      ylab(map_choropleth()) +
      scale_y_continuous(labels = comma)
  })
  
  ##############################
  #### Map Pollination Plot ####
  ##############################
  
  select_poll_data <- reactive({
    map_regions %>%
      filter(region == input$color)
  })
  
  output$States <- renderPlot({
    theme1 <- theme(
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.margin = unit(c(0,0,0,0), "mm")
    )
    
    ggplot() +
      geom_polygon(data = map_regions, aes(long, lat, group = group, fill = region, alpha = 0.65)) +
      borders("state", colour = "grey80") +
      geom_polygon(data = select_poll_data(), aes(long, lat, group = group, fill = region, alpha = 0.75)) +
      coord_map("albers", at0 = 45.5, lat1 = 29.5) +
      scale_fill_manual(values = values) +
      theme1
  }, height = 300, width = 500)
  
  
  totals_data <- reactive({
    totals %>%
      filter(year == input$year)
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(map_regions, hover, threshold = 20, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    totals_point <- totals_data()[totals_data()$region == point$region,]
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.65); ",
                    "left:", left_px + 2 , "px; top:", top_px + 2, "px; padding: 2px 2px 0px 2px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Region: </b>", totals_point$region, "<br/>",
                    "<b> Total Pollinated Acres: </b>", format(totals_point$total_acr, big.mark=",", trim=TRUE), "<br/>",
                    "<b> Total Colonies Used: </b>", format(totals_point$total_col, big.mark=",", trim=TRUE), "<br/>",
                    "<b> Total Spent on Pollination (1K USD): </b>", format(totals_point$total_val, big.mark=",", trim=TRUE), "<br/>")))
    )
  })
  
  year_poll_data <- reactive({
    region_cat_total %>%
      filter(region == input$color) %>%
      filter(year == input$year)
  })
  
  year_all_data <- reactive({
    crop_cat_total %>%
      filter(year == input$year)
  })
  
  output$Crops1 <- renderPlot({
    theme2 <- theme(
      axis.text.x = element_text(angle = 60, hjust = 1.1, vjust = 1.2),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white")
    )
    
    ggplot() +
      geom_bar(data = year_all_data(),
               aes(x= reorder(crop_category, -total_acr), y= total_acr), 
               stat = "identity", fill = "grey88", alpha = 0.35, position = "dodge") +
      geom_bar(data = year_poll_data(),
               aes(x= reorder(crop_category, -total_acr), y= total_acr),
               stat = "identity", fill = values[[input$color]], alpha = 0.75, position = "dodge") +
      labs(x= "Crop Type", y= "Bee Pollinated Acres\n") +
      scale_y_continuous(labels = comma) +
      theme2
  }, height = 300)
  
  
  output$Crops2 <- renderPlot({
    theme2 <- theme(
      axis.text.x = element_text(angle = 60, hjust = 1.1, vjust = 1.2),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white")
    )
    ggplot() +
      geom_bar(data = year_all_data(),
               aes(x= reorder(crop_category, -total_val), y= total_val), 
               stat = "identity", fill = "grey88", alpha = 0.35, position = "dodge") +
      geom_bar(data = year_poll_data(),
               aes(x= reorder(crop_category, -total_val), y= total_val), 
               stat = "identity", fill = values[[input$color]], alpha = 0.75, position = "dodge") +
      labs(x= "Crop Type", y= "Bee Pollination Value (1000 USD)\n") +
      scale_y_continuous(labels = comma) +
      theme2
  }, height = 300)
  
  ########################
  #### Sankey Diagram ####
  ########################
  
  modifData <- reactive({
    poll_data_S <- poll_data_S[,c("region", "crop", input$sankey_variable)]
    poll_data_S$region <- as.character(poll_data_S$region)
    poll_data_S$crop <- as.character(poll_data_S$crop)
    colnames(poll_data_S) <- c("region", "crop", "variable")
    return(poll_data_S)
  })
  
  graf_df <- reactive({
    graph.data.frame(modifData(), directed = FALSE)
  })
  
  wc <- reactive({
    cluster_walktrap(graf_df())
  })
  
  member <- reactive({
    membership(wc())
  })
  
  df_sankey <- reactive({
    df_sankey <- igraph_to_networkD3(graf_df(), group = member())
    df_sankey$nodes$group <- as.character(c)
    df_sankey$links$group <- as.character(df_sankey$links$source)
    df_sankey$links$other_group <- as.character(d)
    return(df_sankey)
  })
  
  output$sankey <- renderSankeyNetwork(
    if(input$check == FALSE){
      sankeyNetwork(Links = df_sankey()$links, Nodes = df_sankey()$nodes, Source = "source",
                    Target = "target", Value = "value", NodeID = "name", fontSize = 12,
                    NodeGroup = "group", units = input$sankey_variable, LinkGroup = "other_group",
                    nodeWidth = 20,
                    colourScale = 'd3.scaleOrdinal().domain(["0", "1", "2", "3",
                    "4", "5", "6", "22"]).range(["#ff9900","#3366cc", "#FF1493",
                    "#228B22", "#B22222", "#40E0D0", "#E0E0E0", "#CCCCCC"]);')
    }else{
      sankeyNetwork(Links = df_sankey()$links, Nodes = df_sankey()$nodes, Source = "source",
                    Target = "target", Value = "value", NodeID = "name", fontSize = 12,
                    NodeGroup = "group", units = input$sankey_variable, LinkGroup = "group",
                    nodeWidth = 20,
                    colourScale = 'd3.scaleOrdinal().domain(["0", "1", "2", "3",
                    "4", "5", "6", "22"]).range(["#ff9900","#3366cc", "#FF1493",
                    "#228B22", "#B22222", "#40E0D0", "#E0E0E0", "#CCCCCC"]);')
    }
  )
}


shinyApp(ui = ui, server = server)
