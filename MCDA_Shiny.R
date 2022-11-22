library(shiny)
library(tidyverse)
#library(rdrop2)
library(bslib)
library(shinythemes)
library(googlesheets4)
library(DT)
library(knitr)

source("chooser.R")

outputDir <- "responses"

gs4_auth(cache = ".secrets", email="lauranaslund@gmail.com")
ss <- gs4_get("https://docs.google.com/spreadsheets/d/1ypCu5EgUhXXPOYEH1I4HhoEebZmzvJutmVhn8nAXMCU/edit?usp=sharing")

obj <- read.csv("example_objectives_3.csv") 
wos <- read.csv("WOS_Citations.csv") %>% pivot_longer(cols = c("X1a":"X5f"), names_to = "variable", values_to = "value") %>% mutate(Obj_Merg = str_sub(variable, 2, 3)) %>% left_join(obj %>% mutate(Obj_Merg = str_sub(Objective, 1, 2)) %>% select(Objective, Obj_Merg), by = "Obj_Merg")

wos$parenthetical <- NA
wos$link <- NA
wos$html <- NA
for(i in 1:nrow(wos)){
  if(wos$Search.String[i] == "tool"){
    wos$parenthetical[i] <- wos$Article.Title[i]
    wos$html[i] <- paste0("<a href = '", wos$DOI[i], "' target = ", "'_blank'", ">", wos$parenthetical[i], "</a>")
  }
  else{
    if(str_count(wos$Authors[i], ";")>0) {
      wos$parenthetical[i] <- paste0("(", str_split(wos$Authors[i], ",")[[1]][1], " et al. ", wos$Publication.Year[i], ")")
    }
    else{
      wos$parenthetical[i] <- paste0("(", str_split(wos$Authors[i], ",")[[1]][1], " ", wos$Publication.Year[i], ")")
    }
    if(str_sub(wos$DOI[i], 1, 1) != "h"){
      wos$link[i] <- paste0("https://doi.org/", wos$DOI[i])
    }
    else{
      wos$link[i] <- wos$DOI[i]
    }
    wos$html[i] <- paste0("<a href = '", wos$link[i], "' target = ", "'_blank'", ">", wos$parenthetical[i], "</a>")
  }
}

shinyInput <- function(FUN, n, id, ...) {
  vapply(n, function(i){
    as.character(FUN(paste0(id, i), ...))
  }, character(1))
  
}

ui <- navbarPage(
  id = "nbPage",
  theme = bs_theme(
    version = 5, 
    bg = "#FFFFFF",
    fg = "#1E1E1E",
    "navbar-bg" = "#1d82b3",
    primary = "#57925b",
    secondary = "#515459",
    base_font = font_google("Merriweather Sans"),
    heading_font = font_google("Oswald")
  ),
  
  title = "Dam Metrics Selector",
  
  tabPanel("Guidance",
           fluidPage(
    includeHTML("Guidance_Tab.html")
    )
  ),
  
  tabPanel("Objectives",
           value = "objectives",
           fluidPage(
             fluidRow(column(3, h1("Objectives Categories")), 
                      column(6, h1("Objectives Selector")), 
                      column(3, actionButton("jumpToMetrics", "View Metrics and Methods", style="
                        background-color: #1d82b3"))),
             
             fluidRow(
               sidebarLayout(
                 sidebarPanel(width = 3, 
                   fluidRow(actionButton("obj1", "1) Account for monetary costs and feasibility", style="
                        text-align:center;
                        height:80px;")),
                   tags$br(),
                   fluidRow(actionButton("obj2", "2) Meet demands for infrastructure services", style="
                        text-align:center;
                        height:80px;")),
                   tags$br(),
                   fluidRow(actionButton("obj3", "3) Reduce safety hazard", style="
                        text-align:center;
                        height:80px;")), 
                   tags$br(),
                   fluidRow(actionButton("obj4", "4) Meet community desires for recreation, historic preservation, and sense of place", style="
                        text-align:center;
                        height:80px;")),
                   tags$br(),
                   fluidRow(actionButton("obj5", "5) Maintain and restore the physical, chemical, and biological integrity of the nation’s waters", style="
                        text-align:center;
                        height:100px;"))
                   
                 ), 
                 mainPanel(width = 9,
                   
                   chooserInput(
                     "mychooser",
                     "Available frobs",
                     "Selected frobs",
                     sort(unique(obj$Objective)),
                     c(),
                     size = 18,
                     multiple = TRUE
                   ), 
                   br(),
                   p(em("Multiple objectives may be selected by holding down Ctrl or Shift.", style = "font-size:14px;"))
                )
             )
             )))
             ,

  tabPanel("Metrics",
           value = "metrics",
           fluidPage(
             DTOutput("data"),
             downloadButton('downloadData', "Download")
                     
                     )),
  tabPanel("Tools",
           value = "tools",
           fluidPage(
             DTOutput("tool")
           )),

  tabPanel(
    "Feedback",
    fluidPage(
      fluidRow(
        column(6, includeHTML("Feedback_Tab.html"))
      ),
      
      fluidRow(
      column(12, textAreaInput("feedback", "", width = "400px", height = "400px"))
    ), 
    fluidRow(
      column(2, actionButton("submit", "Submit")), 
      column(3, textOutput("msg")))
    ))
)

server <- function(input, output, session) {
  
  dataInput <- reactive(
    {
      tibble(
        obj %>%
          filter(Objective %in% input$mychooser$right) %>%
          bind_cols(tibble("More Information" = shinyInput(
            FUN = actionButton,
            n = obj%>% filter(Objective %in% input$mychooser$right) %>% pull(uniqueID),
            id = 'button_',
            label = NULL,
            icon = icon("info-sign", lib = "glyphicon"),
            onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
          ))) 
        %>% dplyr::select(-uniqueID))
    }
  )
  
  output$data <- DT::renderDT({
    dataInput()
  },
  
  escape = FALSE,
  selection = 'none'
  )
  
  toolsInput <- reactive(
    { tibble( wos %>% filter(Objective %in% input$mychooser$right & value == 1) %>% select(Objective, html) %>% rename("Link" = "html") %>% distinct() %>% arrange(Objective))
      
    }
  )
  
  output$tool <- DT::renderDT({
    toolsInput()
  },
  
  escape = FALSE,
  selection = 'none'
  ) 
  
  observeEvent(input$jumpToMetrics, {
    updateNavbarPage(session, "nbPage", selected = "metrics")
  })
  
  # could make this fancier where it knits the html fragments too and outputs a collated file
  output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(dataInput() %>% select(-"More Information"), con, row.names = F)
      }
  )
  

  observeEvent(input$select_button, {
    showModal(modalDialog(
      title = "",
      includeHTML(paste0("table/frag_", str_extract(input$select_button, "\\d+"), ".html")),
      easyClose = TRUE
    ))
  })
  
  
  observeEvent(input$submit, 
               { temp_df <- data.frame("Timestamp" = as.integer(Sys.time()), "Feedback" = input$feedback)
               ss %>% sheet_append(temp_df)
                 output$msg <- renderText("Thank you for your feedback")}
               
               )
  observeEvent(input$obj1, {
    showModal(modalDialog(
      title = "1) Account for monetary costs and feasibility",
      includeHTML("Objective1.html"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$obj2, {
    showModal(modalDialog(
      title = "2) Meet demands for infrastructure services",
      includeHTML("Objective2.html"),
      easyClose = TRUE
    ))
  })

  observeEvent(input$obj3, {
    showModal(modalDialog(
      title = "3) Reduce safety hazard",
      includeHTML("Objective3.html"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$obj4, {
    showModal(modalDialog(
      title = "4) Meet community desires for recreation, historic preservation, and sense of place",
      includeHTML("Objective4.html"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$obj5, {
    showModal(modalDialog(
      title = "5) Maintain and restore the physical, chemical, and biological integrity of the nation’s waters",
      includeHTML("Objective5.html"),
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)
