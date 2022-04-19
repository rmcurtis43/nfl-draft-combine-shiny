library(tidyverse)
library(combineR)
library(janitor)
library(cfbfastR)
library(shiny)
library(shinyjs)
library(shinyBS)
library(bs4Dash)
library(highcharter)
library(shinycssloaders)
library(shinyWidgets)
library(fresh)
library(waiter)
library(scales)
library(gtExtras)
library(gt)
library(ggimage)
library(gghighlight)
library(hrbrthemes)
library(DT)


##################################################################################################################################
##################################################################################################################################
############################################################ SHINY ###############################################################
##################################################################################################################################
##################################################################################################################################
#### load data and functions
source("data/combine_app_data.R")
source("functions.R")



ui <- dashboardPage(
  
 #preloader =  waiter_show_on_load(html = spin_1(), list(color = "black", logo = "https://github.com/rmcurtis43/combineR/raw/main/man/images/combineRsticker.png")),
  preloader = list(html = tagList(spin_5(), "Loading NFL Combine App ..."), list(color = "#000000",  image = 'https://ryan-curtis.netlify.app/author/dr.-ryan-curtis/avatar_hud36951747e40dfa69b4606221f685db5_538473_270x270_fill_q90_lanczos_center.jpg')),
  dashboardHeader(title =  
                    dashboardBrand(
                      title = "NFL Combine App",
                      color = "danger",
                      href = "https://www.nfl.com/combine/",
                      image = "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/National_Football_League_logo.svg/1200px-National_Football_League_logo.svg.png"
                    ), 
                  rightUi =  userOutput("user"),
                  controlbarIcon = icon("book-reader")
                  ),
  dashboardSidebar(
    tags$style(HTML("
      .main-sidebar{
        width: 250px;
      }
    ")),
    #width = 400,
    inputId = "sidebar",
    disable = FALSE,
    skin = 'light',
    status = 'danger',
    sidebarMenu(
      menuItem(
        text = "Player Performance",
        tabName = "player_dash_tab",
        icon = shiny::icon("user-check"),
        startExpanded = TRUE,
        menuSubItem(icon = icon("user-plus"), tabName = "player_dash_tab",
                    actionButton(inputId = "player_dash_player_button", "Select Player" ),
        )
      ),
      menuItem(
        text = "Combine Data Table",
        tabName = "datatable_tab",
        icon = shiny::icon("table")
      ),
      menuItem(
        text = "Benchmark Scores",
        tabName = "benchmark_tab",
        icon = shiny::icon("table")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    use_waiter(),
    setBackgroundImage(
      src = "https://cdn.pixabay.com/photo/2017/08/10/01/38/grass-2616911_1280.jpg",
      shinydashboard = TRUE
    ),
    tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);"),
    tabItems(
      tabItem(
        tabName = "player_dash_tab", 
        fluidRow(
                 column(6,  withSpinner(uiOutput("player_dash_profile_card"), color = "#000000")),
                 column(6,  box(width = 12, height = 635, title = 'Combine Performance',
                                withSpinner(uiOutput("player_dash_main_output"), color = "#000000"),
                                div(uiOutput('player_dash_stat_filter'), style = "text-align: center;")) 
        )
      ),
      fluidRow(
        bsModal("player_dash_player_bs", "", "player_dash_player_button", size = "large", 
                  HTML('<img src="https://cdn4.iconfinder.com/data/icons/users-68/64/Artboard_17-512.png" style="width:100px;height:100px;display: block; margin-left: auto; margin-right: auto; color: #83F3FF">'), 
                h4("SELECT PLAYER", style="text-align: center;"),
                div(withSpinner(uiOutput("player_dash_player_filter"), color = "#000000"), style = "margin: auto; width: 40%;"))
        
                )
      
  
    ),
      tabItem(
        tabName = "benchmark_tab",
        fluidRow(
          box(width = 12, title = 'Benchmark Tables',
              dropdownButton(
                
                tags$h3("Select Test and Positions"),
                
                selectInput(inputId = 'benchmark_tbl_test',
                            label = 'Select Test',
                            choices = c("Height", "Weight", "Vertical Jump", "Broad Jump", "Bench", "3cone", "Shuttle", "40yd"),
                            selected = "40yd"),
                
                selectInput(inputId = 'benchmark_tbl_position',
                            label = 'Select Positions',
                            multiple = TRUE,
                            choices = c('DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR', 'PK', 'LS'),
                            selected = c('DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR')),
                
                circle = TRUE, status = "danger",
                icon = icon("filter"), width = "300px",
                label = "",
                
                tooltip = tooltipOptions(title = "Click to select benchmark test and positions")
              ),
              withSpinner(uiOutput('benchmark_tbl_output'), color = "#000000")
          )
         
        )
      ),
    tabItem(
      tabName = "datatable_tab",
      fluidRow(
        box(width = 12, title = "Combine Data",
            div(style = 'margin-top:-20px; margin-bottom:-5px;text-align: center;', 
                radioGroupButtons(
                  inputId = "datatable_stat",
                  label = "",
                  choices = c("Value", "Percentile","Z-score", "T-score", "STEN"),
                  status = "danger",
                  justified = TRUE,
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"))
                )
            ),
                   withSpinner(DT::dataTableOutput("datatable_DT"), color = "#000000"))
            )
      )
    ),
    bsModal("tsa_chapter_bs", "Total Score of Athleticism - Turner et al. 2019", "tsa_chapter_button", size = "large", uiOutput("tsa_chapter_pdf")),
    bsModal("ams_chapter_bs", "Developing Athlete Monitoring Systems - Thornton 2018", "ams_chapter_button", size = "large", uiOutput("ams_chapter_pdf"))
  ),
  dashboardControlbar(
    width = 300,
    skin = "light",
    controlbarMenu(
      id = "control_menu",

      controlbarItem(
        id = "control_item_1",
        icon = icon("book-open-reader"),
        title = "Resources",
        active = TRUE,
        fluidPage(
          div(actionButton(inputId = "tsa_chapter_button", icon = icon("file-pdf"), "Total Score of Athleticism - Turner 2019" , width = '225px',style = "text-align:left;"),style = "text-align:left;"),
          tags$br(),
          div(actionButton(inputId = "ams_chapter_button", icon = icon("file-pdf"), "Developing Athlete Monitoring Systems - Thornton 2018" , width = '225px',style = "text-align:left;"),style = ""),
          tags$br()
        )
      )
    )
      )
    )



server <- function(input, output, session) {
  
  
  
  toggleModal(session, "player_dash_player_bs", toggle = "open")
  
  
  
  
  
  ####### user profile
  
  output$user <- renderUser({
    dashboardUser(
      name = "Ryan Curtis",
      image = "https://ryan-curtis.netlify.app/author/dr.-ryan-curtis/avatar_hud36951747e40dfa69b4606221f685db5_538473_270x270_fill_q90_lanczos_center.jpg",
      title = "NFL Combine App",
      subtitle = "Author",
      footer = p("@RyanM_Curtis", class = "text-center"),
      fluidRow(
        div(style = "text-align: center;",
        
            dashboardUserItem(
          width = 12,
          div(style = "text-align: center;",
          shinydashboardPlus::socialButton(
            href = "https://twitter.com/RyanM_Curtis",
            icon = icon("twitter", "fa-2x")
          ),
          shinydashboardPlus::socialButton(
            href = "https://github.com/rmcurtis43",
            icon = icon("github", "fa-2x")
          )
        ))
        )
      )
    )
  })
  
  
  ########################## Resources ######################################################3
  
  output$tsa_chapter_pdf <- renderUI({
    
    tags$iframe(style="height:500px; width:100%; scrolling=yes", 
                src= "total_score_of_athleticism_turner_2019.pdf")
  })
  

  output$ams_chapter_pdf <- renderUI({
    
    tags$iframe(style="height:500px; width:100%; scrolling=yes", 
                src= "developing_athlete_monitoring_systems_thornton_2018.pdf")
  })
  

  ######################### PDF Render
  
  observeEvent(input$pdf,{
    shinyjs::runjs(
      "html2canvas(document.getElementById('pdf_content')).then(canvas => {

    var imgData = canvas.toDataURL('image/png');

    var margin = 2;
      var imgWidth = 210;
      var pageHeight = 295;
      var imgHeight = canvas.height * imgWidth / canvas.width;
      var heightLeft = imgHeight;

      var doc = new jsPDF('p', 'mm'),
       margins = {
      top: 40,
      bottom: 60,
      left: 40,
      width: 522
    };
      var position = 10;
      var options = {
       top: '10px',
      right: '10px',
      left: '10px',
      bottom: '60px'
      }

      doc.addImage(imgData, 'PNG', 10, position, imgWidth*0.88, imgHeight*0.88);
      heightLeft -= pageHeight;

      while (heightLeft >= 0) {
        position = heightLeft - imgHeight;
        doc.addPage();
        doc.addImage(imgData, 'PNG', 10, position, imgWidth*0.88, imgHeight*0.88);
        heightLeft -= pageHeight;
      }
      doc.save( 'file.pdf');
      }
    );



    "
      
    )
  })
  
######  Player Dash filters
  

  
  output$player_dash_player_filter <- renderUI({
   
    data <- combine_clean %>%
      distinct(player_id, player, img_player, img_team) %>%
      arrange(player)
        pickerInput(
          "player_dash_player",
          "",
          choices = data$player_id,
          choicesOpt = list(content = paste(data$img_player, data$player, data$img_team)),
          options = list(
            `live-search` = TRUE),
          selected = c("Aaron Donald_Pittsburgh"),
          multiple = FALSE
        )
  })
  
  output$player_dash_season_filter <- renderUI({
    div(style = 'margin-top:-20px; margin-bottom:-15px;', 
        pickerInput(
          "player_dash_season",
          "",
          choices = 2022:2000,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = paste0("count > ", length(2022:2000) - 1),
            `count-selected-text` = "All",
            `live-search` = TRUE
          ),
          selected = 2022:2000,
          multiple = TRUE
        )
    )
  })
  
  
  
  output$player_dash_team_filter <- renderUI({
    
    data <- combine_clean %>%
      distinct(school, img_team) %>%
      arrange(school)
    div(style = 'margin-top:-20px; margin-bottom:-15px;', 
        pickerInput(
          "player_dash_team",
          "",
          choices = unique(data$school),
          choicesOpt = list(content = paste(data$img_team, data$school)),
          multiple = FALSE
        )
    )
  })
  
  
  output$player_dash_stat_filter <- renderUI({
    
    div(style = 'margin-top:-20px; margin-bottom:-15px;text-align: center;', 
      radioGroupButtons(
        inputId = "player_dash_stat",
        label = "",
        choices = c("Percentile","Z-score", "T-score", "STEN"),
        status = "danger",
        justified = TRUE,
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"))
      )
    )
  })
  

  output$player_dash_z_filter <- renderUI({
    
    div(style = 'margin-top:-20px; margin-bottom:-15px;text-align: center;', 
        radioGroupButtons(
          inputId = "player_dash_z",
          label = "",
          choices = c("Position","Designation", "Overall"),
          status = "danger",
          justified = TRUE,
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon"))
        )
    )
  })
  

  player_dash_data_reactive <- reactive({
    req(input$player_dash_player)
   dat <- combine_clean %>%
     ungroup() %>%
      filter(player_id %in% c(input$player_dash_player))
   
   return(dat)
  })
  ############## Player Dash Profile Card

  output$player_dash_profile_card <- renderUI({
    
    req(player_dash_data_reactive())
    
    userBox(width = 12, height = 550,
                 
      title = userDescription(
        title = NULL,
        image = player_dash_data_reactive()$player_image,
        backgroundImage = player_dash_data_reactive()$logo_light,
      ),
      status = "white",
      closable = TRUE,
      maximizable = TRUE,
      div(style = "text-align: center; margin-top: 15px; margin-bottom: -30px;",
          div(style = "display: inline-block; width: 400px; height: 200px; padding: 0px 0px;",
             div(paste0(player_dash_data_reactive()$player[1], " - ",player_dash_data_reactive()$school[1]), style=paste0("font-size: 32px; font-color:", player_dash_data_reactive()$team_color[1],"; font-weight: bold;text-shadow: 0 0 4px #959595;")),
             div(paste0(player_dash_data_reactive()$combine_year[1], " Draft Class"), style="font-size: 26px; font-color: black; text-shadow: 0 0 4px #959595;"),
             div(player_dash_data_reactive()$position[1], style="font-size: 26px; font-color: black;text-shadow: 0 0 4px #959595;") 
              
              ),  
          div(style = "display: block; margin: auto; width: 100%;  margin-top: -40px;",
      div(style = "display: block;  float: left; width: 250px; height: 200px; margin-top: 40px;",
        bs4InfoBox(
          title = "Total Size Score",
          value = paste0(round(player_dash_data_reactive()$tss_percentile_position, 0), "/100"),
          fill = TRUE,
          gradient = TRUE,
          color = ifelse(player_dash_data_reactive()$tss_percentile_position >= 75, "success", ifelse(player_dash_data_reactive()$tss_percentile_position >50, "gray", ifelse(player_dash_data_reactive()$tss_percentile_position <= 25, "danger", "warning"))),
          icon = icon("ruler-vertical"),
          width = 12
        ),
        bs4InfoBox(
          title = "Total Athleticism Score",
          value = paste0(round(player_dash_data_reactive()$tsa_percentile_position, 0), "/100"),
          fill = TRUE,
          gradient = TRUE,
          icon = icon("running"),
          width = 12,
          color = ifelse(player_dash_data_reactive()$tsa_percentile_position >= 75, "success", ifelse(player_dash_data_reactive()$tsa_percentile_position >50, "gray", ifelse(player_dash_data_reactive()$tsa_percentile_position <= 25, "danger", "warning"))),
        )),
      div(style = "display: block;  height: 400px; margin-top: -10px;",
          withSpinner(gt_output("player_dash_gt_tbl"), color = "#000000")
        )
          )
      )
      )
    
  })
  

  output$player_dash_percentile_chart <- renderHighchart({
    
    req(player_dash_data_reactive())
    
    hc_data_position <- player_dash_data_reactive() %>%
      select(player_image, position=position_cfb,designation,matches("percentile_position")) %>%
      rename_with(~str_remove(., '.percentile')) %>%
      pivot_longer(!c(player_image, position, designation), names_to = "Test", values_to = "Percentile Position") %>%
      mutate(Test = case_when(
        grepl("bench", Test) ~ "Bench",
        grepl("height", Test) ~ "Height",
        grepl("weight", Test)~ "Weight",
        grepl("vertical", Test) ~ "Vertical Jump",
        grepl("broad", Test) ~ "Broad Jump",
        grepl("shuttle", Test) ~ "Shuttle",
        grepl("x3cone", Test) ~ "3 Cone",
        grepl("x40yd", Test) ~ "40yd",
        TRUE ~ as.character(Test)
      )) 
    
    
    hc_data_all <- player_dash_data_reactive() %>%
      select(player_image, position=position_cfb,designation,matches("percentile_all")) %>%
      rename_with(~str_remove(., '.percentile')) %>%
      pivot_longer(!c(player_image,position, designation), names_to = "Test", values_to = "Percentile All") %>%
      mutate(Test = case_when(
        grepl("bench", Test) ~ "Bench",
        grepl("height", Test) ~ "Height",
        grepl("weight", Test)~ "Weight",
        grepl("vertical", Test) ~ "Vertical Jump",
        grepl("broad", Test) ~ "Broad Jump",
        grepl("shuttle", Test) ~ "Shuttle",
        grepl("x3cone", Test) ~ "3 Cone",
        grepl("x40yd", Test) ~ "40yd",
        TRUE ~ as.character(Test)
      )) 
    
    
    hc_data_designation <- player_dash_data_reactive() %>%
      select(player_image, position=position_cfb, designation, matches("percentile_designation")) %>%
      rename_with(~str_remove(., '.percentile')) %>%
      pivot_longer(!c(player_image,position, designation ), names_to = "Test", values_to = "Percentile Designation") %>%
      mutate(Test = case_when(
        grepl("bench", Test) ~ "Bench",
        grepl("height", Test) ~ "Height",
        grepl("weight", Test)~ "Weight",
        grepl("vertical", Test) ~ "Vertical Jump",
        grepl("broad", Test) ~ "Broad Jump",
        grepl("shuttle", Test) ~ "Shuttle",
        grepl("x3cone", Test) ~ "3 Cone",
        grepl("x40yd", Test) ~ "40yd",
        TRUE ~ as.character(Test)
      )) 
      
      
    hc_data <- hc_data_position %>%
      left_join(hc_data_designation, by = c("player_image", "position", "Test"))
      
        
    highchart() %>%
      hc_add_series(
        name = paste("Percentiles by Positon and Designation"),
        showInLegend = FALSE,
        hc_data,
        "scatter",
        hcaes(
          x = `Percentile Designation`,
          y = `Percentile Position`
        ),
        colorByPoint = TRUE,
        visible = TRUE,
        
        marker = list(
          symbol = 'circle',
          enabled = TRUE,
          enabledThreshold = 0,
          radius = 5,
          lineWidth = 0,
          fillColor = '#000000',
          lineColor = '#000000',
          zIndex = 1
        ),
        dataLabels = list(
          format = '{point.Test}',
          align = "center",
          enabled = TRUE,
          allowOverlap = TRUE
        ),
        zIndex = 1
      ) %>%
      hc_tooltip(
        headerFormat = "{point.Test}",
        padding = 17,
        pointFormat = "
      <img src ='{point.player_image}' width = '30px'> <br>
      Test: {point.Test} <br>
      Percentile (by Designation): {point.x} <br>
      Percentile (by Position): {point.y} 
        
        ",
        useHTML = TRUE
      ) %>%
      
      hc_plotOptions(
        scatter = list(borderWidth = 1,
                       allowPointSelect = TRUE,
                       opacity = 0.5,
                       zIndex = 1
        )
      ) %>%
      hc_add_series(
        name = paste0("Low Designation Percentile - High Position Percentile"),
        type = 'polygon',
        color = hex_to_rgba("#FDD844", 0.1),
        enableMouseTracking = TRUE,
        visible = TRUE,
        showInLegend = FALSE,
        zIndex = 3,
        tooltip = list(pointFormat = "Low Designation Percentile - High Position Percentile"),
        data = list(c(0, 65), c(35, 65), c(35, 100), c(0, 100))
      ) %>%
      hc_add_series(
        name = paste0("High Designation Percentile - Low Position Percentile"),
        type = 'polygon',
        color = hex_to_rgba("#FDD844", 0.1),
        enableMouseTracking = TRUE,
        visible = TRUE,
        showInLegend = FALSE,
        zIndex = 3,
        tooltip = list(pointFormat = "Low Designation Percentile - High Position Percentile"),
        data = list(c(65, 0), c(100, 0), c(100, 35), c(65, 35))
      ) %>%
      hc_add_series(
        name = paste0("Low"),
        type = 'polygon',
        color = hex_to_rgba("#D00000", 0.1),
        enableMouseTracking = TRUE,
        visible = TRUE,
        showInLegend = FALSE,
        zIndex = 3,
        tooltip = list(pointFormat = "Low Designation Percentile - Low Position Percentile"),
        data = list(c(0, 0), c(35, 0), c(35, 35), c(0, 35))
      ) %>%
      hc_add_series(
        name = paste0("Moderate"),
        type = 'polygon',
        color = hex_to_rgba("#E4E1D6", 0.2),
        enableMouseTracking = FALSE,
        visible = TRUE,
        showInLegend = FALSE,
        zIndex = 3,
        tooltip = list(pointFormat = " "),
        data = list(c(0, 35), c(100, 35), c(100, 65), c(0, 65))
      ) %>%
      hc_add_series(
        name = paste0("Moderate"),
        type = 'polygon',
        color = hex_to_rgba("#E4E1D6", 0.2),
        enableMouseTracking = FALSE,
        visible = TRUE,
        showInLegend = FALSE,
        zIndex = 4,
        tooltip = list(pointFormat = " "),
        data = list(c(35, 100), c(35, 0), c(65, 0), c(65, 100))
      ) %>%
      hc_add_series(
        name = paste0("High"),
        type = 'polygon',
        color =  hex_to_rgba("green", 0.1),
        enableMouseTracking = TRUE,
        visible = TRUE,
        showInLegend = FALSE,
        zIndex = 3,
        tooltip = list(pointFormat = "High Designation Percentile - High Position Percentile"),
        data = list(c(100, 100), c(65, 100), c(65, 65), c(100, 65))
      ) %>%
      hc_xAxis(
        
        title = list(text = paste0(
          paste0("Percentile by Designation (",player_dash_data_reactive()$designation, ")" )
        )),
        labels = list(enabled = FALSE),
        max=100,
        min=0,
        zIndex = 5,
        tickInterval = 0,
        gridLineWidth = 0,
        type = 'scatter',
        gridLineColor = 'transparent',
        lineColor = 'transparent',
        plotLines = list(list(
          value = 50,
          color = '#A8A8A8',
          width = 1
        ))
      ) %>%
      hc_yAxis(
        
        title = list(text = paste0(
          paste0("Percentile by Position (",player_dash_data_reactive()$position_cfb, ")" )
        )),
        labels = list(enabled = FALSE),
        max=100,
        min=0,
        zIndex = 6,
        tickInterval = 0,
        gridLineWidth = 0,
        type = 'scatter',
        gridLineColor = 'transparent',
        lineColor = 'transparent',
        plotLines = list(list(
          value = 50,
          color = '#A8A8A8',
          width = 1
        ))
      )
    
   
  })
  
  
  output$player_dash_z_chart <- renderHighchart({
    
    req(input$player_dash_z, player_dash_data_reactive())

    
    if(input$player_dash_z == "Position") {
      
      hc_data <- player_dash_data_reactive() %>%
        select(player_image, position=position_cfb,designation,weight_z_position, height_z_position, bench_z_position, vertical_z_position, broad_jump_z_position, shuttle_z_position, x3cone_z_position,x40yd_z_position) %>%
        rename_with(~str_remove(., '.percentile')) %>%
        pivot_longer(!c(player_image, position, designation), names_to = "Test", values_to = "Value") %>%
        mutate(Test = case_when(
          grepl("bench", Test) ~ "Bench",
          grepl("height", Test) ~ "Height",
          grepl("weight", Test)~ "Weight",
          grepl("vertical", Test) ~ "Vertical Jump",
          grepl("broad", Test) ~ "Broad Jump",
          grepl("shuttle", Test) ~ "Shuttle",
          grepl("x3cone", Test) ~ "3 Cone",
          grepl("x40yd", Test) ~ "40yd",
          TRUE ~ as.character(Test)
        )) %>%
        mutate(Value = round(Value, 2)) %>%
        arrange(desc(Value))
      
      hc_data %>%
        hchart(
          'bar', 
          hcaes(x = Test, y = Value,
                color = ifelse(is.na(Value), "#FFFFFF", ifelse(Value > 1.5, "#28a745", ifelse(Value > 0, "#adb5bd", ifelse(Value <= -1.5, "#dc3545", 
                                                                                                                           "#ffc107"))))),
          
          borderColor = "black",
          dataLabels = list(
            format = '{point.Value}',
            align = "center",
            enabled = TRUE,
            allowOverlap = TRUE
          )
        ) %>%
        hc_tooltip(
          headerFormat = "{point.Test}",
          padding = 17,
          pointFormat = "
      <img src ='{point.player_image}' width = '30px'> <br>
      Test: {point.Test} <br>
      z-score: {point.y} 
        
        ",
          useHTML = TRUE
        ) %>%
        hc_yAxis(
          
          title = list(text = paste0(
            paste0("Z-Score Relative to Position (", player_dash_data_reactive()$position, ")")
          )),
          labels = list(enabled = TRUE),
          max=3,
          min=-3,
          #zIndex = 5,
          tickInterval = 1,
          gridLineWidth = 1,
          #type = 'bar',
          #gridLineColor = 'transparent',
          lineColor = 'transparent',
          plotLines = list(list(
            value = 0,
            color = '#FFFFFF',
            width = 2
          ))
        ) 
      
    } else if (input$player_dash_z == "Designation") {
      
      hc_data <- player_dash_data_reactive() %>%
        select(player_image, position=position_cfb,designation,weight_z_designation, height_z_designation, bench_z_designation, vertical_z_designation, broad_jump_z_designation, shuttle_z_designation, x3cone_z_designation, x40yd_z_designation) %>%
        rename_with(~str_remove(., '.percentile')) %>%
        pivot_longer(!c(player_image, position, designation), names_to = "Test", values_to = "Value") %>%
        mutate(Test = case_when(
          grepl("bench", Test) ~ "Bench",
          grepl("height", Test) ~ "Height",
          grepl("weight", Test)~ "Weight",
          grepl("vertical", Test) ~ "Vertical Jump",
          grepl("broad", Test) ~ "Broad Jump",
          grepl("shuttle", Test) ~ "Shuttle",
          grepl("x3cone", Test) ~ "3 Cone",
          grepl("x40yd", Test) ~ "40yd",
          TRUE ~ as.character(Test)
        )) %>%
        mutate(Value = round(Value, 2)) %>%
        arrange(desc(Value)) 
      
      hc_data %>%
        hchart(
          'bar', 
          hcaes(x = Test, y = Value,
                color = ifelse(is.na(Value), "#FFFFFF", ifelse(Value > 1.5, "#28a745", ifelse(Value > 0, "#adb5bd", ifelse(Value <= -1.5, "#dc3545", 
                                                                                                                           "#ffc107"))))),
          
          borderColor = "black",
          dataLabels = list(
            format = '{point.Value}',
            align = "center",
            enabled = TRUE,
            allowOverlap = TRUE
          )
        ) %>%
        hc_tooltip(
          headerFormat = "{point.Test}",
          padding = 17,
          pointFormat = "
      <img src ='{point.player_image}' width = '30px'> <br>
      Test: {point.Test} <br>
      z-score: {point.y} 
        
        ",
          useHTML = TRUE
        ) %>%
        hc_yAxis(
          
          title = list(text = paste0(
            paste0("Z-Score Relative to Designation (", player_dash_data_reactive()$designation, ")")
          )),
          labels = list(enabled = TRUE),
          max=3,
          min=-3,
          tickInterval = 1,
          gridLineWidth = 1,
          lineColor = 'transparent',
          plotLines = list(list(
            value = 0,
            color = '#FFFFFF',
            width = 2
          ))
        ) 
      
    } else if (input$player_dash_z == "Overall") {
      
      hc_data <- player_dash_data_reactive() %>%
        select(player_image, position=position_cfb,designation,weight_z_all, height_z_all, bench_z_all, vertical_z_all, broad_jump_z_all, shuttle_z_all, x3cone_z_all, x40yd_z_all) %>%
        rename_with(~str_remove(., '.percentile')) %>%
        pivot_longer(!c(player_image, position, designation), names_to = "Test", values_to = "Value") %>%
        mutate(Test = case_when(
          grepl("bench", Test) ~ "Bench",
          grepl("height", Test) ~ "Height",
          grepl("weight", Test)~ "Weight",
          grepl("vertical", Test) ~ "Vertical Jump",
          grepl("broad", Test) ~ "Broad Jump",
          grepl("shuttle", Test) ~ "Shuttle",
          grepl("x3cone", Test) ~ "3 Cone",
          grepl("x40yd", Test) ~ "40yd",
          TRUE ~ as.character(Test)
        )) %>%
        mutate(Value = round(Value, 2)) %>%
        arrange(desc(Value))
      
      hc_data %>%
        hchart(
          'bar', 
          hcaes(x = Test, y = Value,
                color = ifelse(is.na(Value), "#FFFFFF", ifelse(Value > 1.5, "#28a745", ifelse(Value > 0, "#adb5bd", ifelse(Value <= -1.5, "#dc3545", 
                                                                                                                           "#ffc107"))))),
          
          borderColor = "black",
          dataLabels = list(
            format = '{point.Value}',
            align = "center",
            enabled = TRUE,
            allowOverlap = TRUE
          )
        ) %>%
        hc_tooltip(
          headerFormat = "{point.Test}",
          padding = 17,
          pointFormat = "
      <img src ='{point.player_image}' width = '30px'> <br>
      Test: {point.Test} <br>
      z-score: {point.y} 
        ",
          useHTML = TRUE
        ) %>%
        hc_yAxis(
          
          title = list(text = paste0(
            paste0("Z-Score Relative to All Players")
          )),
          labels = list(enabled = TRUE),
          max=3,
          min=-3,
          tickInterval = 1,
          gridLineWidth = 1,
          lineColor = 'transparent',
          plotLines = list(list(
            value = 0,
            color = '#FFFFFF',
            width = 2
          ))
        ) 
      
      
    }

 
    
  })
  
  
  output$player_dash_t_chart <- renderHighchart({
    
    req(player_dash_data_reactive())
    
    hc_data_position <- player_dash_data_reactive() %>%
      select(player_image, position=position_cfb,designation,weight_t_position, height_t_position, bench_t_position, vertical_t_position, broad_jump_t_position, shuttle_t_position, x3cone_t_position,x40yd_t_position) %>%
      rename_with(~str_remove(., '.percentile')) %>%
      pivot_longer(!c(player_image, position, designation), names_to = "Test", values_to = "Value") %>%
      mutate(Test = case_when(
        grepl("bench", Test) ~ "Bench",
        grepl("height", Test) ~ "Height",
        grepl("weight", Test)~ "Weight",
        grepl("vertical", Test) ~ "Vertical Jump",
        grepl("broad", Test) ~ "Broad Jump",
        grepl("shuttle", Test) ~ "Shuttle",
        grepl("x3cone", Test) ~ "3 Cone",
        grepl("x40yd", Test) ~ "40yd",
        TRUE ~ as.character(Test)
      )) %>%
      mutate(Value = round(Value, 2)) %>%
      mutate(`Test Type` = ifelse(Test %in% c("Weight", "Height"), "Size", "Athleticism"))
    
    hc_data_designation <- player_dash_data_reactive() %>%
      select(player_image, position=position_cfb,designation,weight_t_designation, height_t_designation, bench_t_designation, vertical_t_designation, broad_jump_t_designation, shuttle_t_designation, x3cone_t_designation,x40yd_t_designation) %>%
      rename_with(~str_remove(., '.percentile')) %>%
      pivot_longer(!c(player_image, position, designation), names_to = "Test", values_to = "Value") %>%
      mutate(Test = case_when(
        grepl("bench", Test) ~ "Bench",
        grepl("height", Test) ~ "Height",
        grepl("weight", Test)~ "Weight",
        grepl("vertical", Test) ~ "Vertical Jump",
        grepl("broad", Test) ~ "Broad Jump",
        grepl("shuttle", Test) ~ "Shuttle",
        grepl("x3cone", Test) ~ "3 Cone",
        grepl("x40yd", Test) ~ "40yd",
        TRUE ~ as.character(Test)
      )) %>%
      mutate(Value = round(Value, 2)) %>%
      mutate(`Test Type` = ifelse(Test %in% c("Weight", "Height"), "Size", "Athleticism"))
 
    
    highchart() %>% 
      hc_chart(polar = TRUE) %>% 
      hc_xAxis(categories = c(unique(hc_data_position$Test)),
               tickmarkPlacement = "on",
               lineWidth = 0) %>% 
      hc_yAxis(gridLineInterpolation = "polygon",
               lineWidth = 0,
               min = 0)%>%
      hc_yAxis(min = 0, max = 100) %>%
      hc_legend(enabled = TRUE) %>%
      hc_series(
        
        list(
          name = "Relative to Position",
          data = hc_data_position$Value,
          type = "column",
          colorByPoint = TRUE,
          tooltip = list(pointFormat =
                           "
                        Relative to Position<br>
                        T-score: {point.y}
                                        "),
          dataLabels = list(align = "center",
                            enabled = TRUE),
          colors =  ifelse(is.na(hc_data_position$Value), "#FFFFFF", ifelse(hc_data_position$Value > 75, "#28a745", ifelse(hc_data_position$Value > 50, "#adb5bd", ifelse(hc_data_position$Value <= 25, "#dc3545", 
                                                                                                                                                                          "#ffc107"))))
        ),
        list(
          name = paste0("Relative to Designation (", player_dash_data_reactive()$designation, ")"),
          data = hc_data_designation$Value,
          type = "line",
          colorByPoint = TRUE,
          marker = list(
            lineWidth = 2,
            radius = 6,
            symbol = 'diamond'
          ),
          tooltip = list(pointFormat =
                           "
                        Relative to Designation <br>
                        T-score: {point.y}
                                        "),
          dataLabels = list(align = "center",
                            enabled = FALSE),
          colors =  ifelse(is.na(hc_data_designation$Value), "#FFFFFF", ifelse(hc_data_designation$Value > 75, "#28a745", ifelse(hc_data_designation$Value > 50, "#adb5bd", ifelse(hc_data_designation$Value <= 25, "#dc3545",
                                                                                                                                                                                   "#ffc107"))))
        )
      ) %>%
      hc_plotOptions(
        series = list(
          borderWidth = 1,
          borderColor = 'black',
          dataLabels = list(enabled = TRUE)
        ),
        line = list(
          borderWidth = 2
        )
      )
    
  })
  
  
  output$player_dash_sten_chart <- renderHighchart({

    req(player_dash_data_reactive())

    hc_data_position_sten <- player_dash_data_reactive() %>%
      select(player_image, position=position_cfb,designation,weight_sten_position, height_sten_position, bench_sten_position, vertical_sten_position, broad_jump_sten_position, shuttle_sten_position, x3cone_sten_position,x40yd_sten_position) %>%
      rename_with(~str_remove(., '.percentile')) %>%
      pivot_longer(!c(player_image, position, designation), names_to = "Test", values_to = "STEN_position") %>%
      mutate(Test = case_when(
        grepl("bench", Test) ~ "Bench",
        grepl("height", Test) ~ "Height",
        grepl("weight", Test)~ "Weight",
        grepl("vertical", Test) ~ "Vertical Jump",
        grepl("broad", Test) ~ "Broad Jump",
        grepl("shuttle", Test) ~ "Shuttle",
        grepl("x3cone", Test) ~ "3 Cone",
        grepl("x40yd", Test) ~ "40yd",
        TRUE ~ as.character(Test)
      )) %>%
      mutate(STEN_position = round(STEN_position, 2)) %>%
      left_join(
        player_dash_data_reactive() %>%
          select(designation,weight_sten_designation, height_sten_designation, bench_sten_designation, vertical_sten_designation, broad_jump_sten_designation, shuttle_sten_designation, x3cone_sten_designation,x40yd_sten_designation) %>%
          rename_with(~str_remove(., '.percentile')) %>%
          pivot_longer(!designation, names_to = "Test", values_to = "STEN_designation") %>%
          mutate(Test = case_when(
            grepl("bench", Test) ~ "Bench",
            grepl("height", Test) ~ "Height",
            grepl("weight", Test)~ "Weight",
            grepl("vertical", Test) ~ "Vertical Jump",
            grepl("broad", Test) ~ "Broad Jump",
            grepl("shuttle", Test) ~ "Shuttle",
            grepl("x3cone", Test) ~ "3 Cone",
            grepl("x40yd", Test) ~ "40yd",
            TRUE ~ as.character(Test)
          )) %>%
          mutate(STEN_designation = round(STEN_designation, 2)), 
        by = c('Test', 'designation')
      ) 
    
    
    
    hchart(hc_data_position_sten, "bullet", hcaes(x = Test, y = STEN_position, target = STEN_designation), color = "black") %>%
      hc_chart(inverted = TRUE) %>%
      hc_yAxis(
        title = list(text = paste0(
          paste0("STEN")
        )),
        min = 0,
        max = 10,
        gridLineWidth = 0,
        plotBands = list(
          list(from = 0, to = 2, color = hex_to_rgba("#D00000", 0.6)),
          list(from = 2, to = 5, color = hex_to_rgba("#FDD844", 0.6)),
          list(from = 5, to = 8, color = hex_to_rgba("#E4E1D6", 0.6)),
          list(from = 8, to = 10, color = hex_to_rgba("green", 0.6))
        )
      ) %>%
      hc_xAxis(
        gridLineWidth = 3,
        gridLineColor = "white"
      ) %>% 
      hc_plotOptions(
        series = list(
          name = "STEN (Position)",
          pointPadding = 0.25,
          pointWidth = 15,
          borderWidth = 0,
          targetOptions = list(width = '200%', name = "STEN (Designation)")
        )
      ) 

    

  })

  
  
  output$player_dash_gt_tbl <- render_gt({
    
    hc_data_position_gt <- player_dash_data_reactive() %>%
      select(player_image, position=position_cfb,designation,weight_lbs, height_in, bench, vertical_in, broad_jump_in, shuttle, x3cone,x40yd) %>%
      rename_with(~str_remove(., '.percentile')) %>%
      pivot_longer(!c(player_image, position, designation), names_to = "Test", values_to = "Value") %>%
      mutate(Metrics = case_when(
        grepl("bench", Test) ~ "reps",
        grepl("height", Test) ~ "in",
        grepl("weight", Test)~ "lbs",
        grepl("vertical", Test) ~ "in",
        grepl("broad", Test) ~ "in",
        grepl("shuttle", Test) ~ "sec",
        grepl("x3cone", Test) ~ "sec",
        grepl("x40yd", Test) ~ "sec",
        TRUE ~ as.character(NA_character_)
      )) %>%
      mutate(Score = case_when(
        is.na(Value) ~ "N/A",
        grepl("bench", Test) ~ paste0(round(Value,0), " reps") ,
        grepl("height", Test) ~ paste0(round(Value,0), " in") ,
        grepl("weight", Test)~ paste0(round(Value,0), " lbs") ,
        grepl("vertical", Test) ~ paste0(round(Value,0), " in") ,
        grepl("broad", Test) ~ paste0(round(Value,0), " in") ,
        grepl("shuttle", Test) ~ paste0(round(Value,2), " sec") ,
        grepl("x3cone", Test) ~ paste0(round(Value,2), " sec") ,
        grepl("x40yd", Test) ~ paste0(round(Value,2), " sec") ,
        TRUE ~ as.character(NA_character_)
      )) %>%
      mutate(Test = case_when(
        grepl("bench", Test) ~ "Bench",
        grepl("height", Test) ~ "Height",
        grepl("weight", Test)~ "Weight",
        grepl("vertical", Test) ~ "Vertical Jump",
        grepl("broad", Test) ~ "Broad Jump",
        grepl("shuttle", Test) ~ "Shuttle",
        grepl("x3cone", Test) ~ "3 Cone",
        grepl("x40yd", Test) ~ "40yd",
        TRUE ~ as.character(Test)
      )) %>%
      mutate(Value = round(Value, 2)) %>%
      left_join(
        player_dash_data_reactive() %>%
          select(designation,weight_percentile_position, height_percentile_position, bench_percentile_position, vertical_percentile_position, broad_jump_percentile_position, shuttle_percentile_position, x3cone_percentile_position,x40yd_percentile_position) %>%
          rename_with(~str_remove(., '.percentile')) %>%
          pivot_longer(!designation, names_to = "Test", values_to = "Percentile") %>%
          mutate(Test = case_when(
            grepl("bench", Test) ~ "Bench",
            grepl("height", Test) ~ "Height",
            grepl("weight", Test)~ "Weight",
            grepl("vertical", Test) ~ "Vertical Jump",
            grepl("broad", Test) ~ "Broad Jump",
            grepl("shuttle", Test) ~ "Shuttle",
            grepl("x3cone", Test) ~ "3 Cone",
            grepl("x40yd", Test) ~ "40yd",
            TRUE ~ as.character(Test)
          )) , 
        by = c('Test', 'designation')
      ) %>%
      left_join(
        player_dash_data_reactive() %>%
          select(player_image, position=position_cfb,designation,weight_sten_position, height_sten_position, bench_sten_position, vertical_sten_position, broad_jump_sten_position, shuttle_sten_position, x3cone_sten_position,x40yd_sten_position) %>%
          rename_with(~str_remove(., '.percentile')) %>%
          pivot_longer(!c(player_image, position, designation), names_to = "Test", values_to = "STEN_position") %>%
          mutate(Test = case_when(
            grepl("bench", Test) ~ "Bench",
            grepl("height", Test) ~ "Height",
            grepl("weight", Test)~ "Weight",
            grepl("vertical", Test) ~ "Vertical Jump",
            grepl("broad", Test) ~ "Broad Jump",
            grepl("shuttle", Test) ~ "Shuttle",
            grepl("x3cone", Test) ~ "3 Cone",
            grepl("x40yd", Test) ~ "40yd",
            TRUE ~ as.character(Test)
          )) %>%
          mutate(STEN = round(STEN_position/2, 0)) %>%
          select(Test, STEN, STEN_position),
        by = "Test"
        
      ) %>%
      mutate(Percentile_stars = case_when(
        !is.na(Percentile) ~ round((Percentile/10)/2, 3),
        TRUE ~ as.numeric(0)
      ))
    
    hc_data_position_gt %>%
      select(Test, Score, Percentile_stars) %>%
      mutate(rating = purrr::map(Percentile_stars, rating_stars)) %>% 
      select(-Percentile_stars) %>%
      gt() %>% 
      tab_options(
        column_labels.hidden = TRUE,
        row.striping.include_table_body =FALSE,
        table.border.top.color = "white",
        heading.border.bottom.color = "white",
        row_group.border.bottom.color = "white",
        row_group.border.top.color = "white") %>%
      opt_table_lines(extent = c("none")) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold", size = "8")
        ),
        locations = cells_body(
          columns = Test
        )
      ) %>%
      tab_options(
        table.font.size = px(12),
        table.width = "250px"
      )
  })
  
  
  output$player_dash_main_output <- renderUI({
    
    req(input$player_dash_stat)
    
    if(input$player_dash_stat == "Percentile"){
      
      withSpinner(highchartOutput("player_dash_percentile_chart", height = 500), color = "#000000")
    } else if (input$player_dash_stat == "Z-score") {
      div(
        withSpinner(uiOutput("player_dash_z_filter"), color = "#000000"),
        withSpinner(highchartOutput("player_dash_z_chart", height = 450), color = "#000000")
      )
    } else if (input$player_dash_stat == "T-score") {
      withSpinner(highchartOutput("player_dash_t_chart", height = 500), color = "#000000")
    } else if (input$player_dash_stat == "STEN") {
      withSpinner(highchartOutput("player_dash_sten_chart", height = 500), color = "#000000")
    }
    
    
    
  })
  
  
  
  
  ##################################### Benchmark Tables #######################################
  
  
  
  
  output$benchmark_tbl_output <- renderUI({

    benchmark_table_global(test = c(input$benchmark_tbl_test), positions = c(input$benchmark_tbl_position))
  })
  
 
  
  
  
  ##################################### Team Dash #######################################
  
  
  output$team_dash_team_chart <- renderPlot({
    
    combine_team_ranking <- combine_clean %>%
      drop_na(draft_overall_pick) %>%
      group_by(school, logo=logo_light) %>%
      summarise(
        conference = first(conference),
        url_conference = first(url_conference),
        count = n(),
        count_first_round = sum(ifelse(draft_round == 1, 1, 0)),
        average_draft_overall = mean(draft_overall_pick)
      ) %>%
      ungroup() %>%
      filter(!logo == "https://www.freepnglogos.com/uploads/ncaa-png-logo/ncaa-png-logo-0.png")
    
    
    
    # Vhsd
    mean_count <- combine_team_ranking %>%
      summarise(mean = mean(count))
    
    mean_count_first <- combine_team_ranking %>%
      summarise(mean = mean(count_first_round))
    
    
    combine_team_ranking %>%
      mutate(img = paste0("<img src='", logo, "' width=50px></img>")) %>%
      select(school, conference, logo, img, count, count_first_round) %>%
      ggplot(aes(x=count, y=count_first_round)) +  
      geom_hline(yintercept = mean_count_first$mean, alpha = 0.6, lty = "dashed", size = 1) +
      geom_vline(xintercept = mean_count$mean, alpha = 0.6, lty = "dashed", size = 1) +
      geom_image(aes(image=logo), size=.06) +
      ggtitle("Team Draft Performance") +
      xlab("Total Players Drafted") +
      ylab("Total Players Drafted in the 1st Round ") +
      theme_ipsum_rc() +
      gghighlight(conference %in% c("Big 12", "SEC"),  unhighlighted_params = list(size = 0.05, colour = alpha("grey", 0.4)))
    
    
    
  })
  
  
  ########################################### Datatable ##############################
  output$datatable_stat_filter <- renderUI({
    
    div(style = 'margin-top:-20px; margin-bottom:-15px;text-align: center;', 
        radioGroupButtons(
          inputId = "datatable_stat",
          label = "",
          choices = c("Value", "Percentile","Z-score", "T-score", "STEN"),
          status = "danger",
          justified = TRUE,
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon"))
        )
    )
  })
  
  datatable_DT_react <-  reactive({
    
    if(input$datatable_stat == "Value"){
    data <- combine_clean %>%
      mutate(Player = paste(img_player, player)) %>%
      mutate(Team = paste(img_team, school)) %>%
      mutate(Conference = paste(img_conference, conference)) %>%
      select(Player, `Draft Year`=combine_year, Team, Conference, Position=position_cfb, Height=height_ft_in, Weight=weight_lbs, `Vertical Jump`=vertical_in, `Broad Jump`=broad_jump_in, `Bench Press (reps)`=bench, `3 Cone`=x3cone, Shuttle=shuttle, `40yd`=x40yd) %>%
      mutate_at(vars(Position, Height), as.factor)
    
    
    DT::datatable(
      data = data,
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:150% ;',paste0('NFL Combine Data - Values' )),
      rownames = FALSE,
      filter = 'top',
      escape = FALSE,
      extensions = c('Buttons', 'FixedColumns'),
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        lengthMenu = c(5, 10, 15, 25, 30, 50, 100, 200, 500, 1000),
        
        dom = 'Blfrtip',
        autoWidth = TRUE, 
        columnDefs = list(
          list(className = 'dt-head-center', targets = c("_all")),
          list(width = '80px', targets = c(4, 5)),
          list(className = 'dt-center', targets = c("_all"))
        ),
        buttons = list(
          'colvis',
          list(
            extend = 'csv', 
            title = "table"
          ),
          list(
            extend = 'excel', 
            title = "table"
          ),
          list(
            extend = 'copy'
          ),
          list(
            extend = 'print',
            
            exportOptions = list(
              columns= ':visible',
              stripHtml= FALSE,
              
              autoPrint = TRUE,
              orientation = 'landscape')
          )
        )
      )
    ) %>%
      formatStyle(c("Player", "Team", "Conference"), "white-space" = "nowrap") %>%
      formatStyle(names(data), background = '#fff',  fontSize = '14px')
      
    
    } else if(input$datatable_stat == "Percentile") {
      data <- combine_clean %>%
        mutate(Player = paste(img_player, player)) %>%
        mutate(Team = paste(img_team, school)) %>%
        mutate(Conference = paste(img_conference, conference)) %>%
        select(Player, `Draft Year`=combine_year, Team, Conference, Position=position_cfb, Height=height_percentile_position, Weight=weight_percentile_position, `Vertical Jump`=vertical_percentile_position, `Broad Jump`=broad_jump_percentile_position, `Bench Press`=bench_percentile_position, `3 Cone`=x3cone_percentile_position, Shuttle=shuttle_percentile_position, `40yd`=x40yd_percentile_position) %>%
        mutate_at(vars(Position), as.factor)
      
      
      DT::datatable(
        data = data,
        caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:150% ;',paste0('NFL Combine Data - Percentile (by Position)' )),
        rownames = FALSE,
        filter = 'top',
        escape = FALSE,
        extensions = c('Buttons', 'FixedColumns'),
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          lengthMenu = c(5, 10, 15, 25, 30, 50, 100, 200, 500, 1000),
          
          dom = 'Blfrtip',
          
          columnDefs = list(
            list(className = 'dt-head-center', targets = c("_all")),
            list(className = 'dt-center', targets = c("_all"))
          ),
          buttons = list(
            'colvis',
            list(
              extend = 'csv', 
              title = "table"
            ),
            list(
              extend = 'excel', 
              title = "table"
            ),
            list(
              extend = 'copy'
            ),
            list(
              extend = 'print',
              
              exportOptions = list(
                columns= ':visible',
                stripHtml= FALSE,
                
                autoPrint = TRUE,
                orientation = 'landscape')
            )
          )
        )
      ) %>%
        formatStyle(c("Player", "Team", "Conference"), "white-space" = "nowrap") %>%
        formatStyle(names(data), background = '#fff',  fontSize = '14px')
    } else if(input$datatable_stat == "Z-score") {
      data <- combine_clean %>%
        mutate(Player = paste(img_player, player)) %>%
        mutate(Team = paste(img_team, school)) %>%
        mutate(Conference = paste(img_conference, conference)) %>%
        select(Player, `Draft Year`=combine_year, Team, Conference, Position=position_cfb, Height=height_z_position, Weight=weight_z_position, `Vertical Jump`=vertical_z_position, `Broad Jump`=broad_jump_z_position, `Bench Press`=bench_z_position, `3 Cone`=x3cone_z_position, Shuttle=shuttle_z_position, `40yd`=x40yd_z_position) %>%
        mutate_at(vars(Position), as.factor) %>%
        mutate_if(is.numeric, ~round(., 1))
      
      
      DT::datatable(
        data = data,
        caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:150% ;',paste0('NFL Combine Data - Zscore (by Position)' )),
        rownames = FALSE,
        filter = 'top',
        escape = FALSE,
        extensions = c('Buttons', 'FixedColumns'),
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          lengthMenu = c(5, 10, 15, 25, 30, 50, 100, 200, 500, 1000),
          
          dom = 'Blfrtip',
          
          columnDefs = list(
            list(className = 'dt-head-center', targets = c("_all")),
            #list(visible = FALSE, targets = c(0)),
            list(className = 'dt-center', targets = c("_all"))
          ),
          buttons = list(
            'colvis',
            list(
              extend = 'csv', 
              title = "table"
            ),
            list(
              extend = 'excel', 
              title = "table"
            ),
            list(
              extend = 'copy'
            ),
            list(
              extend = 'print',
              
              exportOptions = list(
                columns= ':visible',
                stripHtml= FALSE,
                
                autoPrint = TRUE,
                orientation = 'landscape')
            )
          )
        )
      ) %>%
        formatStyle(c("Player", "Team", "Conference"), "white-space" = "nowrap") %>%
        formatStyle(names(data), background = '#fff',  fontSize = '14px')
    } else if(input$datatable_stat == "T-score") {
      data <- combine_clean %>%
        mutate(Player = paste(img_player, player)) %>%
        mutate(Team = paste(img_team, school)) %>%
        mutate(Conference = paste(img_conference, conference)) %>%
        select(Player, `Draft Year`=combine_year, Team, Conference, Position=position_cfb, Height=height_t_position, Weight=weight_t_position, `Vertical Jump`=vertical_t_position, `Broad Jump`=broad_jump_t_position, `Bench Press`=bench_t_position, `3 Cone`=x3cone_t_position, Shuttle=shuttle_t_position, `40yd`=x40yd_t_position) %>%
        mutate_at(vars(Position), as.factor) %>%
        mutate_if(is.numeric, ~round(., 0))
      
      
      DT::datatable(
        data = data,
        caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:150% ;',paste0('NFL Combine Data - Tscore (by Position)' )),
        rownames = FALSE,
        filter = 'top',
        escape = FALSE,
        extensions = c('Buttons', 'FixedColumns'),
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          lengthMenu = c(5, 10, 15, 25, 30, 50, 100, 200, 500, 1000),
          
          dom = 'Blfrtip',
          
          columnDefs = list(
            list(className = 'dt-head-center', targets = c("_all")),
            #list(visible = FALSE, targets = c(0)),
            list(className = 'dt-center', targets = c("_all"))
          ),
          buttons = list(
            'colvis',
            list(
              extend = 'csv', 
              title = "table"
            ),
            list(
              extend = 'excel', 
              title = "table"
            ),
            list(
              extend = 'copy'
            ),
            list(
              extend = 'print',
              
              exportOptions = list(
                columns= ':visible',
                stripHtml= FALSE,
                
                autoPrint = TRUE,
                orientation = 'landscape')
            )
          )
        )
      ) %>%
        formatStyle(c("Player", "Team", "Conference"), "white-space" = "nowrap") %>%
        formatStyle(names(data), background = '#fff',  fontSize = '14px')
    } else if(input$datatable_stat == "STEN") {
      data <- combine_clean %>%
        mutate(Player = paste(img_player, player)) %>%
        mutate(Team = paste(img_team, school)) %>%
        mutate(Conference = paste(img_conference, conference)) %>%
        select(Player, `Draft Year`=combine_year, Team, Conference, Position=position_cfb, Height=height_sten_position, Weight=weight_sten_position, `Vertical Jump`=vertical_sten_position, `Broad Jump`=broad_jump_sten_position, `Bench Press`=bench_sten_position, `3 Cone`=x3cone_sten_position, Shuttle=shuttle_sten_position, `40yd`=x40yd_sten_position) %>%
        mutate_at(vars(Position), as.factor) %>%
        mutate_if(is.numeric, ~round(., 0))
      
      
      DT::datatable(
        data = data,
        caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:150% ;',paste0('NFL Combine Data - STEN (by Position)' )),
        rownames = FALSE,
        filter = 'top',
        escape = FALSE,
        extensions = c('Buttons', 'FixedColumns'),
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          lengthMenu = c(5, 10, 15, 25, 30, 50, 100, 200, 500, 1000),
          
          dom = 'Blfrtip',
          
          columnDefs = list(
            list(className = 'dt-head-center', targets = c("_all")),
            #list(visible = FALSE, targets = c(0)),
            list(className = 'dt-center', targets = c("_all"))
          ),
          buttons = list(
            'colvis',
            list(
              extend = 'csv', 
              title = "table"
            ),
            list(
              extend = 'excel', 
              title = "table"
            ),
            list(
              extend = 'copy'
            ),
            list(
              extend = 'print',
              
              exportOptions = list(
                columns= ':visible',
                stripHtml= FALSE,
                
                autoPrint = TRUE,
                orientation = 'landscape')
            )
          )
        )
      ) %>%
        formatStyle(c("Player", "Team", "Conference"), "white-space" = "nowrap") %>%
        formatStyle(names(data), background = '#fff',  fontSize = '14px')
    }
    
  }) 
  
  
  output$datatable_DT <- DT::renderDataTable({
    
    
    datatable_DT_react() 
    
    
  })
 
  
#################################################################################################  
} #### End of APP

shinyApp(ui, server)