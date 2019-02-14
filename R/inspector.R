# require(shiny); require(shinyBS); require(shinymaterial); require(DeclareDesign)
# round_df <- DDshiny:::round_df

####
####  Welcome page
####

library(DesignLibrary)
library(shiny)
library(shinymaterial)
library(shinythemes)
library(shinyBS)
library(ggplot2)
library(rlang)
library(DT)


source("R/aaa_helpers.R")

# welcome window ----------------------------------------------------------

welcome <-         material_modal(
  modal_id = "welcome_modal",
  button_text = "Modal",
  title = "Welcome",
  button_color = "red lighten-3",
  # actionButton("import_library", "Import from library..."),
  # actionButton("import_file", "Import from file..."),
  # actionButton("import_url", "Import from url..."),
  # uiOutput("import_panel_choice")
  renderUI(importLibrary)
)


welcome <- remove_close_button_from_modal(welcome)
welcome[[2]][[1]] <- NULL# skip making outer button

welcome[[3]] <-   shiny::tags$script("
                                     $(document).ready(function(){
                                     $('#welcome_modal').modal('open', {dismissible: false});
                                     });")




# Import designer function ------------------------------------------------

### Different types of import dialogs
importLibrary <- material_card("Import from DesignLibrary",
                               uiOutput("import_library_ui"),
                               actionButton("import_button", "OK")
)

importFile <- material_card("Import from File",
                            fileInput("import_file1", "Choose RDS File",
                                      accept = ".RDS"
                            ),
                            # uiOutput("import_button", "Import")
                            actionButton("import_button", "OK")
)


importUrl <- material_card("Import from URL",
                           material_text_box("import_url_txt", "URL"),
                           actionButton("import_button", "OK")
)

diagnostic_params <-       material_card(
  "Diagnostic Plot",
  my_tipify(numericInput("d_sims", "Num of Sims:", 10), "Diagnosand (y-axis)"),
  my_tipify(numericInput("d_draws", "Num of Draws:", 50) ,"Main design parameter (x-axis)"),
  my_tipify(numericInput("d_draws", "Num of Draws:", 50) ,"Add design parameter (optional layer)"),
  actionButton("run", "Run Design")
)

#' @import shinymaterial
#' @import shinyBS
#'
#'
inspector.ui <- material_page(
  title = "Declare Design Inspector",
  nav_bar_color = nav_bar_color,
  tags$script('
              Shiny.addCustomMessageHandler("closeModal",
              function(name) {
              $(name).modal("close");
              });
              '),
  shiny::tags$title("DeclareDesign Inspector"),
  # background_color = "blue lighten-4",
  # shiny::tags$h1("Page Content"),
  bootstrapLib(),
  withMathJax(),
  # includeCSS(system.file("inst/css/materialize.css", package="DDshiny")), #NOTE:added folder here

  #TODO This is a super gross way of getting the tooltips to update correctly.
  refresh_tips <- shiny::tags$script("
                                     setInterval(function(){
                                     console.log('HIARYLAH');
                                     $('.tooltipped').tooltip({delay: 50});
                                     }, 10*1000);
                                     "),
  uiOutput("welcome"),
  material_row(
    material_column(
      width = 3,
      material_card("",
                    div(style="text-align: center;", # display:inline-block;width:100%;
                        actionButton("refresh",
                                     label = HTML("Design Menu")#,
                                     # onclick = "https://eos.wzb.eu/bicalho/DDinspector/",
                                     # icon = icon("refresh", lib = "glyphicon")
                        ))
      ),
      uiOutput("designParameters")#,
      # uiOutput("plotParameters")
    ),
    material_column(
      width = 6,
      # offset=6,
      material_card("Output",
                    uiOutput("descriptionPanel"),
                    uiOutput("citationPanel"),
                    p("Note: The results of the design diagnosis are obtained from 500 simulations of the design and 100 bootstrap simulations."),
                    # verbatimTextOutput("print"),
                    bsCollapse(id="outputCollapse", open="About",
                               # bsCollapsePanel("Citation", uiOutput("citationPanel")),
                               bsCollapsePanel("Summary", uiOutput("summaryPanel")),
                               bsCollapsePanel("Diagnostics", DT::dataTableOutput("diagnosticsPanel")),
                               bsCollapsePanel("Diagnostic Plot", uiOutput("diagnosticPlot")),
                               bsCollapsePanel("Code", verbatimTextOutput("codePanel"),
                                               downloadButton("download_code", "Export Code...")),
                               bsCollapsePanel("Simulated Data", dataTableOutput("simulationPanel")),
                               bsCollapsePanel("About DeclareDesign Inspector", value="About",
                                               h5("About the DeclareDesign Inspector"),
                                               # p("This software is in alpha release. Please contact the authors before using in experiments or published work."),
                                               p("  This project is generously supported by a grant from the Laura and John Arnold Foundation and seed funding from EGAP.")
                               )
                    )
      )
    ),
    material_column(
      width = 3,
      uiOutput("plotParameters")
    )
  )
)


inspector.server <- function(input, output, clientData, session) {
  library(DeclareDesign)
  # require(pryr)
  # require(base64enc)
  library(ggplot2)
  library(shinyBS)
  library(stringr)
  library(shinymaterial)

  # session$allowReconnect("force") #TODO
  observeEvent(input$refresh, {
    showModal(welcome)
  })

  # create reactive values of DD --------------------------------------------

  DD <-   reactiveValues(design = NULL,
                         # design_instance=NULL,
                         diagnosis=NULL, code="",
                         precomputed=FALSE, observers=list(), design_id = NULL)



  # create design parameters from DD ----------------------------------------

  output$designParameters <- renderUI({
    design_fn <- req(DD$design)
    v <- get_shiny_arguments(design_fn)
    definitions <- attr(design_fn, "definitions")
    f <- names(v)
    boxes <- list()

    #NOTE: This creates the input values from DD objects.
    #This should only be created for arguments that we chose to pass onto Shiny through the attribute `shiny_args` of the design
    for(i in seq_along(f)){
      fi <- f[i]
      input_id <- paste0("d_", fi)
      input_label <- paste0(fi, ":")
      if(length(v[[i]]) == 1){
        boxes[[i]] <- numericInput(input_id, input_label,  v[[i]])
      } else {
        boxes[[i]] <- selectInput(input_id, input_label, sort(v[[i]]), v[[i]][1])
      }
    }

    if(!is.null(definitions) && all(f %in% definitions$names)){
      for(i in seq_along(f)){
        boxes[[i]] <- my_tipify(boxes[[i]], definitions$tips[definitions$names == f[i]])
      }
    }


    #NOTE: Here is where we would need to change to take the vignette that is saved/exported by the function called on design (@Jasper)
    boxes[[length(boxes) + 1]] <- downloadButton("download_design", "Export Design...")
    print("Parameter inputs created")
    do.call(material_card, c(title="Design Parameters", boxes))
  })

  output$plotParameters <- renderUI({
    design_fn <- req(DD$design)
    f <- names(get_shiny_arguments(design_fn))

    boxes <- list()
    boxes[[1]] <- uiOutput("estimator")
    boxes[[2]] <- uiOutput("coefficient")
    boxes[[3]] <- uiOutput("diag_param")#, diagnosand_names, selected = diagnosand_names[1])
    boxes[[4]] <- selectInput("x_param", "Main parameter (x-axis)",
                              choices = f)
    boxes[[5]] <- selectInput("opt_param", "Add layering parameter (optional)",
                              choices = c("(none)", f))

    tips <- c("Design Estimator",
              "Coefficient",
              "Diagnosand (vertical axis)",
              "Parameter to be placed for the horizontal axis",
              "Parameter used for separate curves")

    for(i in 1:length(boxes)){
      boxes[[i]] <- my_tipify(boxes[[i]], tips[i])
    }
    do.call(material_card, c(title="Plot Parameters", boxes))
  })

  output$estimator <- renderUI({
    design_i <- req(DD$design_instance())
    selectInput("estimator", "Estimator Label", choices = unique(draw_estimates(design_i)$estimator_label))
  })

  output$diag_param <- renderUI({
    selectInput("diag_param", "Diagnosand (y-axis)", choices = DD$diagnosis$diagnosand_names)
  })

  observe(updateSelectInput(session, "opt_param",
                            choices = c("(none)", dplyr::setdiff(names(get_shiny_arguments(DD$design)), input$x_param))))


  output$coefficient <- renderUI({
    design_i <- req(DD$design_instance())
    estimates <- draw_estimates(design_i)
    print("Loaded estimate names:")
    print(names(estimates))
    if("term" %in% names(estimates)) coefficients <- estimates$term[estimates$estimator_label == input$estimator]
    else coefficients <- ""
    selectInput("coefficient", "Coefficient", choices = coefficients)
  })

  #REVIEW
  output$welcome <- renderUI({
    query <- parseQueryString(session$clientData$url_search)
    if("import_library" %in% names(query)){
      if (paste0(query[['import_library']], "_designer") %in% ls(as.environment("package:DesignLibrary"))) {
        updateTextInput(session, "import_library_dropdown", value = query[['import_library']])
        e <- as.environment("package:DesignLibrary")
        DD$design <- get(paste0(query[['import_library']], "_designer"), e)
        DD$precomputed <- TRUE
        DD$diagnosis <- readRDS(paste0("data/", query[['import_library']], "_shiny_diagnosis.RDS"))
        if(!is.null(DD$diagnosis)) message(paste0("Loaded ", query, " design diagnosis"))
        session$sendCustomMessage(type = "closeModal", "#welcome_modal")

        message("loaded sidefile")
        return(shiny::tags$script("
                                  console.log('sidefile loaded')
                                  Shiny.onInputChange('import_button', 99999)
                                  "))
      }else{
        welcome
      }
    }else{
      welcome
    }
  })


  output$diagnosticParameters <- renderUI({
    if(!DD$precomputed) diagnostic_params
    else NULL
  })

  observeEvent(input$import_button, {
    # req(input$import_file_button)
    design <- isolate(DD$design)
    if(is.character(design)){
      tf <- tempfile()
      download.file(design, tf)
      design <- DD$design <- readRDS(tf)
    }
    message("***!\n\t", input$import_button, "\n****")
    if(!is.null(design)) {
      session$sendCustomMessage(type = "closeModal", "#welcome_modal")
      # loadDesign(output, design)
    }
  }, ignoreNULL = FALSE)
  observeEvent(input$import_file1, {
    DD$design <- readRDS(input$import_file1$datapath)
    DD$precomputed <- FALSE

    # str(DD$design)
  }, ignoreNULL = TRUE)
  observeEvent(input$import_url_txt, {
    DD$design <- input$import_url_txt
    DD$precomputed <- FALSE

    str(DD$design)
  }, ignoreNULL = TRUE)

  #NOTE: HERE WE ARE DRAWING DESIGN LIBRARIES FROM THE DesignLibrary folder (currently local)
  output$import_library_ui <- renderUI({
    cached <- str_replace(grep("designer$", ls(as.environment("package:DesignLibrary")), value = TRUE), "_designer", "")
    cached <- intersect(cached, gsub("_shiny_diagnosis.RDS", "", list.files("data", pattern = ".RDS")))
    names(cached) <- unique(str_to_title(str_replace_all(str_replace(basename(cached), "[.]R$", ""), "_", " ")))
    selectInput("import_library_dropdown", "Library:", cached)
  })

  observeEvent(input$import_library_dropdown,{
    if(paste0(input$import_library_dropdown, "_designer") %in% ls(as.environment("package:DesignLibrary"))){ #NOTE: change this here
      e <- as.environment("package:DesignLibrary")
      DD$design <- get(paste0(input$import_library_dropdown, "_designer"), e)
    }
    DD$precomputed <- TRUE
    DD$diagnosis <- readRDS(paste0("data/", input$import_library_dropdown, "_shiny_diagnosis.RDS"))
    if(!is.null(DD$diagnosis)) message(paste0("Loaded ", input$import_library_dropdown, " design diagnosis"))
  }, ignoreNULL=TRUE)


  #restrict to diagnosis for the parameters set in shiny `input`
  DD$shiny_args <- reactive({
    args <- list()#formals(DD$design)[-length(formals(DD$design))]
    for(n in names(get_shiny_arguments(DD$design))){
      args[[n]] <- as.numeric(input[[paste0("d_", n)]])
    }
    args
  })

  DD$all_args <- reactive({
    args <- formals(DD$design)#formals(DD$design)[-length(formals(DD$design))]
    for(n in intersect(names(formals(DD$design)), sub("d_", "", names(input)))){
      args[[n]] <- as.numeric(input[[paste0("d_", n)]])
    }
    args
  })

  design_id <- reactive({
    if(DD$precomputed){
      shiny_args <- names(get_shiny_arguments(DD$design))
      t <- c()
      for(n in shiny_args){
        v <- which(DD$diagnosis$diagnosands[[n]] == as.numeric(input[[paste0("d_", n)]]))
        v <- DD$diagnosis$diagnosands$design_label[v]
        t <- c(t, v)
      }
      Mode(t)
    }
  })

  diagnosis_instance <- reactive({
    diag <- lapply(DD$diagnosis, function(o){
      if(is.data.frame(o)){
        o <- o[o$design_label == paste0("design_", design_id()),]
      }
      o
    })
    if(DD$precomputed) diag$diagnosands <- diag$diagnosands[,!names(diag$diagnosands) %in% names(get_shiny_arguments(DD$design))]
    print("Loaded diagnosis instance")
    return(diag)
  })

  DD$design_instance <- reactive({
    e <- environment()
    print("`design_instance` created")
    do.call(DD$design, DD$shiny_args(), envir = parent.env(e))
  })


  output$print <- renderText(capture.output(names(input)))

  output$diagnosticsPanel <-    DT::renderDataTable({
    diag_tab <- get_diagnosands(diagnosis = diagnosis_instance())
    if(DD$precomputed){
      diag_tab <- dplyr::select(diag_tab, -design_label, -n_sims)
    }

    pretty_diagnoses(diag_tab)
  },
  options = list(orderClasses = TRUE, pageLength = 10,
                 scrollX = TRUE, width = 100,
                 rownames = FALSE, dom = "tlp"))

  output$diagnosticPlot <- renderUI({
    plotOutput("user_defined_plot")
  })

  output$user_defined_plot <- renderPlot({
    args <- DD$shiny_args
    plotdf <- NULL
    if(DD$precomputed){
      plotdf <- get_diagnosands(DD$diagnosis)

      #restrict to cases where all other parameters match input
      fix_arg <- names(get_shiny_arguments(DD$design))[!names(get_shiny_arguments(DD$design)) %in% c(input$x_param, input$opt_param)]

      for(col in fix_arg){
        plotdf <- plotdf[plotdf[[col]]==input[[paste0("d_",col)]],]
      }

      #further restrict to estimator chosen
      estimator <- input$estimator #trimws(gsub(".*?[)]$", "", input$estimator), which = "both")
      coefficient <- input$coefficient #regmatches(input$estimator, gregexpr("(?<=\\().*?(?=\\))", input$estimator, perl=T))[[1]][1]

      if(input$coefficient!=""){
        plotdf <- plotdf[plotdf$estimator_label == estimator &
                           plotdf$term == coefficient,]
      }else{
        plotdf <- plotdf[plotdf$estimator_label == estimator,]
      }


    }

    if(input$import_library_dropdown %in% "mediation_analysis"){
      plotdf$estimator_label <- paste0(plotdf$estimator_label, " (", plotdf$coefficient, ")")
    }

    plotdf$diagnosand <- plotdf[[input$diag_param]]
    plotdf$diagnosand_min <- plotdf[[input$diag_param]] - 1.96*plotdf[[paste0("se(", input$diag_param, ")")]]
    plotdf$diagnosand_max <- plotdf[[input$diag_param]] + 1.96*plotdf[[paste0("se(", input$diag_param, ")")]]
    plotdf$x_param <- as.numeric(paste0(plotdf[[input$x_param]]))
    ifelse(input$opt_param != "(none)", plotdf$opt_param <- as.factor(plotdf[[input$opt_param]]), plotdf$opt_param <- NA)

    if(input$opt_param != "(none)"){
      p <- ggplot(plotdf) +
        aes(x=x_param, y=diagnosand, ymin=diagnosand_min, ymax=diagnosand_max,
            group=opt_param, color=opt_param, fill=opt_param) +
        geom_line()

    }else{
      p <- ggplot(plotdf) +
        aes(x=x_param, y=diagnosand, ymin=diagnosand_min, ymax=diagnosand_max)
    }

    p <- p +
      geom_point(na.rm = TRUE) +
      scale_y_continuous(name=input$diag_param) + #, limits=0:1, breaks=0:4/4, minor_breaks = 0:10/10) +
      dd_theme() +  labs(fill=input$opt_param,color=input$opt_param, x = input$x_param)

    p

  })

  output$simulationPanel <-    renderDataTable({
    sims_tab <- draw_data(DD$design_instance())
    sims_tab <- round_df(sims_tab, 4)
    sims_tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = TRUE, pageLength=10, info = FALSE, lengthChange= FALSE, scrollX = TRUE))

  DD$code <- reactive({
    if(!is.null(attr(DD$design_instance(), "code"))){
      code <- attr(DD$design_instance(), "code")
      paste(code, collapse = "\n")
    } else if(requireNamespace("pryr")){
      code <- paste(deparse(pryr::substitute_q(body(DD$design), DD$all_args())), collapse="\n")
      gsub("[{]\n|\n[}]|[}]\n]", "", code) # remove surounding curly
    }
  })

  output$descriptionPanel <- renderUI(HTML(attr(DD$design, "description")))

  output$citationPanel <- renderUI(
    # HTML(format(get_author(paste0(input$import_library_dropdown, "_designer")), style="html"))
    HTML("Author: DeclareDesign Team")
  )

  output$summaryPanel  <- renderUI({
    pretty_summary(summary(DD$design_instance()))

  })

  output$codePanel     <- renderText(DD$code())


  # NOTE: These two options are from function in DDtools that export the design instance and code for specific parameters

  output$download_design <- downloadHandler(
    filename=function() {
      paste0("design-", Sys.Date(), ".RDS")
    },
    content = function(file) {
      saveRDS(DD$design_instance(), file)
    }
  )

  output$download_code <- downloadHandler(
    filename=function() {
      paste0("design-", Sys.Date(), ".R")
    },
    content = function(file) {
      writeLines(DD$code(), file)
    }
  )

}


#' @export
DDinspector <- shinyApp(inspector.ui, inspector.server)

