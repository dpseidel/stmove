#' Build a Report (RStudio Add-in)
#'
#' @description `report_addin()` opens an [RStudio
#'   gadget](https://shiny.rstudio.com/articles/gadgets.html) and
#'   [addin](http://rstudio.github.io/rstudioaddins/) that allows you to
#'   choose which analyses to run and build a report.
#'   Appears as "Build report" in the RStudio Addins menu.
#'
#'
#' @export
report_addin <- function() {

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text

  ui <- miniUI::miniPage(
    # shiny::tags$head(shiny::includeCSS(path(resource_path, "reprex.css"))),
    miniUI::gadgetTitleBar(
      shiny::p(
        "Use",
        shiny::a(href = "https://stmove.pkgdown.com", "stmove"),
        "to build a movement report"
      ),
      right = miniUI::miniTitleBarButton("done", "Build", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      stableColumnLayout(
        shiny::textInput("df", "Data", value = defaultData),
        shiny::textInput("path", "Output File Path"),
        shiny::textInput("proj4", "Projection (proj.4 string)")
      ),
      shiny::uiOutput("pending"),
      miniUI::miniContentPanel(
        shiny::checkboxGroupInput(
          "stats",
          "Calculate which movement statistics?",
          c(
            "Rolling Window" = "rolling",
            "Diurnal" = "diurnal",
            "Lunar" = "lunar",
            "Seasonal" = "seasonal"
          )
        ),
        shiny::conditionalPanel(
          condition = "input.stats.includes('seasonal')",
          shiny::textInput(
            inputId = "seas",
            label = "Season start dates (Julian dates)"
          )
        ), shiny::checkboxGroupInput(
          "construct",
          "Calculate which space use constructions?",
          c(
            "AKDE" = "akde",
            "T-Locoh" = "klocoh"
          )
        ), shiny::checkboxGroupInput(
          "wavelet",
          "Conduct and plot which wavelet analysis?",
          c(
            "Step Size" = "dist",
            "Turning Angle" = "rel.angle",
            "ACF - Step Size" = "acf_dist",
            "ACF - Turning Angle" = "acf_ang",
            "CCF" = "ccf"
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    reactive_data <- shiny::reactive({
      dataString <- input$df

      if (!nzchar(dataString)) {
        return(errorMessage("data", "No dataset specified."))
      }

      if (!exists(dataString, envir = .GlobalEnv)) {
        return(errorMessage("data", paste("No dataset named '", dataString, "' available.")))
      }

      get(dataString, envir = .GlobalEnv)
    })

    output$pending <- shiny::renderUI({
      data <- reactive_data()
      if (isErrorMessage(data)) {
        shiny::h4(style = "color: #AA7732;", data$message)
      }
    })


    shiny::observeEvent(input$done, { # a couple things to think about -- how to specify data, output file?
      shiny::stopApp(build_report(
        df = reactive_data(),
        path = input$path,
        stats = input$stats,
        construct = input$construct,
        proj4 = input$proj4,
        seas = input$seas,
        wavelet = input$wavelet
      ))
    })
  }

  app <- shiny::shinyApp(ui, server, options = list(quiet = TRUE))
  shiny::runGadget(app, viewer = shiny::dialogViewer("Build movement report"))
}

# wrapper function for shiny columns from rstudio/addinexamples
stableColumnLayout <- function(...) {
  dots <- list(...)
  n <- length(dots)
  width <- 12 / n
  class <- sprintf("col-xs-%s col-md-%s", width, width)
  shiny::fluidRow(
    lapply(dots, function(el) {
      shiny::div(class = class, el)
    })
  )
}

# error message class from rstudio/addinexamples
errorMessage <- function(type, message) {
  structure(
    list(type = type, message = message),
    class = "error_message"
  )
}

# error message check from rstudio/addinexamples
isErrorMessage <- function(object) {
  inherits(object, "error_message")
}
