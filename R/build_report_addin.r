#' Build a Report (RStudio Add-in)
#'
#' @description `report_addin()` opens an [RStudio
#'   gadget](https://shiny.rstudio.com/articles/gadgets.html) and
#'   [addin](http://rstudio.github.io/rstudioaddins/) that allows you to
#'   choose which analyses to run and build a report.
#'   Appears as "Build Report" in the RStudio Addins menu.
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
      ), miniUI::miniContentPanel(
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
            "AKDE" = "clipboard",
            "T-Locoh" = "cur_sel"
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
    shiny::observeEvent(input$done, { # a couple things to think about -- how to specify data, output file?
      shiny::stopApp(build_report(
        df = input$df,
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
  shiny::runGadget(app, viewer = shiny::dialogViewer("Render reprex"))
}

# wrapper function for shiny columns
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
