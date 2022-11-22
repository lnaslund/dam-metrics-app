chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; font-size: 16px}") 
      )
    )),
    div(id=inputId, class="chooser", 
        div(class="chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices, style = "max-width: 35em")
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-alt-circle-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-alt-circle-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices, style = "max-width: 35em")
        )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)