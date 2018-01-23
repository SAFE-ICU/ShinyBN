shinybn <- function()
{
  shiny::runApp(appDir = system.file('bn',package = 'ShinyBN'),launch.browser = TRUE)
}
