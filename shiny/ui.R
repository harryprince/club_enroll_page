require(shiny)
require(shinythemes)
shinyUI(
    fluidPage(theme = shinytheme("united"),
        tags$head(
            tags$title('Fantasy 动漫社 2016 春季纳新线上报名 beta-0.1'),
            tags$link(rel = "stylesheet", href = "lib/font-awesome/css/font-awesome.min.css"),
            tags$script(type = 'text/javascript', src = 'custom.js'),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        div(class = 'holder',
            uiOutput('page'),
            br(),
            p('This site is developed by',
              a(tags$i(class = 'fa fa-github'), '梨梓', href = 'https://github.com/lytze', target = '_blank',
                `data-toggle` = 'tooltip', `data-title` = '在 Github 上找到梨梓！'),
              'with',
              a('R-Shiny', href = 'shiny.rstudio.com/', target = '_blank',
                `data-toggle` = 'tooltip', `data-title` = 'R-Shiny 首页')),
            tags$script(type = 'text/javascript', src = 'tooltip-regist.js')
        )
    )
)