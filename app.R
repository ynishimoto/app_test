#SERVER
#######################################################
library(data.table)
#LOAD DATA
dat <- read.csv('dat1.csv')

#SERVER
counties <- names(dat)
server <- function(input, output, session) {
  
  output$plot2 <- renderPlot({
    
    #location <- "Greater.Toronto..ON"
    location <- input$select_county
    prices <- as.numeric(dat[[location]])
    prices <- prices[!is.na(prices) & prices>0]
    avg <- mean(prices)
    std_dev <- sd(prices)
    prices <- prices[prices > avg - std_dev * 1.5 & prices < avg + std_dev * 1.5]
    
    dens <- density(prices, adjust=input$density_adjust)
    #dens <- density(prices, adjust=1)
    x <- dens$x
    y <- dens$y
    y.norm <- dnorm(x, mean=mean(prices), sd=sd(prices))
    
    df_p <- data.frame(x=x, y=y, y.norm=y.norm)
    df_p <- df_p[df_p$x>0,]
    
    ggplot(data=df_p) +
      geom_line(linetype=1, col='blue', size=1.2, aes(x=x, y=y, group=1)) +
      geom_line(linetype=1, col='red', size=1.2, aes(x=x, y=y.norm, group=1)) +
      labs(title='Adjusted Price Distribution') +
      theme(
        plot.title = element_text(size=15, face='bold'),
        panel.background = element_rect(fill='white', colour='white', size=2, linetype="solid"),
        axis.text.x = element_text(colour='black', size=12),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_line(size=0.5, linetype='solid', colour='grey'),
        legend.position="right") +
      scale_y_continuous(labels=function(x) format(x, scientific = TRUE), name='') +
      scale_x_continuous(labels=function(x) prettyNum(x, ','), name='Adjusted Prices ($)') +
      theme(axis.title.x=element_text(vjust=-1))
  })
}



#UI
#######################################################
library(ggplot2)
options(scipen=999)


#UI
ui <- navbarPage("Housing Market Analyzer",
           
           tabPanel("Dashboard",
                    bootstrapPage(
                      selectInput(inputId='select_county', 
                                  shiny::HTML("<span style='color: blue'><i>Select Region: </i></span>"), 
                                  choices=counties, selected=counties[1]),
                      plotOutput(outputId="plot1",  width="600px", height="120px"),
                      plotOutput(outputId="plot2",  width="600px", height="300px"),
                      sliderInput(inputId = "density_adjust", 
                                  shiny::HTML("<span style='color: blue'><i>Adjust Bandwidth: </i></span>"), 
                                  min=0.2, max=3, value=1, step=0.2),
                      plotOutput(outputId="plot3", width="600px", height="120px")
                    )
           )
)

shinyApp(ui=ui, server=server)
