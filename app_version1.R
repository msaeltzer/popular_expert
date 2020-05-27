library(shiny)
library(tidyverse)
library(ggplot2)
library(readstata13)
library(sjmisc)
library(sjlabelled)

# dataset prep and import

df<-read.csv("https://www.dropbox.com/s/sce8ktvvzws3jhy/party_means.csv?dl=1")

df<-df[,-1]

df$country_id <- to_factor(df$country_id)


df$party_id <- to_label(df$party_id)


df$family <- to_factor(df$family)

levels(df$family) <- c("Radical Right", "Conservatives", "Liberal", "Christian Democratic", "Socialist", "Radical Left", "Green", 
                       "Regionalist", "No Family", "Confessional", "Agrarian/ Center")



## Each shiny app needs a user interface and a server

# user interface

# we create a page with tabs: navbar page
ui_full<-navbarPage("Expert Survey on Populism",
           tabPanel("Scatterplot",                        # we begin a tab
                    
                    titlePanel("Comparing Dimensions"),    # give it a title
                    
                    sidebarLayout(                           # we choose a sidebar layout 
                       
                      sidebarPanel(                           # control panel
                        # We need three inputs so far: two scale selections and a on/off button
                                                              # we choose from the scale varnames        
                        selectInput('scale1', 'Scale 1', names(df)[c(9:24,27)],selected=names(df)[27]),
                        selectInput('scale2', 'Scale 2', names(df)[c(9:24,27)],selected=names(df)[27])
                        ,selectInput('pname', 'Partynames',c("On","Off"),"Off")
                      ),
                      
                      mainPanel(                              # plot panel
                        plotOutput("scatter"),
                      ) 
                      
                 ) # end layout
              
           ), # end tab
           
           tabPanel("Barplot",    # new tab
                    sidebarLayout( # again, sidebar layout
                      
                      sidebarPanel( # control panel
                        # choose dimension
                        selectInput('scale', 'Dimension', names(df)[c(9:24,27)],selected=names(df)[27]),
                        # choose by which variable to display
                        selectInput("group", label = "Plot By", 
                                    choices = c("country_id","family"))),
                      
                      mainPanel( # plot panel
                        plotOutput("bar")
                    )
                      
                 ) # end layout
              
            )  # end tab
        
    ) # end UI
  
input<-list()
input$group="family"
input$scale<-"populism"

# we define what the server returns for input
server_full <- function(input, output) {
  
  # plot tab 1
  output$bar<-renderPlot({
                                    # input: what to group by                     input: what scale to display    
    ggplot(data= df %>% group_by(rel=get(input$group)) %>% summarise(scale = mean(get(input$scale), na.rm = TRUE))) + 
      geom_bar(mapping = aes(x = reorder(rel,-scale, FUN = mean), y=scale), fill= 'steelblue', stat=
                 'identity') +
      labs(
        title = paste0(input$scale," per ", input$rel), # paste title from inputs
        x =paste(input$rel),  # paste x axis labelfrom group 
        y =input$scale,       # paste y axis label by scale
        caption = "Source: POPPA"
      ) + theme(
        axis.text.x = element_text(angle = 90)
      ) 
  })
  #plot tab 2
  output$scatter<-renderPlot({
    
              ggplot(df) + geom_point(mapping = aes(x = get(input$scale1), y = get(input$scale2), color = family))+
                labs(x = input$scale1,
                  y = input$scale2,
                  title = paste(input$scale1,"versus",input$scale2),
                  caption = "Source: POPPA",
                  color = "Parties"
                  )+if(input$pname=="On"){  # add a conditional text plot on input button ON
                    geom_text(aes(x = get(input$scale1), y = get(input$scale2),label=party), size=2,hjust=2)}
                })
  
          }


shinyApp(ui=ui_full,server=server_full)
