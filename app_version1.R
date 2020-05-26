library(shiny)
library(tidyverse)
library(ggplot2)
library(readstata13)
library(sjmisc)
library(sjlabelled)

# dataset prep

load("party_means.rdata")

df<-table

#factor the variables

df$country_id <- to_factor(df$country_id)

df$country_id <- to_label(df$country_id)

df$party_id <- to_label(df$party_id)


df$family <- to_factor(df$family)

levels(df$family) <- c("Radical Right", "Conservatives", "Liberal", "Christian Democratic", "Socialist", "Radical Left", "Green", 
                       "Regionalist", "No Family", "Confessional", "Agrarian/ Center")




ui_full<-navbarPage("Expert Survey on Populism",
           tabPanel("Scatterplot",
                    
                    titlePanel("Comparing Dimensions"),
                    
                    sidebarLayout(
                      
                      sidebarPanel(
                        selectInput('scale1', 'Scale 1', names(df)[c(9:24,27)],selected=names(df)[27]),
                        selectInput('scale2', 'Scale 2', names(df)[c(9:24,27)],selected=names(df)[27])
                        ,selectInput('pname', 'Partynames',c("On","Off"),"Off")
                      ),
                      
                      mainPanel(
                        plotOutput("scatter"),
                      )
                      
                 )
              
           ),
           
           tabPanel("Barplot",
                    sidebarLayout(
                      
                      sidebarPanel(
                        
                        selectInput('scale', 'Dimension', names(df)[c(9:24,27)],selected=names(df)[27]),
                        
                        selectInput("group", label = "Plot By", 
                                    choices = c("country_id","family"))),
                      
                      mainPanel(
                        plotOutput("bar")
                    )
                      
                 )
              
            )  
        
    )
  

server_full <- function(input, output) {
  
  output$bar<-renderPlot({
    
    ggplot(data= df %>% group_by(rel=get(input$group)) %>% summarise(scale = mean(get(input$scale), na.rm = TRUE))) + 
      geom_bar(mapping = aes(x = reorder(rel,-scale, FUN = mean), y=scale), fill= 'steelblue', stat=
                 'identity') +
      labs(
        title = paste0(input$scale," per ", input$rel2),
        x =paste(input$rel, "in",input$rel2),
        y =input$scale
      ) + theme(
        axis.text.x = element_text(angle = 90)
      ) 
  })
 
  output$scatter<-renderPlot({
    
              ggplot(df) + geom_point(mapping = aes(x = get(input$scale1), y = get(input$scale2), color = family))+
                labs(x = input$scale1,
                  y = input$scale2,
                  title = paste(input$scale1,input$scale2),
                  caption = "Source: POPPA",
                  color = "Parties"
                  )+if(input$pname=="On"){
                    geom_text(aes(x = get(input$scale1), y = get(input$scale2),label=party), size=2,hjust=2)}
                })
  
          }


shinyApp(ui=ui_full,server=server_full)
