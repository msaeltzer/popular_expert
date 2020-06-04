library(shiny)
library(tidyverse)
library(ggplot2)
library(readstata13)
library(sjmisc)
library(sjlabelled)
library(plotly)
library(RColorBrewer)

# to do: 
  # fill disclaimers
  # test on other devices
  # fix the html tags
  # add features to barplot?

find<-function(df,x){df[,which(names(df)==x)]}




# dataset prep and import

df<-read.csv("https://raw.githubusercontent.com/msaeltzer/popular_expert/master/party_means.csv")
df<-df[,-1]
var<-read.csv("https://raw.githubusercontent.com/msaeltzer/popular_expert/master/vars.csv",header=F,stringsAsFactors = F,sep=",")

df$country_id <- to_factor(df$country_id)

df$party_id <- to_label(df$party_id)

df$family <- to_factor(df$family)

levels(df$family) <- c("Radical Right", "Conservatives", "Liberal", "Christian Democratic", "Social Democratic", "Radical Left", "Green", 
                       "Regionalist", "No Family", "Confessional", "Agrarian/ Center")


## change the names of the variables

names(df)[1]<-"Country"
names(df)[6]<-"Party_Family"

names(df)[c(9:24,27)]<-paste(toupper(names(df)[c(9:24,27)]),"-",var$V3)
 

df<-df[,c(1:8,27,16:24,14:15,9:13,25:26)]



# we create a page with tabs: navbar page

ui_full<-navbarPage(
  
# Top Layout
    
  # html text for the page 
      windowTitle = "Poppa - Populism and Political Parties Expert Survey",   

      #add a picture  
        title = div(
          div(
            id = "img-id",
            img(src = "http://poppa-data.eu/wp-content/uploads/2019/07/cropped-mediumsmall-res-3.png",                   
                height = 60,
                width = 120,
                style = "margin:7px 10px"
            ),
          "Populism and Political Parties Expert Survey 2018"  
          ),
        ),
  
          # tab panels!
           
                    tabPanel("Scatterplot",                        # we begin a tab
                    
                    titlePanel("             "),    # give it a title
                    
                    sidebarLayout(                           # we choose a sidebar layout 
                       
                      sidebarPanel(                           # control panel
                        # We need three inputs so far: two scale selections and a on/off button
                                                              # we choose from the scale varnames        
                        selectInput('scale1', 'X-Axis', names(df)[c(9:25)],selected=names(df)[27]),
                        selectInput('scale2', 'Y-Axis', names(df)[c(9:25)],selected=names(df)[24])
                        ,checkboxInput('pname', 'Party',value=F)
                      #  ,checkboxInput('cname', 'Country',value=F)
                        ,checkboxGroupInput('country', 'Select Country', choices = c("All",levels(df$Country)), selected = "All")
                        
                        
                        
                                     #                 
                                     ),
                        
                
                      
                      mainPanel(                              # plot panel
                        plotlyOutput("scatter")
                      ) 
                      
                 ) # end layout
              
           ), # end tab
           
                
        tabPanel("Information",
         
              h3("The Authors"),
              
                 p(
                   a("Maurits J. Meijers",href="http://maurits-meijers.eu/"),
                   span("is an assistant professor in political science at Radboud University.")
                  ),
                 
                 p(
                   a("Andrej Zaslove",href="https://www.ru.nl/english/people/zaslove-a/"),
                 span("is an associate professor in political science at Radboud University.")
                  ),
                 
                 p(
                   span("Please feel free to access "),
                   a("the data",href="https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/8NEL7B"),
                   span("and the"),
                   a("corresponding paper",href="http://maurits-meijers.eu/wp-content/uploads/2020/04/Meijers_Zaslove_2020_CPS_preprint.pdf")
                   )
    
              ) # end tab panel          
                 
    ) # end UI
  
# we define what the server returns for input
server_full <- function(input, output) {
  
  # plot tab 1


    #plot tab 2
              output$scatter<-renderPlotly({
                tit1<-strsplit(input$scale1,"-")[[1]][1] # only use the first term of the scale title
                tit2<-strsplit(input$scale2,"-")[[1]][1]
                                                                                                                  # add duplication for 1. the text BOXES and 2. the on/off      
                d <- data.frame(x = find(df,input$scale1), y = find(df,input$scale2), Country = as.character(df$Country),Coun = as.character(df$Country), Party_family = df$Party_Family,Party_name=df$party,Pname=df$party)
                 
                # on/off party
                 if(input$pname==F){
                 d$Pname <- ""
               }
                
                # select Country
               if(!"All" %in% input$country){
                dd<-subset(d,Country%in%input$country)}else{dd<-d}
                
               pals<-brewer.pal(9,"Paired")
                 
               # define characteristics of text axis labels
               f <- list(
                  size = 18,
                  color = "#7f7f7f"
                )
                xlab <- list(title = trimws(tit1), titlefont = f)
                ylab <- list(title = trimws(tit2), titlefont = f)            
               # begin plotly figure
                fig <- plot_ly(
                  dd, x = ~x, y = ~ y, color = ~ Party_family, text = ~ Party_name, colors=pals, height = 700, width = 900
                ) %>% add_markers() %>% layout(xaxis = list(range=c(0,10))) %>% layout(yaxis = list(range=c(0,10))) %>% layout(xaxis = xlab, yaxis = ylab) %>% layout(plot_bgcolor='#f2f2f2') %>% add_text(text = ~ Pname, textposition = "top right", showlegend = F) %>%  layout(margin = list(b=160),annotations = list(x = 1, y = .01,text="Source: Populism and Political Parties Expert Survey, www.poppa-data.eu",showarrow=F, xref='paper', yref='paper', 
                                                                                                                                                                                                                                                                                                                           xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=10)))
                })         
          } # end server
# run App
shinyApp(ui=ui_full,server=server_full)
