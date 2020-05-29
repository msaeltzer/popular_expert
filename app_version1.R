library(shiny)
library(tidyverse)
library(ggplot2)
library(readstata13)
library(sjmisc)
library(sjlabelled)
library(plotly)

find<-function(df,x){df[,which(names(df)==x)]}

## test functions
input<-list()
input$scale1<-"populism"
input$scale2<-"populism"
input$country2<-"AU"


# dataset prep and import

df<-read.csv("https://raw.githubusercontent.com/msaeltzer/popular_expert/master/party_means.csv")
df<-df[,-1]
var<-read.csv("https://raw.githubusercontent.com/msaeltzer/popular_expert/master/vars.csv",header=F,stringsAsFactors = F,sep="\t")

df$country_id <- to_factor(df$country_id)

df$party_id <- to_label(df$party_id)

df$family <- to_factor(df$family)

levels(df$family) <- c("Radical Right", "Conservatives", "Liberal", "Christian Democratic", "Socialist", "Radical Left", "Green", 
                       "Regionalist", "No Family", "Confessional", "Agrarian/ Center")



## Each shiny app needs a user interface and a server

# user interface
names(df)[1]<-"Country"
names(df)[6]<-"Party_Family"

names(df)[c(9:24,27)]<-paste(toupper(names(df)[c(9:24,27)]),"-",var$V2)
PAGE_TITLE<-"Expert Survey on Populism"

# we create a page with tabs: navbar page
ui_full<-navbarPage(
  
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
  
  
           
                    tabPanel("Scatterplot",                        # we begin a tab
                    
                    titlePanel("             "),    # give it a title
                    
                    sidebarLayout(                           # we choose a sidebar layout 
                       
                      sidebarPanel(                           # control panel
                        # We need three inputs so far: two scale selections and a on/off button
                                                              # we choose from the scale varnames        
                        selectInput('scale1', 'X-Axis', names(df)[c(9:24,27)],selected=names(df)[27]),
                        selectInput('scale2', 'Y-Axis', names(df)[c(9:24,27)],selected=names(df)[24])
                        ,checkboxInput('pname', 'Party',value=F)
                        ,checkboxInput('cname', 'Country',value=F)
                        ,selectInput('country', 'Select Country',c("All",levels(df$Country))
                        ,textOutput("scale1t")
                        #,textOutput("scale2t")
                        
                                     #                 
                                     )
                        
                        ),
                      
                      mainPanel(                              # plot panel
                        plotlyOutput("scatter")
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
                                    choices = c("Country","Party_Family"))
                        ),
                      
                      mainPanel( # plot panel
                        plotOutput("bar")
                      #   textOutput("scale2t")
                        
              
                    )
                      
                 ) # end layout
              
            ),  # end tab
        
tabPanel("Information",
 
         h3("The Authors"),
         p(a("Maurits J. Meijers",href="http://maurits-meijers.eu/"),
         span("is an assistant professor in political science at Radboud University.")),
         
         p(a("Andrej Zaslove",href="https://www.ru.nl/english/people/zaslove-a/"),
         span("is an associate professor in political science at Radboud University.")),
         
         p(span("Please feel free to access "),
         a("the data",href="https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/8NEL7B"),
         span("and the"),
         a("corresponding paper",href="http://maurits-meijers.eu/wp-content/uploads/2020/04/Meijers_Zaslove_2020_CPS_preprint.pdf"))
)           
           
    ) # end UI
  

input$scale1<-names(df)[27]

# we define what the server returns for input
server_full <- function(input, output) {
  
  # plot tab 1
  output$bar<-renderPlot({
                                    # input: what to group by                     input: what scale to display    
    ggplot(data= df %>% group_by(rel=get(input$group)) %>% summarise(scale = mean(get(input$scale), na.rm = TRUE))) + 
      geom_bar(mapping = aes(x = reorder(rel,-scale, FUN = mean), y=scale), fill= 'steelblue', stat=
                 'identity') +
      labs(
        title = paste0(strsplit(input$scale,"-")[[1]]," per ", input$group), # paste title from inputs
        x =paste(input$group),  # paste x axis labelfrom group 
        y =strsplit(input$scale,"-")[[1]],       # paste y axis label by scale
        caption = "Source: POPPA"
      ) + theme(
        axis.text.x = element_text(angle = 90)
      ) 
  })

    #plot tab 2
              output$scatter<-renderPlotly({
                tit1<-strsplit(input$scale1,"-")[[1]][1]
                tit2<-strsplit(input$scale2,"-")[[1]][1]
                d <- data.frame(x = find(df,input$scale1), y = find(df,input$scale2), Country = as.character(df$Country),Coun = as.character(df$Country), Party_family = df$Party_Family,Party_name=df$party,Pname=df$party)
                 
                 if(input$pname==F){
                 d$Pname <- ""
               }
                
                if(input$cname==F){
                  d$Coun <- ""
                }
                if(input$country!="All"){
                dd<-subset(d,Country==input$country)}else{dd<-d}
                
            
                 
               pals <- c('brown','black','gold1','blue','red','dark red','green','dark blue',"grey","purple","dark green")
               pals <- setNames(pals, levels(df$family))
                f <- list(
                  size = 18,
                  color = "#7f7f7f"
                )
                xlab <- list(title = trimws(tit1), titlefont = f)
                ylab <- list(title = trimws(tit2), titlefont = f)            
                fig <- plot_ly(
                  dd, x = ~x, y = ~ y, color = ~ Party_family, text = ~ Party_name, colors = pals
                ) %>% add_markers() %>% layout(xaxis = xlab, yaxis = ylab) %>% add_text(text = ~ Coun, textposition = "top right", showlegend = F) %>% add_text(text = ~ Pname, textposition = "top right", showlegend = F)})         
 
          }


shinyApp(ui=ui_full,server=server_full)
