# Load R packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(shinycssloaders)
library(shinyjs)
library(scales)

#LOAD DATA

df <- read_csv("julydata.csv")


#UI

#NAVIGATION BAR
  ui <- function(request){
    
    
    fluidPage(tags$head(includeHTML(("google-analytics.html"))),
              theme = shinytheme("cyborg"),
    useShinyjs(),
    
    
    #BUILDING NAV BAR
    
    navbarPage("TROTUP",
               header =         
                 tags$head(
                   #Cookie warning
                   cookie_box <- div(class="alert alert-info", style = "margin-bottom: 0",
                                     "This site uses cookies from Google to deliver its services and to analyse traffic. Your IP address and user agent are shared with Google, together with performance and security metrics, to ensure quality of service, generate usage statistics and to detect and address abuse.",
                                     tags$a(href='https://policies.google.com/technologies/cookies',
                                            " LEARN MORE"),
                                     HTML('<a href="#" class="close" data-dismiss="alert" aria-label="close">DISMISS</a>'))),
       
      #MAIN PAGE 
      
      tabPanel("CALCULATE",
               
               #CALCULATION PANEL
               
               sidebarPanel(
                 div(h4("INPUT AN AGE AND HEIGHT RANGE:", tags$img(src = "trotuplogosmall.svg", height = "30px", width= "40px"))),
                 selectInput("minhght", "Minimum Height (hands high):", choices = c(0, 10.0, 10.1, 10.2, 10.3, 11.0, 11.1, 11.2, 11.3, 12.0, 12.1, 12.2, 12.3, 13.0, 13.1, 13.2, 13.3, 14.0, 14.1, 14.2, 14.3, 15.0, 15.1, 15.2, 15.3, 16.0, 16.1, 16.2, 16.3, 17.0, 17.1, 17.2, 17.3, 18.0, 18.1, 18.2, 18.3, 19.0), selected = "14.2" ),
                 selectInput("maxhght", "Maximum Height (hands high):", choices = c(0, 10.0, 10.1, 10.2, 10.3, 11.0, 11.1, 11.2, 11.3, 12.0, 12.1, 12.2, 12.3, 13.0, 13.1, 13.2, 13.3, 14.0, 14.1, 14.2, 14.3, 15.0, 15.1, 15.2, 15.3, 16.0, 16.1, 16.2, 16.3, 17.0, 17.1, 17.2, 17.3, 18.0, 18.1, 18.2, 18.3, 19.0), selected = "16.2"),
                 sliderInput("ageid", "Age (years):", 0, 20, c(5,10)),
                 tags$head(tags$style(HTML("
                                .btn {
                                color:rgb(255,255,255);
                                text-align: left;
                                #border-color:rgb(0,144,197);
                                background-color:rgb(0,144,197);}

                                # #gobutton:active {
                                # background: green;
                                # }

                                .btn:hover{
                                #border-color:rgb(232,245,251);
                                background-color: rgb(232,245,251);color:   rgb(0,144,197);font-weight: bold;
                                }
                                .btn:focus{
                                background-color:green;
                                }"))),
                 div(actionButton("do", "GO!"), style= "color:blue"),
                 br(),
                 
                 #TIPS FOR SEARCHING 
                 
                 tags$h5("TROTUP TIPS:"),
                 
                 #WARNING THAT APPEARS FOR INVALID SEARCHES
      
                 div(em((helpText(
                   
                  em(div(textOutput("noresultmessage"), style = "color:red")),

                  br(),
                
                  "Young horses often have no height advertised, so set 'min height' to 0 hh")), 
                  
                  "Experience, breeding, and temperament will also affect a horse's value",
                  
                  style = "color:white"))
               
               ), 
      
        #MAIN PANEL
        
                mainPanel(
                            h2("TROTUP", align = "center"),
                            
                            h2(tags$img(src = "trotuplogosmall.svg"), align= "center"),
                      
                            em(h5(div("VALUE A HORSE FOR BUYING, SELLING, INSURING, OR JUST FOR FUN", align = "center", style = "color:gray"))),
                            
                            br(),
                            br(),
                            
                            #RESULT

                            mainPanel(h3(strong(div("MEDIAN PRICE (£): ", style = "color:white"), div(textOutput("view"), style = "color:lightblue"))), align = "left"),
                          
                            br(),
                            
                            #WARNING MESSAGE IF SMALL NUMBER SEARCH RESULTS
                            
                            em(strong(div(textOutput("counter"), style = "color:red"))),

                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            
                            #BOOKMARK BUTTON TO SHARE RESULT
                            
                            bookmarkButton("SHARE THIS TROTUP", title = "SHARE YOUR RESULTS"),
                            
                            br(),
                            br(),
                            
                            #MESSAGE FOR USER TO CHECK OUT THE 'STATS FOR NERDS' PAGE
                            
            em(strong(div(textOutput("morestats"), style = "color:gray"))),
          
                          
                            br(),
                            br(),
                            br(),
                            br(),
                            br()
               )), 
    
      
    # OTHER TABS IN THE NAVIGATION BAR
    
    #STATS FOR NERDS PAGE
    
    tabPanel("STATS FOR NERDS", 
             sidebarPanel(h4(textOutput("usersearch")),
                          div("TROTUP TIPS:", style = "color:gray"),
                          
                          #MESSAGE TELLING USER TO INPUT A VALID SEARCH TO SEE A GRAPH
                          em(h5(div(strong(textOutput("nopricespread")), style= "color:red"))),
                          
                          #EXPLANATION OF DISTRIBUTION AND UPPER AND LOWER QUARTILE PRICES
                          em(div(strong(textOutput("uqval")), style = "color:white"), div(tags$li("75% of all advertised horses in your search cost less than this. Horses priced above this might have a great competition record, excellent breeding, or they might be overpriced!")), style = "color:lightblue"),
                          br(),
                          em(div(strong(textOutput("lqval")), style = "color:white"), div(tags$li("25% of all advertised horses in your search cost less than this. Horses below this might have a vice, a health condition, less experience, or be priced for a quick sale.")), style = "color:lightblue"),
                          ),
#MAIN PANEL WITH PLOT
             mainPanel(
             br(), 
             div(
               style = "position:relative", plotOutput("pricespread") %>% withSpinner()
             ),
             br())
    ),
    
#ABOUT TAB
      tabPanel("ABOUT THE INDEX", 
               
               h4(div("WHAT IS TROTUP?", tags$img(src = "trotuplogosmall.svg", height = "30px", width= "40px"), style= "color:lightblue")),
               
               p(h5(div(strong("Trotup is a data-driven, fully customisable index that estimates how much a horse is worth."), br(), br(), 
                        "The horse market is confusing- have you ever scrolled through hundreds of adverts trying to value a horse? Felt overwhelmed whilst bartering? Wondered if you're overpaying for a youngster or if now is a good time to buy?", 
                        strong(em("Until now, there has been no way to value a horse beyond intuition and gut-feeling."),
                        "Trotup is our answer to that! We analyse the UK equine market so you don't have to."), style = "color:white"))),
               
              br(),
               
              h4(div("WHY WOULD I VALUE MY HORSE? SHE'S PRICELESS!", tags$img(src = "trotuplogosmall.svg", height = "30px", width= "40px"), style = "color:lightblue")),
              
            
              p(h5("Knowing your horse’s market value is crucial for selling, insuring, and buying. It can be hard to put a price on your horse, especially if you've owned them for a long time in a changing economy. Trotup removes the guesswork by using real-world data. ")),

              br(),
              
              h4(div("THE BORING STUFF", tags$img(src = "trotuplogosmall.svg", height = "30px", width= "40px"), style= "color:lightblue")),
              
              h5("To learn more about how we trotup up, take a look at: ",
                 tags$li("our Terms of Use", tags$a(href="TOU.pdf", "here", target="_blank")),
                 tags$li("More information about cookies", tags$a(href="https://policies.google.com/technologies/cookies", "here", target="_blank")), 
                br())),
    
#CONNECT WITH TROTUP TAB      

    tabPanel("CONNECT WITH TROTUP",
        
            div(h4(strong("TROTUP TOGETHER"), tags$img(src = "trotuplogosmall.svg", height = "30px", width= "40px"), style = "color:white")),
            br(),
            h2(tags$a(href="https://twitter.com/trot_up", icon("twitter"), target="_blank"), 

               tags$a(href="https://www.facebook.com/Trotup-104946387960501", icon("facebook"), target = "_blank"),

               tags$a(href="https://www.instagram.com/trotupindex/?hl=en", icon("instagram"), target= "_blank"), 
                  align = "left"),
            br(),
            "follow us on social media for Trotup tips and tricks, and be the first to know when we release new updates and features",
            br(),
            br(),

             p(h5(div("We'd love to hear from you if you've used Trotup!", tags$a(href="mailto:trotuphelp@gmail.com", "Email us", target= "_blank"), "with feedback, or if you have",
                  br(),    
                      tags$ul(
   br(),
    tags$li("features you'd like to see on Trotup"), 
    br(),
  
    tags$li("other business enquiries, or want to work with us!"), 
    br()
   
), style = "color:white"),
    br()                  
                
             
             ))),
  

# WEBSITE FOOTER
 footer = (
   tags$tfoot(div("THE WORLD'S FIRST EQUINE PRICE INDEX, BASED ON DATA FROM THE UK'S LEADING ADVERTISING PLATFORMS", style = "color:lightblue"), 
              tags$a(href="https://twitter.com/trot_up", icon("twitter"), target="_blank"), tags$a(href="https://www.facebook.com/Trotup-104946387960501", icon("facebook"), target = "_blank"), tags$a(href="https://www.instagram.com/trotupindex/?hl=en", icon("instagram"), target= "_blank"), 
              tags$img(src = "trotuplogosmall.svg", height = "30px", width= "40px")))

    ))
}


  # SERVER  
 

  server <- function(input, output,session) {

    #STARTING STATE OF 'STATS FOR NERDS' PAGE: BLANK GRAPH AND MESSAGE TO INPUT SEARCH
    
    output$nopricespread <- renderText({
        paste("ENTER A VALID SEARCH AND CLICK GO!")
 
      })
      output$pricespread <- renderPlot({
        fake <- data.frame()
        ggplot(fake) + geom_point() + xlim(0, 10) + ylim(0, 100) +
          theme(panel.background = element_blank())+
          theme_economist() +
          scale_fill_economist() +
          theme(
            plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5)) 
        
      }) 
      
      #OBSERVE USER INPUTS AND SAVE TO A,B,C,D

      observeEvent(input$do, {
      odf <- df
      A<-input$minhght
      B<-input$maxhght
      C<-input$ageid[1]
      D<-input$ageid[2]

      #FILTER THE DF ACCORDING TO THESE AND CULL OUTLIERS
      
      reactingdf <- odf %>%
        filter(df$height >= A & odf$height <= B & odf$age >= C & odf$age <= D)
      
      filtered_rdf <- reactingdf %>% 
        filter(reactingdf$price >= 100 & reactingdf$price <= 25000)
      
      median <- median(filtered_rdf$price)
      
      samplesize<- nrow(filtered_rdf)
      

      #RETURN MEDIAN PRICE IF POSSIBLE, OTHERWISE, APPROPRIATE WARNING MESSAGE
      
      warningmessage <- ifelse(samplesize <= 20 & B >= A, "WARNING, SMALL SAMPLE! WIDEN YOUR SEARCH?", "")
      
      median_clean <- ifelse(is.na(median) == TRUE, "NO RESULTS!", median)
      
      noresultmessage <- ifelse(A > B, "IS YOUR MIN. HEIGHT LARGER THAN YOUR MAX?", "")
      
      delay(500, output$view <- renderText({
        paste(median_clean)
      }))
      
      output$counter <- renderText({
        paste(warningmessage)
      
        
      })
      
      #STATS FOR NERDS PAGE: PRINT USER SEARCH OR TELL TO ENTER VALID SEARCH
      
      output$noresultmessage <- renderText({
        paste(noresultmessage)
        
      })
      
      output$usersearch <- renderText({
        paste("YOUR SEARCH:", A, "-", B, "HH,", C, "-", D, "YO:")
        

      })
      
      #RENDER GRAPH AND EXPLANATION IF VALID AND LARGE ENOUGH QUERY ENTERED, OTHERWISE BLANK GRAPH AND MESSAGE
      
        #NOTHING
      if(is.na(median) == TRUE){ 
        output$nopricespread <- renderText({
        paste("ENTER A VALID SEARCH TO SEE A GRAPHICAL REPRESENTATION")
        })
        
        output$pricespread <- renderPlot({
          fake <- data.frame()
          ggplot(fake) + geom_point() + xlim(0, 10) + ylim(0, 100) +
            theme(panel.background = element_blank())+
            theme_economist() +
            scale_fill_economist() +
            theme(
              plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5)) 
          
        }) 
        
        #QUARTILES INFO BUT NO GRAPH
      }else if(samplesize < 30){
        output$uqval <- renderText({
          paste("£", quantile(filtered_rdf$price, .75))
          
        })
        
        output$lqval<- renderText({
          paste("£", quantile(filtered_rdf$price, .25))
          
          
        })
        output$nopricespread <- renderText({
          paste("WIDEN YOUR SEARCH TO SEE A GRAPHICAL REPRESENTATION")
        })
        output$pricespread <- renderPlot({
          fake <- data.frame()
          ggplot(fake) + geom_point() + xlim(0, 10) + ylim(0, 100) +
          theme(panel.background = element_blank())+
          theme_economist() +
          scale_fill_economist() +
          theme(
              plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5)) 
          
        })  
          
          }else{
            
            #MESSAGE ON MAIN PAGE TO CHECK OUT GRAPHICAL REPRESENTATION
            
      delay(500, output$morestats <- renderText({
        paste("CLICK 'STATS FOR NERDS' TO LEARN MORE ABOUT THIS SEARCH")
        
        
      }))      
      
      
      output$nopricespread <- renderText({ paste("")})
      
      #HISTOGRAM OF PRICE DIST
        
      output$pricespread <- 
  renderPlot({ 
    ggplot(filtered_rdf, 
         aes(x=filtered_rdf$price)) + 
         geom_histogram(color = "lightblue", fill = "lightblue", alpha = 0.9) +
      theme_economist() +
      scale_fill_economist() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))+
      
      #VERTICAL LINES FOR MEAN MEDIAN AND QUARTILES 
      
      geom_vline(aes(xintercept=median(filtered_rdf$price),
                    color="MEDIAN"), linetype="dashed",
                 size=1) +
      geom_vline(aes(xintercept=mean(filtered_rdf$price),
                     color="MEAN"), linetype="dashed",
                 size=1) +
      geom_vline(aes(xintercept=quantile(filtered_rdf$price, .25),
                     color="25%"), linetype="dashed",
                 size=1) +
      geom_vline(aes(xintercept=quantile(filtered_rdf$price, .75),
                     color="75%"), linetype="dashed",
                 size=1) +
      scale_color_manual(name = "STATS", values = c('75%' = "ORANGE",'MEAN' = "red",'MEDIAN' = "darkblue", '25%' = 'yellow')) +
         ggtitle("DISTRIBUTION OF PRICES") +
      theme(legend.position = c(.8, .8)) +
      labs(x = "PRICE (THOUSANDS OF £)", y = "COUNT") + 
        scale_x_continuous(label=scales::label_dollar(scale = .001, prefix = "  ", suffix = "K"), breaks = seq(0, max(filtered_rdf$price), by = 1000)) + 
        coord_cartesian((x= c(2500,25000)))
        }
    )
    
      #WRITTEN EXPLANATIONS OF THE QUARTILE VALUES
      
    output$uqval <- renderText({
      paste("£", quantile(filtered_rdf$price, .75))
      
      })

    output$lqval<- renderText({
      paste("£", quantile(filtered_rdf$price, .25))
      
    })

      }
  }) 
      
  }

    # CREATE APP
    shinyApp(ui = ui, server = server, enableBookmarking = "url")
    
      
      
     
    

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    

    

    
 