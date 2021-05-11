library(shiny)
library(wordcloud2)
library(rvest)

default_neighbor <- "https://en.wikipedia.org/wiki/Cola"
options(shiny.sanitize.errors = TRUE) 

ui <- fluidPage(

    # Application title
    titlePanel("Wiki Word Cloud"),

    # Text input field for Wikipedia URL
    sidebarLayout(
        sidebarPanel(
            textInput("wikiurl","Enter the Wikipedia article URL"),
            actionButton("getcloud","Get Word Cloud"),
            actionButton("inclneighbors", "☐ Include Neighbors")
        ),

    # Output field for plot
        mainPanel(
            wordcloud2Output('wordcloud2')
        )
    )
)

server <- function(input, output, session) {
        observeEvent(input$getcloud, {
            url <- input$wikiurl
            
            tryCatch(
                {text <- html_text(read_html(url) %>% html_nodes("p"))},
                error=function(errorCondition) {
                    message(errorCondition)
                },
                warning=function(warningCondition) {
                    message(warningCondition)
                },
                finally={message("Invalid Wikipedia Article URL")}
            )
            validate (
                need(text!='',"Please enter a valid Wikipedia article URL")
            )
            text <- gsub("[[:punct:]]", '', text)
            text <- gsub("[[:digit:]]", '', text)
            text <- gsub("\n","",gsub("\t","",text))
            text <- tolower(text)
            text <- removeWords(text,stopwords("en"))
            counts <- as.data.frame(table(unlist( strsplit(text, "\ ") )))
            counts <- with(counts, counts[ Var1 != "", ] )
            output$wordcloud2 <- renderWordcloud2({
                wordcloud2(counts)
            })  
        })
        observeEvent(input$inclneighbors, {
            neighbor_text <- html_text(read_html("https://en.wikipedia.org/wiki/Cola") %>% html_nodes("p"))
            neighbor_text <- gsub("[[:punct:]]", '', neighbor_text)
            neighbor_text <- gsub("[[:digit:]]", '', neighbor_text)
            neighbor_text <- gsub("\n","",gsub("\t","",neighbor_text))
            neighbor_text <- tolower(neighbor_text)
            neighbor_text <- removeWords(neighbor_text,stopwords("en"))
            text <- paste(text, neighbor_text, sep=" ")
            counts <- as.data.frame(table(unlist( strsplit(text, "\ ") )))
            counts <- with(counts, counts[ Var1 != "", ] )
            output$wordcloud2 <- renderWordcloud2({
                wordcloud2(counts)
            })
            
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
