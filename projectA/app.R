library(shiny)
library(ggplot2)
library(tidyverse)
library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)
library(psych)
library(rsconnect)

# Data
api <- "FGo4ZHuPDMcADDGSRC1ZjXGUk"
apiSecret <- "m4zWMREha1fUePelhsck60CQMuO8qKhQucObPycSXvLNneXW5o"
access <- "1243552285424340994-jeL8kxj8zRP95M9khVPCptH6NaWp7m"
accessSecret <- "mMoP3SJ2dGY67j1yT9CLLeXTQ60X48ThtHjq1nJOPds6O"
setup_twitter_oauth(api, apiSecret,access,accessSecret)

set.seed(511)
imported_covid <- searchTwitter("#COVID", 500)
imported_covid19 <- searchTwitter("#COVID19", 500)
imported_covid_19 <- searchTwitter("#COVID-19", 500)
imported_covid_19_ <- searchTwitter("#COVID_19", 500)

# COVID
imported_covid_tbl <- twListToDF(imported_covid) %>%
    dplyr::filter(isRetweet == F)
imported_covid_tbl$text <- imported_covid_tbl$text %>% 
    iconv("UTF-8", "ASCII", sub="")
twitter_covid_cp <- VCorpus(VectorSource(imported_covid_tbl))
# preprocessed lemmas
stops_covid <- c(stopwords(kind = 'en'), '#covid', '#covid19', '#covid-19', 
                 '#covid_19', 'covid','false', 'true',
                 # Spanish words
                 'que', 'los', 'les', 'del', 'las', 'para', 'las', 'por', 'con')
removeURL <- function(x) {
    gsub("http.*", "", x)
    gsub("href.*", "", x)}
twitter_covid_cp <- tm_map(twitter_covid_cp, PlainTextDocument)
twitter_covid_cp <- tm_map(twitter_covid_cp, content_transformer(str_to_lower))
twitter_covid_cp <- tm_map(twitter_covid_cp, removeNumbers)
twitter_covid_cp <- tm_map(twitter_covid_cp, removePunctuation)
twitter_covid_cp <- tm_map(twitter_covid_cp, removeWords, stops_covid)
twitter_covid_cp <- tm_map(twitter_covid_cp, stripWhitespace)
twitter_covid_cp <- tm_map(twitter_covid_cp, content_transformer(removeURL))
twitter_covid_dtm <- DocumentTermMatrix(twitter_covid_cp)
# eliminate sparse terms
twitter_covid_slimmed <- removeSparseTerms(twitter_covid_dtm, .99)
tokenCounts_covid <- apply(twitter_covid_slimmed, 1, sum)
twitter_covid_cleaned_dtm <- twitter_covid_slimmed[tokenCounts_covid > 0,]
twitter_covid_tbl <- as_tibble(as.matrix(twitter_covid_cleaned_dtm))


# COVID19
imported_covid19_tbl <- twListToDF(imported_covid19) %>%
    dplyr::filter(isRetweet == F)
imported_covid19_tbl$text <- imported_covid19_tbl$text %>% 
    iconv("UTF-8", "ASCII", sub="")
twitter_covid19_cp <- VCorpus(VectorSource(imported_covid19_tbl))
# preprocessed lemmas
twitter_covid19_cp <- tm_map(twitter_covid19_cp, PlainTextDocument)
twitter_covid19_cp <- tm_map(twitter_covid19_cp, content_transformer(str_to_lower))
twitter_covid19_cp <- tm_map(twitter_covid19_cp, removeNumbers)
twitter_covid19_cp <- tm_map(twitter_covid19_cp, removePunctuation)
twitter_covid19_cp <- tm_map(twitter_covid19_cp, removeWords, stops_covid)
twitter_covid19_cp <- tm_map(twitter_covid19_cp, stripWhitespace)
twitter_covid19_cp <- tm_map(twitter_covid19_cp, content_transformer(removeURL))
twitter_covid19_dtm <- DocumentTermMatrix(twitter_covid19_cp)
# eliminate sparse terms
twitter_covid19_slimmed <- removeSparseTerms(twitter_covid19_dtm, .99)
tokenCounts_covid19 <- apply(twitter_covid19_slimmed, 1, sum)
twitter_covid19_cleaned_dtm <- twitter_covid19_slimmed[tokenCounts_covid19 > 0,]
twitter_covid19_tbl <- as_tibble(as.matrix(twitter_covid19_cleaned_dtm))

# COVID-19
imported_covid_19_tbl <- twListToDF(imported_covid_19) %>%
    dplyr::filter(isRetweet == F)
imported_covid_19_tbl$text <- imported_covid_19_tbl$text %>% 
    iconv("UTF-8", "ASCII", sub="")
twitter_covid_19_cp <- VCorpus(VectorSource(imported_covid_19_tbl))
# preprocessed lemmas
twitter_covid_19_cp <- tm_map(twitter_covid_19_cp, PlainTextDocument)
twitter_covid_19_cp <- tm_map(twitter_covid_19_cp, content_transformer(str_to_lower))
twitter_covid_19_cp <- tm_map(twitter_covid_19_cp, removeNumbers)
twitter_covid_19_cp <- tm_map(twitter_covid_19_cp, removePunctuation)
twitter_covid_19_cp <- tm_map(twitter_covid_19_cp, removeWords, stops_covid)
twitter_covid_19_cp <- tm_map(twitter_covid_19_cp, stripWhitespace)
twitter_covid_19_cp <- tm_map(twitter_covid_19_cp, content_transformer(removeURL))
twitter_covid_19_dtm <- DocumentTermMatrix(twitter_covid_19_cp)
# eliminate sparse terms
twitter_covid_19_slimmed <- removeSparseTerms(twitter_covid_19_dtm, .99)
tokenCounts_covid_19 <- apply(twitter_covid_19_slimmed, 1, sum)
twitter_covid_19_cleaned_dtm <- twitter_covid_19_slimmed[tokenCounts_covid_19 > 0,]
twitter_covid_19_tbl <- as_tibble(as.matrix(twitter_covid_19_cleaned_dtm))

# COVID_19
imported_covid_19__tbl <- twListToDF(imported_covid_19_) %>%
    dplyr::filter(isRetweet == F)
imported_covid_19__tbl$text <- imported_covid_19__tbl$text %>% 
    iconv("UTF-8", "ASCII", sub="")
twitter_covid_19__cp <- VCorpus(VectorSource(imported_covid_19__tbl))
# preprocessed lemmas
twitter_covid_19__cp <- tm_map(twitter_covid_19__cp, PlainTextDocument)
twitter_covid_19__cp <- tm_map(twitter_covid_19__cp, content_transformer(str_to_lower))
twitter_covid_19__cp <- tm_map(twitter_covid_19__cp, removeNumbers)
twitter_covid_19__cp <- tm_map(twitter_covid_19__cp, removePunctuation)
twitter_covid_19__cp <- tm_map(twitter_covid_19__cp, removeWords, stops_covid)
twitter_covid_19__cp <- tm_map(twitter_covid_19__cp, stripWhitespace)
twitter_covid_19__cp <- tm_map(twitter_covid_19__cp, content_transformer(removeURL))
twitter_covid_19__dtm <- DocumentTermMatrix(twitter_covid_19__cp)
# eliminate sparse terms
twitter_covid_19__slimmed <- removeSparseTerms(twitter_covid_19__dtm, .99)
tokenCounts_covid_19_ <- apply(twitter_covid_19__slimmed, 1, sum)
twitter_covid_19__cleaned_dtm <- twitter_covid_19__slimmed[tokenCounts_covid_19_ > 0,]
twitter_covid_19__tbl <- as_tibble(as.matrix(twitter_covid_19__cleaned_dtm))


# wordcloud - COVID
wordCounts_covid <- colSums(twitter_covid_tbl)
wordNames_covid <- names(twitter_covid_tbl)

# wordcloud - COVID19
wordCounts_covid19 <- colSums(twitter_covid19_tbl)
wordNames_covid19 <- names(twitter_covid19_tbl)

# wordcloud - COVID-19
wordCounts_covid_19 <- colSums(twitter_covid_19_tbl)
wordNames_covid_19 <- names(twitter_covid_19_tbl)

# wordcloud - COVID_19
wordCounts_covid_19_ <- colSums(twitter_covid_19__tbl)
wordNames_covid_19_ <- names(twitter_covid_19__tbl)

# summary 1: pairwise comparison

# create count tokens and filter for those with more than 5 mentions
twitter_covid_tbl_tokens <- data.frame(wordNames_covid, wordCounts_covid) %>%
    as_tibble %>%
    rename(name  = wordNames_covid,
           count = wordCounts_covid) %>%
    mutate(name = as.character(name)) 

twitter_covid19_tbl_tokens <- data.frame(wordNames_covid19, wordCounts_covid19) %>%
    as_tibble %>%
    rename(name  = wordNames_covid19,
           count = wordCounts_covid19) %>%
    mutate(name = as.character(name)) 

twitter_covid_19_tbl_tokens <- data.frame(wordNames_covid_19, wordCounts_covid_19) %>%
    as_tibble %>%
    rename(name  = wordNames_covid_19,
           count = wordCounts_covid_19) %>%
    mutate(name = as.character(name)) 

twitter_covid_19__tbl_tokens <- data.frame(wordNames_covid_19_, wordCounts_covid_19_) %>%
    as_tibble %>%
    rename(name  = wordNames_covid_19_,
           count = wordCounts_covid_19_) %>%
    mutate(name = as.character(name))

# create overlap strings
COVID_COVID19 <- twitter_covid_tbl_tokens %>% 
    inner_join(twitter_covid19_tbl_tokens, by = "name", suffix = c("_COVID", "_COVID19")) %>%
    filter_at(vars(starts_with("count")), ~ . >= 5)
COVID_COVID_19 <- twitter_covid_tbl_tokens %>% 
    inner_join(twitter_covid_19_tbl_tokens, by = "name", suffix = c("_COVID", "_COVID-19")) %>%
    filter_at(vars(starts_with("count")), ~ . >= 5)
COVID_COVID__19 <- twitter_covid_tbl_tokens %>% 
    inner_join(twitter_covid_19__tbl_tokens, by = "name", suffix = c("_COVID", "_COVID_19")) %>%
    filter_at(vars(starts_with("count")), ~ . >= 5)

COVID19_COVID_19 <- twitter_covid19_tbl_tokens %>% 
    inner_join(twitter_covid_19_tbl_tokens, by = "name", suffix = c("_COVID19", "_COVID-19")) %>%
    filter_at(vars(starts_with("count")), ~ . >= 5)
COVID19_COVID__19 <- twitter_covid19_tbl_tokens %>% 
    inner_join(twitter_covid_19__tbl_tokens, by = "name", suffix = c("_COVID19", "_COVID_19")) %>%
    filter_at(vars(starts_with("count")), ~ . >= 5)

COVID_19_COVID__19 <- twitter_covid_19_tbl_tokens %>% 
    inner_join(twitter_covid_19__tbl_tokens, by = "name", suffix = c("_COVID-19", "_COVID_19")) %>%
    filter_at(vars(starts_with("count")), ~ . >= 5)

# count overlaps
summary_comparison <- tibble(
    pair = c("COVID_COVID19", "COVID_COVID_19", 
             "COVID_COVID__19", "COVID19_COVID_19",
             "COVID19_COVID__19", "COVID_19_COVID__19"),
    overlap = c(nrow(COVID_COVID19), nrow(COVID_COVID_19),
                nrow(COVID_COVID__19), nrow(COVID19_COVID_19),
                nrow(COVID19_COVID__19), nrow(COVID_19_COVID__19))
)

# summary bar chart
summary_all <- twitter_covid_tbl_tokens %>%
    inner_join(twitter_covid19_tbl_tokens, by = "name", suffix = c("_COVID", "_COVID19")) %>%
    inner_join(twitter_covid_19_tbl_tokens, by = "name") %>%
    inner_join(twitter_covid_19__tbl_tokens, by = "name") %>%
    mutate(sum = rowSums(select(., starts_with("count")))) %>%
    arrange(desc(sum)) %>%
    top_n(20)

# Twitter hashtags
hashtags <- c("COVID", "COVID19", "COVID-19", "COVID_19")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Project A"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # choose hashtags
            radioButtons("hashtag", "hashtags", choices = hashtags, selected = "COVID")
        ),
        
        # Show a plot and dataset
        mainPanel(
            plotOutput("wordcloud"),
            plotOutput("barchart"),
            DT::dataTableOutput("data")
        )
    )
)

# Define server logic required
server <- function(input, output) {
    # Filter data with reactive
    filtered_data <- reactive({
        if (input$hashtag == "COVID"){
            data <- twitter_covid_tbl
        }
        data
        
        if (input$hashtag == "COVID19"){
            data <- twitter_covid19_tbl
        }
        
        data
        if (input$hashtag == "COVID-19"){
            data <- twitter_covid_19_tbl
        }
        
        data
        if (input$hashtag == "COVID_19"){
            data <- twitter_covid_19__tbl
        }
        data
    })
    
    # Output wordcloud
    output$wordcloud <- renderPlot({ 
        # to create interactive plots. If want to render a static plot, can do it outside renderPlot()
        # renderPlot: ggplot() should be the last function in order for renderPlot to display the plot
        # use filtered data variable to render output
        data <- filtered_data()
        # create scatter plot
        # wordcloud - COVID
        wordCounts <- colSums(data)
        wordNames <- names(data)
        wordcloud(wordNames, wordCounts, max.words = 50, scale=c(4,.2))
    })
    
    output$data <- DT::renderDataTable({
        data <- summary_comparison
        data
    })
    
    output$barchart <- renderPlot({
        data <- summary_all
        ggplot(data, aes(x = name, y = sum)) +
            geom_bar(stat = "identity", fill = "#95C4CB") + 
            coord_flip() + 
            theme_classic()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
