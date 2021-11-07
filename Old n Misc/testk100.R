Eelenas <- readRDS("model_k100_elena (1).rds")
library(LDAvis)
serVis(topicmodels2LDAvis(Eelenas))
servr::daemon_stop(1)

rm(list = ls())

#library(rstudioapi)
#setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))

library(topicmodels)
Eelenas <- readRDS("model_k100_elena (1).rds") #This one we'll use'. 
#It is the model on the full jokes dataset, with K=29 based on the result we just inspected.
Eelenas <- modelBig29
terms(Eelenas ,7) #Have a quick look
#pretty ok, by a quick inspection. Albeit far from perfect.

#Get stuff out of the model, three tables:
#1. Model summary
topicModelSummary100 <- as.data.frame(terms(Eelenas ,15)) #Col = topics, cells = topterms(15)/topic
#2. Topic loadings per doc (gamma), i.e. the strength of the relations between respectiv topics and documents
topicDocProbabilities100 <- as.data.frame(topicmodels::posterior(Eelenas)$topics) #Col = topics, rows = doc_id
#3. Topic loadings per term (beta), i.e. the strength of the relations between respective topics and terms
topicTermProbabilities100 <- as.data.frame(t(topicmodels::posterior(Eelenas)$terms))#Col = terms, rows = topics
#These can be saved as csv and further analysed outside R. It is allowed.

#Let's just enrich the gamma output table slightly first:
colnames(topicDocProbabilities100) <- paste("Topic", colnames(topicDocProbabilities100), sep = "_") #Add "Topic" to the columnames

#Make rownames the first column:
topicDocProbabilities100 <- cbind(rownames(topicDocProbabilities100),
                               data.frame(topicDocProbabilities100,
                                          row.names=NULL)) 

top.topics <- as.data.frame(topics(Eelenas)) #top topics per doc

#marry gamma with toptopics
head(top.topics)
names(top.topics)[1] <- "TopTopic"
topicDocProbabilities100$TopTopic <- top.topics$TopTopic
head(topicDocProbabilities100)
names(topicDocProbabilities100)[1] <- "doc_id" #rename the first column wich holds the doc_id.
#This column is important because it maps to the corresponding column in the file that holds the cleaned texts.

text1 <- readRDS("Text1Full.rds") #Load the the texts/jokes. Pre-cleaned. Thank you. You're wellcome.
str(text1) 
str(topicDocProbabilities100) #This is to see type of data in the different columns...
text1$doc_id <- as.factor(text1$doc_id) #to enable joining, i.e. we want doc_id to be a factor not a number.
library(dplyr) #datawrangling package.
topicDocProbabilities100 <- topicDocProbabilities100 %>% left_join(text1, by = "doc_id") #Don't worry about the warning.
head(topicDocProbabilities100)
saveRDS(topicDocProbabilities100, file = "topicDocProbabilities100.rds") #Save this.  

#Export to csv:
#k <- 29 #For naming
#write.csv(topicModelSummary, file = paste("LDAGibbs",k,"Summary.csv"))
#write.csv(topicDocProbabilities100, file = paste("LDAGibbs",k,"Gamma.csv"))
#write.csv(topicTermProbabilities, file = paste("LDAGibbs",k,"Beta.csv"))

GMY <- "MYA"
GMY



rm(list = ls())

#library(rstudioapi)
#setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))

#We will create an app to interact with out topic model via two outputs,
#topicDocProbabilities100 (gamma) and topic summary. We will enrich the latter a bit.

topicDocProbabilities100 <- readRDS("topicDocProbabilities100.rds")
head(topicDocProbabilities100)
topicDocProbabilities100[, 2:21] <- round(topicDocProbabilities100[, 2:21], 4) #We don't need the full format...

library(topicmodels)
Eelenas <- readRDS("model_k100_elena (1).rds") #we'll use a full-sized model

library(dplyr)
library(tidytext) #tidy text communicates with our model objects
jokesterms <- tidy(Eelenas, matrix = "beta")
head(jokesterms)

topjokes <- jokesterms %>%
  group_by(topic) %>%
  top_n(10, beta) %>% #only top 10 terms (by beta) and their beta
  mutate(rank = order(order(beta, decreasing=TRUE))) %>% #rank them 'within' each topic
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) #have them sorted...

#Let's improve the labels..:
head(topjokes)
str(topjokes)

# Pad topic numbers with 0s before: 01, 02, ..., 09, 10, 11, ... 29. 
topjokes$topic <- stringr::str_pad(string = topjokes$topic, width = 2, side = "left", pad = 0)
topjokes$topic <- paste0("Topic_", topjokes$topic)

saveRDS(topjokes, file = "topjokes.rds") #We'll need this later
head(topjokes)

topjokes <- as.data.frame(topjokes) #Make topjokes a dataframe (and not just a tibble)
topjokes$beta <- round(topjokes$beta, 4) #round to four digits...

#Reshape topjokes to wideformat and transpose, i.e. topics are columns after this:
topjokes_w <- t(reshape(topjokes, idvar = "topic", timevar = "rank", direction = "wide"))
head(topjokes_w)

colnames(topjokes_w) <- as.character(unlist(topjokes_w[1,])) #make top row columnnames
topjokes_w = topjokes_w[-1, ]

# Get total topic loadings
topicTotal <- as.data.frame(colSums(topicDocProbabilities100[, c(2:21)]))
topicTotal$topic <- colnames(topjokes_w) # get zero-padded topic names from previous step
names(topicTotal)[1] <- "totaltopicloading"
head(topicTotal)

saveRDS(topicTotal, file = "topicTotal.rds") #for later use

topicTotal$totaltopicloading <- round(topicTotal$totaltopicloading, 4) #round to four digits

topicTotal_t <- as.data.frame(t(topicTotal[ ,2:2])) #make data.frame, transpose
colnames(topicTotal_t) <- colnames(topjokes_w) #resuse names from topjokes
head(topicTotal_t)

topjokes_w <- rbind(topicTotal_t, topjokes_w) #bind togehter
rownames(topjokes_w)[rownames(topjokes_w) == "1"] <- "Total loading" 
head(topjokes_w)

#Now we'll create the app...:
library(shiny)
library(reactable)

#The user interface
ui <- fluidPage(
  title = "Model Interaction App",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "topicDocProbabilities100"'
      ),
      
      conditionalPanel(
        'input.dataset === "topjokes_w"'
      ),
      width = 4,
      helpText("Here you can interact with the model and the data at the sametime"),
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Documents and topics", reactable::reactableOutput("tbl")),
        tabPanel("Top words per topic", DT::dataTableOutput("tbl2"))
      ),
      width = 30
    )
  )
)

# Function to create tooltips when hovering over column headers of topic columns
with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
            title = tooltip, 
            value)
}

# Extract the top 10 words as a concatenated string separated by commas
top_words_tooltip <- topjokes %>%
  group_by(topic) %>%
  mutate(top_words = paste0(term, collapse=",")) %>%
  slice(1)

top_words <- as.list(top_words_tooltip$top_words)
names(top_words) <- top_words_tooltip$topic

n_topics <- topicDocProbabilities100 %>% 
  select(starts_with("Topic")) %>%
  ncol()

# Empty list to hold column styling info
topic_coldefs <- vector(mode = "list", length = n_topics)

for (i in 1:n_topics){
  # We insert tooltips and some styling (grey background color) for all topic columns
  topic_coldefs[[i]] <- reactable::colDef(style = list(background = "rgba(0, 0, 0, 0.03)"), 
                                          width = 120,
                                          header = with_tooltip(value = names(top_words)[i],
                                                                tooltip = top_words[[i]])
  )
}

# Name the list of topics Topic_1, Topic_2, ..., etc.
names(topic_coldefs) <- colnames(topicDocProbabilities100[2:21])

# Force some columns to be sticky when user scrolls in lateral direction
other_coldefs <- list(
  text = colDef(width = 600,
                sticky = "right",
                # Add a left border style to visually distinguish the sticky column
                style = list(borderLeft = "1px solid #eee"),
                headerStyle = list(borderLeft = "1px solid #eee")),
  TopTopic = colDef(width = 130,
                    sticky = "right",
                    # Add a left border style to visually distinguish the sticky column
                    style = list(borderLeft = "1px solid #eee"),
                    headerStyle = list(borderLeft = "1px solid #eee")),
  doc_id = colDef(sticky = "left",
                  style = list(borderLeft = "1px solid #eee"),
                  headerStyle = list(borderLeft = "1px solid #eee"))
)

coldefs <- c(other_coldefs, topic_coldefs) # combine lists with column definitions



# Server that builds the tables
server <- function(input, output) {
  
  html <- function(x, inline = FALSE) {
    container <- if (inline) htmltools::span else htmltools::div
    container(style = "width:45%;", x)
  }
  
  output$tbl <- renderReactable({
    reactable(topicDocProbabilities100[, 1:23], 
              searchable = TRUE, 
              showSortable = TRUE,
              wrap = FALSE,
              highlight = TRUE,
              columns = coldefs,
              # if there exists a comment, make row expandable
              details = colDef(name = "",
                               sticky = "left",
                               details = function(index) {
                                 if(nchar(topicDocProbabilities100$text[index]) > 75) {
                                   htmltools::tagList(
                                     html(topicDocProbabilities100$text[index])
                                   )
                                 } 
                               })
    )}
  )
  
  output$tbl2 <- DT::renderDataTable({
    DT::datatable(topjokes_w,
                  extensions = c('FixedColumns'),
                  options = list(
                    pageLength = 25,
                    dom = 'Bfrtip',
                    scrollX = TRUE,
                    fixedColumns = list(leftColumns = 1),
                    fixedHeader = TRUE,
                    autoWidth = TRUE
                  ))
  })
  
}


#Combine user interface and server function into an app:
shinyApp(ui, server)

#check out, e.g., topic no 5.

GMY <- "MYA"
GMY