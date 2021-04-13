rm(list = ls())

#library(rstudioapi)
#setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))

#We will create an app to interact with out topic model via two outputs,
#topicDocProbabilities (gamma) and topic summary. We will enrich the latter a bit.

#The app can also be found here:
#https://love-borjeson.shinyapps.io/interact2/

topicDocProbabilities <- readRDS("topicDocProbabilities.rds")
head(topicDocProbabilities)
topicDocProbabilities[, 2:30] <- round(topicDocProbabilities[, 2:30], 4) #We don't need the full format...

library(topicmodels)
modelBig29 <- readRDS("model_lab_29.rds") #we'll use a full-sized model

library(dplyr)
library(tidytext) #tidy text communicates with our model objects
jokesterms <- tidy(modelBig29, matrix = "beta")
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
topjokes$topic <- sprintf("%02i", topjokes$topic) #to get the order of plots right
topjokes$topic <- as.factor(topjokes$topic) #Make topic no a factor
topjokes$temp <- factor("Topic_") #create a new column/var filled with the word Topic_
topjokes$topic <- paste(topjokes$temp,topjokes$topic) #Paste together
topjokes$topic <- gsub("[[:space:]]", "", topjokes$topic) #Take away white space
topjokes <- select(topjokes, -temp) #Remove temp column.
#All this just to get the word "Topic" into the topics column. Phew!
saveRDS(topjokes, file = "topjokes.rds") #We'll need this later
head(topjokes)

topjokes <- as.data.frame(topjokes) #Make topjokes a dataframe (and not just a tibble)
topjokes$beta <- round(topjokes$beta, 4) #round to four digits...

#Reshape topjokes to wideformat and transpose, i.e. topics are columns after this:
topjokes_w <- t(reshape(topjokes, idvar = "topic", timevar = "rank", direction = "wide"))
head(topjokes_w)

colnames(topjokes_w) <- as.character(unlist(topjokes_w[1,])) #make top row columnnames
topjokes_w = topjokes_w[-1, ]

#Get total topic loadings
topicTotal <- as.data.frame(colSums(topicDocProbabilities[, c(2:30)]))
topicTotal$topic <- seq.int(nrow(topicTotal))
topicTotal$topic <- sprintf("%02i", topicTotal$topic) #to be able to match this to previous results further down the line
topicTotal$topic <- as.factor(topicTotal$topic) #Make topic not a factor
topicTotal$temp<- factor("Topic_") #create a new column/var filled with the word Topic_
topicTotal$topic <- paste(topicTotal$temp,topicTotal$topic) #Paste together
topicTotal$topic <- gsub("[[:space:]]", "", topicTotal$topic) #Take away white space
topicTotal <- select(topicTotal, -temp) #Remove temp column.
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
library(DT)

#The user interface
ui <- fluidPage(
  title = "Examples of DataTables",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "topicDocProbabilities"'
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
        tabPanel("Documents and topics", DT::dataTableOutput("tbl")),
        tabPanel("Top words per topic", DT::dataTableOutput("tbl2"))
      ),
      width = 30
    )
  )
)

#The server function
###############################For reference!
#HOW TO CREATE A VECTOR WITH TOPIC SUMMARY TO DISPLAY ON MOUSE-OVER (un-comment below):
#vec <- as.data.frame(terms(modelBig29 ,10)) #Summarize.
#vect <- as.data.frame(t(vec)) #Transpose
#library(tidyr)
#vect2 <- vect %>% unite("z", sep=',', V1:V10, remove = T) #Conflate words within each topic
#vect2$z <- paste0(" '", vect2$z) #Insert ' in beginning
#vect2$z <- paste0(vect2$z, "'") #... and in the end
#vect2 <- as.data.frame(t(vect2)) #Transpose
#vect2 <- vect2 %>% unite("z", sep=',', c(1:29), remove = T) #Conflate between topics
#print(vect2) #Copy the print output from the console and paste it into the Javascript below after "callback = JS("var tips = ['Row Names', 'doc',"
#This is already done below (at your service, allways...)
###############################

server <- function(input, output) {
  
  output$tbl <- DT::renderDataTable({
    DT::datatable(topicDocProbabilities[, 1:32],
                  extensions = c('FixedColumns'),
                  callback = JS("var tips = ['Row Names', 'doc', 
    'doctor,leg,eye,news,head,body,patient,hospital,heart,brain', 'jack,joe,harry,mary,george,dick,mike,sam,jim,tom', 'world,country,people,president,bill,year,bush,clinton,america,george_w_bush', 'car,officer,police,light,cop,driver,gun,window,truck,gas',
    'friend,bar,mama,joke,beer,bartender,drink,tooth,time,bottle', 'blonde,brunette,blonde_,head,lightbulb,blond,wish,genies,chief,hair', 'seat,shotgun,plane,passenger,rule,bear,train,driver,way,trip', 'law,cow,state,street,time,city,texas,california,person,place',
    'people,person,fart,one,group,time,movie,world,thing,food', 'side,foot,road,farmer,chicken,way,mile,town,sign,pig', 'wife,husband,year,child,couple,age,family,month,honey,love', 'boy,mother,teacher,father,son,mom,kid,dad,daughter,school',
    'work,job,company,boss,time,office,party,part,employee,home', 'ball,hole,fire,fish,game,golf,water,team,boat,wood', 'night,room,momma,door,bed,morning,floor,hour,manager,hotel', 'hand,redneck,finger,shoe,clothes,shirt,pair,top,christmas,mirror',
    'thing,life,way,time,one,lot,people,mind,idea,today', 'name,word,number,phone,letter,voice,picture,time,money,order', 'water,hour,table,dinner,coffee,food,restaurant,meal,egg,minute', 'week,priest,dollar,day,church,today,money,service,sister,rabbi',
    'dog,house,john,bird,one,box,owner,day,neighbor,parrot', 'man,woman,guy,man_reply,money,king,wallet,ask,bit,men', 'question,student,answer,class,paper,point,book,roommate,minute,school', 'lady,knock,customer,store,sir,clerk,bob,shop,bill,bank',
    'cat,lawyer,bathroom,toilet,human,judge,chair,door,mouth,shower', 'computer,window,virus,system,time,button,user,microsoft,error,drive', 'girl,baby,sex,time,difference,year_old,pants,date,face,ass',
    'day,horse,tree,shit,animal,elephant,rabbit,monkey,kind,lion', 'god,chuck_norris,hell,heaven,jesus,lord,earth,st_peter,time,face'
    ],
    header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);
}"), # The pasted topic-summary-vector above has to be diveded into seperate lines, 3-5... Or R won't read them.
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(5, 10, 15, 20),
                    scrollX = TRUE,
                    fixedColumns = list(leftColumns = 2, rightColumns = 2),
                    fixedHeader = TRUE,
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '600px', targets = c(32))
                    )))%>% formatStyle(names(topicDocProbabilities[, 2:30]),
                                       background = styleColorBar(range(topicDocProbabilities[, 2:30]), 'lightblue'),
                                       backgroundSize = '98% 88%',
                                       backgroundRepeat = 'no-repeat',
                                       backgroundPosition = 'center')
    
  })
  
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
