#install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("biganalytics") #for big analytics on matrix
install.packages("bigmemory") #solve memory allocation problems

#load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("biganalytics")
library("bigmemory")

#  load first s and pass as parameter:
source('C:/Users/mnari/Documents/kraya/SS-Corona/DBPRO/project/gdelt_test (1)/plot_selected_themes_count.R')
setwd('C:/Users/mnari/Documents/kraya/SS-Corona/DBPRO/project/The_Project/Output/results')
DEU <- read.csv('rates_DEUfiltered.csv', sep='|')
ITA <- read.csv('rates_ITAfiltered.csv', sep='|')
USA <- read.csv('rates_USAfiltered.csv', sep='|')
countries<-list()
countries[[1]]<-DEU
countries[[2]]<-ITA
countries[[3]]<-USA 
country_names<-c("DEU","ITA","USA")
cov_countries<- c("Germany","United States","Italy")
country_list<-c("GM","IT","US")

#avoid warnings
oldw <- getOption("warn")
options(warn = -1)

# freq item analysis
for(i in c(1:3)){
  # taking the theme data
  c<-countries[[i]]
  themes<-c[2]
  name<-paste("themes_",country_names[i],".txt")
  write.table(themes, file = name, sep = "\t",
              row.names = TRUE, col.names = NA)
  text <- readLines(name)
  
  # Load the data as a corpus and inspect it
  docs <- Corpus(VectorSource(text))
  inspect(docs)
  
  #Transformation is performed using tm_map() function to replace,
  #for example, special characters from the text.
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  dim(docs)
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "_")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  #Data cleaning
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  #docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  
  #Build a term-document matrix
  dtm <- TermDocumentMatrix(docs)
  
  
  #m<-big.matrix(dtm)
  M <- as.matrix(dtm)
  v <- sort(rowSums(M),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  
  # Generate the Word cloud
  set.seed(1234)
  jpeg(file = paste("word_cloud_",country_names[i],".jpeg"))
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words = 200000,
            random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"),scale=c(3.5,0.25) )
  dev.off()
  
  #Explore frequent terms and their associations
  freqTerms <-findFreqTerms(dtm, lowfreq = 2)
  #findAssocs(dtm, terms = "economy", corlimit = 0.3)
  # Writing mtcars data
  write.table(freqTerms, file = paste("freq_Terms",country_names[i],".txt"), sep = "\t",
              row.names = TRUE, col.names = NA)
  
  d<-d[d$freq>1,]
  #get the result
  write.csv(d,file=paste("freq_themes_",country_names[i],".csv"),sep = "|")
  
  #Plot word frequencies
  jpeg(file = paste("bar_freq_",country_names[i],".jpeg"))
  p1<-barplot(d$freq, las = 2, names.arg = d$word,
              col ="lightblue", main ="Most frequent words",
              ylab = "Word frequencies")
  p1
  dev.off()
  print(i)
}

#reactivate warnings
options(warn = oldw)

#Now plotting the freq themes aggregated for each country
#reading the freq_themes 
countries_agg<-list()
countries_agg[[1]]<-read.csv(file = 'freq_themes_ DEU .csv' ,sep = ',')
countries_agg[[2]]<-read.csv(file = 'freq_themes_ ITA .csv' ,sep = ',')
countries_agg[[3]]<-read.csv(file = 'freq_themes_ USA .csv' ,sep = ',')
countries_agg[[1]]<-countries_agg[[1]]$X
countries_agg[[2]]<-countries_agg[[2]]$X
countries_agg[[3]]<-countries_agg[[3]]$X
for (v in c(1:3)) {
  countries_agg[[v]]<-as.list(strsplit(countries_agg[[v]], '//s+'))
}


(countries_agg[[1]])

#read the whole csv file 
s <- read.csv('C:/Users/mnari/Documents/kraya/SS-Corona/DBPRO/project/gdelt_test (1)/Jul19-April20-count-Selected-themes-country.csv', sep='|', colClasses=c("character","numeric","character",rep("numeric",2), rep("character",2), rep("numeric",2)))
#read corona cases file
df_cases<- read.csv("C:/Users/mnari/Documents/kraya/SS-Corona/DBPRO/project/gdelt_test (1)/owid-covid-data.csv")
#reading the covid cases themes
covid<-list()
for (j in c(1:3)) {
  covid[[j]]<-df_cases[df_cases$location == cov_countries[j],]
}
#for each country and freq theme
for (j in c(1:3)) {
  for (i in c(1:(length(countries_agg[[j]])))) {
    pattern<- as.character(countries_agg[[j]][i])
    plot_selected_themes_count(s,pattern,country_list[j])
  }
}
#for each country plot the aggregated themes 
plot_selected_themes_count <- function(s, theme_pattern, countries_list){
  print(theme_pattern)
  library(gridExtra)
  library(grid)
  library(ggplot2)
  library(lattice)
  options(digits=3)
  library(scales)
  #theme_set(theme_bw())    
  
  #to upper case
  theme_pattern<-toupper(theme_pattern)
  print(theme_pattern)
  # Select countries according to the country list
  some_countries <- s[s$countryCode %in% countries_list,]
  
  # Select themes ccording to the pattern
  selection <- some_countries[grepl(theme_pattern, some_countries$themes),]
  selection$date <- as.Date(selection$ts, "%Y%m%d")
  
  
  # Aggregate themes selection count per day and country code 

  theme_data <- aggregate(x = selection$count, by = list(selection$date, selection$countryCode), FUN = sum)
  colnames(theme_data) <- c('date','country','count')
  f<-"data_1.csv"
  write.csv(theme_data,file = f)
  # Plot having country as color 
  p1 <- ggplot(data=theme_data, aes(as.POSIXct(date), count, colour=country)) + geom_line() + geom_point() +
    theme(plot.title = element_text(size=14, face="bold"), axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_x_datetime(breaks=date_breaks("1 month")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1) ) +
    labs(title=paste(' Theme pattern:',theme_pattern))
  
  grid.arrange(p1)
  
  # count of the  more mentioned themes in the selection
  cat('more mentioned THEMES in selection: \n')
  t <- aggregate(x = selection$count, by = list(selection$themes), FUN = sum)
  colnames(t) <- c('Themes','count')
  print(head(t[order(t$count, decreasing = TRUE),], n=10))
  
  # take only the common days from both covid cases and theme data
  somee<-covid[[j]]
  somee$date <-as.Date(somee$date,"%Y-%m-%d")
  somee<-somee[1:122,]
  
  bom<-theme_data
  bom$date<-as.Date(bom$date,"%Y%m%d")
  bom <- bom[184:305,]
  
  #merge
  bom$new_cases<- somee$new_cases
  f<-bom
  print("j")
  f$date<-as.Date(f$date,"%Y-%m-%d")
  print("k")
  jpeg(file = paste("my_plot_",country_list[j],"_",theme_pattern,".jpeg"))
  print("zaama?")
  f$count<-normalize(f$count)
  f$new_cases<-normalize(f$new_cases)
  print(cor(f$count,f$new_cases))
  p1<-ggplot(f, aes(x=as.POSIXct(date))) +  geom_line(aes(y = new_cases), color = "darkred") +  geom_line(aes(y = count), color="steelblue")+ 
    theme(plot.title = element_text(size=14, face="bold"), axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_x_datetime(breaks=date_breaks("1 month")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1) ) +
    labs(title=paste(' Theme pattern:',theme_pattern),subtitle = cor(f$count,f$new_cases))
  grid.arrange(p1)
  dev.off()
  p1
  
  
  
}
