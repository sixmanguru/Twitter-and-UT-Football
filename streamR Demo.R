#assumes existing installation
require(twitteR)
require(streamR)
require(ROAuth)
require(RCurl)
require(rjson)
require(qdap)

setwd(getwd())
myPassword <- toString(read.table("Password.txt")[[1]])           #No way I'm divulging this in text! :P
me <- getUser("MyDataScience")

#First time access to twitter feed requires OAuth
#via either streamR or twitteR library. After that,
#save credentials in working directory and load them
#via following command for quick access:
load("twitteR_credentials")                                       #S4 vector created after first-time OAuth
registerTwitterOAuth(twitCred)                                    #Won't work until second+ times

#BE CAREFUL!!!! 10,800 seconds is a THREE HOUR capture period
#Coordinates start with SW corner of UT campus and extend
#to the north east corner of campus.
filterStream( file = "Output/ExampleTweets.json",
              locations = c(-97,30,-96,31), track="hookem",
              timeout = 10800, user = me,
              password = myPassword,
              oauth = twitCred )

dataPath <- "Output/ExampleTweets.json"
tweets   <- parseTweets(dataPath, simplify = FALSE, verbose = TRUE)

#RUN FROM HERE after tweets already parsed!!
#names(tweets) to see what each column holds
text <- list()
time <- list()
for(i in 1:nrow(tweets)) {
  text[[i]] <- c(tweets[i,1])                                      #"text" is [x,1] in the dataframe
  
  working <- unlist(strsplit(toString(tweets[i,9]), " "))          #"created" is [x,9] in the dataframe
  time[[i]] <- c(as.POSIXct(strptime(working[4],"%H:%M:%S")))      #for HH:MM:SS format
}
#del <- "rm Temp/*"                                                #ensure /Temp is empty - sometimes buggy...
#system(del, wait = FALSE)

#All tweets/times are now recorded in separate lists
#DONT want tweets with random characters
#remove nums, punc etc.
sentRefined <- list()                                               #tweet sentiment
timeRefined <- list()                                               #tweet time
k <- 1
for(i in 1:nrow(tweets)) {
  text[[i]] = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text[[i]])
  text[[i]] = gsub("@\\w+", "", text[[i]])
  text[[i]] = gsub("[[:punct:]]", "", text[[i]])
  text[[i]] = gsub("[[:digit:]]", "", text[[i]])
  text[[i]] = gsub("http\\w+", "", text[[i]])
  text[[i]] = gsub("[ \t]{2,}", "", text[[i]])
  text[[i]] = gsub("^\\s+|\\s+$", "", text[[i]])
  
  try.error = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  text[[i]] = sapply(text[[i]], try.error)
  text[[i]] = text[[i]][!is.na(text[[i]])]
  names(text[[i]]) = NULL
  
  if(length(text[[i]]) == 0) {
    next
  }else if(grepl("![a-zA-Z]",text[[i]])) {                          #if still weird chars, skip
    next
  } else {
    sentiment <- polarity(text[[i]], 
                          rm.incomplete = TRUE, 
                          digits = 5)[["group"]][,"ave.polarity"]
    sentRefined[[k]] <- c(sentiment)
    timeRefined[[k]] <- c(time[[i]])
    k <- k+1
  }
}
#This will return all sorts of warnings saying: "no non-missing arguments to max"
#now we have sentiments and corresponding times... let's plot!
lapply(sentRefined, write,
       "Temp/tmp.csv", 
       append=TRUE, 
       ncolumns=1000)
lapply(timeRefined, write,
       "Temp/time.csv", 
       append=TRUE, 
       ncolumns=1000)
#From here I decided to plot in excel for two reasons: Excel offers more plotting
#control, but primarily because plotting keeps freezing in R3.0.2 for some reason
