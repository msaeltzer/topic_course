## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)



## ----------------------------------------------------------------------------
#install.packages('quanteda')




## ----echo=F,,message=FALSE,warning=F-----------------------------------------

load('uk_manifestos.Rdata')

names(data)



## ----------------------------------------------------------------------------

table(data$partyname,data$date)



## ----message=FALSE,warning=F-------------------------------------------------

library(quanteda)

corp<-corpus(data$text,docvars 
           
           =data.frame(party=data$party,partyname=data$partyname,date=data$date))


## ----------------------------------------------------------------------------

#texts(corp)[1]



## ----------------------------------------------------------------------------

df<-docvars(corp)



## ----------------------------------------------------------------------------

party<-docvars(corp,"party")



## ----------------------------------------------------------------------------

ft<-tokens(corp,remove_punct=T,remove_numbers = T)

ft[1]



## ----------------------------------------------------------------------------
stopwords("en")



## ----------------------------------------------------------------------------
ft<-tokens_tolower(ft)

ft<-tokens_select(ft,pattern=stopwords("en"),selection='remove')

ft[1]


## ----echo=TRUE---------------------------------------------------------------
dfc<-dfm(ft) 


## ----------------------------------------------------------------------------

topfeatures(dfc, n = 10)

library(ggplot2)
# Plot the most frequent words 
frequ <- as.numeric(topfeatures(dfc,10))
word <- as.character(names(topfeatures(dfc, 10)))
dfc_plot <- as.data.frame(frequ,word)

ggplot(dfc_plot, aes(x = reorder(word, frequ), y = frequ)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency")


## ----------------------------------------------------------------------------

df2<-docvars(dfc)

identical(df,df2)



## ----------------------------------------------------------------------------

ddf<-dfm_group(dfc,'party')

ddf<-dfm_subset(ddf,party=='Labour')

textplot_wordcloud(dfc, max_words = 100, 
                   color = c('black','red','purple','green','cornflower blue','yellow','dark blue'))



## ----------------------------------------------------------------------------
envi<-tolower(c("CAR", 
"CATALYTIC", 
"CHEMICAL*",  # the * means any word that starts with chemical 
"CHIMNEY*",
"CLEAN*",
"CONGESTION", 
"CYCLIST*", 
"DEPLET*", 
"ECOLOG*", 
"EMISSION*", 
"ENERGY-SAVING", 
"ENVIRONMENT*",
"FUR", 
"GREEN", 
"HABITAT*", 
"HEDGEROW*", 
"HUSBANDED", 
"LITTER*", 
"OPENCAST", 
"OPEN-CAST*", 
"OZONE", 
"PLANET", 
"POPULATION", 
"RECYCL*", 
"RE-CYCL*", 
"RE-USE", 
"TOXIC", 
"WARMING"))



## ----------------------------------------------------------------------------
env_dict<-dictionary(list(environment=envi))



## ----------------------------------------------------------------------------


btw_env <- dfm(dfc, dictionary = env_dict) 
env <- as.numeric(btw_env[,"environment"]) # only positive mentions of the words in category environment
docvars(dfc,"env")<-env/ntoken(dfc) # compute the share


## ----------------------------------------------------------------------------


plot(docvars(dfc,"env")~as.factor(docvars(dfc,"partyname")), las=2,xlab=NULL,ylab=NULL,cex.axis=0.5)


