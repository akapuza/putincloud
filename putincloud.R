# Packages
library(tm)
library(SnowballC)
library(wordcloud)
library(quanteda)
library(stm)
library(rio)
library(qdap)
library(tidyverse)

# 25 марта, первое обращение ---------------------------------

#Read data
text <- readLines("Путин2.txt", encoding = "UTF-8")
docs <- Corpus(VectorSource(text))
inspect(docs)
#Prepare data
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, words = stopwords("russian"))
docs <- tm_map(docs, removeWords, "или")
docs <- tm_map(docs, stemDocument, language="russian")

#Text to term matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 18)

# WordCloud
set.seed(5)
wordcloud(words = d$word, freq = d$freq, min.freq=4, scale=c(2,0.2),
          max.words=100, random.order=F, use.r.layout=FALSE,
          colors=brewer.pal(6, "Dark2"))

#Frequency plot
ggplot(data = d[1:18,], aes(x=fct_reorder(word,freq), y = freq, fill = as.factor(freq), col = as.factor(freq))) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("") +
  ylab("Частота") +
  theme(panel.background = element_rect(fill = "gray99"), axis.text.y = element_text(size=14), 
        axis.text.x = element_text(size=14), axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

# Associations and cluster analysis
findAssocs(dtm, c("эт", "наш", "стран", "мер", "доход", 
                   "так", "котор", "ситуац", "предлага", "процент", "налог"), 0.5)

dm2 <- removeSparseTerms(dtm, sparse=0.96)
d2 <- dist(as.matrix(dm2))
hc <- hclust(d2, method="ward.D")
plot(hc)
groups <- cutree(hc, k=8)     
rect.hclust(hc, k=8, border="red")

#Word network
sw <- c("и","в","во","не","что","он","на","я","с","со","как","а","то","все","она","так","его","но","да","ты","к","у","же","вы","за","бы","по","только","ее","мне","было","вот","от","меня","еще","нет","о","из","ему","теперь","когда","даже","ну","вдруг","ли","если","уже","или","ни","быть","был","него","до","вас","нибудь","опять","уж","вам","сказал","ведь","там","потом","себя","ничего","ей","может","они","тут","где","есть","надо","ней","для","мы","тебя","их","чем","была","сам","чтоб","без","будто","человек","чего","раз","тоже","себе","под","жизнь","будет","ж","тогда","кто","этот","говорил","того","потому","этого","какой","совсем","ним","здесь","этом","один","почти","мой","тем","чтобы","нее","кажется","сейчас","были","куда","зачем","сказать","всех","никогда","сегодня","можно","при","наконец","два","об","другой","хоть","после","над","больше","тот","через","эти","нас","про","всего","них","какая","много","разве","сказала","три","эту","моя","впрочем","хорошо","свою","этой","перед","иногда","лучше","чуть","том","нельзя","такой","им","более","всегда","конечно","всю","между")
word_associate(text, match.string = c("налог",  "страна", "доход", "меры", "процент", "ситуация"),
               stopwords = sw, proportional = TRUE, 
                       network.plot = TRUE)

# 2 апреля, второе обращение ---------------------------------
############################################################
#                     ОБРАЩЕНИЕ 02.04                      #
############################################################

#Read data
text2 <- readLines("putin0204.txt", encoding = "UTF-8")
docs2 <- Corpus(VectorSource(text2))
inspect(docs2)
#Prepare data
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, removeWords, words = stopwords("russian"))
docs2 <- tm_map(docs2, removeWords, "или", "нам", "такж", "сво")
docs2 <- tm_map(docs2, stemDocument, language="russian")

#Text to term matrix
dtm2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
head(d2, 23)

# WordCloud
set.seed(4)
wordcloud(words = d2$word, freq = d2$freq, min.freq=2, scale=c(2.2,0.1),
          max.words=150, random.order=F, use.r.layout=FALSE,
          colors=brewer.pal(8, "Dark2"))

#Frequency plot
ggplot(data = d2[1:23,], aes(x=fct_reorder(word,freq), y = freq, fill = as.factor(freq), col = as.factor(freq))) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("") +
  ylab("Частота") +
  theme(panel.background = element_rect(fill = "gray99"), axis.text.y = element_text(size=14), 
        axis.text.x = element_text(size=14), axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

# Associations and cluster analysis
findAssocs(dtm2, c("решен", "регион", "наш", "эпидем", "ситуац", 
                  "здор", "самоизоляц", "мер"), 0.5)

dm2 <- removeSparseTerms(dtm2, sparse=0.9)
d2 <- dist(as.matrix(dm2))
hc <- hclust(d2, method="ward.D")
plot(hc)
groups <- cutree(hc, k=9)     
rect.hclust(hc, k=9, border="red")

#Word network
sw <- c("и","в","во","не","что","он","на","я", "ещё", "с","со","как","а","то","все","она","так","его","но","да","ты","к","у","же","вы","за","бы","по","только","ее","мне","было","вот","от","меня","еще","нет","о","из","ему","теперь","когда","даже","ну","вдруг","ли","если","уже","или","ни","быть","был","него","до","вас","нибудь","опять","уж","вам","сказал","ведь","там","потом","себя","ничего","ей","может","они","тут","где","есть","надо","ней","для","мы","тебя","их","чем","была","сам","чтоб","без","будто","человек","чего","раз","тоже","себе","под","жизнь","будет","ж","тогда","кто","этот","говорил","того","потому","этого","какой","совсем","ним","здесь","этом","один","почти","мой","тем","чтобы","нее","кажется","сейчас","были","куда","зачем","сказать","всех","никогда","сегодня","можно","при","наконец","два","об","другой","хоть","после","над","больше","тот","через","эти","нас","про","всего","них","какая","много","разве","сказала","три","эту","моя","впрочем","хорошо","свою","этой","перед","иногда","лучше","чуть","том","нельзя","такой","им","более","всегда","конечно","всю","между")
word_associate(text2, match.string = "мер",
               stopwords = sw, proportional = T,  nw.label.cex = 20,
                         network.plot = TRUE)

# 8 апреля, обращение к губернаторам ---------------------------------
############################################################
#                     ОБРАЩЕНИЕ 08.04                      #
############################################################

#Read data
text3 <- readLines("gubern08.04.txt", encoding = "UTF-8")
docs3 <- Corpus(VectorSource(text3))
inspect(docs3)
#Prepare data
docs3 <- tm_map(docs3, stripWhitespace)
docs3 <- tm_map(docs3, content_transformer(tolower))
docs3 <- tm_map(docs3, removePunctuation)
docs3 <- tm_map(docs3, removeWords, words = stopwords("russian"))
docs3 <- tm_map(docs3, removeWords, words =  c("или", "нам", "им", "кажд", "так", "такж", "сво", "эт", "котор"))
docs3 <- tm_map(docs3, stemDocument, language="russian")

#Text to term matrix
dtm3 <- TermDocumentMatrix(docs3)
m3 <- as.matrix(dtm3)
v3 <- sort(rowSums(m3),decreasing=TRUE)
d3 <- data.frame(word = names(v3),freq=v3)
head(d3, 23)

# WordCloud
set.seed(4)
wordcloud(words = d3$word, freq = d3$freq, min.freq=2, scale=c(2.2,0.1),
          max.words=150, random.order=F, use.r.layout=FALSE,
          colors=brewer.pal(8, "Dark2"))

#Frequency plot
ggplot(data = d3[1:18,], aes(x=fct_reorder(word,freq), y = freq, fill = as.factor(freq), col = as.factor(freq))) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("") +
  ylab("Частота") +
  theme(panel.background = element_rect(fill = "gray99"), axis.text.y = element_text(size=14), 
        axis.text.x = element_text(size=14), axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

# Associations and cluster analysis
findAssocs(dtm3, c("работ", "регион", "сем", "рубл", "выплат", 
                   "ситуац", "месяц", "мер", "люд", "дополнительн"), 0.5)

dm3 <- removeSparseTerms(dtm3, sparse=0.93)
d32 <- dist(as.matrix(dm3))
hc <- hclust(d32, method="ward.D")
plot(hc)
groups <- cutree(hc, k=7)     
rect.hclust(hc, k=7, border="red")

#Word network
sw <- c("это","и","которые","в","во","не","что","он","на","я", "ещё", "с","со","как","а","то","все","она","так","его","но","да","ты","к","у","же","вы","за","бы","по","только","ее","мне","было","вот","от","меня","еще","нет","о","из","ему","теперь","когда","даже","ну","вдруг","ли","если","уже","или","ни","быть","был","него","до","вас","нибудь","опять","уж","вам","сказал","ведь","там","потом","себя","ничего","ей","может","они","тут","где","есть","надо","ней","для","мы","тебя","их","чем","была","сам","чтоб","без","будто","человек","чего","раз","тоже","себе","под","жизнь","будет","ж","тогда","кто","этот","говорил","того","потому","этого","какой","совсем","ним","здесь","этом","один","почти","мой","тем","чтобы","нее","кажется","сейчас","были","куда","зачем","сказать","всех","никогда","сегодня","можно","при","наконец","два","об","другой","хоть","после","над","больше","тот","через","эти","нас","про","всего","них","какая","много","разве","сказала","три","эту","моя","впрочем","хорошо","свою","этой","перед","иногда","лучше","чуть","том","нельзя","такой","им","более","всегда","конечно","всю","между")
word_associate(text3, match.string = "работ",
               stopwords = c("russian", sw), proportional = T,  nw.label.cex = 26,
               network.plot = TRUE)
