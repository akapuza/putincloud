# Packages
library(tm)
library(SnowballC)
library(wordcloud)
library(quanteda)
library(stm)
library(rio)
library(qdap)
library(tidyverse)

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

