### Jane Eyre Text Analysis
###
### Following this blog post by Julia Silge:
### http://juliasilge.com/blog/If-I-Loved-NLP-Less/

#---------  LOAD THE DATA  --------------------------

library(stylo)
data(novels)
janeeyre_text <- novels$CBronte_Jane
remove(novels)

# take a look
length(janeeyre)
janeeyre[1:5]

# there should be 38 chapters
chapters_index <- which(startsWith(janeeyre_text, "CHAPTER"))
length(chapters_index)
chapters_index

# save chapter and paragraph text into dataframe
jane <- data.frame("chapter" = rep(NA, 4021), "text" = rep(NA, 4021))
row <- 0
current_chapter <- 0

# remove the title and chapter headings
for(i in 3:length(janeeyre_text)){
  if(i %in% chapters_index){
    current_chapter <- current_chapter + 1
  } else {
    row <- row + 1
    jane[row, 1] <- current_chapter
    jane[row, 2] <- janeeyre_text[i]
  }
}

remove(janeeyre_text)

table(jane[,1])
which(is.na(jane[,1]))


#---------  BEGIN! Feeeliinnngssssss  ---------------

library(syuzhet)
get_nrc_sentiment("There was no possibility of taking a walk that day.")
get_nrc_sentiment("Do you think, because I am poor, obscure, plain, and little, I am soulless and heartless? You think wrong!")
get_nrc_sentiment("Reader, I married him.")

get_nrc_sentiment(jane[1:5,2])

jane_nrc <- cbind("chapter" = jane[,1], "paragraph" = 1:nrow(jane), get_nrc_sentiment(jane[,2]))
jane_nrc$negative <- -jane_nrc$negative

#------------------------------------------------------------------------

library(dplyr)
library(reshape2)
posneg <- jane_nrc %>% select(paragraph, chapter, positive, negative) %>% 
  melt(id = c("paragraph", "chapter"))
names(posneg) <- c("paragraph", "chapter", "sentiment", "value")



library(ggplot2)
# helen dies - 651
which(startsWith(jane[,2], "When I awoke"))
# mr rochester room on fire - 1209
which(startsWith(jane[,2], "Something creaked"))
# kiss under the tree - 2245
which(startsWith(jane[,2], "But what had befallen"))
# wedding is stopped - 2569
which(startsWith(jane[,2], "The clergyman looked up"))
# st john proposes - 3584
which(startsWith(jane[,2], "His lips and cheeks turned white"))
# she proposes to edward, reunited - 3768
which(startsWith(jane[,2], "He put out his hand with a quick gesture"))

# MASON IS BLEEDING - 1731
which(startsWith(jane[,2], "I had forgotten to draw my curtain"))

# miss ingram shows up - 1355
which(startsWith(jane[,2], "The ten minutes John had given seemed very long"))

# her aunt is dying and she goes back - 2002
which(startsWith(jane[,2], "I reached the lodge at Gateshead about"))

## JANE RUNS AWAY ?????

annotatetext <- data.frame(x = c(651, 1209, 2245, 2569, 3584, 3768), 
                           y = c(20.3, 23.3, 20.3, 24.3, 20.3, 18.3), 
                           label = c("Helen dies", "Bedroom fire", 
                                     "Kiss", "Wedding", 
                                     "St. John", "Reunited"))
annotatearrow <- data.frame(x = c(651, 1209, 2245, 2569, 3584, 3768), 
                            y1 = c(19,22,19,23,19,17), 
                            y2 = c(12.1, 14.3, 11.4, 14.1, 11.2, 10.5))

ggplot(data = posneg, aes(x=paragraph, y=value, color=sentiment, fill=sentiment)) +
  geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
  ylab("Sentiment") +
  ggtitle(expression(paste("Positive and Negative Sentiment in ", 
                           italic("Jane Eyre")))) +
  theme(legend.title=element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  theme(legend.justification=c(1,1), legend.position=c(1, 0.97)) +
  geom_text(data = annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE) +
  scale_fill_manual(values = c("goldenrod2", "dodgerblue4")) +
  scale_color_manual(values = c("goldenrod2", "dodgerblue4"))



#----------------------------------------------------------


# OVERALL SENTIMENT: happy + sad

posneg$overall <- jane_nrc$positive + jane_nrc$negative

ggplot(data = posneg, 
       aes(x = paragraph, y = overall)) +
  geom_bar(stat = "identity", position = "dodge", color = "dodgerblue4") + 
  theme_minimal() +
  ylab("Sentiment") +
  ggtitle(expression(paste("Overall Sentiment in ", italic("Jane Eyre")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  theme(legend.justification=c(1,1), legend.position=c(1, 0.71)) +
  geom_text(data = annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

#--------------------------------------------------

jane_ft <- as.numeric(get_transformed_values(jane_sentiment$sentiment, 
                                             low_pass_size = 6,
                                             scale_vals = TRUE,
                                             scale_range = FALSE))
jane_ft <- data.frame(cbind(linenumber = 1:100, ft = jane_ft))

ggplot(data = jane_ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "dodgerblue4", fill = "dodgerblue4") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Overall Sentiment in ", italic("Jane Eyre")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
# helen dies - 651 = 0.1619
# mr rochester room on fire - 1209 = 0.3006715
# kiss under the tree - 2245 = 0.558
# wedding is stopped - 2569 = 0.63889
# st john proposes - 3584 = 0.891
# she proposes to edward, reunited - 3768 = 0.93708
  annotate("text", size = 3, 
           x = c(16, 30, 56, 64, 89, 94), 
           y = c(-0.4, -0.4, -0.55, 1.0, 1.1, 0.6), 
           label = c("Helen", "Fire", "Kiss",
                     "Wedding", "St. John", 
                     "Reunited")) +
  annotate("segment", arrow = arrow(length = unit(0.03, "npc")),
           x = c(16, 30, 56, 64, 89, 94), xend = c(16, 30, 56, 64, 89, 94),
           y = c(-0.36, -0.36, -0.51, 0.92, 1.02, 0.52), 
           yend = c(-0.02, -0.02, -0.02 , 0.02, 0.02, 0.02))


#-----------------------------------------------

# into chapters
jane_chapters <- data.frame("chapter"=1:38,
                            "text"= rep("NA", 38))
jane_chapters$text <- as.character(jane_chapters$text)
for(i in 1:38){
  index <- which(jane$chapter == i)
  jane_chapters[i,2] <- paste(jane[index,2], sep = '', collapse = '')
}
  

jane_chap_nrc <- cbind("chapter" = jane_chapters[,1], get_nrc_sentiment(jane_chapters[,2]))
jane_chap_nrc

emotions <- jane_chap_nrc %>% select(chapter, anger, anticipation, 
                                disgust, fear, joy, sadness, surprise, 
                                trust) %>% melt(id = c("chapter"))
names(emotions) <- c("chapter", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")

emotions$sentiment = factor(emotions$sentiment,levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])

library(viridis)
ggplot(data = emotions, aes(x = chapter, 
                            y = sentiment, 
                            fill = value)) +
  geom_tile(color="white", size=0.1) +
  scale_fill_viridis(name="Score") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Emotions in ", italic("Pride and Prejudice")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# chapter 27 fear???
# WHERE SHE LEAVES!!!! SO COOL!!!!

# but what if you normalize by the number of words in the chapter??

# count number of words
words <- rep(NA, 38)
for(i in 1:38){
  ss <- gsub("[[:punct:]]", "", jane_chapters[i,2])
  words[i] <- length(gregexpr(" ", ss)[[1]])
}


jane_chap_nrc2 <- jane_chap_nrc
for(i in 1:nrow(jane_chap_nrc2)){
  jane_chap_nrc2[i,2:9] <- jane_chap_nrc2[i,2:9]/words[i]
}
emotions2 <- jane_chap_nrc2 %>% select(chapter, anger, anticipation, 
                                     disgust, fear, joy, sadness, surprise, 
                                     trust) %>% melt(id = c("chapter"))
names(emotions2) <- c("chapter", "sentiment", "value")
levels(emotions2$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")

emotions2$sentiment = factor(emotions2$sentiment,levels(emotions2$sentiment)[c(5,8,2,7,6,3,4,1)])

library(viridis)
ggplot(data = emotions2, aes(x = chapter, 
                            y = sentiment, 
                            fill = value)) +
  geom_tile(color="white", size=0.1) +
  scale_fill_viridis(name="Normalized Score") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Emotions in ", italic("Pride and Prejudice")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))


## Ah-ha! So, something cool: chaper 27 that had the
## huge "fear" spot has the most words of all the chapters
## at 11257 - the next largest is 9262.

## The happy ending has only 1817, the lowest of any chapter 
## so, it makes sense that the happy ending gets drowned out?

## but earlier analyses used paragraphs, not chapters - 
sort(table(jane$chapter))
# yup, chapter 38 (the last) still has the smallest number of 
# paragraphs! and chapter 27 is in the top 5.

# 32 and 31 bright happy spot