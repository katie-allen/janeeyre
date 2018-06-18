# Jane Eyre Text Analysis
#
# Following this blog post by Julia Silge:
# http://juliasilge.com/blog/If-I-Loved-NLP-Less/


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
# remove the title and chapter headings
jane <- data.frame("chapter" = rep(NA, 4021), "paragraph" = 1:4021, 
                   "text" = rep(NA, 4021))
row <- 0
current_chapter <- 0
for(i in 3:length(janeeyre_text)){
  if(i %in% chapters_index){
    current_chapter <- current_chapter + 1
  } else {
    row <- row + 1
    jane[row, "chapter"] <- current_chapter
    jane[row, "text"] <- janeeyre_text[i]
  }
}
remove(janeeyre_text)

# double check
table(jane[,1])
which(is.na(jane[,1]))


#---------  SENTIMENT ANALYSIS  ---------------------

library(syuzhet)

# try it on a few sentences
get_nrc_sentiment("There was no possibility of taking a walk that day.")
get_nrc_sentiment("Do you think, because I am poor, obscure, plain, and little,
                  I am soulless and heartless? You think wrong!")
get_nrc_sentiment("Reader, I married him.")

library(reshape2)
library(dplyr)

# for every paragraph, get the positive and negative
jane <- cbind(jane, get_nrc_sentiment(jane[,"text"])[9:10])
jane$negative <- -jane$negative
jane_pn <- jane %>% select(paragraph, chapter, positive, negative) %>% 
  melt(id = c("paragraph", "chapter"))

head(jane)
head(jane_pn)


#---------  PLOT POSITIVE/NEGATIVE  -----------------

library(ggplot2)

# brute force - find interesting parts of the novel
which(startsWith(jane[,2], "When I awoke")) # helen dies - 651
which(startsWith(jane[,2], "Something creaked")) # room on fire - 1209
which(startsWith(jane[,2], "But what had befallen")) # proposal - 2245
which(startsWith(jane[,2], "The clergyman looked up")) # wedding is stopped - 2569
which(startsWith(jane[,2], "His lips and cheeks turned white")) # st john proposes - 3584
which(startsWith(jane[,2], "He put out his hand with a quick gesture")) # reunited - 3768

# markers, so we can follow the story line
annotatetext <- data.frame(x = c(651, 1209, 2245, 2569, 3584, 3768), 
                           y = c(20.3, 23.3, 20.3, 24.3, 20.3, 18.3), 
                           label = c("Helen", "Fire", "Proposal", "Wedding",
                                     "St. John", "Reunited"))
annotatearrow <- data.frame(x = c(651, 1209, 2245, 2569, 3584, 3768), 
                            y1 = c(19,22,19,23,19,17), 
                            y2 = c(12.1, 14.3, 11.4, 14.1, 11.2, 10.5))

# plot the positive and negative sentiment!
ggplot(data = jane_pn, 
       aes(x=paragraph, y=value, color=variable, fill=variable)) +
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


#---------  PLOT OVERALL SENTIMENT  -----------------

# happy - sad = overall
jane$overall <- jane$positive + jane$negative
ggplot(data = jane, 
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


#---------  FILTER and TRANSFORMATION  --------------

# discrete cosine transformation w/ low-pass filter
jane_dct <- as.numeric(get_dct_transform(jane$overall, low_pass_size = 6,
                  x_reverse_len = 100, scale_vals = T, scale_range = F))
jane_dct <- data.frame(cbind(linenumber = 1:100, dct = jane_dct))
#jane_ft <- as.numeric(get_transformed_values(jane_sentiment$sentiment, low_pass_size = 6, scale_vals = TRUE, scale_range = FALSE))
#jane_ft <- data.frame(cbind(linenumber = 1:100, ft = jane_ft))

# plot the plot!
ggplot(data = jane_dct, aes(x = linenumber, y = dct)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "dodgerblue4", fill = "dodgerblue4") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Overall Sentiment in ", italic("Jane Eyre")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  # helen dies - 651 = 0.1619
  # room on fire - 1209 = 0.3006715
  # kiss under the tree - 2245 = 0.558
  # wedding is stopped - 2569 = 0.63889
  # st john proposes - 3584 = 0.891
  # reunited - 3768 = 0.93708
  annotate("text", size = 3, 
           x = c(16, 30, 56, 64, 89, 94), 
           y = c(-0.4, 0.6, -0.4, -0.4, 0.6, -0.54), 
           label = c("Helen", "Fire", "Kiss", "Wedding", "St. John",  "Reunited")) +
  annotate("segment", arrow = arrow(length = unit(0.03, "npc")),
           x = c(16, 30, 56, 64, 89, 94), xend = c(16, 30, 56, 64, 89, 94),
           y = c(-0.36, 0.52, -0.36, -0.36, 0.52, -0.50), 
           yend = c(-0.02, 0.02, -0.02 , -0.02, 0.02, -0.02))


#---------  EMOTION by CHAPTER  ---------------------

# break into chapters
jane_chapters <- data.frame("chapter"=1:38,
                            "text"= rep("NA", 38))
jane_chapters$text <- as.character(jane_chapters$text)
for(i in 1:38){
  index <- which(jane$chapter == i)
  jane_chapters[i,2] <- paste(jane[index,"text"], sep = '', collapse = '')
}
  

# get emotions for each chapter
jane_chapters <- cbind(jane_chapters, get_nrc_sentiment(jane_chapters[,2]))
jane_chapters[1,]
emotions <- jane_chapters %>% select(chapter, anger, anticipation, disgust, 
              fear, joy, sadness, surprise, trust) %>% melt(id = c("chapter"))
names(emotions) <- c("chapter", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment <- factor(emotions$sentiment,
                             levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])

# plot with a pretty heatmap
library(viridis)
ggplot(data = emotions, aes(x = chapter, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  scale_fill_viridis(name="Score") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Emotions in ", italic("Jane Eyre")))) +
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

## FEAR in Chapter 27 = when Jane runs away
## But what if you normalize by the number of words in the chapter??

# count number of words
words <- rep(NA, 38)
for(i in 1:38){
  ss <- gsub("[[:punct:]]", "", jane_chapters[i,"text"])
  words[i] <- length(gregexpr(" ", ss)[[1]])
}

# recalculate emptions
jane_chap2 <- jane_chapters[,c(1,3:10)]
for(i in 1:nrow(jane_chap2)){
  jane_chap2[i,2:9] <- jane_chap2[i,2:9]/words[i]
}
emotions2 <- jane_chap2 %>% melt(id = c("chapter"))
names(emotions2) <- c("chapter", "sentiment", "value")
levels(emotions2$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions2$sentiment = factor(emotions2$sentiment,
                             levels(emotions2$sentiment)[c(5,8,2,7,6,3,4,1)])

# does it change?
ggplot(data = emotions2, aes(x = chapter, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  scale_fill_viridis(name="Normalized Score") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Emotions in ", italic("Jane Eyre")))) +
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
## so, it makes sense that the happy ending gets drowned out.