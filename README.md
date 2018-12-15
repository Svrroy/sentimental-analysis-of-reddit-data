# sentimental-analysis-of-reddit-data
here i have performed sentimental analysis of reddit data using R.


Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
• if pacman isn’t installed then above code will install the package required
library(pacman)
• pacman package is an R package management tool

p_load(magrittr, RedditExtractoR, reshape2, tidytext, tidyverse, wordcloud)
Loading all the packages using p_load function which are required 
• p_load is a general use tool that can install, load, and update packages. 
• magrittr library is used to load the pipe function (%<>%)

• RedditExtractoR library is used to get the reddit data 

• reshape2 library is used to reshape the data 

• tidytext library is used to preprocess the text data like removing stopwords (is, any, they, it, can , are ,am etc.) from data.

• tidyverse library contains the functionality of ggplot2(library for data  visualization), dplyr(library for data processing), stringr (for string manipulation)

• wordcloud library is used to create pretty word clouds, visualize differences and similarity between documents, and avoid over-plotting in scatter plots with text.









links <- reddit_urls(subreddit = "health", page_threshold = 10, sort_by = "relevance")
• reddit_urls function  used to extract URLs of Reddit threads of interest.
• subreddit : can choose any subreddit of our interest
• page_threshold = can limit the number of pages till where we want to get the data 
saveRDS(links, "links.rds")
• R has its own data file format–it’s usually saved using the .rds extension. To read a R data file, we used  the readRDS() function.

links %<>% arrange(desc(num_comments))
• Finding subreddits with most commented and extracting selected URL
• using arrange function arranging data in descending form
• %<>% : Pipe Function -  check this video to understand working of pipe function= https://www.youtube.com/watch?v=P8idxYm8m64 

url <- links[2, "URL"]
• making 2 columns one is index and another column for comments
comments <- reddit_content(url)
saveRDS(comments, "comments.rds")
• Using reddit_content function extracting content/comments from the data file
# Data wrangling #

# Extract comments
comments_tidy <- comments$comment 
• Getting only and saving into the comments_tidy variable for preprocessing


comments_tidy %<>%
  gsub("[^[:alpha:][:blank:]']", "", .) %>%
  tolower()
  • Removing numbers and punctuations and convert to lowercase
  • gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character)
      • [^[:alpha:][:blank:]'] = Regular expression
	• ^ : matches the start of the string.
	• [:alpha:]: alphabetic characters, equivalent to [[:lower:][:upper:]] or [A-z]
• [:blank:]: blank characters, i.e. space and tab.
 

# Split strings and convert to data frame
comments_tidy %<>%
  strsplit(., " ") %>%
  unlist() %>%
  data.frame() 
colnames(comments_tidy) <- "word"
• Splitting strings and converting to dataframe
• unlist() function - https://www.youtube.com/watch?v=am_rbUkhliI
• data.frame() is used to  create data frame
• colnames function is used to give column, column name “word”

comments_tidy %<>% filter(word != "") 
• Removing all the blanks from comments
• filter find cases where conditions are true 



comments_tidy %<>% anti_join(stop_words)
• Removing Stopwords from comments 


viz_theme <- theme(
  strip.background = element_rect(colour = "transparent", fill = "grey90"),
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  strip.text = element_text(size = rel(1), face = "bold"),
  plot.caption = element_text(colour = "grey50"),
  text = element_text(family = "Avenir"))

• Created a user defined function to make custom visualization theme 
• Read this article to know about parameters used to create the customized theme - http://joeystanley.com/blog/custom-themes-in-ggplot2


comments_wordfreq <- comments_tidy %>%
  count(word, sort = TRUE)
• Finding the most common words and saving into the comments_wordfreq variable




comments_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 20) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  theme(text = element_text(size = 30)) + 
  xlab("") + ylab("") + ggtitle("Most common words in Reddit thread", subtitle = " ") +
  ylim(0, 60) + coord_flip() + viz_theme 

ggsave("plot_words.png", width = 12, height = 8, units = "in", dpi = 100)
• Plotting the most common words
• To know  more about ggplot  parameters check this out: https://ggplot2.tidyverse.org/reference/
 

# Sentiment analysis #
comments_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment) %>%
  ggplot(aes(sentiment, n)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(text = element_text(size = 30), axis.text.x = element_text(angle = 65, vjust = 0.5)) +
  xlab("") + ylab("") + ggtitle("Total sentiment scores in Reddit thread", subtitle = " ") +
  ylim(0, 500) + theme(legend.position = "none") + viz_theme 

ggsave("plot_sentiments.png", width = 12, height = 8, units = "in", dpi = 100)
• Calculating the sentiment score of comments using get_sentiments() function
• “nrc lexicon categorizes words in a binary fashion (“yes”/“no”) into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.



bing_counts <- comments_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
• bing lexicon categorizes words in a binary fashion into positive and negative categories. 
• Ungrouping comments using ungroup function

bing_counts_plot <- bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 
• calculating top Words from comments 

ggplot(bing_counts_plot, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  xlab("") + ylab("") + 
  theme(text = element_text(size = 30)) + 
  ggtitle("Most common +/- words in Reddit thread", subtitle = " ") +
  coord_flip() + viz_theme

ggsave("plot_pos_neg_words.png", width = 12, height = 8, units = "in", dpi = 100)

• Plotting most common positive and negative words from the comments


# Wordcloud #

png("wordcloud.png", width = 3.5, height = 3.5, units = 'in', res = 300)
comments_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", #00BFC4 "), max.words = 60)
dev.off()
• Visualizing wordcloud for both positive and negative words comparatively 
• comparison.cloud for comparing positive and negative 
• #F8766D – color code for negative words (Red)
• #00BFC4 - color code for positive words (lite blue)
• acast function - https://www.rdocumentation.org/packages/reshape2/versions/1.4.3/topics/cast

comments[1, "title"]
• printing out first comment of our data 
