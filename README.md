# Marvel-vs-DC-Movies-Analysis
I am a big Marvel fan so I decided to do a simple comparison between Marvel and DC's movies to see which company was better. The data set for my analysis looked at 39 movies from both and compared the movie rating, gross profits, and budget profits to determine which one was better overall.

## Introduction

After a great amount of time contemplating between ideas, I decided to go with the Marvel vs DC data because I 
was genuinely curious about what the data would show me. As a big time Marvel fan, who does like a very few 
amount of DC movies, I wanted to see if the data would support my belief that Marvel is so much better than DC.

### Primary Questions

The main question I want to address is which company is better, *Marvel* or *DC*? Of course 'better' is broad
since many attributes can determine which company would be considered better like ratings and gross values. 
That is the secondary questions that will be investigated, which company is better in terms of movie ratings 
and gross values.

I would expect to discover that Marvel is better overall, considering ratings and gross values. I believe in
this statement because I have seen all Marvel movies, except a couple that were released within the last 3 years 
which I didn't want to waste my time with (like The Marvels) and the Marvel Universe is just incredibly superior
to DC. I have seen many DC movies that I like, such as Batman, the Joker, and Wonder Woman, but there are so many others that are just terrible (in terms of the plot and/or how it was directed) like Zack Snyder's Justice League.

Let's take a look at the data to then dive into the analysis and discover, once and for all, is Marvel indeed 
better than DC? Which company is better?

## Data

The data that I am using for this project can be accessed from *Kaggle* https://www.kaggle.com/datasets/leonardopena/marvel-vs-dc/data 

Let's bring in the data and fix some of the formatting issues that were blocking some of R's functions: 
```{r}
project_data <- data.table::fread("C:/Users/Graduate/Downloads/db.csv")

Encoding(colnames(project_data)) <- "UTF-8"

colnames(project_data) <- iconv(colnames(project_data), 
                                "UTF-8", 
                                "UTF-8", 
                                sub='')
colnames(project_data) <- make.names(colnames(project_data))

cols_to_change <- c("Original.Title", "Company", 
                    "Minutes")

project_data <- as.data.frame(project_data)

for(name in cols_to_change){
    Encoding(project_data[, name]) <- "UTF-8"

    iconv(project_data[, name], "UTF-8", "UTF-8", sub='')
}

```


There are a few other aspects we will need to clean and fix.
We will extract only the numbers (0 to 9) from the columns to remove the special characters that were there.
```{r}
project_data$Minutes <- stringr::str_extract(project_data$Minutes, "[0-9]+")
project_data$Budget <- stringr::str_extract(project_data$Budget, "[0-9]+")
```


We will also remove the space from the column names to make it easier when referring to those columns and fix the formatting of another column:
```{r}
Encoding(project_data$Original.Title) <- "UTF-8"

project_data$Original.Title <- iconv(project_data$Original.Title, 
                                     "UTF-8", 
                                     "UTF-8", 
                                     sub='')
```

Let's take a look at what the data contains:
```{r}
rmarkdown::paged_table(project_data)
```


After seeing the different columns, we can tell we will need to fix some data types and make them numeric.
Then let's confirm if our change is correct. 
```{r}
project_data$Minutes <- as.numeric(project_data$Minutes)
project_data$Budget <- as.numeric(project_data$Budget)

str(project_data)
```

And lastly, just to be certain, lets confirm that there are no duplicated movies in this data:
```{r}
duplicated(project_data$Original.Title)
```
No duplicates, all good!



To get the top *5* movies, let's investigate some details for those top 5 movies:

Lets get the top 5 rated movies, see what company they are from and what movies they are:
```{r}
top5company <- tail(project_data$Company[order(project_data$Rate)], n = 5)
top5company

top5movie <- tail(project_data$Original.Title[order(project_data$Rate)], n = 5)
top5movie
```
The highest rated movie is from DC, along with number 2 and 5. In 3rd and 4th place it's Marvel. 
The highest rated movie was The Dark knight from DC. 
This was incredibly surprising for me since 3 out of the top 5 movies are DC in terms of movie ratings. 



Lets get the top 5 gross worldwide, see what company they are from, and which are the movies:
```{r}
top5gross <- tail(project_data$Company[order(project_data$Gross.Worldwide)], n = 5)
top5gross

top5grossmov <- tail(project_data$Original.Title[order(project_data$Gross.Worldwide)], 
                     n = 5)
top5grossmov
```
The higher gross profits worldwide are from Marvel. Avengers: Endgame was the highest gross worldwide.


Lets get the top 5 budgets, see what company they are from, and which are the movies:
```{r}
topbudget <- tail(project_data$Company[order(project_data$Budget)], n = 5)
topbudget

topbudgetmov <- tail(project_data$Original.Title[order(project_data$Budget)], n = 5)
topbudgetmov
```
Marvel has the top 2 higher budgets and DC the following 3.
The movie with the highest budget was Avengers: Endgame, which was also the highest gross worldwide.


Let's combine all of these 'top 5s' into a data frame for us to see it better. Let's also invert the
order so that the number one appears at the top of the list.
```{r}
compilation <- data.frame(top5company, top5movie, top5gross, top5grossmov, 
                          topbudget, topbudgetmov)

newcompilation <- compilation[5:1,]
rmarkdown::paged_table(newcompilation)
```



Let's now look into the correlation between variables to have an understanding of how different variables influence movie ratings.

What is the correlation between a movie's budget and gross profits?
```{r}
cor(project_data$Gross.Worldwide, project_data$Budget)
```
There's strong moderate correlation of 0.656 between a movie's gross and budget.

Is there a correlation between movie rating and movie minutes?
```{r}
cor(project_data$Rate, project_data$Minutes)
```
A correlation of 0.58 is a moderate correlation between the movie rating and the length of the movie
showing that minutes perhaps do influence the rating of a movie, to a certain extent.

Is there a correlation between movie rating and movie release?
```{r}
cor(project_data$Rate, project_data$Release)
```
A correlation of 0.33 is right at the border between a weak and moderate correlation, therefore,
we will consider it does not have a strong correlation, since it's close to being a weak one.

Is there a correlation between movie rating and gross worldwide?
```{r}
cor(project_data$Rate, project_data$Gross.Worldwide)
```
A correlation of 0.56 is considered moderate, so there is some influence between the gross profits and rate.


## Methods

We got info on top 5 movies, but lets get overall averages to compare using all the movies in the data set for both companies

Using simple summary statistics, we will have a straightforward comparison between average values. 

Getting the average rate for Marvel movies:
```{r, echo = FALSE}
library(dplyr)
```

```{r}
MarvelMeanRate <- project_data %>%
  filter(Company == "Marvel") %>%
  summarise(MarvelAvgRating = mean(Rate))
```

Marvel's movies have an average rating of 7.48.

Getting the average rate for DC movies:
```{r}
DCMeanRate <- project_data %>%
  filter(Company == "DC") %>%
  summarise(DCAvgRating = mean(Rate))
```
DC's movies have an average rating of 6.81.

Difference between Movie Rates:
```{r}
RateDif <- MarvelMeanRate - DCMeanRate
colnames(RateDif) <- 'Rate_Dif'
```
Marvel's average movie rating is 0.67 higher than DC's


Getting the average gross worldwide for Marvel movies:
```{r}
MarvelGrossRate <- project_data %>%
  filter(Company == "Marvel") %>%
  summarise(MarvelAvgGross = mean(Gross.Worldwide))
```
Marvel's average gross worldwide is $981,965,740

Getting the average gross worldwide for DC movies:
```{r}
DCGrossRate <- project_data %>%
  filter(Company == "DC") %>%
  summarise(DCAvgGross = mean(Gross.Worldwide))
```
DC's average gross worldwide is $605,632,646

Difference between Gross Rates:
```{r}
GrossDif <- (MarvelGrossRate - DCGrossRate)
colnames(GrossDif) <- 'Gross_Dif'
```
Marvel's average gross worldwide is $376,333,094 higher than DC's


Based on the average values, Marvel is superior to DC in both average movie ratings and average gross worldwide.

To see these results more easily, let's get them to a table format:
```{r}
compilation2 <- data.frame(Marvel_Mean_rate = MarvelMeanRate, DC_Mean_Rate = DCMeanRate, 
                           RateDif, MarvelGrossRate, DCGrossRate, GrossDif)
rmarkdown::paged_table(compilation2)
```


## Results

Lets visualize the gross worldwide with rating
```{r}
library(ggplot2)

ggplot(project_data, aes(x = Gross.Worldwide, y = Rate, color = Company)) +   
  geom_point(aes(size = Gross.Worldwide)) +
  scale_color_manual(values = c('blue', 'red')) +
  theme_minimal() +
  labs(title = "Movie Rate by Worldwide Gross Profit",
       x = "Gross Profit Worldwide",
       y = "Movie Rating")
```
The graph shows us how even though marvel has a cluster of gross profits worldwide, it does stand out compared to DC when looking at all the red dots in the right half of the graph. 
DC is standing out in terms of ratings, having more data points between 8 and 9 ratings than Marvel.


Lets visualize the movie titles by rating
```{r}
ggplot(project_data, aes(x = Original.Title, y = Rate, color = Company)) + 
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Ratings per Movie",
       x = "Movie Titles",
       y = "Movie Rating")
```


Lets see the distribution of ratings between the companies
We can see how DC movie ratings are very spread out compared to Marvel that are all above a 6.5
```{r}
ggplot(project_data, aes(x = Company, y = Rate, color = Company)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  theme_minimal() +
  labs(title = "Ratings per Company",
       x = "",
       y = "Movie Rating")
```

Lets plot the gross worldwide per company.
We can clearly see how Marvel outperforms DC, having a significant greater gross worldwide.
```{r}
ggplot(project_data, aes(x = Company, y = Gross.Worldwide, fill = Company)) +
  geom_col() +
  scale_fill_manual(values = c('blue', 'red')) +
  theme_minimal() +
  labs(title = "Gross Worldwide per Company",
       x = "",
       y = "Gross Worldwide")
```

Let's also plot the linear model for gross worldwide and ratings for both companies.
We can see Marvel's steeper slope as compared to DC since they have their ratings more spread out. 
```{r}
ggplot(project_data, aes(x = Rate, y = Gross.Worldwide, color = Company)) +
  geom_point() +
  scale_color_manual(values = c('blue', 'red')) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Gross Worldwide vs. Rating for Marvel and DC")
```


## Discussion

Starting by looking at the top 5 movies for both marvel and DC gives us an idea of the best movies in terms of ratings and gross worldwide, but is not enough to form a thorough interpretation about the two companies as a whole. So through the analysis of the entire data set, we can arrive at more significant conclusions. 

Marvel had higher average movie ratings and gross worldwide when considering the entire data set. The graphs also show how Marvel has a very large difference from DC in terms of gross worldwide.  Marvel is more consistent with their movies, where it's possible to see that the distribution of their movies has ratings much closer together, whereas DC has ratings ranging from very low to very high values. 

These points let us confidently admit that yes, Marvel is better than DC. Marvel is more consistent in achieving high rating and gross worldwide. DC can sometimes release a new movie hit, but when considering all the movies, Marvel is far superior to DC.

This matters because if a movie watcher that doesn't consistently follow either of the companies, if they wanted to choose 1 movie from either Marvel or DC and ensure it was a great movie, their best chances would be picking a Marvel movie.

This also lets us predict how upcoming movies from Marvel and DC would perform. It would be interesting to perform this analysis once more but considering the movies until 2024 as well and see if the newer movies would affect the data in any way.
