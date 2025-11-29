---
layout: post
title: annotating training data in r
date: February 22, 2022
---

Some collaborators and I recently started a project analyzing a large amount of tweets we obtained via the Twitter API. To analyze these data, we are planning to train a machine learning model, which means we need training data, which means we need annotations ('ground truth' as its commonly referred to in computer science).

A lot of the work of data annotation for machine learning is done through crowdsourcing platforms like [MTurk](https://www.mturk.com/). In this case, we are doing the annotating ourselves. 

Not having much of experience annotating data, we began to brainstorm ideas for the most efficient method. Our first thought was to create an Excel spread sheet that we could annotate row by row. Tweet in one column. Annotation in the other. Easy. 

Certainly, this could have worked, but it wasn't much fun as a concept. Which brought us to our next idea, that being a set-up using `library(googlesheets4)` and some `library(tidyverse)` packages in R.

## the idea

What we envisioned was an application that would (1) print one tweet at a time to our screens, (2) ask us a binary question about the tweet (we are building a classifier model, meaning there are only two possible outcomes), and (3) move to the next tweet, repeating steps one and two. 

At some point, we'll stop annotating and quit the session. When we come back for another session (and another, and another, and another...), we want to start the process again, but of course without having to annotate the data we had already annotated.

As it turns out, R has just the tools to do this. All in less than 20 lines of code. 

Here's how we did it:

## step one

First, you'll need to install/load the four R libraries we used to make the annotation tool. We used `library(googlesheets4)` to store our un-annotated and annotated data, and `library(tibble)`, `library(dplyr)`, and `library(crayon)` to build the annotator function.

```r
library(googlesheets4) #install.packages("googlesheets4")
library(tibble) #install.packages("tibble")
library(dplyr) #install.packages("dplyr")
library(crayon) #install.packages("crayon")
```

## step two

Next, create a Google Sheet and save your data to it. Alternatively, you could use a locally stored file or RStudio's global env for your input and output data. We are using Google Sheets because it allows us to collaborate using the same data stored in the cloud.

You can get your data into Google Sheets in a couple of ways. One way is to read in a .csv or other file type into R and write that file directly to a Google Sheet using `googlesheets4::write_sheet()`. Another way is to copy and paste your data into Google Sheets, although we wouldn't advise this if you have a large amount of data. 

Once you've written your data into Google Sheets, name this first sheet (here I'm calling it 'unannotated') and create a second ('annotated'). (By 'sheet' I mean the little tabs at the bottom.) The second should have the same variable names as the first, plus an additional named column that will take the output of your annotations. 

For example:

![](/images/2022-1-22-annotating-training-data-in-r/annotator-spreadsheet3.png)

## step four

Now it's time to build the annotator function. The function starts by reading in the data from both sheets and then using using `dplyr::anti_join()` to remove any data that has already been annotated. Next, it uses a for loop to sequence through the rows in the spreadsheet, printing one string at a time (in our case, a tweet), and asking us to determine whether the tweet is or is not an instance of what we are annotating for. 

The interactive component is achieved using base R's `menu` function, which asks us to enter a 1 or 2 on our keyboards corresponding to the answer. (Note: you can add more than two options. For example, you might add an 'unsure' option for more ambiguous cases you wish to deal with later.) 

Finally, the function will store the result in a tibble, and append the output to our Google Sheet using `googlesheets4::sheet_append()`. You'll notice that the function also makes use of `library(crayon)`. This isn't necessary by any means. We just found that putting the string in a unique colour made it a little easier to distinguish from any surrounding text or console messages. 

```r
tweetannotate <- function(){

  sheet_url = "<sheet URL>"

  df1 <- read_sheet(sheet_url, 
                    sheet = "unannotated") %>% 
          mutate(tweet = as.character(tweet))

  df2 <- read_sheet(sheet_url, 
                    sheet = "annotated") %>% 
          mutate(tweet = as.character(tweet))

  df3 <- anti_join(df1, df2, by = "tweet")

  for (row in 1:nrow(df3)){

    username <- paste0(df3[row, "username"])
    tweet <- paste0(df3[row, "tweet"])

    cat(crayon::green(tweet))

    answer <- menu(c("Instance of x", 
                      "Not an instance of x"), 
                      title = "")

    output <- tibble(
      username = username,
      tweet = tweet,
      annotation = as.numeric(answer)
    ) %>%
      mutate(annotation = case_when(
        annotation == 1 ~"Instance of x",
        TRUE ~ "Not an instance of x"
      ))

    sheet_append(sheet_url, output, sheet = "annotated")
  }
}
```

## step five

To run the function and start the annotation process, enter `tweetannotate()` into your RStudio console. If you don't want to see the `library(googlesheets4)` messages for every entry, you could add another line of code to silence them. We've found that it's handy to have the Google Sheet open alongside or in the background. That way, if you accidentally enter 1 when you meant to enter 2 (or vice versa), you can easily manually correct it.

![](/images/2022-1-22-annotating-training-data-in-r/annotate-tweet-video.gif)

## building on the current application

The function provided above is just a basic template and can be adapted in all kinds of ways. For example, if multiple people are going to be annotating the data, you may want to keep track of who is doing which annotations. This could be achieved by adding another column to the spreadsheet (e.g., 'annotator') and using `menu` again at the beginning of the function to get the annotator to select their name or initials from a list.

Another idea might be to add some randomization into the process, so that you are annotating random rows on each session, rather than following some inherent order.

Finally, you may want to calculate some statistical measure for the reliability of agreement between annotators on your research team. Two popular metrics for measuring this are [Cohen's](https://en.wikipedia.org/wiki/Cohen%27s_kappa) and [Fleiss'](https://en.wikipedia.org/wiki/Fleiss%27_kappa) kappa coefficients. To calculate these metrics, you'll need to have a proportion of your data annotated multiple times by different researchers. This could be achieved by adjusting the `dplyr::anti_join` portion of the code in order to ensure that some degree of overlap is retained.
