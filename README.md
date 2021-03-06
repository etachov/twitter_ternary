# Ternary Twitter

![](https://github.com/etachov/twitter_ternary/blob/master/how_to.gif)

<a href = "https://en.wikipedia.org/wiki/Ternary_plot" target = "_blank">Ternary plots</a> are a great way to visualize Twitter interactions. This repo contains a Shiny app tailored for one specific use case: finding out if you've made a <a href = "http://www.esquire.com/news-politics/news/a54440/twitter-ratio-reply/" target = "_blank">Bad Tweet</a>. 

You can download and run the app using with a few lines of code: 

```R
# install these libraries before runGitHub
library(shiny)
library(dplyr)
library(markdown)
library(ggtern)
library(Cairo)

runGitHub("twitter_ternary", "etachov") 
```

There's also a web version <a href = "https://etachov.shinyapps.io/Twitter_Tern/" target = "_blank">here</a>.
