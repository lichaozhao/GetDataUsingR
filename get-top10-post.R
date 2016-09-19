#initial 
#http://www.w3school.com.cn/cssref/css_selectors.asp css selector
library(rvest)
library(XML)
Sys.setlocale(,"CHS")

#address that we want to get data.
smth <- "http://m.newsmth.net"

#get main page of m.newsmth.net
mainpage <- read_html(smth, encoding = "UTF-8")

#top 10 post info of newsmth
top10title <- html_nodes(mainpage, "li a") %>% html_text()
top10link <- html_nodes(mainpage, "li a") %>% html_attr("href")  

top10 <- data.frame(top10title, top10link) 
top10$top10link <- paste(smth,top10$top10link, sep="")

#define a function to get every topic list. 
getUrlContent <- function(posturl, isgetContent = T) {
  post <- read_html(posturl, encoding = "UTF-8")
  
  #get items of author, posttime, postcontent etc. then combine them. 
  author <- html_nodes(post, "div.nav.hl a:nth-child(2)") %>% html_text()
  posttime <- html_nodes(post, "div.nav.hl a:nth-child(3)") %>% html_text() 
  if(isgetContent) {
    postcontent <- html_nodes(post, "li div.sp") %>% html_text()
    final<- data.frame(author, posttime, postcontent)
  }
  else {
    final<- data.frame(author, posttime)
  }

  #get page number of this post. 
  pages = html_nodes(post, "div.sec.nav:nth-child(2) a:nth-child(3)")%>%html_text() 
  pages = as.numeric(substr(pages, regexpr("/",pages)[1]+1, nchar(pages)))
  
  return(list(postinfo = final, pagescount = pages))
}

#call getUrlContent, isgetContent = F means just get author and posttime info. 
result <- getUrlContent(top10$top10link[1], F)
posts = result$postinfo

#get all post for second page. 
for(i in 2:result$pagescount) {
  posts<-rbind(posts,getUrlContent(paste(top10$top10link[1],paste("?p=",i,sep = ""),sep=""), F)$postinfo)
}

head(posts)
posts
posts$author

library(dplyr)

