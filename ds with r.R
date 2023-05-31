library("rvest")
library("tidyverse")
#a
html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
main_table = html%>%html_table()
data=data.frame(main_table[1])
data=data[,-1]
#problem c1
tennis <- function(p)
{
  c=rbinom(n=5,size=1,prob=p)
  v=0
  b=0
  n=0
  for(i in c){
    if(b>=3) {return(v)}
    if(n>=3) {return(v)}
    if(n==1){
      v=v+1
    }    else{
      n=n+1
    }
    v=v+1
    
  }
  return(v)
}
#problem c2
matches = {}
for(i in 1:1000) 
{
  matches[i] <- tennis(0.7)
}
ans <-mean(matches)


results = {}
#Problem D
MontyHall = function(){
  v=sample(x=1:3,size = 1)
  if(v ==1) {
    return(1)
  }
  else {
    return(0)
  }
  if(v==3){
    return(1)
  }
  else {
    return(0)
  }
}
for(i in 1:1000) 
{
  results[i] <- MontyHall()
}
p <- sum(results)/1000

#problem E
html3 = read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
ranking=html3%>%html_elements(".countdown-index")%>%html_text()
name=html3%>%html_elements(".article_movie_title a")%>%html_text()
t=html3%>%html_elements(".tMeterScore")%>%html_text()
y=html3%>%html_elements(".start-year")%>%html_text()

data3=data.frame(Ranking=ranking, Movie=name,Tomato_Score=t,Year=y)


