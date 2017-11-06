library(httr)
library(rvest)
library(foreach)
library(doParallel)
library(parallel)
# data frame template
test =function(x){
  
  # generate the url 
  url=paste('http://guansmushroom.com',sep='')
  # load the webpage

  print('load page success')
  node=html_nodes(webpage,'td')
  nodedata=html_text(node)
  # get each element
  
  # save the data frame to a file
  write.csv(total,'diamonds.csv')
}
cl=makeCluster(detectCores(logical = TRUE))
registerDoParallel(cl)
 for (i in 0:20000)
  {
       webpage=read_html(url)
      print(i)
  }
stopCluster()
cl 



