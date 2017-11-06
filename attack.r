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
  webpage=read_html(url)
  print('load page success')
  node=html_nodes(webpage,'td')
  nodedata=html_text(node)
  # get each element
  for (i in 0:20000)
  {
     
    
  }
  # save the data frame to a file
  write.csv(total,'diamonds.csv')
}
cl=makeCluster(detectCores(logical = TRUE))
registerDoParallel(cl)
foreach(x=0:299000,combin='rbind' %dopar% test(x))
stopCluster()
cl 



