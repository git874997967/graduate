library(httr)
library(rvest)
library(foreach)
library(doParallel)
library(parallel)
# data frame template
test =function(x){
total=data.frame(shape=character(),caret=numeric(),cut=character(),color=character(),clarity=character(),table=numeric(),depth=numeric(),cert=character(),measure=character(),price=character())
     # generate the url 
     url=paste('http://www.diamondse.info/webService.php?shape=none&minCarat=0.2&maxCarat=30&minColor=1&maxColor=9&minPrice=100&maxPrice=1000000&minCut=5&maxCut=1&minClarity=1&maxClarity=10&minDepth=0&maxDepth=90&minWidth=0&maxWidth=90&gia=1&ags=1&egl=0&oth=1&currency=USD&rowStart=',x*20,'&sortCol=price&sortDir=ASC',sep='')
     # load the webpage
     webpage=read_html(url)
     print('load page success')
     node=html_nodes(webpage,'td')
     nodedata=html_text(node)
     # get each element
     for (i in 0:19)
     {
       shape=nodedata[2+i*13]
       caret=nodedata[3+i*13]
       cut=nodedata[4+i*13]
       color=nodedata[5+i*13]
       clarity=nodedata[6+i*13]
       table=nodedata[7+i*13]
       depth=nodedata[8+i*13]
       cert=nodedata[9+i*13]
       measure=nodedata[10+i*13]
       price=nodedata[11+i*13]
      # print(paste(i,'-----',shape,caret,cut,color,clarity,table,depth,cert,measure,price))
       # pack as one dataframe
       one=c(shape,caret,cut,color,clarity,table,depth,cert,measure,price)
       # append to the total
       total=t(data.frame(t(total),one))
     }
     # save the data frame to a file
     write.csv(total,'diamonds.csv')
   }
cl=makeCluster(detectCores(logical = TRUE))
registerDoParallel(cl)
foreach(x=0:250000,combin='rbind' %dopar% test(x))
stopCluster()
cl 
 
 

