
modelsA <- model_select(df,4)
modelsB <- model_select(df,5)
modelsC <- model_select(df,6)
#Draw the Maps

Sp22n <-"2022-04-05"
Sp22x <- "2022-05-31"

n <-"2022-04-05"
x <- "2022-05-31"

#Spring 2020
png(file="images/aatest.png",
    width=1000, height=1000, pointsize = 25) 

main <- "Spring 2020"
c <- "#DF5327"
 #Where:
  #n = date start
  #x = date end
  #c = color of dot
  
  #Creates new dataframe 
  dfz <- orderModels[orderModels$name >= as.Date(n) & orderModels$name <= as.Date(x),]

  #Creates the point of refences, the last observation all the values are on Models
  ref <- as.numeric(modelsA[[tail(dfz$order, n=1)]])
  
  #Creates first diagram
  taylor.diagram(ref,as.numeric(modelsA[[dfz$order[1]]]), col=c,pch=1, cex=1.2, pcex = 5,
                 main = main,
                 xlab="Standart Deviation (lb/acre)",
                 pos.cor=TRUE,
                 show.gamma = T,
                 sd.arcs = T)
  
  
  #Adds diagrams
  for (i in 2:length(dfz$order)){
    taylor.diagram(ref,as.numeric(models[[dfz$order[i]]]), add=TRUE,col="red",pch=i+13, cex=1.2,
                   pcex = 2.5,main = main) 
    #Cex = plotting text and symbols should be scaled relative to the default
    #pcex = point expansion for the plotted points.
  }
  

  for (i in 1:length(dfz$order)){
    taylor.diagram(ref,as.numeric(modelsB[[dfz$order[i]]]), add=TRUE,col="green",pch=i+13, cex=1.2,
                   pcex = 2.5,main = main) 
    #Cex = plotting text and symbols should be scaled relative to the default
    #pcex = point expansion for the plotted points.
  }
  
  for (i in 1:length(dfz$order)){
    taylor.diagram(ref,as.numeric(modelsC[[dfz$order[i]]]), add=TRUE,col="blue",pch=i+13, cex=1.2,
                   pcex = 2.5,main = main) 
    #Cex = plotting text and symbols should be scaled relative to the default
    #pcex = point expansion for the plotted points.
  }

  
  # Add legends
  Legends <- function(n,x){
    
    #Creates df outside function to use in naming legeds
    dfz <- orderModels[orderModels$name >= as.Date(n) & orderModels$name <= as.Date(x),]
    frst <- dfz$order[1] #Selects the first value of the list
    last <- tail(dfz$order, n=1) # Selects the Last value on the list
    
    
    return(dfz)
  }
  
  #legend("topright",legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col="black",title = "Dates")
  
  #legend(0.5,0.5, legend = c("Above", "Average", "Bellow"), col = c("#DF5327", "green", "blue"),
      #   pch = c(16), cex = 1.2, pt.cex = 2.5, title = "Models")
  
  
  legend("topright", title="Dates   Scenario", # << THIS IS THE HACKISH PART
         legend=c(substr(dfz$name, 6, 10), "Above", "Average", "Bellow"), 
         col=c(rep("black",length(substr(dfz$name, 6, 10))),'red',"green",'blue'), pch=c(1,15,16,17,rep(16,3)),
         bty="n", border=F, ncol=1)
  
dev.off()


