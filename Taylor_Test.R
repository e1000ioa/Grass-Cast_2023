Taylor_Maker <- function(n,x, season, x_text, y_text,angle) {
  
  #Where:
  #n = date start
  #x = date end
  #Season = Spring or Summer
  
  year <- year(as.Date(n))
  
  modelsA <- model_select(df,4) #Bellow
  modelsB <- model_select(df,5) #Average
  modelsC <- model_select(df,6) #Above
  
  ColorA <- "#CB3446"
  ColorB <- "#3446CB"
  ColorC <- "#46CB34"
  
  #Creates new dataframe  filtering the dates
  dfz <- orderModels[orderModels$name >= as.Date(n) & orderModels$name <= as.Date(x),]
  
  #Creates the point of re fences, the last observation all the values are on Models
  ref <- as.numeric(modelsA[[tail(dfz$order, n=1)]])
  
  #Generates the plot
  png(file= paste0("images/taylor/TaylorDiagram_",season,year,".png"),
      width=1000, height=1000, pointsize = 25) 
  
  #Creates first diagram
  taylor.diagram(ref,as.numeric(modelsA[[dfz$order[1]]]), col=ColorA, pch=15, cex=1.2, pcex = 2.5,
                 main = paste(season,year),
                 xlab="Standart Deviation (lb/acre)",
                 pos.cor=TRUE,
                 show.gamma = T,
                 sd.arcs = T)
  
  #Adds diagrams
  for (i in 2:(length(dfz$order)-1)){
    
    #BElow avg Model
    taylor.diagram(ref,as.numeric(modelsA[[dfz$order[i]]]), add=TRUE,col=ColorA,pch=head(c(15,16,17,18,19,20), length(dfz$order)-1)[i], cex=1.2,
                   pcex = 2.5)
    
    #Separates the Null Hypothesis
    taylor.diagram(ref,as.numeric(modelsA[[tail(dfz$order, n=1)]]), add=TRUE,col="black",pch=4, cex=1.2,
                   pcex = 2.5) 
    
    #Cex = plotting text and symbols should be scaled relative to the default
    #pcex = point expansion for the plotted points.
  }
  
  
  for (i in 1:(length(dfz$order)-1)){
    #Average model
    taylor.diagram(ref,as.numeric(modelsB[[dfz$order[i]]]), add=TRUE,col=ColorB,pch=head(c(15,16,17,18,19,20), length(dfz$order)-1)[i], cex=1.2,
                   pcex = 2.5, order = -100) 
    #Above avg model
    taylor.diagram(ref,as.numeric(modelsC[[dfz$order[i]]]), add=TRUE,col=ColorC, pch=head(c(15,16,17,18,19,20), length(dfz$order)-1)[i], cex=1.2,
                   pcex = 2.5) 
  }
  
  legend("topright", title="Dates | Scenario", 
         legend=c(substr(dfz$name, 6, 10), "Bellow", "Average", "Above"), 
         col=c(rep("black",length(substr(dfz$name, 6, 10))),ColorA,ColorB,ColorC), pch=c(head(c(15,16,17,18,19,20), length(dfz$order)-1),13,rep(16,3)),
         bty="n", border=F, ncol=1, x.intersp = 1.5)
  
  # Add text box
  text(x = x_text, y = y_text, pos = 1, srt = angle, label = "RMSE (lb/acre)", font = 3, cex = 0.8)
  
  dev.off()

}

#2020
Taylor_Maker("2020-05-15","2020-06-02","Spring",90,54,20)
Taylor_Maker("2020-06-16","2020-09-01","Summer",180,202,20)

#2021
Taylor_Maker("2021-04-14","2021-06-01","Spring",140,110,12)
Taylor_Maker("2021-06-16","2021-08-24","Summer",250,400,20)

#2022
Taylor_Maker("2022-04-05","2022-05-31","Spring",90,55,18)
Taylor_Maker("2022-06-14","2022-09-01","Summer",290,420,12)

