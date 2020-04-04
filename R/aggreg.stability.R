aggreg.stability <- 
function (sample.id = NA, dm.classes, aggre.mass) 
{
    total.weight <- c() 
    for (j in 1:length(aggre.mass[,1])) {
           total.weight[j] <- sum(aggre.mass[j,])
    }
    proportion <- matrix(nrow=length(aggre.mass[,1]),ncol=length(aggre.mass[1,])) 
    for (j in 1:length(aggre.mass[,1])) {
       for (k in 1:length(aggre.mass[1,])) {      
           proportion[j,k] <- (aggre.mass[j,k]/total.weight[j])
       }
    }
   # MWD ------------
      SUM <- matrix(nrow=length(aggre.mass[,1]),ncol=length(aggre.mass[1,])) 
      for (j in 1:length(aggre.mass[,1])) {
        for (k in 1:length(aggre.mass[1,])) {       
           SUM[j,k] <- (dm.classes[k]*proportion[j,k])
        }
      }
     DMP <- c()
     for (j in 1:length(aggre.mass[,1])) {
         DMP[j] <- sum(SUM[j,])
        }
   # GMD ------------
      SUM2 <- matrix(nrow=length(aggre.mass[,1]),ncol=length(aggre.mass[1,])) 
      for (j in 1:length(aggre.mass[,1])) {
         for (k in 1:length(aggre.mass[1,])) {
           
           SUM2[j,k] <- (proportion[j,k]*log(dm.classes[k]))
          }
       }  
     DMG <- c()
     for (j in 1:length(aggre.mass[,1])) {
         DMG[j] <- exp(sum(SUM2[j,]))
        }
   prop <- round(proportion*100,0)
   colnames(prop) <- as.character(dm.classes)
   aggregation.indices <- data.frame(sample.id, MWD=DMP, GMD=DMG, 
      total.mass=total.weight, prop)
   return(aggregation.indices)
}
