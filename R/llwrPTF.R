llwrPTF <- 
function(air,critical.PR, h.FC, h.WP,p.density, Bd,clay.content,org.carbon=NULL)
{
    if (is.null(org.carbon)) {org.carbon <- rep(2,times=length(Bd))}
    if (length(Bd) != length(clay.content)) {stop("incompatible dimensions!: please, check the length of Bd and clay.content")}
    if (length(clay.content) != length(org.carbon)) {stop("incompatible dimensions!: please, check the length of org.carbon and clay.content")}
    if (length(air) != 1) {stop("please, inform only one value to air")}
    if (length(critical.PR) != 1) {stop("please, inform only one value to critical.PR")}
    if (length(h.FC) != 1) {stop("please, inform only one value to h.FC")}
    if (length(h.WP) != 1) {stop("please, inform only one value to h.WP")}
    if (length(p.density) != 1) {stop("please, inform only one value to p.density")}

    # BRAZIL
     llwrBRAZIL <- function(Bd,air,critical.PR, h.FC, h.WP, p.density,clay.content)
     {
        thetaAIR <- c()
        thetaCC <- c()
        thetaWP <- c()
        thetaPR <- c()
        for (j in 1:length(Bd)){
           thetaAIR[j] <- (1-(Bd[j]/ p.density))-air     
           thetaCC[j] <- (1-(Bd[j]/p.density))*(h.FC/(3.0846 - 0.0441 * clay.content[j]))^(-1/(0.9354 + 0.2863 * clay.content[j]))
           thetaWP[j] <- (1-(Bd[j]/p.density))*(h.WP/(3.0846 - 0.0441 * clay.content[j]))^(-1/(0.9354 + 0.2863 * clay.content[j]))
           thetaPR[j] <- (critical.PR/((0.0121 - 0.0001 * clay.content[j])*(Bd[j] ^ (2.911 + 0.0428 * clay.content[j]))))^(1 / (-0.8546 - 0.0412 * clay.content[j]))
        }

        SL <- c()
        IL <- c()
        for (j in 1:length(Bd)){
             if (thetaAIR[j] < thetaCC[j]) {SL[j] <- thetaAIR[j]}
             else if (thetaAIR[j] > thetaCC[j]) {SL[j] <- thetaCC[j]}
             if (thetaWP[j] > thetaPR[j]) {IL[j] <- thetaWP[j]}
            else if (thetaWP[j] < thetaPR[j]) {IL[j] <- thetaPR[j]}
        }

        LLRW.Brazil <- (SL - IL)
        for (j in 1:length(LLRW.Brazil)){
             if (LLRW.Brazil[j]<0) {LLRW.Brazil[j]<-0}
         }
         return(LLRW.Brazil)
     }

     # CANADA
     llwrCANADIAN <- function(Bd,air,critical.PR, h.FC, h.WP, p.density,clay.content,org.carbon)
     {
       thetaAIR <- c()
       thetaCC <- c()
       thetaWP <- c()
       thetaPR <- c()
       for (j in 1:length(Bd)){
         thetaAIR[j] <- (1-(Bd[j]/ p.density))-air     
         thetaCC[j] <- exp((-4.1518 + 0.6851 * log(clay.content[j]) + 0.4025 * log(org.carbon[j]) + 0.2731 * log(Bd[j])) + (-0.5456 + 0.1127 * log(clay.content[j]) + 0.0223 * log(org.carbon[j]) + 0.1013 * log(Bd[j])) * log(h.FC / 10000))
         thetaWP[j] <- exp((-4.1518 + 0.6851 * log(clay.content[j]) + 0.4025 * log(org.carbon[j]) + 0.2731 * log(Bd[j])) + (-0.5456 + 0.1127 * log(clay.content[j]) + 0.0223 * log(org.carbon[j]) + 0.1013 * log(Bd[j])) * log(h.WP / 10000))
         thetaPR[j] <- exp((log(critical.PR) - (-3.6733 - 0.1447 * clay.content[j] + 0.7653 * org.carbon[j]) - ((3.8521 + 0.0963 * clay.content[j]) * log(Bd[j]))) / (-0.4805 - 0.1239 * clay.content[j] + 0.208 * org.carbon[j]))
       }
     SL <- c()
     IL <- c()
     for (j in 1:length(Bd)){
          if (thetaAIR[j] < thetaCC[j]) {SL[j] <- thetaAIR[j]}
          else if (thetaAIR[j] > thetaCC[j]) {SL[j] <- thetaCC[j]}
          if (thetaWP[j] > thetaPR[j]) {IL[j] <- thetaWP[j]}
          else if (thetaWP[j] < thetaPR[j]) {IL[j] <- thetaPR[j]}
     }
     LLRW.Canadian <- (SL - IL)
     for (j in 1:length(LLRW.Canadian)){
          if (LLRW.Canadian[j]<0) {LLRW.Canadian[j]<-0}
     }
        return(LLRW.Canadian)
    }

   LLWR.C <- llwrCANADIAN(Bd=Bd,clay.content=clay.content,
                         air=air,critical.PR=critical.PR, h.FC=h.FC, h.WP=h.WP,
                         p.density=p.density,org.carbon=org.carbon)
   LLWR.B <- llwrBRAZIL(Bd=Bd,clay.content=clay.content,
                       air=air,critical.PR=critical.PR, h.FC=h.FC, h.WP=h.WP,
                       p.density=p.density)
   LLWR <- data.frame(LLWR.B,LLWR.C)
   return(LLWR)
}
