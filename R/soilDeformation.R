soilDeformation <- function(stress,p.density, iBD,
                            N, CI, k, k2, m,graph=FALSE,...)
{

   if (length(stress) != length(p.density)) {stop("incompatible dimensions!: please, check the length of the arguments")}
   else if (length(stress) != length(iBD)) {stop("incompatible dimensions!: please, check the length of the arguments")}
   else if (length(stress) != length(N)) {stop("incompatible dimensions!: please, check the length of the arguments")}
   else if (length(stress) != length(CI)) {stop("incompatible dimensions!: please, check the length of the arguments")}
   else if (length(stress) != length(k)) {stop("incompatible dimensions!: please, check the length of the arguments")}
   else if (length(stress) != length(k2)) {stop("incompatible dimensions!: please, check the length of the arguments")}
   else if (length(stress) != length(m)) {stop("incompatible dimensions!: please, check the length of the arguments")}
       
      # Determinando Parametros
        lp <- log(stress)               # log do stress
        Vi <- (p.density/iBD)           # volume inicial

        NYL <- c()                      # N da YL
        for (j in 1:length(N)){
             NYL[j] <- N[j]+CI[j]*(0-m[j])
             } 

        XRCL2 <- c()                    # x da RCL 2 com a YL
        for (j in 1:length(N)){
             XRCL2[j] <- (NYL[j]-Vi[j])/(-k[j]+CI[j])
             } 

        YRCL2 <- c()                    # y na RCL 2 com a YL
        for (j in 1:length(N)){
             YRCL2[j] <- (Vi[j]-k[j]*XRCL2[j])
             } 

        NRCL2 <- c()                    # N da RCL 2
        for (j in 1:length(N)){
             NRCL2[j] <- (YRCL2[j] + k2[j]*XRCL2[j])
             } 

        XRCL2.VCL <- c()                # x na RCL 2 com a VCL                
        for (j in 1:length(N)){
             XRCL2.VCL[j] <- (N[j]-NRCL2[j])/(-k2[j]+CI[j])
             } 

        YRCL2.VCL <- c()                # y na RCL 2 com a VCL 
        for (j in 1:length(N)){
             YRCL2.VCL[j] <- (NRCL2[j]-k2[j]*XRCL2.VCL[j])
             } 

        # Funcoes
        
          fVCL <- function (x) N[1]-CI[1]*x

          fYL <- function (x) NYL[1]-CI[1]*x
 
          fRCL <- function (x) Vi[1]-k[1]*x
          NRCL <- fRCL(0)
 
          fRCL2 <- function (x) NRCL2[1]-k2[1]*x
          

   # Graph
          if (graph) {
              curve(fRCL2, from=0,to=10,lwd=2, xlab="ln[Stress(kPa)]",
                    ylab="v", col="darkgray", lty=2,...)
              segments(x0=XRCL2[1],x1=XRCL2.VCL[1],y0=YRCL2[1],
                       y1=YRCL2.VCL[1],col="darkgray", lty=1, lwd=2,...)
              curve(fRCL,lwd=2, col="blue", add=T,...)
              curve(fYL,lwd=3, col="red", add=T,...) 
              curve(fVCL,lwd=3, add=T,...)
  
        if (lp[1]>0 & lp[1]<=XRCL2[1]) 
           {y0 <- Vi[1]
            x0 <- log(1)
            y1 <- NRCL[1]-k[1]*lp[1]
            x1 <- lp[1]}
        else if (lp[1]>XRCL2[1] & lp[1]<=XRCL2.VCL[1])
           {y0 <- NRCL2[1]-k2[1]*lp[1]
            y0 <- y0+k[1]*lp[1]
            x0 <- log(1)
            y1 <- NRCL2[1]-k2[1]*lp[1]
            x1 <- lp[1]}
        else if (lp[1]>XRCL2.VCL[1])
           {y0 <- N[1]-CI[1]*lp[1]
            y0 <- y0+k[1]*lp[1]
            x0 <- log(1)
            y1 <- N[1]-CI[1]*lp[1]
            x1 <- lp[1]}

      segments(x0=x0,x1=x1,y0=y0,
               y1=y1,col="orange", lty=3, lwd=2,...)
      segments(x0=lp[1],x1=lp[1],y0=0,
              y1=y1,col="orange", lty=3, lwd=2,...)
      points(x=lp[1],y=y1,pch=16,col="orange")
        }

    volume <- c()
    for (j in 1:length(N)){
         if (lp[j]>0 & lp[j]<=XRCL2[j]) {volume[j] <- (Vi[j])}
         else if (lp[j]>XRCL2[j] & lp[j]<=XRCL2.VCL[j]) 
                 {volume[j] <- (NRCL2[j]-k2[j]*lp[j])
                  volume[j] <-  volume[j]+k[j]*lp[j]}
         else if (lp[j]>XRCL2.VCL[j]) 
                 {volume[j] <- (N[j]-CI[j]*lp[j])
                  volume[j] <-  volume[j]+k[j]*lp[j]}
         }
    
    # OUT
    round(iBD,3)
    fBD <- round((p.density/volume),4)
    vf <- (p.density/fBD)
    increasing <- round(((fBD*100)/iBD)-100, 2)
    out <- data.frame(iBD,fBD,round(Vi,4),round(vf,4), increasing)
    colnames(out) <- c("iBD","fBD","vi","vf","I%")
    return(out)
}
