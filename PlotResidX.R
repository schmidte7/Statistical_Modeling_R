#######################################################################
# 
# Fonction R permettant de faire une analyse des residus par variable
# explicative.
#
#######################################################################
# Last modified: 23.07.2012.
#######################################################################
# Warranty: none.
#######################################################################

PlotResidX = function(matx, resid, col="blue", ylab="Residuals", ...){
  # 
  old.par = par(no.readonly=TRUE) # all par settings
  on.exit(par(old.par))
  #
	nb.var = ncol(matx)
	noms = colnames(matx)
	more.graph = FALSE
	if(nb.var==2){
		par(mfrow=c(1,2), pty="s")}
		else if(nb.var<5){
			par(mfrow=c(2,2), pty="s")}
			else if(nb.var<7){
				par(mfrow=c(2,3), pty="s")}
				else if(nb.var<10){
					par(mfrow=c(3,3), pty="s")}
					else if(nb.var<13){
						par(mfrow=c(3,4), pty="s")}
						else{
							par(mfrow=c(3,4), pty="s")
							more.graph = TRUE
	}
	iter = 1
	for(i in 1:nb.var){
		if(iter > 12 || iter >24 || iter > 36){
			par(mfrow=c(3,4), pty="s")
		}
		plot(matx[,i], resid, xlab=noms[i], col=col, ylab=ylab, ...)
		abline(h=0, lty=3, col="red")
		iter = iter+1
	}
}

#######################################################################

