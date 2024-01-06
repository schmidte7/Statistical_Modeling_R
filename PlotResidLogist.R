#######################################################################
# 
# Fonction R construisant les graphiques des résidus du type "deviance"
# et du type "pearson" pour la régression logisitque classique (binaire).
#
#######################################################################
# Last modified: 15.09.2011.
#######################################################################
# Warranty: none.
#######################################################################

PlotResidLogist = function(obj, id.n=3, col="blue", id.do=TRUE, ...)
{
  # 
  old.par = par(no.readonly=TRUE) # all par settings
  on.exit(par(old.par))
  # 
  resid.dev = residuals.glm(obj, type="deviance")
  n.dev = length(resid.dev)
  ord.dev = order(abs(resid.dev))
  id.dev = ord.dev[(n.dev-id.n+1):n.dev]
  # 
  resid.pea = residuals.glm(obj, type="pearson")
  n.pea = length(resid.pea)
  ord.pea = order(abs(resid.pea))
  id.pea = ord.pea[(n.pea-id.n+1):n.pea]
  # 
  par(mfrow=c(1,2), pty="s")
  plot(1:n.dev, resid.dev, xlab="Index", ylab="Deviance residuals", col=col, ...)
  if(id.do){
    text((1:n.dev)[id.dev]+3, resid.dev[id.dev],
         as.character((1:n.dev)[id.dev]), col="black", ...)
  }
  abline(h=0, lty=3, col="red")
  plot(1:n.pea, resid.pea, xlab="Index", ylab="Pearson residuals", col=col, ...)
  if(id.do){
  text((1:n.pea)[id.pea]+3, resid.pea[id.pea],
       as.character((1:n.pea)[id.pea]), col="black", ...)
  }
  abline(h=0, lty=3, col="red", cex=1.2)
}

#######################################################################
