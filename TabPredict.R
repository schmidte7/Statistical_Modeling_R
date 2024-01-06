#######################################################################
# 
# Fonction R permettant de calculer des IC classiques ("lm") et
# robustes ("lmRob") pour les résponses prédites moyennes d'une
# régression.
#
#######################################################################
# Last modified: 15.02.2010.
#######################################################################
# Warranty: none.
#######################################################################

TabPredict = function(object, conf.level=0.95, newdata=NULL)
  {
    if(inherits(object, "lmRob"))
      {
       if(is.null(newdata)) predobj = predict.lmRob(object, se.fit=TRUE)
       if(!is.null(newdata)) predobj = predict.lmRob(object, newdata=newdata, se.fit=TRUE)
       alpha = 1-conf.level
       t.value =  qt(1-alpha/2, predobj$df)
       fit = predobj$fit
       lower = fit - t.value * predobj$se.fit
       upper = fit + t.value * predobj$se.fit
       predobj = data.frame(Fit=fit, LCL=lower, UCL=upper)
      }
    if(inherits(object, "lm"))
      {
       predobj = predict.lm(object, newdata=newdata, interval="confidence", level=conf.level)
       colnames(predobj) = c("Fit", "LCL", "UCL")
      }
    predobj
}

#######################################################################

