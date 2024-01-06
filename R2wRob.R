#######################################################################
# 
# Fonction R permettant de calculer des mesures robustes du coefficient
# de determination R^2 (utilisant la fonction "lmRob" du package "robust").
#
# Source: http://www.unige.ch/fapse/mad/equipe/renaud/soft/robust.html
# Reference: Renaud, O. & Victoria-Feser, M.-P. (2010). A robust
#             coefficient of determination for regression.
#             Journal of Statistical Planning and Inference,
#            140, 1852-1862.
#
#######################################################################
# Last modified: 09.09.2013.
#######################################################################
# Warranty: none.
#######################################################################

R2wRob = function(rob.obj, ShortOutput=TRUE)
  {
  # 
  wt.bisquare = function(u, cc) {
    U <- abs(u/cc)
    w <- ((1. + U) * (1. - U))^2.
    w[U > 1.] <- 0.
    w
  }
  weight.rob=function(rob.obj, c){
    resid.rob=rob.obj$resid
    scale.rob=(rob.obj$scale)*rob.obj$df.residual/length(resid.rob)
    resid.rob= resid.rob/scale.rob
    weight=wt.bisquare(resid.rob, cc=c)
  }
  # 
  if (attr(rob.obj, "class") !="lmRob")
    stop("This function works only on lmRob objects!")
  pred = rob.obj$fitted.values
  resid = rob.obj$resid
  resp = resid+pred
  #
  tmp.eff = rob.obj$robust.control$efficiency
  tmp.c = lmRob.c(rob.obj$robust.control$efficiency)
  cat(paste("\nlmRob with ", round(100*tmp.eff, 0),"% efficiency (c=", format(signif(tmp.c)), ")\n", sep=""))
  wgt = weight.rob(rob.obj, c=tmp.c)
  #
  correc = lmRob.a(tmp.c)
  #
  scale.rob = rob.obj$scale
  resp.mean = sum(wgt*resp)/sum(wgt)
  pred.mean = sum(wgt*pred)/sum(wgt)
  yMy = sum(wgt*(resp-resp.mean)^2)
  rMr = sum(wgt*resid^2)
  r2 = (yMy-rMr) / yMy
  r2correc= (yMy-rMr) / (yMy-rMr +rMr*correc)
  r2adjcor = 1-(1-r2correc) * (length(resid)-1) / (length(resid)-length(rob.obj$coefficients)-1)  
  r2adj = 1-(1-r2) * (length(resid)-1) / (length(resid)-length(rob.obj$coefficients)-1)  
  #
  cat("\nRobust multiple R-squared:", format(signif(r2)))
  cat(", Robust adjusted R-squared:", format(signif(r2adj)), "\n")
  #  
  if (ShortOutput)  cat("\n")
  if (!ShortOutput) cat(paste("\nBy applying a correction for consistency considerations (a=", format(signif(correc)), "):\n", sep=""))
  if (!ShortOutput) cat("Robust multiple R-squared:", format(signif(r2correc)))
  if (!ShortOutput) cat(", Robust adjusted R-squared:", format(signif(r2adjcor)), "\n\n")
  #
  invisible(list(R2wRob.NoCorrection=r2,
                 R2wRob.Adjusted=r2adj, 
                 R2wRob.WithCorrection=r2correc,
                 R2wRob.AdjustedWithCorrection=r2adjcor))            
}

# require(robust)
# 
# data(stack.dat)
# 
# stack.rob1 = lmRob(Loss ~ ., data=stack.dat, control=lmRob.control(efficiency=0.9))
# summary(stack.rob1)
# 
# R2wRob(stack.rob1)
# 
# R2wRob(stack.rob1, ShortOutput=FALSE
# 
# R2wRob(lmRob(Loss ~ ., data=stack.dat, control=lmRob.control(efficiency=0.80)))
# R2wRob(lmRob(Loss ~ ., data=stack.dat, control=lmRob.control(efficiency=0.90)))
# R2wRob(lmRob(Loss ~ ., data=stack.dat, control=lmRob.control(efficiency=0.95)))
# R2wRob(lmRob(Loss ~ ., data=stack.dat, control=lmRob.control(efficiency=0.99)))
# R2wRob(lmRob(Loss ~ ., data=stack.dat, control=lmRob.control(efficiency=0.9999)))
# 
# summary(lm(Loss ~ ., data=stack.dat))

#######################################################################
# 
# Fonctions pour calculer la constance c du "bisquare/biweight" a partir
#   de l'efficiency.
#
# Source: http://www.unige.ch/ses/dsec/staff/faculty/Cantoni-Eva/Books/RobustBiostat.html
#         -> Fonctions dans "Chapter3_functions.r".
#
#######################################################################
#
# lmRob.c(0.95)  # -> 4.685
# lmRob.c(0.9)   # -> 3.881
#
#######################################################################
  
Norm.TM = function(cc){
  norm.trunc.mom=function(mom,cc,Lm2){drop(-cc^(mom-1)*dnorm(cc)+(mom-1)*pnorm(cc)*Lm2)}
  norm.mom=function(mom){drop(factorial(mom)/(2^(mom/2)*factorial(mom/2)))}
  L0=pnorm(-cc)
  L2=norm.trunc.mom(2,-cc,L0)
  L4=norm.trunc.mom(4,-cc,L2)
  L6=norm.trunc.mom(6,-cc,L4)
  L8=norm.trunc.mom(8,-cc,L6)
  L10=norm.trunc.mom(10,-cc,L8)
  L12=norm.trunc.mom(12,-cc,L10)
  L14=norm.trunc.mom(14,-cc,L12)
  int.0=1-2*pnorm(-cc)
  int.2=norm.mom(2)-2*L2
  int.4=norm.mom(4)-2*L4
  int.6=norm.mom(6)-2*L6
  int.8=norm.mom(8)-2*L8
  int.10=norm.mom(10)-2*L10
  int.12=norm.mom(12)-2*L12
  int.14=norm.mom(14)-2*L14
  drop(c(int.0,int.2,int.4,int.6,int.8,int.10,int.12,int.14))
}

lmRob.c = function(effw,tolw=.Machine$double.eps^0.25,up=6){
  fun=function(cc,eff){drop((eff-lmRob.eff(cc))^2)}
  optimize(f=fun, interval=c(1,up), eff=effw, tol=tolw)[[1]]
}

lmRob.eff = function(cc){
  int=Norm.TM(cc)
  I0=int[1]
  I2=int[2]
  I4=int[3]
  I6=int[4]
  I8=int[5]
  I10=int[6]
  I12=int[7]
  I14=int[8]
  eff=((5/cc^4)*I4-(6/cc^2)*I2+I0)^2/((1/cc^8)*I10-(4/cc^6)*I8+(6/cc^4)*I6-(4/cc^2)*I4+I2)
  eff
}
  
#######################################################################
# 
# Fonctions pour calculer la constance a partir de c.
# 
#######################################################################
#
# lmRob.a(4.685)  # -> 1.208
# lmRob.a(3.881)  # -> 1.318
#
#######################################################################

lmRob.a = function(cc){
  int=Norm.TM(cc)
  I0=int[1]
  I2=int[2]
  I4=int[3]
  I6=int[4]
  ewr = (1/cc^4)*I4 - (2/cc^2)*I2 + I0
  erp = (1/cc^4)*I6 - (2/cc^2)*I4 + I2
  a=ewr/erp
  a
}

#######################################################################

