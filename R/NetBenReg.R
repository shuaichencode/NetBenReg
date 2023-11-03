
#' Kaplan-Meier Function
#'
#' This function estimates the survival probability using Kaplan-Meier method, assuming censoring is not covariate-dependent. used to calculate weights of inverse probability of censoring.
#' @param X a vector containing continuous positive follow-up time.
#' @param delta a vector containing binary indicator of event, 1 - complete, 0 - censored.
#' @return a vector containing the survival probability at each follow-up time
#' @keywords Kaplan-Meier

KM<-function(X,delta)
{
  n=length(X)
  K=numeric(n)
  k<-1
  ord<-order(X)
  u=1
  while(u<=n)
  {
      s<-u+1      
      while((s<=n)&(X[ord[u]]==X[ord[s]])) s=s+1
      k<-k*(1-sum(delta[ord[u:(s-1)]]==1)/(n-u+1))
      K[ord[u:(s-1)]]<-k
      u=s
  }
  return(K)
}



#' Clean Data Function
#'
#' This function keeps data with non-missing values only, drops categorical levels unused in final data, and drops categorical variables with only 1 level.
#' @param data a data.frame containing data to be cleaned.
#' @return a data.frame containing the cleaned data

clean_data<-function(data)
{
  data=dplyr::filter(data, dplyr::if_all(tidyselect::all_of(names(data)),  ~ !is.na(.)))	#keep non-missing only
  data=dplyr::mutate(data, dplyr::across(tidyselect::where(is.factor), droplevels)) 	#drop some levels unused in final data
  data=dplyr::select(data, which(sapply(data,nlevels)!=1))	#drop variables with only 1 level
  return(data)
}



#' Naive method using complete case only for net-benefit regression
#'
#' This function uses complete case only for net-benefit regression, not recommended but included for comparison.
#' @param X a vector containing continuous positive follow-up time.
#' @param delta a vector containing binary indicator of event (such as death), 1 - complete, 0 - censored.
#' @param Cost.total a vector containing observed total costs.
#' @param Eff.total a vector containing observed total effectiveness. If not provided, assume effectiveness is survival. 
#' @param group a vector containing binary treatment indicator, 1 - treatment, 0 - control.
#' @param Z a vector, matrix, or dataframe containing covariates for net-benefit regression. If not provided, will do unadjusted analysis using simple regression. Defaults to NULL.
#' @param interaction a vector containing covariate names to be included in interactions with treatment, must be a subset of variable names in Z, otherwise the variable not in Z will be ignored. Defaults to NULL.
#' @param lambda a vector or scalar containing cost-effectiveness threshold values (e.g., willingness-to-pay for 1 unit of additional effectiveness).
#' @param Cost.only logical. If TRUE, fit a regression with dependent variable as cost. Defaults to FALSE.
#' @keywords net-benefit regression, complete case only


NetBenReg_CC<-function(X,delta,Cost.total,Eff.total,group,Z=NULL,interaction=NULL,lambda=NULL,Cost.only=FALSE)
{
  lambda1=lambda
  if(is.null(lambda))lambda1=1	
  if(Cost.only)lambda1=0

  #total cost and surv among complete cases only
  if(Cost.only)DV=Cost.total else DV=lambda1*Eff.total-Cost.total
  id=1:length(X)

  if(is.null(Z))	Z=matrix(0,length(X),0) 
 
  data=data.frame(DV,X,delta,group,Z,id) 
  data=data[data$delta==1,]
  data=clean_data(data)
  main.name=names(data)[-c(1:4,dim(data)[2])]
  Z.name=paste(main.name,collapse = "+")

  if(!is.null(interaction))
  {
	int.name= intersect(main.name,interaction)	#interaction terms must be in Z
	AZ.name=paste(paste("group",int.name,sep=":"),collapse = "+")
	if(length(int.name)>0)form=paste("DV~group+",Z.name,"+",AZ.name,sep="") else interaction=NULL
  }else int.name=character(0)

  if(is.null(interaction)){form=paste("DV~group+",Z.name,sep="")}
  if(dim(data)[2]==5){form="DV~group"}

  #use GEE to obtain robust SE
  fit.gee<-geepack::geeglm(stats::as.formula(form),id=id,data=data)

  if(is.null(lambda)) {
	if(!Cost.only) {
		Reg.type="Effect"
	} else {
		Reg.type="Cost"
	}
	lambda=NA
  } else {
	Reg.type="NBR"
  }

  est=summary(fit.gee)$coef[,1]
  se=summary(fit.gee)$coef[,2]
  if((is.na(lambda))|length(int.name)>0) CEAC=NA else CEAC=stats::pnorm(est[2]/se[2])	#if there is interaction, does not provide CEAC here

  return(list(Method='CC',lambda=lambda,est=est,se=se,covariance=summary(fit.gee)$cov.unscaled,
	coef.table=summary(fit.gee)$coef,CEAC=CEAC,int.name=int.name,covar1st=data[1,-c(1:4,dim(data)[2]),drop=FALSE],
	Reg.type=Reg.type,n0=length(X),n=dim(data)[1]))			
}



#' Naive method using all data ignoring censoring status for net-benefit regression
#'
#' This function uses all data ignoring censoring status for net-benefit regression, not recommended but included for comparison.
#' @param X a vector containing continuous positive follow-up time.
#' @param delta a vector containing binary indicator of event (such as death), 1 - complete, 0 - censored.
#' @param Cost.total a vector containing observed total costs.
#' @param Eff.total a vector containing observed total effectiveness. If not provided, assume effectiveness is survival. 
#' @param group a vector containing binary treatment indicator, 1 - treatment, 0 - control.
#' @param Z a vector, matrix, or dataframe containing covariates for net-benefit regression. If not provided, will do unadjusted analysis using simple regression. Defaults to NULL.
#' @param interaction a vector containing covariate names to be included in interactions with treatment, must be a subset of variable names in Z, otherwise the variable not in Z will be ignored. Defaults to NULL.
#' @param lambda a vector or scalar containing cost-effectiveness threshold values (e.g., willingness-to-pay for 1 unit of additional effectiveness).
#' @param Cost.only logical. If TRUE, fit a regression with dependent variable as cost. Defaults to FALSE.
#' @keywords net-benefit regression, all data ignoring censoring status

NetBenReg_AL<-function(X,delta,Cost.total,Eff.total,group,Z=NULL,interaction=NULL,lambda=NULL,Cost.only=FALSE)
{
  lambda1=lambda
  if(is.null(lambda))lambda1=1
  if(Cost.only)lambda1=0

  if(Cost.only)DV=Cost.total else DV=lambda1*Eff.total-Cost.total
  id=1:length(delta)

  if(is.null(Z)) Z=matrix(0,length(delta),0) 
 
  data=data.frame(DV,X,delta,group,Z,id) 
  data=clean_data(data)
  main.name=names(data)[-c(1:4,dim(data)[2])]
  Z.name=paste(main.name,collapse = "+")

  if(!is.null(interaction))
  {
	int.name= intersect(main.name,interaction)	#interaction terms must be in Z
	AZ.name=paste(paste("group",int.name,sep=":"),collapse = "+")
	if(length(int.name)>0)form=paste("DV~group+",Z.name,"+",AZ.name,sep="") else interaction=NULL
  }else int.name=character(0)

  if(is.null(interaction)){form=paste("DV~group+",Z.name,sep="")}
  if(dim(data)[2]==5){form="DV~group"}

  #use GEE to obtain robust SE
  fit.gee<-geepack::geeglm(stats::as.formula(form),id=id,data=data)

  if(is.null(lambda)) {
	if(!Cost.only) {
		Reg.type="Effect"
	} else {
		Reg.type="Cost"
	}
	lambda=NA
  } else {
	Reg.type="NBR"
  }

  est=summary(fit.gee)$coef[,1]
  se=summary(fit.gee)$coef[,2]
  if((is.na(lambda))|length(int.name)>0) CEAC=NA else CEAC=stats::pnorm(est[2]/se[2])	#if there is interaction, does not provide CEAC here

  return(list(Method='AL',lambda=lambda,est=est,se=se,covariance=summary(fit.gee)$cov.unscaled,
	coef.table=summary(fit.gee)$coef,CEAC=CEAC,int.name=int.name,covar1st=data[1,-c(1:4,dim(data)[2]),drop=FALSE],
	Reg.type=Reg.type,n0=length(X),n=dim(data)[1]))			
}



#' Simple weighted method for net-benefit regression
#'
#' This function uses simple weighted (unpartitioned) method for net-benefit regression.
#' @param X a vector containing continuous positive follow-up time.
#' @param delta a vector containing binary indicator of event (such as death), 1 - complete, 0 - censored.
#' @param Cost.total a vector containing observed total costs.
#' @param Eff.total a vector containing observed total effectiveness. If not provided, assume effectiveness is survival. 
#' @param group a vector containing binary treatment indicator, 1 - treatment, 0 - control.
#' @param Z a vector, matrix, or dataframe containing covariates for net-benefit regression. If not provided, will do unadjusted analysis using simple regression. Defaults to NULL.
#' @param interaction a vector containing covariate names to be included in interactions with treatment, must be a subset of variable names in Z, otherwise the variable not in Z will be ignored. Defaults to NULL.
#' @param Sep.K logical, if TRUE, estimate K (survival function of censoring time, used in inverse probability of censoring weighting) using Kaplan-Meier estimator within each group separately. Defaults to TRUE.
#' @param lambda a vector or scalar containing cost-effectiveness threshold values (e.g., willingness-to-pay for 1 unit of additional effectiveness).
#' @param Cost.only logical. If TRUE, fit a regression with dependent variable as cost. Defaults to FALSE.
#' @param Aux.X. a vector, matrix, or dataframe containing auxiliary covariates (e.g., for propensity model). Defaults to NULL.
#' @param returnData logical. If TRUE, return the cleaned data used in analysis. Defaults to FALSE.
#' @keywords net-benefit regression, simple weighted

NetBenReg_SW<-function(X,delta,Cost.total,Eff.total,group,Z=NULL,interaction=NULL,Sep.K=TRUE,lambda=NULL,Cost.only=FALSE,Aux.X=NULL,returnData=FALSE)
{
  lambda1=lambda
  if(is.null(lambda))lambda1=1
  if(Cost.only)lambda1=0

  if(Cost.only)DV=Cost.total else DV=lambda1*Eff.total-Cost.total
  if(is.null(Z)) Z=matrix(0,length(delta),0) 

  if(is.null(Aux.X)) data=data.frame(DV,X,delta,group,Z) else data=data.frame(DV,X,delta,group,Z,Aux.X) 
  data=clean_data(data)
  if(dim(Z)[2]>0)Z.name=paste(names(data)[5:(dim(Z)[2]+4)],collapse = "+")else Z.name=character(0)
  covar1st=data[1,-c(1:4),drop=FALSE]

  #create covariates matrix, need to create dummy for factor
  if(!is.null(interaction))
  {
	int.name= intersect(names(data)[5:(dim(Z)[2]+4)],interaction)	#interaction terms must be in Z
	AZ.name=paste(paste("group",int.name,sep=":"),collapse = "+")
	if(length(int.name)>0)form=paste("DV~group+",Z.name,"+",AZ.name,sep="") else interaction=NULL
  } else int.name=character(0)
  if(is.null(interaction)){form=paste("DV~group+",Z.name,sep="")}
  if(dim(Z)[2]==0){form="DV~group"}
  covar=stats::model.matrix(stats::as.formula(form), data = data)

  # estimate K by K-M (assume no covariates, otherwise need to use Cox model)
  if(!Sep.K)data$K=KM(data$X, 1-data$delta)
  if(Sep.K){
	data$K=0
	data$K[data$group==0]=KM(data$X[data$group==0], 1-data$delta[data$group==0])
	data$K[data$group==1]=KM(data$X[data$group==1], 1-data$delta[data$group==1])
  }
  if (sum(data$K[data$delta==1]==0)>0) stop("Estimate of probability of censoring = 0 at some time point. Time limit L may be too large.")
  if (min(data$K[data$delta==1])<0.1) warning("Estimate of probability of censoring < 10% at some time point. To have more stable results, could choose a smaller time limit L.")
  data$w0=data$delta/data$K				#weight
  data$w0[data$delta==0]=0

  #coef
  #to prevent singular matrix, drop variables with \sum w0*X^2=0 from covar, and use 0 as their coef est
  index.drop=which(apply(covar^2*data$w0,2,sum)==0)
  if(length(index.drop)>0)
  {
  	covar1=covar[,-index.drop]
	est.coef=numeric(dim(covar)[2])  
  	est.coef[-index.drop]=solve(t(covar1)%*%(covar1*data$w0))%*%(apply(covar1*data$w0*data$DV,2,sum))
	est.coef[index.drop]=0
  } else est.coef=solve(t(covar)%*%(covar*data$w0))%*%(apply(covar*data$w0*data$DV,2,sum))
  
  #use formula to obtain robust SE
  A_mat=t(covar)%*%covar
  Q_mat=tmp1=matrix(0,dim(data)[1],dim(covar)[2])
  N.largerX=numeric(dim(data)[1])
  for(i in 1:dim(data)[1])
  {
	N.largerX[i]=sum(data$X>=data$X[i])
	Q_mat[i,]=apply(covar*data$w0*(data$X>data$X[i])*c(data$DV-covar%*%est.coef),2,sum)/N.largerX[i]
  }
  for(i in 1:dim(data)[1])
	tmp1[i,]=apply(Q_mat*(1-data$delta)*(data$X<=data$X[i])/N.largerX,2,sum)	
  tmp2=covar*data$w0*c(data$DV-covar%*%est.coef)+Q_mat*(1-data$delta)-tmp1
  B_mat=t(tmp2)%*%tmp2
  cov_mat=solve(A_mat)%*%B_mat%*%solve(A_mat)
  se=sqrt(diag(cov_mat))

  coef.table=data.frame(Estimate=est.coef,Std.err=se,Wald=(est.coef/se)^2,p=2-2*stats::pnorm(abs(est.coef/se)))
  row.names(coef.table)=colnames(covar)

  if(is.null(lambda)) {
	if(!Cost.only) {
		Reg.type="Effect"
	} else {
		Reg.type="Cost"
	}
	lambda=NA
  } else {
	Reg.type="NBR"
  }

  if((is.na(lambda))|length(int.name)>0) CEAC=NA else CEAC=stats::pnorm(est.coef[2]/se[2])	#if there is interaction, does not provide CEAC here

  n=dim(data)[1]
  if(!returnData)data=NULL
  return(list(Method='SW',lambda=lambda,est=c(est.coef),se=se,covariance=cov_mat,coef.table=coef.table,CEAC=CEAC,
	int.name=int.name,covar1st=covar1st,Reg.type=Reg.type,data=data,form=form,n0=length(X),n=n))
}




#' Partitioned method for net-benefit regression
#'
#' This function uses partitioned method for net-benefit regression.
#' @param X a vector containing continuous positive follow-up time.
#' @param delta a vector containing binary indicator of event (such as death), 1 - complete, 0 - censored.
#' @param Cost.grp a matrix or dataframe containing observed grouped costs, Cost.grp[i,j] is observed cost of ith people accumulated in jth interval.
#' @param Eff.grp a matrix or dataframe containing observed grouped effectiveness. Eff.grp[i,j] is observed effectiveness of ith people accumulated in jth interval. If not provided, assume effectiveness is survival. 
#' @param Part.times a vector containing end time points of each time interval, must be monotonically increasing.
#' @param group a vector containing binary treatment indicator, 1 - treatment, 0 - control.
#' @param Z a vector, matrix, or dataframe containing covariates for net-benefit regression. If not provided, will do unadjusted analysis using simple regression. Defaults to NULL.
#' @param interaction a vector containing covariate names to be included in interactions with treatment, must be a subset of variable names in Z, otherwise the variable not in Z will be ignored. Defaults to NULL.
#' @param Sep.K logical, if TRUE, estimate K (survival function of censoring time, used in inverse probability of censoring weighting) using Kaplan-Meier estimator within each group separately. Defaults to TRUE.
#' @param lambda a vector or scalar containing cost-effectiveness threshold values (e.g., willingness-to-pay for 1 unit of additional effectiveness).
#' @param Cost.only logical. If TRUE, fit a regression with dependent variable as cost. Defaults to FALSE.
#' @param Aux.X. a vector, matrix, or dataframe containing auxiliary covariates (e.g., for propensity model). Defaults to NULL.
#' @param returnData logical. If TRUE, return the cleaned data and some more results used in analysis. Defaults to FALSE.
#' @keywords net-benefit regression, partitioned

NetBenReg_PT<-function(X,delta,Cost.grp,Eff.grp,Part.times,group,Z=NULL,interaction=NULL,Sep.K=TRUE,lambda=NULL,Cost.only=FALSE,Aux.X=NULL,returnData=FALSE)
{
  lambda1=lambda
  if(is.null(lambda))lambda1=1
  if(Cost.only)lambda1=0

  if(is.null(Z)) Z=matrix(0,length(delta),0) 

  if(is.null(Aux.X)) data=data.frame(Cost.grp,Eff.grp,X,delta,group,Z)  else data=data.frame(Cost.grp,Eff.grp,X,delta,group,Z,Aux.X) 
  data=clean_data(data)
  if(dim(Z)[2]>0)Z.name=paste(names(data)[(dim(Cost.grp)[2]+dim(Eff.grp)[2]+4):
	(dim(Z)[2]+dim(Cost.grp)[2]+dim(Eff.grp)[2]+3)],collapse = "+")else Z.name=character(0)
  covar1st=data[1,-c(1:(dim(Cost.grp)[2]+dim(Eff.grp)[2]+3)),drop=FALSE]

  #create covariates matrix, need to create dummy for factor
  if(!is.null(interaction))
  {
	int.name= intersect(names(data)[(dim(Cost.grp)[2]+dim(Eff.grp)[2]+4):(dim(Z)[2]+dim(Cost.grp)[2]+dim(Eff.grp)[2]+3)],interaction)	#interaction terms must be in Z
	AZ.name=paste(paste("group",int.name,sep=":"),collapse = "+")
	if(length(int.name)>0)form=paste("~group+",Z.name,"+",AZ.name,sep="") else interaction=NULL
  } else int.name=character(0)
  if(is.null(interaction)){form=paste("~group+",Z.name,sep="")}
  if(dim(Z)[2]==0){form="~group"}
  covar=stats::model.matrix(stats::as.formula(form), data = data)

  x=w0=d=totalQ=DV=K.new=matrix(0,dim(data)[1],length(Part.times)-1)
  K=numeric(dim(data)[1])
  coef.split=matrix(0,length(Part.times)-1,dim(covar)[2])
  Ksi=array(0,c(length(Part.times)-1,dim(data)[1],dim(covar)[2]))
  B.split=array(0,c(length(Part.times)-1,length(Part.times)-1,dim(covar)[2],dim(covar)[2]))

  Cost.grp1=data[,1:dim(Cost.grp)[2]]
  Eff.grp1=data[,(dim(Cost.grp)[2]+1):(dim(Cost.grp)[2]+dim(Eff.grp)[2])]

## loop for each month
for(j in 2:length(Part.times))
{
   trunc.time=Part.times[j] 
 
   # truncate follow-up time for each interval
   # set death indicator to be 1 if truncated
   for(l in 1:dim(data)[1]) 
   {
      x[l,j-1]<-min(data$X[l],trunc.time)          #new follow-up x[l,j-1] of lth patient for j-1th interval
      if(data$X[l]>=trunc.time)  d[l,j-1]<-1  else d[l,j-1]=data$delta[l]         #d[l,j]=new death indicator of lth patient for j-1th interval
   }

# re-estimate K by K-M using new data(assume no covariates, otherwise need to use Cox model)
# (theoretically not needed but better coverage in simulation) 
  if(!Sep.K)K=KM(x[,j-1],1-d[,j-1])
  if(Sep.K){
	K[data$group==0]=KM(x[data$group==0,j-1], 1-d[data$group==0,j-1])
	K[data$group==1]=KM(x[data$group==1,j-1], 1-d[data$group==1,j-1])
  }
  if (sum(K[d[,j-1]==1]==0)>0) stop("Estimate of probability of censoring = 0 at some time point. Time limit L may be too large.")
  if (min(K[d[,j-1]==1])<0.1) warning("Estimate of probability of censoring < 10% at some time point. To have more stable results, could choose a smaller time limit L.")

  #weight need to be adjusted for x[l,j-1]<-min(X[l],trunc.time) due to truncated time 
  w0[,j-1]=d[,j-1]/K	
  w0[d[,j-1]==0,j-1]=0
  K.new[,j-1]=K

  #coef
  if(Cost.only)DV[,j-1]=Cost.grp1[,j-1] else DV[,j-1]=lambda1*Eff.grp1[,j-1]-Cost.grp1[,j-1]	#new dependent variable within this interval

  #to prevent singular matrix, drop variables with \sum w0*X^2=0 from covar, and use 0 as their coef est
  index.drop=which(apply(covar^2*w0[,j-1],2,sum)==0)
  if(length(index.drop)>0)
  {
  	covar1=covar[,-index.drop]  
  	coef.split[j-1,-index.drop]=solve(t(covar1)%*%(covar1*w0[,j-1]))%*%(apply(covar1*w0[,j-1]*DV[,j-1],2,sum))
  	coef.split[j-1,index.drop]=0
  } else coef.split[j-1,]=solve(t(covar)%*%(covar*w0[,j-1]))%*%(apply(covar*w0[,j-1]*DV[,j-1],2,sum))

}

#sum up all coef across intervals
est=apply(coef.split,2,sum)

#use formula to obtain robust SE
## loop for each split time interval
for(j in 2:length(Part.times))
{
  Q_mat=tmp1=matrix(0,dim(data)[1],dim(covar)[2])
  N.largerX=numeric(dim(data)[1])
  for(i in 1:dim(data)[1])
  {
	N.largerX[i]=sum(x[,j-1]>=x[i,j-1])
	Q_mat[i,]=apply(covar*w0[,j-1]*(x[,j-1]>x[i,j-1])*c(DV[,j-1]-covar%*%coef.split[j-1,]),2,sum)/N.largerX[i]
  }
  for(i in 1:dim(data)[1])
	tmp1[i,]=apply(Q_mat*(1-d[,j-1])*(x[,j-1]<=x[i,j-1])/N.largerX,2,sum)	
  Ksi[j-1,,]=covar*w0[,j-1]*c(DV[,j-1]-covar%*%coef.split[j-1,])+Q_mat*(1-d[,j-1])-tmp1
}

for(j in 2:length(Part.times))
for(k in 2:length(Part.times))
{
  B.split[j-1,k-1,,]=t(Ksi[j-1,,])%*%Ksi[k-1,,]
}

A_mat=t(covar)%*%covar
B_mat=apply(B.split,c(3,4),sum)
cov_mat=solve(A_mat)%*%B_mat%*%solve(A_mat)

  coef.table=data.frame(Estimate=est,Std.err=sqrt(diag(cov_mat)),
	Wald=(est/sqrt(diag(cov_mat)))^2,p=2-2*stats::pnorm(abs(est/sqrt(diag(cov_mat)))))
  row.names(coef.table)=colnames(covar)

  if(is.null(lambda)) {
	if(!Cost.only) {
		Reg.type="Effect"
	} else {
		Reg.type="Cost"
	}
	lambda=NA
  } else {
	Reg.type="NBR"
  }

  if((is.na(lambda))|length(int.name)>0) CEAC=NA else CEAC=stats::pnorm(est[2]/sqrt(diag(cov_mat))[2])	#if there is interaction, does not provide CEAC here

  n=dim(data)[1]
  if(!returnData) data=coef.split=DV=w0=K.new=x=d=NULL
  return(list(Method='PT',lambda=lambda,est=est,se=sqrt(diag(cov_mat)),covariance=cov_mat,coef.table=coef.table,
	CEAC=CEAC,int.name=int.name,covar1st=covar1st,Reg.type=Reg.type,data=data,form=form,coef.split=coef.split,
	DV=DV,w0=w0,K.new=K.new,x=x,d=d,n0=length(X),n=n))#,A_mat=A_mat,Ksi=Ksi))
}



#' Doubly robust simple weighted method for net-benefit regression
#'
#' This function uses doubly robust simple weighted (unpartitioned version) method for net-benefit regression.
#' @param X a vector containing continuous positive follow-up time.
#' @param delta a vector containing binary indicator of event (such as death), 1 - complete, 0 - censored.
#' @param Cost.total a vector containing observed total costs.
#' @param Eff.total a vector containing observed total effectiveness. If not provided, assume effectiveness is survival. 
#' @param group a vector containing binary treatment indicator, 1 - treatment, 0 - control.
#' @param Z a vector, matrix, or dataframe containing covariates for net-benefit regression. If not provided, will do unadjusted analysis using simple regression. Defaults to NULL.
#' @param PS.Z a vector, matrix, or dataframe containing covariates matrix for propensity score model using logistic regression. If not provided, will fit an unadjusted logistic regression (e.g., for randomized studies). Defaults to NULL.
#' @param interaction a vector containing covariate names to be included in interactions with treatment, must be a subset of variable names in Z, otherwise the variable not in Z will be ignored. Defaults to NULL.
#' @param Sep.K logical, if TRUE, estimate K (survival function of censoring time, used in inverse probability of censoring weighting) using Kaplan-Meier estimator within each group separately. Defaults to TRUE.
#' @param PS.trim a value between (0, 0.5) to trim extreme propensity scores outside the range of (PS.trim, 1-PS.trim). Defaults to 0.1.
#' @param lambda a vector or scalar containing cost-effectiveness threshold values (e.g., willingness-to-pay for 1 unit of additional effectiveness).
#' @param Cost.only logical. If TRUE, fit a regression with dependent variable as cost. Defaults to FALSE.
#' @keywords net-benefit regression, simple weighted, doubly robust

NetBenReg_SW_DR<-function(X,delta,Cost.total,Eff.total,group,Z=NULL,PS.Z=NULL,interaction=NULL,Sep.K=TRUE,PS.trim=0.1,lambda=NULL,Cost.only=FALSE)
{

  if(is.null(PS.Z)) PS.Z=matrix(0,length(delta),0) 
  if(is.null(Z)) Z=matrix(0,length(delta),0) 

  SW_results<-NetBenReg_SW(X=X,delta=delta,Cost.total=Cost.total,Eff.total=Eff.total,group=group,Z=Z,interaction=interaction,Sep.K=Sep.K,lambda=lambda,Cost.only=Cost.only,Aux.X=PS.Z,returnData=TRUE)
  data=SW_results$data
  form=SW_results$form
  est.coef=SW_results$est
  reg.coef.table=SW_results$coef.table

  #create new data used in prediction
  datanew1=datanew0=data
  datanew1$group=1
  datanew0$group=0
  new1=stats::model.matrix(stats::as.formula(form), data = datanew1)
  new0=stats::model.matrix(stats::as.formula(form), data = datanew0) 

  m0=new0%*%est.coef
  m1=new1%*%est.coef

  #estimate PS by logistic reg
  #create formula for PS
  if(dim(PS.Z)[2]>0){
	PS.Z.name=paste(names(data)[(dim(Z)[2]+5):(dim(Z)[2]+dim(PS.Z)[2]+4)],collapse = "+")
  }else PS.Z.name=character(0)
  form=paste("group~",PS.Z.name,sep="")
  if(dim(PS.Z)[2]==0){form="group~1"}
  PS.covar=stats::model.matrix(stats::as.formula(form), data = data)

  # Estimate propensity score model
  PSmodel=stats::glm(stats::as.formula(form), data = data,family="binomial")
  PS=stats::predict(PSmodel, type='response')
  PScoef=summary(PSmodel)$coef

  #trim PS to prevent extreme value
  if(min(PS)<0.1){
	if(PS.trim<min(PS)){ 
		warning(paste("Minimum of estimated propensity score is ",round(min(PS),digits=3),". Recommend to use PS.trim to trim them.",sep="")) }else
		message(paste("Minimum of estimated propensity score is ",round(min(PS),digits=3),". PS are trimmed by PS.trim=",PS.trim,".",sep="")) 
  }
  if(max(PS)>0.9){
	if(1-PS.trim>max(PS)){ 
		warning(paste("Maximum of estimated propensity score is ",round(max(PS),digits=3),". Recommend to use PS.trim to trim them.",sep="")) }else
		message(paste("Maximum of estimated propensity score is ",round(max(PS),digits=3),". PS are trimmed by 1-PS.trim=",1-PS.trim,".",sep="")) 
  }
  PS[PS<PS.trim]=PS.trim
  PS[PS>1-PS.trim]=1-PS.trim

  #DR est
  mu1=sum(data$w0*(data$group*data$DV/PS-(data$group-PS)/PS*m1))/sum(data$w0)  
  mu0=sum(data$w0*((1-data$group)*data$DV/(1-PS)+(data$group-PS)/(1-PS)*m0))/sum(data$w0)
  est=mu1-mu0	

  #obtain SE 
  W=data$group*data$DV/PS-(data$group-PS)/PS*m1-(1-data$group)*data$DV/(1-PS)-(data$group-PS)/(1-PS)*m0-est
  W0=apply(c(data$w0*data$group/PS*(data$DV-m1)*(1-PS))*PS.covar,2,sum)/sum(data$w0*data$group/PS)+
	apply(c(data$w0*(1-data$group)/(1-PS)*(data$DV-m0)*PS)*PS.covar,2,sum)/sum(data$w0*(1-data$group)/(1-PS))
  S=c(data$group-PS)*PS.covar
  ESS=(t(PS.covar)%*%diag(PS*(1-PS))%*%PS.covar)/dim(data)[1]

  G0=G1=G02=G12=N.largerX=numeric(dim(data)[1])
  Y_bar=matrix(NA,dim(data)[1],dim(data)[1])
  for(i in 1:dim(data)[1])
  {
	Y_bar[,i]=(data$X>=data$X[i])
	G1[i]=mean(data$w0*data$group*(data$X>=data$X[i])*(W-sum(data$group*data$w0*(data$X>=data$X[i])*W)/sum(data$group*data$w0*(data$X>=data$X[i]))))
	G0[i]=mean(data$w0*(1-data$group)*(data$X>=data$X[i])*(W-sum((1-data$group)*data$w0*(data$X>=data$X[i])*W)/sum((1-data$group)*data$w0*(data$X>=data$X[i]))))
	G12[i]=mean(data$w0*data$group*(data$X>=data$X[i])*(W-sum(data$group*data$w0*(data$X>=data$X[i])*W)/sum(data$group*data$w0*(data$X>=data$X[i])))^2)
	G02[i]=mean(data$w0*(1-data$group)*(data$X>=data$X[i])*(W-sum((1-data$group)*data$w0*(data$X>=data$X[i])*W)/sum((1-data$group)*data$w0*(data$X>=data$X[i])))^2)
  }
  phi=data$w0*(c(W)-c(W0%*%solve(ESS)%*%t(S)))+data$group*(1-data$delta)*G1/data$K/apply(data$group*Y_bar,2,sum)+
	(1-data$group)*(1-data$delta)*G0/data$K/apply((1-data$group)*Y_bar,2,sum)
  DRvar=sum(phi^2)/dim(data)[1]/dim(data)[1]

  coef.table=data.frame(Estimate=est,Std.err=sqrt(DRvar),Wald=(est/sqrt(DRvar))^2,p=2-2*stats::pnorm(abs(est/sqrt(DRvar))))
  row.names(coef.table)="group"

  if(is.null(lambda)) {
	if(!Cost.only) {
		Reg.type="Effect"
	} else {
		Reg.type="Cost"
	}
	lambda=NA
  } else {
	Reg.type="NBR"
  }

  if(is.na(lambda))CEAC=NA else CEAC=stats::pnorm(est/sqrt(DRvar))

  n=dim(data)[1]
  return(list(Method='DR_SW',lambda=lambda,est=est,se=sqrt(DRvar),coef.table=coef.table,CEAC=CEAC,
	Regmodel=reg.coef.table,PSmodel=PScoef,PS=PS,group=data$group,Reg.type=Reg.type,n0=length(X),n=n))
}



#' Doubly robust partitioned method for net-benefit regression
#'
#' This function uses doubly robust partitioned method for net-benefit regression.
#' @param X a vector containing continuous positive follow-up time.
#' @param delta a vector containing binary indicator of event (such as death), 1 - complete, 0 - censored.
#' @param Cost.grp a matrix or dataframe containing observed grouped costs, Cost.grp[i,j] is observed cost of ith people accumulated in jth interval.
#' @param Eff.grp a matrix or dataframe containing observed grouped effectiveness. Eff.grp[i,j] is observed effectiveness of ith people accumulated in jth interval. If not provided, assume effectiveness is survival. 
#' @param Part.times a vector containing end time points of each time interval, must be monotonically increasing.
#' @param group vector containing binary treatment indicator, 1 - treatment, 0 - control.
#' @param Z a vector, matrix, or dataframe containing covariates for net-benefit regression. If not provided, will do unadjusted analysis using simple regression. Defaults to NULL.
#' @param PS.Z a vector, matrix, or dataframe containing covariates matrix for propensity score model using logistic regression. If not provided, will fit an unadjusted logistic regression (e.g., for randomized studies). Defaults to NULL.
#' @param interaction a vector containing covariate names to be included in interactions with treatment, must be a subset of variable names in Z, otherwise the variable not in Z will be ignored. Defaults to NULL.
#' @param Sep.K logical, if TRUE, estimate K (survival function of censoring time, used in inverse probability of censoring weighting) using Kaplan-Meier estimator within each group separately. Defaults to TRUE.
#' @param PS.trim a value between (0, 0.5) to trim extreme propensity scores outside the range of (PS.trim, 1-PS.trim). Defaults to 0.1.
#' @param lambda vector or scalar containing cost-effectiveness threshold values (e.g., willingness-to-pay for 1 unit of additional effectiveness).
#' @param Cost.only logical. If TRUE, fit a regression with dependent variable as cost. Defaults to FALSE.
#' @keywords net-benefit regression, partitioned, doubly robust

NetBenReg_PT_DR<-function(X,delta,Cost.grp,Eff.grp,Part.times,group,Z=NULL,PS.Z=NULL,interaction=NULL,Sep.K=TRUE,PS.trim=0.1,lambda=NULL,Cost.only=FALSE)
{

  if(is.null(PS.Z)) PS.Z=matrix(0,length(delta),0) 
  if(is.null(Z)) Z=matrix(0,length(delta),0) 

  PT_results<-NetBenReg_PT(X=X,delta=delta,Cost.grp=Cost.grp,Eff.grp=Eff.grp,Part.times=Part.times,group=group,Z=Z,interaction=interaction,Sep.K=Sep.K,lambda=lambda,Cost.only=Cost.only,Aux.X=PS.Z,returnData=TRUE)
  data=PT_results$data
  form=PT_results$form
  est.coef=PT_results$est
  reg.coef.table=PT_results$coef.table
  coef.split=PT_results$coef.split
  DV=PT_results$DV
  w0=PT_results$w0
  K.new=PT_results$K.new
  x=PT_results$x
  d=PT_results$d

  #create new data used in prediction
  datanew1=datanew0=data
  datanew1$group=1
  datanew0$group=0
  new1=stats::model.matrix(stats::as.formula(form), data = datanew1)
  new0=stats::model.matrix(stats::as.formula(form), data = datanew0) 

  #estimate PS by logistic reg
  #create formula for PS
  if(dim(PS.Z)[2]>0){
	PS.Z.name=paste(names(data)[(dim(Z)[2]+dim(Cost.grp)[2]+dim(Eff.grp)[2]+4):(dim(Z)[2]+dim(PS.Z)[2]+dim(Cost.grp)[2]+dim(Eff.grp)[2]+3)],collapse = "+")
  }else PS.Z.name=character(0)
  form=paste("group~",PS.Z.name,sep="")
  if(dim(PS.Z)[2]==0){form="group~1"}
  PS.covar=stats::model.matrix(stats::as.formula(form), data = data)

  # Estimate propensity score model
  PSmodel=stats::glm(stats::as.formula(form), data = data,family="binomial")
  PS=stats::predict(PSmodel, type='response')
  PScoef=summary(PSmodel)$coef

  #trim PS to prevent extreme value
  if(min(PS)<0.1){
	if(PS.trim<min(PS)){ 
		warning(paste("Minimum of estimated propensity score is ",round(min(PS),digits=3),". Recommend to use PS.trim to trim them.",sep="")) }else
		message(paste("Minimum of estimated propensity score is ",round(min(PS),digits=3),". PS are trimmed by PS.trim=",PS.trim,".",sep="")) 
  }
  if(max(PS)>0.9){
	if(1-PS.trim>max(PS)){ 
		warning(paste("Maximum of estimated propensity score is ",round(max(PS),digits=3),". Recommend to use PS.trim to trim them.",sep="")) }else
		message(paste("Maximum of estimated propensity score is ",round(max(PS),digits=3),". PS are trimmed by 1-PS.trim=",1-PS.trim,".",sep="")) 
  }
  PS[PS<PS.trim]=PS.trim
  PS[PS>1-PS.trim]=1-PS.trim

  ## loop for each split time interval 
  m0=m1=matrix(0,dim(data)[1],length(Part.times)-1)
  mu0=mu1=numeric(length(Part.times)-1)
  for(j in 2:length(Part.times))
  {
    m0[,j-1]=new0%*%coef.split[j-1,]
    m1[,j-1]=new1%*%coef.split[j-1,]

    mu1[j-1]=sum(w0[,j-1]*(data$group*DV[,j-1]/PS-(data$group-PS)/PS*m1[,j-1]))/sum(w0[,j-1])  
    mu0[j-1]=sum(w0[,j-1]*((1-data$group)*DV[,j-1]/(1-PS)+(data$group-PS)/(1-PS)*m0[,j-1]))/sum(w0[,j-1])
  }

  mu1.sum=sum(mu1)  
  mu0.sum=sum(mu0)
  est=mu1.sum-mu0.sum	#est by DR

#use formula to obtain SE
## loop for each split time interval
S=c(data$group-PS)*PS.covar
ESS=(t(PS.covar)%*%diag(PS*(1-PS))%*%PS.covar)/dim(data)[1]

G0=G1=W=phi=matrix(0,dim(data)[1],length(Part.times)-1)
W0=matrix(0,dim(PS.covar)[2],length(Part.times)-1)
Y_bar=array(NA,c(dim(data)[1],dim(data)[1],length(Part.times)-1))
for(j in 2:length(Part.times))
{
  W[,j-1]=data$group*DV[,j-1]/PS-(data$group-PS)/PS*m1[,j-1]-(1-data$group)*DV[,j-1]/(1-PS)-(data$group-PS)/(1-PS)*m0[,j-1]-(mu1[j-1]-mu0[j-1])
  W0[,j-1]=apply(c(w0[,j-1]*data$group/PS*(DV[,j-1]-m1[,j-1])*(1-PS))*PS.covar,2,sum)/sum(w0[,j-1]*data$group/PS)+
	apply(c(w0[,j-1]*(1-data$group)/(1-PS)*(DV[,j-1]-m0[,j-1])*PS)*PS.covar,2,sum)/sum(w0[,j-1]*(1-data$group)/(1-PS))
  for(i in 1:dim(data)[1])
  {
	Y_bar[,i,j-1]=(x[,j-1]>=x[i,j-1])
	G1[i,j-1]=mean(w0[,j-1]*data$group*(x[,j-1]>=x[i,j-1])*(W[,j-1]-sum(data$group*w0[,j-1]*(x[,j-1]>=x[i,j-1])*W[,j-1])/sum(data$group*w0[,j-1]*(x[,j-1]>=x[i,j-1]))))
	G0[i,j-1]=mean(w0[,j-1]*(1-data$group)*(x[,j-1]>=x[i,j-1])*(W[,j-1]-sum((1-data$group)*w0[,j-1]*(x[,j-1]>=x[i,j-1])*W[,j-1])/sum((1-data$group)*w0[,j-1]*(x[,j-1]>=x[i,j-1]))))
  }
  phi[,j-1]=w0[,j-1]*(c(W[,j-1])-c(W0[,j-1]%*%solve(ESS)%*%t(S)))+
	data$group*(1-d[,j-1])*G1[,j-1]/K.new[,j-1]/apply(data$group*Y_bar[,,j-1],2,sum)+(1-data$group)*(1-d[,j-1])*G0[,j-1]/K.new[,j-1]/apply((1-data$group)*Y_bar[,,j-1],2,sum)

}

DRvar=sum(t(phi)%*%phi)/dim(data)[1]/dim(data)[1]

  coef.table=data.frame(Estimate=est,Std.err=sqrt(DRvar),Wald=(est/sqrt(DRvar))^2,p=2-2*stats::pnorm(abs(est/sqrt(DRvar))))
  row.names(coef.table)="group"

  if(is.null(lambda)) {
	if(!Cost.only) {
		Reg.type="Effect"
	} else {
		Reg.type="Cost"
	}
	lambda=NA
  } else {
	Reg.type="NBR"
  }

  if(is.na(lambda))CEAC=NA else CEAC=stats::pnorm(est/sqrt(DRvar))

  n=dim(data)[1]
  return(list(Method='DR_PT',lambda=lambda,est=est,se=sqrt(DRvar),coef.table=coef.table,CEAC=CEAC,
	Regmodel=reg.coef.table,PSmodel=PScoef,PS=PS,group=data$group,Reg.type=Reg.type,n0=length(X),n=n))
}




#' Net-benefit regression with possibly censored cost-effectiveness data
#'
#' @description
#' This function fits net-benefit regression with possibly censored patient-level cost-effectiveness data. 
#' * Allows simple weighted (not using cost/effectiveness history) and partitioned (using cost/effectiveness history) methods, and two naive methods (complete case only, all data ignoring censoring status). Doubly robust method can also be adopted by further including propensity score model.
#' * Subjects with missing values will be excluded during model fitting; however, censoring rate within time horizon is calculated using as many as possible patients.
#' @param Followup a vector containing continuous positive follow-up time of length n.
#' @param delta a vector containing binary indicator of event (such as death) of length n, 1 - complete, 0 - censored.
#' @param group a vector containing binary treatment indicator, 1 - considered treatment, 0 - comparison.
#' @param Cost a vector, n x m matrix or dataframe containing observed total or grouped costs, Cost[i,j] is observed cost of the ith people accumulated in the jth interval. Can be a vector or a one-column matrix for total costs only.
#' @param Eff a vector, n x m matrix or dataframe containing observed total or grouped costs, Eff[i,j] is observed effectiveness of the ith people accumulated in the jth interval. Can be a vector or a one-column matrix for total effectiveness only. If not provided, assume effectiveness is survival. 
#' @param Part.times a vector of length m, containing end time point of each time interval. Must be monotonically increasing (eg, 1st interval is time [0, Part.times[1]], 2nd is (Part.times[1], Part.times[2]]). Required if using PT method without Eff provided, or truncating grouped costs and effectiveness if they may be partially outside time limit L. 
#' @param Z a vector, n x p matrix or dataframe containing covariates for net-benefit regression. If not provided, will do unadjusted analysis using simple regression. Defaults to NULL.
#' @param PS.Z a vector, matrix, or dataframe containing covariates matrix for propensity score model using logistic regression. If not provided, will fit an unadjusted logistic regression (e.g., for randomized studies). Defaults to NULL.
#' @param interaction a vector containing covariate names to be included in interactions with treatment, must be a subset of variable names in Z, otherwise the variable not in Z will be ignored. Defaults to NULL.
#' @param Method method for estimation. `"SW"` - simple weighted, `"PT"` - partitioned, `"CC"` - naive complete case only, `"AL"` - naive all data ignoring censoring status, doubly robust method requires either `"SW"` or `"PT"`.
#' @param Sep.K logical, if TRUE, estimate K (survival function of censoring time, used in inverse probability of censoring weighting) using Kaplan-Meier estimator within each group separately. Defaults to TRUE.
#' @param PS.trim a value between (0, 0.5) to trim extreme propensity scores outside the range of (PS.trim, 1-PS.trim). Defaults to 0.1.
#' @param Doubly.Robust logical, if TRUE, perform doubly robust method. Defaults to FALSE.
#' @param Eff.only logical. If TRUE, fit a regression with dependent variable as effect. Defaults to FALSE.
#' @param Cost.only logical. If TRUE, fit a regression with dependent variable as cost. Defaults to FALSE.
#' @param lambda a vector or scalar containing cost-effectiveness threshold values (e.g., willingness-to-pay for 1 unit of additional effectiveness).
#' @param L time limit horizon, used to truncate event time (equivalent to having an event at L if the patient is alive at L), costs and effectiveness if they are outside this time limit, assuming cost and effectiveness are evenly spread within each time interval during truncation.
#'
#' @return 
#'
#' @keywords net-benefit regression, censoring
#' @export
#' @examples
#' data(CEdata)	#load simulated cost-effectiveness data
#' lambda <- seq(0,6,0.5)	# choose cost-effectiveness threshold as a sequence from $0 to $6,000 for 1 additional unit of effectiveness
#'
#' # fit covariate-adjusted net-benefit regression (along with cost-only and effect-only regressions) using SW method, effectiveness is QALY
#' # use cost and effectiveness history to better truncate them by L
#' fit1 <- NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38], 	
#' 	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=lambda, L=10)
#' print(fit1)
#'
#' # fit unadjusted net-benefit regression (eg, for randomized studies), by removing option Z 
#' fit2 <- NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38], 
#'	Part.times=1:15, Method='SW', Eff.only=TRUE,Cost.only=TRUE, L=10, lambda=lambda)
#' print(fit2)
#'
#' # fit covariate-adjusted regression using SW method, effectiveness is life years
#' fit3 <- NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=NULL,
#'	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, lambda=lambda, L=10)
#' print(fit3)
#'
#' # fit covariate-adjusted regression using PT method with QALY as effectiveness
#' fit4<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],
#'	Part.times=1:15,Method='PT',Z=CEdata[,5:7], Eff.only=TRUE,lambda=lambda,L=10)
#' print(fit4)
#'
#' # (dataset with unequal time intervals) assume cost.1 (QALY.1) is cost/QALY in first 2 years, cost.2 (QALY.2) and cost.3 (QALY.3) are cost/QALY in the following 6 months
#' # other time intervals keep the same
#' # set intervals as [0,2],(2,2.5],(2.5,3],(3,4],...,(14,15] using Part.times option
#' fit5<-NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38], 
#'	Part.times=c(2,2.5,3:15), Method='PT', Z=CEdata[,5:7], Eff.only=TRUE, lambda=lambda, L=10)
#' print(fit5)
#'
#' # fit covariate-adjusted regression with interactions between treatment and covariates  #######
#' # include both Treatment by LBBB and Treatment by Female interactions 
#' fit6 <- NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38], 
#'	Part.times=1:15, Method='PT', Z=CEdata[,5:7], interaction=c("LBBB","Female"), Eff.only=TRUE, lambda=lambda, L=10)
#' print(fit6)
#'
#' # Doubly robust PT regression with Treatment by LBBB interaction to estimate causal average incremental net benefit (INB)
#' # all three covariates are used to estimate propensity scores by logistic regression
#' fit7 <- NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38],
#'	Part.times=1:15, Method='PT', Z=CEdata[,5:7], interaction=c("LBBB"), PS.Z=CEdata[,5:7],	Doubly.Robust=TRUE,
#'	Eff.only=TRUE, lambda=lambda, L=10)
#' print(fit7)

NetBenReg<-function(Followup,delta,group,Cost=NULL,Eff=NULL,Part.times=NULL,Z=NULL,PS.Z=NULL,interaction=NULL,
	Method=c('SW','PT','CC','AL'),Sep.K=TRUE,PS.trim=0.1,Doubly.Robust=FALSE,Eff.only=FALSE,Cost.only=FALSE,lambda=NULL,L)
{
  ##### check data for possible errors in input data

  if(!(Method %in% c('SW','PT','CC','AL')))stop("Method must be one of 'SW', 'PT', 'CC', 'AL'.\n")
  if(!(Sep.K %in% c(TRUE,FALSE)))stop("Sep.K must be either TRUE or FALSE.\n")
  if(!(Doubly.Robust %in% c(TRUE,FALSE)))stop("Doubly.Robust must be either TRUE or FALSE.\n")
  if(!(Eff.only %in% c(TRUE,FALSE)))stop("Eff.only must be either TRUE or FALSE.\n")
  if(!(Cost.only %in% c(TRUE,FALSE)))stop("Cost.only must be either TRUE or FALSE.\n")

  if(!is.numeric(L)) stop("L must be numeric.\n")
  if(!(is.atomic(L) && length(L) == 1)) stop("Time limit L is not a scalar.\n") 
  if(L<=0) stop("Time limit L must be positive.\n") 

  if((!Eff.only)&(!Cost.only)){
	if(is.null(lambda)) stop("lambda is required if not fitting effectiveness-only or cost-only regression.\n") 
  }

  #check whether is vector
  if(class(delta)[1]=="factor")delta=as.numeric(as.character(delta))
  if(class(group)[1]=="factor")group=as.numeric(as.character(group))
  if(!(is.vector(Followup) && is.atomic(Followup))) stop("Followup must be a vector.\n")
  if(!(is.vector(delta) && is.atomic(delta))) stop("delta must be a vector.\n")
  if(!(is.vector(group) && is.atomic(group))) stop("group must be a vector.\n")
  if(!is.numeric(Followup)) stop("Followup must be numeric (>0).\n")
  if(!is.numeric(delta)) stop("delta must be numeric (0 or 1).\n")
  if(!is.numeric(group)) stop("group must be numeric (0 or 1).\n")

  if(!is.null(lambda)){
	if(class(lambda)[1]=="factor")lambda=as.numeric(as.character(lambda))
	if(!(is.vector(lambda) && is.atomic(lambda))) stop("lambda must be a vector.\n")
	if(!is.numeric(lambda)) stop("lambda must be numeric.\n")
	if(any(is.na(lambda))) message("NA value in lambda is removed.\n")
	lambda=sort(lambda)
  	if(min(lambda)<0) stop("Values in lambda cannot be negative.\n")
  }

  n=length(Followup)
  if(length(delta)!=n) stop("Length of delta is different to length of Followup.\n")
  if(length(group)!=n) stop("Length of group is different to length of Followup.\n") 
  if(!is.numeric(PS.trim)) stop("PS.trim must be numeric.\n")
  if(!(is.atomic(PS.trim) && length(PS.trim) == 1)) stop("PS.trim is not a scalar.\n") 
  if(PS.trim<0) {warning("PS.trim is negative. Propensity scores are not trimmed.\n");PS.trim=0}
  if(PS.trim>=0.5) stop("PS.trim is too large. Must be between 0 and 0.5. \n")

  if((!identical(levels(as.factor(group)),c("0","1")))||(length(unique(group)) == 1)) stop("Values in group must be 0 or 1 and cannot be in one group only.\n")

  if(length(unique(delta)) == 1){
		if(!(levels(as.factor(delta))%in%c("0","1")))stop("Values in delta must be 0 or 1.\n")
  }else if(!identical(levels(as.factor(delta)),c("0","1"))) stop("Values in delta must be 0 or 1.\n")
  
  if(min(Followup,na.rm=TRUE)<=0) stop("Values in Followup cannot be zero or negative.\n")
  if(L>max(Followup,na.rm=TRUE)) stop("Time limit L is greater than max follow-up times. Choose a smaller L.\n") 

  if(((Cost.only)|(!is.null(lambda)))&(is.null(Cost))) stop("Cost is required if fitting cost-only or net-benefit regression.\n") 

  if(!is.null(Part.times)){
	if(!(is.vector(Part.times) && is.atomic(Part.times))) stop("Part.times must be a vector.\n")
	if(any(is.na(Part.times))) stop("NA value in Part.times is not allowed.\n")
	if(!is.numeric(Part.times)) stop("Part.times must be numeric.\n")
	if(min(Part.times)<0) stop("Values in Part.times cannot be negative.\n")
	if (!all(Part.times == cummax(Part.times))) stop("Part.times are not monontonically increasing.\n")
	if(length(Part.times)>1)if (Part.times[length(Part.times)]<L) stop("Time limit L is greater than last value of Part.times. Choose a smaller L.\n")
	if(Part.times[length(Part.times)]<max(Followup,na.rm=TRUE)) warning("Last value of Part.times is smaller than max Followup.\n")

	m=length(Part.times)
	Part.times=c(0,Part.times)	#add time 0
  }else m=1

  if(!is.null(Cost)){
  	if(is.vector(Cost) && is.atomic(Cost))Cost=matrix(Cost,ncol=1)
	else if(is.data.frame(Cost)|is.matrix(Cost))	Cost=as.matrix(Cost)
	else stop("Cost must be a vector/matrix/data.frame.\n")

	if(dim(Cost)[1]!=n) stop("Number of rows of Cost is different to length of Followup.\n")
	if(dim(Cost)[2]==0)stop("Number of columns in Cost is 0.\n")
  	if(!all(sapply(Cost, is.numeric))) stop("Cost must be numeric.\n")
  	if(min(Cost,na.rm=TRUE)<0) warning("There is negative value in Cost.\n")

  	if(dim(Cost)[2]==1){
		if(Method=="PT"){
			Method="SW"
			message("Only total Cost provided. SW method is used.\n")
		}
  	}else if(dim(Cost)[2]!=m)stop("Length of Part.times is different to number of columns of Cost.\n") 
  }


  if(is.null(Eff)) {  		# if Eff not provided, assume effectiveness is survival 
    if((!is.null(lambda))|Eff.only)message('Eff is not provided. Assume effectiveness is survival time.\n')
    if(!is.null(Part.times)){
	Eff=matrix(rep(Part.times[-1]-Part.times[-(m+1)],each=n),n,m)
	for(i in 1:n)
	{
		if(is.na(Followup[i]))Eff[i,]=0 else
  		for(j in 1:m)
		{
  			# truncate Effect by follow-up time to obtain observed grouped life time
			if(Followup[i]<Part.times[j+1])
			{
				if (Followup[i]<Part.times[j]) Eff[i,j]=0 
				else Eff[i,j]=Followup[i]-Part.times[j]
			}
		}  
  	}
    }else Eff=matrix(Followup,n,1)
  } else	#Eff provided
  {
  	if(is.vector(Eff) && is.atomic(Eff))Eff=matrix(Eff,ncol=1)
	else if(is.data.frame(Eff)|is.matrix(Eff))	Eff=as.matrix(Eff)
	else stop("Eff must be a vector/matrix/data.frame.\n")

  	if(dim(Eff)[2]==0)stop("Number of columns in Eff is 0.\n")
  	if(!all(sapply(Eff, is.numeric))) stop("Eff must be numeric.\n")
	if(dim(Eff)[1]!=n) stop("Number of rows of Eff is different to length of Followup.\n")

  	if(dim(Eff)[2]==1){
		if(Method=="PT"){
			Method="SW"
			message("Only total Eff provided. SW method is used.\n")
		}
 	}else if(dim(Eff)[2]!=m)stop("Length of Part.times is different to number of columns of Eff.\n") 

  }

  if(m>1) {
	if (!is.null(Cost)){
		if((dim(Cost)[2]==1)&&(dim(Eff)[2]==1)) {Part.times=NULL;m=1;message("Part.times is not used since only total Cost and Eff are provided.\n")}
	}
  }

  if(Method=='PT')
  {
	if(m==1) {
		message('Only 1 time interval is provided for PT method. SW method is used.\n')
		Method='SW'
	} #else if((is.null(Eff))&(is.null(Part.times)))stop("Either Eff or Part.times is needed for PT method.\n")
  }

  if((is.null(Part.times))&(m>1)) stop("When cost history is available, Part.times is required to truncate them by time limit L.\n")

#  if((is.null(Part.times))&(!is.null(Eff))) message("Part.times is not provided. Assume all costs and effectiveness are evenly spreaded until follow-up time when truncated by time limit L.\n")
#  if((is.null(Part.times))&(is.null(Eff))) message("Part.times is not provided. Assume all costs are evenly spreaded until follow-up time when truncated by time limit L.\n")

  if(!is.null(Z)){
	if(class(Z)[1]=="factor")Z=as.character(Z)
	if(is.vector(Z)&& is.atomic(Z))Z=data.frame(Z)
	else if(is.data.frame(Z)|is.matrix(Z))	Z=as.data.frame(Z)
	else stop("Z must be a vector/matrix/data.frame.\n")

	if(dim(Z)[1]!=n) stop("Number of rows of covariates Z is different to length of Followup.\n")
	Z=dplyr::mutate(Z, dplyr::across(tidyselect::where(is.character), as.factor))	#change those character as factor
  }

# check interaction terms 
  if(!is.null(interaction)){
	if(!(is.vector(interaction) && is.atomic(interaction))) stop("interaction must be a vector.\n")
  	if(length(setdiff(interaction,names(Z)))>0) message("One or more variables in interaction are not in covariates Z, which are removed from interaction.\n")
  }

  ###### truncate data by L ####

  # truncate follow-up time & death indicator
  deltaL=delta
  FollowupL=Followup
  deltaL[Followup>=L]=1
  FollowupL[Followup>=L]=L

  # truncate Cost and Eff. Assume Cost and Eff evenly spreaded in each time interval
  if(m==1){		#only total cost/eff are provided, assume they spreaded evenly in follow-up time
	if(!is.null(Cost))Cost[,1]=Cost[,1]*FollowupL/Followup
	Eff[,1]=Eff[,1]*FollowupL/Followup
  }else
  {
	if(!is.null(Cost))if(dim(Cost)[2]==1)Cost[,1]=Cost[,1]*FollowupL/Followup
	if(dim(Eff)[2]==1)Eff[,1]=Eff[,1]*FollowupL/Followup

  	for(i in 1:n)
	{
  		for(j in 1:m)
		{
  			# truncate Cost and Effect by follow-up time
			if(!is.na(FollowupL[i]))
			if(FollowupL[i]<Part.times[j+1])
			{
				if (FollowupL[i]<=Part.times[j]) {if(dim(Eff)[2]>1)Eff[i,j]=0;if(!is.null(Cost))if(dim(Cost)[2]>1)Cost[i,j]=0}
				else {	#prorate
				  if(dim(Eff)[2]>1)Eff[i,j]=Eff[i,j]*(FollowupL[i]-Part.times[j])/(min(Followup[i],Part.times[j+1])-Part.times[j])	
				  if(!is.null(Cost))if(dim(Cost)[2]>1)Cost[i,j]=Cost[i,j]*(FollowupL[i]-Part.times[j])/(min(Followup[i],Part.times[j+1])-Part.times[j])	
				}
			}
		}  
  	}
	
	# also revise intervals if needed
	if(Part.times[m+1]>L)
	{
		mL=min(which(Part.times>=L))-1
		Part.times=c(Part.times[1:mL],L)
		if(!is.null(Cost))if(dim(Cost)[2]>1)Cost=Cost[,1:mL]
		if(dim(Eff)[2]>1)Eff=Eff[,1:mL]
		if(mL==1){
			if(!is.null(Cost))Cost=matrix(Cost,ncol=1)
			Eff=matrix(Eff,ncol=1)
  			if(Method=='PT' & m>1){
				message('Only 1 time interval within time limit L is provided. SW method is used.\n')
				Method='SW'
			}
		}
		m=mL
	}
  }

  censorrate=1-mean(deltaL,na.rm =TRUE)

  if(sum(1-deltaL,na.rm =TRUE)==0) {
	message('Data is not censored within time limit L. All methods are equivalent to OLS.\n')
	if(!Doubly.Robust) Method='CC' else Method='SW' 
	OLS=1
  } else OLS=0

  if(Doubly.Robust){
	if(!is.null(PS.Z)){
		if(class(PS.Z)[1]=="factor")PS.Z=as.character(PS.Z)
		if(is.vector(PS.Z)&& is.atomic(PS.Z))PS.Z=data.frame(PS.Z)
		else if(is.data.frame(PS.Z)|is.matrix(PS.Z))	PS.Z=as.data.frame(PS.Z)
		else stop("PS.Z must be a vector/matrix/data.frame.\n")

		if(dim(PS.Z)[1]!=n) stop("Number of rows of covariates PS.Z is different to length of Followup.\n")
		PS.Z=dplyr::mutate(PS.Z, dplyr::across(tidyselect::where(is.character), as.factor))	#change those character as factor
	}
	if((Method!='SW')&(Method!='PT')) stop("SW or PT method is required for doubly robust method.\n")
  }
  if(!Doubly.Robust){
	if(!is.null(PS.Z)) message("Non-doubly robust method is used, and hence PS.Z is not used.\n")
  }

  ###### estimate

  if (is.null(lambda))n.lam=0 else n.lam=length(lambda)
  if(Eff.only){n.lam=n.lam+1;n.lam.E=n.lam}
  if(Cost.only){n.lam=n.lam+1;n.lam.C=n.lam}
  results <- as.list(1:n.lam)

  if(Method=="CC"){
    if(sum(1-deltaL,na.rm =TRUE)>0)
	message('Complete case only method is not recommended for censored data due to bias and efficiency loss.\n')
   
    Eff.total=apply(Eff,1,sum)
    if((!is.null(lambda))|Cost.only)Cost.total=apply(Cost,1,sum)
    if(!is.null(lambda)){
	for(j in 1:length(lambda))
	{
		fit=NetBenReg_CC(X=FollowupL,delta=deltaL,Cost.total=Cost.total,Eff.total=Eff.total,group=group,Z=Z,
			interaction=interaction,lambda=lambda[j])
		fit$L=L;fit$censorrate=censorrate
		if(OLS) fit$Method="OLS"
		results[[j]]<-fit
  	}
    }
    if(Eff.only){
	fit=NetBenReg_CC(X=FollowupL,delta=deltaL,Cost.total=rep(0,n),Eff.total=Eff.total,group=group,Z=Z,interaction=interaction)
	fit$L=L;fit$censorrate=censorrate
	if(OLS) fit$Method="OLS"
	results[[n.lam.E]]<-fit
    }
    if(Cost.only){
	fit=NetBenReg_CC(X=FollowupL,delta=deltaL,Cost.total=Cost.total,Eff.total=Eff.total,group=group,Z=Z,interaction=interaction,Cost.only=TRUE)
	fit$L=L;fit$censorrate=censorrate
	if(OLS) fit$Method="OLS"
	results[[n.lam.C]]<-fit
    }
  }else
  if(Method=="AL"){

    if(sum(1-deltaL,na.rm =TRUE)>0){
    	message('All data method is not recommended for censored data due to bias.\n')
    }

    Eff.total=apply(Eff,1,sum)
    if((!is.null(lambda))|Cost.only)Cost.total=apply(Cost,1,sum)
    if(!is.null(lambda)){
	for(j in 1:length(lambda))
	{
		fit=NetBenReg_AL(X=FollowupL,delta=deltaL,Cost.total=Cost.total,Eff.total=Eff.total,group=group,Z=Z,
			interaction=interaction,lambda=lambda[j])
		fit$L=L;fit$censorrate=censorrate
		if(OLS) fit$Method="OLS"
		results[[j]]<-fit
  	}
    }
    if(Eff.only){
	fit=NetBenReg_AL(X=FollowupL,delta=deltaL,Cost.total=rep(0,n),Eff.total=Eff.total,group=group,Z=Z,interaction=interaction)
	fit$L=L;fit$censorrate=censorrate
	if(OLS) fit$Method="OLS"
	results[[n.lam.E]]<-fit
    }
    if(Cost.only){
	fit=NetBenReg_AL(X=FollowupL,delta=deltaL,Cost.total=Cost.total,Eff.total=Eff.total,group=group,Z=Z,interaction=interaction,Cost.only=TRUE)
	fit$L=L;fit$censorrate=censorrate
	if(OLS) fit$Method="OLS"
	results[[n.lam.C]]<-fit
    }
  }else
  if(Method=="SW"){
    if(!Doubly.Robust){

      Eff.total=apply(Eff,1,sum)
      if((!is.null(lambda))|Cost.only)Cost.total=apply(Cost,1,sum)
	if(!is.null(lambda)){
		for(j in 1:length(lambda))
		{
			fit=NetBenReg_SW(X=FollowupL,delta=deltaL,Cost.total=Cost.total,Eff.total=Eff.total,group=group,Z=Z,
				interaction=interaction,lambda=lambda[j],Sep.K=Sep.K)
			fit$L=L;fit$censorrate=censorrate
			if(OLS) fit$Method="OLS"
			results[[j]]<-fit
  		}
    	}
    	if(Eff.only){
		fit=NetBenReg_SW(X=FollowupL,delta=deltaL,Cost.total=rep(0,n),Eff.total=Eff.total,group=group,Z=Z,interaction=interaction,Sep.K=Sep.K)
		fit$L=L;fit$censorrate=censorrate
		if(OLS) fit$Method="OLS"
		results[[n.lam.E]]<-fit
    	}
      if(Cost.only){
		fit=NetBenReg_SW(X=FollowupL,delta=deltaL,Cost.total=Cost.total,Eff.total=Eff.total,group=group,Z=Z,interaction=interaction,Sep.K=Sep.K,Cost.only=TRUE)
		fit$L=L;fit$censorrate=censorrate
		if(OLS) fit$Method="OLS"
		results[[n.lam.C]]<-fit
      }
    }else{

	if(is.null(PS.Z))message("PS.Z is not provided, and propensity scores are estimated by unadjusted logistic regression.\n")
	if(is.null(Z))message("Z is not provided, and simple net-benefit regressions are fitted.\n")

	Eff.total=apply(Eff,1,sum)
      if((!is.null(lambda))|Cost.only)Cost.total=apply(Cost,1,sum)
	if(!is.null(lambda)){
		for(j in 1:length(lambda))
		{
			fit=NetBenReg_SW_DR(X=FollowupL,delta=deltaL,Cost.total=Cost.total,Eff.total=Eff.total,group=group,Z=Z,
				PS.Z=PS.Z,interaction=interaction,lambda=lambda[j],Sep.K=Sep.K,PS.trim=PS.trim)
			fit$L=L;fit$censorrate=censorrate
			if(OLS) fit$Method="DR_OLS"
			results[[j]]<-fit
  		}
    	}
    	if(Eff.only){
		fit=NetBenReg_SW_DR(X=FollowupL,delta=deltaL,Cost.total=rep(0,n),Eff.total=Eff.total,group=group,Z=Z,
			PS.Z=PS.Z,interaction=interaction,Sep.K=Sep.K,PS.trim=PS.trim)
			fit$L=L;fit$censorrate=censorrate
			if(OLS) fit$Method="DR_OLS"
		results[[n.lam.E]]<-fit
    	}
      if(Cost.only){
		fit=NetBenReg_SW_DR(X=FollowupL,delta=deltaL,Cost.total=Cost.total,Eff.total=Eff.total,group=group,Z=Z,
			PS.Z=PS.Z,interaction=interaction,Sep.K=Sep.K,PS.trim=PS.trim,Cost.only=TRUE)
			fit$L=L;fit$censorrate=censorrate
			if(OLS) fit$Method="DR_OLS"
		results[[n.lam.C]]<-fit
      }
    }
  }else
  if(Method=="PT"){
    if(!Doubly.Robust){

	if(!is.null(lambda)){
		for(j in 1:length(lambda))
		{
			fit=NetBenReg_PT(X=FollowupL,delta=deltaL,Cost.grp=Cost,Eff.grp=Eff,Part.times=Part.times,group=group,Z=Z,
				interaction=interaction,lambda=lambda[j],Sep.K=Sep.K)
			fit$L=L;fit$censorrate=censorrate
			if(OLS) fit$Method="OLS"
			results[[j]]<-fit
  		}
    	}
    	if(Eff.only){
		fit=NetBenReg_PT(X=FollowupL,delta=deltaL,Cost.grp=matrix(0,n,m),Eff.grp=Eff,Part.times=Part.times,group=group,Z=Z,
			interaction=interaction,Sep.K=Sep.K)
		fit$L=L;fit$censorrate=censorrate
		if(OLS) fit$Method="OLS"
		results[[n.lam.E]]<-fit
    	}
      if(Cost.only){
		fit=NetBenReg_PT(X=FollowupL,delta=deltaL,Cost.grp=Cost,Eff.grp=Eff,Part.times=Part.times,group=group,Z=Z,
			interaction=interaction,Sep.K=Sep.K,Cost.only=TRUE)
		fit$L=L;fit$censorrate=censorrate
		if(OLS) fit$Method="OLS"
		results[[n.lam.C]]<-fit
      }
    }else{

	if(is.null(PS.Z))message("PS.Z is not provided and propensity scores are estimated by unadjusted logistic regression.\n")
	if(is.null(Z))message("Z is not provided and simple net-benefit regressions are fitted.\n")

	if(!is.null(lambda)){
		for(j in 1:length(lambda))
		{
			fit=NetBenReg_PT_DR(X=FollowupL,delta=deltaL,Cost.grp=Cost,Eff.grp=Eff,Part.times=Part.times,group=group,Z=Z,
				PS.Z=PS.Z,interaction=interaction,lambda=lambda[j],Sep.K=Sep.K,PS.trim=PS.trim)
			fit$L=L;fit$censorrate=censorrate
			if(OLS) fit$Method="DR_OLS"
			results[[j]]<-fit
  		}
    	}
    	if(Eff.only){
		fit=NetBenReg_PT_DR(X=FollowupL,delta=deltaL,Cost.grp=matrix(0,n,m),Eff.grp=Eff,Part.times=Part.times,group=group,Z=Z,
			PS.Z=PS.Z,interaction=interaction,Sep.K=Sep.K,PS.trim=PS.trim)
		fit$L=L;fit$censorrate=censorrate
		if(OLS) fit$Method="DR_OLS"
		results[[n.lam.E]]<-fit
    	}
      if(Cost.only){
		fit=NetBenReg_PT_DR(X=FollowupL,delta=deltaL,Cost.grp=Cost,Eff.grp=Eff,Part.times=Part.times,group=group,Z=Z,
			PS.Z=PS.Z,interaction=interaction,Sep.K=Sep.K,PS.trim=PS.trim,Cost.only=TRUE)
		fit$L=L;fit$censorrate=censorrate
		if(OLS) fit$Method="DR_OLS"
		results[[n.lam.C]]<-fit
      }
    }
  }

  class(results) <- "NetBenReg"
  return(results)
}


#' Create CEAC plot based on fitted net-benefit regression with doubly robust method
#'
#' This function creates cost-effectiveness acceptability curve (CEAC) plot based on fitted net-benefit regression with doubly robust method.
#' @param object a fitted "NetBenRegDR" model object.
#' @param add logicial. If TRUE, will add the curve to the existing plot, instead of creating a new plot. Defaults to FALSE.
#' @param xlab label given to the x-axis. Defaults to "Cost-effectiveness threshold".
#' @param ylab label given to the y-axis. Defaults to "Probability Treatment 1 is cost-effective".
#' @param pch a numeric value specifying the point to label the curve. The 'points' help file contains examples of the possible marks. Defaults to 20.
#' @param cex a numeric value specifying the size of the marks. Defaults to 1.
#' @param lty a vector of integers specifying line types for each curve. Defaults to 1.
#' @param lwd a vector of numeric values for line widths. Defaults to 1.
#' @param type character indicating the type of plotting. The 'plot.default' help file contains more explanatuib. Defaults to 'o'.
#' @param ... other arguments that will be passed forward to the underlying plot.default method. 
#' @keywords cost-effectiveness acceptability curve, net-benefit regression, doubly robust

plotNetBenRegDR<-function(object, add=FALSE,xlab = "Cost-effectiveness threshold",ylab="Probability Treatment 1 is cost-effective",pch=20,cex=1,lty=1,lwd=1,type='o', ylim=NULL,...)
{

  n.lam=length(object)
  if(is.na(object[[n.lam]]$lambda)) n.lam=n.lam-1	
  if(is.na(object[[n.lam]]$lambda)) n.lam=n.lam-1	#one for Cost.only and one for Eff.only
  if(n.lam<1)stop("No lambda provided.\n")

  lambda=CEAC=rep(NA,n.lam)
  for(i in 1:n.lam)
  {
	res=object[[i]]
	lambda[i]=res$lambda
	CEAC[i]=res$CEAC
  }

  if(n.lam==1) warning("One 1 value in lambda and CEAC is a point only.\n")

  if(!add){
	if (is.null(ylim)) ylim=c(0,1)
	graphics::plot(lambda,CEAC,ylim = ylim, pch=pch,cex = cex,
             xlab = xlab, ylab=ylab,type=type,lty=lty,lwd=lwd,...)
  }else {
	graphics::lines(lambda,CEAC,lty=lty,lwd=lwd,type=type,pch=pch,cex = cex,...)
  }

#  return(data.frame(lambda,CEAC))	#also return CEAC values
}


#' Create CEAC plot based on fitted net-benefit regression
#'
#' This function creates cost-effectiveness acceptability curve (CEAC) plot based on fitted net-benefit regression (without doubly robust method).
#' @param object a fitted "NetBenReg" model object.
#' @param subgroup a list of covariates to define subgroup. If there is interaction in net-benefit regression model, values for those covariates in the interaction are required. 
#' @param add logicial. If TRUE, will add the curve to the existing plot, instead of creating a new plot. Defaults to FALSE.
#' @param xlab label given to the x-axis. Defaults to "Cost-effectiveness threshold".
#' @param ylab label given to the y-axis. Defaults to "Probability Treatment 1 is cost-effective".
#' @param pch a numeric value specifying the point to label the curve. The 'points' help file contains examples of the possible marks. Defaults to 20.
#' @param cex a numeric value specifying the size of the marks. Defaults to 1.
#' @param lty a vector of integers specifying line types for each curve. Defaults to 1.
#' @param lwd a vector of numeric values for line widths. Defaults to 1.
#' @param type character indicating the type of plotting. The 'plot.default' help file contains more explanatuib. Defaults to 'o'.
#' @param ... other arguments that will be passed forward to the underlying plot.default method. 
#' @keywords cost-effectiveness acceptability curve, net-benefit regression

plotNetBenReg<-function(object, subgroup=NULL, add=FALSE,xlab = "Cost-effectiveness threshold",ylab="Probability Treatment 1 is cost-effective",pch=20,cex=1,lty=1,lwd=1,type='o', ylim=NULL,...)
{

  n.lam=length(object)
  if(is.na(object[[n.lam]]$lambda)) n.lam=n.lam-1
  if(is.na(object[[n.lam]]$lambda)) n.lam=n.lam-1	#one for Cost.only and one for Eff.only
  if(n.lam<1)stop("No lambda provided.\n")
  if(n.lam<2) warning('One 1 value in lambda and CEAC is a point only.\n')

  lambda=CEAC=rep(NA,n.lam)

  if(length(object[[1]]$int.name)==0){  #no interaction
	for(i in 1:n.lam)
	{
		res=object[[i]]
		lambda[i]=res$lambda
		CEAC[i]=res$CEAC
	}
  }else{	#there is interaction
	if(is.null(subgroup)) stop("subgroup is required since there is interaction between treatment and covariates.\n")
	if(!is.list(subgroup)) stop("subgroup must be a list for covariate values.\n")

	# check whether subgroup Covariate is correct
	n.sub=length(subgroup)
	sub1=data.frame(subgroup)
	if(dim(sub1)[1]!=1) stop("Only one value should be provided for each covariate in subgroup.")

	sub0=object[[1]]$covar1st
	int1=object[[1]]$int.name
	int2=intersect(names(sub1),int1) 
	if(!identical(sort(int1),sort(int2))) stop("Values for all covariates in interactions need to be provided.\n")
	for(j in 1:length(int2)){
		if(is.factor(sub0[[int1[j]]]))if(!(sub1[[int1[j]]][1]%in%levels(sub0[[int1[j]]]))) 	stop("Invalid factor level in subgroup\n.") 
		sub0[[int1[j]]][1]=sub1[[int1[j]]][1]
	}
	sub3<-c(stats::model.matrix(stats::as.formula(paste("~",paste(int1,collapse = "+"),sep="")),data=sub0))		#get design matrix for interaction
	p=length(object[[1]]$est)	#2nd is for group, last few ones are for interactions

	for(i in 1:n.lam)
	{
		res=object[[i]]
		lambda[i]=res$lambda
		est=c(res$est)
		cov_mat=res$covariance
		est.subgroup=est[c(2,(p-length(sub3)+2):p)]%*%sub3
		se.subgroup=sqrt(sub3%*%cov_mat[c(2,(p-length(sub3)+2):p),c(2,(p-length(sub3)+2):p)]%*%sub3)		
		CEAC[i]=stats::pnorm(est.subgroup/se.subgroup)
	}
  }

  if(!add){
	if (is.null(ylim)) ylim=c(0,1)
	graphics::plot(lambda,CEAC,ylim = ylim, pch=pch,cex = cex,
             xlab = xlab, ylab=ylab,type=type,lty=lty,lwd=lwd,...)
  }else {
	graphics::lines(lambda,CEAC,lty=lty,lwd=lwd,type=type,pch=pch,cex = cex,...)
  }

#  return(data.frame(lambda,CEAC))	#also return CEAC values
}




#' Create CEAC plot based on fitted net-benefit regression
#'
#' This function creates cost-effectiveness acceptability curve (CEAC) plot based on the fitted net-benefit regression.
#' @param object a fitted "NetBenReg" model object.
#' @param subgroup a list of covariates to define subgroup. If there is interaction in net-benefit regression model, values for those covariates in the interaction are required. 
#' @param add logicial. If TRUE, will add the curve to the existing plot, instead of creating a new plot. Defaults to FALSE.
#' @param xlab label given to the x-axis. Defaults to "Cost-effectiveness threshold".
#' @param ylab label given to the y-axis. Defaults to "Probability Treatment 1 is cost-effective".
#' @param pch a numeric value specifying the point to label the curve. The 'points' help file contains examples of the possible marks. Defaults to 20.
#' @param cex a numeric value specifying the size of the marks. Defaults to 1.
#' @param lty a vector of integers specifying line types for each curve. Defaults to 1.
#' @param lwd a vector of numeric values for line widths. Defaults to 1.
#' @param type character indicating the type of plotting. The 'plot.default' help file contains more explanatuib. Defaults to 'o'.
#' @param ... other arguments that will be passed forward to the underlying plot.default method. 
#' @keywords cost-effectiveness acceptability curve, net-benefit regression
#' @export
#' @examples
#' data(CEdata)
#'
#' # for non-DR method without interaction or DR method, one curve for overall
#' fit1 <- NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38],
#'	Part.times=1:15, Method='PT', Z=CEdata[,5:7], interaction=c("LBBB"), PS.Z=CEdata[,5:7],	Doubly.Robust=TRUE,
#'	Eff.only=TRUE, lambda=lambda, L=10)
#' plot(fit1,ylab="Probability new treatment is cost-effective",xlab="Cost-effectiveness threshold (in $1000) for one additional QALY",lwd=2, pch=19,cex=1.2)
#'
#' # for non-DR method with interaction, one curve for each subgroup defined by interactions
#' fit2 <- NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38], 
#'	Part.times=1:15, Method='PT', Z=CEdata[,5:7], interaction=c("LBBB","female"), Eff.only=TRUE, lambda=lambda, L=10)
#' plot(fit2,subgroup=list(LBBB=0,female=0),add=TRUE,col="gray50",lwd=2,lty=2,pch=15,cex=1.2)		#subgroup of non-LBBB male, adjusted for age
#' plot(fit2,subgroup=list(LBBB=1,female=1),add=TRUE,col="gray50",lwd=2,lty=3,pch=17,cex=1.2)		#subgroup of LBBB female, adjusted for age 

plot.NetBenReg<-function(object, subgroup=NULL, add=FALSE,xlab = "Cost-effectiveness threshold",ylab="Probability Treatment 1 is cost-effective",
	pch=20,cex=1,lty=1,lwd=1,type='o',ylim=NULL, ...)
{
	if ((object[[1]]$Method=='DR_SW')|(object[[1]]$Method=='DR_PT')|(object[[1]]$Method=='DR_OLS'))
		plotNetBenRegDR(object=object, add=add,xlab = xlab,ylab=ylab,pch=pch,cex=cex,lty=lty,lwd=lwd,type=type, ylim=ylim, ...)	
	else plotNetBenReg(object, subgroup=subgroup, add=add,xlab = xlab,ylab=ylab,pch=pch,cex=cex,lty=lty,lwd=lwd,type=type,ylim=ylim, ...)		
}


#' Print the summary results of the fitted net-benefit regression
#'
#' This function prints the summary results of fitted net-benefit regression (non-doubly robust method).
#' @param object a fitted "NetBenReg" model object.
#' @keywords summary, net-benefit regression
#' @export
#' @examples
#' data(CEdata)	
#' fit1 <- NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38], 	
#' 	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=seq(0,6,0.5), L=10)
#' print(fit1)

print.NetBenReg<-function(object)
{
  cat("All n =",object[[1]]$n0,", Used n =",object[[1]]$n,"\n")
  cat("Time limit horizon L =",object[[1]]$L,"\n")
  cat("Censoring rate within L =",round(object[[1]]$censorrate,3)*100,"%\n")

  if(object[[1]]$Method=="OLS") cat("Method: Ordinary Least Squares\n")
  else if(object[[1]]$Method=="CC") cat("Method: Complete Case Only\n")
  else if(object[[1]]$Method=="AL") cat("Method: All Data Ignoring Censoring\n")
  else if(object[[1]]$Method=="SW") cat("Method: Simple Weighted\n")
  else if(object[[1]]$Method=="DR_OLS") cat("Method: Doubly Robust Ordinary Least Squares (estimate is causal average INB with given lambda)\n")
  else if(object[[1]]$Method=="DR_SW") cat("Method: Doubly Robust Simple Weighted (estimate is causal average INB with given lambda)\n")
  else if(object[[1]]$Method=="PT") cat("Method: Partitioned\n")
  else if(object[[1]]$Method=="DR_PT") cat("Method: Doubly Robust Partitioned (estimate is causal average INB with given lambda)\n")
  cat("\n")

  n.lam=length(object)
  for(i in 1:n.lam)
  {
	res=object[[i]]

  	if(is.na(res$lambda)) {
		if(res$Reg.type=="Effect") {
			cat("for Effect:\n");
		} else {
			cat("for Cost:\n");
		}
		lambda=NA
  	} else {
		cat("lambda = ",res$lambda,":\n",sep="");
  	}
  	print.data.frame(res$coef.table)
	cat("\n")
  }
  invisible(object)
}



