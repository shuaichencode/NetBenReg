test_that("NetBenReg() prints error for data error", {
  data(CEdata)
  CEdata=CEdata[1:200,]

  #inconsistent n in data
  expect_error(NetBenReg(Followup=CEdata$survival[-1],delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Length of delta is different to length of Followup.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead[-1],group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Length of delta is different to length of Followup.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt[-1],Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Length of group is different to length of Followup.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[-1,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Number of rows of Cost is different to length of Followup.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[-1,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Number of rows of Eff is different to length of Followup.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[-1,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Number of rows of covariates Z is different to length of Followup.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Doubly.Robust=TRUE, PS.Z=CEdata[-1,5:7], Eff.only=TRUE, 
	Cost.only=TRUE, lambda=1, L=10),
	regexp="Number of rows of covariates PS.Z is different to length of Followup.\n"
  )

  #wrong dim for Followup/delta/group
  expect_error(NetBenReg(Followup=CEdata[,1:2],delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Followup must be a vector.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata[,1:2],group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="delta must be a vector.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata[,1:2],Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="group must be a vector.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata[,0],Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="group must be a vector.\n"
  )

  # too large L
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Part.times=1:15,
	Method='SW',Z=CEdata[,5:7], Eff.only=TRUE,lambda=1,L=15),
	regexp="Time limit L is greater than max follow-up times. Choose a smaller L.\n"
  )

  # too small L
  expect_error(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=0),
	regexp="Time limit L must be positive.\n"
  )

  # character L
  expect_error(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L="*"),
	regexp="L must be numeric.\n"
  )

  # vector L, error
  expect_error(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=c(10,15)),
	regexp="Time limit L is not a scalar.\n"
  )

  #inconsistent length of Part.times and Cost/Eff data
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,9:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Length of Part.times is different to number of columns of Cost.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,25:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Length of Part.times is different to number of columns of Eff.\n"
  )

  #wrong data structure Cost/Eff
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=list(CEdata[,8:22]),Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Cost must be a vector/matrix/data.frame.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=list(CEdata[,24:38]),				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Eff must be a vector/matrix/data.frame.\n"
  )

  #empty Cost/Eff
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,0],Eff=CEdata[,25:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Number of columns in Cost is 0.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,0],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Number of columns in Eff is 0.\n"
  )

  # data value out of normal range
  CEdata1=CEdata
  CEdata1$dead[1]=-1
  expect_error(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Values in delta must be 0 or 1.\n"
  )
  CEdata1=CEdata
  CEdata1$Trt[1]=-1
  expect_error(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Values in group must be 0 or 1 and cannot be in one group only.\n"
  )
  CEdata1=CEdata
  CEdata1$survival[1]=0
  expect_error(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Values in Followup cannot be zero or negative.\n"
  )

  #one group only
  CEdata1=CEdata
  CEdata1$Trt=0
  expect_error(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Values in group must be 0 or 1 and cannot be in one group only.\n"
  )

  ## wrong lambda value
  #<0
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=c(-1,1), L=10),
	regexp="Values in lambda cannot be negative.\n"
  )

  #character 
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda="a", L=10),
	regexp="lambda must be numeric.\n"
  )

  #matrix
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=matrix(c(1:4),2,2), L=10),
	regexp="lambda must be a vector.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=matrix(c(1:2),2,1), L=10),
	regexp="lambda must be a vector.\n"
  )

  #NA
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=NA, L=10),
	regexp="lambda must be numeric.\n"
  )

  #none when not fitting cost/eff-only model
  expect_error(NetBenReg(Followup=CEdata$survival[-1],delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=FALSE, Cost.only=FALSE, lambda=NULL, L=10),
	regexp="lambda is required if not fitting effectiveness-only or cost-only regression.\n"
  )

  # wrong Part.times value
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=c(-1,2:15), Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Values in Part.times cannot be negative.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=c(NA,2:15), Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="NA value in Part.times is not allowed.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=c("*",2:15), Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Part.times must be numeric.\n"
  )

  # interaction is not a vector
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], interaction=data.frame(a=c("Z","LBBB")),Eff.only=TRUE, 
	Cost.only=TRUE, lambda=1, L=10),
	regexp="interaction must be a vector.\n"
  )

  #PS.trim>=0.5, error
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Doubly.Robust=TRUE, PS.trim=0.5, Eff.only=TRUE, Cost.only=TRUE, 
	lambda=1, L=10),
	regexp="PS.trim is too large. Must be <0.5. Recommend to be between 0 and 0.1.\n"
  )
  # PS.trim character
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Doubly.Robust=TRUE, PS.trim="*", Eff.only=TRUE, Cost.only=TRUE, 
	lambda=1, L=10),
	regexp="PS.trim must be numeric.\n"
  )
  # PS.trim vector
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Doubly.Robust=TRUE, PS.trim=c(0,1), Eff.only=TRUE, Cost.only=TRUE, 
	lambda=1, L=10),
	regexp="PS.trim is not a scalar.\n"
  )

# other options out of range
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='OLS', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Method must be one of 'SW', 'PT', 'CC', 'AL'.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Sep.K=-1,Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Sep.K must be either TRUE or FALSE.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=2, Cost.only=TRUE, lambda=1, L=10),
	regexp="Eff.only must be either TRUE or FALSE.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=10, lambda=1, L=10),
	regexp="Cost.only must be either TRUE or FALSE.\n"
  )
  expect_error(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, Doubly.Robust="a",lambda=1, L=10),
	regexp="Doubly.Robust must be either TRUE or FALSE.\n"
  )

})

test_that("NetBenReg() prints warning for certain data", {
  data(CEdata)
  CEdata=CEdata[1:200,]

  #negative Eff is allowed 
  #negative Cost is allowed without error, but will have a warning about cost<0
  CEdata1=CEdata
  CEdata1[1,8]=CEdata1[1,24]=-1
  expect_warning(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="There is negative value in Cost.\n"
  )

  #PS.trim<0
  expect_warning(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Doubly.Robust=TRUE, PS.trim=-0.1, Eff.only=TRUE, Cost.only=TRUE, 
	lambda=1, L=10),
	regexp="PS.trim is negative. Propensity scores are not trimmed.\n"
  )
})

test_that("NetBenReg() prints message for certain data", {
  data(CEdata)
  CEdata=CEdata[1:200,]

  ## use total cost/eff only
  #SW
  # message about not using Part.time
  expect_message(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,23],Eff=CEdata[,39],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Part.times is not used since only total Cost and Eff are provided.\n"
  )
  # message about no Eff
  expect_message(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,23],Eff=NULL,				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Eff is not provided. Assume effectiveness is survival time.\n"
  )
  #PT
  # message about using SW
  expect_message(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,23],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='PT', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Only total Cost provided. SW method is used.\n"
  )
  # message about using SW, message about not using Part.time
  expect_message(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,23],Eff=CEdata[,39],				
	Part.times=1:15, Method='PT', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  )

  #message about using SW
  expect_message(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,39],				
	Part.times=1:15, Method='PT', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Only total Eff provided. SW method is used.\n"
  )
  # message about using SW, message about no Eff
  expect_message(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,23],Eff=NULL,				
	Part.times=1:15, Method='PT', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  )

  #PS.Z not used since DR not used, hence do not check PS.Z size, but will print a message about not using PS.Z
  expect_message(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], PS.Z=CEdata[-1,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="Non-doubly robust method is used, and hence PS.Z is not used.\n"
  )

  #NA along with other values in lambda, no error, just a message about removing NA from lambda
  expect_message(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=c(1,NA), L=10),
	regexp="NA value in lambda is removed.\n"
  )

  # interaction has element not in Z
  expect_message(NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], interaction=c("Z","LBBB"),Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10),
	regexp="One or more variables in interaction are not in covariates Z, which are removed from interaction.\n"
  )

})


test_that("NetBenReg() excludes observation with missing data", {
  data(CEdata)
  CEdata1=CEdata[1:200,]
  CEdata1$survival[1]=NA
  CEdata1$dead[2]=NA
  CEdata1$Trt[3]=NA
  CEdata1[4,8]=NA
  CEdata1[5,24]=NA
  CEdata1[6,5]=NA
  CEdata1[7,7]=NA

  expect_equal(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)[[1]]$n,193)

  #PS.Z not used and hence n=194 
  expect_equal(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:6], PS.Z=CEdata1[,5:7],Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)[[1]]$n,194)

  #PS.Z used and hence n=193
  expect_equal(NetBenReg(Followup=CEdata1$survival,delta=CEdata1$dead,group=CEdata1$Trt,Cost=CEdata1[,8:22],Eff=CEdata1[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata1[,5:6], Doubly.Robust=TRUE,PS.Z=CEdata1[,5:7],Eff.only=TRUE, Cost.only=TRUE, 
	lambda=1, L=10)[[1]]$n,193)

})

test_that("NetBenReg() works for non-censored data (OLS method)", {
  data(CEdata)
  CEdata=CEdata[1:200,]
  CEdata$dead=1

  #confirm with R package ccostr results for AL method 
  result1<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],			
	Part.times=1:15, Method='SW',Eff.only=TRUE, Cost.only=TRUE, L=10)
  expect_equal(result1[[1]]$L,10)
  expect_equal(result1[[1]]$censorrate,0)
  expect_equal(result1[[1]]$Method,"OLS")
  expect_equal(result1[[1]]$est, c(5.5022416,0.5397421),tolerance=10^(-6))
  expect_equal(result1[[2]]$est, c(14.318189,3.258949),tolerance=10^(-6))

  result2<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],			
	Part.times=1:15, Method='SW',Eff.only=TRUE, Doubly.Robust=T,Cost.only=TRUE, L=10)
  expect_equal(result2[[1]]$Method,"DR_OLS")
  expect_equal(result2[[1]]$est, 0.5397421,tolerance=10^(-6))
  expect_equal(result2[[2]]$est, 3.258949,tolerance=10^(-6))
})

test_that("NetBenReg() works for SW method", {
  data(CEdata)
  CEdata=CEdata[1:200,]

  #confirm with R package ccostr results for CC/AL/SW method (non-DR, no covariates, and survival as Eff)
  result1<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],			
	Part.times=1:15, Method='SW',Eff.only=TRUE, Cost.only=TRUE, L=10)
  expect_equal(result1[[1]]$est, c(7.0017029,0.9702369),tolerance=10^(-6))
  expect_equal(result1[[2]]$est, c(17.138312,1.878267),tolerance=10^(-6))

  result1_2<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],			
	Part.times=1:15, Method='AL',Eff.only=TRUE, Cost.only=TRUE, L=10)
  expect_equal(result1_2[[1]]$est, c(5.5022416,0.5397421),tolerance=10^(-6))
  expect_equal(result1_2[[2]]$est, c(14.318189,3.258949),tolerance=10^(-6))

  result1_3<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],			
	Part.times=1:15, Method='CC',Eff.only=TRUE, Cost.only=TRUE, L=10)
  expect_equal(result1_3[[1]]$est, c(5.6684215,0.9560267),tolerance=10^(-6))
  expect_equal(result1_3[[2]]$est, c(16.989283,2.976691),tolerance=10^(-6))

  #with covariates and QALY 
  result2<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result2[[1]]$lambda,1)
  expect_equal(result2[[2]]$lambda,NA)
  expect_equal(result2[[3]]$lambda,NA)
  expect_equal(result2[[1]]$Reg.type, "NBR")
  expect_equal(result2[[2]]$Reg.type, "Effect")
  expect_equal(result2[[3]]$Reg.type, "Cost")
  expect_equal(result2[[1]]$est, c(-14.1424731,-1.5511383,-0.5660164,3.5264564,-1.6475049),tolerance=10^(-6))
  names(result2[[1]]$se)=NULL
  expect_equal(result2[[1]]$se, c(0.8880101,1.2304859,1.3322049,1.3089410,1.1814137),tolerance=10^(-6))

  #with covariates, interaction and QALY 
  result3<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], interaction=names(CEdata[,5:7]), Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result3[[1]]$est, c(-13.60125926,-3.25026827,-0.40316264,0.73760175,-1.00904510,-0.01162947,4.83997395,-0.83469177),
	tolerance=10^(-6))
  names(result3[[1]]$se)=NULL
  expect_equal(result3[[1]]$se, c(0.9719616,1.8218765,2.0031200,2.0838745,1.8879829,2.7560806,2.7211575,2.4310212),tolerance=10^(-6))

  #total cost/eff
  result3<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,23],Eff=CEdata[,39],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result3[[1]]$est, c(-14.9937464,0.3194587,0.5084204,4.5693905,-2.9524940),tolerance=10^(-6))
  names(result3[[1]]$se)=NULL
  expect_equal(result3[[1]]$se, c(1.299140,1.537191,1.606845,1.552384,1.747725),tolerance=10^(-6))

})

test_that("NetBenReg() works for DR SW method", {
  data(CEdata)
  CEdata=CEdata[1:200,]

  result1<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='SW', Z=CEdata[,5:7], interaction="LBBB",Doubly.Robust=TRUE, PS.Z=CEdata[,5:7],
	Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result1[[1]]$est,  -1.159175,tolerance=10^(-6))
  expect_equal(result1[[1]]$se, 1.305585,tolerance=10^(-6))
  PScoef=c(result1[[1]]$PSmodel[,1])
  names(PScoef)=NULL
  expect_equal(PScoef,c(-0.5091174,-0.7322510,1.5096399,0.2462307),tolerance=10^(-6))
  expect_equal(c(result1[[1]]$Regmodel[,1]),c(-13.3980108,-3.6224978,-0.5069367,0.8774262,-1.4483318,4.6732970),tolerance=10^(-6))

  result2<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],			
	Part.times=1:15, Method='PT', interaction="LBBB",Doubly.Robust=TRUE, PS.Z=CEdata$LBBB,
	Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result2[[1]]$est,   -1.68189,tolerance=10^(-6))
  expect_equal(result2[[1]]$se, 1.292848,tolerance=10^(-6))
  PScoef=c(result2[[2]]$PSmodel[,1])
  names(PScoef)=NULL
  expect_equal(PScoef,c(-0.5596158,1.4210983),tolerance=10^(-6))
  expect_equal(c(result2[[1]]$Regmodel[,1]),c(-10.0155619,-0.7890219),tolerance=10^(-6))
})


test_that("NetBenReg() works for PT method", {
  data(CEdata)
  CEdata=CEdata[1:200,]

  #with covariates, interaction, and QALY 
  result1<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='PT', Z=CEdata[,5:7], interaction="LBBB",Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result1[[1]]$est, c(-13.6279978,-3.7869694,-0.7123743,1.5756090,-1.3532303,4.5636984),tolerance=10^(-6))
  names(result1[[1]]$se)=NULL
  expect_equal(result1[[1]]$se, c(0.8255161,1.5701285,1.2413490,1.7057099,1.0166169,2.2476256),tolerance=10^(-6))

  #total cost/eff
  result2<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,23],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='PT', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result2[[1]]$est, c( -14.8090623,0.3689726,0.5248684,4.3698223,-2.8264182),tolerance=10^(-6))
  names(result2[[1]]$se)=NULL
  expect_equal(result2[[1]]$se, c(1.325171,1.571074,1.647222,1.587821,1.784594),tolerance=10^(-6))

  #total cost/eff
  result3<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,39],				
	Part.times=1:15, Method='PT', Z=CEdata$LBBB, Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result3[[1]]$est, c(-15.258548,-1.529262,3.353396),tolerance=10^(-6))
  names(result3[[1]]$se)=NULL
  expect_equal(result3[[1]]$se, c(0.8789711,1.1868505,1.1899282),tolerance=10^(-6))
})

test_that("NetBenReg() works for DR PT method", {
  data(CEdata)
  CEdata=CEdata[1:200,]

  result1<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='PT', Z=CEdata[,5:7], interaction="LBBB",Doubly.Robust=TRUE, PS.Z=CEdata$LBBB,
	Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result1[[1]]$est,-1.489651,tolerance=10^(-6))
  expect_equal(result1[[1]]$se, 1.148301,tolerance=10^(-6))
  PScoef=c(result1[[1]]$PSmodel[,1])
  names(PScoef)=NULL
  expect_equal(PScoef,c(-0.5596158,1.4210983),tolerance=10^(-6))
  expect_equal(c(result1[[1]]$Regmodel[,1]),c(-13.6279978,-3.7869694,-0.7123743,1.5756090,-1.3532303,4.5636984),tolerance=10^(-6))

  result2<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],			
	Part.times=1:15, Method='PT', interaction="LBBB",Doubly.Robust=TRUE, PS.Z=CEdata$LBBB,
	Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result2[[1]]$est, -1.68189,tolerance=10^(-6))
  expect_equal(result2[[1]]$se, 1.292848,tolerance=10^(-6))
  PScoef=c(result2[[2]]$PSmodel[,1])
  names(PScoef)=NULL
  expect_equal(PScoef,c(-0.5596158,1.4210983),tolerance=10^(-6))
  expect_equal(c(result2[[1]]$Regmodel[,1]),c(-10.0155619,-0.7890219),tolerance=10^(-6))

  result3<-NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],				
	Part.times=1:15, Method='PT', Z=CEdata$LBBB,interaction="LBBB",Doubly.Robust=TRUE, 
	Eff.only=TRUE, Cost.only=TRUE, lambda=1, L=10)
  expect_equal(result3[[1]]$est,-1.477937,tolerance=10^(-6))
  expect_equal(result3[[1]]$se,  1.047668,tolerance=10^(-6))
  PScoef=c(result3[[1]]$PSmodel[,1])
  names(PScoef)=NULL
  expect_equal(PScoef,c(0.1402293),tolerance=10^(-6))
  expect_equal(c(result3[[1]]$Regmodel[,1]),c(-15.207785,-1.477937,3.638166),tolerance=10^(-6))
})


test_that("NetBenReg snapshot", {
  data(CEdata)	#load simulated cost-effectiveness data
  CEdata=CEdata[1:200,]
  lambda <- 3	# choose cost-effectiveness threshold 

  # fit covariate-adjusted net-benefit regression (along with cost-only and effect-only regressions) using SW method, effectiveness is QALY
  # use cost and effectiveness history to better truncate them by L
  expect_snapshot(
     NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38], 
     	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=lambda, L=10)
  )

  # fit unadjusted net-benefit regression (eg, for randomized studies), by removing option Z 
  expect_snapshot(
     NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38], 
    	Part.times=1:15, Method='SW', Eff.only=TRUE,Cost.only=TRUE, L=10, lambda=lambda)
  )
    
  # fit covariate-adjusted regression using SW method, effectiveness is life years
  expect_snapshot(
     NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=NULL,
    	Part.times=1:15, Method='SW', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=lambda, L=10)
  )
    
  # fit covariate-adjusted regression using PT method with QALY as effectiveness
  expect_snapshot(
     NetBenReg(Followup=CEdata$survival,delta=CEdata$dead,group=CEdata$Trt,Cost=CEdata[,8:22],Eff=CEdata[,24:38],
    	Part.times=1:15,Method='PT',Z=CEdata[,5:7], Eff.only=TRUE,Cost.only=TRUE, lambda=lambda,L=10)
  )
    
  # (dataset with unequal time intervals) assume cost.1 (QALY.1) is cost/QALY in first 2 years, cost.2 (QALY.2) and cost.3 (QALY.3) are cost/QALY in the following 6 months
  # other time intervals keep the same
  # set intervals as [0,2],(2,2.5],(2.5,3],(3,4],...,(14,15] using Part.times option
  expect_snapshot(
     NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38], 
    	Part.times=c(2,2.5,3:15), Method='PT', Z=CEdata[,5:7], Eff.only=TRUE, Cost.only=TRUE, lambda=lambda, L=10)
  )
    
  # SW: fit covariate-adjusted regression with interactions between treatment and covariates  #######
  # include both Treatment by LBBB and Treatment by Female interactions 
  expect_snapshot(
     NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38], 
    	Part.times=1:15, Method='SW', Z=CEdata[,5:7], interaction=c("LBBB","Female"), Eff.only=TRUE, Cost.only=TRUE, lambda=lambda, L=10)
  )

  # PT: fit covariate-adjusted regression with interactions between treatment and covariates, effectiveness is life years  #######
  # include both Treatment by LBBB and Treatment by Female interactions 
  expect_snapshot(
     NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=NULL, 
    	Part.times=1:15, Method='PT', Z=CEdata[,5:7], interaction=c("LBBB","Female"), Eff.only=TRUE, Cost.only=TRUE, lambda=lambda, L=10)
  )
  
  # Doubly robust SW regression with interaction to estimate causal average incremental net benefit (INB)
  # all three covariates are used to estimate propensity scores by logistic regression
  expect_snapshot(
     NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38],
    	Part.times=1:15, Method='SW', Z=CEdata[,5:7], interaction=names(CEdata[,5:7]), PS.Z=CEdata[,5:7],	Doubly.Robust=TRUE,
    	Eff.only=TRUE, Cost.only=TRUE, lambda=lambda, L=10)
  )

  # Doubly robust PT regression with Treatment by LBBB interaction to estimate causal average incremental net benefit (INB)
  # all three covariates are used to estimate propensity scores by logistic regression
  fit7<-NetBenReg(Followup=CEdata$survival, delta=CEdata$dead, group=CEdata$Trt, Cost=CEdata[,8:22], Eff=CEdata[,24:38],
    	Part.times=1:15, Method='PT', Z=CEdata[,5:7], interaction=c("LBBB"), PS.Z=CEdata[,5:7],	Doubly.Robust=TRUE,
    	Eff.only=TRUE, Cost.only=TRUE, lambda=lambda, L=10)
  expect_snapshot(print(fit7))
  expect_snapshot(summary(fit7[[1]]$PS))
})







