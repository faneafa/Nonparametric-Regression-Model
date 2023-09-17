data=read.csv("d://data.csv", header=TRUE)
yhat=read.csv("d://yhat.csv", header=TRUE)
beta=read.csv("d://parameter.csv", header=TRUE)
data=as.matrix(data)
y=as.matrix(data[,2])
beta=as.matrix(beta)
yhat=as.matrix(yhat[,2])
res=y-yhat
ybar=mean(y)
Bbar=mean(beta[,2])
alpha=0.05
n=nrow(data)
q=nrow(beta)
SSR=sum((yhat-ybar)^2)
SSE=sum((y-yhat)^2)
SST=SSR+SSE
MSR=SSR/(q-1)
MSE=SSE/(n-q)
Rsq=(SSR/SST)*100

#F-test
cat("------------------------------------","\n")
cat("Conclusion of simultaneous test results","\n")
cat("------------------------------------","\n")
Fcal=MSR/MSE
pvalue=pf(Fcal,(q-1),(n-q),lower.tail=FALSE)
if (pvalue<=0.05){
cat("Reject Ho. There is at least 1 significant predictor","\n")
cat("","\n")}else{
cat("Failed to reject Ho. All predictors had no significant effect","\n")
cat("","\n")}

#t-test
cat("------------------------------------","\n")
cat("Conclusion of individual test results","\n")
cat("------------------------------------","\n")
tcal=rep(NA,q)
pval=rep(NA,q)
for (i in 1:q)
{
	S=((beta[i,2]-Bbar)^2)/(q-1)
	SE=S/SSE
	tcal[i]=beta[i,2]/SE
	pval[i]=2*(pt(abs(tcal[i]),(n-q),lower.tail=FALSE))
	if (pval[i]<=0.05){
		cat("Reject Ho. The predictor was significant with pvalue",pval[i],"\n")}else{
		cat("Failed to reject Ho. The predictor was not significant with pvalue",pval[i],"\n")}
}
cat("Analysis of Variance","\n")
cat("======================================","\n")
cat("Source df SS MS Fcal","\n")
cat("Regression ",q-1," ",SSR," ",MSR,"",Fcal,"\n")
cat("Error ",n-q," ",SSE,"",MSE,"\n")
cat("Total ",n-1," ",SST,"\n")
cat("======================================","\n")
cat("s=",sqrt(MSE)," Rsq=",Rsq,"\n")
cat("pvalue(F)=",pvalue,"\n")