data=read.csv("d://data.csv", header=TRUE)
data
y=as.matrix(data[,2])
x1=as.matrix(data[,3])
x2=as.matrix(data[,4])
x3=as.matrix(data[,5])

estimation=function(y,x1,x2,x3,K)
{
	n=length(y)
	p=ncol(data)-2
	q=(p*(K+1))+1
	C=matrix(0,n,q)
	result=matrix(0,K,2)
	error=rep(0,n)
cat("=============================================================")
	cat("\n\ty\t\tyhat\t\terror") 
cat("\n=============================================================")
	for(i in 1:n)
	{
		for(r in 1:k)
		{
			C[i,1]=1
			C[i,2]=x1[i]
			C[i,2+r]=cos(r*x1[i])
			C[i,3+k]=x2[i]
			C[i,3+k+r]=cos(r*x2[i])
			C[i,4+(2*k)]=x3[i]
			C[i,4+(2*k)+r]=cos(r*x3[i])
		}
	}
	library(pracma)
	I=diag(1,n,n)
	A=C%*%pinv(t(C)%*%C)%*%t(C)
	yhat=A%*%y
	beta=rep(0,q)
	beta=pinv(t(C)%*%C)%*%t(C)%*%y
	error=y-yhat
	MSE=sum((error)^2)/n
	for(i in 1:n)
	{
		cat("\n","\t",y[i],"\t",yhat[i],"\t",error[i],"\n")
	}
cat("\n=========================================================\n")
	cat("MSE=",MSE,"\n")
	S=0
		for(i in 1:n)
		{
			s=(yhat[i]-mean(y))^2
			S=S+s
		}
		F=0
		for(i in 1:n)
		{
			f=(y[i]-mean(y))^2
			F=F+f
		}
	R=S/F
	cat("Coefficient of determination =")
	print(R)
	print(beta)
write.csv(yhat,file="d://yhat.csv")
write.csv(beta,file="d://parameter.csv")
}
dx=estimation(y,x1,x2,x3,K=3)
