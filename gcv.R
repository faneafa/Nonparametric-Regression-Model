data=read.csv("d://data.csv", header=TRUE)
data
y=as.matrix(data[,2])
x1=as.matrix(data[,3])
x2=as.matrix(data[,4])
x3=as.matrix(data[,5])

fourier=function(y,x1,x2,x3,K)
{
	n=length(y)
	p=ncol(data)-1
	q=(p*(K+1))+1
	C=matrix(0,n,q)
	result=matrix(0,K,2)
	for (k in 1:K)
	{
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
		df=sum(diag(A))
		W=(1/n)*I
		MSE=t(y-yhat)%*%W%*%(y-yhat)
		trace=((1-df)/n)^2
		GCV=MSE/trace
		result[k,1]=k
		result[k,2]=GCV
	}
	print(result)
	GCV2=min(result[,2])
	s=1
	repeat{
		if(result[s,2]==GCV2)
		{
			kOpt=result[s,1]
			GCVOpt=GCV2
			break
		}
		else s=s+1
	}
	cat("The optimal value of K is",kOpt,"\n")
}
fourier(y,x1,x2,x3,k<-(3))
