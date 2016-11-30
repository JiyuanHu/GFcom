MLE.beta.com <-
function(beta.com,se1,se2,C,se.com){
	L = function(beta,x){
		g_x = function(x){
			pnorm((x-C*se1)/(se.com*se1/se2))+pnorm((-x-C*se1)/(se.com*se1/se2));
		}
		numerator = dnorm((x-beta)/se.com,log = TRUE)+log(g_x(x));
		denominator.func = function(t){
			(dnorm((t-beta)/se.com)+dnorm((t+beta)/se.com))*g_x(t);
		}		
		all = numerator- log(integrate(denominator.func,0,Inf)$value);		
		-all;
	}	
	optims = optim(par = beta.com,fn=L,x=beta.com,method = 'L-BFGS-B',lower = -1,upper=1,control = list(maxit = 1e7),hessian=TRUE);
	beta.MLE = optims$par;
	convergence = optims$convergence;
	hessian = optims$hessian[1,1];
	while(convergence!=0 | hessian<0){
		s = runif(1,-1,1);
		optims = optim(par = s,fn=L,x=beta.com,method = 'L-BFGS-B',lower = -1,upper=1,control = list(maxit = 1e7),hessian =TRUE);
		if(optims$convergence ==0 & optims$hessian[1,1]>0){
			beta.MLE = optims$par;
			hessian = optims$hessian[1,1];
		}
		convergence = optims$convergence;
	}
	se.MLE = sqrt(1/hessian);
	c(beta.MLE,se.MLE);
}
