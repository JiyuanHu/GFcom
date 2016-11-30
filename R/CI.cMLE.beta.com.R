CI.cMLE.beta.com <-
function(beta.com,se1,se2,C,se.com,alpha.CI=0.05,cMLE.beta){
	density.func = function(x,beta){
		g_x = function(x){
			pnorm((x-C*se1)/(se.com*se1/se2))+pnorm((-x-C*se1)/(se.com*se1/se2));
		}
		numerator = dnorm((x-beta)/se.com)*g_x(x);
		denominator.func = function(t){
			(dnorm((t-beta)/se.com)+dnorm((t+beta)/se.com))*g_x(t);
		}
		tmp = numerator/integrate(denominator.func,0,Inf,stop.on.error = FALSE)$value;
	}
	find.root1 = function(beta){
		integrate(density.func,-Inf,beta.com,beta=beta,stop.on.error = FALSE)$value-alpha.CI/2;
	}
	upper = tryCatch(uniroot(find.root1,lower=cMLE.beta,upper=1)$root,error=function(e) NA)
	find.root2 = function(beta){
		integrate(density.func,-Inf,beta.com,beta=beta,stop.on.error = FALSE)$value-(1-alpha.CI/2);	
	}
	lower = tryCatch(uniroot(find.root2,lower=-1,upper=cMLE.beta)$root,error=function(e) NA)
	return (c(lower,upper));
}
