alpha.est <-
function(ps,beta,pi,model){
	if(model ==1){#additive model:
		x = 0:2;
	}else if (model ==0) {#recessive model  
		x = c(0,0,1);
	}else{#dominant model
		x = c(0,1,1);
	}	
	f = function(alpha){
		prob = exp(alpha+ x*beta)/(1+exp(alpha+ x*beta));
		sum(prob*ps) -pi;
	}
	res = uniroot(f,interval = c(-10,10),tol = 1e-15);
	res$root;
}
