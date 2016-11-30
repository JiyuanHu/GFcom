repetition <-
function(n.cases1,n.controls1,n.cases2,n.controls2,ps,alpha,beta,pi,C,model,M){
	C0=C^2*.96;
	No = 0;
	if(beta<0.25){
		rep.No = 1e6;
	}else{
		rep.No = 1e5;
	}
	No.repetition = 1;
	probs = get.probs(ps,pi,alpha,beta,model);
	genotype.stage1 = res.stage1= NULL;T.trend.stage1 = NULL;
	time1 = Sys.time();
	while (No <=M){
		dat = sampling(n.cases1,n.controls1,probs,rep.No);
		cases = dat$cases;sums = dat$sums;controls = dat$controls;
		if(model==1){
			score =0:2;
		}else if (model==0){
			score =c(0,0,1);
		}else{
			score =c(0,1,1);
		}		
		numerator = 2*n.controls1*rowSums(t(score*t(cases-controls)))^2;
		denominator = rowSums(t(score^2*t(sums*((n.cases1+n.controls1)-sums))))-2*score[2]*score[3]*sums[,2]*sums[,3];
		T = numerator/denominator;
		ind = which(T>C0);
		K = length(ind);
		if(K>0){
			if(K >10*M){ind = ind[1:(50*M)]; K = 50*M}
			tmp = matrix(cbind(cases,controls)[ind,],nrow = (K));
			Ttmp = T[ind];
			res = matrix(NA,nrow = K,ncol =3);
			for (i in 1:K){
				if(K==1){
					dat1 = transform.genotypes(tmp,model);
				}else{
					dat1 = transform.genotypes(tmp[i,],model);
				}
				fit1 = glm(disease~genotype,family=binomial(link="logit"),data = dat1);
				stat = summary(fit1)$coefficients[2,];
				res[i,]= stat[1:3];
				}
				ind2 = which(abs(res[,3])>C);
			if(length(ind2)>0){
				No = No + length(ind2);
				if(K==1){
				genotype.stage1 = rbind(genotype.stage1,tmp);				
				}else{
				genotype.stage1 = rbind(genotype.stage1,tmp[ind2,]);				
				}
				T.trend.stage1 = c(T.trend.stage1,Ttmp[ind2]);
				res.stage1 = rbind(res.stage1,res[ind2,]);
			}
			No.repetition = No.repetition +1;				
		}	
	}
	totalN = rep.No*No.repetition;totalM = No;power1 = totalM/totalN;
	if(No>M){
		genotype.stage1 =genotype.stage1[1:M,];
		res.stage1 =res.stage1[1:M,];
		T.trend.stage1 =T.trend.stage1[1:M];			
	}
	M1 = length(T.trend.stage1);
	dat.stage2 = sampling(n.cases2,n.controls2,probs,rep.No=M1);		
	genotype.stage2 = cbind(dat.stage2$cases,dat.stage2$controls);
	genotypes.of.2stages = cbind(genotype.stage1,genotype.stage2);
	colnames(genotypes.of.2stages) = c('DAA1','DAa1','Daa1','NAA1','NAa1','Naa1','DAA2','DAa2','Daa2','NAA2','NAa2','Naa2');
	res.stage2 = matrix(NA,nrow = M1,ncol =2);
	for (i in 1:M1){
		tmp2 = genotype.stage2[i,];	
		dat2 = transform.genotypes(tmp2,model);
		fit2 = glm(disease~genotype,family=binomial(link="logit"),data = dat2);
		stat2= summary(fit2)$coefficients[2,];		
		res.stage2[i,]=stat2[1:2];
	}
	beta1 = res.stage1[,1];se1 = res.stage1[,2];beta2 = res.stage2[,1];se2 = res.stage2[,2];		
	beta.com = se2^2/(se1^2+se2^2)*beta1 + se1^2/(se1^2+se2^2)*beta2;
	se.com = se1*se2/sqrt(se1^2+se2^2);	
	est = cbind(beta1,se1,beta2,se2,beta.com,se.com);
	name = c('beta1','se1','beta2','se2','beta.com','se.com');	
	colnames(est)=name;
	est = data.frame(est)
	res =list(est=est,genotypes.of.2stages = genotypes.of.2stages);	
	rm(dat,est,genotypes.of.2stages);gc();
	return(res);	
}
