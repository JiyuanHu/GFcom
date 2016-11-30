transform.genotypes <-
function(nsamples,model){
	genotypes = function(x,nsample){
		rep(x,nsample);
	}
	if(model==1){
		x = 0:2;
	}else if (model==0){
		x = c(0,0,1);
	}else{
		x = c(0,1,1);
	}
	genotype.cases = genotypes(x,nsamples[1:3]);
	genotype.controls = genotypes(x,nsamples[4:6]);
	dat = data.frame(disease = c(rep(1,sum(nsamples[1:3])),rep(0,sum(nsamples[4:6]))),
			genotype = c(genotype.cases,genotype.controls));
	return(dat);
}
