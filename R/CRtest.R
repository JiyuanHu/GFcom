CRtest <-
function(cases.stage1,controls.stage1,n.cases,n.controls,model){
	sums = cases.stage1+controls.stage1;
	if(model==1){
		score =0:2;
	}else if (model==0){
		score =c(0,0,1);
	}else{
		score =c(0,1,1);
	}
	numerator = 2*n.controls*rowSums(t(score*t(cases.stage1-controls.stage1)))^2;
	denominator = rowSums(t(score^2*t(sums*((n.cases + n.controls)-sums))))-2*score[2]*score[3]*sums[,2]*sums[,3];
	denominator[denominator==0]=1e-8;
	T = numerator/denominator;
	T
}
