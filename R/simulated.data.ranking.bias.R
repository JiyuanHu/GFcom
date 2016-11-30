simulated.data.ranking.bias <-
function(n.cases1,n.controls1,n.cases2,n.controls2, pi,I, M,model,K,C0,psudo){
	dat = first.stage.simulation(n.cases1,n.controls1,pi,I,M,model);
	d = selection.by.rank(dat,C0,K,model,psudo);
	d = followup.stage.simulation(d,n.cases2,n.controls2,model);
	return(d);
}
