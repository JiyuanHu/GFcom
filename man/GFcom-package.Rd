\name{GFcom-package}
\alias{GFcom-package}
\alias{GFcom}
\docType{package}
\title{
Efficient Estimation of Disease Odds Ratios for the Follow-up Genetic Association Studies}
\description{
This package is developed to efficiently estimate the disease odds ratios of candidate SNPs for the follow-up genetic association studies. 
OR estimates from the initial GWAS scan suffers the well known selection bias, or the ”Winner’s curse”. The selection bias can become rather severe when the association tests for the candidate markers lacks statistical power. The newly developed OR estimator GFcom produces both point and CI estimates for the candidate SNPs. The point estimate of GFcom is as follows:

beta.GFcom = beta.cMLE - tau* (beta.cMLE - beta2)

where beta.cMLE is the conditional MLE utilizing data from both GWAS and the follow-up study, tau is a shrinkage estimator which reduces the estimation bias of beta.cMLE, beta2 is the MLE produced by the follow-up study. Both the point and CI estimate of GFcom shows efficient performance compared with other OR estimators. 
}
\details{
\tabular{ll}{
Package: \tab GFcom\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-11-29\cr
License: \tab GPL (>= 2)\cr
}
}
\author{
Jiyuan Hu 

Maintainer: Jiyuan Hu <jiyuan_hu@fudan.edu.cn>
}
\references{
HU J, LI X, PAN D, LI Q.(2016) Efficient Estimation of Disease Odds Ratios for the Follow-up Genetic Association Studies.
}
\keyword{ package }
\examples{
#################################################
#############simulating significance bias data###
beta = log(1.3); #log Odds Ratio
p =0.30;#Minor allele frequency
pi =0.01;#Disease prevalence rate
C = 5; #Threshold of Wald test or
#alpha = 5e-7; #Significance level
n.cases1=1000;
n.controls1=1000;
n.cases2= 2000;
n.controls2 = 2000;
M = 100;
model=1;
alpha.CI = 0.05;
############
# Not run:
# d = simulated.data.significance.bias(n.cases1,n.controls1,n.cases2,n.controls2,
#   beta= beta,p=p,pi=pi,C= C,M=M,model= model);
# d = GFcom.estimate (d,CI.estimation = TRUE,alpha.CI = alpha.CI);

##################################################
#############simulating ranking bias data#########
psudo = 1.1;
n.cases1 = n.controls1 = 1000;
n.cases2 = n.controls2 = 2000;
pi =0.01;
I = 100; 
M = 1e5;
model = 1;
K = 10; 
C0 = 12;
alpha.CI = 0.05; 
############
# Not run:
# drank = simulated.data.ranking.bias(n.cases1,n.controls1,n.cases2,n.controls2,
#   pi=pi,I, M=M,model= model,K= K,C0 = C0,psudo= psudo);
# drank = GFcom.estimate (drank,CI.estimation = TRUE,alpha.CI = alpha.CI);

##################################################
#############Real data(I) with significance bias##
data(CD);
############
# Not run:
# d = data.preprocessing(genotypes.of.2stages = CD,psudo= 1.1);
# d = GFcom.estimate (d,CI.estimation = TRUE,alpha.CI = 0.05,reportOR = TRUE);

##################################################
#############Real data(II) with ranking bias##
data(lung)
############
# Not run:
#d = data.preprocessing(dat = lung,psudo= 1.1);
#d = GFcom.estimate (d,CI.estimation = TRUE,alpha.CI = 0.05,reportOR = TRUE);
}
