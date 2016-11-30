\name{GFcom.estimate}
\alias{GFcom.estimate}
\title{
The point and CI estimates of odds ratios (log odds ratios) for the follow-up genetic association studies.
}
\description{
This function gives the point and CI estimates of odds ratios (log odds ratios) for follow-up genetic association studies.
}
\usage{
GFcom.estimate(d, CI.estimation = TRUE, alpha.CI = 0.05, reportOR = FALSE)
}
\arguments{
  \item{d}{
  A list produced by functions \code{data.preprocessing()}, \code{simulated.data.significance.bias()} or \code{simulated.data.ranking.bias()}. The list contains the following elements: \code{pars}, \code{est} and \code{genotypes.of.2.stages}.
}
  \item{CI.estimation}{
If \code{CI.estimation == TRUE}, this function will produce CI estimates for log ORs.
}
  \item{alpha.CI}{
The significance level of the confidence interval.
}
  \item{reportOR}{
If \code{reportOR == TRUE}, this function will produce CI estimates for ORs as well.
}
}
\value{
The returning object is still the input list \code{d}, with the following elements added to \code{d} itself:
  \item{point.est}{
    The point estimate of GFcom for log ORs.
  }
  \item{CI.est}{
    The confidence interval estimate of GFcom for log ORs.
  }
  \item{point.est.OR}{
    The point estimate of GFcom for ORs.
  }
  \item{CI.est.OR}{
    The confidence interval estimate of GFcom for ORs.
  }
}
\references{
HU J, LI X, PAN D, LI Q.(2016) Efficient Estimation of Disease Odds Ratios for the Follow-up Genetic Association Studies.
}
\author{
Jiyuan Hu
}
\examples{
#################################################
#############simulating significance bias data###
beta = log(1.3); #Log Odds Ratio
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
# d = simulated.data.significance.bias(n.cases1,n.controls1,n.cases2,n.controls2,beta= beta,
#   p=p,pi=pi,C= C,M=M,model= model);
# d = GFcom.estimate (d,CI.estimation = TRUE,alpha.CI = alpha.CI)

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
#  pi=pi,I, M=M,model= model,K= K,C0 = C0,psudo= psudo);
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
data(lung);
############
# Not run:
# d = data.preprocessing(dat = lung,psudo= 1.1);
# d = GFcom.estimate (d,CI.estimation = TRUE,alpha.CI = 0.05,reportOR = TRUE);
}
\keyword{ GFcom }

