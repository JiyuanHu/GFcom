\name{simulated.data.significance.bias}
\alias{simulated.data.significance.bias}
\title{
The Generation of Simulated Data with significance bias at GWAS
}
\description{
This function generates simulated two-stage association study data with significance bias at GWAS.
}
\usage{
simulated.data.significance.bias(n.cases1, n.controls1, n.cases2, n.controls2, 
  beta = NULL, OR = NULL, p, pi, C = NULL, alpha = NULL, M = 100, model = 1)
}

\arguments{
  \item{n.cases1}{
Number of disease individuals at GWAS.
}
  \item{n.controls1}{
Number of normal individuals at GWAS.
}
  \item{n.cases2}{
Number of disease individuals at the follow-up study.
}
  \item{n.controls2}{
Number of normal individuals at the follow-up study.
}
  \item{beta}{
True log odds ratio.
}
  \item{OR}{
True odds ratio. Either \code{beta} or \code{OR} should be unNULL.
}
  \item{p}{
Minor allele frequency.
}
  \item{pi}{
Disease prevalence rate.
}
  \item{C}{
The threshold for Wald statistics used for the association test.
}
  \item{alpha}{
The significance level of association tests. Notice that \code{C} is the upper \code{alpha}/2 quantile of a standard normal distribution.
}
  \item{M}{
Number of significant repetitions.
}
  \item{model}{
Genetic model. \code{model}=1 indicates additive genetic model; \code{model}=0 indicates recessive genetic model; \code{model}=2 indicates dominant genetic model.
}
}
\value{
\item{par}{A list containing the input parameters of the function as well as   
  \describe{
    \item{alpha0}{The retrospective baseline log OR.}
    \item{ps}{The genotype frequencies for genotypes AA, Aa and aa, respectively.}
  }
}
\item{est}{A data frame containing 6 variables of \code{M} observations.
  \describe{
    \item{beta1}{The estimated log ORs at GWAS.}
    \item{se1}{The estimated standard error of log ORs at GWAS.}
    \item{beta2}{The estimated log ORs at the follow-up study.}
    \item{se2}{The estimated standard error of log ORs at the follow-up study.}
    \item{beta.com}{The combined estimated log ORs for both studies.}
    \item{se.com}{The combined estimated standard error of log ORs for both studies.}
    }
  }
\item{genotypes.of.2stages}{A matrix of genotype data for both studies. The first 6 columns are the genotype frequency for disease and normal groups at GWAS. The last 6 conlumns are the genotype frequency for disease and normal groups at the follow-up study.
}

}
\references{
HU J, LI X, PAN D, LI Q.(2016) Efficient Estimation of Disease Odds Ratios for the Follow-up Genetic Association Studies.
}
\author{
Jiyuan Hu
}
\examples{
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
# d = simulated.data.significance.bias(n.cases1,n.controls1,n.cases2,n.controls2, beta= beta,
#   p=p,pi=pi,C= C,M=M,model= model);
# d = GFcom.estimate (d,CI.estimation = TRUE,alpha.CI = alpha.CI)

}
\keyword{ simulated data }
\keyword{ significance bias }
