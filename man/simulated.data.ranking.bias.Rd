\name{simulated.data.ranking.bias}
\alias{simulated.data.ranking.bias}
\title{
The Generation of Simulated Data with ranking bias at GWAS
}
\description{
This function generates simulated two-stage genetic association study data with ranking bias at GWAS.
}
\usage{
simulated.data.ranking.bias(n.cases1, n.controls1, n.cases2, n.controls2, 
  pi, I, M, model, K, C0, psudo)
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
  \item{pi}{
Disease prevalence rate.
}
  \item{I}{
Number of truely diease-associated SNPs among \code{M} SNPs}
  \item{M}{
Number of simulated SNPs in the genome-wide scale.
}
  \item{model}{
Genetic model. \code{model}=1 indicates additive genetic model; \code{model}=0 indicates recessive genetic model; \code{model}=2 indicates dominant genetic model.
}
  \item{K}{
Number of selected SNPs at the GWAS. Only the top \code{K} most significant SNPs are selected and reported.
}
  \item{C0}{
The screening threshold of the Cochran-Armitage Trend Test (CATT). To speed up the calculation, CATT is undertook first to filter out most of the SNPs. SNPs passing this screening step will further fit the logistic model. 
}
  \item{psudo}{
The multiplier used to calculated the psudo-significance level.
}
}
\value{
\item{par}{A list containing the input parameters of the function as well as:
  \describe{
      \item{beta}{The true log OR value of top \code{K} SNPs.}
      \item{p}{The minor allele frequencies of top \code{K} SNPs.}
      \item{alpha0}{The retrospective baseline log ORs of top \code{K} SNPs.}
      \item{alpha}{The psudo-significance level.}
      \item{C}{The psudo-threshold of Wald statistics for genetic association test.}
  }
}
\item{est}{A data frame containing 6 variables of \code{K} SNPs.
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
}
\keyword{ simulated data }
\keyword{ ranking bias }
