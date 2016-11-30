\name{data.preprocessing}
\alias{data.preprocessing}
\title{
Real data preprocessing used to calculate parameters for the OR estimation.
}
\description{
This function does the preprocessing steps, calculates parameters for the OR estimation of follow-up genetic association studies.
}
\usage{
data.preprocessing(dat = NULL, genotypes.of.2stages = NULL, model = 1, psudo = 1.1)
}
\arguments{
  \item{dat}{
  A data frame containing six variables. Each row represents one candidate SNP.
  \describe{
    \item{\code{OR1}}{The OR estimates at the GWAS.}
    \item{\code{OR1.L}}{The lower bound of 95\% CI of ORs at the GWAS.}
    \item{\code{OR1.U}}{The upper bound of 95\% CI of ORs at the GWAS.}
    \item{\code{OR2}}{The OR estimates at the follow-up study.}
    \item{\code{OR2.L}}{The lower bound of 95\% CI of ORs at the follow-up study.}
    \item{\code{OR2.U}}{The upper bound of 95\% CI of ORs at the follow-up study.}
  }
}
  \item{genotypes.of.2stages}{
  A data frame containing 12 variables. Each row represents one candidate SNP. In the following A is the reference allele and a is the variant allele, respectively.
  \describe{
    \item{\code{DAA1}}{The frequency of genotype AA in disease group at GWAS scan.}
    \item{\code{DAa1}}{The frequency of genotype Aa in disease group at GWAS scan.}
    \item{\code{Daa1}}{The frequency of genotype aa in disease group at GWAS scan.}
    \item{\code{NAA1}}{The frequency of genotype AA in normal group at GWAS scan.}
    \item{\code{NAa1}}{The frequency of genotype Aa in normal group at GWAS scan.}
    \item{\code{Naa1}}{The frequency of genotype aa in normal group at GWAS scan.}
    \item{\code{DAA2}}{The frequency of genotype AA in disease group at the follow-up study.}
    \item{\code{DAa2}}{The frequency of genotype Aa in disease group at the follow-up study.}
    \item{\code{Daa2}}{The frequency of genotype aa in disease group at the follow-up study.}
    \item{\code{NAA2}}{The frequency of genotype AA in normal group at the follow-up study.}
    \item{\code{NAa2}}{The frequency of Aa in normal group at the follow-up study.}
    \item{\code{Naa2}}{The frequency of genotype aa in normal group at the follow-up study.}
  }}
  \item{model}{
Genetic model. \code{model}=1 by default, which indicates the additive genetic model.
}

  \item{psudo}{
  The multiplier used to calculated the psudo-significance level. \code{psudo}=1.1 by default.
}
}
\value{
\item{par}{A list containing the following elements:
  \describe{
      \item{alpha}{
        The psudo-significance level.
      }
      \item{C}{
        The psudo-threshold of Wald statistics for the association test.
      }
  \item{K}{
      Number of candidate SNPs selected at the GWAS. Only the top \code{K} most significant SNPs are selected and reported.
    }
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
\item{genotypes.of.2stages}{A matrix of genotype data for both studies. The first 6 columns are the genotype frequency for disease and normal groups at GWAS. The last 6 conlumns are the genotype frequency for disease and normal groups at the follow-up study. This element might be missed if the input parameter \code{genotypes.of.2stages}=NULL.
}
}
\references{
HU J, LI X, PAN D, LI Q.(2016) Efficient Estimation of Disease Odds Ratios for the Follow-up Genetic Association Studies.
}
\author{
Jiyuan Hu
}
\examples{
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
\keyword{ Real data }

