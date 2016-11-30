\name{lung}
\alias{lung}
\docType{data}
\title{Estimated Odds Ratios (OR) of candidate SNPs for lung cancer}
\description{
This is a real follow-up study data of lung cancer. This data contains the estimated ORs and corresponding 95\% confidence intervals of 10 candidate SNPs for the initial GWAS scan and the follow-up study.
}
\usage{data(lung)}
\format{
  A data frame containing six variables for 10 SNPs.
  \describe{
    \item{\code{OR1}}{The OR estimates at the GWAS.}
    \item{\code{OR1.L}}{The lower bound of 95\% CI of ORs at the GWAS.}
    \item{\code{OR1.U}}{The upper bound of 95\% CI of ORs at the GWAS.}
    \item{\code{OR2}}{The OR estimates at the follow-up study.}
    \item{\code{OR2.L}}{The lower bound of 95\% CI of ORs at the follow-up study.}
    \item{\code{OR2.U}}{The upper bound of 95\% CI of ORs at the follow-up study.}
  }
}
\references{
Amos CI, Wu X, Broderick P et al. Genome-wide association scan of tag SNPs identifies a susceptibility locus for lung cancer at 15q25.1. Nature Genetics 2008; 40(5): 616–622.
}
\keyword{Real data}
\keyword{ ranking bias }