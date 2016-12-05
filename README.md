# GFcom
Efficient Estimation of Disease Odds Ratios for the Follow-up Genetic Association Studies

Nowadays, follow-up genetic association studies are considered as an essential procedure to validate the association findings from the initial GWAS scan. One key problem in the follow-up studies is how to accurately estimate the disease odds ratios (OR) of the candidate genetic markers.
The traditional adjusted estimators developed for the OR estimation in GWAS have limited performance in the follow-up study data. 
This indicates that directly applying the adjusted estimators to the follow-up studies might be inappropriate. 
GFcom fills this gap and presents a novel shrinkage estimator of ORs explicitly for the follow-up studies. 
Both simulation studies and real data applications demonstrated the statistical efficiency improvement of our shrinkage estimator over the traditional estimators. 

The manual file is **"GFcom-manual.pdf"**. 

Installation of GFcom in R:

> library(‘devtools’);

> install_github(‘JiyuanHu/GFcom’);
