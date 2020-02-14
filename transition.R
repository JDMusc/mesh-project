library(dplyr)

source('utils.R')

data = readRDS('pubmed_cr_abstracts.RDS')

clean_data = 'pubmed_cr_abstracts.RDS' %>% 
	readRDS %>% 
	mutate(pmid=as.integer(pmid),
	       txt=clean_txt(paste0(title, " . ", abstract))) %>% 
	select(pmid, label, txt)

saveRDS(clean_data, 'pubmed_clean.RDS')
