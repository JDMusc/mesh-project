library(dplyr)


yearChar = function(year) ifelse(is.character(year), year, as.character(year))

loadMeshTermsDfYear = function(year) {
  year = yearChar(year)
  
  mesh_fs = dir('data', pattern = '_mesh', full.names = T)
  mesh_fs_year = mesh_fs %>% sapply(function(f) grepl(year, f)) %>% {mesh_fs[.]}
  
  mesh_terms_df = readRDS(mesh_fs_year[1])
  for(i in 2:length(mesh_fs_year)){
    mesh_terms_df = mesh_fs_year[i] %>% readRDS %>% {rbind(mesh_terms_df, .)}
  }
  
  return(mesh_terms_df)
}


getMeshCountsYear = function(year) {
  year = yearChar(year)
  year %>%
  loadMeshTermsDfYear %>% 
  group_by(term, type) %>% 
  summarise(n = n()) %>% 
  as.data.frame
}


loadMeshTermsCounts = function(){
  yearDf = function(year) getMeshCountsYear(year) %>% rename(!!year := n)
  
  years = 2011:2017 %>% as.character
  n_years = length(years)
  
  counts = yearDf(years[1])
  for(y in years[2:n_years]){
    counts = merge(counts, yearDf(y), all = T)
  }
  counts[is.na(counts)] = 0
  
  col_2011_ix = 3
  counts$n = rowSums(counts[, col_2011_ix:(col_2011_ix+n_years - 1)])
  counts$percent_with_term = counts$n/totalPmidCount() * 100
  
  counts = counts %>% arrange(desc(percent_with_term))
  
  return(counts)
}


yearFromFileName = function(records_f) 
  regexpr("201[1-7]", records_f)[1] %>% 
  {substr(records_f, ., . + 3)} %>%
  as.numeric


getMeshTermsDf = function(records_f) {
  records = readRDS(records_f)
  counts = records@Mesh %>% sapply(function(m) ifelse(is.data.frame(m), nrow(m), 0))
  n_terms = sum(counts)
  
  #pre-allocation orders of magnitude faster
  nTermsVec = function(mo) vector(mode = mo, length = n_terms)
  terms = nTermsVec("character")
  term_types = nTermsVec("character")
  pmids = nTermsVec("character")
  orders = nTermsVec("integer")
  years = nTermsVec("integer")
  
  ix = 1
  for(i in 1:length(counts)){
    n_terms = counts[i]
    if(n_terms > 0){
      next_ix = ix + n_terms
      stop_ix = next_ix - 1
      
      mesh = records@Mesh[[i]]
      terms[ix:stop_ix] = as.character(mesh$Heading)
      term_types[ix:stop_ix] = as.character(mesh$Type)
      
      pmids[ix:stop_ix] = records@PMID[i]
      orders[ix:stop_ix] = ix:stop_ix
      years[ix:stop_ix] = records@YearPubmed[i]
      
      ix = next_ix
    }
  }
  
  return(
    data.frame(pmid = pmids, term = terms, 
               type = term_types, 
               rank = orders,
               year = years)
  )
}

saveMeshTermsDf = function(records_f, 
                          dest_f = sub('_records.RDS', '_mesh.RDS', records_f))
  records_f %>% getMeshTermsDf %>% saveRDS(file = dest_f)


saveAllRecordFilesMeshTermsDf = function()
  dir('data', pattern = '_records.RDS', full.names = T) %>% 
  sapply(saveMeshTermsDf)


totalPmidCount = function() 
  dir('data', pattern = '_records.RDS', full.names = T) %>%
  sapply(function(f) length(readRDS(f)@PMID)) %>%
  sum

