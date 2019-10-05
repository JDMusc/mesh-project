library(dplyr)
library(RISmed)
source('PMID_functions.R')


basicQuery = function() '"case reports"[Publication Type]'

getRandomRecords = function(n_pubs_per_call, n_calls, 
                            mindate = 2011, maxdate = 2017) {
  query_str <- '"case reports"[Publication Type]'
  pmids <- GetPMIDs(query_str, mindate=mindate, maxdate=maxdate, n_calls = n_calls,
                    n_pubs_per_call = n_pubs_per_call)
  
  search_query <- EUtilsSummary(query_str, retmax = n_pubs_per_call, mindate=mindate, maxdate=maxdate)
  search_query@PMID <- pmids
  records <- EUtilsGet(search_query)
  
  return(records)
}


getYearCount = function(year){
  query_str <- basicQuery()
  sumYear <- EUtilsSummary(query_str, retmax = 1, mindate = year, maxdate = year)
  return(sumYear@count)
}


getPMIDsForYear = function(year){
  count = getYearCount(year)
  pmids <- GetPMIDs(basicQuery(), mindate = year, maxdate = year, n_calls = 1,
                    n_pubs_per_call  = count)
  return(pmids)
}

saveRandomRecordsForPmids = function(pmids, query_str = basicQuery(), 
                                     n_calls = 10, n_pubs_call = 10000,
                                     mindate = 2011, maxdate = 2017) {
  search_query <- EUtilsSummary(query_str, retmax = n_pubs_call, mindate=mindate, maxdate=maxdate)
  
  for(i in 1:n_calls) {
    start_ix = (i - 1)*n_pubs_call + 1
    stop_ix = start_ix + n_pubs_call - 1
    search_query@PMID <- pmids[start_ix:stop_ix]
    records <- EUtilsGet(search_query)
    f_name = paste0(i, "records.RDS")
    saveRDS(records, f_name)
  }
}



saveYearRecords = function(year, n_calls = 7) {
  pmids = getPMIDsForYear(year)
  count = length(pmids)
  
  n_records_call = count %/% n_calls
  remainder = count %% n_calls
  
  makeSummaryQuery = function (start_ix, stop_ix) {
    n = stop_ix - start_ix + 1
    sum_query <- EUtilsSummary(basicQuery(), retmax = n, mindate=year, maxdate=year)
    sum_query@PMID <- pmids[start_ix:stop_ix]
    return(sum_query)
  }
  
  makeFName = function(i) paste(year, i, "records.RDS", sep = '_')
  
  for(i in 1:n_calls) {
    start_ix = (i - 1)*n_records_call + 1
    stop_ix = start_ix + n_records_call - 1
    
    makeSummaryQuery(start_ix, stop_ix) %>% 
      EUtilsGet %>%
      saveRDS(makeFName(i))
  }
  
  if(remainder > 0){
    start_ix = count - remainder + 1
    stop_ix = count
    makeSummaryQuery(start_ix, stop_ix) %>% 
      EUtilsGet %>% 
      saveRDS(makeFName(n_calls + 1))
  }
  
}