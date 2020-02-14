import pandas as pd
import pyreadr as R

import utils

original_data = R.read_r('pubmed_cr_abstracts.RDS')[None]
original_data = original_data.fillna('na')
original_data['txt'] = original_data.title + ' . ' + original_data.abstract

clean_data = R.read_r('pubmed_clean.RDS')[None]

n_recs = original_data.shape[0]

pyCleaned = lambda i: utils.cleanTxt(original_data['txt'][i])
rCleaned = lambda i: clean_data['txt'][i]
origData = lambda i: original_data['txt'][i]

def cleanCheck(i):
    match = False
    err = False
    try:
        match = pyCleaned(i) == clean_data['txt'][i]
    except:
        err = True

    return (match, err)


matches = [cleanCheck(i) for i in range(0, n_recs)]

matches = pd.DataFrame(matches, columns = ['match', 'error'])
matches['pmid'] = original_data['pmid'][0:n_recs]
