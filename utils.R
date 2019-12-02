# utility functions: by JO 
# First created 7-21-2019
# including pre-processing
# ==== Load Packages ====
library(stringr)
library(qdap)
library(qdapRegex)

#' Clean the text before training (custom cleaner for word vectors by JO)
#' \code{require(stringr)}
#' @param x A text vector.
#' @param post_training A logical value, indicating purpose. See below.
#' @param remove_nums A char value, "all": remove all numbers, 
#' "smart": only removes decimal numbers and #s >100.
#' "none" or other: does not remove numbers.
#' @return The \code{cleantxt} a cleaned text vector.
#' @comments Note: Keras tokenizer removes:
#' !\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n
#' post_training= TRUE:  should be used after the w2v model is trained, to clean text before classifier training/testing.
#'   => sentences (CR and perieods) are separated with </s> in prep for training/testing classifier.
#' post_training= FALSE: should be used for training the w2v/glove models.
#'   => sentences (ending with period) split into separate vector elements
#' Purpose: removes many punctuations, splits by periods into sentences (with "</s>").
clean_txt <- function(x, post_training = TRUE, remove_nums = "smart", EHR = TRUE){
  # see https://regexr.com/
  # remove extra spaces, make lower case.
  cleantxt <- x %>% 
    iconv("latin1", "ASCII", sub=" ") %>% 
    iconv("UTF-8", "ASCII", sub=" ") %>% 
    tolower() %>% # lower case
    gsub("_", " ", .) %>%  # replace "_" with spaces (cause, may use _ later for special tokens)
    gsub("\\b\\d{1,2}/\\d{1,2}/(\\d\\d){1,2}\\b", "_date_", .) %>%  # replace dates with token
    gsub("\\b\\d{1,2}-\\d{1,2}-(\\d\\d){1,2}\\b", "_date_", .) %>%  # replace dates with token
    rm_date(., pattern="@rm_date2", replacement = "_date_") %>% # replace Nov 8, 2018 dates with token
    gsub("(\\d{1,2}:\\d{2}(:\\d{2})?)", " _time_ ", .) %>% # replace time with token
    gsub("(?<!\\w)([a-z])\\.", "\\1", ., perl = T) %>% # abbrev periods (keep sentence periods)
    gsub("(dr|mr|ms|mrs|sr|jr)\\.", "\\1", ., perl = T) %>% # title periods
    gsub("(?<=\\S)\\-", "\\1 \\-", ., perl = T) %>% # separate dashes with spaces: ebv- to ebv -
    gsub("([\\-±~])(\\S)","\\1 \\2", ., perl = T) %>% # preceding -, ±, or ~ with space: ~3 to ~ 3
    gsub("(\\d),(\\d)", "\\1\\2", .) %>% # remove commas between digits with no space.
    gsub("[\\/(){}$+?@!|&%:,;<>=^#~]", " ", .) %>%  # replace punct with spaces
    gsub('\\[|\\]|\\*|\\"', ' ', .) %>% # replace brackets [], *, \ with spaces
    gsub("(\\d)([a-zA-Z])","\\1 \\2", ., perl=TRUE) %>% # separate 89yo to 89 yo
    {
      if(EHR){
        # for EHR take care of things like ox3
        gsub("(?<=[a-z])(?=[0-9])","\\1 \\2", ., perl=TRUE) # separate ox3 to ox 3
      } else {
        # separate special situations
        . # do nothing, just pass . along
      } 
    } %>% 
    {
      if(remove_nums=="all") {
        # gsub('[0-9]+', '', .) # remove all nums
        # remove all numbers including negative and decimals
        rm_number(., replacement = "#")
      }
      else if(remove_nums=="smart") { 
        gsub("(\\s)\\-?\\d*\\.\\d+\\b", "\\1", .) %>% # remove decimal numbers only
          gsub('\\b\\d{3,}\\b', '_lgnum_', .) # remove #'s >100, replace with _lgnum_
      }
      else {
        . # just pass along without removing #'s
      }
    } %>% 
    gsub("('|`)s", " ", ., perl = T) %>% # remove 's
    gsub("('|`)", " ", ., perl = T)      # remove '
  
  if(post_training) { # preparing data for classifier with pretrained w2v 
    # replace periods after words, but not numbers, with </s>
    cleantxt <- gsub("\\.(?!\\d)", " </s> ", cleantxt, perl=TRUE) %>% 
      # replace \n with </s>
      gsub("\n", " </s> ", ., perl=TRUE) %>% 
      # remove \r
      gsub("\r", "", ., perl=TRUE) %>% 
      # Pad ends with </s>
      str_trim("right") %>% gsub("(?<!</s>)$", " </s>", ., perl = T)
  } else { # this is for preparing for w2v training
    # split sentences into new vecs by replacing periods after words with splits.
    cleantxt <- unlist(strsplit(cleantxt, "(?<=\\.)\\s(?=[A-Z|a-z])", perl = T))
    cleantxt <- unlist(strsplit(cleantxt, "\n", perl = T))
    # # replace periods after words, but not numbers, with space
    cleantxt <- gsub("((?<![0-9])\\.)|(\\.(?![0-9]))", " ", cleantxt, perl=TRUE)
  }
  # removes: extra spaces, \t, \n
  cleantxt <- str_squish(cleantxt) # removes: spaces, \t, \n
  return(cleantxt)
}

#' Function to Convert text vector to tfidf dfm. matrix
#' @param stem_words A logical value, if true, stem words.
text2dfm <- function(txt, stem_words = TRUE){
  x <- txt %>% 
    # Tokenize training & test sets.
    tokens(what = "word",
           remove_numbers = TRUE, remove_punct = TRUE,
           remove_symbols = TRUE, remove_hyphens = TRUE) %>% 
    # Lower case the tokens
    # tokens_tolower() %>% # not if already lowered before call to text2dfm()
    # remove stopwords
    tokens_remove(stopwords("english")) %>% 
    # Performing stemming on the tokens
    {
      if(stem_words){
        tokens_wordstem(., language = "english")
      }else{
        . # just pass along
      }
    } %>% 
    # Create our 1st bag-of-words model.
    dfm(tolower = FALSE)
  # replace bad feature names e.g. reserved words
  fn <- featnames(x)
  x <- dfm_replace(x, fn, make.names(fn, unique = T), verbose = F)
  return(x)
}

#' Function to Convert text vector to tfidf dfm. matrix
#' but removes all deep learning tokens first,
#' then passes txt to text2dfm()
#' e.g. \code{"_date_" and "</s>"}.
#' @param stem_words A logical value, if true, stem words.
text2dfm_not_dl <- function(txt, stem_words = TRUE){
  x <- txt %>% 
    str_remove_all("</s>") %>% # remove </s>
    str_remove_all("_\\w+_") %>% # remove, _date_ and other _*_ tokens
    text2dfm(stem_words)
  return(x)
}

#' Retrieve first n words from notes that have been cleaned using clean_txt
#' @param x a string
#' @param n number of words to bring back
get_first_n_words <- function(x, n){
  x %>% 
    str_extract(paste0("(\\w+(\\W+|$)){1,",n,"}")) %>% 
    str_trim(side = "right") %>% # remove trailing spaces
    str_remove("\\s\\W+$") # remove trailing # or </
}

#' Get summary metrics
#' collect and output: Accuracy, Precision, Recall, F1, AUC.
#' @param df a data frame to collect the metrix. If NULL, then df is initialized.
#' @param model_name character(), model name.
#' @param test_labels a vector of the test set labels (0's and 1's).
#' @param pred_prob a vector of the predicted probabilities.
summary_metrics <- function(df, model_name, test_labels, pred_prob){
  if(is.null(df)) {
    # initialize df
    df <- data.frame(mod_nm = character(),
                     auc = numeric(),
                     cil = numeric(), 
                     ciu = numeric(), 
                     acc = numeric(), 
                     acc_l = numeric(), 
                     acc_u = numeric(),
                     precision =numeric(),
                     recall = numeric(),
                     F1 = numeric(),
                     stringsAsFactors = FALSE)
  }
  pred_bin <- factor(if_else(pred_prob > .5, 1, 0))
  ROC <- roc(response = test_labels, predictor = pred_prob)
  ci95 <- ci(ROC)
  Auc=ROC$auc
  cil=ci95[1]
  ciu=ci95[3]
  c <- confusionMatrix(pred_bin, factor(test_labels), positive = "1", mode = "everything")
  
  # output results
  cat("Accuracy:", c$overall[["Accuracy"]], "\n")
  cat("Precision:", c$byClass[["Precision"]], "\n")
  cat("Recall:", c$byClass[["Recall"]], "\n")
  cat("F1:", c$byClass[["F1"]], "\n")
  cat("AUC:", round(Auc,3), paste0("(", round(cil,3), "-", round(ciu,3), ")"))
  
  # save results in df
  df %<>% add_row(mod_nm = model_name,
                  auc = Auc, 
                  cil=ci95[1], 
                  ciu=ci95[3],
                  acc =c$overall[["Accuracy"]],
                  acc_l =c$overall[["AccuracyLower"]],
                  acc_u =c$overall[["AccuracyUpper"]],
                  precision =c$byClass[["Precision"]],
                  recall =c$byClass[["Recall"]],
                  F1=c$byClass[["F1"]]
  )
  return(df)
}
