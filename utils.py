import datefinder as df
import re
from toolz import pipe as p
import unicodedata


def cleanTxt(x, post_training = True, remove_nums = "smart", EHR = True):
  return p(x,
    toAscii, 
    lambda _: _.lower(),
    removeUnderscore,
    replaceDates,
    replaceTimes,
    removeAbbreviationPeriods,
    removeTitlePeriods,
    separateDashes,
    separateSpecialCharacters,
    removeNumberCommas,
    removePunctuation,
    removeBracketsStarQuote,
    separateNumbersAge,
    lambda _: separateLettersNumbers(_) if EHR else _,
    lambda _: replaceNums(_) if remove_nums == "all" else (
      replaceLargeInts(removeDecimals(_)) if remove_nums == "smart" else _),
    removeApostrophes,
    lambda _: postTrainPrep(_) if post_training else w2vPrep(_)
    )


def toAscii(uni_str):
  return unicodedata.normalize(
    'NFKD', uni_str).encode(
      'ascii','ignore')


def removeUnderscore(sent):
  return re.sub('_', ' ', sent)


date_token = "_date_"
def replaceDates(sent):
  words = [sent[s:e].strip().replace('on ', '') 
    for (_, (s, e)) in df.find_dates(sent, index = True)]
  for w in words:
    sent = sent.replace(w, date_token)
  
  return sent
  

time_token = "_time_"
time_regex = r"(\d{1,2}:\d{2}(:\d{2})?)"
def replaceTimes(sent):
  return re.sub(time_regex, time_token, sent)
  

abbrev_period_regex = r"(?<!\w)([a-z])\."
def removeAbbreviationPeriods(sent):
  return re.sub(abbrev_period_regex, r"\1", sent)


title_periods_regex = r"(dr|mr|ms|mrs|sr|jr)\."
def removeTitlePeriods(sent):
  return re.sub(title_periods_regex, r"\1", sent)


separate_dashes_regex = r"(?<=)(\S)-"
separate_dashes_replace = r"\1 -"
def separateDashes(sent):
  return re.sub(separate_dashes_regex, separate_dashes_replace, sent)


separate_special_characters_regex = r"([-±~])(\S)"
separate_special_characters_replace = r"\1 \2"
def separateSpecialCharacters(sent):
  return re.sub(separate_special_characters_regex, 
    separate_special_characters_replace, 
    sent)


remove_number_commas_regex = r"(\d),(\d)"
remove_number_commas_replace = r"\1\2"
def removeNumberCommas(sent):
  return re.sub(remove_number_commas_regex,
    remove_number_commas_replace,
    sent)
    

puncts = r"(){}$+?@!|&%:,;<>=^#~"
remove_puncts_regex = "[" + puncts + "]"
remove_puncts_replace = r" "
def removePunctuation(sent):
  return re.sub(remove_puncts_regex,
    remove_puncts_replace,
    sent)


brackets_star_quote = ["[", "]", "*", '"']
remove_brackets_star_quote_regex = r"[\[*\"\]]"
remove_brackets_star_quote_replace = r" "
def removeBracketsStarQuote(sent):
  return re.sub(remove_brackets_star_quote_regex,
    remove_brackets_star_quote_replace,
    sent)


separate_numbers_age_regex = r"(\d)([a-zA-Z])"
separate_numbers_age_replace = r"\1 \2"
def separateNumbersAge(sent):
  return re.sub(separate_numbers_age_regex,
    separate_numbers_age_replace,
    sent)


separate_letters_numbers_regex = r"([a-z]+)([0-9]+)"
separate_letters_numbers_replace = r"\1 \2"
def separateLettersNumbers(sent):
  return re.sub(separate_letters_numbers_regex,
    separate_letters_numbers_replace,
    sent)
    

replace_nums_regex = r'[0-9]+'
replace_nums_replace = '#'
def replaceNums(sent):
  return re.sub(replace_nums_regex,
    replace_nums_replace,
    sent)


remove_decimals_regex = r'(\s)-?\d*\.\d+'
remove_decimals_replace = r'\1'
def removeDecimals(sent):
  return re.sub(remove_decimals_regex,
    remove_decimals_replace,
    sent)


replace_large_ints_regex = r'\d{3,}'
replace_large_ints_replace = '_lgnum_'
def replaceLargeInts(sent):
  return re.sub(replace_large_ints_regex,
    replace_large_ints_replace,
    sent)
    

remove_apostrophes_regex = r"('|`)s?"
remove_apostrophes_replace = " "
def removeApostrophes(sent):
  return re.sub(remove_apostrophes_regex,
    remove_apostrophes_replace,
    sent)


def strSquish(sent):
  return " ".join(sent.split())


def postTrainPrep(sent):
  sent = re.sub(r"\.(?!\d)", " </s> ", sent)
  sent = re.sub(r"\n", " </s> ", sent)
  sent = re.sub(r"\r", "", sent)
  sent = re.sub(r"(?<!</s>)$", " </s>", sent)
  return strSquish(sent)


regexMapFlat = lambda re_fn, string_vec: [s for string in string_vec for s in re_fn(string)]
regexMap = lambda re_fn, string_vec: [re_fn(string) for string in string_vec]
def w2vPrep(text):
  return p(
    text, 
    lambda _: re.split(r"(?<=[\?\.])\s{1,2}(?=[A-Z|a-z])", _),
    lambda sent_vec: regexMapFlat(lambda s: re.split(r"\n", s), sent_vec),
    lambda sent_vec: regexMap(
      lambda s: re.sub(r"((?<![0-9])\.)|(\.(?![0-9]))", " ", s), # # replace periods after words, but not numbers
      sent_vec),
    lambda sent_vec: regexMap(strSquish, sent_vec)
    ) 
