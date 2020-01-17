import pytest

import utils

def test_toAscii():
  assert utils.toAscii("hello 今日は") == "hello "


def test_removeUnderscore():
  assert utils.removeUnderscore("hello_ok_bye_") == "hello ok bye "


onIn = lambda s: "on " + s + " in ..."
date_token = utils.date_token
    
def test_replaceDateNumbers():
  
  assert utils.replaceDateNumbers("11/14/19") == date_token
  assert utils.replaceDateNumbers("11/14/2019") == date_token
  assert utils.replaceDateNumbers(onIn("11/14/19")) == onIn(date_token)
  
  assert utils.replaceDateNumbers("11-14-19") == date_token
  assert utils.replaceDateNumbers("11-14-2019") == date_token
  assert utils.replaceDateNumbers(onIn("11-14-19")) == onIn(date_token)
  
    
def test_replaceDateStrings():
  assert utils.replaceDateStrings("Nov. 14") == date_token
  assert utils.replaceDateStrings("November 14") == date_token
  assert utils.replaceDateStrings("November 14, 2019") == date_token
  assert utils.replaceDateStrings("Nov 14") == date_token
  assert utils.replaceDateStrings(onIn("Nov 14")) == onIn(date_token)



def test_replaceTimes():
  time_token = utils.time_token
  
  onIn = lambda s: "on " + s + "in ..."
  
  assert utils.replaceTimes("10:30") == time_token
  assert utils.replaceTimes("10:30:28") == time_token
  assert utils.replaceTimes(onIn("10:30:28")) == onIn(time_token)
  
  
def test_removeAbbreviationPeriods():
  assert utils.removeAbbreviationPeriods(
    "d. johnson eats with m. williams") == "d johnson eats with m williams"
    
  assert utils.removeAbbreviationPeriods("m. johnson") == "m johnson"
  assert utils.removeAbbreviationPeriods("mr. johnson") == "mr. johnson"
  
  
def test_removeTitlePeriods():
  assert utils.removeTitlePeriods("mr. johnson") == "mr johnson"
  assert utils.removeTitlePeriods(
    "dr. johnson eats with mrs. williams") == "dr johnson eats with mrs williams"


def test_separateDashes():
  assert utils.separateDashes("check the ebv- ...") == "check the ebv - ..."
  assert utils.separateDashes("ebv-") == "ebv -"


def test_separateSpecialCharacters():
  chars = "-±~"
  for ch in chars:
    assert utils.separateSpecialCharacters("~" + ch) == ("~ " + ch)


def test_removeNumberCommas():
  assert utils.removeNumberCommas("1,00,00") == "10000"
  assert utils.removeNumberCommas(onIn("1,00,00")) == onIn("10000")


def test_removePunctuation():
  for pu in utils.puncts:
    assert utils.removePunctuation(pu) == " "
    assert utils.removePunctuation(onIn(pu)) == onIn(" ")
    assert utils.removePunctuation("hello $!@#$ abc") == "hello       abc"
    
    
def test_removeBracketsStarQuote():
  for c in utils.brackets_star_quote:
    assert utils.removeBracketsStarQuote(c) == " "
    assert utils.removeBracketsStarQuote(onIn(c)) == onIn(" ")


def test_separateNumbersAge():
  age = "89yo"
  age_out = "89 yo"
  assert utils.separateNumbersAge(age) == age_out
  assert utils.separateNumbersAge(onIn(age)) == onIn(age_out)
  assert utils.separateNumbersAge("8yo") == "8 yo"  


def test_separateLettersNumbers():
  word = "ox3"
  word_out = "ox 3"
  assert utils.separateLettersNumbers(word) == word_out
  assert utils.separateLettersNumbers(onIn(word)) == onIn(word_out)
  assert utils.separateLettersNumbers("ox30") == "ox 30"  


def test_replaceNums():
  word = "35"
  word_out = "#"
  assert utils.replaceNums(word) == word_out
  assert utils.replaceNums(onIn(word)) == onIn(word_out)


def test_removeDecimals():
  word = " 35.124"
  word_out = " "
  assert utils.removeDecimals(word) == word_out
  assert utils.removeDecimals(onIn(word)) == onIn(word_out)


def test_replaceLargeInts():
  word = " 1241234"
  word_out = " " + utils.replace_large_ints_replace
  assert utils.replaceLargeInts(word) == word_out
  assert utils.replaceLargeInts(onIn(word)) == onIn(word_out)


def test_removeApostrophes():
  words = ["The cat'", "The cat's house", "The cat`", "The cat`s house"]
  words_out = ["The cat ", "The cat  house", "The cat ", "The cat  house"]
  for ix in range(len(words)):
    word = words[ix]
    word_out = words_out[ix]
    assert utils.removeApostrophes(word) == word_out


def test_strSquish():
  sent = "hello    there.\r how are you?\t I am fine."
  expected = "hello there. how are you? I am fine."
  assert utils.strSquish(sent) == expected


def test_postTrainPrep():
  sent = "We have to go soon. It is getting late \r. That is too bad.\n Alright"
  sent_out1 = "We have to go soon </s>"
  sent_out2 = " It is getting late </s>"
  sent_out3 = " That is too bad </s> </s> Alright </s>"
  assert utils.postTrainPrep(sent) == sent_out1 + sent_out2 + sent_out3
  

def test_w2vPrep():
  text = "The patient came in with poor vital signs. The nurse asked, do you have symptoms? He responded yes."
  sents = ["The patient came in with poor vital signs", 
    "The nurse asked, do you have symptoms?", "He responded yes"]
  assert utils.w2vPrep(text) == sents
  
