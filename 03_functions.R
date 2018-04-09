regextract = function(pattern, text){
  text_ext = regmatches(x = text,
                        m = gregexpr(pattern = pattern,
                                     text = text))
  return(unlist(text_ext))
}

unique_noun_check = function(x, pattern){
  return(unique(unlist(lapply(x, FUN = "grep", pattern = pattern, value = TRUE))))
}