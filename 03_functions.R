regextract = function(pattern, text){
  text_ext = regmatches(x = text,
                        m = gregexpr(pattern = pattern,
                                     text = text))
  return(unlist(text_ext))
}