category_mistake <- function(mistake) {
  if (is_infix(mistake) | is.call(mistake)) {
    mistake <- mistake[[1]]
  }
  if (!is.character(mistake)) mistake <- deparse_to_string(mistake)
  
  class_tbl <- tidycode::get_classifications(lexicon = "crowdsource", 
                                             include_duplicates = FALSE)
  if (mistake %in% class_tbl$func) {
    class_tbl$classification[class_tbl$func == mistake]
  }
}
