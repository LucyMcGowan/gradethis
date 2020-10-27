category_mistake <- function(solution) {
  if (is_infix(solution) | is.call(solution)) {
    solution <- solution[[1]]
  }
  if (!is.character(solution)) solution <- deparse_to_string(solution)
  
  class_tbl <- tidycode::get_classifications(lexicon = "crowdsource", 
                                             include_duplicates = FALSE)
  if (solution %in% class_tbl$func) {
    class_tbl$classification[class_tbl$func == solution]
  }
}
