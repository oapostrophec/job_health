########## the function to find given cml elements in text

find_cml_elements <- function(what_to_find=c("text", "textarea"), where_to_look) {
  els = paste("<cml:", what_to_find, sep="")
  words = strsplit(where_to_look, split = " ")[[1]]
  num_els = 0
  for (i in 1:length(els)) {
    this_el = els[i]
    els_grepped = grepl(words, pattern=this_el)
    num_els = num_els + sum(els_grepped)
  }
  return(num_els)
}