#' check_targets
#'@title Check target columns for agreement

##'Wrapper function for `check_row`, which checks three elements of a named vector against each other and returns TRUE (if they match) or FALSE (if they don't) while ignoring NAs.
##' 
##' @param row named vector or row of a data frame
##' @param new_row default is to unlist `row`
##' @return a logical vector
##' @author Matt Espe, Myfanwy Johnston
##' @export

check_targets = function(dataframe, cols = c("lut_Species_MitoFish", 
                                                    "BLAST_scientific_name", 
                                                    "Species")) {
  d = dataframe[, cols]
  apply(d, 1, check_row)
  
}

# this is the function that check_targets wraps around

check_row = function(row, new_row = na.omit(unlist(row))) {
  length(unique(new_row)) == 1 
}