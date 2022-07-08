# Making a report data table

##' Creates a detection data.frame from CQ results, suitable for summary reports. 
##'
##' 
##' @title Make Detection Data.frame
##' @param df data.frame of CQ results, as returned by \code{read_qpcr}
##' @param threshold numeric, the threshold CQ, below which is considered a detection
##' @param labs character vector of length 2 corresponding to the
##'     string to label for 0 (nondetect) and 1 (detect)
##' @param cq_col string, the name of the CQ column
##' @param target_col string, the name of the target column
##' @param filter_col string, the name of the filter column
##' @return data.frame of detection table
##' @author Matt Espe
##' @export
make_detection_table = function(df,
                                threshold = 40,
                                labs = c("ND", "(+)"),
                                cq_col = "Cq",
                                target_col = "Target",
                                filter_col = "FilterID")
{
    tmp = tapply(df[[cq_col]] < threshold, list(df[[filter_col]], df[[target_col]]), any)
    
    ans = lapply(as.data.frame(tmp), function(x)
        factor(as.integer(x), levels = c(0,1), labels = labs))
    ans[[filter_col]] = rownames(tmp)

    ans = as.data.frame(ans[c(filter_col, unique(df$Target))])
    return(ans)
}
