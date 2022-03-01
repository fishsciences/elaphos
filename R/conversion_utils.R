##' Convenience function to convert from Cq to [eDNA] and vice-versa
##'
##' @title Convert CQ value to [eDNA] and vice-versa
##' @param Cq_values numeric vector, value of CQ
##' @return numeric vector, [eDNA] values
##' @rdname lnconc_to_cq
##' @details Mirror of the same functions from the artemis package (fishsciences.github.io/artemis)
##' @export
cq_to_lnconc = function(Cq_values, std_curve_alpha, std_curve_beta)
{
  (Cq_values - std_curve_alpha) / std_curve_beta
}


##' @title Convert [eDNA] to Cq
##' @param x log[eDNA] values
##' @param std_curve_alpha the alpha (intercept) value for the
##'     standard curve
##' @param std_curve_beta the beta (slope) value for the standard
##'     curve
##' @param censor logical, whether Cq values larger than the upper_Cq
##'     should be censored. If TRUE, these any values above the
##'     threshold will be replaced with the upper_Cq value.
##' @param upper_Cq the max Cq value
##' @return vector of Cq values
##' @author Matt Espe
##' @export
lnconc_to_cq = function(x,
                        std_curve_alpha,
                        std_curve_beta,
                        censor = TRUE, upper_Cq = 40)
  
{
  ans = std_curve_beta * x +
    std_curve_alpha
  
  if(censor)
    ans[ans > upper_Cq] = upper_Cq
  ans
}
