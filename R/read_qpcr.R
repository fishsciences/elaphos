#' read_qpcr: a function to read the .xls files exported by QuantStudio
#'
#' @description this takes a single .xls file as its input.  It returns a dataframe.
#'
#' @param File the path to the .xls file
#' @param Cq_threshold the Cq value at which to assume non-detection
#' @param Experiment the name of the experiment or project associated with the qpcr data
#' @param Keep_controls logical indicating whether you want to retain the FilterIDs with matching strings provided in the \code{Control_strings} argument
#' @param Column_numbers integer vector corresponding to the columns you want to keep from the raw qpcr output
#' @param Column_names character vector of column names to apply to the \code{Column_numbers}
#' @param Control_strings character vector of strings to check for in the FilterID column that indicate a control sample
#' @return a data frame with the following columns: "Experiment", "TechRep", "PlateID", "Control", and those specified in Column_names.
#'
#' @export

# arguments: file, cq threshold, experiment
read_qpcr = function(File,
                     Cq_threshold = 40.0,
                     Experiment,
                     Keep_controls = FALSE,
                     Column_numbers = c(2, 4, 5, 9),
                     Column_names = c("WellPosition", "FilterID", "Target", "Cq"),
                     Control_strings = c("NTC", "CT+", "0", "BHOO", "DS", "BHOO+", "DS+")) {

d = as.data.frame(read_excel(path = path.expand(File),
                                     sheet = "Results",
                                     skip = 42,
                                     na = ""))

d = d[ , Column_numbers]; colnames(d) = Column_names # keep columns we need

# convert non-detections
d$Cq[d$Cq == "Undetermined"] <- Cq_threshold
d$Cq = as.numeric(d$Cq)

# identify controls
controls = d$FilterID %in% Control_strings
if(!Keep_controls) d = d[!controls, ]

# order Techreps
d = d[order(d$FilterID, d$Target), ]
var = paste(d$FilterID, d$Target)
d$TechRep = sequence(rle(var)$lengths)

# make Experiment column
d$Experiment = Experiment
# make PlateID columns from filename
d$PlateID = paste(basename(File))
d$Control = ifelse(d$FilterID %in% Control_strings, 1, 0)

as.data.frame(d[ , c("Experiment", "TechRep",  "PlateID",  "Control", Column_names)])

}
