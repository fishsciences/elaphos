#' read_qpcr: a function to read the .xls files exported by QuantStudio
#'
#' @description this takes a single .xls file as its input.  It returns a dataframe.
#'
#' @param file the path to the .xls file
#' @param Cq_threshold the Cq value at which to assume non-detection
#' @param Experiment the name of the experiment or project associated with the qpcr data
#' @return a dataframe with the following columns: "Experiment", "FilterID", "TechRep", "Target", "Cq", "TechRepID", "PlateID", "WellID"
#'
#' @importFrom readxl read_excel
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

d = as.data.frame(d[ , c("Experiment", "TechRep",  "PlateID",  "Control", Column_names)])
return(d)

}
