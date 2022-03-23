

fpath = system.file("test_qpcr_data/", package = "elaphos")

files = list.files(path = fpath, pattern = ".xls", full.names = TRUE)

cqs = lapply(files, read_qpcr, Experiment = "test")

cqs = do.call(rbind, cqs)

# should be 0 NAs in the test data
stopifnot(sum(colSums(is.na(cqs))) == 0)

# should be 0 controls in this data, since keep controls was false
stopifnot(sum(cqs$Control) == 0)

# test keep_controls
wconts = lapply(files, read_qpcr, Experiment = "test controls", Keep_controls = TRUE)
wconts = do.call(rbind, wconts)

# should be some controls
stopifnot(!sum(wconts$Control) == 0)

length(cqs$FilterID)
table(cqs$FilterID, cqs$Target)
table(cqs$Target, cqs$TechRep)
