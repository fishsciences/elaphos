library(elaphos)
qpcr_file = system.file("test_qpcr_data/test_qpcr_data/2022-01-18_Mont_Bhoo_P1.xls", package = "elaphos")

if(file.exists(qpcr_file)){
    d2 = read_qpcr(qpcr_file, Experiment = "test")
    
    dd = d2
    dd$Target = "test"
    d2 = rbind(d2, dd) # so there are multiple targets
    ans = elaphos:::make_detection_table(d2)
    
    stopifnot(all(colnames(ans) %in% c("FilterID", "BHOO", "test")))
}
