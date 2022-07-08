# test compare_cols
library(elaphos)
library(stats)

d = structure(
  list(
    lut_Species_MitoFish = c(
      NA,
      NA,
      NA,
      NA,
      "Pan paniscus",
      "Pan paniscus",
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA
    ),
    BLAST_scientific_name = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "Homo sapiens    ",
      "Homo sapiens    ",
      "Homo sapiens    ",
      "Homo sapiens    ",
      "Homo sapiens    ",
      "Homo sapiens    ",
      "no hit"
    ),
    Species = c(
      "Phoxinus sp",
      "Larus dominicanus",
      "Larus dominicanus",
      "Ichthyaetus relictus",
      "Pan paniscus",
      "Pan paniscus",
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "Homo sapiens"
    )
  ),
  row.names = c(1L, 2L, 3L, 4L,
                5L, 6L, 2631L, 2632L, 2633L, 2634L, 2635L, 2636L, 13L),
  class = "data.frame"
)

test = d[1, ] 
stopifnot(apply(test, 1, elaphos:::check_row)) # should return TRUE

test = unlist(d[13, ]) # should return FALSE
stopifnot(!(elaphos:::check_row(test)))

test = d[5, ] # should return TRUE
stopifnot(elaphos:::check_row(test))

d$target_agreement = check_targets(d)
stopifnot(sum(d$target_agreement) == 12)
