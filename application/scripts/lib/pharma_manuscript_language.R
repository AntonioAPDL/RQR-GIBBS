pma_language_findings <- function(text) {
  text <- gsub("[[:space:]]+", " ", text)
  patterns <- c(
    "named scan comparator" = "\\bTCSP\\b|\\bTSCP\\b|tcsp_mc",
    "empirical content as tolerance confidence" =
      "held[- ]out (empirical )?(content|coverage) (is|establishes|verifies|guarantees|proves|provides) (the |nominal )*(tolerance|coverage) confidence",
    "posterior probability as tolerance confidence" =
      "posterior (content probability|coverage) (is|equals|guarantees|establishes) (the |nominal )*tolerance confidence",
    "endpoint summaries as prediction intervals" =
      "(generalized[- ]posterior endpoint summaries|fitted intervals) (are|provide) (posterior )?predicti(ve|on) intervals",
    "statistical intervals as specifications" =
      "(fitted|reported|MTI[- ]ECM) intervals (are|define|serve as) (product |regulatory |release |process[- ]control |specification )*(limits|specifications)",
    "external benchmark as proposed method" =
      "(our|proposed) (Young[- ]+Mathew|Wilks)",
    "overlapping splits as independent evidence" =
      "(overlapping|repeated|random) splits (are|provide) independent (sampling )?replications",
    "local analysis terminology" = "\\bmetadata\\b|\\bpipeline\\b|\\bcontract\\b|article[- ]facing",
    "group separation as proof of independence" =
      "(campaign|lot|group) separation (establishes|proves|guarantees) independence",
    "unresolved measurements as corrected data" =
      "(hardness|tensile) (values|measurements) were (corrected|repaired) automatically"
  )
  names(patterns)[vapply(patterns, grepl, logical(1L), x = text,
                         ignore.case = TRUE, perl = TRUE)]
}
