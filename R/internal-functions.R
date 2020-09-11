
# nocov start

release_questions <- function() {

  c(
    "Have you updated the version number in inst/CITATION (two fields)?",
    "Have you run all the tests listed in cran-comments.md?",
    "Have you evaluated the impact on downstream dependencies?",
    "Have you updated the 'downstream dependencies' section of cran-comments?"
  )
}

# nocov end
