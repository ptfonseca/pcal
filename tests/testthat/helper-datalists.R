
data <- read.delim("data.txt")

for (col in seq_along(data)) {
  assign(paste0(tolower(names(data)[col]), "_bl1"), msdigit(data[, col]))
  assign(paste0(tolower(names(data)[col]), "_bl2"), smsdigit(data[, col]))
}

datalist_bl1 <- list(
  austria_bl1,
  belgium_bl1,
  finland_bl1,
  france_bl1,
  germany_bl1,
  greece_bl1,
  ireland_bl1,
  italy_bl1,
  luxembourg_bl1,
  netherlands_bl1,
  portugal_bl1,
  spain_bl1
)

datalist_bl2 <- list(
  austria_bl2,
  belgium_bl2,
  finland_bl2,
  france_bl2,
  germany_bl2,
  greece_bl2,
  ireland_bl2,
  italy_bl2,
  luxembourg_bl2,
  netherlands_bl2,
  portugal_bl2,
  spain_bl2
)
