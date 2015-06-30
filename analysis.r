library(stringr)
library(dplyr)
library(memoise)
library(rjson)

# Read URL content, with throttling and caching of the response
if (!file.exists('read_url.rds')) {
  read_url <- memoise(function(url, throttle=getOption('ads.throttle', 1)) {
    Sys.sleep(throttle)
    con <- url(url)
    on.exit(close(con))
    message(url)
    return(readLines(con, warn=FALSE))
  })
} else {
  read_url <- readRDS('read_url.rds')
}

read_urls <- function(urls) {
  return(sapply(urls, read_url))
}

# Read ADS dev key
ads_key <- Sys.getenv("ADS_KEY")

# Get pubdates for the first 10 papers that are 1st author
ads_url <- str_c("http://adslabs.org/adsabs/api/search/?q=author:%22^@author.%22&sort=DATE+asc&fl=pubdate&max_rows=40&filter=property:refereed&dev_key=", ads_key)

journals <- c('APJ', 'MNRAS', 'A&A', 'AJ', 'PASP', 'ICAR', 'NATUR', 'SCI')

# Read list of authors from authors.rds, as a character vector where each item is Lastname, F.
# authors.rds not included (for privacy reasons), but you can easily create your own.
data <- data_frame(author=unlist(readRDS("authors.rds"))) %>%
  mutate(id=row_number()) %>%
  filter(!(str_detect(author, 'Array') | str_detect(author, 'University') | str_detect(author, 'Collaboration') | str_detect(author, 'University'))) 

# Loop through the list of authors
intervals <- ldply(data$author, function(a) {
  # Get the list of papers from the JSON returned by the ADS API
  url <- str_replace(ads_url, '@author', URLencode(a))
  text <- NULL
  tries <- 0
  error <- NULL
  while (is.null(text)) {
    text <- tryCatch(read_urls(url), error=function(e) { error <<- e; Sys.sleep(20); NULL })
    tries <- tries + 1
    if (tries > 10)
      stop(error)
  }
  docs <- fromJSON(text)$results$docs

  # Loop through the list of papers for the author
  ldply(docs, function(d) {
    # Extract the journal and year from the bibcode
    match <- str_match(d$bibcode, '([[:digit:]]{4})([[:alnum:]&]{1,5})')
    year <- as.numeric(match[2])
    journ <- str_to_upper(match[3])

    # Only include the paper if the journal is in the list of allowed journals
    if (journ %in% journals) {
      month <- as.numeric(str_match(d$pubdate, '[[:digit:]]{4}-([[:digit:]]{2})')[2])
      data_frame(author=a, bibcode=journ, year=year, month=month, months=year*12+month)
    }
  }, .id=NULL)
})

minyear <- 1960

# Create "intervals" dataframe -- the diff column will contain the number of months elapsed
# between the (i+1)-th and the i-th npap.
intervals <- intervals %>%
  # Inner join with the authors dataframe in order to get the author IDs for cross-checking
  inner_join(data) %>%
  # Remove the name of the author for privacy reasons
  select(-author) %>%
  group_by(id) %>%
  # Create a npap column containing the paper index (1st, 2nd, etc.), and a
  # min_year column (the year of the first paper published)
  mutate(zero_month=any(month == 0), min_year=min(year), npap=row_number()) %>%
  # Remove any author that has a paper with a 0-month stamp, or with first paper published before
  # minyear
  filter(zero_month == FALSE & min_year > minyear) %>%
  # Compute differences between successive papers
  mutate(diff=c(NA, diff(months))) %>%
  ungroup() 

write.csv(intervals, file="data.csv")

# Calculate quantiles
quantiles <- intervals
quantiles <- quantiles %>%
  # Only look at first 15 papers
  filter(npap <= 15 & npap > 1) %>%
  group_by(npap) %>%
  # Calculate quantiles
  summarise(q10 = quantile(diff, 0.1, na.rm=TRUE),
            q25 = quantile(diff, 0.25, na.rm=TRUE),
            q50 = quantile(diff, 0.5, na.rm=TRUE),
            avg = mean(diff, na.rm=TRUE),
            q75 = quantile(diff, 0.75, na.rm=TRUE),
            q90 = quantile(diff, 0.9, na.rm=TRUE)) %>%
  ungroup()

max_npap <- intervals
max_npap <- max_npap %>%
  group_by(id) %>%
  summarise(max_npap = max(npap))

# Plot the quantiles
p <- ggplot(quantiles, aes(x=npap)) +
  geom_ribbon(aes(ymin=q25, ymax=q75), alpha=0.25) + 
  geom_line(aes(y=q50)) +
  geom_point(aes(y=q50)) +
  ylab("Median number of months between papers") +
  xlab("Number of papers") 

p2 <- ggplot(max_npap, aes(x=max_npap)) +
  geom_histogram(binwidth=1) +
  ylab("Number of authors") +
  scale_x_discrete()

p3 <- ggplot(intervals, aes(x=bibcode)) +
  xlab("Journal") +
  ylab("Number of papers published") +
  geom_histogram()

p4 <- ggplot(intervals, aes(x=factor(month))) +
  xlab("Month") +
  ylab("Number of papers published") +
  geom_histogram(binwidth=1)

# Save response cache to disk upon exit
saveRDS(read_url, 'read_url.rds')
