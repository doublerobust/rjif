# Offline transport for tests, demos, and dry runs.
# It is NOT a Jev emulator: it produces *plausible* structured answers with
# pseudo-probabilities derived from keyword overlap, so you can exercise the
# Rjif control flow and calibration code without a key. Set:
#   options(Rjif.transport = rjif_mock_transport)
rjif_mock_transport <- function(body) {
  state <- tolower(body$state %||% "")
  words <- unique(strsplit(gsub("[^a-z0-9 ]", " ", state), " ")[[1]])
  words <- words[nzchar(words)]
  # fraction of a query text's word set that appears in the state,
  # squished away from 0/1 so thresholds actually do something
  query_signal <- function(txt) {
    toks <- unique(strsplit(gsub("[^a-z0-9 ]", " ", tolower(txt)), " ")[[1]])
    toks <- toks[nzchar(toks)]
    if (!length(toks) || !length(words)) return(0.3)
    hits <- sum(toks %in% words)
    min(0.95, 0.05 + 0.9 * hits / length(toks))
  }
  score_one <- function(txt) query_signal(txt)
  tok_in <- nchar(body$state %||% "") %/% 4L
  answers <- lapply(body$questions, function(q) {
    if (q$type == "noul") {
      v <- score_one(q$instructions)
      list(type = "noul", noul = v)
    } else if (q$type == "choice") {
      crit <- names(q$criteria)
      raws <- vapply(crit, function(k)
        score_one(paste(k, q$criteria[[k]])) + stats::runif(1, 0, 0.05),
        numeric(1))
      probs <- exp(raws) / sum(exp(raws))
      pick <- crit[which.max(probs)]
      conf <- min(1, max(0.05, probs[[which.max(probs)]] + stats::runif(1, -0.2, 0.1)))
      list(type = "choice", choice = pick,
           probabilities = as.list(round(probs, 4)),
           confidence = round(conf, 4))
    } else {
      levels <- as.character(q$criteria)
      raws <- vapply(levels, score_one, numeric(1))
      w <- (seq_along(levels) - 1)
      sc <- round(sum(raws * w) / max(1e-9, sum(raws)), 3)
      list(type = "score", score = sc,
           legend = as.list(setNames(levels, as.character(w))),
           confidence = round(min(1, max(raws)), 4))
    }
  })
  list(model = body$model %||% "jev-mock", answers = answers,
       usage = list(input_tokens = tok_in, output_tokens = 0L))
}
