minimal_incidence <- function(x, groups, date, count, interval,
                              cumulative = FALSE, date_group = NULL) {
  tibble::new_tibble(x,
                     groups = groups,
                     date = date,
                     count = count,
                     interval = interval,
                     cumulative = cumulative,
                     date_group = NULL,
                     nrow = nrow(x),
                     class = "incidence2")
}
