#' Flag low counts and set them to NAs
#'
#' Low counts may be genuine, but they can also reflect actually missing data or
#' strong under-reporting. This function aims to detect the latter by flagging
#' any count below a certain threshold, expressed as a fraction of the median
#' count.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @md
#'
#' @param x An [incidence2::incidence] object.
#' 
#' @param counts A `character` indicating the counts for which low values should
#'   be flagged.
#'
#' @param threshold
#'
#' @param set_missing A `logical` indicating if the low counts identified should
#'   be replaced with NAs (`TRUE`, default). If `FALSE`, a new logical column called
#'   `low_count` will be added, which will indicate which entries are flagg
#' 
#' @return An [incidence2::incidence] object.
#'
#' @export
#' 
flag_lowcounts <- function(x, counts, threshold = 0.01) {
  if (!inherits(x, "incidence2")) {
    stop(sprintf("`%s` is not an incidence object", deparse(substitute(x))))
  }
  x_attr <- attributes(x)

  if (missing(counts)) {
    counts <- incidence2::get_count_names(x)[1]
  }

  count_names <- incidence2::get_count_names(x)
  group_names <- incidence2::get_group_names(x)

  if (!counts %in% count_names) {
    msg <- sprintf("`%s` is not a count variable of `x` (acceptable: %s)",
                   counts,
                   paste(count_names, collapse = ", "))
    stop(msg)
  }

  out <- dplyr::group_by(x, dplyr::.data[[group_names]])
  out <- dplyr::mutate(out,
                       low_counts_ = .data[[counts]] < threshold * median(.data[[counts]], na.rm = TRUE))

  ## restore attributes and return
  onames <- names(out)
  attributes(out) <- x_attr
  names(out) <- onames
  out
  
}
