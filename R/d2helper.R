##### ----------- load_user_data ------------
#' Load stored PGCR data for a specified membership ID
#'
#' @param user_id Destiny membership ID
#'
#' @return
#' @export
#'
#' @examples
load_pgcr_data <- function(user_id = NULL) {
  stopifnot("user_id must not be NULL" = !is.null(user_id))

  if(fs::file_exists(glue::glue("{membershipId}-data/PGCRdata"))) {
    load(glue("{membershipId}-data/PGCRdata"), envir = .GlobalEnv)
  } else {
    print("No stored PGCR data set found.\n")
  }
}

##### ----------- save_pgcr_data -------------
#' Save current PGCR data set
#' @description Saves data to a directory prepended with current membership ID
#' @return
#' @export
#'
#' @examples
save_pgcr_data <- function(id = NULL, data_set = NULL) {
  stopifnot(!is.null(id) | !is.null(data_set))

  if(!fs::dir_exists(glue("{membershipId}-data")))
  {
    fs::dir_create(glue("{membershipId}-data"))
  }
  save(levelWeapon, file=glue("{membershipId}-data/PGCRdata"))
}

##### ----------- filter_mode ----------------
#' @title Filter to specified activity value
#' @description This convenience function filters for a specific game type
#'   (mode) in record using dplyr's filter, searching via 'grepl' for the
#'   specified activity mode value in the combined mode value string.
#' @details Due to the way that the data set is built from the raw JSON postgame
#'   carnage report files, the `modeString` is stored in the D2Rstats data as a
#'   series of activity types concatenated with a pipe character. This function
#'   applies a regular expression to determine if the specified mode value is
#'   found within that concatenated set of values.
#'
#' @param data D2Rstats weapon-level data set to operate on
#' @param mode Required: Integer representing a Destiny 2 activity type
#'
#' @return
#' @export
#'
#' @examples d2data |> filter_mode(mode = 82) # filter to Dungeon activities
filter_mode <- function(data, mode=NULL) {
  # find the specified mode at beginning, end of line, or concatenated with |s
  filter(data, grepl(pattern = glue('^{mode}$|^{mode}\\||\\|{mode}$|\\|{mode}\\|'), modeString))
}

##### ------------- raise -----------------
#' Simplify repeated calls to nested elements of PGCR data
#'
#' @param .data data frame passed into raise
#' @param unnest list element whose `basic.displayValue` will be brought up
#' @param label name of new field created by raise
#' @param displayValue set to FALSE to use raw value rather than the displayValue in the nested json
#'
#' @return
#' @export
#'
#' @examples
raise <- function(.data, unnest, label, displayValue = TRUE) {
  # unnest is list element whose basic.displayValue field we will bring up
  if (!is.data.frame(.data)) {
    abort("`.data` must be a data frame.")
  }
  if (!is.logical(displayValue)) {
    abort("`displayValue` must be a boolean; True to use the displayValue component, False to use the raw value.")
  }
  if (displayValue == TRUE) {
    # default
    unnest_wider(.data, unnest) |>
      unnest_wider(basic) |>
      unnest_longer(displayValue, keep_empty = TRUE) |>
      mutate("{{label}}" := displayValue) |>
      select(-displayValue, -value)
  } else {
    # if displayValue is false, use `value` rather than `displayValue`
    unnest_wider(.data, unnest) |>
      unnest_wider(basic) |>
      unnest_auto(value) |>
      mutate("{{label}}" := value) |>
      select(-displayValue, -value)
  }
}

