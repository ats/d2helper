
##### ------------- animate_history _______________
#' @title Build race-style bar plots of Destiny 2 weapon usage over time
#' @description Creates an animated "race" graph showing the growth and change
#' of total weapon usage through the period of time in the provided weapon-level
#' activity data.
#'
#' @param x The D2Rstats weapon-level data set to process
#' @param activity_type One activity type integer to filter
#' @param limit The total number of top weapons to plot
#' @param label The title to put on the plot
#' @param prefix Prefix to output filename; only used if gif or av are TRUE
#' @param gif Boolean: produce .gif file output?
#' @param av Boolean: produce .mov file output?
#'
#' @return If `gif` and `mp4` are `FALSE`, returns animated plot object that can be further manipulated or rendered using `animate`
#' @export
#'
#' @examples levelWeapon |> animate_history(activity_type=82,
#'   label="Dungeon weapons over time",
#'   limit=40, mp4=TRUE)
animate_history <- function(x, activity_type=NULL, limit=30, label="",
                            prefix="d2rstats", gif=FALSE, mp4=FALSE) {

  # if an activity_type is set, filter all records to that type
  # otherwise we'll use all history for all activities, which
  # may be time-consuming
  if (!is.null(activity_type)) {
    x <- x |> filter_mode(mode = activity_type)
  }

  #filter(modeIn(activity_type, modeString)) |>
  gamedata <- x |>
    mutate(date = lubridate::as_date(period)) |>
    group_by(date, itemName, bucketName) |>
    arrange(date, itemName) |>
    summarize(groupedWeaponKills = sum(as.numeric(uniqueWeaponKills))) |>
    group_by(itemName) |>
    mutate(cumWeaponKills = cumsum(groupedWeaponKills))
  paint::paint(gamedata)
  # gamedata

  outputPath <- glue("{membershipId}-output")

  dataWide <- gamedata |>
    filter(itemName != "") |>
    tidyr::pivot_wider(id_cols = date,
                       names_from = itemName,
                       values_from = cumWeaponKills)
  # paint::paint(dataWide)

  # dataExploded <- gamedata |>
  dataExploded <- dataWide |>
    tidyr::pivot_longer(2:ncol(dataWide), values_to = "cumWeaponKills") |>
    # tidyr::pivot_longer(2:ncol(gamedata), values_to = "cumWeaponKills") |>
    group_by(name) |> tidyr::fill(cumWeaponKills) |>
    mutate(cumWeaponKills = tidyr::replace_na(cumWeaponKills, 0))
  paint::paint(dataExploded)

  animatedPlot <- dataExploded |> ungroup() |>
    filter(cumWeaponKills > 0) |>
    group_by(date) |>
    slice_max(cumWeaponKills, n=limit) |> #ungroup() |>
    arrange(date, cumWeaponKills) |> mutate(order = 1:n()) |> ungroup() |>

    ggplot(aes(order, group=name)) +
    geom_tile(
      aes(y=cumWeaponKills/2, height=cumWeaponKills, width=.8),
      color="pink", linewidth=.75
    ) +
    geom_text(aes(x=order, y=cumWeaponKills+5, label=name, hjust="inward"),
              color="white",
              size=4, check_overlap = TRUE) +
    coord_flip() +
    labs(title=label,
         subtitle='{closest_state}') +
    ylab("Total eliminations") +
    xlab("") +
    theme_solarized(light=FALSE) +
    theme(plot.subtitle = element_text(size = 20),
          plot.title = element_text(size=22)) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(line = element_blank()) +
    theme(axis.text.y = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    transition_states(date, transition_length = 20, state_length = 1, wrap=FALSE) +
    view_follow(fixed_y=TRUE) +
    # enter_fly(x_loc=0, y_loc=0) + exit_fly(x_loc=0, y_loc = 0) +
    enter_appear(early=TRUE)+
    # enter_fade() +
    enter_recolor(color = "yellow", fill="yellow") + enter_drift(x_mod = -5)
  # enter_reset() + exit_reset()

  prefix <- fs::path_sanitize(prefix)
  if (gif) {
    animate(animatedPlot, nframes = 200, end_pause = 10,
            renderer = gifski_renderer(glue("{outputPath}/{prefix}_weapons.gif")))
  }
  if (mp4) {
    animate(animatedPlot, height=800, width=800, nframes=600, fps=20,
            renderer = av_renderer(glue("{outputPath}/{prefix}_weapons.mp4")))
  }

  # output animatedPlot if  output isn't specified
  if (gif==FALSE & mp4==FALSE) {
    return(animatedPlot)
  }
}
