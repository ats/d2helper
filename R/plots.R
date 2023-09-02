
#' Build treemap of weapon usage
#'
#' @param data data set to use for plot
#' @param type weapon type to filter
#' @param tier_weapon weapon tier to filter (e.g. "legendary")
#' @param mode game mode to filter for
#' @param limit maximum number of weapons to include in plot
#' @param title plot title
#' @param subtitle plot subtitle
#'
#' @return
#' @export
#'
#' @examples
weapon_treemap <- function(data=NULL, type=NULL,
                           tier_weapon = NULL, mode = NULL,
                           name_contains = NULL,
                           limit=200, title=NULL,
                           subtitle=NULL
)
{
  if (!is.null(mode)) {
    #print("filtering on mode\n")
    data |>
      # rowwise() |> filter(modeIn(mode, modeString)) -> data
      filter_mode(mode=mode) -> data
    #  print(data |> select(modeString) |> unique())
  }

  if (!is.null(name_contains)) {
    data |>
      filter(grepl(name_contains, displayProperties.name.x, ignore.case=TRUE)) -> data
  }

  if (!is.null(tier_weapon)) {
    data |> filter(tier==tier_weapon) -> data
    #subtitle <- tier_weapon
  }

  if (!is.null(type)) {
    data |> rowwise() |>
      filter(itemType==type) -> data
    # panel_subtitle <- paste0("type: ", type)
  }

  data |>
    group_by(itemName, itemType) |> tally(as.numeric(uniqueWeaponKills), name="total") |>
    ungroup() |>
    filter(total >= limit) |>
    ggplot(aes(area=total, fill=total, label=as.factor(itemName),
               subgroup=as.factor(itemType))) +
    geom_treemap(start="topleft") +
    geom_treemap_subgroup_border(start="topleft") +
    geom_treemap_subgroup_text(start="topleft", place = "centre", grow = T, alpha = 0.5, colour =
                                 "yellow", fontface = "italic", min.size = 0) +
    geom_treemap_text(start="topleft", colour = "gray", place = "topleft", reflow = T) +
    ggtitle(title, subtitle) + theme(legend.position = "none")
}

#' Build barplot of weapon usage
#'
#' @param data data set to use for plot
#' @param type weapon type to filter
#' @param tier_weapon weapon tier to filter (e.g. "legendary")
#' @param mode game mode to filter for
#' @param limit maximum number of weapons to include in plot
#' @param ncol number of columns to build in a panel of barplots
#' @param text_size font size for display
#' @param title plot title
#' @param subtitle plot subtitle
#'
#' @return
#' @export
#'
#' @examples
weapon_barplot <- function(data=NULL, type=NULL,
                           tier_weapon = NULL, mode = NULL,
                           name_contains = NULL,
                           limit=5,
                           ncol=2,
                           text_size=2,
                           title=NULL,
                           subtitle=NULL
)
{
  if (!is.null(mode)) {
    #print("filtering on mode\n")
    data |>
      # rowwise() |> filter(modeIn(mode, modeString)) -> data
      filter_mode(mode) -> data
    #print(data |> select(modeString) |> unique())
  }

  if (!is.null(name_contains)) {
    data |>
      filter(grepl(name_contains, displayProperties.name.x, ignore.case=TRUE)) -> data
  }

  if (!is.null(tier_weapon)) {
    data |> filter(tier==tier_weapon) -> data
    #subtitle <- tier_weapon
  }

  if (!is.null(type)) {
    data |> rowwise() |>
      filter(itemType==type) -> data
    # panel_subtitle <- paste0("type: ", type)
  }

  # make sorted levels
  itemtype_levels <- data |> group_by(itemType) |> summarise(total = sum(as.numeric(uniqueWeaponKills))) |> arrange(desc(total)) |> select(itemType)
  # print(itemtype_levels$itemType)

  data |> group_by(itemType, itemName) |> summarise(total = sum(as.numeric(uniqueWeaponKills))) |>

    arrange(desc(total)) |>group_by(itemType) |> slice_head(n=limit) |>
    ggplot(aes(x=total, y=forcats::fct_reorder(itemName, total))) +
    # theme_dark() +
    geom_col() + facet_wrap(.~factor(itemType, levels=itemtype_levels$itemType),
                            scales="free_y", ncol = ncol) +
    # theme(strip.text = element_blank()) +
    theme(strip.text.x = element_text(
      margin = margin( b = 0, t = 0)
    )) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(size=5)) +
    theme(axis.text.y = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    # theme(axis.text.y = element_text(size = 6)) +
    # scale_y_discrete(position="left",
    #                  label = function(x) stringr::str_trunc(x, 15)) +
    geom_text(aes(x=max(total), label=forcats::fct_reorder(itemName, total)), hjust=1, size=text_size) +
    ggtitle(title, subtitle)

}
