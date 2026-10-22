#' Plot Index of Abundance
#'
#' @inheritParams plot_spawning_biomass
#' @param facet Character vector. Column name or names used for faceting. "fleet" is always added on to any faceting selections
#'
#' Default: NULL
#'
#' Options: May include, but is not limited to: "fleet", "year", "area", NULL
#' @param unit_label String. Index of abundance/CPUE units
#'
#' Default: ""
#' @param focus Character vector. Column name or names used to filter data.
#' (i.e. select names of fleets to zoom into the plot)
#'
#' Default: NULL
#' @param log_scale Logical. TRUE/FALSE; plot the y axis on a log scale. The
#' axis labels stay in the units of the index.
#'
#' Default: `FALSE`
#' @returns A plot showing the observed and predicted index.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' Observed values are drawn as points with intervals and predicted values as
#' a line. The interval comes from the uncertainty of the observation and is
#' calculated by \link[stockplotr]{filter_data}. Its scale follows
#' `uncertainty_label`. A label that starts with "log_" (e.g., "log_sd") is a
#' standard deviation on the log scale, as used by a lognormal likelihood, so
#' the 95% interval is `estimate * exp(c(-1.96, 1.96) * uncertainty)`. Other
#' labels use the natural-scale rules in \link[stockplotr]{filter_data}.
#' Observations without uncertainty have no interval.
#'
#' For a lognormal likelihood, the interval brackets the median of the fitted
#' observation distribution. The line is that median only when the model
#' reports the predicted index without a bias adjustment.
#'
#' @seealso [convert_output()], [filter_data()], [process_data()], [plot_obsvpred()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_index(
#'   dat = stockplotr::example_data,
#'   unit_label = "fish/hr",
#'   interactive = FALSE
#' )
#'
plot_index <- function(
  dat,
  unit_label = "",
  group = NULL,
  # facet always assigned to fleet since that is how indices are calc'd -- unless replaced with NULL
  facet = NULL,
  interactive = TRUE,
  module = NULL,
  focus = NULL,
  log_scale = FALSE,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  # Set cpue unit label for plot
  u_units <- ifelse(
    is.null(unit_label) | unit_label == "",
    glue::glue("Estimated Index"),
    glue::glue("Estimated Index ({unit_label})")
  )

  if (is.null(facet) || "fleet" %notin% facet) {
    facet <- c(facet, "fleet")
  }
  # Filter data
  prepared_data <- filter_data(
    dat,
    # Only the observed and predicted index. A looser match also picks up
    # other index quantities, such as log_index_hat from an Rceattle sdreport.
    label_name = "index_observed|index_predicted",
    era = NULL,
    geom = "line",
    # ifelse guarantees the code doesn't miss grouping when label has > 1 value
    # ifelse(length(unique(prepared_data$label)) > 1, "label", group)
    # above does not work if it happens during this step lol
    group = group,
    facet = facet,
    interactive = interactive,
    module = module
  )

  # Subset data if focus
  if (!is.null(focus)) {
    prepared_data <- prepared_data |>
      dplyr::filter(fleet %in% focus)
  }

  processed_data <- process_data(
    dat = prepared_data,
    group = group,
    facet = facet
  )
  prepared_data <- processed_data[[1]]
  group <- processed_data[[2]]
  facet <- processed_data[[3]]

  # move label to grouping and set grouping into facet
  if (!is.null(group)) {
    facet <- c(facet, group)
  }
  # process_data() drops a variable with a single value, e.g., the fleet of a
  # model with one index, so it can no longer be used to facet
  facet <- facet[facet %in% colnames(prepared_data)]
  # transform prep data so group_var = label
  # prepared_data <- prepared_data |>
  #   dplyr::mutate(
  #     group_var = label
  #   )
  # plot time series with multiple labels
  plt <- plot_obsvpred(
    dat = prepared_data,
    x = "year",
    y = "estimate",
    observed_label = "index_observed",
    predicted_label = "index_predicted",
    geom = "line",
    xlab = "Year",
    ylab = "Estimated Index",
    group = group,
    facet = facet
  ) +
    theme_noaa() +
    ggplot2::scale_x_continuous(
      breaks = ggplot2::waiver(),
      # labels = scales::label_number(accuracy = 1),
      guide = ggplot2::guide_axis(
        minor.ticks = TRUE
      )
    )
  # Overwrite facets from base plot_timeseries bc scales need to be free
  if ("fleet" %in% colnames(prepared_data)) {
    facet <- paste("~", paste(facet, collapse = " + "))
    facet_formula <- stats::reformulate(facet)
    plt <- plt + ggplot2::facet_wrap(facet_formula, scales = "free", labeller = function(labels) {
      # Clean column names (e.g., "growth_pattern" -> "Growth Pattern")
      names(labels) <- tools::toTitleCase(gsub("_", " ", names(labels)))
      # Combine into "Variable: Value" format
      lapply(names(labels), function(var) paste0(var, ": ", labels[[var]]))
    })
  }

  # Add intervals to the observed values. The uncertainty describes the
  # observation error, so predicted values get no interval.
  observed_data <- prepared_data |>
    dplyr::filter(
      grepl("index_observed", label),
      !is.na(estimate_lower)
    )
  if (nrow(observed_data) > 0) {
    plt <- plt +
      ggplot2::geom_linerange(
        data = observed_data,
        ggplot2::aes(
          x = year,
          ymin = estimate_lower,
          ymax = estimate_upper,
          color = group_var
        ),
        show.legend = FALSE
      )
  }

  if (log_scale) {
    # Remove the blank layer that extends the y axis to 0, which cannot be
    # shown on a log scale
    plt$layers <- Filter(
      function(layer) !inherits(layer$geom, "GeomBlank"),
      plt$layers
    )
    plt <- plt +
      ggplot2::scale_y_log10(
        labels = scales::label_comma()
      )
  }

  ### Make RDA ----
  if (make_rda) {
    # Obtain relevant key quantities for captions/alt text
    cpue.start.year <- min(prepared_data$year)
    cpue.end.year <- max(prepared_data$year)
    cpue.min <- min(prepared_data$estimate) |> round(digits = 3)
    cpue.max <- max(prepared_data$estimate) |> round(digits = 3)
    cpue.units <- ifelse(unit_label == "",
      "an estimated index ratio", # default if empty
      unit_label
    )

    # calculate & export key quantities
    export_kqs(
      cpue.start.year,
      cpue.end.year,
      cpue.min,
      cpue.max,
      cpue.units
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      cpue.start.year,
      cpue.end.year,
      cpue.min,
      cpue.max,
      cpue.units
    )

    create_rda(
      object = plt,
      # get name of function and remove "plot_" from it
      topic_label = gsub("plot_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir
    )
  }
  # Output final plot
  plt
}
