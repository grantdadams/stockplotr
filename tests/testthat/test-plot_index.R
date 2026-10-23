make_index_data <- function(uncertainty = c(0.2, 0.2, 0.3, 0.3)) {
  tibble::tibble(
    label = rep(c("index_observed", "index_predicted"), each = 4),
    estimate = c(100, 120, 90, 110, 105, 115, 95, 100),
    module_name = "INDEX",
    year = rep(c(2000, 2001, 2000, 2001), 2),
    fleet = rep(c("survey_a", "survey_a", "survey_b", "survey_b"), 2),
    # Converters can attach the observation uncertainty to the predicted rows
    # too, so both carry it here
    uncertainty = rep(uncertainty, 2),
    uncertainty_label = "log_sd"
  )
}

get_linerange_data <- function(plt) {
  layer <- Filter(
    function(layer) inherits(layer$geom, "GeomLinerange"),
    plt$layers
  )
  if (length(layer) == 0) {
    return(NULL)
  }
  layer[[1]]$data
}

test_that("plot_index generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_index(
      stockplotr::example_data,
      interactive = FALSE
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_index(
      stockplotr::example_data,
      interactive = FALSE
    ),
    "gg"
  )
})

test_that("plot_index draws 95% intervals on observed values only", {
  plt <- plot_index(make_index_data(), interactive = FALSE)
  interval_data <- get_linerange_data(plt) |>
    dplyr::arrange(fleet, year)

  expect_equal(unique(interval_data$label), "index_observed")
  expect_equal(nrow(interval_data), 4)
  # Lognormal 95% interval around each observation
  expect_equal(
    interval_data$estimate_lower,
    stats::qlnorm(0.025, meanlog = log(c(100, 120, 90, 110)), sdlog = c(0.2, 0.2, 0.3, 0.3)),
    tolerance = 1e-4
  )
  expect_equal(
    interval_data$estimate_upper,
    stats::qlnorm(0.975, meanlog = log(c(100, 120, 90, 110)), sdlog = c(0.2, 0.2, 0.3, 0.3)),
    tolerance = 1e-4
  )
  expect_no_error(ggplot2::ggplot_build(plt))
})

test_that("plot_index draws no intervals without uncertainty", {
  plt <- plot_index(
    make_index_data(uncertainty = rep(NA_real_, 4)),
    interactive = FALSE
  )
  expect_null(get_linerange_data(plt))
  expect_no_error(ggplot2::ggplot_build(plt))
})

test_that("plot_index plots a single Rceattle index", {
  fit <- readRDS(test_path("fixtures", "rceattle_goa_atf.rds"))
  out <- convert_output(file = fit, model = "rceattle")

  plt <- plot_index(out, interactive = FALSE)
  # Only the observed and predicted index are plotted, not other index
  # quantities such as log_index_hat from the sdreport
  plotted_labels <- unique(unlist(lapply(plt$layers, function(layer) layer$data$label)))
  expect_setequal(plotted_labels, c("index_observed", "index_predicted"))
  # One index fleet, so there is nothing to facet by
  expect_no_error(ggplot2::ggplot_build(plt))
})

test_that("plot_index keeps other facets when there is one index fleet", {
  one_fleet <- make_index_data() |>
    dplyr::filter(fleet == "survey_a") |>
    dplyr::mutate(area = rep(c("north", "south"), 2))

  plt <- plot_index(one_fleet, facet = "area", interactive = FALSE)
  expect_equal(names(plt$facet$params$facets), "area")
  expect_no_error(ggplot2::ggplot_build(plt))
})

# test_that("rda file made when indicated", {
#   # export rda
#   plot_index(
#     stockplotr::example_data,
#     make_rda = TRUE,
#     unit_label = "metric tons",
#     interactive = FALSE,
#     figures_dir = getwd()
#   )

#   # expect that both figures dir and the index_figure.rda file exist
#   check_and_remove_tmp_files("index_figure.rda")
# })
