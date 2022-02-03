context("generate data")
describe("generate_data", {
  context("generate_data")
  n_year <- 2L
  n_site <- 2L
  n_run <- 2L
  n_period <- 2L
  it("generates datasets", {
    expect_is(
      dataset <- generate_data(
        n_year = n_year,
        n_site = n_site,
        n_period = n_period,
        n_run = n_run
      ),
      "data.frame"
    )
    expect_identical(max(dataset$Year), n_year)
    expect_identical(max(dataset$Period), n_period)
    expect_identical(max(dataset$Site), n_site)
    expect_identical(max(dataset$Run), n_run)
    expect_named(
      dataset,
      c("Year", "Period", "Site", "Mu", "Run", "Count")
    )
    expect_is(
      dataset <- generate_data(
        n_year = n_year,
        n_site = n_site,
        n_period = n_period,
        n_run = n_run,
        details = TRUE
      ),
      "data.frame"
    )
    expect_named(
      dataset,
      c(
        "Year", "Period", "Site", "Mu", "YearEffect", "PeriodEffect",
        "SiteEffect", "Run", "Count"
      )
    )
  })
  it("gerenates lists of datasets", {
    expect_is(
      dataset <- generate_data(
        n_year = n_year,
        n_site = n_site,
        n_period = n_period,
        n_run = n_run,
        as_list = TRUE
      ),
      "list"
    )
    expect_named(
      dataset[[1]],
      c("Year", "Period", "Site", "Mu", "Count", "Run")
    )
    expect_is(
      dataset <- generate_data(
        n_year = n_year,
        n_site = n_site,
        n_period = n_period,
        n_run = n_run,
        details = TRUE,
        as_list = TRUE
      ),
      "list"
    )
    expect_named(
      dataset[[1]],
      c(
        "Year", "Period", "Site", "Mu", "YearEffect",
        "PeriodEffect", "SiteEffect", "Count", "Run"
      )
    )
  })
})
