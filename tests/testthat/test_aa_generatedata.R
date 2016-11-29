describe("generateData", {
  context("generateData")
  n.year <- 2L
  n.site <- 2L
  n.run <- 2L
  n.period <- 2L
  it("generates datasets", {
    expect_is(
      dataset <- generateData(
        n.year = n.year,
        n.site = n.site,
        n.period = n.period,
        n.run = n.run
      ),
      "data.frame"
    )
    expect_identical(max(dataset$Year), n.year)
    expect_identical(max(dataset$Period), n.period)
    expect_identical(max(dataset$Site), n.site)
    expect_identical(max(dataset$Run), n.run)
    expect_named(
      dataset,
      c("Year", "Period", "Site", "Mu", "Run", "Count")
    )
    expect_is(
      dataset <- generateData(
        n.year = n.year,
        n.site = n.site,
        n.period = n.period,
        n.run = n.run,
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
      dataset <- generateData(
        n.year = n.year,
        n.site = n.site,
        n.period = n.period,
        n.run = n.run,
        as.list = TRUE
      ),
      "list"
    )
    expect_named(
      dataset[[1]],
      c("Year", "Period", "Site", "Mu", "Run", "Count")
    )
    expect_is(
      dataset <- generateData(
        n.year = n.year,
        n.site = n.site,
        n.period = n.period,
        n.run = n.run,
        details = TRUE,
        as.list = TRUE
      ),
      "list"
    )
    expect_named(
      dataset[[1]],
      c(
        "Year", "Period", "Site", "Mu", "YearEffect",
        "PeriodEffect", "SiteEffect", "Run", "Count"
      )
    )
  })
})
