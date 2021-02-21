library(magrittr)

Rblpapi::blpConnect()

dat <- Rblpapi::bdh("SPX Index", 
                    fields = c("TOT_RETURN_INDEX_GROSS_DVDS", "BEST_PE_RATIO"), 
                    start.date = as.Date("1990-01-01"), 
                    options  = c("periodicitySelection" = "QUARTERLY")) %>%
  dplyr::mutate(
    return = dplyr::lead(TOT_RETURN_INDEX_GROSS_DVDS, n = 20) %>%
      magrittr::divide_by(TOT_RETURN_INDEX_GROSS_DVDS) %>%
      magrittr::raise_to_power(1/5) %>%
      magrittr::subtract(1) %>%
      magrittr::multiply_by(100)) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    Decade = date %>% format("%Y") %>% stringr::str_sub(1,3) %>% paste0("0s")
  ) 

head(dat)

fx <- lm(dat$return ~ dat$BEST_PE_RATIO)
current_pe <- Rblpapi::bdp("SPX Index", "BEST_PE_RATIO") %>% dplyr::pull()
expected_return <- fx$coefficients[[1]] + (fx$coefficients[[2]]*current_pe)

p <- ggplot2::ggplot(dat, ggplot2::aes(BEST_PE_RATIO, return)) +
  ggplot2::geom_point(ggplot2::aes(color = Decade)) +
  ggplot2::scale_color_manual(values = pamngr::pam.pal())+
  ggplot2::geom_smooth(method = "lm", color = "blue") +
  ggplot2::geom_point(ggplot2::aes(current_pe, expected_return), 
                      size = 5,
                      color = "blue") +
  ggplot2::geom_point(ggplot2::aes(current_pe, expected_return), 
                      size = 4,
                      color = "black") 

p <- p %>%
  pamngr::pam_plot(
    plot_title = "S&P 500 Return Expectations",
    axis_titles = TRUE,
    x_lab = "Price to Next Year's Earnings",
    y_lab = "Subsequent 5 Year Annualized Return"
  )

p <- p + ggplot2::ylim(c(-40,40))

p %>% pamngr::all_output("valuationAndReturn")