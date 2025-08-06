#' module_gcamindia_LA142.buildings
#'
#' Provides residential and commercial building energy consumption by region/fuel/historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.india_state_in_EJ_comm_F}, \code{L142.india_state_in_EJ_resid_F}. The corresponding file in the
#' original data system was \code{LA142.Buildings.R} (gcam-india level1).
#' @details Provides for each US state industrial energy consumption and industrial feedstock consumption by region/fuel/historical year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author PD August 2021 updated for GCAM6/7 Chetna Arora 2023


module_gcamindia_LA142.buildings <- function(command, ...) {

  MODULE_INPUTS <-
    c("L101.india_state_EB_EJ_state_S_F",
      "L142.in_EJ_R_bld_F_Yh")

  MODULE_OUTPUTS <-
    c("L142.india_state_in_EJ_comm_F",
      "L142.india_state_in_EJ_resid_F")


  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence package checks
    fuel <- year <- state <- value <- sector <- GCAM_region_ID <- multiplier <- NULL

    # --------------------------------------------------	---------------------------
    # 1. Read data

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # ===================================================

    #gcam region code
    gcam.india_REGION <- "India"
    gcam.INDIA_CODE   <- 17

    # 1. Reshape wide-to-long to create a 'year' column
    L101.india_state_EB_EJ_state_S_F_long <-  L101.india_state_EB_EJ_state_S_F %>%
      pivot_longer(
        cols = where(is.numeric),  # Pivots all numeric columns
        names_to = "year",
        values_to = "value"
      )

    #India Re-Allocation of Energy in EJ: An Update
    L142.in_EJ_R_bld_F_Yh_no2021India <- L142.in_EJ_R_bld_F_Yh[(!L142.in_EJ_R_bld_F_Yh$GCAM_region_ID == '17') | (!L142.in_EJ_R_bld_F_Yh$year == '2021'), ]

    L101.india_state_EB_EJ_state_C_2021 <- L101.india_state_EB_EJ_state_S_F_long %>%
      filter(sector == "comm", year =="2021") %>% select(-sector) %>%
      mutate(sector = 'bld_comm')

    L101.india_state_EB_EJ_state_U <- L101.india_state_EB_EJ_state_S_F_long %>%
      filter(sector == "resid urban")
    L101.india_state_EB_EJ_state_R <- L101.india_state_EB_EJ_state_S_F_long %>%
      filter(sector == "resid rural")

    L101.india_state_EB_EJ_state_U_R <- bind_rows(L101.india_state_EB_EJ_state_U, L101.india_state_EB_EJ_state_R) %>%
      group_by(state, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ()

    L101.india_state_EB_EJ_state_U_R_2021 <- L101.india_state_EB_EJ_state_U_R %>%
      filter (year =="2021") %>% mutate(sector ='bld_resid')

    L101.india_state_EB_EJ_state_bld_2021 <- bind_rows(L101.india_state_EB_EJ_state_U_R_2021, L101.india_state_EB_EJ_state_C_2021)

   # L142.in_EJ_R_bld_F_Yh_India <- L142.in_EJ_R_bld_F_Yh %>%
    #  filter (GCAM_region_ID == gcam.INDIA_CODE, year =='2015') %>%
    #  left_join_error_no_match(L101.india_state_EB_EJ_state_bld_2015, by = c("sector","fuel","year")) %>%
    #  select (GCAM_region_ID, sector, fuel, year, value = value.y)

    #L142.in_EJ_R_bld_F_Yh <- bind_rows(L142.in_EJ_R_bld_F_Yh_India, L142.in_EJ_R_bld_F_Yh_no2015India)

   #Calculating Urban and Rural shares for all years 1971-2021
    L101.india_state_EB_EJ_state_Urban_shares <- L101.india_state_EB_EJ_state_U_R %>%
      left_join_error_no_match(L101.india_state_EB_EJ_state_U, by = c("state","fuel","year")) %>%
      mutate(share = value.y/value.x) %>%
      select (state, sector, fuel, year, share)

    L101.india_state_EB_EJ_state_Urban_shares[is.na(L101.india_state_EB_EJ_state_Urban_shares)] <- 0

    L101.india_state_EB_EJ_state_Rural_shares <- L101.india_state_EB_EJ_state_U_R %>%
      left_join_error_no_match(L101.india_state_EB_EJ_state_R, by = c("state","fuel","year")) %>%
      mutate(share = value.y/value.x) %>%
      select (state, sector, fuel, year, share)

    L101.india_state_EB_EJ_state_Rural_shares[is.na(L101.india_state_EB_EJ_state_Rural_shares)] <- 0

    L101.india_state_EB_EJ_state_Urban <- L142.in_EJ_R_bld_F_Yh %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE, sector == "bld_resid") %>%
      select(-GCAM_region_ID) %>%
      #Convert year to numeric in the second dataframe before joining
    left_join_error_no_match(L101.india_state_EB_EJ_state_Urban_shares %>% mutate(year = as.numeric(year)),
      by = c("fuel", "year")) %>%
      mutate(value = value * share) %>%
      mutate(GCAM_region_ID = gcam.INDIA_CODE) %>%
      mutate(sector = sector.y) %>%
      select(GCAM_region_ID, sector, fuel, year, value)

    L101.india_state_EB_EJ_state_Rural <- L142.in_EJ_R_bld_F_Yh %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE, sector == "bld_resid") %>%
      select(-GCAM_region_ID) %>%
      left_join_error_no_match(L101.india_state_EB_EJ_state_Rural_shares %>% mutate(year = as.numeric(year)),
                               by = c("fuel", "year")) %>%
      mutate(value = value * share) %>%
      mutate(GCAM_region_ID = gcam.INDIA_CODE) %>%
      mutate(sector = sector.y) %>%
      select(GCAM_region_ID, sector, fuel, year, value)

    # binding both rural and urban energy files
    L142.india_state_in_EJ_resid_F <- bind_rows(L101.india_state_EB_EJ_state_Rural, L101.india_state_EB_EJ_state_Urban)


    L142.india_state_in_EJ_comm_F <- L142.in_EJ_R_bld_F_Yh %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE, sector =='bld_comm') %>%
      distinct()


    ## OUTPUTS
    L142.india_state_in_EJ_comm_F %>%
      add_title("Comm sector input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level consumption among states") %>%
      add_legacy_name("L142.india_state_in_EJ_comm_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L142.in_EJ_R_bld_F_Yh") ->
      L142.india_state_in_EJ_comm_F

    L142.india_state_in_EJ_resid_F %>%
      add_title("Resid sector input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level consumption among states") %>%
      add_legacy_name("L142.india_state_in_EJ_resid_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L142.in_EJ_R_bld_F_Yh") ->
      L142.india_state_in_EJ_resid_F


    return_data(L142.india_state_in_EJ_comm_F, L142.india_state_in_EJ_resid_F)
  } else {
    stop("Unknown command")
  }
}
