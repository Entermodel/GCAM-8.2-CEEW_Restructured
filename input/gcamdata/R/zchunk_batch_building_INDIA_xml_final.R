#' module_gcamindia_batch_building_India_xml
#'
#' Construct XML data structure for \code{building_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_india.xml}. The corresponding file in the
#' original data system was \code{batch_building_india.xml} (gcamusa XML).
module_gcamindia_batch_building_India_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L244.india_state_DeleteConsumer_bld", #done
      "L244.india_state_DeleteSupplysector_bld", #done
      "L244.SubregionalShares_gcamindia", #done
      "L244.PriceExp_IntGains_gcamindia", #done
      "L244.india_state_Floorspace_gcamindia", #done
      "L244.DemandFunction_serv_gcamindia", #done
      "L244.DemandFunction_flsp_gcamindia", #done
      "L244.india_state_Satiation_flsp", #done
      "L244.Satiation_impedance_gcamindia",
      "L244.india_state_SatiationAdder", #done
      "L244.GompFnParam_gcamindia",
      "L244.india_state_HDDCDD_HadCM3",
      "L244.india_state_ThermalBaseService", #done
      "L244.india_state_GenericBaseService", #done
      "L244.india_state_ThermalServiceSatiation", #done
      "L244.india_state_GenericServiceSatiation", #done
      "L244.GenericServiceImpedance_gcamindia",
      "L244.ThermalServiceImpedance_gcamindia",
      "L244.GenericServiceAdder_gcamindia",
      "L244.ThermalServiceAdder_gcamindia",
      "L244.GenericServiceCoef_gcamindia",
      "L244.ThermalServiceCoef_gcamindia",
      "L244.india_state_Intgains_scalar", #done
      "L244.india_state_ShellConductance_bld", #done
      "L244.india_state_Supplysector_bld", #done
      "L244.india_state_FinalEnergyKeyword_bld", #done
      "L244.india_state_SubsectorShrwtFllt_bld", #done
      "L244.india_state_SubsectorInterp_bld", #done
      "L244.india_state_SubsectorInterpTo_bld", #done
      "L244.india_state_SubsectorLogit_bld", #done
      "L244.india_state_StubTech_bld", #done
      "L244.india_state_StubTechCalInput_bld", #done
      "L244.india_state_StubTechMarket_bld", #done
      "L244.india_state_GlobalTechIntGainOutputRatio", #done
      "L244.india_state_GlobalTechInterpTo_bld", #done
      "L244.india_state_GlobalTechEff_bld", #done
      "L244.india_state_GlobalTechShrwt_bld", #done
      "L244.india_state_GlobalTechCost_bld", #done
      "L244.india_state_GlobalTechSCurve_bld",
      "L244.GenericServicePrice_gcamindia",
      "L244.ThermalServicePrice_gcamindia",
      "L244.GenericBaseDens_gcamindia",
      "L244.ThermalBaseDens_gcamindia")

  MODULE_OUTPUTS <-
    c(XML = "building_india.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # --------------------------------------------------	---------------------------
    # 1. Read data

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)
   # ===================================================

    # Produce outputs


    create_xml("building_india.xml") %>%
      add_xml_data(L244.india_state_DeleteConsumer_bld, "DeleteConsumer") %>%
      add_xml_data(L244.india_state_DeleteSupplysector_bld, "DeleteSupplysector") %>%
      add_xml_data(L244.SubregionalShares_gcamindia, "SubregionalShares") %>%
      add_xml_data(L244.PriceExp_IntGains_gcamindia, "PriceExp_IntGains") %>%
      add_xml_data(L244.india_state_Floorspace_gcamindia, "Floorspace") %>%
      add_xml_data(L244.DemandFunction_serv_gcamindia, "DemandFunction_serv") %>%
      add_xml_data(L244.DemandFunction_flsp_gcamindia, "DemandFunction_flsp") %>%
      add_xml_data(L244.india_state_Satiation_flsp, "Satiation_flsp") %>%
      add_xml_data(L244.Satiation_impedance_gcamindia, "SatiationImpedance") %>%
      add_xml_data(L244.india_state_SatiationAdder, "SatiationAdder") %>%
      add_xml_data(L244.GompFnParam_gcamindia, "GompFnParam")  %>%
     add_xml_data(L244.india_state_ThermalBaseService, "ThermalBaseService") %>%
      add_xml_data(L244.india_state_GenericBaseService, "GenericBaseService") %>%
      add_xml_data(L244.india_state_ThermalServiceSatiation, "ThermalServiceSatiation") %>%
      add_xml_data(L244.india_state_GenericServiceSatiation, "GenericServiceSatiation") %>%
      add_xml_data(L244.GenericServiceImpedance_gcamindia, "GenericServiceImpedance") %>%
      add_xml_data(L244.ThermalServiceImpedance_gcamindia, "ThermalServiceImpedance") %>%
      add_xml_data(L244.GenericServiceAdder_gcamindia, "GenericServiceAdder") %>%
      add_xml_data(L244.ThermalServiceAdder_gcamindia, "ThermalServiceAdder") %>%
      add_xml_data(L244.GenericServiceCoef_gcamindia, "GenericServiceCoef") %>%
      add_xml_data(L244.ThermalServiceCoef_gcamindia, "ThermalServiceCoef") %>%
      add_xml_data(L244.india_state_Intgains_scalar, "Intgains_scalar") %>%
      add_xml_data(L244.india_state_ShellConductance_bld, "ShellConductance") %>%
      add_logit_tables_xml(L244.india_state_Supplysector_bld, "Supplysector") %>%
      add_xml_data(L244.india_state_FinalEnergyKeyword_bld, "FinalEnergyKeyword") %>%
      add_xml_data(L244.india_state_SubsectorShrwtFllt_bld, "SubsectorShrwtFllt") %>%
      add_xml_data(L244.india_state_SubsectorInterp_bld, "SubsectorInterp") %>%
      add_logit_tables_xml(L244.india_state_SubsectorLogit_bld, "SubsectorLogit") %>%
      add_xml_data(L244.india_state_StubTech_bld, "StubTech") %>%
      add_xml_data(L244.india_state_StubTechCalInput_bld, "StubTechCalInput") %>%
      add_xml_data(L244.india_state_StubTechMarket_bld, "StubTechMarket") %>%
      add_xml_data(L244.india_state_GlobalTechIntGainOutputRatio, "GlobalTechIntGainOutputRatio") %>%
      add_xml_data(L244.india_state_GlobalTechInterpTo_bld, "GlobalTechInterpTo") %>%
      add_xml_data(L244.india_state_GlobalTechEff_bld, "GlobalTechEff") %>%
      add_xml_data(L244.india_state_GlobalTechShrwt_bld, "GlobalTechShrwt") %>%
      add_xml_data(L244.india_state_GlobalTechCost_bld, "GlobalTechCost") %>%
      add_xml_data(L244.india_state_HDDCDD_HadCM3, "HDDCDD") %>%
      add_xml_data(L244.india_state_GlobalTechSCurve_bld, "GlobalTechSCurve")  %>%
      add_xml_data(L244.GenericServicePrice_gcamindia, "GenericServicePrice") %>%
      add_xml_data(L244.ThermalServicePrice_gcamindia, "ThermalServicePrice") %>%
      add_xml_data(L244.GenericBaseDens_gcamindia, "GenericBaseDens") %>%
      add_xml_data(L244.ThermalBaseDens_gcamindia, "ThermalBaseDens") %>%
      add_precursors("L244.india_state_DeleteConsumer_bld",
                     "L244.india_state_DeleteSupplysector_bld",
                     "L244.SubregionalShares_gcamindia",
                     "L244.PriceExp_IntGains_gcamindia",
                     "L244.india_state_Floorspace_gcamindia",
                     "L244.DemandFunction_serv_gcamindia",
                     "L244.DemandFunction_flsp_gcamindia",
                     "L244.india_state_Satiation_flsp",
                     "L244.india_state_SatiationAdder",
                     "L244.india_state_ThermalBaseService",
                     "L244.india_state_GenericBaseService",
                     "L244.india_state_ThermalServiceSatiation",
                     "L244.india_state_GenericServiceSatiation",
                     "L244.india_state_Intgains_scalar",
                     "L244.india_state_ShellConductance_bld",
                     "L244.india_state_Supplysector_bld",
                     "L244.india_state_FinalEnergyKeyword_bld",
                     "L244.india_state_SubsectorShrwtFllt_bld",
                     "L244.india_state_SubsectorInterp_bld",
                     "L244.india_state_SubsectorInterpTo_bld",
                     "L244.india_state_SubsectorLogit_bld",
                     "L244.india_state_StubTech_bld",
                     "L244.india_state_StubTechCalInput_bld",
                     "L244.india_state_StubTechMarket_bld",
                     "L244.india_state_GlobalTechIntGainOutputRatio",
                     "L244.india_state_GlobalTechInterpTo_bld",
                     "L244.india_state_GlobalTechEff_bld",
                     "L244.india_state_GlobalTechShrwt_bld",
                     "L244.india_state_GlobalTechCost_bld",
                     "L244.india_state_HDDCDD_HadCM3",
                     "L244.india_state_GlobalTechSCurve_bld",
                     "L244.Satiation_impedance_gcamindia",
                     "L244.GompFnParam_gcamindia",
                     "L244.GenericServiceImpedance_gcamindia",
                     "L244.GenericServiceCoef_gcamindia",
                     "L244.GenericServiceAdder_gcamindia",
                     "L244.ThermalServiceImpedance_gcamindia",
                     "L244.ThermalServiceCoef_gcamindia",
                     "L244.ThermalServiceAdder_gcamindia",
                     "L244.GenericServicePrice_gcamindia",
                     "L244.ThermalServicePrice_gcamindia",
                     "L244.GenericBaseDens_gcamindia",
                     "L244.ThermalBaseDens_gcamindia") ->
      building_india.xml

    return_data(building_india.xml)
  } else {
    stop("Unknown command")
  }
}
