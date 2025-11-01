#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Filename : func.R
# Use      : Convenient functions for public health 
# Author   : Tomas Sou (soutomas)
# Created  : 2025-10-31
# Updated  : 2025-11-01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Notes
# - NA
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Updates 
# - NA
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Odds ratio 
#' 
#' Odds ratio (OR):
#' - `OR = (Odds in the exposed)/(Odds in the unexposed)`
#'
#' @param a_case_exp `<num>` Number of cases exposed.
#' @param b_ctrl_exp `<num>` Number of controls exposed.
#' @param c_case_uex `<num>` Number of cases unexposed.
#' @param d_ctrl_uex `<num>` Number of controls unexposed.
#' @returns Odds ratio. 
#' @export
#' @examples
#' odds_ratio(240,160,160,240)
odds_ratio = function(a_case_exp,b_ctrl_exp,c_case_uex,d_ctrl_uex){
  out = (a_case_exp/b_ctrl_exp)/(c_case_uex/d_ctrl_uex)
  return(out) 
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Risk ratio (relative risk)
#'
#' @description
#'   Incidence exposed (Ie) or Risk exposed (Re):
#'   - `Re = cases_exposed/(cases_exposed + cases_exposed)`
#'   
#'   Incidence unexposed (Iu) or risk unexposed (Ru):
#'   - `Ru = cases_unexposed/(cases_unexposed + non-cases_unexposed)`
#'   
#'   Risk ratio (RR) or relative risk:
#'   - `RR = Incidence_exposed / Incidence_unexposed`
#'   
#'   Attributable risk (AR) or risk difference:
#'   - `AR = Incidence_exposed - Incidence_unexposed` 
#' 
#' @param a_case_exp `<num>` Number of cases exposed.
#' @param b_ncas_exp `<num>` Number of non-cases exposed.
#' @param c_case_uex `<num>` Number of cases unexposed.
#' @param d_ncas_uex `<num>` Number of non-cases unexposed.
#' @returns A list containing risk exposed, risk unexposed, 
#'   risk ratio (relative risk) and attributable risk. 
#' @export
#' @examples
#' risk_ratio_attr(60,140,20,180)
#' risk_ratio_attr(60,140,20,180)
risk_ratio_attr = function(a_case_exp,b_ncas_exp,c_case_uex,d_ncas_uex){
  risk_exposed = a_case_exp/(a_case_exp+b_ncas_exp)
  risk_unexposed = c_case_uex/(c_case_uex+d_ncas_uex)
  rela_risk = risk_exposed/risk_unexposed
  attr_risk = risk_exposed-risk_unexposed
  out = list(
    Risk_exposed = risk_exposed, 
    Risk_unexposed = risk_unexposed, 
    Risk_ratio = rela_risk, 
    Attributable_risk = attr_risk
  ) 
  return(out)
}
