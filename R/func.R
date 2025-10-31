#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Filename : func.R
# Use      : Convenient functions for public health 
# Author   : Tomas Sou (soutomas)
# Created  : 2025-10-31
# Updated  : 2025-10-31
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
#' @param a_caex `<num>` Number of cases exposed.
#' @param b_coex `<num>` Number of controls exposed.
#' @param c_caux `<num>` Number of cases unexposed.
#' @param d_coux `<num>` Number of controls unexposed.
#' @returns Odds ratio. 
#' @export
#' @examples
#' odds_ratio(240,160,160,240)
odds_ratio = function(a_caex,b_coex,c_caux,d_coux){
  out = (a_caex/b_coex)/(c_caux/d_coux)
  return(out) 
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Risk ratio (relative risk)
#'
#' @description
#'   Incidence exposed (Ie) or Risk exposed (Re)):
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
#' @param a_caex `<num>` Number of cases exposed.
#' @param b_ncex `<num>` Number of non-cases exposed.
#' @param c_caux `<num>` Number of cases unexposed.
#' @param d_ncux `<num>` Number of non-cases unexposed.
#' @returns Risk ratio (relative risk) and/or attributable risk. 
#' @export
#' @examples
#' risk_ratio_attr(60,140,20,180)
#' risk_ratio_attr(60,140,20,180)
risk_ratio_attr = function(a_caex,b_ncex,c_caux,d_ncux){
  risk_exposed = a_caex/(a_caex+b_ncex)
  risk_unexposed = c_caux/(c_caux+d_ncux)
  rela_risk = risk_exposed/risk_unexposed
  attr_risk = risk_exposed-risk_unexposed
  out = list(Re=risk_exposed, Ru=risk_unexposed, RR=rela_risk, AR=attr_risk) 
  return(out)
}
