#' @importFrom magrittr "%>%"
#' @export
oecd_bli_2016_from_raw_data <- function() {
  normalize <- function(x) (x - min(x))/(max(x) - min(x))
  reverse <- function(x) 1-x
  I <- read.csv(system.file("extdata", "BLI_10102016095652859.csv", package = "rankoptimalweights"),
                stringsAsFactors = FALSE) %>%
    dplyr::filter(Inequality=="Total", LOCATION!="OECD") %>%
    dplyr::select(LOCATION,INDICATOR,Value) %>%
    dplyr::mutate(ReverseINDICATOR = INDICATOR %in% c("HO_BASE", "HO_HISH", "JE_LTUR", "JE_LMIS",
                                                      "EQ_AIRP", "PS_REPH", "WL_EWLH")) %>%
    dplyr::group_by(INDICATOR)  %>%
    dplyr::mutate(NValue = ifelse(ReverseINDICATOR, reverse(normalize(Value)), normalize(Value)),
                  DIMENSION = substr(INDICATOR,1,2))
  D <- dplyr::group_by(I,LOCATION,DIMENSION) %>%
    dplyr::summarize(Value=mean(NValue)) %>%
    tidyr::spread(DIMENSION,Value) %>%
    mutate(COUNTRY=countrycode::countrycode(LOCATION,"iso3c","country.name"))
  # http://www.oecd.org/statistics/OECD-Better-Life-Index-2016-definitions.pdf
  return(select(D, Country=LOCATION, Housing=HO, Income=IW, Jobs=JE, Community=SC, Education=ES,
                Environment=EQ, CivicEngagement=CG, Health=HS, LifeSatisfaction=SW,
                Safety=PS, WorkLifeBalance=WL))
}
