#' @title Did It Snow?
#'
#' @author Robert
#'
#' @details On a given date, did it (probably) snow at a given NEON site? TIS ONLY
#'
#' @param site A NEON TIS site
#' @param date A date... YYYY-MM-DD
#'
#' @return Logical T/F for if it might have snowed
#'
#' @export
#'

didItSnow=function(site, date){
  # set the default. It hasn't
  itSnowed=FALSE

  # load up the site info
  siteInfo=ytBangerz::siteInfo

  domn=siteInfo$DomainCode[siteInfo$SiteCode==site]

  # What is the stream for SP3 HMP155 Temp?
  hmpTempStream=paste0("NEON.",domn,".",site,"00098.001.01309.003.000.000")

  # return temperature data
  hmpTemp=ytBangerz::getL0Data(site = site, streams = hmpTempStream, startDate = date, endDate = date+1, creds = creds)

  # Were there any readings at or below 2 dC?
  coldOut=any(hmpTemp$readout_val_double<=2)

  if(coldOut){
    hmpRhStream=paste0("NEON.",domn,".",site,".DP0.00098.001.01357.003.000.000")
    # return humidity
    hmpRH=ytBangerz::getL0Data(site = site, streams = hmpRhStream, startDate = date, endDate = date+1, creds = creds)

    # was it wet out?
    wetOut=any(hmpRH$readout_val_double>=90)

    if(wetOut){
      # if we're here, it probably snowed
      itSnowed=TRUE
    }
  }
  return(itSnowed)
}
