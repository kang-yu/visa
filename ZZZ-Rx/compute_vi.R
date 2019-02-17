#' Calculate Vegetation Indices in Remote Sensing Studies
#'
#' This function calculates vegetation indices used in remote sensing studies
#' and applications.
#'
#' @param x matrix or dataframe using columns for wavebands and rows for observations,
#' colnames should contain wavelength, rownames can be used for sample IDs.
#' @return
#' \item{v}{returns a dataframe of vegetation indices}
#'
#' @export

compute_vi <- function(x){

  if (!is.data.frame(x)) x <- as.data.frame(x)
  wl <- colnames(x)

  # Using "X" as prefix because R automatically put "X" before numberic column-head
  if (!any(grep("X", wl))){
    temp1 <- gregexpr("[0-9]+", wl)
    wvlt <- as.numeric(unique(unlist(regmatches(wl, temp1))))
    colnames(x) <- paste0("X", wvlt)
  }

  # v <- x[1]; # v[1] <- NA
  v <- x[0]


  #########################################################################
  # Chl, Simple ratios,

  v$ZM <- x$X750 / x$X710
  v$CI.RE <- (x$X750 - x$X700) / x$X700
  v$CI.G <- (x$X750 - x$X550) / x$X550
  v$SR.Vog <- x$X740/x$X720

  #########################################################################
  # Water
  v$WI <- x[,"X970"] / x[,"X900"] # Penuelas et al 1997
  v$WI2 <- x[,"X1300"] / x[,"X1450"] # Seelig et al 2008
  v$NDWI <- (x[,"X857"] - x[,"X1241"]) / (x[,"X857"] + x[,"X1241"])

  # Eitel et al. /Forest Ecology and Management 229 (2006) 170–182
  pmax1500.1750 <- names(which.max(colMeans(x[, paste0("X", c(1500:1750))])))
  pmin1500.1750 <- names(which.min(colMeans(x[, paste0("X", c(1500:1750))])))
  Rmax1500.1750 <- x[,pmax1500.1750]
  Rmin1500.1750 <- x[,pmin1500.1750]
  v$MDWI <- (Rmax1500.1750 - Rmin1500.1750) / (Rmax1500.1750 + Rmin1500.1750)

  # NDVI
  v$NDVI <- (x[,"X800"] - x[,"X680"]) / (x[,"X800"] + x[,"X680"])
  v$ND705 <- (x[,"X750"] - x[,"X705"]) / (x[,"X750"] + x[,"X705"])

  v$PRI <- (x[,"X531"] - x[,"X570"]) / (x[,"X531"] + x[,"X570"])
  v$NPQI <- (x[,"X415"] - x[,"X435"])/(x[,"X415"] + x[,"X435"])
  v$SIPI <- (x[,"X800"] - x[,"X445"]) / (x[,"X800"] - x[,"X680"])
  v$PSRI <- (x[,"X680"] - x[,"X500"]) / x[,"X750"]

  v$mSR705 <- (x[,"X750"] - x[,"X445"]) / (x[,"X705"] - x[,"X445"])
  v$mND705 <- (x[,"X750"] - x[,"X705"]) / (x[,"X750"] + x[,"X705"] - 2 * x[,"X445"])

  v$MTCI <- (x[,"X750"] - x[,"X710"])/(x[,"X710"] - x[,"X680"])
  v$RRDI <- (x[,"X750"] - x[,"X740"])/(x[,"X740"] - x[,"X700"])

  ###################################################################
  ## Red-edge position methods

  # 700 + 40*[(R670 + R780)/2-R700]/(R740-R700)
  # Red-edge Position Index
  v$REPI <- 700 + 40 * ((x[,"X670"] + x[,"X780"])/2 - x[,"X700"])/(x[,"X740"] - x[,"X700"])

  ###################################################################
  # Combined indices
  # CARI
  v$MCARI <- ((x[,"X700"] - x[,"X670"]) - 0.2 * (x[,"X700"] - x[,"X550"])) * (x[,"X700"]/x[,"X670"])
  v$TCARI <- 3 * ((x[,"X700"] - x[,"X670"]) - 0.2 * (x[,"X700"] - x[,"X550"]) * (x[,"X700"]/x[,"X670"]))
  v$OSAVI <- (1 + 0.16) * (x[,"X800"] - x[,"X670"])/(x[,"X800"] + x[,"X670"] + 0.16)
  v$MCARI2OSAVI <- v$MCARI / v$OSAVI
  v$TCARI2OSAVI <- v$TCARI / v$OSAVI
  v$MCARI2TCARI <-  v$MCARI / v$TCARI

  # Disease
  v$HI <- (x[,"X534"] - x[,"X698"])/(x[,"X534"] + x[,"X698"]) - 1/2 * x[,"X704"]
  v$DSI <- 6.9 * (x[,"X605"] - x[,"X455"]) - 1.2

  # CAI (Oppelt, 2002; Laudien et al., 2003)
  w <- 600:740
  r <- x[, which(colnames(x) %in% paste0("X", w))]
  A.trapezoid <- (x[,"X600"] + x[,"X740"]) * (740 - 600)/2
  A.AURC <- apply(r, 1, function(x) flux::auc(w, x)) # area under reflectance-curve
  v$CAI <- A.trapezoid - A.AURC

  return(v)
}
