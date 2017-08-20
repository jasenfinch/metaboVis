#' @importFrom purrr map
#' @importFrom mzR openMSfile
#' @importFrom dplyr rename

chromatogram <- function(files = files, time = 'Scan', intensity = 'TIC', backend = 'pwiz'){
  f <- str_split(files,'/') %>%
    map(~{return(.[length(.)])}) %>%
    unlist()
  mz <- map(files,~{
    openMSfile(.,backend = backend) %>%
      header() %>%
      return()
  })
  names(mz) <- f
  mz <- mz %>%
    bind_rows(.id = 'Sample') %>%
    select(Sample,acquisitionNum,polarity,totIonCurrent,retentionTime,basePeakIntensity) %>%
    rename(Scan = acquisitionNum,TIC = totIonCurrent, basePeak = basePeakIntensity) %>%
    group_by(Sample,polarity) %>%
    mutate(Scan = seq_along(polarity))

  mz$polarity[mz$polarity == 0] <- 'Negative'
  mz$polarity[mz$polarity == 1] <- 'Positive'

  ggplotly(ggplot(mz,aes_string(x = time, y = intensity, group = 'Sample', colour = 'polarity')) +
             geom_line() +
             scale_colour_ptol(guide = F) +
             theme(legend.position = "none") +
             theme_bw() +
             facet_wrap(~polarity))
}
