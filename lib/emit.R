#' @name emit function
#' @description writes plots to disk and to screen, fixed size 1024x768 PNG; creates directory if necessary
#' @param p the plot object
#' @param tag a name tag for the plot file, default 'unknown'
#' @param path the disk path prefix, relative, default 'plots/', must exist
#' @param prefix plot group name prefix, default 'sc_' for 'scorecard'
#' @param suffix plot name suffix, default '.png'
#' @param showWarnings whether to show warnings on directory creation, passed to dir.create(), default FALSE
#' @param recursive whether to create path elements other than last, passed to dir.create(), default TRUE
#' @param width plot image width in pixels, default 2014
#' @param height plot image height in pixels, default 768
#' @return nothing
emit <- function(p,
                 tag='uknnown',
                 path="plots/",
                 prefix="sc_",
                 suffix=".png",
                 showWarnings=FALSE,
                 recursive=TRUE,
                 width=1024,
                 height=768) {
  if ("grob" %in% class(p)) {
    grid.newpage()
    grid.draw(p)
  } else {
    print(p)
  }
  dir.create(path,showWarnings,recursive)
  name = paste(path,prefix,tag,suffix,sep='')
  dev.copy(png,width=width,height=height,name)
  dev.off()
}