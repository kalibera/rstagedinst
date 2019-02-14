#! /usr/bin/Rscript
# ^^^^ please replace with full path to Rscript from R-devel
#      or run with Rscript from R-devel

# requires a single argument: package name
#
# The package is loaded (so it has to be on R_LIBS path) and scanned for
# hard-coded temporary installation directories. 
#
# It reports directory names as found via serialization of the whole
# namespace into ASCII, these paths may be repeated if they are found
# multiple times.  Prepended with "CONTAINS:"
# 
# It also reports "paths" to objects holding these directories, in two
# forms.
#
# OBJPATH: - this should be an executable R expression to reach the object,
#            but not very concise, not very easy to read
#
# SPATH:   - short version of the same path, in a simplified, concise
#             notation
#           
#               $name named vector element
#               [i]   unnamed vector element
#               -A    attributes
#               -E    environment
#                @    S4 data part
#

addvnames <- function(objpath, x, i) {
  n <- base::names(x)
  if (!base::is.null(x))
    base::c(objpath, base::paste0("[[\"", n[i], "\"]]"))
  else
    base::c(objpath, base::paste0("[[",i,"]]"))
}

addvsnames <- function(spath, x, i) {
  n <- base::names(x)
  if (!base::is.null(x))
    base::c(spath, "$", n[i])
  else
    base::c(spath, "[", i, "]")
}

# recursive search function
#    prev - object accessed previously (for primitive infinite recursion
#           prevention)
#       x - object to search from
# objpath - vector of path elements (strings) to x, to be concatenated
#           before printing; this should be executable when pasted to R
#           command line to get the object again
#   spath - like objpath, but more concise path, more suitable for reading
#
# the search is desctructive, environment get a special attribute to prevent
#   infinite recursion

.wcheck_found <- FALSE
wcheck <- function(prev, x, objpath, spath) {

 # uncomment for debugging
 #base::cat("CHECKING: ", base::paste0(objpath, collapse=""), "\n")
 #base::cat("SCHECKING: ", base::paste0(spath, collapse=""), "\n")

  if (base::is.null(x) || base::identical(prev, x) ||
      !base::is.null(base::attr(x, "__dpkg__checked")) ||

      (base::isS4(x) && base::is.environment(x)
       && !base::is.null(base::attr(x, "__dpkg__checked"))) ||

      (identical(x,.GlobalEnv) && !is.null(prev)))

    return()

  wcheck(x, base::attributes(x),
           base::c("attributes(", objpath, ")"),
           base::c(spath, "-A"))

  if (base::is.atomic(x) && !base::is.character(x))
    return()

  if (base::is.character(x) && base::length(x) == 1) {
    found <- base::grep(pattern="00new", x[[1]], fixed=TRUE, value=TRUE)
    if (base::length(found)) {
      if (!.wcheck_found) {
        .wcheck_found <<- TRUE
        cat("\nPackage contains these objects with hard-coded paths (walkcheck):\n")
      }
      cat("OBJPATH: ", paste0(objpath, collapse=""), found, "\n")
      cat("SPATH: ", paste0(spath, collapse=""), found, "\n")
    }
    return()
  }

  if (base::is.vector(x) || base::is.list(x)) {
    for(i in base::seq_along(x))
      wcheck(x, x[[i]], addvnames(objpath, x, i), addvsnames(spath, x, i))
  } else if (base::isS4(x) && base::is.environment(x)) {
    wcheck(x, x@.Data, base::c(objpath, "@.Data"), base::c(spath, "@"))
  } else if (base::is.environment(x) &&
            !base::isNamespace(x)) {
    
    xl <- base::as.list.environment(x, all.names=TRUE)
    xln <- base::names(xl)
    base::attributes(xl) <- attributes(x)
    base::names(xl) <- xln
    base::attr(x, "__dpkg__checked") <- TRUE
    wcheck(x, xl,
             base::c("as.list(", objpath, ",all.names=TRUE)"), 
             spath)
  } else if (base::is.function(x)) {
    wcheck(x, base::environment(x),
             base::c("environment(", objpath, ")"),
             base::c(spath, "-E-"))
  }
}

# Check for strings including "00new" (hardcoded temporary installation
# paths) in R object x using serialization, so similar to how it is done in
# R CMD INSTALL, but slower and giving usually the whole path, which often
# can help in finding where it is set - just by additional manual grep of
# the package source. This should be more reliable than the diagnostic
# search.

sercheck <- function(x) {
  tmpf <- base::tempfile()
  f <- base::file(tmpf, "w")
  base::serialize(x, f, ascii=TRUE)
  base::close(f)
  found <- base::grep(pattern="00new", base::readLines(tmpf), value=TRUE)
  base::unlink(tmpf)
  if (base::length(found)) {
    base::cat("Package contains these hard-coded paths (sercheck):\n")
    base::cat(base::paste("CONTAINS:", found, collapse="\n"), "\n")
    TRUE
  } else
    FALSE
}

# Diagnostic walking search through namespace/package nsname, for strings
# containing "00new" (hardcoded temporary installation paths).  Reports
# paths to get the discovered objects from the namespace root.

walkcheck <- function(nsname,
                    l = base::as.list(base::getNamespace(nsname), all.names=TRUE)) {
  .wcheck_found <<- FALSE
  wcheck(NULL, l, base::c("as.list(getNamespace(\"",nsname,"\"), all.names=TRUE)"), nsname )
  .wcheck_found
}

nsname <- commandArgs(trailingOnly=T)
if (length(nsname)) {
  base::suppressPackageStartupMessages(base::library(nsname, character.only=TRUE))
  l <- base::as.list(base::getNamespace(nsname), all.names=TRUE)  
  ress <- sercheck(l)
  resw <- walkcheck(nsname, l)
  base::q(save="no", status = ress || resw, runLast = FALSE)
}
