# Exact encoding of plain R data as JSON-ready lists, shared by voipexa (the
# ModelsCloud service) and voiremote (the client). Numbers travel as their
# raw IEEE bytes in base64, so a matrix of PSA samples arrives bit for bit,
# and every attribute (names, dim, dimnames, class, levels, row names) is
# kept. Only data can be encoded: functions, environments and other code
# objects are refused, so nothing sent is ever run.

.vr_encode <- function(x) {
  enc_attr <- function(x) {
    a <- attributes(x)
    if (is.null(a)) return(NULL)
    lapply(a, .vr_encode)
  }
  b64 <- function(r) jsonlite::base64_enc(r)
  if (inherits(x, "vr_voi_data")) {
    return(list(t = "voi_data", name = x$name, get = lapply(x$get, function(k) k)))
  }
  out <- if (is.null(x)) {
    list(t = "null")
  } else if (is.function(x) || is.environment(x) || is.language(x) ||
             isS4(x) || typeof(x) %in% c("externalptr", "weakref", "bytecode")) {
    stop("Only data can be sent to ModelsCloud: R functions and other code ",
         "objects (", typeof(x), ") are not supported.", call. = FALSE)
  } else if (is.list(x)) {
    list(t = "list", v = unname(lapply(unclass(x), .vr_encode)))
  } else {
    v <- x
    attributes(v) <- NULL                     # as.vector() would turn a factor into labels
    switch(typeof(v),
      double = list(t = "dbl", b = b64(writeBin(v, raw(), size = 8, endian = "little"))),
      integer = list(t = "int", b = b64(writeBin(v, raw(), size = 4, endian = "little"))),
      logical = list(t = "lgl", b = b64(writeBin(as.integer(v), raw(), size = 4, endian = "little"))),
      character = list(t = "chr", v = as.list(v)),
      raw = list(t = "raw", b = b64(v)),
      stop("Cannot send R objects of type ", typeof(v), ".", call. = FALSE))
  }
  if (!is.null(x)) {
    if (out$t %in% c("dbl", "int", "lgl", "raw", "chr")) out$n <- length(x)
    a <- enc_attr(x)
    if (!is.null(a)) out$a <- a
  }
  out
}

.vr_decode <- function(e) {
  if (is.null(e$t)) stop("Malformed encoded object.", call. = FALSE)
  if (identical(e$t, "voi_data")) return(.vr_voi_data(e$name, e$get))
  b64 <- function(s) if (length(s) == 0 || identical(s, "")) raw() else jsonlite::base64_dec(s)
  x <- switch(e$t,
    null = return(NULL),
    list = lapply(e$v, .vr_decode),
    dbl = readBin(b64(e$b), "double", n = e$n, size = 8, endian = "little"),
    int = readBin(b64(e$b), "integer", n = e$n, size = 4, endian = "little"),
    lgl = as.logical(readBin(b64(e$b), "integer", n = e$n, size = 4, endian = "little")),
    chr = vapply(e$v, function(s) if (is.null(s)) NA_character_ else as.character(s),
                 character(1), USE.NAMES = FALSE),
    raw = b64(e$b),
    stop("Unknown encoded type: ", e$t, call. = FALSE))
  if (e$t == "chr" && length(x) != e$n) x <- character(e$n)
  if (!is.null(e$a)) attributes(x) <- lapply(e$a, .vr_decode)
  # attributes() expands a data frame's automatic row names to 1:n; restore
  # the compact form so the object is identical to the one sent.
  rn <- attr(x, "row.names")
  if (is.data.frame(x) && is.integer(rn) && identical(rn, seq_along(rn))) {
    attr(x, "row.names") <- c(NA_integer_, -length(rn))
  }
  x
}

# JSON text of an encoded object. Character vectors are written as arrays
# even when they have one element, and missing strings as null.
.vr_to_json <- function(e) {
  jsonlite::toJSON(e, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
}

.vr_from_json <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)

# A reference to one of voi's example datasets, optionally narrowed by a path
# of list elements or matrix/data frame columns (names or positions). Lets a
# request use the 10,000-sample examples without sending them.
.vr_voi_data <- function(name, get = NULL) {
  name <- as.character(unlist(name))
  known <- utils::data(package = "voi")$results[, "Item"]
  if (length(name) != 1L || !name %in% known) {
    stop("Unknown voi dataset: ", paste(name, collapse = ","), ". Available: ",
         paste(known, collapse = ", "), ".", call. = FALSE)
  }
  x <- getExportedValue("voi", name)
  for (k in unlist(get)) {
    x <- if (is.matrix(x) || is.data.frame(x)) x[, k] else x[[k]]
  }
  x
}

#' Refer to a voi example dataset by name
#'
#' Sends only the name: the service uses its own copy of the dataset.
#' @param name One of voi's datasets, such as `"chemo_nb"` or `"chemo_pars"`.
#' @param ... Optional path: list elements, then columns, by name or position
#'   (for example `voi_data("chemo_cea", "e", "Novel")`).
#' @return An object that the remote functions send as a reference.
#' @export
voi_data <- function(name, ...) {
  structure(list(name = name, get = list(...)), class = "vr_voi_data")
}
