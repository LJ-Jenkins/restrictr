#' @import cli
#' @import rlang
#' @import vctrs
#' @importFrom glue glue
NULL

on_load(
  local_use_cli(
    format = TRUE,
    inline = TRUE
  )
)
