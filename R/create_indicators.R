create_indicators <- function(n, colors, label = "Indicator") {
  if (length(colors) < n) {
    stop("The length of the colors vector must be at least equal to n.")
  }

  # Create a list of indicator divs
  indicators <- lapply(seq_len(n), function(i) {
    div(
      id = paste0("indicator_", i),
      style = sprintf(
        "
        width: 50px;
        height: 25px;
        background-color: %s;
        border: 0px solid white;
        display: inline-block;
        margin-right: 0px;",
        colors[i]
      )
    )
  })

  # Wrap the indicators in a container div
  div(
    id = "indicator_container",
    style = "display: flex; align-items: center; margin-bottom: 1px;",
    do.call(tagList, indicators),
    span(label, style = "font-size: 14px; font-weight: bold; margin-left: 10px;")
  )
}
