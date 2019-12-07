play_game <- function(a, b) {
  state <- c(0, 0, 1)
  while (status(state[1], state[2]) == "sin terminar") {
    # show(state)
    state <- play_point(state, a, b)
  }
  if (status(state[1], state[2]) == "Gana 1") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}