status.test <- function(s.ftn) {
  x.vec <- (-1):11
  y.vec <- (-1):11
  plot(x.vec, y.vec, type = "n", xlab = "x", ylab = "y")
  for (x in x.vec) {
    for (y in y.vec) {
      s <- s.ftn(x, y)
      if (s == "imposible") text(x, y, "X", col = "red")
      else if (s == "sin terminar") text(x, y, "?", col = "blue")
      else if (s == "Gana 1") text(x, y, "1", col = "green")
      else if (s == "Gana 2") text(x, y, "2", col = "green")
    }
  }
  return(invisible(NULL))
}