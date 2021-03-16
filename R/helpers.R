disk.usage <- function(path = Sys.getenv("HOME"), unit = "G") {
  if (length(system("which df", intern = TRUE, ignore.stderr = TRUE))) {
    cmd <- sprintf("df -B%s %s", unit, path)
    exec <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    exec <- strsplit(exec[length(exec)], "[ ]+")[[1]][3:4]
    structure(exec, names = c("used", "available"))
  } else {
    ## TODO: currently Linux/macOS only
    stop("'df' command not found")
  }
}

revText <- function(x) {
  paste(rev(strsplit(x, NULL)[[1]]), collapse = "")
}
