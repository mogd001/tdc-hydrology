# Clear outputs folder
if (dir.exists("./outputs")) {
  unlink("./outputs", recursive = TRUE)
  Sys.sleep(2) # give time for the removal of the directory to take place
}
dir.create("./outputs")