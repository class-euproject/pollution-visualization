

temp = list.files(path = "./Data", pattern="^results",full.names = TRUE)
for (i in 1:length(temp)){
  print(temp[i])

  data <- read.csv(temp[i])

  write.csv(data, "./Data/stream.csv")
  Sys.sleep(3)
}


