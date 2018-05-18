#install.packages("gmapsdistance")
#install.packages("RCurl")
#devtools::install_github("rodazuero/gmapsdistance@058009e8d77ca51d8c7dbc6b0e3b622fb7f489a2")
require(gmapsdistance)


transport = as.data.frame(education2016Df[,1])
transport$suburbTemp = paste0(transport$`education2016Df[, 1]`, "+Victoria+Australia")
transport$suburbTemp = gsub( " *\\(.*?\\) *", "", transport$suburbTemp)
transport$suburbTemp = sub("\\s", "+", transport$suburbTemp)
transport$suburbTemp = sub("\\s", "+", transport$suburbTemp)
transport$suburbTemp = sub("\\s", "+", transport$suburbTemp)
transport$suburbTemp = sub("\\s", "+", transport$suburbTemp)
transport$suburbTemp = sub("\\++", "+", transport$suburbTemp)

#key = "AIzaSyBjvv674LsHsJlG6zb-lrKt-Z-QONQIvgI"
driving = gmapsdistance(origin = transport[,2], destination = "Melbourne+CBD+Victoria+Australia", mode = "driving" , key = "AIzaSyC1-Q_N4kA_VjaFqXAuHHt7kK9U6nCFjfU", arr_date = "2019-04-24", arr_time = "22:30:00", traffic_model = "best_guess", combinations = "all")
transit = gmapsdistance(origin = transport[,2], destination = "Melbourne+CBD+Victoria+Australia", mode = "transit" , key = "AIzaSyAU_6qj-S_VsEM4VybDWBJ0-_vtfs42Xpo", arr_date = "2019-04-24", arr_time = "22:30:00", combinations = "all")


transport$driving = round(as.vector(driving$Time[[2]])/60)
transport$transit = round(as.vector(transit$Time[[2]])/60)

transport = transport[,-2]
colnames(transport) = c("Suburb", "Driving Time", "Public Transport Time")

write.csv(transport, file = "Transport/transport_by_suburb.csv", row.names=FALSE)