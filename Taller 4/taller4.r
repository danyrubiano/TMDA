require(bnlearn)
library("Rgraphviz")

bif <- read.bif("G:\\Mi unidad\\Universidad\\Semestre 11\\TMDA\\Taller 4\\insurance.bif")
net <- read.net("G:\\Mi unidad\\Universidad\\Semestre 11\\TMDA\\Taller 4\\insurance.net")
dsc <- read.dsc("G:\\Mi unidad\\Universidad\\Semestre 11\\TMDA\\Taller 4\\insurance.dsc")
insurance_net <- readRDS("G:\\Mi unidad\\Universidad\\Semestre 11\\TMDA\\Taller 4\\insurance.rds")

data = insurance[,c(2,11,1,3:10,12:27)]

# Creacion de red
set.seed(0)
#En base a la literatura se crea el DAG asociado a la red bayesiana
modelstring =paste("[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]",
                   "[RiskAversion|Age:SocioEcon][OtherCar|SocioEcon][VehicleYear|SocioEcon:RiskAversion]",
                   "[MakeModel|SocioEcon:RiskAversion][SeniorTrain|Age:RiskAversion]",
                   "[HomeBase|SocioEcon:RiskAversion][AntiTheft|SocioEcon:RiskAversion]",
                   "[RuggedAuto|VehicleYear:MakeModel][Antilock|VehicleYear:MakeModel]",
                   "[DrivingSkill|Age:SeniorTrain][CarValue|VehicleYear:MakeModel:Mileage]",
                   "[Airbag|VehicleYear:MakeModel][DrivQuality|RiskAversion:DrivingSkill]",
                   "[Theft|CarValue:HomeBase:AntiTheft][Cushioning|RuggedAuto:Airbag]",
                   "[DrivHist|RiskAversion:DrivingSkill][Accident|DrivQuality:Mileage:Antilock]",
                   "[ThisCarDam|RuggedAuto:Accident][OtherCarCost|RuggedAuto:Accident]",
                   "[MedCost|Age:Accident:Cushioning][ILiCost|Accident]",
                   "[ThisCarCost|ThisCarDam:Theft:CarValue][PropCost|ThisCarCost:OtherCarCost]",
                   sep = "")

dag_lit = model2network(modelstring)
score.dag_lit <- score(dag_lit, data)
logLik.dag_lit <- logLik(dag_lit, data)

#Se realiza la búsqueda el algoritmo Hill-Climb
res.hc <- hc(x = data)

fitted.hc <- bn.fit(res.hc, data = insurance )
score.hc <- score(res.hc, insurance)
logLik.hc <- logLik(res.hc, insurance)

shd(res.hc,dag_lit,debug = TRUE)
print(unlist(compare(res.hc,dag_lit)))
compare(res.hc,dag_lit, arcs = TRUE)

# mmhc
res.mmhc <- mmhc(x = data)

fitted.mmhc <- bn.fit(res.mmhc, data = insurance )
score.mmhc <- score(res.mmhc, insurance)
logLik.mmhc <- logLik(res.mmhc, insurance)

shd(res.mmhc,dag_lit,debug = TRUE)
print(unlist(compare(res.mmhc,dag_lit)))
compare(res.mmhc,dag_lit, arcs = TRUE)

# mmpc
res.mmpc <- mmpc(x = data)

fitted.mmpc <- bn.fit(res.mmpc, data = insurance )
score.mmpc <- score(res.mmpc, insurance)
logLik.mmpc <- logLik(res.mmpc, insurance)

shd(res.mmpc,dag_lit,debug = TRUE)
print(unlist(compare(res.mmpc,dag_lit)))
compare(res.mmpc,dag_lit, arcs = TRUE)

####

#al saber que hc entrega los mejores resultados se intenta variar la semilla y el restart
maximoTp = 0
for(seed in 0:20){
  print(seed)
  for(i in 0:20){
    set.seed(seed)
    res = hc(x = data, start = dag_lit, restart = i)
    tph = unlist(compare(dag_lit, res))["tp"]
    if(maximoTp < tph){
      maximoTp = tph
      print(paste("tp: ",maximoTp," i: ",i," seed: ",seed, "HC"))
    }
  }
}

# "tp:  45  i:  0  seed:  0 HC"
set.seed(0)
res.hc1 <- hc(x = data,start = dag_lit, restart = 0)

fitted.hc1 <- bn.fit(res.hc1, data = insurance )
score.hc1 <- score(res.hc1, insurance)
logLik.hc1 <- logLik(res.hc1, insurance)

shd(res.hc1,dag_lit,debug = TRUE)
print(unlist(compare(res.hc1,dag_lit)))
compare(res.hc1,dag_lit, arcs = TRUE)

####
res.hc2 <- hc(x = data,start = dag_lit, restart = 0,whitelist = data.frame(c("Age"),c("MedCost")))

fitted.hc2 <- bn.fit(res.hc2, data = data )
score.hc2 <- score(res.hc2, data)
logLik.hc2 <- logLik(res.hc2, data)

shd(res.hc2,dag_lit,debug = TRUE)
print(unlist(compare(res.hc2,dag_lit)))
compare(res.hc2,dag_lit, arcs = TRUE)

####
res.hc3 <- hc(x = data,start = dag_lit, restart = 0,whitelist = data.frame(c("Age","RiskAversion","RiskAversion","Theft","CarValue","HomeBase","AntiTheft")
                                                                           ,c("MedCost","VehicleYear","MakeModel","ThisCarCost","Theft","Theft","Theft")),
              blacklist = data.frame(c("DrivQuality","Accident"),c("Mileage","Mileage")))

fitted.hc3 <- bn.fit(res.hc3, data = data )
score.hc3 <- score(res.hc3, data)
logLik.hc3 <- logLik(res.hc3, data)

shd(res.hc3,dag_lit,debug = TRUE)
print(unlist(compare(res.hc3,dag_lit)))
compare(res.hc3,dag_lit, arcs = TRUE)


###
# plots

par(mfrow = c(1,2), omi = rep(0,4), mar = c(1,0,1,0))
strength = arc.strength(dag_lit, data)
strength1 = arc.strength(res.hc3, data)
g1 <- strength.plot(res.hc3, strength1)
g <- strength.plot(dag_lit, strength)
graph::nodeRenderInfo(g1) <- list(fontsize=60)
Rgraphviz::renderGraph(g1)
graph::nodeRenderInfo(g) <- list(fontsize=60)
Rgraphviz::renderGraph(g)

par(mfrow = c(1,2), omi = rep(0,4), mar = c(1,0,1,0))
g2 <- graphviz.plot(res.hc3)
g3 <- graphviz.plot(dag_lit, highlight = list(arcs = arcs(res.hc3)))
graph::nodeRenderInfo(g2) <- list(fontsize=50)
Rgraphviz::renderGraph(g2)
graph::nodeRenderInfo(g3) <- list(fontsize=50)
Rgraphviz::renderGraph(g3)

#### Consultas

cpquery(fitted.hc3,evidence = (Age == "Adolescent"),event = (RiskAversion == "Psychopath"))
cpquery(fitted.hc3,evidence = (Age == "Adult"),event = (RiskAversion == "Psychopath"))
cpquery(fitted.hc3,evidence = (Age == "Senior"),event = (RiskAversion == "Psychopath"))

# Segun la edad y la situación socioeconomica, ¿cómo incide en la aversión a ser psicopata?
cpquery(fitted.hc3,evidence = (Age == "Adolescent" & SocioEcon == "Middle"),event = (RiskAversion == "Psychopath"))
cpquery(fitted.hc3,evidence = (Age == "Adolescent" & SocioEcon == "Prole"),event = (RiskAversion == "Psychopath"))
cpquery(fitted.hc3,evidence = (Age == "Adolescent" & SocioEcon == "UpperMiddle"),event = (RiskAversion == "Psychopath"))
cpquery(fitted.hc3,evidence = (Age == "Adolescent" & SocioEcon == "Wealthy"),event = (RiskAversion == "Psychopath"))

cpquery(fitted.hc3,evidence = (Age == "Adult" & SocioEcon == "Middle"),event = (RiskAversion == "Psychopath"))
cpquery(fitted.hc3,evidence = (Age == "Adult" & SocioEcon == "Prole"),event = (RiskAversion == "Psychopath")) #<---- Este es el importante
cpquery(fitted.hc3,evidence = (Age == "Adult" & SocioEcon == "UpperMiddle"),event = (RiskAversion == "Psychopath"))
cpquery(fitted.hc3,evidence = (Age == "Adult" & SocioEcon == "Wealthy"),event = (RiskAversion == "Psychopath"))

cpquery(fitted.hc3,evidence = (Age == "Senior" & SocioEcon == "Middle"),event = (RiskAversion == "Psychopath"))
cpquery(fitted.hc3,evidence = (Age == "Senior" & SocioEcon == "Prole"),event = (RiskAversion == "Psychopath"))
cpquery(fitted.hc3,evidence = (Age == "Senior" & SocioEcon == "UpperMiddle"),event = (RiskAversion == "Psychopath"))
cpquery(fitted.hc3,evidence = (Age == "Senior" & SocioEcon == "Wealthy"),event = (RiskAversion == "Psychopath"))


cpquery(bif,evidence = (Age == "Adolescent" & SocioEcon == "Middle"),event = (RiskAversion == "Psychopath"))
cpquery(bif,evidence = (Age == "Adolescent" & SocioEcon == "Prole"),event = (RiskAversion == "Psychopath"))
cpquery(bif,evidence = (Age == "Adolescent" & SocioEcon == "UpperMiddle"),event = (RiskAversion == "Psychopath"))
cpquery(bif,evidence = (Age == "Adolescent" & SocioEcon == "Wealthy"),event = (RiskAversion == "Psychopath"))

cpquery(bif,evidence = (Age == "Adult" & SocioEcon == "Middle"),event = (RiskAversion == "Psychopath"))
cpquery(bif,evidence = (Age == "Adult" & SocioEcon == "Prole"),event = (RiskAversion == "Psychopath")) #<---- Este es el importante
cpquery(bif,evidence = (Age == "Adult" & SocioEcon == "UpperMiddle"),event = (RiskAversion == "Psychopath"))
cpquery(bif,evidence = (Age == "Adult" & SocioEcon == "Wealthy"),event = (RiskAversion == "Psychopath"))

cpquery(bif,evidence = (Age == "Senior" & SocioEcon == "Middle"),event = (RiskAversion == "Psychopath"))
cpquery(bif,evidence = (Age == "Senior" & SocioEcon == "Prole"),event = (RiskAversion == "Psychopath"))
cpquery(bif,evidence = (Age == "Senior" & SocioEcon == "UpperMiddle"),event = (RiskAversion == "Psychopath"))
cpquery(bif,evidence = (Age == "Senior" & SocioEcon == "Wealthy"),event = (RiskAversion == "Psychopath"))

#################################################################################
# Dado un accidente,  ¿en qué medida la presencia de airbag y las condiciones de 
# amortiguación generan costos médicos altos?

cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Excellent" & Accident == "Moderate"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Excellent" & Accident == "Mild"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Excellent" & Accident == "None"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Excellent" & Accident == "Severe"),event = (MedCost == "HundredThou"))

cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Fair" & Accident == "Moderate"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Fair" & Accident == "Mild"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Fair" & Accident == "None"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Fair" & Accident == "Severe"),event = (MedCost == "HundredThou"))

cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Good" & Accident == "Moderate"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Good" & Accident == "Mild"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Good" & Accident == "None"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Good" & Accident == "Severe"),event = (MedCost == "HundredThou"))

cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Poor" & Accident == "Moderate"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Poor" & Accident == "Mild"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Poor" & Accident == "None"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "True" & Cushioning == "Poor" & Accident == "Severe"),event = (MedCost == "HundredThou"))

cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Excellent" & Accident == "Moderate"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Excellent" & Accident == "Mild"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Excellent" & Accident == "None"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Excellent" & Accident == "Severe"),event = (MedCost == "HundredThou"))

cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Fair" & Accident == "Moderate"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Fair" & Accident == "Mild"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Fair" & Accident == "None"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Fair" & Accident == "Severe"),event = (MedCost == "HundredThou"))

cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Good" & Accident == "Moderate"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Good" & Accident == "Mild"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Good" & Accident == "None"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Good" & Accident == "Severe"),event = (MedCost == "HundredThou"))

cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Poor" & Accident == "Moderate"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Poor" & Accident == "Mild"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Poor" & Accident == "None"),event = (MedCost == "HundredThou"))
cpquery(fitted.hc3,evidence = (Airbag == "False" & Cushioning == "Poor" & Accident == "Severe"),event = (MedCost == "HundredThou"))


#########################################################################
# ¿En qué medida intervienen en un accidente severo características de 
# kilometraje, antilock y calidad de conducción? 

cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Excellent" & Mileage == "Domino"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Normal" & Mileage == "Domino"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Poor" & Mileage == "Domino"),event = (Accident == "Severe")) #<--- maximiza la probabilidad de un accidente severo

cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Excellent" & Mileage == "FiftyThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Normal" & Mileage == "FiftyThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Poor" & Mileage == "FiftyThou"),event = (Accident == "Severe"))

cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Excellent" & Mileage == "FiveThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Normal" & Mileage == "FiveThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Poor" & Mileage == "FiveThou"),event = (Accident == "Severe"))

cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Excellent" & Mileage == "TwentyThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Normal" & Mileage == "TwentyThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "False" & DrivQuality == "Poor" & Mileage == "TwentyThou"),event = (Accident == "Severe"))

####
cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Excellent" & Mileage == "Domino"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Normal" & Mileage == "Domino"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Poor" & Mileage == "Domino"),event = (Accident == "Severe"))

cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Excellent" & Mileage == "FiftyThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Normal" & Mileage == "FiftyThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Poor" & Mileage == "FiftyThou"),event = (Accident == "Severe"))

cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Excellent" & Mileage == "FiveThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Normal" & Mileage == "FiveThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Poor" & Mileage == "FiveThou"),event = (Accident == "Severe"))

cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Excellent" & Mileage == "TwentyThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Normal" & Mileage == "TwentyThou"),event = (Accident == "Severe"))
cpquery(fitted.hc3,evidence = (Antilock == "True" & DrivQuality == "Poor" & Mileage == "TwentyThou"),event = (Accident == "Severe"))

###################################################################
# Ante un robo de un auto, ¿en qué medida afecta el valor del auto, 
# la presencia de un sistema antirobos y la ubicacion del hogar?

cpquery(fitted.hc3,evidence = (HomeBase == "City" & CarValue == "FiftyThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "City" & CarValue == "FiveThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "City" & CarValue == "Million" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "City" & CarValue == "TenThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "City" & CarValue == "TwentyThou" & AntiTheft == "False"),event = (Theft == "True"))

cpquery(fitted.hc3,evidence = (HomeBase == "City" & CarValue == "FiftyThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "City" & CarValue == "FiveThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "City" & CarValue == "Million" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "City" & CarValue == "TenThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "City" & CarValue == "TwentyThou" & AntiTheft == "True"),event = (Theft == "True"))

cpquery(fitted.hc3,evidence = (HomeBase == "Rural" & CarValue == "FiftyThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Rural" & CarValue == "FiveThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Rural" & CarValue == "Million" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Rural" & CarValue == "TenThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Rural" & CarValue == "TwentyThou" & AntiTheft == "False"),event = (Theft == "True"))

cpquery(fitted.hc3,evidence = (HomeBase == "Rural" & CarValue == "FiftyThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Rural" & CarValue == "FiveThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Rural" & CarValue == "Million" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Rural" & CarValue == "TenThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Rural" & CarValue == "TwentyThou" & AntiTheft == "True"),event = (Theft == "True"))

cpquery(fitted.hc3,evidence = (HomeBase == "Secure" & CarValue == "FiftyThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Secure" & CarValue == "FiveThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Secure" & CarValue == "Million" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Secure" & CarValue == "TenThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Secure" & CarValue == "TwentyThou" & AntiTheft == "False"),event = (Theft == "True"))

cpquery(fitted.hc3,evidence = (HomeBase == "Secure" & CarValue == "FiftyThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Secure" & CarValue == "FiveThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Secure" & CarValue == "Million" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Secure" & CarValue == "TenThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Secure" & CarValue == "TwentyThou" & AntiTheft == "True"),event = (Theft == "True"))

cpquery(fitted.hc3,evidence = (HomeBase == "Suburb" & CarValue == "FiftyThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Suburb" & CarValue == "FiveThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Suburb" & CarValue == "Million" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Suburb" & CarValue == "TenThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Suburb" & CarValue == "TwentyThou" & AntiTheft == "False"),event = (Theft == "True"))

cpquery(fitted.hc3,evidence = (HomeBase == "Suburb" & CarValue == "FiftyThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Suburb" & CarValue == "FiveThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Suburb" & CarValue == "Million" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Suburb" & CarValue == "TenThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(fitted.hc3,evidence = (HomeBase == "Suburb" & CarValue == "TwentyThou" & AntiTheft == "True"),event = (Theft == "True"))

###
cpquery(fitted.hc3, evidence = (Age == "Adolescent"), event = (MedCost == "HundredThou"))
cpquery(fitted.hc3, evidence = (Age == "Adult"), event = (MedCost == "HundredThou"))
cpquery(fitted.hc3, evidence = (Age == "Senior"), event = (MedCost == "HundredThou"))

cpquery(bif, evidence = (Age == "Adolescent"), event = (MedCost == "HundredThou"))
cpquery(bif, evidence = (Age == "Adult"), event = (MedCost == "HundredThou"))
cpquery(bif, evidence = (Age == "Senior"), event = (MedCost == "HundredThou"))


cpquery(fitted.hc3, evidence = (Age == "Adolescent"), event = (PropCost == "Million"))
cpquery(fitted.hc3, evidence = (Age == "Adult"), event = (PropCost == "Million"))
cpquery(fitted.hc3, evidence = (Age == "Senior"), event = (PropCost == "Million"))

cpquery(bif, evidence = (Age == "Adolescent"), event = (PropCost == "Million"))
cpquery(bif, evidence = (Age == "Adult"), event = (PropCost == "Million"))
cpquery(bif, evidence = (Age == "Senior"), event = (PropCost == "Million"))


cpquery(fitted.hc3, evidence = (Age == "Adolescent"), event = (ILiCost == "Million"))
cpquery(fitted.hc3, evidence = (Age == "Adult"), event = (ILiCost == "Million"))
cpquery(fitted.hc3, evidence = (Age == "Senior"), event = (ILiCost == "Million"))

cpquery(bif, evidence = (Age == "Adolescent"), event = (ILiCost == "Million"))
cpquery(bif, evidence = (Age == "Adult"), event = (ILiCost == "Million"))
cpquery(bif, evidence = (Age == "Senior"), event = (ILiCost == "Million"))


cpquery(fitted.hc3, evidence = (Age == "Adolescent"), event = (DrivHist == "Many"))
cpquery(fitted.hc3, evidence = (Age == "Adult"), event = (DrivHist == "Many"))
cpquery(fitted.hc3, evidence = (Age == "Senior"), event = (DrivHist == "Many"))

cpquery(bif, evidence = (Age == "Adolescent"), event = (DrivHist == "Many"))
cpquery(bif, evidence = (Age == "Adult"), event = (DrivHist == "Many"))
cpquery(bif, evidence = (Age == "Senior"), event = (DrivHist == "Many"))


cpquery(fitted.hc3, evidence = (Age == "Adolescent"), event = (RiskAversion == "Adventurous"))
cpquery(fitted.hc3, evidence = (Age == "Adult"), event = (RiskAversion == "Adventurous"))
cpquery(fitted.hc3, evidence = (Age == "Senior"), event = (RiskAversion == "Adventurous"))

cpquery(bif, evidence = (Age == "Adolescent"), event = (RiskAversion == "Adventurous"))
cpquery(bif, evidence = (Age == "Adult"), event = (RiskAversion == "Adventurous"))
cpquery(bif, evidence = (Age == "Senior"), event = (RiskAversion == "Adventurous"))


cpquery(fitted.hc3, evidence = (Age == "Adolescent"), event = (Accident == "Severe"))
cpquery(fitted.hc3, evidence = (Age == "Adult"), event = (Accident == "Severe"))
cpquery(fitted.hc3, evidence = (Age == "Senior"), event = (Accident == "Severe"))

cpquery(bif, evidence = (Age == "Adolescent"), event = (Accident == "Severe"))
cpquery(bif, evidence = (Age == "Adult"), event = (Accident == "Severe"))
cpquery(bif, evidence = (Age == "Senior"), event = (Accident == "Severe"))


cpquery(bif,evidence = (HomeBase == "City" & CarValue == "FiftyThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "City" & CarValue == "FiveThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "City" & CarValue == "Million" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "City" & CarValue == "TenThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "City" & CarValue == "TwentyThou" & AntiTheft == "False"),event = (Theft == "True"))

cpquery(bif,evidence = (HomeBase == "City" & CarValue == "FiftyThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "City" & CarValue == "FiveThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "City" & CarValue == "Million" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "City" & CarValue == "TenThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "City" & CarValue == "TwentyThou" & AntiTheft == "True"),event = (Theft == "True"))

cpquery(bif,evidence = (HomeBase == "Rural" & CarValue == "FiftyThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Rural" & CarValue == "FiveThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Rural" & CarValue == "Million" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Rural" & CarValue == "TenThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Rural" & CarValue == "TwentyThou" & AntiTheft == "False"),event = (Theft == "True"))

cpquery(bif,evidence = (HomeBase == "Rural" & CarValue == "FiftyThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Rural" & CarValue == "FiveThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Rural" & CarValue == "Million" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Rural" & CarValue == "TenThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Rural" & CarValue == "TwentyThou" & AntiTheft == "True"),event = (Theft == "True"))

cpquery(bif,evidence = (HomeBase == "Secure" & CarValue == "FiftyThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Secure" & CarValue == "FiveThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Secure" & CarValue == "Million" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Secure" & CarValue == "TenThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Secure" & CarValue == "TwentyThou" & AntiTheft == "False"),event = (Theft == "True"))

cpquery(bif,evidence = (HomeBase == "Secure" & CarValue == "FiftyThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Secure" & CarValue == "FiveThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Secure" & CarValue == "Million" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Secure" & CarValue == "TenThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Secure" & CarValue == "TwentyThou" & AntiTheft == "True"),event = (Theft == "True"))

cpquery(bif,evidence = (HomeBase == "Suburb" & CarValue == "FiftyThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Suburb" & CarValue == "FiveThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Suburb" & CarValue == "Million" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Suburb" & CarValue == "TenThou" & AntiTheft == "False"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Suburb" & CarValue == "TwentyThou" & AntiTheft == "False"),event = (Theft == "True"))

cpquery(bif,evidence = (HomeBase == "Suburb" & CarValue == "FiftyThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Suburb" & CarValue == "FiveThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Suburb" & CarValue == "Million" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Suburb" & CarValue == "TenThou" & AntiTheft == "True"),event = (Theft == "True"))
cpquery(bif,evidence = (HomeBase == "Suburb" & CarValue == "TwentyThou" & AntiTheft == "True"),event = (Theft == "True"))

