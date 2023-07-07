load("~/Documents/GitHub/SoftSkillsUniversityPrograms/DataForFigure4.RData")
rm(list=setdiff(ls(), "TODAS"))
TODAS$keyword
softskills <- data.frame(table(TODAS$keyword))
softskills$Var1
TODAS[TODAS=="acercar"] <- 1 #bring closer
TODAS[TODAS=="analizar"] <- 2 #analyze
TODAS[TODAS=="argumentar"] <- 3 #argue
TODAS[TODAS=="ayudar"] <- 4 #help
TODAS[TODAS=="cambiar"] <- 5 #change
TODAS[TODAS=="compartir"] <- 6 #share
TODAS[TODAS=="competir"] <- 7 #compete
TODAS[TODAS=="comprender"] <- 8 #understand
TODAS[TODAS=="comprometerse"] <- 9 #commit
TODAS[TODAS=="comunicar"] <- 10 #communicate
TODAS[TODAS=="conflictos"] <- 11 #conflict
TODAS[TODAS=="controlar"] <- 12
TODAS[TODAS=="crear"] <- 13
TODAS[TODAS=="creatividad"] <- 14
TODAS[TODAS=="decidir"] <- 15
TODAS[TODAS=="dirigir"] <- 16
TODAS[TODAS=="empatía"] <- 17
TODAS[TODAS=="equipos"] <- 18
TODAS[TODAS=="ético"] <- 19
TODAS[TODAS=="evaluar"] <- 20
TODAS[TODAS=="flexibilidad"] <- 21
TODAS[TODAS=="fomentar"] <- 22
TODAS[TODAS=="fortalecer"] <- 23
TODAS[TODAS=="generar"] <- 24
TODAS[TODAS=="gestionar"] <- 25
TODAS[TODAS=="identificar"] <- 26
TODAS[TODAS=="impulsar"] <- 27
TODAS[TODAS=="innovar"] <- 28
TODAS[TODAS=="interactuar"] <- 29
TODAS[TODAS=="liderar"] <- 30
TODAS[TODAS=="manifestar"] <- 31
TODAS[TODAS=="motivar"] <- 32
TODAS[TODAS=="orientar"] <- 33
TODAS[TODAS=="pensamiento crítico"] <- 34
TODAS[TODAS=="persuasión"] <- 35
TODAS[TODAS=="planificar"] <- 36
TODAS[TODAS=="reconocer"] <- 37
TODAS[TODAS=="reflexionar"] <- 38
TODAS[TODAS=="resolver"] <- 39
TODAS[TODAS=="respetar"] <- 40
TODAS[TODAS=="responsable"] <- 41
TODAS[TODAS=="solucionar problemas"] <- 42
TODAS[TODAS=="tomar decisiones"] <- 43