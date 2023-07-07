load("~/Documents/GitHub/SoftSkillsUniversityPrograms/DataForFigure4.RData")
rm(list=setdiff(ls(), "TODAS"))
TODAS$keyword
softskills <- data.frame(table(TODAS$keyword))
softskills$Var1
TODAS[TODAS=="acercar"] <- 1 # Bring closer
TODAS[TODAS=="analizar"] <- 2 # Analytical
TODAS[TODAS=="argumentar"] <- 3 # Argue
TODAS[TODAS=="ayudar"] <- 4 # Helping others
TODAS[TODAS=="cambiar"] <- 5 # Change
TODAS[TODAS=="compartir"] <- 6 # Sharing
TODAS[TODAS=="competir"] <- 7 # Competitiveness
TODAS[TODAS=="comprender"] <- 8 # Understanding
TODAS[TODAS=="comprometerse"] <- 9 # Commit
TODAS[TODAS=="comunicar"] <- 10 # Communication
TODAS[TODAS=="conflictos"] <- 11 # Conflict
TODAS[TODAS=="controlar"] <- 12  # Controlling
TODAS[TODAS=="crear"] <- 13 # Create
TODAS[TODAS=="creatividad"] <- 14 # Creativity
TODAS[TODAS=="decidir"] <- 15 # Decide
TODAS[TODAS=="dirigir"] <- 16 # Addressing
TODAS[TODAS=="empatía"] <- 17 # Empathy
TODAS[TODAS=="equipos"] <- 18 # Teamwork
TODAS[TODAS=="ético"] <- 19 # Ethical Thinking
TODAS[TODAS=="evaluar"] <- 20 # Evaluate
TODAS[TODAS=="flexibilidad"] <- 21 # Flexibility
TODAS[TODAS=="fomentar"] <- 22 # Foment
TODAS[TODAS=="fortalecer"] <- 23 # Strength
TODAS[TODAS=="generar"] <- 24 # Generate
TODAS[TODAS=="gestionar"] <- 25 # Management
TODAS[TODAS=="identificar"] <- 26 # Identify
TODAS[TODAS=="impulsar"] <- 27 # Thrust
TODAS[TODAS=="innovar"] <- 28 # Innovate
TODAS[TODAS=="interactuar"] <- 29 # Social Interaction
TODAS[TODAS=="liderar"] <- 30 # Leadership
TODAS[TODAS=="manifestar"] <- 31 # Expose
TODAS[TODAS=="motivar"] <- 32 # Motivate
TODAS[TODAS=="orientar"] <- 33 # Guidance 
TODAS[TODAS=="pensamiento crítico"] <- 34 # Critical thinking
TODAS[TODAS=="persuasión"] <- 35 # Persuasiveness
TODAS[TODAS=="planificar"] <- 36 # Planning
TODAS[TODAS=="reconocer"] <- 37 # Acknowledge
TODAS[TODAS=="reflexionar"] <- 38 # Thoughtfulness
TODAS[TODAS=="resolver"] <- 39 # Solving
TODAS[TODAS=="respetar"] <- 40 # respect
TODAS[TODAS=="responsable"] <- 41 # Accountability
TODAS[TODAS=="solucionar problemas"] <- 42 # solving problems
TODAS[TODAS=="tomar decisiones"] <- 43 # Decision making