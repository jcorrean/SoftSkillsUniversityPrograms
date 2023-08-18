load("~/Documents/GitHub/SoftSkillsUniversityPrograms/DataForFigure4.RData")
rm(list=setdiff(ls(), "TODAS"))
TODAS$keyword
softskills <- data.frame(table(TODAS$keyword))
softskills$Var1
TODAS[TODAS=="acercar"] <- 1
TODAS[TODAS=="analizar"] <- 2
TODAS[TODAS=="argumentar"] <- 3
TODAS[TODAS=="ayudar"] <- 4
TODAS[TODAS=="cambiar"] <- 5
TODAS[TODAS=="compartir"] <- 6
TODAS[TODAS=="competir"] <- 7
TODAS[TODAS=="comprender"] <- 8
TODAS[TODAS=="comprometerse"] <- 9
TODAS[TODAS=="comunicar"] <- 10
TODAS[TODAS=="conflictos"] <- 11
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

# "acercar" 1 # Bring closer
# "analizar" 2 # Analytical
# "argumentar" 3 # Argue
# "ayudar" 4 # Helping others
# "cambiar" 5 # Change
# "compartir" 6 # Sharing
# "competir" 7 # Competitiveness
# "comprender" 8 # Understanding
# "comprometerse" 9 # Commit
# "comunicar" 10 # Communication
# "conflictos" 11 # Conflict
# "controlar" 12  # Controlling
# "crear" 13 # Create
# "creatividad" 14 # Creativity
# "decidir" 15 # Decide
# "dirigir" 16 # Addressing
# "empatía" 17 # Empathy
# "equipos" 18 # Teamwork
# "ético" 19 # Ethical Thinking
# "evaluar" 20 # Evaluate
# "flexibilidad" 21 # Flexibility
# "fomentar" 22 # Foment
# "fortalecer" 23 # Strength
# "generar" 24 # Generate
# "gestionar" 25 # Management
# "identificar" 26 # Identify
# "impulsar" 27 # Thrust
# "innovar" 28 # Innovate
# "interactuar" 29 # Social Interaction
# "liderar" 30 # Leadership
# "manifestar" 31 # Expose
# "motivar" 32 # Motivate
# "orientar" 33 # Guidance 
# "pensamiento crítico" 34 # Critical thinking
# "persuasión" 35 # Persuasiveness
# "planificar" 36 # Planning
# "reconocer" 37 # Acknowledge
# "reflexionar" 38 # Thoughtfulness
# "resolver" 39 # Solving
# "respetar" <- 40 # respect
# "responsable" # Accountability
# "solucionar problemas"  # solving problems
# "tomar decisiones" # Decision making