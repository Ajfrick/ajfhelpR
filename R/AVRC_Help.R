# EDI Classes
Edi_classA = c("A-1.0",   "A-2.0",   "A-3.0",   "A-3.1")
Edi_classE = c("E-1.0A",  "E-1.0B",  "E-1.0C",  "E-2.0",   "E-2.0A" , "E-2.0B")
Edi_classes = c(Edi_classA,Edi_classE,"E-3.0")

Aeh70 = c("A-1.0",   "A-2.0",   "A-3.0",   "A-3.1" ,"E-1.0A") # <70 day EDI Classes
Aeh133 = c(Aeh70, "E-1.0B")                                   # <133day EDI Classes

# File Paths

# Dates
PIRC.PreSTI = as.Date("2010-01-01")
PIRC.PreDrug_Q = as.Date("2008-07-01")
