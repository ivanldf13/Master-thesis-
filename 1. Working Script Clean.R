# Loading useful packages ----
library(dplyr)
library(ggplot2)
library(quanteda)
library(readtext)
library(spacyr)
library(stringr)
library(tidyr)
library(tidytext)
library(tm)
library(xlsx)
library(ggpage)



# Retrieving and reading the documents to be used ----
Corpus <- readtext("./AR.TXT/*.pdf") 
# data.txt <- readtext("./AR.TXT/*.txt")
# data <- rbind(data.pdf, data.txt)
# rm(data.pdf, data.txt)

save(Corpus, file="Corpus.Rda")
rm(data.pdf)
rm(data)

# Load saved data ----
load("Corpus.Rda")

# Create the Stopwords (en) objects ----
stop <- tibble(words = stopwords::data_stopwords_stopwordsiso$en)
# Changing the type of variable for doc_id

Corpus <- Corpus %>% 
  mutate(Year = str_match(string = doc_id, "\\d{4}"),
         Year = as.numeric(Year)) %>% 
  select(-doc_id)
# Cleaning the most salient things to erase, unless punctuation ----
# data.clean is the file before the unnest function, with the 1st layer of
# cleaning (words wrongly written) & lower cases
data.clean.no.punct <- Corpus  %>% 
  mutate(doc_id = str_replace_all(doc_id, "Annual-Report-", "AR-"), 
         doc_id = str_remove_all(doc_id, ".pdf")) %>% 
  mutate(text = str_remove_all(text,"\n"),
         text = str_remove_all(text, "[:punct:]"),
         text = str_remove_all(text, "[:digit:]"),
         text = tolower(text),
         text = str_remove_all(text,"\\^we the rockefeller foundation$"),
         text = str_remove_all(text, "©"),
         text = str_replace_all(text, "univer\\s{1,5}sity", "university"),
         text = str_replace_all(text, "univer . sity", "university"),
         text = str_replace_all(text, "zationseffort", "zations effort"),
         text = str_replace_all(text, "organi zation", "organization"),
         text = str_replace_all(text, "organi . zation", "organization"),
         text = str_replace_all(text, "moderni zation", "modernization"),
         text = str_replace_all(text, "organi zations", "organizations"),
         text = str_replace_all(text, "organi usa zation", "organization"),
         text = str_replace_all(text, " depart ments", "departmens"),
         text = str_replace_all(text, "cit ity", "city"),
         text = str_replace_all(text, "captiv ity", "captivity"),
         text = str_replace_all(text, "qual ity", "quality"),
         text = str_replace_all(text, "activ ity", "activity"), 
         text = str_replace_all(text, "appropria tions", "appropriations"),
         text = str_replace_all(text, "appro[:space:]priations", "appropriations"),
         text = str_replace_all(text, "appro\\n{1,5}priations", "appropriations"),
         text = str_replace_all(text, "ac priations", "appropriations"), 
         text = str_replace_all(text, "ag priations", "appropriations"), 
         text = str_replace_all(text, "aq priations", "appropriations"), 
         text = str_remove_all(text, "^rockefeller foundation$"),
         text = str_replace_all(text, "paulodepart", "paulo depart"),
         text = str_replace_all(text, "researchdepart", "research depart"),
         text = str_replace_all(text, "adjust j", "adjust"),
         text = str_replace_all(text, "equip p", "equip"),
         text = str_replace_all(text, "equip j", "equip"),
         text = str_replace_all(text, "equip o", "equip"),
         text = str_replace_all(text, "depart o", "depart"),
         text = str_replace_all(text, "depart j", "depart"),
         text = str_replace_all(text, "nonallgn", "non alligne"),
         text = str_replace_all(text, "govern ment", "government"),
         text = str_replace_all(text, "govern ments", "governments"),
         text = str_replace_all(text, "depart ment","department"), 
         text = str_replace_all(text, "depart ments","departments"),
         text = str_replace_all(text, "treat ments","treatments"),
         text = str_replace_all(text, "equip ment","equipment"),
         text = str_replace_all(text, "equip ments","equipments"),
         text = str_replace_all(text, "develop ment","development"),
         text = str_replace_all(text, " develop ments","developments"),
         text = str_replace_all(text, "require ment","requirement"),
         text = str_replace_all(text, "require ments","requirements"),
         text = str_replace_all(text, "move ment","movement"),
         text = str_replace_all(text, "move ments","movements"),
         text = str_replace_all(text, "improve ment","improvement"),
         text = str_replace_all(text, "improve ments","improvements"),
         text = str_replace_all(text, "endow ment","endowment"),
         text = str_replace_all(text, "endow ments","endowments"),
         text = str_replace_all(text, "employ ment","employment"),
         text = str_replace_all(text, "install ment","installment"),
         text = str_replace_all(text, "install ments","installments"),
         text = str_replace_all(text, "agree ment","agreement"),
         text = str_replace_all(text, "agree ments","agreements"),
         text = str_replace_all(text, "establish ment","establisment"),
         text = str_replace_all(text, "disburse ment","disbursement"),
         text = str_replace_all(text, "disburse ments","disbursements"),
         text = str_replace_all(text, "adjust ment","adjustment"),
         text = str_replace_all(text, "adjust ments","adjustments"),
         text = str_replace_all(text, "supple ment","supplement"),
         text = str_replace_all(text, "supple ments","supplements"),
         text = str_replace_all(text, "experi ments","experiments"),
         text = str_replace_all(text, "settle ments","settlements"),
         text = str_replace_all(text, "experi ment","experiment"),
         text = str_replace_all(text, "pay ments","payments"),
         text = str_replace_all(text, "invest ments","investments"),
         text = str_replace_all(text, "ele ment","elements"),
         text = str_replace_all(text, "achieve ments","achievements"),
         text = str_replace_all(text, "commit ments","commitments"),
         text = str_replace_all(text, "appoint ments","appointments"),
         text = str_replace_all(text, "docu ments","documents"),
         text = str_replace_all(text, "arrange ments","arragements"),
         text = str_replace_all(text, " assign ments","assignements"),
         text = str_replace_all(text, "education cation","education"),
         text = str_replace_all(text, "communi cation","communication"),
         text = str_replace_all(text, "appli cation","application"),
         text = str_replace_all(text, "appli cations","applications"),
         text = str_replace_all(text, "classifi cation","classification"),
         text = str_replace_all(text, "publi cation","publication"),
         text = str_replace_all(text, "telecommun", "telecommuni"),
         text = str_replace_all(text, "telecommuni cations", "telecommunications"),
         text = str_replace_all(text, "qualifi cations", "qualifications"),
         text = str_replace_all(text, "communi cations", "communications"),
         text = str_replace_all(text, "devel opment", "development"),
         text = str_replace_all(text, "emergedthe", "emerged the"),
         text = str_replace_all(text, "worldpopu", "world popu"),
         text = str_replace_all(text, "popu lation", "population"),
         text = str_replace_all(text, "overpopu lation", "overpopulation"),
         text = str_replace_all(text, "trans lation", "translation"),
         text = str_replace_all(text, "re lation", "relation"),
         text = str_replace_all(text, "inocu lation", "inoculation"),
         text = str_replace_all(text, "iso lation", "isolation"),
         text = str_replace_all(text, "specu lation", "speculation"),
         text = str_replace_all(text, "comis j", "comis"),
         text = str_replace_all(text, "inthe", "in the"),
         text = str_replace_all(text, "commis sion", "commission"),
         text = str_replace_all(text, "commis sions", "commissions"),
         text = str_replace_all(text, "comprehen sion", "comprehension"),
         text = str_replace_all(text, "deci sion", "decision"),
         text = str_replace_all(text, "divi sion", "division"),
         text = str_replace_all(text, "divi sions", "divisions"),
         text = str_replace_all(text, "profe sion", "profesion"),
         text = str_replace_all(text, "profe sions", "profesions"),
         text = str_replace_all(text, "man sion", "mansion"),
         text = str_replace_all(text, "supervi sion", "supervision"),
         text = str_replace_all(text, "provi sion", "provision"),
         text = str_replace_all(text, "promis sion", "promission"),
         text = str_replace_all(text, "mis sion", "mission"),
         text = str_replace_all(text, "mis sions", "missions"),
         text = str_replace_all(text, "pen sions", "pensions"),
         text = str_replace_all(text, "exten sions", "extensions"),
         text = str_replace_all(text, "discu sions", "discusions"),
         text = str_replace_all(text, "foun a", "foun"),
         text = str_replace_all(text, "foun i", "foun"),
         text = str_replace_all(text, "foun dation", "foundation"),
         text = str_replace_all(text, "foun dations", "foundations"),
         text = str_replace_all(text, "accomo dations", "accomodations"),
         text = str_replace_all(text, "de velopment", "development"),
         text = str_replace_all(text, "de velopments", "developments"),
         text = str_replace_all(text, "con struction", "construction"),
         text = str_replace_all(text, "recon struction", "reconstruction"),
         text = str_replace_all(text, "oo structions", "obnstructions"),
         text = str_replace_all(text, "indebted a", "indebted"),
         text = str_replace_all(text, "indebted k", "indebted"),
         text = str_replace_all(text, "feebleminded ness", "feebleminded nes"),
         text = str_replace_all(text, "andinterrelated", "and interrelated"),
         text = str_replace_all(text, "readi ness", "readiness"),
         text = str_replace_all(text, "dizi ness", "diziness"),
         text = str_replace_all(text, "blind ness", "blindness"),
         text = str_replace_all(text, "sick ness", "sickness"),
         text = str_replace_all(text, "busi ness", "business"),
         text = str_replace_all(text, "busi nesses", "businesses"),
         text = str_replace_all(text, "sensitive ness", "sensitiveness"),
         text = str_replace_all(text, "indebted ness", "indebtedness"),
         text = str_replace_all(text, "feebleminded ness", "feeblemindedness"),
         text = str_replace_all(text, "effective ness", "effectiveness"),
         text = str_replace_all(text, "poor ness", "poorness"),
         text = str_replace_all(text, "interrelated ness", "interrelatedness"),
         text = str_replace_all(text, "care ness", "careness"),
         text = str_replace_all(text, "aware ness", "awareness"),
         text = str_replace_all(text, "weak nesses", "weaknesses"),
         text = str_replace_all(text, "internship nesses", "internshipnesses"),
         text = str_replace_all(text, "com q", "com"),
         text = str_replace_all(text, "com munity", "comunity"),
         text = str_replace_all(text, "com munities", "comunities"),
         text = str_replace_all(text, "im munity", "immunity"),
         text = str_replace_all(text, "corpo ~", "corpo"),
         text = str_replace_all(text, "corpo ration", "corporations"),
         text = str_replace_all(text, "prepa ration", "preparation"),
         text = str_replace_all(text, "explo ration", "exploration"),
         text = str_replace_all(text, "adminis > j", "adminis"),
         text = str_replace_all(text, "adminis tration", "administration"),
         text = str_replace_all(text, "concen tration", "concentration"),
         text = str_replace_all(text, "concen trations", "concentrations"),
         text = str_replace_all(text, "regis tration", "registration"),
         text = str_replace_all(text, "interna tional", "international"),
         text = str_replace_all(text, "addi tional", "additional"),
         text = str_replace_all(text, "agricul tural", "agricultural"),
         text = str_replace_all(text, "cul tural", "cultural"),
         text = str_replace_all(text, "culnj tural", "cultural"),
         text = str_replace_all(text, "struc tural", "structural"),
         text = str_replace_all(text, "corpoi", "corpor"),
         text = str_replace_all(text, "oper >", "oper"),
         text = str_replace_all(text, "collaboi", "collabor"),
         text = str_replace_all(text, "situ ation", "situation"),
         text = str_replace_all(text, "consider ation", "consideraation"),
         text = str_replace_all(text, "consider ations", "considerations"),
         text = str_replace_all(text, "appropi ation", "appropriation"),
         text = str_replace_all(text, "demobiliz ation", "demobilization"),
         text = str_replace_all(text, "oper ation", "operation"),
         text = str_replace_all(text, "associ ation", "association"),
         text = str_replace_all(text, "found ation", "foundation"),
         text = str_replace_all(text, "cooper ation", "cooperation"),
         text = str_replace_all(text, "programm ation", "programmation"),
         text = str_replace_all(text, "oper ation", "operation"),
         text = str_replace_all(text, "oper ations", "operations"),
         text = str_replace_all(text, "radi ation", "radiation"),
         text = str_replace_all(text, "cre ation", "creaation"),
         text = str_replace_all(text, "tax ation", "taxation"),
         text = str_replace_all(text, "feder ation", "federation"),
         text = str_replace_all(text, "ap propri", "appropri"),
         text = str_replace_all(text, "fund ations", "fundations"),
         text = str_replace_all(text, "populationappropri", "population appropri"),
         text = str_replace_all(text, "limi tation", "limitation"),
         text = str_replace_all(text, "sani tation", "sanitation"),
         text = str_replace_all(text, "vege tation", "vegetation"),
         text = str_replace_all(text, "infes tation", "infestation"),
         text = str_replace_all(text, "documen tation", "documentation"),
         text = str_replace_all(text, "compu tation", "computation"),
         text = str_replace_all(text, "compu tations", "computations"),
         text = str_replace_all(text, "transpor tation", "transportation"),
         text = str_replace_all(text, "interpre tation", "interpretation"),
         text = str_replace_all(text, "rehabili tation", "rehabilitation"),
         text = str_replace_all(text, "admin istration", "administration"),
         text = str_replace_all(text, "commun ity", "community"),
         text = str_replace_all(text, "calam ity", "calamity"),
         text = str_replace_all(text, "hered ity", "heredity"),
         text = str_replace_all(text, "produc ity", "productvity"),
         text = str_replace_all(text, "valid ity", "validity"),
         text = str_replace_all(text, "qual ity", "quality"),
         text = str_replace_all(text, "jniver ~", "univers"),
         text = str_replace_all(text, "univers ity", "university"),
         text = str_replace_all(text, "ca lamities", "calamities"),
         text = str_replace_all(text, "accaountantfacil", "accaountant facil "),
         text = str_replace_all(text, "valid ity", "validity"),
         text = str_replace_all(text, "objectiv ity", "objectivity"),
         text = str_replace_all(text, "fertil ity", "fertility"),
         text = str_replace_all(text, "facil ity", "facility"),
         text = str_replace_all(text, "insular ity", "insularity"),
         text = str_replace_all(text, "investiga tion", "investigation"),
         text = str_replace_all(text, "investiga tions", "investigations"),
         text = str_replace_all(text, "popu atlon", "population"),
         text = str_replace_all(text, "environmental ity", "environmentality"),
         text = str_replace_all(text, "expendi tures", "expenditures"),
         text = str_replace_all(text, "sci ence", "science"),
         text = str_replace_all(text, "influ ence", "influence"),
         text = str_replace_all(text, "confe rence", "conference"),
         text = str_replace_all(text, "refer ence", "reference"),
         text = str_replace_all(text, "independ ence", "independence"),
         text = str_replace_all(text, "refer ence", "reference"),
         text = str_replace_all(text, "expendi ", "expenditures"),
         text = str_replace_all(text, "cd room", "cd-room"),
         text = str_replace_all(text, "nce", "rice"),
         text = str_replace_all(text, "viricent", "vincent"),
         text = str_replace_all(text, "organizuion", "organization"),
         text = str_replace_all(text, "autnoma", "autonoma"),
         text = str_replace_all(text, "ommunities", "communities"),
         text = str_replace_all(text, "ommunity", "community"),
         text = str_replace_all(text, "insurarice", "insurance"),
         text = str_replace_all(text, "ommunities", "communities"),
         text = str_replace_all(text, "resilierice", "resilience"),
         text = str_replace_all(text, "intracontmental", "intracontinental"),
         text = str_replace_all(text, "scierice", "science"),
         text = str_replace_all(text, "scierices", "sciences"),
         text = str_replace_all(text, "enharice", "enhace"),
         text = str_replace_all(text, "erice$", "ence"),
         text = str_replace_all(text, "californiaa", "california"),
         text = str_replace_all(text, "cccommunity", "community"),
         text = str_replace_all(text, "ccommunity", "community"),
         text = str_replace_all(text, "cccommunities", "communities"),
         text = str_replace_all(text, "ccommunities", "communities"),
         text = str_replace_all(text, "certifi cates", "certificates"),
         text = str_replace_all(text, "cer tificates", "certificates"),
         text = str_replace_all(text, "foundationtion", "foundation"),
         text = str_replace_all(text, "ofeducation", "of education"),
         text = str_replace_all(text, "inaddition", "in addition"),
         text = str_replace_all(text, "theadvancement", "the advancement"),
         text = str_replace_all(text, "rockfellerfoundation", "rockfeller foundation"),
         text = str_replace_all(text, "thedevelopment", "the development"),
         text = str_replace_all(text, "medicaleducation", "medical education"),
         text = str_replace_all(text, "healthdivision", "health division"),
         text = str_replace_all(text, "thedivision", "the division"),
         text = str_replace_all(text, "thedirection", "the direction"),
         text = str_replace_all(text, "thegovernment", "the government"),
         text = str_replace_all(text, "healthdepartment", "health department"),
         text = str_replace_all(text, "anappropriation", "an appropriation"),
         text = str_replace_all(text, "andequipment", "and equipment"),
         text = str_replace_all(text, "ofinfection", "of infection"),
         text = str_replace_all(text, "healtheducation", "health education"),
         text = str_replace_all(text, "healthorganization" , "health organization"),
         text = str_replace_all(text, "thecommission", "the commission"), 
         text = str_replace_all(text, "theestablishment", "the establishment"),
         text = str_replace_all(text, "thepopulation", "the population"),
         text = str_replace_all(text, "theimprovement", "the improvement"),
         text = str_replace_all(text, "thereproduction", "the reproduction"),
         text = str_replace_all(text, "theassociation", "the association"),
         text = str_replace_all(text, "publicadministration", "public administration"),
         text = str_replace_all(text, "ofdevelopment", "of development"),
         text = str_replace_all(text, "offoundation", "of foundation"),
         text = str_replace_all(text, "incollaboration", "in collaboration"),
         text = str_replace_all(text, "fordevelopment", "for development"),
         text = str_replace_all(text, "thetreatment", "the treatment"), 
         text = str_replace_all(text, "thepreparation", "the preparation"),
         text = str_replace_all(text, "anddistribution", "and distribution"),
         text = str_replace_all(text, "theadvancement", "the advancement"),
         text = str_replace_all(text, "highereducation", "higher education"), 
         text = str_replace_all(text, "incooperation", "in cooperation"), 
         text = str_replace_all(text, "theorganization", "the organization"),
         text = str_replace_all(text, "foodproduction", "food production"),
         text = str_replace_all(text, "afoundation", "a foundation"),
         text = str_replace_all(text, "thecorporation", "the corporation"),
         text = str_replace_all(text, "thedistribution", "the distribution"),
         text = str_replace_all(text, "soilpollution", "soil pollution"),
         text = str_replace_all(text, "communitydevelopment", "community development"),
         text = str_replace_all(text, "theintroduction", "the introduction"), 
         text = str_replace_all(text,"andtreatment", "and treatment"),
         text = str_replace_all(text, "thecreation", "the creation"),
         text = str_replace_all(text, "andgovernment", "and government"),
         text = str_replace_all(text, "aneducation", "an education"),
         text = str_replace_all(text, "thecooperation", "the cooperation"),
         text = str_replace_all(text, "withfoundation", "with foundation"),
         text = str_replace_all(text, "theinfection", "the infection"),
         text = str_replace_all(text, "healthadministration", "healthadministration"),
         text = str_replace_all(text, "theconstruction", "the construction"),
         text = str_replace_all(text, "hookworminfection", "hookworm infection"),
         text = str_replace_all(text, "theapplication", "the application"),
         text = str_replace_all(text, "economicdevelopment", "economic development"),
         text = str_replace_all(text, "theinvestigation", "the investigation"),
         text = str_replace_all(text, "reportfoundation", "report foundation"),
         text = str_replace_all(text, "thesolution", "the solution"),
         text = str_replace_all(text, "ofequipment", "of equipment"),
         text = str_replace_all(text, "nursingeducation", "nursing education"),
         text = str_replace_all(text, "oftreatment", "of treatment"),
         text = str_replace_all(text, "specialattention", "special attention"),
         text = str_replace_all(text, "healthcommission", "health commission"),
         text = str_replace_all(text, "sciencenutrition", "science nutrition"),
         text = str_replace_all(text, "thecompletion", "the completion"),
         text = str_replace_all(text, "andfixedequipment", "and fixed equipement"),
         text = str_replace_all(text, "ofnutrition", "of nutrition"),
         text = str_replace_all(text, "ruralreconstruction", "rural reconstruction"),
         text = str_replace_all(text, "complementfixation", "complement fixation"), 
         text = str_replace_all(text, "thestation", "the station"),
         text = str_replace_all(text, "saltflotation", "salt flotation"),
         text = str_replace_all(text, "ofinformation", "of informtion"),
         text = str_replace_all(text, "thepromotion", "the promotion"),
         text = str_replace_all(text, "theuniversity", "the university"),
         text = str_replace_all(text, "thefoundation", "the foundation"),
         text = str_replace_all(text, "thedepartment", "the department"),
         text = str_replace_all(text, "thefoundations", "the foundations"),
         text = str_replace_all(text, "highquality", "high quality"),
         text = str_replace_all(text, "perioduniversity", "period university"),
         text = str_replace_all(text, "thehumanities", "the humanities"),
         text = str_replace_all(text, "grantsuniversity", "grants university"),
         text = str_replace_all(text, "internationalrelations", "international relations"), 
         text = str_replace_all(text, "guniversity", "university"),
         text = str_replace_all(text, "thecity", "the city"),
         text = str_replace_all(text, "columbiauniversity", "columbia university"),
         text = str_replace_all(text, "hssuniversity", "hss university"),
         text = str_replace_all(text, "anddevelopment", "and development"),
         text = str_replace_all(text, "paymentsuniversities", "payments universities"),
         text = str_replace_all(text, "forfiveyears", "for five years"),
         text = str_replace_all(text, "infieldof", "in field of"),
         text = str_replace_all(text, "highyielding", "high yielding"),
         text = str_replace_all(text, "minoritygroup", "minority group"),
         text = str_replace_all(text, "univesitythe", "university the"),
         text = str_replace_all(text, "sciencebased", "science based"),
         text = str_replace_all(text, "foundationadministered", "foundation administered"),
         text = str_replace_all(text, "arthropodborne", "arthropod-borne"),
         text = str_replace_all(text, "nee", "rice"),
         text = str_replace_all(text, "hivaids", "hiv aids"),
         text = str_replace_all(text, "foundationscience", "foundation science"),
         text = str_replace_all(text, "tne", "the"),
         text = str_replace_all(text, "latrines", "latrine"),
         text = str_replace_all(text, "foundationuniversity", "foundation university"),
         text = str_replace_all(text, "wuniversity", "university"),
         text = str_replace_all(text, "otherexhibition", "other exhibition"),
         text = str_replace_all(text, "theappropriations", "the appropriations"),
         text = str_replace_all(text, "rockefellerfoundation", "rockefeller foundation"),
         text = str_replace_all(text, "harvarduniversity", "harvard university"),
         text = str_replace_all(text, "aboveuniversity", "above university"),
         text = str_replace_all(text, "ofpopulation", "of population"),
         text = str_replace_all(text, "foundationfoundation", "foundation"),
         text = str_replace_all(text, "generaleducation", "general education"),
         text = str_replace_all(text, "nonproliferation", "non-proliferation"),
         text = str_replace_all(text, "umversidad", "universidad"),
         text = str_replace_all(text, "foundationtable", "foundation table"),
         text = str_replace_all(text, "therockefeller", "the rockefeller"),
         text = str_replace_all(text, "periodunivkhsity", "period university"),
         text = str_replace_all(text, "colombiauniversity", "colombia university"),
         text = str_replace_all(text, "foundationcontents", "foundation contents"),
         text = str_replace_all(text, "brazillaboratory", "brazil laboratory"),
         text = str_replace_all(text, "foundationgrants", "foundation grants"),
         text = str_replace_all(text, "brazillaboratory", "brazil laboratory"),
         text = str_replace_all(text, "universitythe", "university the"),
         text = str_replace_all(text, "varietiestanzania", "varieties tanzania"),
         text = str_replace_all(text, "policieshuman", "policies human"),
         text = str_replace_all(text, "salaamtanzania", "salaam tanzania"),
         text = str_replace_all(text, "ascomprehensive", "as comprehensive"),
         text = str_replace_all(text, "salaamfrom", "salaam from"),
         text = str_replace_all(text, "characterizationbiotechnologyscnpps", 
                                "characterization biotechnology scnpps"),
         text = str_replace_all(text, "salaamfrom", "salaam from"),
         text = str_replace_all(text, "associatepatricia", "associate patricia"),
         text = str_replace_all(text, "associatejohn", "associate john"),
         text = str_replace_all(text, "biologyjudge", "biologyjudge"),
         text = str_replace_all(text, "europeanracial", "european racial"),
         text = str_replace_all(text, "mexicanamerican", "mexican american"),
         text = str_replace_all(text, "thegrants", "the grants"),
         text = str_replace_all(text, "availableto", "available to"),
         text = str_replace_all(text, "theeditors", "the editors"),
         text = str_replace_all(text, "fourfellowships", "four fellowships"),
         text = str_replace_all(text, "valuespririceton", "values princenton"),
         text = str_replace_all(text, "japaneseunited", "japanese united"),
         text = str_replace_all(text, "medicineuniversity", "medicine university"),
         text = str_replace_all(text, "facultyof", "faculty of"),
         text = str_replace_all(text, "veterinarymedicine", "veterinary medicine"),
         text = str_replace_all(text, "peruuniversity", "peru university"),
         text = str_replace_all(text, "biologygenetics", "biologoy genetics"),
         text = str_replace_all(text, "developmentsustainable", "development sustainable"),
         text = str_replace_all(text, "projectsupport", "project support"),
         text = str_replace_all(text, "autnoma", "autonoma"),
         text = str_replace_all(text, "darice", "dance"),
         text = str_replace_all(text, "entrarice", "entrance"),
         text = str_replace_all(text, "substarices", "substances"),
         text = str_replace_all(text, "importarice", "importance"),
         text = str_replace_all(text, "coricentrating", "concentrating"),
         text = str_replace_all(text, "periodjapanhokkaido", "period japan hokkaido"),
         text = str_replace_all(text, "japanhokkaido", "japan hokkaido"),
         text = str_replace_all(text, "sociologydemography", "sociology demography"),
         text = str_replace_all(text, "soilshokkaido", "soils hokkaido"),
         text = str_replace_all(text, "japangraduate", "japan graduate"),
         text = str_replace_all(text, "japanhague", "japan hague"),
         text = str_replace_all(text, "laboratoryteaching", "laboratory teaching"),
         text = str_replace_all(text, "hokkaidouniversity", "hokkaido university"),
         text = str_replace_all(text, "httpwwwhindustantimescomnewsfeedindiaindiacheersasmonsoonarriveshopesofbetterfarmoutputraisedarticleaspx", 
                                "http www hindustan times com news feed india india cheersas monsoon arrives hopes of better farm out put raised article aspx"),
         text = str_replace_all(text, "resiliericestrategies", "resilience strategies"),
         text = str_replace_all(text, "broadlyshared", "broadly shared"),
         text = str_replace_all(text, "economiesand", "economies and"),
         text = str_replace_all(text, "changewhile", "change while"),
         text = str_replace_all(text, "insurariceproducts", "insurance products"),
         text = str_replace_all(text, "themworldand", "them world an"),
         text = str_replace_all(text, "economicshocks", "economic shocks"),
         text = str_replace_all(text, "smallholder", "small holder"),
         text = str_replace_all(text, "climatechange", "climate change"),
         text = str_replace_all(text, "activitiesexhibition", "activities exhibition"),
         text = str_replace_all(text, "testimonyvernacular", "testimony vernacular"),
         text = str_replace_all(text, "collaborationand", "collaboration and"),
         text = str_replace_all(text, "technologyand", "technology and"),
         text = str_replace_all(text, "providechroreographic", "provide choreographic"),
         text = str_replace_all(text, "anyonesexpextation", "anyones expectation"),
         text = str_replace_all(text, "foundationphotograph", "foundation photograph"),
         text = str_replace_all(text, "specialscholarships", "special scholarships"),
         text = str_replace_all(text, "scholarshipassistarice", "scholarship assistance"),
         text = str_replace_all(text, "automaticdrips", "automatic drips"),
         text = str_replace_all(text, "studiesand", "studies and"),
         text = str_replace_all(text, "trainingbases", "training bases"),
         text = str_replace_all(text, "withmore", "with more"),
         text = str_replace_all(text, "whetherthey", "wether they"),
         text = str_replace_all(text, "withmore", "with more"),
         text = str_replace_all(text, "bloodforming", "blood forming"),
         text = str_replace_all(text, "basicbiological", "basic biological"),
         text = str_replace_all(text, "pathologicalthrough", "pathological through"),
         text = str_replace_all(text, "theblood", "the blood"),
         text = str_replace_all(text, "substariceswhich", "substances which"),
         text = str_replace_all(text, "arrangementshave", "arrangement shave"),
         text = str_replace_all(text, "statesintensive", "states intensive"),
         text = str_replace_all(text, "whichhave", "which have"), 
         text = str_replace_all(text, "southernstates", "southern states"), 
         text = str_replace_all(text, "ofinvestigations", "of investigations"),
         text = str_replace_all(text, "substariceswhich", "substances which"),
         text = str_replace_all(text, "morethat", "more that"),
         text = str_replace_all(text, "theirresponsibility", "their responsibility"),
         text = str_replace_all(text, "degreeof", "degree of"),
         text = str_replace_all(text, "entirerepublic", "entire republic"),
         text = str_replace_all(text, "effectivemethods", "effective methods"),
         text = str_replace_all(text, "theimportant", "the important"),
         text = str_replace_all(text, "averagedegree", "average degree"),
         text = str_replace_all(text, "governmentprivate", "government private"),
         text = str_replace_all(text, "educationagriculture", "education agriculture"),
         text = str_replace_all(text, "betterqualified", "better qualified"),
         text = str_replace_all(text, "andcommodities", "and commodities"),
         text = str_replace_all(text, "residualspraying", "residual spraying"),
         text = str_replace_all(text, "tworepresentative", "two representative"),
         text = str_replace_all(text, "toaccomplish", "to accomplish"),
         text = str_replace_all(text, "worldwideusefulness", "worldwide usefullness"),
         text = str_replace_all(text, "andeconomic", "and economic"),
         text = str_replace_all(text, "favorabletime", "favorable time"),
         text = str_replace_all(text, "methodthe", "method the"),
         text = str_replace_all(text, "authoritieshowever", "authorities however"),
         text = str_replace_all(text, "andeconomic", "and economic"),
         text = str_replace_all(text, "theinternational", "the internatonal"),
         text = str_replace_all(text, "tooperate", "to operate"),
         text = str_replace_all(text, "theressources", "the ressources"),
         text = str_replace_all(text, "decreasein", "decrease in"),
         text = str_replace_all(text, "morelimited", "more limited"),
         text = str_replace_all(text, "witheressources", "the ressources"),
         text = str_replace_all(text, "whichhas", "which has"),
         text = str_replace_all(text, "thecommittee", "the committee"),
         text = str_replace_all(text, "isan", "is an"),
         text = str_replace_all(text, "truethat", "true that"),
         text = str_replace_all(text, "theressources", "the ressources"),
         text = str_replace_all(text, "decadebecomes", "decade becomes"),
         text = str_replace_all(text, "socialsanitary", "social sanitary"),
         text = str_replace_all(text, "theirdistribution", "their distribution"),
         text = str_replace_all(text, "housingfamily", "housing family"),
         text = str_replace_all(text, "problemswhich", "problems which"),
         text = str_replace_all(text, "andalso", "and also"),
         text = str_replace_all(text, "ofrecognized", "of recognised"),
         text = str_replace_all(text, "thebureau", "the bureau"),
         text = str_replace_all(text, "byfor", "by for"),
         text = str_replace_all(text, "theeconomic", "the economic"),
         text = str_replace_all(text, "feasabilitypower", "feasability power"),
         text = str_replace_all(text, "forcollaborative", "for collaborative"),
         text = str_replace_all(text, "andrelationships", "and relationships"),
         text = str_replace_all(text, "foundationsystematically", "foundation systematically"),
         text = str_replace_all(text, "prospectthought", "prospect thought"),
         text = str_replace_all(text, "toinequality", "to inequality"),
         text = str_replace_all(text, "toincrease", "to increase"),
         text = str_replace_all(text, "andwomen", "and women"),
         text = str_replace_all(text, "selfsufficiency", "self sufficiency"),
         text = str_replace_all(text, "providessupport", "provides support"),
         text = str_replace_all(text, "agegroups", "age groups"),
         text = str_replace_all(text, "foundationintroductionthe", "foundation
                                introduction the"),
         text = str_replace_all(text, "proposedpersonal", "proposed personal"),
         text = str_replace_all(text, "scholarsphilanthropic", "scholars philanthropic"),
         text = str_replace_all(text, "noeconomic", "no economic"),
         text = str_replace_all(text, "foundationsupported", "foundation supported"),
         text = str_replace_all(text, "journalismeducationberkeley", 
                                "journalism education berkeley"),
         text = str_replace_all(text, "towardsits", "towards its"),
         text = str_replace_all(text, "numberof", "number of"),
         text = str_replace_all(text, "newspaperindustry", "newspaper industry economic"),
         text = str_replace_all(text, "robinsonfoundationnew", 
                                "robinson foundation new"),
         text = str_replace_all(text, "fundadevelopment", "fund a development"),
         text = str_replace_all(text, "citizensexpanding", "citizens expanding"),
         text = str_replace_all(text, "businesscollaboratweprogramsaimedat", 
                                "business collaborat we programs aimed at"),
         text = str_replace_all(text, "improvingyouth", "improving youth"),
         text = str_replace_all(text, "eemployment", "employment"),
         text = str_replace_all(text, "testedin", "tested in"),
         text = str_replace_all(text, "careeropportunities", "career opportunities"),
         text = str_replace_all(text, "theeconomic", "the economic"),
         text = str_replace_all(text, "foundationopportunities", "foundation opportunities"),
         text = str_replace_all(text, "developmentsaffecting", "developments affecting"),
         text = str_replace_all(text, "givingsupport", "giving support"),
         text = str_replace_all(text, "sciencesmathematics", "sciences mathematics"),
         text = str_replace_all(text, "iriception", "inception"),
         text = str_replace_all(text, "coricentrated", "concentrated"),
         text = str_replace_all(text, "coriception", "conception"),
         text = str_replace_all(text, "coricerned", "concerned"),
         text = str_replace_all(text, "sirice", "since"),
         text = str_replace_all(text, "coricentration", "concentration"),
         text = str_replace_all(text, "socialadvarice", "social advance"),
         text = str_replace_all(text, "comparativecommunity", "comparative community"),
         text = str_replace_all(text, "ofminorityyouth", "of minority youth"),
         text = str_replace_all(text, "ministration", "administration"),
         text = str_replace_all(text, "appf priation", "appropriation"),
         text = str_replace_all(text, "appro priation", "appropriation"),
         text = str_replace_all(text, "priation", "appropriation"),
         text = str_replace_all(text, "approapproriations", "appropriations"),
         text = str_replace_all(text, "approapproriation", "appropriation"),
         text = str_replace_all(text, "adadministration", "administration"),
         text = str_replace_all(text, "thenational", "the national"),
         text = str_replace_all(text, "internationalhealth", "international health"),
         text = str_replace_all(text, "publichealth", "public health"),
         text = str_replace_all(text, "thenational", "the national"),
         text = str_replace_all(text, "themedical", "the medical"),
         text = str_replace_all(text, "ofhealth", "of health"),
         text = str_replace_all(text, "thesocial", "the social"),
         text = str_replace_all(text, "interna tional", "international"),
         text = str_replace_all(text, "addi tional", "additional"),
         text = str_replace_all(text, "aroundadvarices", "around advances"),
         text = str_replace_all(text, "agricul tural", "agricultural"),
         text = str_replace_all(text, "conferencewas", "conference was"),
         text = str_replace_all(text, "conferencewith", "conference with"),
         text = str_replace_all(text, "influenceof", "influence of"),
         text = str_replace_all(text, "evidencethat", "evidence that"),
         text = str_replace_all(text, "residenceproperty", "residence property"),
         text = str_replace_all(text, "cul tural", "cultural"),
         text = str_replace_all(text, "culnj tural", "cultural"),
         text = str_replace_all(text, "struc tural", "structural"),
         text = str_replace_all(text, "struc ture", "structure"),
         text = str_replace_all(text, "organi zations", "organizations"))



# First, we see what class of object it is our file with class(object).

## The best class to see elements within an object is tibble ----
data.clean.no.punct <- tibble(data.clean.no.punct)
save(data.clean.no.punct, file = "data.clean.no.punct.Rda")

data.clean.punct <- tibble(data.clean.punct)
save(data.clean.punct, file = "data.clean.punct.Rda")
# 2ndA, we do the tokenisation of the corpus WITHOUT PUNCTUATION and the 2nd layer ----
# of cleaning (removing fragment of words, pseudowords and acronyms 
tokenised.no.punct <- data.clean.no.punct %>% 
  unnest_tokens("words", "text") %>% 
  filter(!(words %in% c("york", "rockefeller", 1913:2013, 1:10, "rf", "univ", "Rf",
                        "c.m", "dr", "cent", "m.d", "mns", "ph.d", "tion", "par",
                        "agric", "inst", "ing", "ap", "pre", "r.r", "pro",
                        "ooo", "d.c", "san", "coll", "tions", "dc", "acad", 
                        "cont", "mex", "ry", "ser", "cap", "med", "natl", "phd",
                        "med", "meph", "xiv", "xvi", "xv", "mada", "bmr", "ingagr",
                        "annual","report","ny", "wwwrockefellerfoundationorgwhat", 
                        "appointed", "programs", "table", "instalments", "remaining", 
                        "fig", "hc", "Ih", "|h", "lh", "percentage", "dh", "pc", 
                        "appropria", "mtg", "xix", "cort", "starr", "stoll", 
                        "vincent", "ova", "rr", "cum", "cons", "viii", "amino",
                        "rfs", "xvii", "xviii", "min", "xix", "sas", "xn", "los", 
                        "bsc", "shss", "qqq", "qqq", "qoo", "los", "iads", "nev", 
                        "inclen", "foi", "por", "Its", "llc", "rodin", "ix", "xii", 
                        "xiii", "xx", "xxii", "Ilc", "greene", "buttrick", "wickliffe", 
                        "ds", "dme", "apr", "barnard", "schillings", "vm", "xin", 
                        "neu", "sheldon", "pfeiffer", "gilardoni","ment", "ments", 
                        "opment", "sion", "tation", "ization", "struction", 
                        "istration", "velopment", "ation", "istration", "lation", 
                        "ration", "zation", "duction", "ganization", "mation",
                        "mentation", "mentation", "provement", "augment", "agement", 
                        "vention", "eration", "fection", "propriation",  "versity", 
                        "ments", "sity", "cation", "dation", "partment", "tration",
                        "sions", "munity", "ness", "ation", "ity","istration",
                        "lation", "divi", "nal", "popul", "educ", "tion", "ration", 
                        "ities","mention", "disserta", "tee", "ro", "cm", "per", 
                        "ref", "vn", "ls", "ga", "g", "c", "h", "dvm", "ms", 
                        "mrs","w", "ca", "http", "ns", "cd", "pp", "investiga",  
                        "shall", "ok", "forl", "backi", "fas", "fss", "appro")))
  save(tokenised.no.punct, file = "tokenised.no.punct.Rda")

# 2ndB, we do the tokenisation of the corpus WITH PUNCTUATION and the 2nd layer ----
# of cleaning (removing fragment of words, pseudowords and acronyms
tokenised.punct <- data.clean.punct %>% 
  unnest_tokens("words", "text", strip_punct = FALSE) %>% 
  filter(!(words %in% c("york", "rockefeller", 1913:2013, 1:10, "rf", "univ", "Rf",
                        "c.m", "dr", "cent", "m.d", "mns", "ph.d", "tion", "par",
                        "agric", "inst", "ing", "ap", "pre", "r.r", "pro",
                        "ooo", "d.c", "san", "coll", "tions", "dc", "acad", 
                        "cont", "mex", "ry", "ser", "cap", "med", "natl", "phd",
                        "med", "meph", "xiv", "xvi", "xv", "mada", "bmr", "ingagr",
                        "annual","report","ny", "wwwrockefellerfoundationorgwhat", 
                        "appointed", "programs", "table", "instalments", "remaining", 
                        "fig", "hc", "Ih", "|h", "lh", "percentage", "dh", "pc", 
                        "appropria", "mtg", "xix", "cort", "starr", "stoll", 
                        "vincent", "ova", "rr", "cum", "cons", "viii", "amino",
                        "rfs", "xvii", "xviii", "min", "xix", "sas", "xn", "los", 
                        "bsc", "shss", "qqq", "qqq", "qoo", "los", "iads", "nev", 
                        "inclen", "foi", "por", "Its", "llc", "rodin", "ix", "xii", 
                        "xiii", "xx", "xxii", "Ilc", "greene", "buttrick", "wickliffe", 
                        "ds", "dme", "apr", "barnard", "schillings", "vm", "xin", 
                        "neu", "sheldon", "pfeiffer", "gilardoni","ment", "ments", 
                        "opment", "sion", "tation", "ization", "struction", 
                        "istration", "velopment", "ation", "istration", "lation", 
                        "ration", "zation", "duction", "ganization", "mation",
                        "mentation", "mentation", "provement", "augment", "agement", 
                        "vention", "eration", "fection", "propriation",  "versity", 
                        "ments", "sity", "cation", "dation", "partment", "tration",
                        "sions", "munity", "ness", "ation", "ity","istration",
                        "lation", "divi", "nal", "popul", "educ", "tion", "ration", 
                        "ities","mention", "disserta", "tee", "ro", "cm", "per", 
                        "ref", "vn", "ls", "ga", "g", "c", "h", "dvm", "ms", 
                        "mrs", "w", "ca", "http", "ns", "cd", "pp", "investiga",  
                        "shall","ok", "forl", "backi", "fas", "fss", "appro")))
save(tokenised.punct, file = "tokenised.punct.Rda")

# Count nº of "and" & "the" ----
freq.million <- tokenised.punct %>%
  group_by(doc_id) %>%
  count(words) %>% 
  mutate(freqmill =((n/sum(n))*1000000)) %>% 
  ungroup()

freq.million %>% 
  filter(words %in% c("and", "the"))

# Frequency per million for "and" & "the" per doc_id ----
freq.million.and <- freq.million %>% 
  filter(words %in% c("and")) %>% 
  summarise(freq.million.and = mean(freqmill)) 
  
freq.million.the <- freq.million %>% 
  filter(words %in% c("the")) %>% 
  summarise(freq.million.and = mean(freqmill)) 

save(freq.million, freq.million.and, freq.million.the, file = "freq.million.Rda")

# 3rd Cleaning stopwords ----
tokenised.no.punct.nsw <- anti_join(tokenised.no.punct,stop)
save(tokenised.no.punct.nsw, file = "tokenised.no.punct.nsw.Rda")


# 4th Cleaning ----

# tokenised.no.punct.nsw <- tokenised.no.punct.nsw %>% 
#   mutate(words = str_replace_all(words, "erice$", "ence"),
#          words = str_replace_all(words, "erices$", "ences"),
#          words = str_replace_all(words, "(erice)(.*)", "ence\\2"),
#          words = str_replace_all(words, "(arice)(.*)", "ance\\2"))
# 
#   # filter(grepl("erice", words))
#   
# tokenised.punct <- tokenised.punct %>% 
#   mutate(words = str_replace_all(words, "erice$", "ence"),
#          words = str_replace_all(words, "erices$", "ences"),
#          words = str_replace_all(words, "(erice)(.*)", "ence\\2"),
#          words = str_replace_all(words, "(arice)(.*)", "ance\\2"))

# filter(grepl("erice", words))

save(tokenised.no.punct.nsw, file = "tokenised.no.punct.nsw.Rda")
save(tokenised.punct, file = "tokenised.punct.Rda")

dc.np.final <- tokenised.no.punct.nsw %>% 
  group_by(doc_id) %>% 
  summarise(text = paste(words, collapse=" "))
  
dc.p.final <- tokenised.punct %>% 
  group_by(doc_id) %>% 
  summarise(text = paste(words, collapse=" "))

save(dc.np.final, file = "dc.np.final.Rda")
save(dc.p.final, file= "dc.p.final.Rda")

# view(name of the object)

# Grouping by year and counting words ----
wordnumperyear <- tokenised.no.punct.nsw %>% 
  group_by(Year) %>% 
  count(words, sort=TRUE) %>% 
  top_n(n = 500, n)

save(wordnumperyear, file = "wordnumperyear.Rda")

#  if arranged by default by doc_id arrange(desc(doc_id))

## Top x of the whole period ----
wordnumperyear %>% 
  top_n(n=15, n) %>%
  arrange(Year) %>% 
  print(n=25)

## Graph per year ----
wordnumperyear %>% 
  filter(Year == "1965") %>% 
  top_n(n=35, n) %>%
  ggplot(aes(n, reorder(words,n))) +
  geom_col() +
  labs(y="TOP 35 1965")+
  

# Creating a table per period for the whole of the corpus ----
table.per.period <- tokenised.no.punct.nsw %>% 
  mutate(period = cut(Year,c(0, 1919, 1929, 1939, 1945, 1955, 1965, 1975, 1985,
                             1995, 2005, 2014), 
                      labels = c("1913-1919", "1920-1929", "1930-1939", "1940-1945",
                                 "1946-1955", "1956-1965", "1966-1975", "1976-1985",
                                 "1986-1995", "1996-2005", "2006-2013")))  %>% 
  group_by(period) %>% 
  count(words) %>% 
  filter(n> 10) 

save(table.per.period, file = "table.per.period.Rda")

## Creating the graph per period for the whole of the corpus ----
table.per.period %>% 
  top_n(n, n = 20) %>% 
  ggplot(aes(n, reorder_within(words, n, period), group = period))+
  geom_col()+
  facet_wrap(~period, scales = "free_y")+
  scale_y_reordered()+
  labs(y="TOP 20")+
  theme(legend.position = "none")

## Creating the graph per period choosing it ----
table.per.period %>% 
  filter(period %in% c("1913-1919", "1920-1929", "1930-1939")) %>% 
  top_n(n, n = 50) %>% 
  ggplot(aes(n, reorder_within(words, n, period), group = period))+
  geom_col()+
  facet_wrap(~period, scales = "free_y")+
  scale_y_reordered()+
  labs(y="TOP 15")+
  theme(legend.position = "none")

# Creating a table per year for the whole of the corpus ----
table.per.year <- tokenised.no.punct.nsw%>% 
  mutate(year = str_extract(doc_id, "\\d{4}"), 
         year = as.numeric(year)) %>% 
  group_by(year) %>% 
  count(words) %>% 
  filter(n> 15) 

save(table.per.year, file = "table.per.year.Rda")

## Creating the graphic per year choosing them ----
table.per.year %>% 
  filter(year %in% c(1913, 1919, 1985, 1995, 2005)) %>% 
  top_n(n, n = 25) %>% 
  ggplot(aes(n, reorder_within(words, n, year), group = factor(year)))+
  geom_col()+
  facet_wrap(~year, scales = "free_y")+
  scale_y_reordered()+
  labs(y="TOP 25")+
  theme(legend.position = "none")

# Save on a xlsx file 
openxlsx::write.xlsx(output, file="test.xlsx")


# TF-IDF 
testtf <- tokenised.no.punct.nsw %>% 
  group_by(Year) %>% 
  count(words, sort=TRUE) %>% 
  top_n(n=100, n) %>% 
  arrange(Year)

save(testtf, file = "testtf.Rda")

# table
result.testtf <- testtf %>% 
  bind_tf_idf(words, Year, n)

# TF-IDF graph per period as previously defined ----
table.per.period %>% 
  bind_tf_idf(words, period, n) %>% 
  top_n(tf_idf, n = 20) %>% 
  ggplot(aes(tf_idf, reorder_within(words, tf_idf, period), group = period))+
  geom_col()+
  facet_wrap(~period, scales = "free_y")+
  scale_y_reordered()+
  labs(y="TF-IDF TOP 20")+
  theme(legend.position = "none")

# TF-IDF graph per period chosen ----
table.per.period %>% 
  bind_tf_idf(words, period, n) %>% 
  filter(period %in% c("1946-1955", "1956-1965","1966-1975", "1976-1985",
                       "1986-1995")) %>%
  top_n(tf_idf, n = 50) %>% 
  ggplot(aes(tf_idf, reorder_within(words, tf_idf, period), group = period))+
  geom_col()+
  facet_wrap(~period, scales = "free_y")+
  scale_y_reordered()+
  labs(y="TOP 50")+
  theme(legend.position = "none")

# TF-IDF per year chosen ----
table.per.year %>% 
  bind_tf_idf(words, year, n) %>% 
  filter(year %in% c(1913, 1929, 1939)) %>%   
  top_n(tf_idf, n = 60) %>% 
  ggplot(aes(tf_idf, reorder_within(words, tf_idf, year), group = year))+
  geom_col()+
  facet_wrap(~year, scales = "free_y")+
  scale_y_reordered()+
  labs(y="TOP 25")+
  theme(legend.position = "none")
  

#filter(doc_id %in% paste0("AR-",1913:1936))
#filter(doc_id %in% paste0("AR-",c("1913-1914",1915:1936)))
#filter(doc_id %in% paste0("AR-",1936:1939))

data.clean.no.punct %>% 
  filter(doc_id == "AR-2012")