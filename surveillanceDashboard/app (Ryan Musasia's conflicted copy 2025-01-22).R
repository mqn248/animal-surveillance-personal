library(future)
library(promises)
plan(multisession)
options(future.rng.onMisuse = "ignore")
source("global2.R")




# Define UI for application that draws a histogram
ui <- fluidPage(
  #useShinydashboard(),
  useShinyjs(),
  tags$head(
    tags$link(rel = "icon", href = "veterinarylogo.jpg")
  ),
  tags$head(
    tags$title("Animal Surveillance")
  ),
  includeCSS("styles.css"),
  includeCSS("nav.css"),
  id = "animal_surveillance",
  
  # 1.0 Summary -----------------------------------------------------------------
  
  tabsetPanel(
    id = "main-tabset",
    type = "pills",
    tabPanel(
      "Summary",
      dashboardPage(
        dashboardHeader(title = "Summary"),
        dashboardSidebar(width = "200px",  
                         sidebarMenu(
                           menuItem("News Headlines", tabName = "news",
                                    menuSubItem("World", newtab = T, tabName = "world", selected = T),
                                    menuSubItem("Local", newtab = T, tabName = "local"),
                                    startExpanded = TRUE
                                    
                           ),
                           menuItem('Animal Surveillance', tabName = "surveillance")                                   
                         )
                         ),
      
        dashboardBody(includeCSS("menu.css"),
                      tabItems(
                        ## 1.1 News Headline -------------------------------------------------
                        
                        tabItem(
                          tabName = "world",
                          tabsetPanel(
                            type = 'pills',
                            tabPanel(
                              'World Animal Health News',
                              panel(
                                position = 'left',
                                status = 'primary',
                                heading = 'Headlines',
                                HTML('<iframe width="100%" height="1080" src="https://www.woah.org/en/home/"></iframe>')
                              )
                            )
                          )
                        ),
                      # Animal Surveillance -----------------------------------------------------
                      tabItem(
                        tabName = "surveillance",
                        div(
                          id = 'intro',
                          h4(
                            strong("Summary of the Animal Surveillance Report System"), style = "color: #27AAE1;text-align:center; margin-bottom:10px;"),
                          
                          fluidRow(
                            infoBoxOutput("v1"),
                            infoBoxOutput("v2"),
                            infoBoxOutput("v3"),
                            infoBoxOutput("v4"),
                            infoBoxOutput("v5"),
                          )
                        ),
                        
                        div(
                          id = "intro",
                          h4(
                            strong("Disease Occurrence and Risk"), style = "color: #27AAE1;text-align:center; margin-bottom:10px;"),
                          fluidRow(
                            column(1),
                            column(3,
                                   selectInput(
                                     inputId = "select_disease",
                                     label = "Select disease",
                                     choices = c("Anaplasmosis", "Foot and Mouth Disease (FMD)", "Wound", "Pneumonia", "Helminthiasis", 
                                                 "Coccidosis", "Fracture", "Bees Sting", "Black Quarter", "Lumpy Skin Disease (LSD)", 
                                                 "Calf Scours", "Tetanus", "Ephemeral fever", "Contagious Caprine Pleuropneumonia (CCPP)", 
                                                 "Brucellosis", "Trypanosomiasis", "East Coast Fever(ECF)", "Foot Rot", "Poisoning", 
                                                 "Caseous Lymphad", "orf", "Mastitis", "Goat Pox", "Peste des Petit Ruminants (PPR)", 
                                                 "Animal bites", "Liver Cyrrhosis", "Hydited Cyst", "Rabies", "Mange", "Fasciola", 
                                                 "Meningitis", "Ketosis", "Heart Water", "Post Parturient Condition", "Dystocia", "Colic", 
                                                 "Coeneurosis", "Laminitis", "Acute Camel Disease Syndromes (ACDS)", "footrot", "Anthrax", 
                                                 "Footrot", "Red Urine", "Abcess", "Abscess", "Newcastle Disease (NCD)", "Rift Valley Fever (RVF)", 
                                                 "Enterotoxaemia", "Orf Disease", "Fowl Typhoid", "Fowl Typoid", "Trauma", "Warble", "Leptospirosis", 
                                                 "Bottle Jow", "Johnes Disease", "Diarrhoea", "Bloat", "Distocia", "Foetal Mumification", 
                                                 "Babesia Bovis", "Haemorrhagic septicemia", "Fowl Pox", "Cirrhosis", "Plant Poison", 
                                                 "Animal Bite", "TVT", "Iron Defficiency", "Infectious Bursai Disease (Gumboro)", "Arthritis", 
                                                 "Myocarditis", "Camel Pox", "Pink Eye", "Foot And Rot", "injury", "H.S", "Dytocia", "Injury", 
                                                 "Milk Fever", "Parasite Infestation", "caseous Lymphad", "Haemorrhagic Septicemia", 
                                                 "Clinical Mastits", "Babesiosis", "Orf", "Orf Disease Was Diagnosed", "Eclampsia", "Collibacillosis", 
                                                 "Bluetongue", "Wryneck", "Bovine papillomatosis", "No", "Myasis", "Metritis", "Retained Placenta", 
                                                 "None", "Contagious Bovine Pleuropneumonia (CBPP)", "Acds", "Flea Allergy Dermatitis", "ORF", 
                                                 "Fructure", "Fracture Of Tibia And Fibula", "Skin Abscess", "Ephemeral Fever", "Coccidiosis", 
                                                 "Contagious Ecthyma", "Ectoparasite Infestation", "Pg", "Acute Camel Death Syndrome", 
                                                 "Cutenous Myiasis", "Taeniosis", "Sheep Pox", "Eye Infection", "Not Confirmed Yet", 
                                                 "Upper respiratory infections", "Septic Wounds", "Cut Wound", "Epmiral Fever", "Sores Around The Gums", 
                                                 "Retained Afterbirth", "N/A", "Salmonellosis", "Acute Trypanosomiasis", "Pinkeye", "Ketosis/Hypocalcaemia", 
                                                 "Coenurosis", "Hynes Attacked", "Plant Poisoning", "Late Abortion", "Ruminal Tympany", "Retained Urine", 
                                                 "Listeriosis", "Acute Camel Disease", "Dermatitis", "Subclinical Mastitis", "blue tongue", "Enteritis", 
                                                 "Blue Tongue Disease", "Canine Distemper (CD)", "Parvovirus", "Contagious Abortion", "Three Day Sickness", 
                                                 "Conjuctivitis", "Abortion", "Bee Sting", "Stomach Enteritis", "Uterine Infection", "Pink Eye Infection", 
                                                 "Photosensitization", "Neurological Diseases", "Dermatophycosis", "Infection Coryza", "Unkown", 
                                                 "Wounds and Abrasion", "Contagious Skin Necrosis", "Wounds", "Bluetongue Disease", "Bacterial Enteritis", 
                                                 "Clinical Mastitis", "Foot Abscess", "Anestrus", "Coryza", "Worms Infestation", "Parakeratosis", 
                                                 "Fracture Of Radius And Ulna", "Contagious Porcine Paralysis", "Rab", "Fowltyphoid", "Helmethiasis", 
                                                 "Diarrhea", "Hemorrhagic Septicemia", "Ringworms", "Diahorrea", "Miscarriage", "Hapatitis", "Pregnancy Test", 
                                                 "Septic Wound", "Cervical Prolapse", "Sprain", "Acute Mastitis", "Blue  Tongue", "African Swine Fever (ASF)", 
                                                 NA, "Ring Worm", "Teat Warts", "Overgrown Horns", "Verminous Pneumonia", "Fracture Of Femur", 
                                                 "Streptothricosis", "Allergy", "Ehrlichiosis", "Septicemia", "Mineral Salt Defficiency", "Infertility", 
                                                 "Menge", "Warbles", "Skina Abscess", "Perineal Hernia", "Otitis Interner", "Infectious Coryza", "Tick Fever", 
                                                 "Suspected Brucellosis", "Telangiectasis", "Ephemeral", "Liver cyrrhosis", "Tick Infestation", "Exophthalmos", 
                                                 "Wry neck", "Trichomonas", "Hynes Attacked and Killed 1 Bovine /Left 1 Injuries", "Blue Tongue", "Strangulation", 
                                                 "Hardware", "Allergies", "Cutaneous Myiasis", "Chronic Mange Infestation", "Sheep Enterotoximea", "Fibrosis", 
                                                 "Pustular Dermatitis", "Congestion", "Malaria", "Pheumonia", "Diarhroea", "Wooden Tongue", "Non", "Suspected RVF", 
                                                 "Fracture Of The Mandible", "Knee Bruises", "Skin Wounds", "Camel Cough", "Bloat And Constipation", "Emaciation", 
                                                 "Nsd", "Traumatic Injury", "Anaestrus", "Organophosphate", "Wart Infection", "Caprine arthritis", "Babeosis", 
                                                 "Abdominal Hernia", "Kerato Conjuctivities", "Leg Fracture", "Predation by Hynes", "Locia", "Tryps", "Helminthes", 
                                                 "Torticolis", "Limping", "Poor Body Condition", "Fowl paralysis", "Laryngotracheitis", "Bovine Papillomatosis", 
                                                 "Aggressiveness due to loneliness", "Demedicosis", "Myiasis", "Oestrus ovina", "Sweating Sickness", "GIT Infection", 
                                                 "Vitamin D deficiency", "Physical Trauma", "Ear Canker", "Liver Abscess", "Water Intoxication", "Aflatoxin", 
                                                 "Hide Limb Fracture", "Caseous Lymphanitis", "Circling", "Anaphylaxis", "Hyenas", "Compound Fructure", 
                                                 "Suspected Snake Bite", "Canine Distemper", "Cervical Prolapse In An Incalf Cow", "Suspected TRP", 
                                                 "Caseous Lymphangitis", "Inflammation of Abomesum", "Abortions", "Eternal Parasite Infestation", "Foot rot", 
                                                 "Eperythrozoon Bacteria", "Umbilical Hernia", "Fractured Carpals", "Uterine Prolapse", "Aflatoxicosis", 
                                                 "Transmissible Venereal Tumour", "tumour", "Infectious Bronchitis", "Conjunctivitis", "Bacterial Mastitis", 
                                                 "Mycosis", "Decubitus Ulcer", "Gape Worm Infestation", "Lack Of Minerals", "Tick Paralysis", "Atresia Ani", 
                                                 "Torsion", "Carpal Flexion Dystocia", "Shipping Fever", "Fascioliasis", "Carpus Sprain", "Eperythrozoon Bactreria", 
                                                 "Equine trypanosomiasis", "Iron Deficiency", "Horn Buds", "Locked Jaw/ Tetanus", "Abscesses", "Streptococcis", 
                                                 "Tick Bite Fever", "Orf disease", "Keratoconjunctivities", "Oestrus Ovis", "Worms", "R.A.B", "Skin Infection", 
                                                 "Respiratory Infection, Helminthiasis", "Helminthias", "Dermatophilosis", "Theileriosis", "Ruminal Acidosis", 
                                                 "Sheep scab", "hemorrhagic septicemia", "Keratoconjunctivitis", "Swine Erysipelas", "Nutritional Diarrhea", 
                                                 "Bovine Filariasis", "Helmenthiasis", "Bacterial Infection", "Manges", "Mummified Fetus", "Suspected CCPP", 
                                                 "Traumatic Reticulopercarditis", "Trpanosomiasis", "Hypoglycemia", "Wry Neck", "Respiratory Infection", 
                                                 "Iron", "Vaginal Prolapse", "Leg Wounds", "East Coast Fever", "Helminthisiasis", "O", "Skin Nodules", 
                                                 "Delayed Heat", "Iron Deficiency,tooth & Tail Clipping", "Anorexic", "Pregnancy Diagnostic", "Helmethiosis", 
                                                 "Calcium Deficiency", "Foot rote", "Foitrot", "Reproductive Disorder", "Heamorhagic Septicemia", "00", 
                                                 "Helmenthiosis", "CCPP", "Ecf", "Chlamydia", "Predation {Hyena}", "Pullorium disease", "Dietary Diarrhea", 
                                                 "Footrote", "Wildlife(Hynes Attacked)", "Vitamin Deficiencies", "Caseous Lymphanginitis", "Qfever", 
                                                 "Pink Eye Disease", "Miases", "Flea Infestation", "Eye Disease", "Skin Rashes And Manges", "Mammary Gland Tumor", 
                                                 "Malnutrition", "Fetal Mummification", "Equine influenza", "Haemorrhagic Septicaemia", "Helminth", "V.Prolapse", 
                                                 "Faecal Impaction", "Salmonelosis", "Kids Score", "Fot rot", "Skin Wound", "Stomatitis", "Myaisis", 
                                                 "Respiratory Complex", "Eye Injury", "Coneurosis", "Insect Bite", "Kidney Stones", "Photosensation", 
                                                 "Management", "Mycotoxicosis", "Route Management Program", "Repeat Breeder Syndrome", "White Muscle", 
                                                 "Tuberculosis", "Wild Life Attacked", "Normal", "Hypocalcemia", "Food Poisoning", "Tumour", "Anaplamosis", 
                                                 "Wildlife Attacked", "Trypanosomiasis, Respiratory Infection", "Fever", "Eye Problem", "Lymphatic Nodes Swelling", 
                                                 "Skin Allergy", "Orf Virus", "Tumor", "Respiratory Problem", "Bacteria Scours", "TRP", "Dislocation", "TBDs", 
                                                 "Vectors", "TBDs,TRYPS", "White Muscle Disease", "Bitten By Hyena", "Parasites", "Pimply Guts", "Entritis", 
                                                 "NSD", "Stinging", "Retained afterbirth", "Pink Eyes", "Uterine Irrigation", "Babesios", "Eperythrozoon", 
                                                 "Peritonitis", "Pyrethrin Poisoning", "Grass Tetany", "Worm Infestation", "Menges", "Anaplasmosis....DDx ECF", 
                                                 "Avian leucosis", "Dystochia", "Sprain Of The Elbow Joint", "Stomatitis And Mammilitis", "Acute Pneumonia", 
                                                 "Envenomation", "Retained", "HS", "Sinusitis", "Ectoparasites Infestation", "Trypanosoma", "Hydatidosis, Helminthiasis", 
                                                 "Hydatidosis", "Lameness", "Post Parturient Treatment", "Poor Milking Technique Causing Pain", 
                                                 "Traumatic Reticulopericarditis", "Hydatid Cyst", "Heamoligic Septicemia", "Auricular Hematoma", "Fungul", 
                                                 "Fowl typhoid", "Choke", "Humeral Fracture", "Dermatomycosis", "Enteritis 0", "Actinomycosis", 
                                                 "Trypanosomiasis DDx Mineral Deficiency", "Lymph Swelling", "Edema With Pus", "Footh Rot", "Warts", "Wary Neck", 
                                                 "Sarcoptic Mange", "Kennel cough", "AFLATOXICOSIS", "White Bacilary Diarrhoea", "Nerval Illness", 
                                                 "Diqrrhoea due lush pasture", "Heartwater Disease", "Hypocalcaemia", "Redwater", "Septicaemia/Salmonella", 
                                                 "Bee Stings", "Castration", "Disease Characterized By General Elongation Of Hooves Leading To Lameness", 
                                                 "Bovine Ephemeral Fever", "Traumatic Deep Cut", "Hemonchosis", "Eye Infection/Keratoconjunctivitis", 
                                                 "Gape Worm", "Trapnomiasis", "Mastitis/blocked Teat", "Transmissible Venereal Tumor", "Endometritis", 
                                                 "Retain Placenta", "Downer Cow Syndrome", "Photosensitivity", "Rhinitis", "Actinobacillosis", "Suspected Fmd", 
                                                 "Aflatoxin Poisoning", "Hypocalcimia", "Non specific disease", "Broken Hind Limb", "Retained  Afterbirth", 
                                                 "Food Poisoming", "Retain After Birth", "1", "Glossitis", "Hynes Predation", "Predation", "Ascites", 
                                                 "Haemonchosis", "Swollen Edema With Pus", "Wrinkles Nack", "Postpartum Uterine Infection", "Infectious Corrhiza", 
                                                 "DHLP", "Retained After birth", "RAT", "Anaemia", "Numbness", "Paraphimosis", "Amoebiasis", "Milkman disease", 
                                                 "Dermatophytosis", "Vibriosis", "Trypanosomiasis....ddx Respiratory Infection", "Postpertum Uterine Infection", 
                                                 "Swelling Of Glands", "Highly Pathogenic Avian Influenza (HPAI)", "Gun Shot Wounds", "Dehorning", 
                                                 "Traumatic Reticuloperitonitis", "Foot Injury", "RAB", "Mummified foetus", "Tick  Fever", "Yes", 
                                                 "Cutaneous Wounds(Fungal)", "Thelezia", "Not Known", "O0", "Nil", "Malignant Cuttehral Fever", "High Fever", 
                                                 "Anaplasmosis Babesiosis and Eperythrozoon Bacteria", "Cervical Injury", "Lympadenities", 
                                                 "Anaplasmosis,Babesiosis and Theileria", "Helminths", "Helmets", "Rolling Tongue", "Retained After Birth", 
                                                 "Stick Tight Fleas", "Git Infection", "parturition eodema", "Puncture Wound", "Mange Mites", 
                                                 "African Horse Sickness", "Strangles", "Foot And Mouth", "Masty", "pink eye", "Wax Moth", "Hydronephrosis", 
                                                 "Vaginitis", "Lice Infestation", "Keratoconjutivitis", "wound", "Leg Injury", "Fatty Degeneration", 
                                                 "Camel Abscess", "Grain Overload", "Uterus Prolapse", "Goat Dystentry", "Milk fever", "Feline Dermatitis", 
                                                 "Infectous Corryza", "Helmenthesis", "Laceration", "Premature Birth", "Thorn Prick", "Metal Cut Wound", 
                                                 "Heartwater", "Diamond", "Mange Infestation", "Sodium Selenite Hypersensitivity", "Prolapse", "Bacterial Diarrhea", 
                                                 "Camel Mange", "Routine Check Up", "Heat Synchronization", "Syphilis", "Myopericarditis", "Calving", 
                                                 "Hoof Trimming", "Scepticaemia", "Ticks Infestation", "Infertility  Disease", "Pneumonia And Mange", "Unknown", 
                                                 "Off Disease", "Eye infection", "Mange/Ear Cancer", "Gangrenous Disease", "Swelling Lymph Nodes", 
                                                 "Respiratory Complications", "Sprain On Left Hind Limb", "Dog Bite", "Benign tumour of mammary gland", 
                                                 "Traumatic Pericarditis", "Severed Artery", "Bottle Jow Oedema(malnutrition)", "CRDB(chronic Respiratory Disease)", 
                                                 "Malnutrition Especially Protein Deficiency", "Hydatid", "Bite Injury", "Ketosis/Enterotoxaemia", "Toxins", 
                                                 "Downer cow syndrome", "Bovine Salmonellosis", "Pyometra", "Cowdriosis", "Trichomoniasis", 
                                                 "Effects Of Dexamethasone", "Profuse Diarrhoea", "Postparturent Agalactia", "Matitis", "Calfscours Disease", 
                                                 "Heavy Breathing", "Tryps and Helminthiasis", "CRD", "Hyenas Bite", "Non Specific Diarrhoea", "Retained  After Birth", 
                                                 "Anaemic", "Post Parturient Bleeding", "Traumatic of eye", "Stress", "Swine dysentery", "Sarcoid", 
                                                 "enterotoxaemia", "Dermatophylosis", "Cellulitis", "Foot root", "Foot Root", "Suspected Canine tick Borne Fever (Babesiosis and Or Canine Ehrlichiosis)", 
                                                 "Ocular Opacity", "Cuten", "Contagious keratoconjunctivitis", "Bee Bite", "Food Poison", "Damerthophylosis", 
                                                 "Foot Rote", "Footroot", "Snake Bite", "Carpal Abrassion Wound", "Skin Deases", "Ophthalmitis", "Atrophic rhinitis", 
                                                 "Fungal Infection")
                                   )
                            ),
                            column(1),
                            column(2,
                                   prettyRadioButtons(
                                     inputId = "owner_selector",
                                     label = "Select organisation :",
                                     choices = c(
                                       "All",
                                       "Government/Public Entity",
                                       "Private"
                                     ),
                                     selected = "Government/Public Entity",
                                     icon = icon("check"),
                                     animation = "smooth",
                                     bigger = T,
                                     fill = T,
                                     thick = T,
                                     outline = T,
                                     status = "primary",
                                     width = "100%"

                                   )
                            ),
                            column(1),
                            column(2,
                                   pickerInput(
                                     inputId = "county_selector",
                                     label = "Select county", 
                                     choices = unique(data_county$county),
                                     options = pickerOptions(container = "body", 
                                                             liveSearch = TRUE),
                                     width = "100%",
                                     selected = unique(data_county$county[1])
                                   )
                                   
                                   
                            ),
                            
                          ),
                          fluidRow(
                            column(6,
                                   shinycssloaders::withSpinner(
                                     highchartOutput("kenya_map", width = "100%", height = "600px")
                                   )
                            ),
                            column(6,
                                   shinycssloaders::withSpinner(
                                     highchartOutput("county_map", width = "100%", height = "600px")
                                   )
                            )
                          )
                        )
                      )
                      )
                ),
      )
    ),
    
    # Login -------------------------------------------------------------------
    
    tabPanel('Login',
             
             fluidRow(
               style = 'height: 40vh; display: flex; justify-content: center; align-items: center;',
               loginUI(id = "login")
             )
             
    ),
  
    
    # 2.0 Priority Diseases -----------------------------------------------------
    
    navbarMenu(title =  "Zoonotic Diseases",
               tabPanel(
                 "Priority Diseases",
                 dashboardPage(
                   dashboardHeader(title = "Priority Diseases"),
                   dashboardSidebar(width = "200px",
                                    sidebarMenu(
                                      menuItem("Anthrax", tabName = "ant"),        
                                      menuItem('African Swine Fever (ASF)', tabName = "asf"),
                                      menuItem('Avian Influenza (AI)', tabName = "ai"),
                                      menuItem('Brucellosis', tabName = "bru"),
                                      menuItem('Contagious Bovine Pleuro Pneumonia (CBPP)', tabName = "cbpp"), 
                                      menuItem('Contagious Caprine Pleuro Pneumonia (CCPP)', tabName = "ccpp"), 
                                      menuItem('Foot Mouth Disease (FMD)', tabName = "fmd"), 
                                      menuItem('Lumpy Skin Disease (LSD)', tabName = "lsd"), 
                                      menuItem('New Castle Disease (NCD)', tabName = "ncd"), 
                                      menuItem('Peste des Petits Ruminant (PPR)', tabName = "ppr"), 
                                      menuItem('Rabies', tabName = "rabies"), 
                                      menuItem('Rift Valley Fever (RVF)', tabName = "rvf"), 
                                      menuItem('Sheep and Goat Pox (SGP)', tabName = "sgp") 
                                    )),

                   dashboardBody(includeCSS("menu.css"), 
                                 tabItems(
                                   ## 2.1 Anthrax ----------------------------------------------------------
                                   
                                   tabItem(tabName = "ant"
                                          
                                   ), 
                                   ## 2.2 African Swine Fever (ASF) ----------------------------------------------------
                                   
                                   tabItem(tabName = "asf"

                                   ),
                                   # 2.3 Avian Influenza (AI) -------------------------------------------------------------
                                   
                                   tabItem(tabName = "ai"

                                   ),
                                   
                                   ## 2.4 Brucellosis ----------------------------------------------------
                                   
                                   tabItem(tabName = "bru"

                                   ),
                                   
                                   ## 2.5 Contagious Bovine Pleuro Pneumonia (CBPP) ----------------------------------------------------
                                   
                                   tabItem(tabName = "cbpp"

                                   ),
                                   
                                   ## 2.6 Contagious Caprine Pleuro Pneumonia (CCPP) ----------------------------------------------------
                                   
                                   tabItem(tabName = "ccpp"

                                   ),
                                   
                                   ## 2.7 Foot Mouth Disease (FMD) ----------------------------------------------------
                                   
                                   tabItem(tabName = "fmd"

                                   ),
                                   
                                   ## 2.8 Lumpy Skin Disease (LSD) ----------------------------------------------------
                                   
                                   tabItem(tabName = "lsd"

                                   ),
                                   
                                   ## 2.9 New Castle Disease (NCD) ----------------------------------------------------
                                   
                                   tabItem(tabName = "ncd"

                                   ),
                                   
                                   ## 2.10 Peste des Petits Ruminant (PPR) ----------------------------------------------------
                                   
                                   tabItem(tabName = "ppr"

                                   ),
                                   
                                   ## 2.11 Rabies ----------------------------------------------------
                                   
                                   tabItem(tabName = "rabies"

                                   ),
                                   
                                   ## 2.12 Rift Valley Fever (RVF) ----------------------------------------------------
                                   
                                   tabItem(tabName = "rvf"

                                   ),
                                   
                                   ## 2.13 Sheep and Goat Pox (SGP) ----------------------------------------------------
                                   
                                   tabItem(tabName = "sgp"

                                   )
                                   
                                 )
                            )
                 )
               ),
               

      )
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # 0. Authentication ----------------------------------------------------------
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = 'user',
    pwd_col = 'password'
  )
  authenticated <- reactiveVal({})
  observe({
    if (credentials()$user_auth) {
      authenticated(TRUE)  # Set authenticated to TRUE if user_auth is TRUE
      shiny::showTab(inputId = "animal_surveillance", target = "Animal Surveillance")
      shinyalert::shinyalert(
        title = "Logged in successfully!",
        text = "Redirecting to first page",
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        timer = 10000,
        immediate = TRUE,
        closeOnClickOutside = FALSE,
        closeOnEsc = FALSE
      )
      
      # This should switch to the Summary tab
      updateTabsetPanel(session, "main-tabset", selected = "Summary")
      
    } else {
      authenticated(FALSE)  # Set authenticated to FALSE if user_auth is FALSE
      shiny::hideTab(inputId = "animal_surveillance", target = "Animal Surveillance")
    }
  })


# Animal Surveillance Maps ------------------------------------------------

  observeEvent(c(input$select_disease, input$county_selector), {
    df_disease <- data |> 
      filter(`Disease/ Condition` == input$select_disease)
    
    df_disease_sub <- data_subcounty |> 
      filter(`Disease/ Condition` == input$select_disease, county == input$county_selector)
    
    shapefile <- jsonlite::toJSON(county) 
    subcounty <- subcounty |> 
      filter(county == input$county_selector) |> 
      jsonlite::toJSON() 
    
    output$kenya_map <- renderHighchart({
      ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
      # Create stops for the color axis
      stops <- color_stops(9, colors = ylgn_palette)
      
      highchart(type = "map") |>
        hc_add_series(
          name = "Back to main plot",
          mapData =  county, # This county shapefile,
          data = list_parse(df_disease),
          value = 'n',
          borderWidth = 0.8,
          nullColor = "white",
          joinBy = "county",
          showInLegend = FALSE,
          borderColor = "black",
          dataLabels = list(enabled = TRUE, format = '{point.county}'),
          tooltip = list(
            useHTML = TRUE,
            headerFormat = "<p>",
            pointFormat = paste0("<b style=\"color:#1874CD\"> Number at Risk:</b> {point.`Number at Risk`:.2f}<br>"),
            footerFormat = "</p>"
          )
        ) |>
        hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
        hc_colorAxis(
          min = min(df_disease$`Number at Risk`),
          max = max(df_disease$`Number at Risk`), 
          stops = stops  
        ) |> 
        hc_exporting(enabled = TRUE)
    })
    output$county_map <- renderHighchart({
      ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
      # Create stops for the color axis
      stops <- color_stops(9, colors = ylgn_palette)
      
      highchart(type = "map") |>
        hc_add_series(
          name = "Back to main plot",
          mapData =  subcounty, # This Subcounty shapefile,
          data = list_parse(df_disease_sub),
          value = 'n',
          borderWidth = 0.8,
          nullColor = "white",
          joinBy =  "sub_county",
          showInLegend = FALSE,
          borderColor = "black",
          dataLabels = list(enabled = TRUE, format = '{point.sub_county}'),
          tooltip = list(
            useHTML = TRUE,
            headerFormat = "<p>",
            pointFormat = paste0("<b style=\"color:#1874CD\"> Number at Risk:</b> {point.`Number at Risk`:.2f}<br>"),
            footerFormat = "</p>"
          )
        ) |>
        hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
        hc_colorAxis(
          min = min(df_disease$`Number at Risk`),
          max = max(df_disease$`Number at Risk`), 
          stops = stops  
        ) |> 
        hc_exporting(enabled = TRUE)
    })
  })
  
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
