######## INTERVENTION GROUP ANALYSIS BY SEVERITY BEFORE INTERVENTION ########
lapply(c("metafor","ggplot2","gridExtra" ,"psych", 
         "RCurl", "irr", "nortest", "moments","GPArotation",
         "nFactors","gdata","meta","qgraph","devtools",
         "googlesheets","plyr"), library, character.only=T)
data <- read.csv('/Users/oper/Desktop/Tables Depression_Atrittion - depression_SR_metanalysis.csv.csv')

####### RECODE #######
data$age_ig<-car::recode(data$age_mean_ig,"20:30='20-30'; 30.01:40='30-40'; 40.01:45='40-45'; 
                         45.01:50='45-50'; 50.01:60='50-60'; 60.01:99='60 or more'")
data$follow_up_recoded<-car::recode(data$follow_up, "0:6='6 months or less'; 6.01:12='6-12 months';
                                    12.02:99='more than 12 months'")
data$freq_adm_recoded<-car::recode(data$freq_adm_ig, "0:1='1 or less'; 1.01:2='2'; 2.1:3='3'")
data$diag_crit<-car::recode(data$depression_diag_criteria, "1='DSM only'; 2='DSM plus severity scales'")
data$out_meth<-car::recode(data$outcome_method, "1='One scale'; 2='Two or more scales'")
data$sample_size<-car::recode(data$total_sample_ig, "1:50='1-50'; 51:100='51-100'; 101:150='101-150';
                              151:200='151-200'; 201:300='201-300'; 301:400='301:400'; 401:500='401-500'; 
                              500:9999= '500 or more'")
data$region<-car::recode(data$country, "c('Japan','China','South Korea')='East Asia and Pacific'; c('Austria',
                         'France', 'Germany', 'Italy', 'Netherlands', 'Poland', 'Russia', 'Spain', 'Sweden',
                         'Switzerland', 'UK')='Europe and Central Asia'; 'Brazil'='South America and The Caribean';
                         'Iran'='Middle East and North Africa'; c('USA', 'Canada')= 'North America'; 
                         else='Multiple Countries'")
data$meds<-car::recode(data$intervention, "12='Amiketone - bupropion'; 
                       20='Azaspirodecanedione derivatives - buspirone';
                       11='Melatonin agonist - agomelatine';
                       13='Selective Norepinephrine Reuptake Inhibitor - Reboxetine';
                       c(6,7,8,9,10)='Serotonin and Norepinephrine Reuptake Inhibitor - desvenlafaxine, duloxetine, milnacipran, levomilnacipran, venlafaxine';
                       c(17, 1,2,3,4,56,15,14)='Setotonin Reuptake Inhibitor -  vilazodone, citalopram, fluoxetine, paroxetine, sertraline, trazodone, vortioxetine';
                       16='Tetracyclic antidepressant - mirtazapine';
                       c(18,19)='Tricyclic Antidepressant - imipramine, nortriptyline';
                       22= 'Aldosterone Antagonist - Spironolactone'; 44='Androgen - Testosterone';
                       c(29,28)='Anti-epileptic Agente - Topiramate, Lamotrigine';
                       34='Anticholinergic - Scopolamine'; 
                       c(23,24,25,26,27)='Atypical Antipsychotic - Olanzepine, Quetiapine, Ziprasidone, Risperidone, Aripriprazole';
                       45= 'Biguanide - Metformin'; c(49,50)='Central Nervous System Stimulant - Lisdexamfetamine, Methylphenidate';
                       32='Drugs Used in Alcohol Dependence - Acamprosate';
                       47='General Anethetic - Ketamine';
                       c(35,36,37)='HMG-CoA Reductase Inhibitor - Lovastatin, Atorvastatin, Simvastatin';
                       c(43,42)='L-Thyroxine or L-Triiodothyronine - Levothyroxine Sodium, Liothyronine';
                       c(51,52)='Mineral Suplements - Zinc'; 21='Mineralocorticoids - Fludocortisone';
                       5='Monoamine Oxidase Inhibitor - Selegiline';
                       30='N-methyl-D-aspartate Receptor Antagonist - Memantine';
                       41='Non steroidal Anti-inflammatory Drug - Celocoxib';
                       c(39,40)='Omega-3 Fatty Acids'; 33='Substance P/Neurokinin-1 Receptor Antagonist - Aprepitant';
                       48='Sympathomimetic-like Agent - Modafinil'; 46='Thiazolidinedione - Pioglitazone';
                       53='Tumor Necrosis Factor Blocker - Infliximab';
                       55='Corticotropin-releasing factor (CRF) receptor antagonists';
                       58='AMPA Receptor Positive Allosteric Modulator - Org 26576';
                       54='NMDA Channel Blocker - Lanicenine'; else='combinations'")

####### SUBSETS #######
abesence <- subset(data, data$severity_b_ig=='absence')
mild <- subset(data, data$severity_b_ig=='mild')
moderate <- subset(data, data$severity_b_ig=='moderate')
severe <- subset(data, data$severity_b_ig=='severe')

###### OVERALL
funneldata<-with(moderate,data.frame(attrition_ig, total_sample_ig, Ref., author_year))
funneldata<-na.omit(funneldata[order(funneldata$author_year),])
funnelmod<-metaprop(attrition_ig, total_sample_ig, sm='PLN', data=funneldata, studlab=Ref.)
tiff("/Users/oper/Desktop/Intervention Group/funnel.tiff", width = 1000, height = 1000,compression = 'lzw')
funnel(funnelmod)
dev.off()



###### SEVERITY INTERVENTION GROUP ######
severity<-with(data,data.frame(attrition_ig, total_sample_ig, author_year, Ref., severity_b_ig))
severity<-na.omit(severity[order(severity$author_year),])
metaseverity<-metaprop(attrition_ig, total_sample_ig, sm="PLN", data=severity,studlab=Ref., byvar=severity_b_ig)
tiff("/Users/oper/Desktop/Intervention Group/Ovearall by Severity.tiff", width = 800, height = 2200,compression = 'lzw')
forest(metaseverity)
dev.off()

quality<-with(data,data.frame(attrition_ig, total_sample_ig, author_year, Ref., quality))
quality<-na.omit(quality[order(quality$author_year),])
metaquality<-metaprop(attrition_ig, total_sample_ig, sm="PLN", data=quality,studlab=Ref., byvar=quality)
tiff("/Users/oper/Desktop/Intervention Group/Ovearall by Quality.tiff", width = 800, height = 2200,compression = 'lzw')
forest(metaquality)
dev.off()

###### ANALYSIS ABSENCE - 2 artigos ... ######
#1# COUNTRY
#2# GENDER
#3# AGE
#4# SAMPLE SIZE
#5# FOLLOW UP
#6# FREQ ADM
#7# DIAG CRITERIA
#8# OUTCOME METHOD

###### ANALYSIS FOR MILD DEPRESSION ######
#1# REGION
mild_region<-with(mild,data.frame(attrition_ig, total_sample_ig, author_year, Ref., region))
mild_region<-na.omit(mild_region[order(mild_region$author_year),])
mild_region$region<-as.character(mild_region$region)
metamild_region<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mild_region,studlab=Ref., byvar=region)
tiff("/Users/oper/Desktop/mild_ig/mild_region.tiff", width = 800, height = 600,compression = 'lzw')
forest(metamild_region)
dev.off()

#2# GENDER
mild_gender<-with(mild,data.frame(attrition_ig, total_sample_ig, author_year, Ref., gender))
mild_gender<-na.omit(mild_gender[order(mild_gender$author_year),])
mild_gender$gender<-as.character(mild_gender$gender)
metamild_gender<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mild_gender,studlab=Ref., byvar=gender)
tiff("/Users/oper/Desktop/mild_ig/mild_gender.tiff", width = 800, height = 400,compression = 'lzw')
forest(metamild_gender)
dev.off()

#3# AGE - alguns NAs apareceram na tiff
mild_age<-with(mild,data.frame(attrition_ig, total_sample_ig, author_year, Ref., age_ig))
mild_age<-na.omit(mild_age[order(mild_age$author_year),])
mild_age$age_ig<-as.character(mild_age$age_ig)
metamild_age<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mild_age,studlab=Ref., byvar=age_ig)
tiff("/Users/oper/Desktop/mild_ig/mild_age.tiff", width = 800, height = 800,compression = 'lzw')
forest(metamild_age)
dev.off()

#4# SAMPLE SIZE
mild_ss<-with(mild,data.frame(attrition_ig, total_sample_ig, author_year, Ref., sample_size))
mild_ss<-na.omit(mild_ss[order(mild_ss$author_year),])
metamild_ss<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mild_ss,studlab=Ref., byvar=sample_size)
tiff("/Users/oper/Desktop/mild_ig/mild_ss.tiff", width = 800, height = 600,compression = 'lzw')
forest(metamild_ss)
dev.off()

#5# FOLLOW UP
mild_fu<-with(mild,data.frame(attrition_ig, total_sample_ig, author_year, Ref., follow_up_recoded))
mild_fu<-na.omit(mild_fu[order(mild_fu$author_year),])
metamild_fu<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mild_fu,studlab=Ref., byvar=follow_up_recoded)
tiff("/Users/oper/Desktop/mild_ig/mild_fu.tiff", width = 800, height = 400,compression = 'lzw')
forest(metamild_fu)
dev.off()

#6# FREQ ADM
mild_fa<-with(mild,data.frame(attrition_ig, total_sample_ig, author_year, Ref., freq_adm_recoded))
mild_fa<-na.omit(mild_fa[order(mild_fa$author_year),])
metamild_fa<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mild_fa,studlab=Ref., byvar=freq_adm_recoded)
tiff("/Users/oper/Desktop/mild_ig/mild_fa.tiff", width = 800, height = 400,compression = 'lzw')
forest(metamild_fa)
dev.off()

#7# DIAG CRITERIA
mild_diag<-with(mild,data.frame(attrition_ig, total_sample_ig, author_year, Ref., diag_crit))
mild_diag<-na.omit(mild_diag[order(mild_diag$author_year),])
metamild_diag<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mild_diag, studlab=Ref., byvar=diag_crit)
tiff("/Users/oper/Desktop/mild_ig/mild_diag.tiff", width = 800, height = 400,compression = 'lzw')
forest(metamild_diag)
dev.off()

#8# OUTCOME METHOD
mild_om<-with(mild,data.frame(attrition_ig, total_sample_ig, author_year, Ref., out_meth))
mild_om<-na.omit(mild_om[order(mild_om$author_year),])
metamild_om<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mild_om, studlab=Ref., byvar=out_meth)
tiff("/Users/oper/Desktop/mild_ig/mild_om.tiff", width = 800, height = 400,compression = 'lzw')
forest(metamild_om)
dev.off()

#9# MEDS
mild_meds<-with(mild,data.frame(attrition_ig, total_sample_ig, author_year, Ref., meds))
mild_meds<-na.omit(mild_meds[order(mild_meds$author_year),])
metamild_meds<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mild_meds, studlab=Ref., byvar=meds)
tiff("/Users/oper/Desktop/mild_ig/mild_meds.tiff", width = 800, height = 800,compression = 'lzw')
forest(metamild_meds)
dev.off()


###### ANALYSIS FOR MODERATE DEPRESSION ######
#1# REGION
mod_region<-with(moderate,data.frame(attrition_ig, total_sample_ig, author_year, Ref., region))
mod_region<-na.omit(mod_region[order(mod_region$author_year),])
mod_region$region<-as.character(mod_region$region)
metamod_region<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mod_region,studlab=Ref., byvar=region)
tiff("/Users/oper/Desktop/mod_ig/mod_region.tiff", width = 800, height = 1600,compression = 'lzw')
forest(metamod_region)
dev.off()

#2# GENDER
mod_gender<-with(moderate,data.frame(attrition_ig, total_sample_ig, author_year, Ref., gender))
mod_gender<-na.omit(mod_gender[order(mod_gender$author_year),])
metamod_gender<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mod_gender,studlab=Ref., byvar=gender)
tiff("/Users/oper/Desktop/mod_ig/mod_gender.tiff", width = 800, height = 1400,compression = 'lzw')
forest(metamod_gender)
dev.off()

#3# AGE - alguns NAs apareceram na tiff
mod_age<-with(moderate,data.frame(attrition_ig, total_sample_ig, author_year, Ref., age_ig))
mod_age<-na.omit(mod_age[order(mod_gender$author_year),])
metamod_age<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mod_age,studlab=Ref., byvar=age_ig)
tiff("/Users/oper/Desktop/mod_ig/mod_age.tiff", width = 800, height = 1800,compression = 'lzw')
forest(metamod_age)
dev.off()

#4# SAMPLE SIZE
mod_ss<-with(moderate,data.frame(attrition_ig, total_sample_ig, author_year, Ref., sample_size))
mod_ss<-na.omit(mod_ss[order(mod_ss$author_year),])
metamod_ss<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mod_ss,studlab=Ref., byvar=sample_size)
tiff("/Users/oper/Desktop/mod_ig/mod_ss.tiff", width = 800, height = 1600,compression = 'lzw')
forest(metamod_ss)
dev.off()

#5# FOLLOW UP
mod_fu<-with(moderate,data.frame(attrition_ig, total_sample_ig, author_year, Ref., follow_up_recoded))
mod_fu<-na.omit(mod_fu[order(mod_ss$author_year),])
metamod_fu<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mod_fu,studlab=Ref., byvar=follow_up_recoded)
tiff("/Users/oper/Desktop/mod_ig/mod_fu.tiff", width = 800, height = 1400,compression = 'lzw')
forest(metamod_fu)
dev.off()

#6# FREQ ADM
mod_fa<-with(moderate,data.frame(attrition_ig, total_sample_ig, author_year, Ref., freq_adm_recoded))
mod_fa<-na.omit(mod_fa[order(mod_ss$author_year),])
metamod_fa<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mod_fa,studlab=Ref., byvar=freq_adm_recoded)
tiff("/Users/oper/Desktop/mod_ig/mod_fa.tiff", width = 800, height = 1200,compression = 'lzw')
forest(metamod_fa)
dev.off()

#7# DIAG CRITERIA
mod_diag<-with(moderate,data.frame(attrition_ig, total_sample_ig, author_year, Ref., diag_crit))
mod_diag<-na.omit(mod_diag[order(mod_ss$author_year),])
metamod_diag<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mod_diag, studlab=Ref., byvar=diag_crit)
tiff("/Users/oper/Desktop/mod_ig/mod_diag.tiff", width = 800, height = 1400,compression = 'lzw')
forest(metamod_diag)
dev.off()

#8# OUTCOME METHOD
mod_om<-with(moderate,data.frame(attrition_ig, total_sample_ig, author_year, Ref., out_meth))
mod_om<-na.omit(mod_om[order(mod_om$author_year),])
metamod_om<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mod_om, studlab=Ref., byvar=out_meth)
tiff("/Users/oper/Desktop/mod_ig/mod_om.tiff", width = 800, height = 1400,compression = 'lzw')
forest(metamod_om)
dev.off()

#9# MEDS
moderate$medsmod<-car::recode(moderate$meds,"c('AMPA Receptor Positive Allosteric Modulator - Org 26576',
                     'Androgen - Testosterone','Central Nervous System Stimulant - Lisdexamfetamine, Methylphenidate',
                     'combinations', 'Corticotropin-releasing factor (CRF) receptor antagonists',
                     'L-Thyroxine or L-Triiodothyronine - Levothyroxine Sodium, Liothyronine',
                     'N-methyl-D-aspartate Receptor Antagonist - Memantine',
                     'NMDA Channel Blocker - Lanicenine','Non steroidal Anti-inflammatory Drug - Celocoxib', 'Omega-3 Fatty Acids', 
'Selective Norepinephrine Reuptake Inhibitor - Reboxetine',
                     'Tricyclic Antidepressant - imipramine, nortriptyline')='Others'")
mod_meds<-with(moderate,data.frame(attrition_ig, total_sample_ig, author_year, Ref., medsmod))
mod_meds<-na.omit(mod_meds[order(mod_meds$author_year),])
metamod_meds<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=mod_meds, studlab=Ref., byvar=medsmod)
tiff("/Users/oper/Desktop/mod_ig/mod_meds.tiff", width = 800, height = 1600,compression = 'lzw')
forest(metamod_meds)
dev.off()

###### ANALYSIS FOR SEVERE DEPRESSION ######
#1# REGION
sev_region<-with(severe,data.frame(attrition_ig, total_sample_ig, author_year, Ref., region))
sev_region<-na.omit(sev_region[order(sev_region$author_year),])
sev_region$region<-as.character(sev_region$region)
metasev_region<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=sev_region,studlab=Ref., byvar=region)
tiff("/Users/oper/Desktop/sev_ig/sev_region.tiff", width = 800, height = 1200,compression = 'lzw')
forest(metasev_region)
dev.off()

#2# GENDER
sev_gender<-with(severe,data.frame(attrition_ig, total_sample_ig, author_year, Ref., gender))
sev_gender<-na.omit(sev_gender[order(sev_gender$author_year),])
metasev_gender<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=sev_gender,studlab=Ref., byvar=gender)
tiff("/Users/oper/Desktop/sev_ig/sev_gender.tiff", width = 800, height = 1000,compression = 'lzw')
forest(metasev_gender)
dev.off()

#3# AGE - alguns NAs apareceram na tiff
sev_age<-with(severe,data.frame(attrition_ig, total_sample_ig, author_year, Ref., age_ig))
sev_age<-na.omit(sev_age[order(sev_gender$author_year),])
sev_age$age_ig<-as.character(sev_age$age_ig)
metasev_age<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=sev_age,studlab=Ref., byvar=age_ig)
tiff("/Users/oper/Desktop/sev_ig/sev_age.tiff", width = 800, height = 1400,compression = 'lzw')
forest(metasev_age)
dev.off()

#4# SAMPLE SIZE
sev_ss<-with(severe,data.frame(attrition_ig, total_sample_ig, author_year, Ref., sample_size))
sev_ss<-na.omit(sev_ss[order(sev_ss$author_year),])
metasev_ss<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=sev_ss,studlab=Ref., byvar=sample_size)
tiff("/Users/oper/Desktop/sev_ig/sev_ss.tiff", width = 800, height = 1400,compression = 'lzw')
forest(metasev_ss)
dev.off()

#5# FOLLOW UP
sev_fu<-with(severe,data.frame(attrition_ig, total_sample_ig, author_year, Ref., follow_up_recoded))
sev_fu<-na.omit(sev_fu[order(sev_ss$author_year),])
metasev_fu<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=sev_fu,studlab=Ref., byvar=follow_up_recoded)
tiff("/Users/oper/Desktop/sev_ig/sev_fu.tiff", width = 800, height = 1000,compression = 'lzw')
forest(metasev_fu)
dev.off()

#6# FREQ ADM
sev_fa<-with(severe,data.frame(attrition_ig, total_sample_ig, author_year, Ref., freq_adm_recoded))
sev_fa<-na.omit(sev_fa[order(sev_ss$author_year),])
metasev_fa<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=sev_fa,studlab=Ref., byvar=freq_adm_recoded)
tiff("/Users/oper/Desktop/sev_ig/sev_fa.tiff", width = 800, height = 800,compression = 'lzw')
forest(metasev_fa)
dev.off()

#7# DIAG CRITERIA
sev_diag<-with(severe,data.frame(attrition_ig, total_sample_ig, author_year, Ref., diag_crit))
sev_diag<-na.omit(sev_diag[order(sev_ss$author_year),])
metasev_diag<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=sev_diag, studlab=Ref., byvar=diag_crit)
tiff("/Users/oper/Desktop/sev_ig/sev_diag.tiff", width = 800, height = 1000,compression = 'lzw')
forest(metasev_diag)
dev.off()

#8# OUTCOME METHOD
sev_om<-with(severe,data.frame(attrition_ig, total_sample_ig, author_year, Ref., out_meth))
sev_om<-na.omit(sev_om[order(sev_ss$author_year),])
metasev_om<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=sev_om, studlab=Ref., byvar=out_meth)
tiff("/Users/oper/Desktop/sev_ig/sev_om.tiff", width = 800, height = 1000,compression = 'lzw')
forest(metasev_om)
dev.off()

#9# MEDS
severe$medssev<-car::recode(severe$meds,"c('Anticholinergic - Scopolamine', 
                            'Monoamine Oxidase Inhibitor - Selegiline',
                            'Tumor Necrosis Factor Blocker - Infliximab',
                            'Mineral Suplements - Zinc',
                            'Atypical Antipsychotic - Olanzepine, Quetiapine, Ziprasidone, Risperidone, Aripriprazole', 'combinations)
                            ='Others'")
sev_meds<-with(severe,data.frame(attrition_ig, total_sample_ig, author_year, Ref., medssev))
sev_meds<-na.omit(sev_meds[order(sev_meds$author_year),])
sev_meds$medssev<-as.character(sev_meds$medssev)
metasev_meds<-metaprop(attrition_ig ,total_sample_ig, sm="PLN",data=sev_meds, studlab=Ref., byvar=medssev)
tiff("/Users/oper/Desktop/sev_ig/sev_meds.tiff", width = 800, height = 1000,compression = 'lzw')
forest(metasev_meds)
dev.off()


###### NETWORK ANALYSIS AND EFFECT SIZES ######
#1# ES
datanet <- read.csv('/Users/oper/Desktop/Tables Depression_Atrittion - depression_SR_attcause.csv.csv')
datanet2<-lapply(datanet,function(x) car::recode(x,"NA=0"))
datanet2<-as.data.frame(datanet2)
data_network<-remove.vars(datanet2,c("Study"))
data_network<-as.data.frame(data_network)
effect_size<-colSums(as.data.frame(data_network))
effect_sizes<-as.matrix(effect_size/dim(as.data.frame(data_network))[1])
intensity<-length(dim(effect_sizes>=0.25))
intensity_data<-rowSums(as.data.frame(data_network))
intensity_sizes<-intensity_data/intensity

#2# NW
variable_data <- t(as.matrix(data_network)) %*% as.matrix(data_network)
#study_data<-as.matrix(data_network)
#rownames(study_data)<-datanet$Study
#network_data<-rbind(variable_data,study_data)
#network_data <- (as.matrix(network_data)) %*% t(as.matrix(network_data))
diag(variable_data) <- 0
names<-c(c("LE","AE","LFU","LAB","WC","TT","SE","DISC", "ID", "O", "RM", "MADRS", "PV", "PD", "SD", "PhD", "NT",
           "ADM", "ILL", "SC", "PA", "EC", "IN", "IBS", "FTCQ", "IR", "REC", "NMED", "CDOS", "SevD", "HOSP",
           "CAD", "AP"))

variable2<-cor(data_network,method="spearman")

size_edges<-c(effect_sizes[,1]*10)
#color<-c("red","yellow","lightblue","lightblue","red","red","yellow","yellow",rep("grey",46))
#shape<-c(rep("circle",8),rep("square",46)) 
#label.cex<- c(rep(1.5,8),rep(1.0,46))
#groups<-c("Ensaio Clínico","Medicamentos","Outras Razões")

network_meta <- qgraph(variable_data ,layout = "spring",minimum=0.2,cut=0.4,label.scale=FALSE,
                       grey=T,borders=TRUE, vsize=size_edges, labels=names, legend=TRUE, posCol = "gray")

tiff("/home/joao/Desktop/sporedata_depression_sr_network.tiff", width = 1000, height = 700,compression = 'lzw')
network_meta <- qgraph(network_data,layout = "spring",minimum=0.5,cut=100,labels=names,label.scale=FALSE,
                       label.cex = label.cex,vsize=size_edges,shape=shape,grey=T,color=color,borders = FALSE,posCol = "grey")
legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))
#legend(-1.32,-0.5	, bty="n",c("EA: Efeitos Adversos","OT: Outro Tratamento","ECR: Questões com o ECR","FR: Falha no Retorno","MD: Problemas com medicamentos","ST: Melhora nos Sintomas","QF: Questões Familiares","OU: Outras Razões"),cex=1.2)
dev.off()

####### QUALITY GRAPH ######
q<-read.csv("/Users/oper/Desktop/Tables Depression_Atrittion - quality_graph.csv")
q<-remove.vars(q,"RBIAS")
barplot(as.matrix(q))
