---
title: "Standardisation Indirecte"
author: "Gwladys BARETH"
date: "22/02/2022"
output:
  html_document: 
    df_print: paged
    toc: true
    number_sections: true
    toc_float: true
    theme :  "cerulean"
    #highlight : "pygments"
     
editor_options: 
  chunk_output_type: console
---

> Objectif : Y-a-t-il un sur-risque de cancer dans les communes d'Antissis et de Tildam par rapport à la région de Mutrador ? 
<br />
<br />


# Description des jeux de données : {.tabset}


```{r}
Cas_Antissis_Tildam <- read.csv2("R:\\Masters\\Stage_G_BARETH\\SIR\\Distribu_ok\\Cas_de_cancers_Tildam_et_Antissis.csv")
Cas_Mutrador <- read.csv2("R:\\Masters\\Stage_G_BARETH\\SIR\\Distribu_ok\\Cas_de_cancers_Mutrador(ref).csv")
Population_totale_Tildam_Antissis <- read.csv2("R:\\Masters\\Stage_G_BARETH\\SIR\\Distribu_ok\\Population_totale_de_Mutrador(ref).csv")

```

## Cas_Mutrador : 
```{r}
head(Cas_Mutrador)
```
Le tableau de données `Cas_Mutrador` correspond aux **<u>cas de cancer</u>** de toute la région de Mutrador, sans distinction de commune. On peut y retrouver le nombre de personnes `K` ayant eu un cancer pour un âge donné `AGE_CL1`, un sexe `SEXE` donné et pour une année donnée `ANNEE`; ainsi que le taux d'incidence spécifique `TISp` à ces paramètres pour les années de 2008 à 2018. <br /> <br />

## Cas_Antissis_Tildam : 
```{r}
head(Cas_Antissis_Tildam)
```
Le tableau de données `Cas_Antissis_Tildam`  classe les cas de cancer dans les communes d'Antissis (code INSEE: 98436) et de Tildam (code INSEE 97551) en fonction du sexe `SEXE`, de l'année de découverte du cancer `DATINCID`, de l'âge au diagnosique `AGE_CL1` et du nom de la commune `COMMUNE`, pour les années de 2008 à 2018 `ANNEE`.  <br /> <br />

## Population_totale_Tildam_Antissis : 
```{r}
head(Population_totale_Tildam_Antissis)
```
Le tableau de données `Population_totale_Tildam_Antissis` correspond à la population totale dans les communes de Tildam et d'Antissis, classée en fonction de son âge `AGE_CL1`, sexe `SEXE`, commune `CINSEE` en fonction de l'année `ANNEE`. La colonne `PA` correspond au nombre de personne-année sur une année, soit la moyenne du nombre de personne pour l'année en cours, pour un âge, un sexe et une commune donnés. <br /> <br />

# Nombre de cas observés de cancer  {.tabset}

<div style="text-indent: 15px;">Pour se familiariser avec tout cela, on peut commencer par chercher le nombre de cas de cancer observés en Mutrador sur la période 2008 à 2018. Nous aurons besoin du tableau `Cas_Mutrador`. Pour avoir le nombre total de cas de cancer, il nous suffit d'additionner tous les "k" présents dans ce tableau de données. </div>  

<br />

## En Mutrador : 

```{r}
Cas_Mutrador <- read.csv2("R:\\Masters\\Stage_G_BARETH\\SIR\\Distribu_ok\\Cas_de_cancers_Mutrador(ref).csv")
Nombre_cas_obs_Mutrador <- sum(Cas_Mutrador$K)
Nombre_cas_obs_Mutrador
# Création d'une fontion qui effectuera la somme des "k" selon les conditions données :
Nombre_cas_obs <- function (y){return(sum(Cas_Mutrador$K[y]))}

# Si nous avions voulu le nombre de cas de cancer en Mutrador en fonction du sexe, voici ce que nous devons faire :
Nombre_cas_obs_Mutrador_Hommes <- Nombre_cas_obs(Cas_Mutrador$SEXE=="M")
print(Nombre_cas_obs_Mutrador_Femmes <- Nombre_cas_obs(Cas_Mutrador$SEXE=="F"))

# Nombre de cancers observés par classe d'âge : 
# Attention  : Il faut utiliser la fonction as.numeric() puisque la colonne "Cas_Mutrador$AGE_CL1" est considérée comme une chaine de caractère et non pas une variable numérique ce qui entraîne des erreurs de calculs.

Cas_Mutrador$AGE_CL1 <- as.numeric(Cas_Mutrador$AGE_CL1)
Nombre_cas_obs_Mutrador_0_24 <- Nombre_cas_obs(Cas_Mutrador$AGE_CL1>= 0 & Cas_Mutrador$AGE_CL1<= 24)
Nombre_cas_obs_Mutrador_25_49 <- Nombre_cas_obs(Cas_Mutrador$AGE_CL1>= 25 & Cas_Mutrador$AGE_CL1<= 49)
Nombre_cas_obs_Mutrador_50_74 <- Nombre_cas_obs(Cas_Mutrador$AGE_CL1>= 50 & Cas_Mutrador$AGE_CL1<= 74)
Nombre_cas_obs_Mutrador_75 <- Nombre_cas_obs(Cas_Mutrador$AGE_CL1>= 75)

# Nombres de cancers observés durant différentes périodes. 
Nombre_cas_obs_Mutrador_2008_2012 <- Nombre_cas_obs(Cas_Mutrador$ANNEE>=2008 & Cas_Mutrador$ANNEE<=2012)
Nombre_cas_obs_Mutrador_2013_2018 <- Nombre_cas_obs(Cas_Mutrador$ANNEE>=2013 & Cas_Mutrador$ANNEE<=2018)
```

## à Antissis (98436) :


Nous allons créer une nouvelle fonction `Nombre_cas_obs_Antissis` qui retournera la longueur de la colonne `NUM_IDPAT` de la table `Cas_Antissis_Tildam` (soit le nombre de personnes) respectant le fait que le code Insee soit égal à "98436", plus les conditions que nous allons définir au cas par cas. 

```{r, echo=c(4:8,11)}
Cas_Antissis_Tildam <- read.csv2("R:\\Masters\\Stage_G_BARETH\\SIR\\Distribu_ok\\Cas_de_cancers_Tildam_et_Antissis.csv")
names(Cas_Antissis_Tildam)
Cas_Antissis_Tildam$AGE_CL1 <- as.numeric(Cas_Antissis_Tildam$AGE_CL1)

Nombre_cas_obs_Antissis <- function(conditions) {return(length(Cas_Antissis_Tildam$NUM_IDPAT[Cas_Antissis_Tildam$CINSEE=="98436" & conditions ])) }

# Examples d'utilisation de la fonction pour créer des variables :
Nombre_cas_obs_Antissis_total <- Nombre_cas_obs_Antissis(NA)
Nombre_cas_obs_Antissis_Hommes <- Nombre_cas_obs_Antissis(Cas_Antissis_Tildam$SEXE=="M")
Nombre_cas_obs_Antissis_Femmes <- Nombre_cas_obs_Antissis(Cas_Antissis_Tildam$SEXE=="F")
Nombre_cas_obs_Antissis_0_24 <- Nombre_cas_obs_Antissis(Cas_Antissis_Tildam$AGE_CL1 <= 24)
Nombre_cas_obs_Antissis_25_49 <- Nombre_cas_obs_Antissis(Cas_Antissis_Tildam$AGE_CL1 >= 25 & Cas_Antissis_Tildam$AGE_CL1 <= 49)
Nombre_cas_obs_Antissis_50_74 <- Nombre_cas_obs_Antissis(Cas_Antissis_Tildam$AGE_CL1 >= 50 & Cas_Antissis_Tildam$AGE_CL1 <= 74)
Nombre_cas_obs_Antissis_75 <- Nombre_cas_obs_Antissis(Cas_Antissis_Tildam$AGE_CL1 >= 75)
Nombre_cas_obs_Antissis_2008_2012 <- Nombre_cas_obs_Antissis(Cas_Antissis_Tildam$ANNEE>=2008 & Cas_Antissis_Tildam$ANNEE<=2012)
Nombre_cas_obs_Antissis_2013_2018 <- Nombre_cas_obs_Antissis(Cas_Antissis_Tildam$ANNEE>=2013 & Cas_Antissis_Tildam$ANNEE<=2018)
```

## à Tildam (97551) : 

Utilisons les mêmes paramètres et conditions que la fonction `Nombre_cas_obs_Antissis` crée ci-dessus, mais en changeant simplement le fait que le code Insee soit égal à "97551". 
```{r, echo=c(4:8)}
Cas_Antissis_Tildam <- read.csv2("R:\\Masters\\Stage_G_BARETH\\SIR\\Distribu_ok\\Cas_de_cancers_Tildam_et_Antissis.csv")
Cas_Antissis_Tildam$AGE_CL1 <- as.numeric(Cas_Antissis_Tildam$AGE_CL1)

Nombre_cas_obs_Tildam <- function(conditions) {return(length(Cas_Antissis_Tildam$NUM_IDPAT[Cas_Antissis_Tildam$CINSEE=="97551" & conditions ])) }

# Examples d'utilisation de la fonction pour créer des variables :
Nombre_cas_obs_Tildam_total <- Nombre_cas_obs_Tildam(NA)
Nombre_cas_obs_Tildam_Hommes <- Nombre_cas_obs_Tildam(Cas_Antissis_Tildam$SEXE=="M")
Nombre_cas_obs_Tildam_Femmes <- Nombre_cas_obs_Tildam(Cas_Antissis_Tildam$SEXE=="F")
Nombre_cas_obs_Tildam_0_24 <- Nombre_cas_obs_Tildam(Cas_Antissis_Tildam$AGE_CL1 <= 24)
Nombre_cas_obs_Tildam_25_49 <-  Nombre_cas_obs_Tildam(Cas_Antissis_Tildam$AGE_CL1 >= 25 & Cas_Antissis_Tildam$AGE_CL1 <= 49)
Nombre_cas_obs_Tildam_50_74 <- Nombre_cas_obs_Tildam(Cas_Antissis_Tildam$AGE_CL1 >= 50 & Cas_Antissis_Tildam$AGE_CL1 <= 74)
Nombre_cas_obs_Tildam_75 <- Nombre_cas_obs_Tildam(Cas_Antissis_Tildam$AGE_CL1 >= 75)
Nombre_cas_obs_Tildam_2008_2012 <- Nombre_cas_obs_Tildam(Cas_Antissis_Tildam$ANNEE>=2008 & Cas_Antissis_Tildam$ANNEE<=2012)
Nombre_cas_obs_Tildam_2013_2018 <- Nombre_cas_obs_Tildam(Cas_Antissis_Tildam$ANNEE>=2013 & Cas_Antissis_Tildam$ANNEE<=2018)
```
 

# Création d'un tableau de données. {.tabset} 
## Le tableau : 
Maintenant que nous avons toutes ces données; essayons de créer le tableau final, puis assignons les valeurs correspondantes à la première colonne. Pour commencer, il nous faut 14 colonnes et 9 lignes pour ce tableau. 

```{r}
zone_Etude <- data.frame(matrix(NA,ncol = 14, nrow = 9))
```

```{r, echo=FALSE}
zone_Etude
```

<div style="padding-top:10px; padding-bottom:10px; padding-left:5px; border: 2px solid #A0A0A0; text-align: left;"> Petit point sur l'affichage des tableaux en Marckdown :<br /> Afin d'avoir un affichage stylisé, il faut ajouter la ligne de commande : **"df_print: paged"** comme dans l'exemple ci-contre :<br /><br />
<div style="text-indent: 15px;">output:</div>
<div style="text-indent: 30px;">html_document: </div>
<div style="text-indent: 30px;">df_print: paged</div><br />
Ensuite pour afficher un tableau avec ce style, il faut créer un chunk et simplement mettre le nom du tableau sans rien ajouter. </div>

***Penser à rendre le tableau intéractif avec : https://thinkr.fr/tableaux-interactifs-avec-r-pour-shiny-et-vos-pages-web/***
<div style="text-align: center;"><a>https://bookdown.org/yihui/rmarkdown/html-document.html#data-frame-printing</a></div>


<br />

## Noms des colonnes et lignes : 

```{r}
colnames(zone_Etude) <- c(
  "Cas observés en Mutrador",
  "Taux d incidence spécifique Mutrador",
  "Cas observés à Antissis",
  "Cas attendus à Antissis",
  "RSI Antissis",
  "Borne inf SIR Antissis",
  "Borne sup SIR Antissis",
  "p_value_Antissis",
  "Cas observés à Tildam",
  "Cas attendus à Tildam",
  "RSI Tildam",
  "Borne inf SIR Tildam",
  "Borne sup SIR Tildam", 
  "p_value_Tildam")  
row.names(zone_Etude) <- c("Total", "Hommes", "Femmes", "[00-24]", "[25-49]", "[50-74]", "[75 et plus]","[2008-2012]","[2013-2018]")
```

```{r,cols.print=4, echo=FALSE}
zone_Etude
```

## Intégration des données : {.tabset}
### A Mutrador : 
Maintenant remplissons la première colonne : [x,y] **x** fait référence à la ligne et **y** à la colonne.  
```{r}
zone_Etude[,1] <- c(
  Nombre_cas_obs_Mutrador,
  Nombre_cas_obs_Mutrador_Hommes, 
  Nombre_cas_obs_Mutrador_Femmes,
  Nombre_cas_obs_Mutrador_0_24,
  Nombre_cas_obs_Mutrador_25_49, 
  Nombre_cas_obs_Mutrador_50_74, 
  Nombre_cas_obs_Mutrador_75, 
  Nombre_cas_obs_Mutrador_2008_2012,
  Nombre_cas_obs_Mutrador_2013_2018
)
```

```{r, echo=FALSE}
zone_Etude
```

### Cancer observé à Antissis : 
```{r}
zone_Etude[,3] <- c(Nombre_cas_obs_Antissis_total, 
                    Nombre_cas_obs_Antissis_Hommes, 
                    Nombre_cas_obs_Antissis_Femmes, 
                    Nombre_cas_obs_Antissis_0_24, 
                    Nombre_cas_obs_Antissis_25_49, 
                    Nombre_cas_obs_Antissis_50_74,
                    Nombre_cas_obs_Antissis_75,
                    Nombre_cas_obs_Antissis_2008_2012, 
                    Nombre_cas_obs_Antissis_2013_2018)


```

```{r, echo=FALSE}
zone_Etude
```

### Cancer observés à Tildam  : 

```{r}
zone_Etude[,9] <- c(Nombre_cas_obs_Tildam_total,
                    Nombre_cas_obs_Tildam_Hommes, 
                    Nombre_cas_obs_Tildam_Femmes, 
                    Nombre_cas_obs_Tildam_0_24, 
                    Nombre_cas_obs_Tildam_25_49, 
                    Nombre_cas_obs_Tildam_50_74, 
                    Nombre_cas_obs_Tildam_75,
                    Nombre_cas_obs_Tildam_2008_2012, 
                    Nombre_cas_obs_Tildam_2013_2018)
```

```{r, echo=FALSE}
zone_Etude
```


<br />
<br />

# Taux d'incidence spécifique : TIS {.tabset}

## Calcul : 
<div style="padding-top:10px; padding-bottom:10px;border: 3px solid #A0A0A0; text-align: center;"> "Le taux spécifique pour la classe d'âge i, que nous appelerons Ti , peut aussi être calculé simplement comme un taux pour 100 000 en divisant le nombre de cas dans la classe d'âge $O_i$ par le nombre correspondant de personnes-années exposées $N_i$ et multipliant le résultat par 100.000."
$$T_i = \frac{O_i}{N_i}*100.000 $$<br /> (*cf : Méthodes statistiques pour les registres, Par le centre international de Recherche sur le Cancer*)</div>
<br />
<u>Taux d'incidence spécifique global</u><br />
Le taux d'incidence spécifique global en Mutrador, tous âges confondus et sexe confondus, est calculé par le nombre total des cas observés en Mutrador divisé par le nombre total de personne-années observées. <br />
Comme l'opération sera répétée plusieurs fois, il est possible de créer une fonction qui sera appelée par la suite : `Calcul_TIS`. Le paramètre `pour_habitants` est par défaut mis à 100.000, mais il est possible de changer cette valeur lors de l'appel de la fonction en précisant la nouvelle valeur à mettre. Attention, il est important de préciser à R que la colonne `AGE_CL1` dans le tableau `Cas_Mutrador` est une donnée numérique puisque cela influe sur les calculs. 

```{r, echo=c(1:8, 11)}
Cas_Mutrador$AGE_CL1 <- as.numeric(Cas_Mutrador$AGE_CL1)

Calcul_TIS <- function(nombre_cas_observe, Personne_annee_Mutrador, pour_habitants = 100000 ) {return(nombre_cas_observe/sum(Personne_annee_Mutrador)*pour_habitants)}

TIS_global <- Calcul_TIS(nombre_cas_observe = Nombre_cas_obs_Mutrador, Personne_annee_Mutrador = Cas_Mutrador$PA_REV)

# Exemple pour les sous-groupes  : 
TIS_Hommes <- Calcul_TIS(Nombre_cas_obs_Mutrador_Hommes, Cas_Mutrador$PA_REV[Cas_Mutrador$SEXE=="M"])
TIS_Femmes <- Calcul_TIS(Nombre_cas_obs_Mutrador_Femmes, Cas_Mutrador$PA_REV[Cas_Mutrador$SEXE=="F"])
TIS_0_24 <- Calcul_TIS(Nombre_cas_obs_Mutrador_0_24, Cas_Mutrador$PA_REV[Cas_Mutrador$AGE_CL1>= 0 & Cas_Mutrador$AGE_CL1<= 24])
TIS_25_49 <- Calcul_TIS(Nombre_cas_obs_Mutrador_25_49, Cas_Mutrador$PA_REV[Cas_Mutrador$AGE_CL1>= 25 & Cas_Mutrador$AGE_CL1<= 49])
TIS_50_74 <- Calcul_TIS(Nombre_cas_obs_Mutrador_50_74, Cas_Mutrador$PA_REV[Cas_Mutrador$AGE_CL1>= 50 & Cas_Mutrador$AGE_CL1<= 74])
TIS_75 <- Calcul_TIS(Nombre_cas_obs_Mutrador_75, Cas_Mutrador$PA_REV[Cas_Mutrador$AGE_CL1>= 75])
TIS_2008_2012 <- Calcul_TIS(Nombre_cas_obs_Mutrador_2008_2012, Cas_Mutrador$PA_REV[Cas_Mutrador$ANNEE>= 2008 & Cas_Mutrador$ANNEE <=2012] )
TIS_2013_2018 <- Calcul_TIS(Nombre_cas_obs_Mutrador_2013_2018, Cas_Mutrador$PA_REV[Cas_Mutrador$ANNEE>= 2013 & Cas_Mutrador$ANNEE<=2018] )
```

## Intégration des données :

Créons la deuxième colonne du tableau : 
```{r}
zone_Etude[,2] <- c(TIS_global, TIS_Hommes, TIS_Femmes, TIS_0_24, TIS_25_49, TIS_50_74, TIS_75, TIS_2008_2012, TIS_2013_2018)
```

```{r,cols.print=4, echo=FALSE}
zone_Etude
```


# Nombre de cas de cancer attentus {.tabset}

## Calculs des nombres de cas de cancers attendus à Antissis et Tildam :

Vient la partie un peu plus délicate. <br/>
En effet, afin de calculer le nombre de cas attendus, il nous d'abord découper les populations d'Antissis et de Tildam en fonction de l'âge de chaque personne, de leur année de diagnostique, de leur sexe et de leur commune. 
Pour ce faire, nous allons créer un second tableau `Tableau_Mutrador_vs_Antissis_Tildam` qui nous permettra d'organiser toutes ces données. 

<u>Explication du code :</u> <br/><br/>
Mon tableau de données `data.frame(matrix)` **Tableau_Mutrador_vs_Antissis_Tildam** contient 8 colonnes (ncol = 8) et le nombre de ligne n'est pas spécifié puisque je ne sais pas de combien j'en aurais besoin `matrix(NA,ncol = 8)`. <br/>
La fonction `colnames()` permet de donner un nom aux colonnes.<br/>

```{r, eval=FALSE}
Tableau_Mutrador_vs_Antissis_Tildam <- data.frame(matrix(NA,ncol = 8))
colnames(Tableau_Mutrador_vs_Antissis_Tildam) <- c("Commune",
                                     "Annee_du_diag",
                                     "Sexe",
                                     "age_du_patient",
                                     "nombre_de_cas_obs",
                                     "TIS",
                                     "population", 
                                     "nombre_de_cas_attendus")
```


Je crée une variable `"position"` qui me permettra d'indiquer où les données doivent être inscrites dans le tableau. Elle prend la position "1" puisque je veux que l'on commence à la première ligne du tableau. <br/>
```{r, eval=FALSE}
position <- 1
```

Ensuite vient la `boucle for`, qui dit que : <br/>
Pour chaque paramètre dans la liste ci-dessous : <br/>
-- l'âge allant de 0 à 99 et pour année de diagnostic allant de 2008 à 2018<br/> 
--pour chaque sexe (Homme ou Femme) <br/>
-- et enfin pour chacune des deux communes, <br/>
Je veux qu'il me retourne le nombre de personnes ayant un cancer à Tildam et Antissis  `variable X`, le taux d'incidence spécifique en Mutrador et le nombre de personne en Mutrador correspondant aux paramètres en cours. 
```{r, eval=FALSE}
for (age in 0:99) {
   for (annee in 2008:2018) {
     for (sexe in list("F","M")) {
       for (INSEE in list("97551","98436")) {puis ce que la boucle for() doit faire}
```

Enfin je souhaites qu'il assigne ces données dans les cellules du tableau correspondantes et qu'à chaque tour de boucle, il décale d'une ligne pour écrire sur la suivante.  <br/>
```{r, eval=FALSE}
Tableau_Mutrador_vs_Antissis_Tildam [position,] <- c(...)

position <- position + 1
```

   
```{r}
#Voici cette portion du code en entier : 

Cas_Antissis_Tildam <- read.csv2("R:\\Masters\\Stage_G_BARETH\\SIR\\Distribu_ok\\Cas_de_cancers_Tildam_et_Antissis.csv")
Population_totale_Tildam_Antissis <- read.csv2("R:\\Masters\\Stage_G_BARETH\\SIR\\Distribu_ok\\Population_totale_de_Mutrador(ref).csv")

Tableau_Mutrador_vs_Antissis_Tildam <- data.frame(matrix(NA,ncol = 8))
colnames(Tableau_Mutrador_vs_Antissis_Tildam) <- c("Commune",
                                     "Annee_du_diag",
                                     "Sexe",
                                     "age_du_patient",
                                     "nombre_de_cas_obs",
                                     "TIS",
                                     "population", 
                                     "nombre_de_cas_attendus")
position <- 1

for (age in 0:99) {
   for (annee in 2008:2018) {
     for (sexe in list("F","M")) {
       for (INSEE in list("97551","98436")) {
         
      Cas_Antissis_Tildam$AGE_CL1 <- as.numeric(Cas_Antissis_Tildam$AGE_CL1)   
      Cas_Antissis_Tildam$CINSEE <- as.numeric(Cas_Antissis_Tildam$CINSEE)
      Cas_Antissis_Tildam$ANNEE <- as.numeric(Cas_Antissis_Tildam$ANNEE)
      
      x <- length(which(Cas_Antissis_Tildam$SEXE == sexe 
                        & Cas_Antissis_Tildam$ANNEE == annee 
                        & Cas_Antissis_Tildam$CINSEE == INSEE 
                        & Cas_Antissis_Tildam$AGE_CL1 == age ))
      
        TISp <- Cas_Mutrador$TISp[which((Cas_Mutrador$ANNEE == annee
                                & Cas_Mutrador$SEXE == sexe
                                & Cas_Mutrador$AGE_CL1 == age))]
        
        population <- Population_totale_Tildam_Antissis$PA[which((Population_totale_Tildam_Antissis$ANNEE == annee
                                    & Population_totale_Tildam_Antissis$SEXE == sexe
                                    & Population_totale_Tildam_Antissis$CINSEE == INSEE 
                                    & Population_totale_Tildam_Antissis$AGE_CL1 == age))]
                               
      Tableau_Mutrador_vs_Antissis_Tildam [position,] <- c(if(is.null(INSEE))NA else INSEE,
                                             if(is.null(annee))NA else annee,
                                             if(is.null(sexe))NA else sexe,
                                             if(is.null(age))NA else age,
                                             if(is.null(x))NA else x,
                                             if(is.null(TISp))NA else TISp, 
                                             population, 
                                             NA)
      
          position <- position + 1
         }
       }
     }
}
```

```{r, echo=FALSE}
Tableau_Mutrador_vs_Antissis_Tildam
```

## Le calcul : {.tabset}


### Première étape : 

<div style="padding-top:10px; padding-bottom:10px;border: 3px solid #A0A0A0; text-align: center;">Le nombre de cas attendus (**Ai**) est calculé en appliquant les taux par âges théoriques (**Ti**) à la population étudiée: 
 $$ \sum_{i=1}^{n} A_{i} = \sum_{i=1}^{n}\frac{T_{i}N_{i}}{100000} $$
ou un **Ai** , le nombre de cas attendus dans la classe d'âge i, est le produit du "taux théorique" et du nombre de personnes dans la classe d'âge i (**Ni**) dans la population étudiée.</div><br/>

Dans notre exemple, **Ai** correspond aux Taux d'incidence spécifique (TIS) calculé en Mutrador (notre population de référence), **Ni** au nombre total de personnes résidants à Antissis ou Tildam. 
```{r}
Tableau_Mutrador_vs_Antissis_Tildam$nombre_de_cas_attendus <- as.numeric(Tableau_Mutrador_vs_Antissis_Tildam$TIS)/100000*as.numeric(Tableau_Mutrador_vs_Antissis_Tildam$population)
```

<br />
```{r, echo=FALSE, cols.print=6 }
Tableau_Mutrador_vs_Antissis_Tildam
```

<u>Si on lisait la première ligne de tableau, cela donnerait</u> : <br />
Dans la commune de Tildam (97551), en 2008, il y avait 0 personne de 0 an ayant un cas de cancer, 26.288 personnes de 0 an dans la population entière de Tildam. Le TIS est à 0 et le nombre de cas attendus l'est également. Le calcul du nombre de cas attendus pour ces paramètres se fait comme ceci : 
 $$ A_{0,F,97551,2008} = \frac{0*26.287}{100000} $$
 
 <u>Autre exemple, ligne 4337</u>  (onglet 434 du tableau): Dans la commune de Tildam (97551), en 2014, il y avait 0 cas de cancer pour les femmes âgées de 98 ans. On retrouvait 7.043 femmes âgées de 98 ans dans la population totale de Tildam, et le TIS était de 1694.915. Le nombre de cas attendus était de 0.11937. Le calcul du nombre de cas attendus pour ces paramètres se fait comme ceci (en arrondissant) : 
 $$ A_{98,F,97551,2008} = \frac{1694.915*7.043}{100000} = 0.11937 $$
 <br /> 
On parle dans cet exemple de 7.043 femmes âgées de 98 ans, cela s'explique par le fait que l'on ait contabilisé le nombre de femme de 98 ans au début de l'année 2014 puis le nombre de femme à la fin de cette même année et que l'on en ai fait une moyenne. Ce résultat s'exprime en Personne-année. 

### Seconde étape : 

Maintenant que nous avons le nombre de cas attendus pour chaque année en fonction de l'âge, du sexe et de la commune; il suffit de les additionner pour pouvoir compléter le tableau. Là encore, l'opération va être faite un bon nombre de fois, d'où la création de deux nouvelles fonctions `Nombre_cas_attendus_Antissis()` et `Nombre_cas_attendus_Tildam()`

<u>*Exemples de calculs pour le nombre de cas de cancer attendus à Antissis et Tildam: *</u>
```{r, echo=c(1:6, 10, 15:20)}
Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient <- as.numeric(Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient)

#A Antissis : 
Nombre_cas_attendus_Antissis <- function(x) {return(sum(Tableau_Mutrador_vs_Antissis_Tildam$nombre_de_cas_attendus[Tableau_Mutrador_vs_Antissis_Tildam$Commune == 98436 & x] ))}

Nombre_cas_attendus_Antissis_global <- sum(Tableau_Mutrador_vs_Antissis_Tildam$nombre_de_cas_attendus[Tableau_Mutrador_vs_Antissis_Tildam$Commune == 98436])
Nombre_cas_attendus_Antissis_Hommes <- Nombre_cas_attendus_Antissis(x = (Tableau_Mutrador_vs_Antissis_Tildam$Sexe == "M"))
Nombre_cas_attendus_Antissis_Femmes <- Nombre_cas_attendus_Antissis(Tableau_Mutrador_vs_Antissis_Tildam$Sexe == "F")
Nombre_cas_attendus_Antissis_0_24 <- Nombre_cas_attendus_Antissis(Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient >=0 & Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient <=24)
Nombre_cas_attendus_Antissis_25_49 <- Nombre_cas_attendus_Antissis(Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient >=25 & Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient <=49 )
Nombre_cas_attendus_Antissis_50_74 <- Nombre_cas_attendus_Antissis(Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient >=50 & Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient <=74)
Nombre_cas_attendus_Antissis_75 <- Nombre_cas_attendus_Antissis(Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient >=75)
Nombre_cas_attendus_Antissis_2008_2012 <- Nombre_cas_attendus_Antissis(Tableau_Mutrador_vs_Antissis_Tildam$Annee_du_diag <=2012 & Tableau_Mutrador_vs_Antissis_Tildam$Annee_du_diag >=2008)
Nombre_cas_attendus_Antissis_2013_2018 <- Nombre_cas_attendus_Antissis(Tableau_Mutrador_vs_Antissis_Tildam$Annee_du_diag <=2018 & Tableau_Mutrador_vs_Antissis_Tildam$Annee_du_diag >=2013)

#A Tildam : 
Nombre_cas_attendus_Tildam <- function(x) {return(sum(Tableau_Mutrador_vs_Antissis_Tildam$nombre_de_cas_attendus[Tableau_Mutrador_vs_Antissis_Tildam$Commune == 97551 & x] ))}

Nombre_cas_attendus_Tildam_global <- sum(Tableau_Mutrador_vs_Antissis_Tildam$nombre_de_cas_attendus[Tableau_Mutrador_vs_Antissis_Tildam$Commune == 97551])
Nombre_cas_attendus_Tildam_Hommes <- Nombre_cas_attendus_Tildam(x = (Tableau_Mutrador_vs_Antissis_Tildam$Sexe == "M"))
Nombre_cas_attendus_Tildam_Femmes <- Nombre_cas_attendus_Tildam(Tableau_Mutrador_vs_Antissis_Tildam$Sexe == "F")
Nombre_cas_attendus_Tildam_0_24 <- Nombre_cas_attendus_Tildam(Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient >=0 & Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient <=24)
Nombre_cas_attendus_Tildam_25_49 <- Nombre_cas_attendus_Tildam(Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient >=25 & Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient <=49 )
Nombre_cas_attendus_Tildam_50_74 <- Nombre_cas_attendus_Tildam(Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient >=50 & Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient <=74)
Nombre_cas_attendus_Tildam_75 <- Nombre_cas_attendus_Tildam(Tableau_Mutrador_vs_Antissis_Tildam$age_du_patient >=75)
Nombre_cas_attendus_Tildam_2008_2012 <- Nombre_cas_attendus_Tildam(Tableau_Mutrador_vs_Antissis_Tildam$Annee_du_diag <=2012 & Tableau_Mutrador_vs_Antissis_Tildam$Annee_du_diag >=2008)
Nombre_cas_attendus_Tildam_2013_2018 <- Nombre_cas_attendus_Tildam(Tableau_Mutrador_vs_Antissis_Tildam$Annee_du_diag <=2018 & Tableau_Mutrador_vs_Antissis_Tildam$Annee_du_diag >=2013)

```


```{r, echo=FALSE}
zone_Etude[,4] <- c(Nombre_cas_attendus_Antissis_global, 
                    Nombre_cas_attendus_Antissis_Hommes, 
                    Nombre_cas_attendus_Antissis_Femmes, 
                    Nombre_cas_attendus_Antissis_0_24, 
                    Nombre_cas_attendus_Antissis_25_49, 
                    Nombre_cas_attendus_Antissis_50_74, 
                    Nombre_cas_attendus_Antissis_75, 
                    Nombre_cas_attendus_Antissis_2008_2012, 
                    Nombre_cas_attendus_Antissis_2013_2018)

zone_Etude[,10] <- c(Nombre_cas_attendus_Tildam_global, 
                    Nombre_cas_attendus_Tildam_Hommes, 
                    Nombre_cas_attendus_Tildam_Femmes, 
                    Nombre_cas_attendus_Tildam_0_24, 
                    Nombre_cas_attendus_Tildam_25_49, 
                    Nombre_cas_attendus_Tildam_50_74, 
                    Nombre_cas_attendus_Tildam_75, 
                    Nombre_cas_attendus_Tildam_2008_2012, 
                    Nombre_cas_attendus_Tildam_2013_2018)

```

```{r, echo=FALSE}
zone_Etude
```

# Ratio standardisé d'incidence : RSI {.tabset}

## Calcul : 

<div style="padding-top:10px; padding-bottom:10px;border: 3px solid #A0A0A0; text-align: center;">"Le taux standardisé sur l'âge est le taux théorique qui aurait été obtenu si les taux spécifiques observés pour chaque âge étaient appliqués à une population standard : cette population est communément appelée la population de référence. Le ratio standardisé (M) peut être calculé en comparant le nombre de cas observé (Oi) avec celui attendu (Ai).
$$ M = \frac{\sum O_i}{\sum A_i}$$ 
**Il est généralement exprimé sous forme de pourcentage en multipliant par 100.** S'il est appliqué aux données d'incidence, il est communément connu comme étant le RSI (Ratio Standardisé d'incidence); s'il est appliqué aux données de mortalité, il est connu comme étant le RSM (Ratio Standardisé de Mortalité).</div> 
<br/>
Appliquons ce calcul pour chacun de nos sous-groupes : 
```{r}
zone_Etude[,5]<- zone_Etude[,3]/zone_Etude[,4]
zone_Etude[,11]<- zone_Etude[,9]/zone_Etude[,10]
```

<u>Explication du code</u> : <br/>
Pour chaques lignes de la colonne n°5 `RSI Antissis` du tableau `zone_Etude`, divise la ligne correspondante de la colonne n°3 `Cas observés à Antissis` par celle de la colonne n°4 `Cas attendus à Antissis`.  

```{r, echo=FALSE}
zone_Etude
```

## Interprétation : 

```{r, echo=FALSE}
zone_Etude
```

Le ratio standardisé d'incidence de 0.864 dans la population totale d'Antissis signifie que l'incidence des cancers dans la population totale de Antissis est divisée de 0.136 fois (soit 13.6%) par rapport à l'incidence dans la population totale de Mutrador. <br/><br/>
<u>Autre exemple</u> :<br/>
Le ratio standardisé d'incidence de 4.572 à Antissis chez les [00-24] ans signifie que l'incidence des cancers dans cette population spécidique d'Antissis est multipliée de 4.572 fois par rapport à l'incidence dans la population totale de Mutrador. <br/><br/>
**Mais attention ! Il faut savoir si l'intervalle de confiance à 95% comprend la valeur 1 pour chaque SIR avant de pouvoir conclure à un résultat significatif.**<br/><br/>


# Test de signification du SIR {.tabset}

## calcul : 

On ne pouvait pas faire ici, un test automatique du CHI carré (en tout cas je n'ai pas trouvé comment, si quelqu'un sait, je suis preneuse de la solution).

Il s'agit de tester l'hypothèse : $$ H_o : SIR = 1 $$
L'approche conventionnelle est la statistique du Chi-carré à 1 degré de liberté : 
$$ X^2 = \frac{(\mid O_i-Ai\mid -\frac{1}{2})^2}{A_i} \\\textrm{Si A < 10 alors on utilise le test suivant : }\\ X^2 = 9*O_i^** (1-\frac{1}{9*O_i^*}-(\frac{A_i}{O_i^*})^{\frac{1}{3}})^2 \\\textrm{Qui suit une loi à 1 dll avec } O_i^*=O_i \textrm{ si } O_i\geq A_i \textrm{ et } O_i^* = O +1 \textrm{ sinon. } $$ 


```{r}
Tableau_chi <- data.frame(matrix(nrow = 9,ncol = 2))
colnames(Tableau_chi)<- c("Chi_Antissis", 
                          "Chi_Tildam")
row.names(Tableau_chi) <- c("Total", "Hommes", "Femmes", "[00-24]", "[25-49]", "[50-74]", "[75 et plus]","[2008-2012]","[2013-2018]")

for(i in 1:nrow(zone_Etude)){
  if (zone_Etude[i,4]>=10) {
    X <- ((abs(zone_Etude[i,3]-zone_Etude[i,4])-(1/2))^2)/zone_Etude[i,4]
   Tableau_chi[i,1] <- X
  
  } else {
       if(zone_Etude[i,3]>=zone_Etude[i,4]) {
          Y <- zone_Etude[i,3]
          X <- 9*Y*((1-(1/(9*Y))-((zone_Etude[i,4]/Y)^(1/3)))^2)
           Tableau_chi[i,1] <- X
           
        } else {
            Y <- zone_Etude[i,3]+1
            X <- 9*Y*(1-(1/(9*Y))-((zone_Etude[i,4]/Y)^(1/3)))^2
            Tableau_chi[i,1] <- X
                }
  }
}

for(i in 1:nrow(zone_Etude)){
 if (zone_Etude[i,10]>=10) {
    X <- ((abs(zone_Etude[i,9]-zone_Etude[i,10])-(1/2))^2)/zone_Etude[i,10]
    Tableau_chi[i,2] <- X
  
  } else {
       if(zone_Etude[i,9]>=zone_Etude[i,10]) {
          Y <- zone_Etude[i,9]
          X <- 9*Y*((1-(1/(9*Y))-((zone_Etude[i,10]/Y)^(1/3)))^2)
           Tableau_chi[i,2] <- X
           
        } else {
            Y <- zone_Etude[i,9]+1
            X <- 9*Y*(1-(1/(9*Y))-((zone_Etude[i,10]/Y)^(1/3)))^2
            Tableau_chi[i,2] <- X
                }
  }
}

Tableau_chi
```


## P_value :  


La statistique du Chi-carré suit une loi à un 1 degré de liberté et si l'on prend un risque alpha de 5%, soit : $$ \chi_{ref} = 3.84 $$ <br/>

Calcul du petit p avec la fonction pchisq().   

```{r}
petit_p_Antissis <- pchisq(Tableau_chi[,1], df = 1, lower.tail = FALSE )
zone_Etude[,8]<- petit_p_Antissis
petit_p_Tildam <- pchisq(Tableau_chi[,2], df = 1, lower.tail = FALSE )
zone_Etude[,14]<- petit_p_Tildam 
zone_Etude
```

<u>Explications:</u><br/>
pchisq() gives the distribution function; <br/>
lower.tail -> if TRUE (default), probabilities are P[X ≤ x], otherwise, P[X > x].
<br/><br/>

## Interprétation : 

Voyons quelles sont les tests statistiques significatifs. 

```{r}
Significatif_Antissis1 <- subset(zone_Etude, zone_Etude[,8] < 0.05, c( 'RSI Antissis', "p_value_Antissis" ))
Significatif_Antissis1

Significatif_Tildam <- subset(zone_Etude, zone_Etude[,14]< 0.05, c( 'RSI Tildam', "p_value_Tildam"))
Significatif_Tildam
```
 
Il n'y a pas de différence significative entre le ratio standardisé d'incidence entre la population de Tildam et la région de Mutrador. <br/><br/>
Par contre : <br/>
-- Dans la population des 0-24 ans à Antissis durant la période 2008-2018, il y a 4.573 fois plus de risque d'incidence de cancer que par rapport à la population de Mutrador à la même période et pour la même tranche d'âge.<br/>
-- Dans la population des plus de 75 ans à Antissis durant la période 2008-2018, il y a une diminution de l'incidence de cancer de 36.5 % (1-0.3654) par rapport à la population de Mutrador à la même période et pour la même tranche d'âge.<br/>
-- Pour la population d'Antissis entre 2013 et 2018, on constate une diminution de l'incidence de 21.6 % (1-0.7841178) par rapport à la population de Mutrador pour la même période. <br/>


# Calcul de l'erreur type (intervalle de confiance) du RSI {.tabset}

## Première formule en fonction de la distribution de Poisson : l'approximation de Byar {.tabset}

### Formule : 

**Statistical Methods in Cancer Research Volume II: The Design and Analysis of Cohort Studies / IARC Scientific Publication No. 82** (peut être retrouvé en ligne) 

>*"Si le test de l'hypothèse nulle donne comme verdict "non significatif", il peut être important de démontrer que l'étude n'a pas suffisament de précision, pour mettre en évidence de larges différences entre la population d'étude et celle de référence. Ou, si le résultat est positif, quelqu'un pourrait vouloir examiner sa cohérence avec d'autres études. Mettre en place une intervalle de confiance des SIR peut permettre d'atteindre ces buts."*   


Plusieurs expressions ont été proposée pour obtenir un intervalle de confiance à 95% approché du SIR. Nous utiliserons en premier la formule donnée par les livres : *"Epidémiologie, Principes et méthodes quantitatives / INSERM"* et celui "*Statistical Methods in Cancer Research Volume II*" : <br/><br/>
<div style="padding-top:10px; padding-bottom:10px; padding-left:5px; border: 3px solid #A0A0A0; text-align: left;">$$ SIR_I = \frac{O_i}{Ai}*(1-\frac{1}{9*O_i}-\frac{Z_{\alpha/2}}{3*\sqrt{O_i}})^3 \\ 
SIR_S = \frac{O_i+1}{Ai}*(1-\frac{1}{9*(O_i+1)}-\frac{Z_{\alpha/2}}{3*\sqrt{O_i+1}})^3$$</div> 

### Calculs : 

Effectuons ces calculs : 
<br/>
```{r}
IC_1_inf <- function(obs,att){return ((zone_Etude[,obs]/zone_Etude[,att])*((1-(1/(9*(zone_Etude[,obs])))-(1.96/(3*sqrt(zone_Etude[,obs]))))^3))}
IC_1_sup <- function(obs,att){return (((((zone_Etude[,obs])+1))/zone_Etude[,att])*((1-(1/(9*((zone_Etude[,obs])+1)))+((1.960)/(3*sqrt((zone_Etude[,obs])+1))))^3))}
  
zone_Etude[,6] <- IC_1_inf(3,4)
zone_Etude[,7] <- IC_1_sup(3,4)
zone_Etude[,12] <- IC_1_inf(9,10)
zone_Etude[,13] <- IC_1_sup(9,10)

zone_Etude
```

### Interprétation : 

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(dplyr)

for(i in 1:nrow(zone_Etude)) {
 zone_Etude [i,15] <- between(1, zone_Etude[i,6], zone_Etude[i,7])
 zone_Etude [i,16] <- between(1, zone_Etude[i,12], zone_Etude[i,13])
}


Significatif_Antissis <- subset(zone_Etude, V15 == FALSE, c( 'RSI Antissis', 'Borne inf SIR Antissis', 'Borne sup SIR Antissis', "p_value_Antissis"))

Significatif_Tildam <- subset(zone_Etude, V16 == FALSE, c( 'RSI Tildam', 'Borne inf SIR Tildam', 'Borne sup SIR Tildam', "p_value_Tildam"))

 Significatif_Antissis
 Significatif_Tildam 
```
```{r, echo=FALSE}
zone_Etude  <- zone_Etude[,-16]
zone_Etude  <- zone_Etude[,-15]
```

Les intervalles de confiance ne comprenant pas la valeur "1" et étant donc significatives sont : <br/>
- Dans la population des 0-24 ans à Antissis durant la période 2008-2018, il y a 4.573 fois plus de risque d'incidence de cancer que par rapport à la population de Mutrador durant la même période et pour la même tranche d'âge. <br/>
- Chez les 75 et plus à Antissis durant la période 2008-2018,il y a une diminution du risque de 36.5% d'incidence de cancer par rapport à la population de Mutrador durant la même période et pour la même tranche d'âge. <br/>
- Durant la période 2013-2018 pour toute la population d'Antissis, on observe une diminution du risque de 21.6% par rapport à la population de Mutrator durant la même période. <br/>


**Mais attention** ! Tous ces résultats sont à interpréter avec une très grande prudence due aux faibles effectifs présents dans les populations étudiées. Avec de plus grands effectifs, l'intervalle de confiance à 95% se réduirait, entrainant peut être la significativité de certains résultats qui ne l'étaient pas auparavant, mais au contraire, pourrait avoir l'effet inverse en invalidant des résultats significatifs. <br/> <br/>

## Seconde formule {.tabset}

### Formule : 

Maintenant essayons avec une autre formule trouvée dans le livre : *"Epidémiologie" écrit par P.Czernichow, J.Chaperon et X. Le Coutour* et utilisé également dans le chapitre 9 *Méthodes statistiques pour les registres du centre international de recherche sur le cancer par P. Boyle et D.M.Parkin.*
<div style="padding-top:10px; padding-bottom:10px; padding-left:5px; border: 3px solid #A0A0A0; text-align: left;">Le ratio standardisé (M) est calculé par la formule précédemment écrite et sa variance, Var (M), est: 
$$ Var(M) = \frac{\sum O_i}{(\sum A_i)^2}  $$ 

L'écart-type de ce ratio indirect, e.t.(M), est la racine carrée de la variance Var (M). 

Si nous écrivons le RSI (rapport d'incidence standardisé) sous la forme $$ RSI = M*100 \hspace{5 mm} \textrm{ (sous forme de pourcentage)  alors}\\ Var(RSI) = Var (M*100) = 10.000 * Var(M) \hspace{5 mm} \textrm{d'où}\\ s.e.(RSI) = \sqrt{Var(RSI)} \\ \textrm{En conséquence, l'intervalle de confiance à 95% du RSI est :}\\ RSI \pm (Z_{\alpha/2}* s.e(RSI))$$</div> 
<br/><br/>
<div style="padding-top:10px; padding-bottom:10px; padding-left:5px; border: 3px solid #A0A0A0; text-align: left;"><u>Autre forme d'écriture :</u><br/>

$$ RSI = M \hspace{5 mm} \textrm{  alors}\\ Var(RSI) = Var (M) \hspace{5 mm} \textrm{d'où}\\ e.t.(RSI) = e.t.(M) = \sqrt{Var(M)} \\ \textrm{On peut en conclure que l'intervalle de confiance à 95% s'écrit comme suit :}\\ RSI \pm (Z_{\alpha/2}*e.t.(RSI)) \Leftrightarrow M \pm (1.96 * \sqrt{Var(M)}) \Leftrightarrow M \pm (1.96 * \sqrt{\frac{\sum O_i}{(\sum A_i)^2}}) $$</div>  

### Calculs : 

Effectuons ces calculs : 
```{r}
zone_Etude[,6]<- zone_Etude[,5] - (1.96*sqrt(zone_Etude[,3]/((zone_Etude[,4])^2)))
zone_Etude[,7]<- zone_Etude[,5] + (1.96*sqrt(zone_Etude[,3]/((zone_Etude[,4])^2)))
zone_Etude[,12]<-zone_Etude[,11] - (1.96*sqrt(zone_Etude[,9]/((zone_Etude[,10])^2)))
zone_Etude[,13]<-zone_Etude[,11] + (1.96*sqrt(zone_Etude[,9]/((zone_Etude[,10])^2))) 
```

```{r}
zone_Etude
#write.csv2(zone_Etude, file="Tableau du ratio d'incidence standardisé.csv", quote = FALSE, row.names = TRUE)
```

*La fonction write.csv2() me permet de créer un fichier csv de mon tableau de donnée : zone_Etude.* <br/> 

### Interprétation : 

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(dplyr)

for(i in 1:nrow(zone_Etude)) {
 zone_Etude [i,15] <- between(1, zone_Etude[i,6], zone_Etude[i,7])
 zone_Etude [i,16] <- between(1, zone_Etude[i,12], zone_Etude[i,13])
}


Significatif_Antissis <- subset(zone_Etude, V15 == FALSE, c( 'RSI Antissis', 'Borne inf SIR Antissis', 'Borne sup SIR Antissis', "p_value_Antissis"))

Significatif_Tildam <- subset(zone_Etude, V16 == FALSE, c( 'RSI Tildam', 'Borne inf SIR Tildam', 'Borne sup SIR Tildam', "p_value_Tildam"))

 Significatif_Antissis
 Significatif_Tildam 
```
```{r, echo=FALSE}
zone_Etude  <- zone_Etude[,-16]
zone_Etude  <- zone_Etude[,-15]
```
Les seules intervalles de confiance ne comprenant pas la valeur "1" et étant donc théoriquement significatives sont : <br/>
- A Antissis pour la classe d'âge des 75 ans et plus, une diminution de l'incidence du risque de cancer de 36.5% par rapport à l'incidence en Mutrador pour cette même classe d'âge, durant la période 2008-2018. <br/>
- A Antissis pour la période 2013-2018, une diminution de l'incidence du risque de cancer de 21.6% par rapport à l'incidence en Mutrador pour la même période. <br/>
- A Tildam pour la classe d'âge allant de 0 à 24 ans, une diminution du risque d'incidence de cancer de 58.4% par rapport à l'incidence en Mutrador pour cette même classe d'âge, durant la période 2008-2018. Cependant la p-value > 0,05 ce qui invalide ce résultat.  <br/>

<u>**Pour rappel**</u>: avec les calculs précédents de l'intervalle de confiance, nous avions ceci : 
```{r, echo=FALSE}
Significatif_Antissis1
```


# Commentaires

En fonction du calcul utilisé pour l'intervalle de confiance, nous obtenons des résultats  discordants avec ceux précédent, témoignant du manque de puissance dû aux effectifs très réduits. 

# Création d'un tableau de données intéractif : 

https://thinkr.fr/tableaux-interactifs-avec-r-pour-shiny-et-vos-pages-web/ 

Il faut tout d'abord utiliser le package `dt` qui se base sur la librairie JavaScript `DataTables`. 

> Note : Attention, comme je vous le disais en introduction de ce package, {DT} est issu d’une librairie JavaScript, il en découle un indiçage différent. Dans R, le premier élément d’un vecteur a pour indice 1, alors que dand JavaScript, il aura pour indice 0. Dans le cadre de nos tables, les noms des lignes est considérée comme la première colonne, donc d’indice 0 – jusque ici, pas de souci, notre colonne d’indice 1 dans le datatable correspondra bien à la colonne 1 de nos données R. Si maintenant on supprime les noms de lignes avec rownames = FALSE, c’est comme si on supprimait une colonne et les indices s’en trouvent décalés : la première colonne de nos données aura pour indice 0.


```{r}
#installation et chargement
# install.packages("DT")
library(DT)
```


```{r}
library(DT)
Arrondi_valeurs_colonne <- function(colonne, nombre_apres_virgule = 3){return(round(colonne, nombre_apres_virgule))}

for(i in 1:14){
  zone_Etude[,i] <- Arrondi_valeurs_colonne(zone_Etude[,i])
}

# HEADER <- htmltools::withTags(table(
#   class = 'display',
#   thead(
#     tr(
#       th(colspan = 2, "Mutrador"),
#       th(colspan = 6, "Antissis"),
#       th(colspan = 6, "Tildam"),
#      tr(
#        th(colspan = 1, rowspan = 2,"Cas observés"),
#        th(colspan = 1, rowspan = 2,"Taux d'incidence spécifique"),
#        th(colspan = 1, rowspan = 2,"Cas observés"),
#        th(colspan = 1, rowspan = 2,"Cas attendus"),
#        th(colspan = 1, rowspan = 2,"RSI"),
#        th(colspan = 2, rowspan = 1, "Intervalle de confiance"),
#        th(colspan = 1, rowspan = 2,"p_value"),
#        th(colspan = 1, rowspan = 2,"Cas observés"),
#        th(colspan = 1, rowspan = 2,"Taux d'incidence spécifique"),
#        th(colspan = 1, rowspan = 2, "Cas observés"),
#        th(colspan = 1, rowspan = 2, "Cas attendus"),
#        th(colspan = 1, rowspan = 2,"RSI"),
#        th(colspan = 2, rowspan = 1, "Intervalle de confiance"),
#        th(colspan = 1, rowspan = 2, "p_value"),
#      ),
#      tr(lapply(rep(c('Borne inf.', 'Borne sup.'), 2),),
#     ),
#   ),
# )))

datatable(zone_Etude, 
          #container = HEADER,
          class = "display compact",
          options = list(
            pageLength = 10,
            paging = TRUE,
            scroller = TRUE,
            scrollX = 400 
            )
          )
```



