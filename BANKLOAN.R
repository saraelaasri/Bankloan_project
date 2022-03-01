---
title: "Prédiction et modélisaton du prêt personnel bancaire"
author: "Ikram Fella, Sara Elaasri"
date: "21/12/2021"
output: html_document
---

#Introduction
Les prêts consistent un facteur primordial pour l'existence et la constance du métier bancaire, une procédure client-banque visant à générer les intérêts, principale source de bénéfice et de gain pour les banques . C'est pour cette raison là qu'une bonne analyse et étude des différents éléments influançant cet algorithme d'accord de crédit s'avère indispensable pour la maximisation du plus-value à apporter, raison qui explique les investissements apprécialbles des banques dans la création des modèles qui prédisent avec précision la probabilité qu'un client achète un prêt personnel et explorent les moyens contribuant à éliminer les clients passifs.
Pour achever cette mission, nous allons en premier lieu analyser les données qui agissent sur les prêts personnels et dans un second lieu essayer de construire un modèle adéquat et optimiser ses performances.
Le fichier à traiter dans ce jeu de prédiction "bankloan.csv" contient des données sur 5000 individus. Ces paramètres comprennent les informations démographiques du client (âge, revenus, etc.), la relation de ce dernier avec la banque (hypothèque, compte-titres, etc.) et sa réponse à la dernière campagne de prêt personnel . Dans cet échantillon étudié, seul 480 (= 9,6%) ont accepté le prêt personnel qui leur a été proposé lors de la campagne précédente.

#Importation des données

```{r}
loanbank_data <- read.csv("bankloan.csv")
l2<-loanbank_data
```
#Chargement des libraby nécessaires

```{r}
library(tidymodels)
library(tidyverse)
library(car)
library(corrplot)
library(caret)
library(ggthemes)
library(magrittr)
library(gridExtra)
library(patchwork)
library(inspectdf)
library(ggridges)
library(Rcpp)
library(inspectdf)
library(ggplot2)
library(dplyr)
theme_set(theme_bw())
```

# Explorons la forme des données.

```{r}
dim(loanbank_data)
```

# Vérifions si les données contiennent des valeurs manquantes ou NaN.

```{r}
is_null(loanbank_data)
is.na.data.frame(loanbank_data)
```

```{r}
sample_n(loanbank_data,5000)
```

#Vue générale des données
```{r}
glimpse(loanbank_data)
```

#Conversion des variables catégorielles
#On va convertir les variables Education, Family, CreditCard, Online, CD.Account et Securities.Account au type factor qui correspond aux variables catégorielles :

```{r}
loanbank_data $Family%<>% as.factor()
loanbank_data $ZIP.Code %<>% as.factor()
loanbank_data $Education %<>% as.factor()
loanbank_data $CreditCard %<>% as.factor()
loanbank_data $Online %<>% as.factor()
loanbank_data $CD.Account %<>% as.factor()
loanbank_data $Securities.Account %<>% as.factor()
```


#Description des variables 

```{r}
summary(loanbank_data)
```

##Description des variables catégorielles

```{r}
categ_cols <- loanbank_data %>% select_if(~ class(.) == "factor")
for (col in names(categ_cols)) {
  # cat(col , ":")
  # print(round(prop.table(table(insurance_data[[col]])),digits=2))
  # cat("\n")
  t <- loanbank_data %>%
    group_by_(col) %>%
    summarise(count = n()) %>%
    mutate(frequency = paste0(round(100 * count / sum(count), 0), "%")) %>% 
    # gt::gt() %>% 
    knitr::kable("html", align = "lcc") %>%
    kableExtra::kable_styling(full_width = F, position = "center") %>% 
    print()
}
```
#Observations :
- Une majorité de l'échantillon étudié sont des Undergraduate avec un pourcentage de 42%.
- Un manque drastique d'individus obtenant un compte de titres avec la banque et un compte de certificat de dépôt.
- 60% des clients utilisent les services bancaires par Internet.
- Le pourcentage des clients utilisant une carte de crédit émise par cette banque ne couvre que 29%.
- Le jeu de données est équilibrée par rapport au : Family

#Proportions des modalités dans les variables catégorielles

```{r}
show_plot(inspect_cat(loanbank_data))
```
#visualisation:
#Matrice de corrélation
```{r}

library(reshape2)
#calculer la matrice de correlation
newXa <- sapply(loanbank_data, as.numeric)

cormat <- round(cor(newXa),2)
head(cormat)
#Obtenir les triangles inférieur et supérieur de la matrice de corrélation
# Obtenir le triangle supérieur
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
#Réorganiser la matrice de corrélation
reorder_cormat <- function(cormat){
# Utiliser la corrélation entre les variables
  # comme mésure de distance
dd <- as.dist((1-cormat)/2)
hc <- hclust(dd)
cormat <-cormat[hc$order, hc$order]
}
# Reordonner la matrice de corrélation
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Fondre la matrice de corrélation
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Créer un ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab",
   name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed()
#Ajouter les coefficients de corrélation sur le heatmap
ggheatmap + 
geom_text(data = melted_cormat,
            mapping = aes(Var2,Var1,label = round(value, digit = 2)),color = "black", size = 4) +
theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  legend.justification = c(1, 0),
  legend.position = c(0.6, 0.7),
  legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                title.position = "top", title.hjust = 0.5))
print(ggheatmap)
  
```

#Observation:
Les variables les plus corrélés entre eux  sont :
“Age”et “Experience” 
“CCAVG”et "Income"
"Personal.Loan" et "Income"
"Income' influence ‘ann_CCAvg', 'Personal Loan', 'CD Account' et 'Mortgage'.
#Visualisation des interactions entre les variables

```{r}
CPCOLS <- c("#5B8888", "#bfd1d9", "#B57779", "#F4EDCA")

 loanbank_data%>%
  dplyr::select(Age,Experience,Income,CCAvg,Personal.Loan,Mortgage
                 ) %>%
  GGally::ggpairs(
    lower = list(
      continuous = GGally::wrap("points", col ="#4c86ad",alpha=0.6),
      combo = GGally::wrap("box",
        fill = "white", col ="black"
          
      )
    ),
    upper = list(
      continuous = GGally::wrap("cor", col = "#4c86ad"),
      combo = GGally::wrap("facetdensity",
        col =
          "black"
      )
    ),
    diag = list(
      continuous = GGally::wrap(
        "barDiag",
        fill = "#f5dfb3", col ="black",
        bins = 18
      ),
      discrete = GGally::wrap("barDiag", fill = "#f5dfb3", col ="black")
    ),
  )
 

```
#Observations :

La répartition par âge des clients est relativement la même, sauf les 23 , 24et 67ans qui ont une population faible

La distribution de income est totalement faible .
La distribution de CCAVG est négativement asymétrique.

#l'influence de l'age et le revenu sur les prêts personnels
```{r}
 loanbank_data%>%
  ggplot(aes(x = Age, y =Income, col = Personal.Loan)) +
  geom_point(alpha = 0.6, size = 2.5)


```
Les clients avec un revenu  supérieur à 100 000 sont plus susceptibles d'obtenir un prêt
#l'influence de l'age et annual CV average sur les prêts personnels
on commence par convertir the CCAvg de monthly average à annual average 

```{r}
loanbank_data['ann_CV'] = loanbank_data['CCAvg'] * 12
 loanbank_data%>%
  ggplot(aes(x = Age,ann_CV, col = Personal.Loan)) +
  geom_point(alpha = 0.6, size = 2.5)

```
Les Clients avec annual CV average plus que  30 sont plus susceptibles d'obtenir un prêt
#l'influence de  la Famille et l' Experience sur les prêts personnels:
```{r}
 #Family
counts <- table(loanbank_data$Personal.Loan,loanbank_data$Family)
barplot(counts,
  xlab="Family", col=c("darkblue","red"),
  legend = rownames(counts), beside=TRUE)
p5 <- ggplot(data =loanbank_data, aes(group=Personal.Loan,Family )) +
  geom_boxplot(fill = c("#cfe6b8", "#8bb054")) +
  labs(x = element_blank()) +
  ggtitle("Boxplot of personal loan by family")
p5

#Experience
counts1 <- table(loanbank_data$Personal.Loan,loanbank_data$Experience)
barplot(counts1,
  xlab="Experience", col=c("darkblue","red"),
  legend = rownames(counts1), beside=TRUE)
```

On observe dans les deux graphs  Family et Experience ont un effet tres faible  sur la distribution des  prêts personnels .
#l'influence des autres variables  sur les prêts personnels:
```{r}
#CreditCard
counts2 <- table(loanbank_data$Personal.Loan,loanbank_data$CreditCard)
barplot(counts2,
  xlab="CreditCard", col=c("darkblue","red"),
  legend = rownames(counts2), beside=TRUE)

#Securities Account
counts3 <- table(loanbank_data$Personal.Loan,loanbank_data$Securities.Account)
barplot(counts3,
  xlab="Securities Account", col=c("darkblue","red"),
  legend = rownames(counts3), beside=TRUE)
#CD Account
counts4 <- table(loanbank_data$Personal.Loan,loanbank_data$CD.Account)
barplot(counts4,
  xlab="CD Account", col=c("darkblue","red"),
  legend = rownames(counts4), beside=TRUE)

#CD Account et Securities Account
loanbank_data%>%
  ggplot(aes(x = CD.Account,Securities.Account,fill = Personal.Loan)) +
  geom_bar(stat = "identity")
#CD Account et credit card
loanbank_data%>%
  ggplot(aes(x = CD.Account,CreditCard,fill = Personal.Loan)) +
  geom_bar(stat = "identity")

 
```
Donc tous les colonnes du data depend l'un de l'autre

#AFC - Analyse Factorielle des Correspondances

Tout d'abord, nous aimerions supprimer 2 colonnes  : « ID » et « ZIP.Code »

```{r}
loanbank_data1<-select (l2,-c(ID,ZIP.Code))

```

```{r}
library("FactoMineR")
library("factoextra")
#Graphique du tableaux de contingence et test de chi2
library("gplots")
# 1. convertir les données en tant que table
dt2<- as.data.frame(as.matrix(loanbank_data1[1:10,]))
library(tidyverse)
dt2 %>%
  rownames_to_column(" Age ") %>%
  gather(Variable, Value, -Age ) %>%
  ggplot(aes(x = Variable, y =  Age , size = Value)) +
  geom_point(colour = "sky blue") +
  
  labs(title = "Balloon Plot forloanbank variables by AGE",
     subtitle = "Area is proportional to Freq.",
     x = "",
     y = "") +
  theme_bw()
#calculer l’AFC
res.ca <- CA (loanbank_data1[1:20,],graph= FALSE)

print(res.ca)
fviz_ca_biplot (res.ca, repel = TRUE)
fviz_ca_col(res.ca,col.col = "cos2",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) 


```
Dans le graphique ci-dessus, les lignes sont représentées par des points bleus et les colonnes par des triangles rouges.

La distance entre les points lignes ou entre les points colonnes donne une mesure de leur similitude (ou dissemblance). Les points lignes avec un profil similaire sont proches sur le graphique. Il en va de même pour les points colonnes.
Donc tous les variabless quantitatives sont en corrélation  entre eux sauf Mortgage

#Modelisation

#Après avoir enquêté sur les plots précédentes, nous travaillerons sur toutes les données après avoir supprimé les colonnes 'ID' et 'ZIP Code' parce que nous trouvons que toutes les colonnes sont affectées les unes par les autres

```{r}
loanbank_data<- select(loanbank_data,-c(ID, ZIP.Code))
```
#Modéle linéaire
#Initialisation du modèle
#Commençons par un modèle linéaire simple en utilisant les deux variables Income et Education

```{r}
simp.mod = lm(Personal.Loan ~Income + Education,
               data = loanbank_data,
               x = TRUE,
               y = TRUE)
summary(simp.mod)
```


```{r}
simp.mod %>%
  augment() %>%
  ggplot(aes(x = Income, y = Personal.Loan)) +
  geom_point(aes(col = Education)) +
  scale_color_manual(values = c("darkblue", "darkgreen","darkred")) +
  geom_line(aes(y = .fitted)) +
  geom_segment(aes(xend = Income, yend = .fitted), col = "grey") +
  facet_grid( ~ Education)
```


```{r}
simp.mod %>%
augment() %>%
  ggplot(aes(x = .fitted, y = Personal.Loan)) +
  geom_point(shape = 1, aes(col = Education)) +
  scale_color_manual(values = c("darkblue", "darkgreen","darkred")) +
  geom_abline(slope = 1, lty = "dashed")
```


```{r}
simp.mod%>%
  glance() %>%
  dplyr::select(r.squared,
         adj.r.squared,
         AIC,
         BIC,
         statistic,
         p.value,
         df.residual,
         nobs) %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```

#Observations:
-Ce modèle linéaire contribue à l'obtention d'un R-carré de 0.3222, impliquant par cela que les deux variables Income et Education ne peuvent expliquer que 32,22% du comportement des prêts personnels.
-Avec le niveau de signification obtenu (un p.value < 0,05 ), on peut constater que les daux variables choisies (Income et Education) sont des prédicteurs statiquement significatifs pour l'accord d'un Personal Loan.
-On remarque au même degré que c'est plus probable pour les Graduates et les Advanced ( individus de niveau d'Education respectivement de 2 et 3) d'avoir un prêt personnel qu'un Undergrad (individu de niveau 1 d'Education), menant à la supposition que le niveau d'étude influence d'une manière révélatrice la possibilité d'acquérir le prêt pour le client.

#verification des hypothèses de la régression pour le modèle initial
#Vérification de la linéarité des données

```{r}
plot(simp.mod, which=1)
```

```{r}
plot(simp.mod, which=3)
```
#Observations
On aperçoit un éloignement à même dire un carctère d'intru pour un groupe d'individu sur la forme affichée ci-dessus de la variabilité des résidus en fonction des valeurs prédites, indiquant par conséquent une hétéroscédasticité (une différence entre les variances des résidus des variables examinées).
La partie suivante sera consacrée à l'amélioration du modèle, et ceci en effectuant des modifications sur la variable Income.

#Amélioration du modèle

```{r}
mod.Income2 = lm(Personal.Loan ~ I(Income ^ 2) + Education, data = loanbank_data)
summary(mod.Income2)
```

```{r}
mod.Income2 %>%
  glance() %>%
  dplyr::select(r.squared,
         adj.r.squared,
         AIC,
         BIC,
         statistic,
         p.value,
         df.residual,
         nobs) %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```

```{r}
plot(mod.Income2, which = 1)
```

```{r}
plot(mod.Income2, which = 2)
```
#Observations:
On peut assumer que les deux premières hypothèses sont bien vérifiés pour ce deuxième modèle.
On va essayer d’améliorer encore la précision en ajoutant d’autres variables explicative :

```{r}
mod.extend = lm(Personal.Loan ~ I(Income ^ 2) + Education + Family + CreditCard + Online +
                  CD.Account + Securities.Account,
                data = loanbank_data)
summary(mod.extend)
```

```{r}
indice=mod.extend %>%
  glance() %>%
  dplyr::select(r.squared,
         adj.r.squared,
         AIC,
         BIC,
         statistic,
         p.value,
         df.residual,
         nobs)
indice%>% knitr::kable(align = "c") %>% 
        kableExtra::kable_styling(full_width = F, position = "left")
```
#Observations:
En ajoutant d’autres prédicteurs le r-carrée ajustée augmente à 41.80%.

On va tester l’importance des variables Experience et CCAvg dans la performance du modèle
H0 : l’ajout de la variable n’a aucune importance


```{r}
anova(lm(Personal.Loan ~ I(Income ^ 2) + Education + Age + Family + Experience +CreditCard ,
         data = loanbank_data) ,
      mod.extend)
```

```{r}
anova(lm(Personal.Loan ~ I(Income ^ 2) + Education + Age + Family + CCAvg +CreditCard ,
         data = loanbank_data) ,
      mod.extend)
```
#Observations:
Avec un niveau de risque 5%, On rejete l’hypothèse H0 pour les deux cas.
Donc, on garde les deux variables dans le modèle.

#Vérification des hypothèses pour le nouveau modèle amélioré
#Linéarité et normalité des résidus:

```{r}
par(mfrow = c(1, 3))
for (i in c(1, 2, 3)) {
  plot(mod.extend, which = i)
}
```
#Obseravtions:
La distribution des résidus n’est pas normale.
L’hypothèse de linéarité est quasiment vérifiée et la variance s'approche à la constance, on effectuera un extra test pour une précision de valeurs.

#Hétéroscedasticité:

```{r}
library(zoo)
library(lmtest)
bptest(mod.extend) # tester l'existence de l'hétéroscédasticité lineaire
```

```{r}
ncvTest(mod.extend)# tester l'existence de l'hétéroscédasticité non lineaire
```
#Observations:
Il n’y a aucun signe d’hétéroscédasticité.

```{r}
par(mfrow = c(2, 2))
for (i in c(4:6)) {
  plot(mod.extend, which = i)
}
influencePlot(mod.extend, main = "Influence Plot")
```
#Observations:
On a absence de points très influents dans le modèle, malgré l’existence des points ayant un résidu important mais ne sont pas extrêmes par rapport aux prédicteurs, leurs faible hat-values ne leurs permet pas d’influencer la droite de régression, ainsi que des points extrêmes ayant un faible résidu.
Les points ayant les plus grandes valeurs de cook’s distance (mesure d’influence basée sur le leverage et le résidu) sont plus caractérisées par un grand résidu (ordre de grandeur > 3) => l’erreur contribue plus dans l’influence de ces points.

```{r}
perf=mod.extend %>%
  glance() %>%
  dplyr::select(r.squared,
         adj.r.squared,
         AIC,
         BIC,
         statistic,
         p.value,
         df.residual,
         nobs)
perf%>% knitr::kable(align = "c") %>% 
        kableExtra::kable_styling(full_width = F, position = "left")
```
#Contribution des variables dans le modèle :

```{r}
library(MASS)
library(relaimpo)
mod.extend.shapley <-
  calc.relimp(lm(Personal.Loan ~ I(Income ^ 2) + Education + Age + Family + CCAvg + CreditCard , 
                  data = loanbank_data),
              type = "lmg") 
```

```{r}
library(caret)
mod.extend.shapley$lmg %>%
  sort(T) %>% 
  data.frame()  -> temp
names(temp) <- c("Shaply_value")
p = temp %>% 
  cbind(name = rownames(.)) %>% 
  ggplot()+
  geom_col(aes(reorder(name, -Shaply_value), Shaply_value), stat = "identity",
           fill = c("#4a9687","#c2aebb","#f5dfb3","#e09e8f","#cfe6b8","#f2d696"), col="black")+
  ggtitle("Relative Importance of Predictors")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Predictor Labels", y="Shapley Value Regression")
grid.arrange(p ,tableGrob(round(temp,4)), ncol = 2, widths = c(4, 2))
```
#Modèle additif généralisé

```{r}
library(mgcv)
# Build the model
mod.gam <- gam(Personal.Loan ~ s(Income) + Education + s(Experience) + Family + Age +
                 CreditCard,
               data = loanbank_data)
summary(mod.gam)
```
```{r}
mod.gam %>%
  glance() %>%
  mutate(adj.r.squared = 0.4168) %>%
  dplyr::select(adj.r.squared, AIC, BIC, nobs) %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```
#Obsrvations:
On n’observe aucune amélioration dans le modele additif généralisé par rapport au modéle précèdent.Donc on préfère le modèle linéaire pour des raisons de simplicité et interpretabilité.

#K-nearest neighbour
#Modèle initial
Afin de pouvoir évaluer le modèle knn, on va diviser notre jeu de données en deux parties : 80% pour le train set, et 20% pour le test set.

```{r}
set.seed(123456)
loanbank_data_split <-
  initial_split(loanbank_data, prop = .8, strata = "Personal.Loan")
loanbank_train <- training(loanbank_data_split)
loanbank_test <- testing(loanbank_data_split)
```

#A l’aide du package parsnip, On va instancier un modéle knn pour la regression et l’entrainer sur la train set.

```{r}
library(parsnip)
library(kknn)
model_knn <- nearest_neighbor(mode = "regression") %>%
 set_engine("kknn")  %>%
  fit(Personal.Loan ~ ., data = loanbank_train)
```

#Puis on va utiliser ce modèle pour effectuer les prédictons sur le test set afin d’évaluer sa performance.

```{r}
model_knn_pred <- model_knn %>%
  predict(new_data = loanbank_test) %>%
  bind_cols(loanbank_test %>% dplyr::select(Personal.Loan))

modelPerform = data.frame(
  RMSE = RMSE(model_knn_pred$.pred, model_knn_pred$Personal.Loan),
  R2 = R2(model_knn_pred$.pred, model_knn_pred$Personal.Loan)
)
modelPerform %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```

#Amélioration du modèle
#Pour améliorer encore plus la performance, on va essayer de trouver le k optimal.
Pour automatiser les prétraitements, on va commencer par créer une recipe pour standariser les variables numeriques Income et Age, et la combiner avec le modèle knn dans un workflow.

```{r}
#creating recipe
loan_recipe <-
  recipe(Personal.Loan ~ Age + Income + Experience + Education + CCAvg + Family +
           CreditCard, data = loanbank_train) %>%
  step_scale(Age, Income) %>%
  step_center(Age, Income)
#model specification
loan_spec <-
  nearest_neighbor(weight_func = "rectangular", neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")
#workflow(model+recipe)
loan_wf <- workflow() %>%
  add_recipe(loan_recipe) %>%
  add_model(loan_spec)
loan_wf
```

#Pour trouver le k optimale pour le modèle, on va utiliser la méthode de validation du cross-fold avec 5 folds, qui sert à diviser les données sur cinq partie, et à chaque itération réserver une partie pour la validation et utiliser les autres pour l’entrainement, puis calculer la performance en utilisant la moyenne des performances sur les différents folds.

Ici on va chercher le k optimale dans l’intervalle [1,100], et mesurer la performance avec cross-fold validation pour réduire l’erreur due à l’interaction possible entre les variables lors d’une simple division.

```{r}
set.seed(1000)
loan_vfold <- vfold_cv(loanbank_train, v = 5, strata = Personal.Loan)

gridvals <- tibble(neighbors = seq(1, 100))

loan_results <- loan_wf %>%
  tune_grid(resamples = loan_vfold, grid = gridvals) %>%
  collect_metrics()

loan_results  %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = T, position = "left")  %>% 
  kableExtra::kable_paper() %>%
  kableExtra::scroll_box( height = "300px")
```

#En comparant le RMSE pour les différents k,on peut constater que si on prend trop ou très peu de voisins, nous obtenons une faible précision.

```{r}
loan_results %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(neighbors, mean)) +
  geom_line(col="red") +
  geom_point(col = "#336980")+
  labs(title="Residuals vs number of neighbours")
```
#Observations:
On constate de la figure ci-dessus que la valeur optimale des neighbours à considérer est de 1, ceci peut être expliqué par le fait qu'on vise à prédire une variable à caractère catégoriel, avec juste deux états possibles: 0 et 1, or pour générer différents modèles linéires et effectuer une modélisation versatile pour le dataset, on traite Personal.Loan comme une variable numérique, et par conséquent  on traitera une observation d'un client pour prédire le bénéfice d'un prêt personnel ou non, et ceci toujours pour les variables Age + Income + Experience + Education + CCAvg + Family +CreditCard.

```{r}
loan_min <- loan_results %>%
  filter(.metric == "rmse") %>%
  filter(mean == min(mean))
loan_min %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```

```{r}
kmin <- loan_min %>% pull(neighbors)
```

#maintenant on va re-entrainer notre modèle avec la valeur optimale de k=1, et évaluer sa performance.

```{r}
set.seed(123456)
loan_spec <-
  nearest_neighbor(weight_func = "rectangular", neighbors = kmin) %>%
  set_engine("kknn") %>%
  set_mode("regression")

loan_fit <- workflow() %>%
  add_recipe(loan_recipe) %>%
  add_model(loan_spec) %>%
  fit(data = loanbank_train)
loan_fit
```

```{r}

loan_res <- loan_fit %>%
  predict(loanbank_test) %>%
  bind_cols(loanbank_test)

loan_res %>%  ggplot(aes(y = Personal.Loan, x = .pred, col = Education)) +
  geom_point() +
  scale_color_manual(values = c("darkblue", "darkgreen","darkred")) +
  geom_abline(slope = 1)

```

```{r}
loan_summary <- loan_res %>%
  metrics(truth = Personal.Loan, estimate = .pred)

loan_summary %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```

#Random forest
#Modèl initial
#On va commencer par entrainer un modèle de “random forest” sur le training-set en utilisant l’ensemble des variables et en gardant les paramètres par défaut qu’on va évaluer sur le test-set.

```{r}
library(randomForest)
model_rand_forest <- rand_forest(mode = "regression") %>%
  set_engine("randomForest") %>%
  fit(Personal.Loan ~ Age + Income + Experience + Education + CCAvg + Family +
           CreditCard, data = loanbank_train)
```

```{r}
model_rand_forest_pred <- model_rand_forest %>%
  predict(new_data = loanbank_test) %>%
  bind_cols(loanbank_test %>% dplyr::select(Personal.Loan))
model_rand_forest_pred %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")  %>% 
  
  kableExtra::scroll_box(width = "230px", height = "300px")
```

```{r}
modelPerform_rand_forest = data.frame(
  RMSE = RMSE(
    model_rand_forest_pred$.pred,
    model_rand_forest_pred$Personal.Loan
  ),
  R2 = R2(
    model_rand_forest_pred$.pred,
    model_rand_forest_pred$Personal.Loan
  )
)
modelPerform_rand_forest %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```

#Observations :
Ce modèle a réussi à expliquer 89,80% de la variance totale avec un RMSE de 0.0988951.

#Amélioration du modèle
On va essayer maintenant d’améliorer ces performances en réglant les hyperparamètres min_n(nombre minimale d’observations requis dans un noeud pour le diviser) et mtry(Le nombre de prédicteurs qui seront échantillonnés au hasard à chaque division lors de la création des modèles d’arbre).

Pour automatiser les prétraitements, on va utiliser une recipe pour centrer et réduire les variables numeriques Income et Age, et la combiner avec la spécification du modèle dans un workflow.



```{r}
#creating recipe
loan_recipe2 <-
  recipe(Personal.Loan ~ Age + Income + Experience + Education + CCAvg + Family +
           CreditCard, data = loanbank_train) %>%
  step_scale(Age, Income) %>%
  step_center(Age, Income)
#model specification
loan_spec_rf <- rand_forest(min_n = tune(),mtry=tune()) %>%
  set_engine("randomForest") %>%
  set_mode("regression")
#workflow(model+recipe)
loan_wf_rf <- workflow() %>%
  add_recipe(loan_recipe2) %>%
  add_model(loan_spec_rf)
loan_wf_rf
```

#Tuning des hyperparamètres :
On va utiliser la méthode “Grid search” Pour pouvoir trouver la combinaison optimale des hyperparamètres “mtry” et “min_n”.

```{r}
set.seed(123456)

loan_results_rf_grid <- loan_wf_rf %>%
  tune_grid(resamples = loan_vfold, grid = 10) 

loan_results_rf <- loan_results_rf_grid%>%
  collect_metrics()

loan_results_rf %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = T, position = "left")  %>% 
  kableExtra::kable_paper() %>%
  kableExtra::scroll_box( height = "300px")
```

#Visualisation de l’erreur pour les combinaisons de “mtry” et “min_n” :

```{r}
loan_results_rf %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(x=mtry, y=mean,col=min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(title="RMSE for each combinaison of mtry & min_n",y = "rmse")+
  theme(plot.title = element_text(hjust = 0.5))
```

#finalement, on va choisir les meilleurs hyper-paramètres pour notre modèle, puis mettre à jour la spécification du modèle d’origine pour créer la spécification de modèle finale.

```{r}
best_param <- loan_results_rf %>%
  filter(.metric == "rmse") %>%
  filter(mean == min(mean))
best_param %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```

```{r}
best_mtry <- best_param %>% pull(mtry)
best_min_n <- best_param %>% pull(min_n)
```

```{r}
best_rmse <- select_best(loan_results_rf_grid,"rmse")
final_rf <- finalize_model(
  loan_spec_rf,
  best_rmse
)
final_rf
```

#Les hyper-paramètres optimaux sont mtry = 4 et min_n = 3.

```{r}
last_fit(final_rf, Personal.Loan ~ Age + Income + Experience + Education + CCAvg + Family +
           CreditCard, loanbank_data_split) %>% 
  collect_metrics() %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```

#L’optimisation des hyperparamètres a amélioré les preformance,

```{r}
model_final_rf <- rand_forest(mode = "regression",mtry=best_mtry,min_n = best_min_n) %>%
  set_engine("randomForest") %>%
  fit(Personal.Loan ~ Age + Income + Experience + Education + CCAvg + Family +
           CreditCard, data = loanbank_train)
```


```{r}
model_final_rf_pred <- model_final_rf %>%
  predict(new_data = loanbank_test) %>%
  bind_cols(loanbank_test %>% dplyr::select(Personal.Loan))
model_final_rf_pred %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")  %>% 
  # kableExtra::kable_paper() %>%
  kableExtra::scroll_box(width = "230px", height = "300px")
```

```{r}
modelPerform_model_final = data.frame(
  RMSE = RMSE(
    model_final_rf_pred$.pred,
    model_final_rf_pred$Personal.Loan
  ),
  R2 = R2(
    model_final_rf_pred$.pred,
    model_final_rf_pred$Personal.Loan
  )
)
modelPerform_model_final %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```

```{r}
model_final_rf_pred %>%  ggplot(aes(y = Personal.Loan, x = .pred)) +
  geom_point(col="#336980",size=2,alpha=0.5) +
  geom_abline(slope = 1,col="#821f2e")
```

#Comparaison des modèles

```{r}
result_linear_mod = predict(mod.extend, loanbank_data)
lm_rmse = RMSE(result_linear_mod, loanbank_data$Personal.Loan)
lm_rsq = indice %>% pull(r.squared)
knn_rmse = loan_summary  %>% filter(.metric == "rmse") %>% pull(.estimate)
knn_rsq = loan_summary  %>% filter(.metric == "rsq") %>% pull(.estimate)
rf_rsq = modelPerform_model_final %>% pull(R2)
rf_rmse = modelPerform_model_final %>% pull(RMSE)

model <- c("Linear model", "KNN", "Random forest")
R_squared <- c(lm_rsq, knn_rsq, rf_rsq)
RMSE <- c(lm_rmse, knn_rmse, rf_rmse)

data.frame(model, R_squared, RMSE) %>% 
  knitr::kable(align = "c") %>% 
  kableExtra::kable_styling(full_width = F, position = "left")
```


#Conclusion:
Le modeling du Personal.Loan qui a compris trois modèles différents (Linear model, K-nearest neighbour et Random Forest) nous amène à conclure après comparaison des résultats obtenus que les deux méthodes des k plus proches voisins et de la Forêt d'arbres décisionnels permettent d'atteindre les meilleurs niveaux de précision (arrivant presque à 90%), contrairement au modèle linéaire qui offre une fidélité modeste, avec une faible explication de la variabilité des données de réponse autour de la moyenne et une droite de régression n'étant capable de déterminer que 41,8% de la distribution des points.

La visualisation du comportement  des variables nous guide à conclure un manque d'indépendance entre les paramètres étudiés. Cette analyse de corrélation a également énoncé que le revenu de l'individu (Income) a un effet considérable sur tout le jeu de données mais un plus dstinguable sur les prêts personnels.
En conséquence, pour obtenir encore plus de précision dans ses prédictions, on propose à cette banque de collecter plus de données sur ses clients et principalement de se focaliser de plus sur les revenus des individus.
Finalement, on doit toujours penser à utiliser des modèles supplémentaires pour des meilleurs précisions, on cite le Robut regression qui donne un poid faibles aux points influenceurs.
 
 
