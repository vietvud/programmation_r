# 2.1.1
brutToNet1 <- function(salaire_brut)
{ 
  if(is.numeric(salaire_brut)) # condition pour que l'argument saisi soit du bon type
  { salaire_net <- salaire_brut * (1 - 0.22) # calcul effectué si la condition est respectée  
  return (salaire_net)} # résultat souhaité renvoyé
  
  else {print("ERREUR: type not expected")}  # erreur affichée si la condition est non respectée
}


# 2.1.2
brutToNet2 <- function(salaire_brut, type_sal) #création de la fonction qui s'appelle 'brutToNet2 qui prend en entrée deux arguments: le salaire brut et le type de salarié
{
  taux <- 0.075 # Mise en défaut la valeur du taux de prélèvement
  if (is.numeric(salaire_brut) & type_sal %in% c("C", "N")) # condition pour que les arguments saisis soient du bon type
  {
    if (type_sal == "C")
    {
      salaire_net <- salaire_brut * (1 - 0.25 - taux) 
      return (salaire_net)}
    
    if(type_sal == "N") 
    { salaire_net <- salaire_brut * (1 - 0.22 - taux) 
    return (salaire_net)}
  } # calcul effectué si la condition est respectée et renvoie le résultat souhaité
  
  else {print("ERREUR. Contract unknown")} # erreur affichée si la condition est non respectée
}

# 2.1.3
brutToNet3 <- function(salaire_brut, type_sal, tauxpr, tempstr) #création de la fonction qui s'appelle 'brutToNet2 qui prend en entrée quatre arguments: le salaire brut, le type de salarié (obligatoires), le taux de prélèvement, le temps de travail (facultatifs)
{
  if (missing(tauxpr)){tauxpr <- 7.5}
  if (missing(tempstr)){tempstr <- 100} # Mise en défaut la valeur des arguments facultatifs s'ils ne sont pas saisis
  
  if (is.numeric(salaire_brut) & type_sal %in% c("C", "N"))  # condition pour que les arguments saisis soient du bon type
  { if (type_sal == "C")
  {salaire_net <- salaire_brut * (1 - 0.25) }
    if(type_sal == "N") 
    { salaire_net <- salaire_brut * (1 - 0.22)}} # calcul effectué si la condition est respecté, enregistre le résultat souhaité, permet de poursuivre le programme
  else {print("ERREUR: contract unkown")} # erreur affichée si la condition est respectée et arrête le programme
  
  if (is.numeric(tempstr) & tempstr >= 0 & tempstr <= 100) # condition pour que les arguments saisis soient du bon type
  { salaire_netav <- salaire_net * tempstr / 100} # calcul effectué si la condition est respecté, enregistre le résultat souhaité, permet de poursuivre le programme
  else {print("ERREUR: time must be in range (0;100)")} # erreur affichée si la condition est respecté et arrête le programme
  
  if (is.numeric(tauxpr) & tauxpr >= 0 & tauxpr <= 100) # condition pour que les arguments saisis soient du bon type
  { salaire_netap <- salaire_netav - (salaire_netav * tauxpr / 100)} # calcul effectué si la condition est respecté, enregistre le résultat souhaité, permet de poursuivre le programme
  else {print("ERREUR: rate must be in range (0;100)")} # erreur affichée si la condition est respectée et arrête le programme
  
  maliste <- list(paste("Salaire net avant impôt:" ,salaire_netav, "€"),
                  paste ("Salaire net après impôt:",salaire_netap, "€")) # création d'une liste permettant de stocker les deux résultat obtenus pour renvoyer 
  return(maliste) # renvoi du résultat souhaité 
}

brutToNet1(1000)
brutToNet2(1000, "C")
brutToNet3(1000, "N")
