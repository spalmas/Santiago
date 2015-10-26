<<<<<<< HEAD
biomass <- function(spp, dbh, h, crown){
  #Get equation type of list of species
  biom_eq_type_list = species[as.character(spp),'biomass_eq_type']
  
  biomass_i = c()
  i = 1
  for (biom_eq_type in biom_eq_type_list){

    dbh_i = dbh[i]
    h_i = h[i]
    crown_i = crown[i]
    spp_i  = as.character(spp[i])
    
    a = species[spp_i,'a_eq_biom']
    b = species[spp_i,'b_eq_biom']
    c = species[spp_i,'c_eq_biom']
    d = species[spp_i,'d_eq_biom']
    e = species[spp_i,'e_eq_biom']
    f = species[spp_i,'f_eq_biom']
    g = species[spp_i,'g_eq_biom']
    
    
#     ######If statements to manage species exceptions
#     #The range of dbh vlaues for Schinus biomass eq is 8-45
#     if (spp_i == 'Schinus molle' & dbh_i > 45){ dbh_i = 45 }
# 
#     #The range of dbh vlaues for Salix babylonica probaly does not reach 80. Dbh  changed to 40
#     #if (spp_i == 'Salix babylonica' & dbh_i > 40){ dbh_i = 40 }
#     
#     #The range of dbh vlaues for citris limon probaly does not reach 156. Dbh changed to 15
#     if (spp_i == 'Citrus limon' & dbh_i > 15){ dbh_i = 15 }
#     
#     #The range of dbh vlaues for Prunus armeniaca probaly does not reach 134. Dbh changed to 25
#     if (spp_i == 'Prunus armeniaca' & dbh_i > 25){ dbh_i = 25 }
#     
#     #The range of dbh vlaues for Jacaranda mimosifolia probaly does not reach 166. Dbh changed to 20
#     if (spp_i == 'Jacaranda mimosifolia' & dbh_i > 20){ dbh_i = 20 }
    
    #######Biomass estimate
    if (biom_eq_type == 1){
      biomass_i[i] = exp(a + b * log(dbh_i))
      
    } else if (biom_eq_type == 2){
      biomass_i[i] = a + b*dbh_i + c*dbh_i^d + e*h_i^f + g * dbh_i^2 * h_i 

    } else if (biom_eq_type == 3){
      biomass_i[i] = a * pi * (crown_i /2)^2 * b * h_i^c
                         
    } else if (biom_eq_type == 4){
      biomass_i[i] = a*h_i^b *  (dbh_i/c)^d
                         
    } else if (biom_eq_type == 5){
      biomass_i[i] = a * dbh_i^b + c * dbh_i^d
                         
    } else if (biom_eq_type == 6){
      biomass_i[i] = exp(a + b * dbh_i)
                         
    } else if (biom_eq_type == 7){
      biomass_i[i] = a + b * log(dbh_i)
      
    } else if (biom_eq_type == 8){
      biomass_i[i] = a * h_i^b + c * h_i^d
      
    } else if (biom_eq_type == 0){
      biomass_i[i] = 0
    }
    
    #Delete all trees with biomass larger than 2500, biomass eq biomass eq put of limits? measurement too big?
    #if (!is.na(biomass_i[i]) & biomass_i[i] > 2500){biomass_i[i] = 0} 
    
    i = i + 1
  }
  
  return (biomass_i)
}
=======
biomass <- function(spp, dbh, h, crown){
  #Get equation type of list of species
  biom_eq_type_list = species[as.character(spp),'biomass_eq_type']
  
  biomass_i = c()
  i = 1
  for (biom_eq_type in biom_eq_type_list){

    dbh_i = dbh[i]
    h_i = h[i]
    crown_i = crown[i]
    spp_i  = as.character(spp[i])
    
    a = species[spp_i,'a_eq_biom']
    b = species[spp_i,'b_eq_biom']
    c = species[spp_i,'c_eq_biom']
    d = species[spp_i,'d_eq_biom']
    e = species[spp_i,'e_eq_biom']
    f = species[spp_i,'f_eq_biom']
    g = species[spp_i,'g_eq_biom']
    
    
#     ######If statements to manage species exceptions
#     #The range of dbh vlaues for Schinus biomass eq is 8-45
#     if (spp_i == 'Schinus molle' & dbh_i > 45){ dbh_i = 45 }
# 
#     #The range of dbh vlaues for Salix babylonica probaly does not reach 80. Dbh  changed to 40
#     #if (spp_i == 'Salix babylonica' & dbh_i > 40){ dbh_i = 40 }
#     
#     #The range of dbh vlaues for citris limon probaly does not reach 156. Dbh changed to 15
#     if (spp_i == 'Citrus limon' & dbh_i > 15){ dbh_i = 15 }
#     
#     #The range of dbh vlaues for Prunus armeniaca probaly does not reach 134. Dbh changed to 25
#     if (spp_i == 'Prunus armeniaca' & dbh_i > 25){ dbh_i = 25 }
#     
#     #The range of dbh vlaues for Jacaranda mimosifolia probaly does not reach 166. Dbh changed to 20
#     if (spp_i == 'Jacaranda mimosifolia' & dbh_i > 20){ dbh_i = 20 }
    
    #######Biomass estimate
    if (biom_eq_type == 1){
      biomass_i[i] = exp(a + b * log(dbh_i))
      
    } else if (biom_eq_type == 2){
      biomass_i[i] = a + b*dbh_i + c*dbh_i^d + e*h_i^f + g * dbh_i^2 * h_i 

    } else if (biom_eq_type == 3){
      biomass_i[i] = a * pi * (crown_i /2)^2 * b * h_i^c
                         
    } else if (biom_eq_type == 4){
      biomass_i[i] = a*h_i^b *  (dbh_i/c)^d
                         
    } else if (biom_eq_type == 5){
      biomass_i[i] = a * dbh_i^b + c * dbh_i^d
                         
    } else if (biom_eq_type == 6){
      biomass_i[i] = exp(a + b * dbh_i)
                         
    } else if (biom_eq_type == 7){
      biomass_i[i] = a + b * log(dbh_i)
      
    } else if (biom_eq_type == 8){
      biomass_i[i] = a * h_i^b + c * h_i^d
      
    } else if (biom_eq_type == 0){
      biomass_i[i] = 0
    }
    
    #Delete all trees with biomass larger than 2500, biomass eq biomass eq put of limits? measurement too big?
    #if (!is.na(biomass_i[i]) & biomass_i[i] > 2500){biomass_i[i] = 0} 
    
    i = i + 1
  }
  
  return (biomass_i)
}
>>>>>>> 5c4efa2c7ce696bd54de300cccccda4d481dbe6c
