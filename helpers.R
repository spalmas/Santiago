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
    
    if (biom_eq_type == 1){
      biomass_i[i] = exp(a + b * log(dbh_i))
      
    } else if (biom_eq_type == 2){
      biomass_i[i] = a + b*dbh_i + c*dbh_i^d + e*h_i^f + g * dbh_i^2 * h_i 

    } else if (biom_eq_type == 3){
      biomass_i[i] = a * pi * (crown_i /2)^2 * b * h_i^c
                         
    } else if (biom_eq_type == 4){
      biomass_i[i] = a*h_i^b *  (dbh_i/c)^d
                         
    } else if (biom_eq_type == 5){
      biomass_i[i] = a * dbh[i]^b + c * dbh[i]^d
                         
    } else if (biom_eq_type == 6){
      biomass_i[i] = exp(a + b * dbh[i])
                         
    } else if (biom_eq_type == 7){
      biomass_i[i] = a + b * log(dbh[i])
      
    } else if (biom_eq_type == 0){
      biomass_i[i] = 0
    }
    
    i = i + 1
  }
  
  return (biomass_i)
}