
#my function to process formula
{
  table_to_formula = function(temp_formula){
    r = c()
    elem = temp_formula$elem
    count = temp_formula$count
    len = length(elem)
    for(i in 1:len){
      if(count[i]!=0){
        r=paste(r, elem[i],  count[i], sep="")
      }
    }
    if(sum(count<0)!=0){
      r=paste(r, "Illegal_formula")
    }
    return (r)
  }
}


#Variables
{
  H_iso=(1.00782503224-1)
  N_iso=(14.0030740048-14)
  O_iso=(15.99491462-16)
  Cl_iso=(34.96885268-35)
  F_iso=(18.99840322-19)
  Br_iso=(78.9183371-79)
  I_iso=(126.904473-127)
  S_iso=(31.97207100-32)
  P_iso=(30.97376163-31)
  Si_iso=(27.97692653-28)
  B_iso=(11.009305167-11)
  Na_iso=(22.9897692809-23)
  K_iso=(38.96370668-39)
  Ca_iso=(39.962591-40)
  Ni_iso=(57.935343-58)
  Cu_iso=(62.929597-63)

  H_mass = 1.00782503224
  e_mass = 0.00054857990943


  # element ratio from 7 golden rule
  H_C_ratio_min = 0.1
  H_C_ratio_max = 6
  N_C_ratio_max = 4
  O_C_ratio_max = 3
  P_C_ratio_max = 2
  S_C_ratio_max = 3
  Si_C_ratio_max = 1
  F_C_ratio_max = 6
  Cl_C_ratio_max = 2
}


#' mz_formula
#'
#' @param Accurate_mass accurate mass from ms data
#' @param charge 1 for positive, 0 for neutral mass, -1 for negative
#' @param ppm output result tolerance window
#' @param C_range carbon number range
#' @param H_range hydrogen number range
#' @param O_range oxygen number range
#' @param N_range nitrogen number range
#' @param Cl_range chlorine number range
#' @param P_range phosphorus number range
#' @param S_range sulfur number range
#' @param Na_range sodium number range
#' @param K_range potassium number range
#' @param F_range fluorine number range
#' @param Br_range bromine number range
#' @param I_range iodine number range
#' @param Si_range silicon number range
#' @param N_rule nitrogen rule
#' @param db_min min of double bond and ring number
#' @param db_max max of double bond and ring number
#' @param B_range boron number range
#' @param metal_ion total number of metal ion
#' @param Ca_range Ca number range
#' @param Cu_range Cu number range
#' @param Ni_range Ni number range
#' @param Elem_ratio_rule element ratio from 7 golden rule, 0.1<= H/C <=6, N/C<=4, O/C<=3, P/C<=2, S/C<=3, Si/C<=1, F/C<=6, Cl/C<=2
#'
#' @export
#' @return a dataframe recording formula, mass difference and db_r number that fall within ppm of input mass
#'
#' @examples mz_formula(148.0604, 1, 5)
#'


mz_formula = function(Accurate_mass = 148.0604,
                      charge = 1,
                      ppm = 5,
                      C_range = 0:99,
                      H_range = 0:100,
                      O_range = 0:20,
                      N_range = 0:12,
                      Cl_range = 0:0,
                      P_range = 0:3,
                      S_range = 0:3,
                      Na_range = 0:0,
                      K_range = 0:0,
                      F_range = 0:0,
                      Br_range = 0:0,
                      I_range = 0:0,
                      Si_range = 0:0,
                      B_range = 0:0,
                      Ca_range = 0:0,
                      Cu_range = 0:0,
                      Ni_range = 0:0,
                      N_rule = T,
                      Elem_ratio_rule = F,
                      db_min = 0,
                      db_max = 99,
                      metal_ion = 0:3)
{


  if(charge==0){mz_neutral=Accurate_mass}
  else{mz_neutral = (Accurate_mass - (H_mass-e_mass)*sign(charge))*abs(charge)}
  tolerance = mz_neutral*ppm/10^6

  H_min0 = min(H_range)
  C_min = min(C_range)
  C_max = max(C_range)

  temp_formula = list(elem = c("C", "H", "N", "O", "Cl", "S", "P",
                               "Na", "K", "F", "Br", "I", "Si","B",
                               "Ca","Cu","Ni"), count = rep(1,17))
  temp_formula$elem = temp_formula$elem[order(temp_formula$elem)]
  temp_formula_list = list()

  iteration=0
  mz_integer = floor(mz_neutral+0.2)
  mz_decimal = mz_neutral - mz_integer

  for(B in B_range){
    for(Si in Si_range){
      for(I in I_range){
        for(Br in Br_range){
          for(fluorine in F_range){
            for(Cu in Cu_range)
            for(Ni in Ni_range)
            for(Ca in Ca_range)
            for(K in K_range){
            for(Na in Na_range){
              if((K + Na + Ca + Ni + Cu) > max(metal_ion)) next
                CHNOPSCl = mz_integer - 23*Na - 39*K - 19*fluorine - 79*Br - 127*I - 28*Si - 11*B - 40*Ca - 58*Ni - 63*Cu
                for(P in P_range){
                  for(S in S_range){
                    for(Cl in Cl_range){
                      CHNO = CHNOPSCl - 35*Cl - 32*S - 31*P
                      if(CHNO<0) break
                      for(N in N_range){
                        #(1) N rule
                        if(N_rule){
                          if(N%%2!=mz_integer%%2) next
                        }
                        for(O in O_range){
                          CH = CHNO - 14*N - 16*O

                          if(CH<0) break
                          else {
                            Heavy_atom_decimal = (Cl_iso*Cl) + (F_iso*fluorine) + (Br_iso*Br) + (I_iso*I) + (S_iso*S) +
                              (P_iso*P) + (Si_iso*Si) + (Na_iso*Na) + (K_iso*K) + (N_iso*N) + (O_iso*O) + (B_iso*B) +
                              (Ca_iso*Ca) + (Cu_iso*Cu) + (Ni_iso*Ni)
                            H_min = max(H_min0, floor((mz_decimal-Heavy_atom_decimal-tolerance)/H_iso))
                            C_lower = max(C_min, floor(CH/14)-1)
                            C_upper = min(C_max, floor(CH/12)+1)
                          }
                          for(C in C_lower:C_upper){

                            iteration = iteration+1

                            ##(2) H number
                            H = CH - 12*C
                            if(H < H_min) next

                            ##(3) Saturation, ring&double bond
                            db_r = (C+Si+1) - ((H+Cl+fluorine+Br+I+Na+K+Ca*2+Cu*2+Ni*2) - (B+N+P))/2;
                            if(db_r < db_min | db_r >db_max) next

                            ##(4) Decimal place
                            differ = (H_iso*H) + Heavy_atom_decimal - mz_decimal
                            if(differ > tolerance | differ < -tolerance) next

                            ##(5) Element ratio from 7 golded rule
                            if(Elem_ratio_rule & C!=0){
                              Meet_ratio_rule = all (H/C >= H_C_ratio_min,
                                                     H/C <= H_C_ratio_max,
                                                     N/C <= N_C_ratio_max,
                                                     O/C <= O_C_ratio_max,
                                                     P/C <= P_C_ratio_max,
                                                     S/C <= S_C_ratio_max,
                                                     Si/C <= Si_C_ratio_max,
                                                     fluorine/C <= F_C_ratio_max,
                                                     Cl/C <=Cl_C_ratio_max
                                                     )
                              if(!Meet_ratio_rule) next
                            }
                            ## record

                            temp_formula$count = c(B, Br, C, Ca, Cl, Cu, fluorine, H, I, K, N, Na, Ni, O, P, S, Si)
                            temp_formula_list[[length(temp_formula_list)+1]]=c(table_to_formula(temp_formula),differ,db_r)
                          }}}}}}}}}}}}}

  if(mz_neutral > 700){
    mz_integer = floor(mz_neutral-.8)
    mz_decimal = mz_neutral - mz_integer
    for(B in B_range){
      for(Si in Si_range){
        for(I in I_range){
          for(Br in Br_range){
            for(fluorine in F_range){
              for(Cu in Cu_range)
                for(Ni in Ni_range)
                  for(Ca in Ca_range)
                    for(K in K_range){
                      for(Na in Na_range){
                        if((K + Na + Ca + Ni + Cu) > max(metal_ion)) next
                        CHNOPSCl = mz_integer - 23*Na - 39*K - 19*fluorine - 79*Br - 127*I - 28*Si - 11*B - 40*Ca - 58*Ni - 63*Cu
                        for(P in P_range){
                          for(S in S_range){
                            for(Cl in Cl_range){
                              CHNO = CHNOPSCl - 35*Cl - 32*S - 31*P
                              if(CHNO<0) break
                              for(N in N_range){
                                #(1) N rule
                                if(N_rule){
                                  if(N%%2!=mz_integer%%2) next
                                }
                                for(O in O_range){
                                  CH = CHNO - 14*N - 16*O

                                  if(CH<0) break
                                  else {
                                    Heavy_atom_decimal = (Cl_iso*Cl) + (F_iso*fluorine) + (Br_iso*Br) + (I_iso*I) + (S_iso*S) +
                                      (P_iso*P) + (Si_iso*Si) + (Na_iso*Na) + (K_iso*K) + (N_iso*N) + (O_iso*O) + (B_iso*B) +
                                      (Ca_iso*Ca) + (Cu_iso*Cu) + (Ni_iso*Ni)
                                    H_min = max(H_min0, floor((mz_decimal-Heavy_atom_decimal-tolerance)/H_iso))
                                    C_lower = max(C_min, floor(CH/14)-1)
                                    C_upper = min(C_max, floor(CH/12)+1)
                                  }
                                  for(C in C_lower:C_upper){

                                    iteration = iteration+1

                                    ##(2) H number
                                    H = CH - 12*C
                                    if(H < H_min) next

                                    ##(3) Saturation, ring&double bond
                                    db_r = (C+Si+1) - ((H+Cl+fluorine+Br+I+Na+K+Ca*2+Cu*2+Ni*2) - (B+N+P))/2;
                                    if(db_r < db_min | db_r >db_max) next

                                    ##(4) Decimal place
                                    differ = (H_iso*H) + Heavy_atom_decimal - mz_decimal
                                    if(differ > tolerance | differ < -tolerance) next

                                    ##(5) Element ratio from 7 golded rule
                                    if(Elem_ratio_rule & C!=0){
                                      Meet_ratio_rule = all (H/C >= H_C_ratio_min,
                                                             H/C <= H_C_ratio_max,
                                                             N/C <= N_C_ratio_max,
                                                             O/C <= O_C_ratio_max,
                                                             P/C <= P_C_ratio_max,
                                                             S/C <= S_C_ratio_max,
                                                             Si/C <= Si_C_ratio_max,
                                                             fluorine/C <= F_C_ratio_max,
                                                             Cl/C <=Cl_C_ratio_max
                                      )
                                      if(!Meet_ratio_rule) next
                                    }

                                    ## record

                                    temp_formula$count = c(B, Br, C, Ca, Cl, Cu, fluorine, H, I, K, N, Na, Ni, O, P, S, Si)
                                    temp_formula_list[[length(temp_formula_list)+1]]=c(table_to_formula(temp_formula),differ,db_r)
                                  }}}}}}}}}}}}}
  }





  #print(paste("iteration =", iteration))
  if(length(temp_formula_list)==0){
    return(data.frame(formula = as.character(), differ = as.numeric(), db_r = as.numeric()))
  }

  formula_df=as.data.frame(matrix(unlist(temp_formula_list),ncol=3,byrow = T),stringsAsFactors=F)
  colnames(formula_df) = c("formula", "differ", "db_r")
  formula_df$differ=as.numeric(formula_df$differ)
  formula_df$db_r=as.numeric(formula_df$db_r)
  formula_df = formula_df[with(formula_df, order(abs(differ))),]

  return(formula_df)
}


