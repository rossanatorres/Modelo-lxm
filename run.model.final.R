rm(list=ls())
library(Rcpp)
library(dplyr)
set.seed(1245)

load("~/Dropbox/Modelo CDMX/Modelo en Github/lambda_estimates/growth_rate.rda")
days <- 91 #días máximos que hemos simulado
growth.rate <- growth.rate[2:days + 1] #Quitar día 1 y tomar el máximo de días a simular

load("~/Dropbox/Modelo CDMX/Modelo en Github/lambda_estimates/Rt_GTlognormal.rda")
days.infect <- 5.2 #Promedio de días en el que el sujeto es infeccioso, estamos suponiendo que
                   #son los mismos días que de incubación: Lauer et al. https://doi.org/10.7326/M20-0504

beta.t <- Rt/days.infect

sourceCpp(code='
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double daysNotQuarantined(double node_birth, double node_end,  int l, int m,
bool quarantine_flag){
    /* daysNotQuarantined
     * ---------------------------------------------------------------------------
     * Function to estimate the amount of days an individual spends without
     * being isolated and not being contagious. We consider days in quarantine
     * occur in an interval [node_birth, node_end] where the quarantine happens
     * with l days outside and m days inside the house in a cyclic fashion.
     * node_birth       (double) .- Start day of the node when it is infected
     * node_end         (double) .- End day of the node when it isolates forever
     * l                (int)    .- Number of days outside the house
     * m                (int)    .- Number of days inside the house
     * quarantine_flag  (bool)   .- TRUE if quarantine is in place
     */
    
    //Quarantine period
    int period = l+m;
    int int_birth  = ceil(node_birth);
    int int_end    = floor(node_end);
    
    int startday = int_birth % period;
    int endday   = int_end % period;
    int Days = 0;
    int P = 0;
    int Q = 0;
    
    
    // Without quarantine
    if (quarantine_flag == FALSE){
        
        Days =  int_end - int_birth +1;
        
        //With quarantine
        
        //Scenario 0:
        
    } else {
        
        if (int_end == int_birth){
            
            Days = 1;
            
        } else {
            
            // Scenario 1: startday <=l & endday <=l
            
            if (startday != 0 & endday != 0 & startday <= l & endday  <= l){
                
                Days = int_end - int_birth;
                
                P = int_birth +(period-startday);
                
                Q = int_end - (endday);
                
                // P and Q are auxiliary variables to check if the nodes int_birth and int_end belong to
                // the same period (week).
                
                if (P > Q){
                    
                    Days = Days + 1;
                    
                } else {
                    
                    if (P == Q){
                        
                        Days = Days - m + 1;  }
                    
                    else {
                        
                        Days =  Days - (((Q-P) / period) + 1)*m + 1;
                    }
                }
            }
            
            //Scenario 2: startday <=l & endday > l
            
            if ((startday <=l & startday != 0) & (endday > l | endday == 0)){
                
                //Part 1:
                
                if (endday == 0){
                    
                    Days = int_end - int_birth - m +1;
                    
                    P = int_birth + (period - startday);
                    
                    Q = int_end - period;
                    
                    if ( P>Q ){
                        
                        Days = Days+0;
                        
                    } else {
                        
                        if (P == Q){
                            
                            Days = Days- m;
                            
                        }
                        
                        else {
                            
                            Days =  Days - (((Q-P)/period) + 1) *m;}
                        
                    }
                }
                
                
                //Part 2
                
                if (endday != 0){
                    
                    Days = int_end - int_birth - (endday - l) +1;
                    
                    P = int_birth +(period-startday);
                    
                    Q = int_end - endday;
                    
                    if (P>Q){
                        
                        Days = Days + 0;
                        
                    } else {
                        
                        if (P == Q){
                            
                            Days = Days - m;
                            
                        } else {
                            
                            Days =  Days - (((Q - P) / period) + 1) *m; }
                        
                    }
                }
                
            }
            
            
            // Case 3: startday >l & endday <= l
            
            // This condition guarantees that int_birth and int_endday
            // belong to different periods (weeks)
            
            if ((startday > l |  startday == 0) & (endday <= l & endday != 0)){
                
                
                //Part 1:
                
                if(startday == 0){
                    
                    Days = int_end - int_birth;
                    
                    P = int_birth;
                    
                    Q = int_end - (endday);
                    
                    if (P == Q){
                        
                        Days = Days + 0;
                        
                    } else {
                        
                        Days =  Days - (((Q - P) / period))*m; }
                    
                }
                
                //Part 2:
                
                if (startday != 0){
                    
                    Days = int_end - int_birth;
                    
                    Days = Days - (period - startday);
                    
                    P = int_birth + (period - startday);
                    
                    Q = int_end - (endday);
                    
                    if (P == Q){
                        
                        Days = Days + 0;
                        
                    } else {
                        
                        Days =  Days - (((Q - P) / period))*m;}
                    
                }
            }
            
            
            // Case 4: startday >l & endday > l
            
            
            if ((startday > l |  startday == 0) & (endday > l | endday == 0)){
                
                
                //Part 1: startday == 0 & endday == 0
                
                if (startday == 0 & endday == 0){
                    
                    Days = ((int_end - int_birth) / period)*l;
                    
                }
                
                //Part 2: startday > l & endday == 0
                
                if (startday > l & endday == 0 ){
                    
                    P = int_birth + (period - startday);
                    Q = int_end - (endday);
                    
                    if (P>Q){
                        
                        Days = 0;
                        
                    } else {
                        
                        Days = ((int_end - (int_birth + period - startday)) / period)*l;}
                    
                }
                
                //Part 3: startday == 0 & endday > l
                
                // This condition guarantees that int_birth and int_endday
                // belong to different periods (weeks)
                
                if(startday == 0 & endday > l){
                    
                    Days = (((int_end + (period - endday)) - int_birth) / period)*l;
                    
                }
                
                //Parte 4: startday > l & endday > l
                
                if (startday > l & endday > l){
                    
                    P = int_birth + (period - startday);
                    
                    Q = int_end - (endday);
                    
                    if (P > Q){
                        
                        Days = 0;
                        
                    } else {
                        
                        Days = (((int_end + (period - endday)) - P) / period)*l;
                        
                    }
                }
            }
        }
        
        
    }
    
    return Days;
    
}


#include <Rcpp.h>
#include <iostream>
#include <random>
#include <string>
#include <iterator>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]

NumericVector mod(NumericVector a, NumericVector n){
    // /*mod
    //   * ---------------------------------------------------------------------------
    //   * Function to estimate module for vectors
    //   */
    
    return a - floor(a/n)*n;
}


#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int InfectionOnset(int Parent_time, int s,  int W, int M,
bool quarantine_flag){
    
    
    // / * InfectionOnset
    //   * ---------------------------------------------------------------------------
    //   * Function to estimate the day of infection onset for each secondary case, based on the
    //   * primary case infectious period (which may follow an lxm days quarantine scheme)
    //   * Parent_time       (int)    .- Primary case infection onset
    //    * s                (int)    .- Primary case symptoms onset. Takes into account if the
    //                                   individual self-quarantines when developing symptoms or
    //                                   stays contagious the whole time
    //    * W                (int)    .- Number of days outside the house (l)
    //    * M                (int)    .- Number of days inside the house (m)
    //    * quarantine_flag  (bool)   .- TRUE if quarantine is in place
    //    */
    
    NumericVector contagioNoCuarent;
    NumericVector period;
    NumericVector modulo;
    NumericVector daysContagious;
    NumericVector noCuarent;
    int Uik;
    
    if(s == 0){
        s = s + 1;
    }
    
    if (quarantine_flag){
        
        contagioNoCuarent = seq(Parent_time, Parent_time + s);
        
        period = rep(W + M, contagioNoCuarent.size());
        
        modulo = mod(contagioNoCuarent, period);
        
        daysContagious = contagioNoCuarent[modulo > 0  & modulo <= W];
        
        Uik = sample(daysContagious, 1)[0];
        
    } else {
        
        Uik = R::runif(Parent_time, Parent_time + s);} //Secondary case infection onset
    
    Uik = ceil(Uik);
    
    return Uik;
    
}

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List iterateChildren(NumericVector Ncases, int currentK, int currentT,
int currentSim, double parent_time, double lambda,
int maxk, int maxsim, int maxt, double proba_autoquarantine,
double meanlog, double sdlog, int l, int m, bool quarantine_flag){
    
    
    
    
    //Check that we have not arrived yet
    currentSim = currentSim + 1;
    List L(2);
    
    //Account for current simulation
    L(0) = currentSim;
    L(1) = Ncases;
    
    if (currentK < maxk & currentT < maxt & currentSim < maxsim){
        
        //Symptom onset simulation
        double S = R::rlnorm(meanlog, sdlog);
        
        //Check if individual self-quarantines when developing symptoms or
        //stays contagious the whole time
        if (R::runif(0,1) > proba_autoquarantine){
            S = S + 8; //Viral load after symptom onset heavily declines after 8 days
        }
        
        //R0 throughout days
        //double lambda = (R::pgamma(S, shape, scale)*R0)/S;
        
        int node_maxt = ceil(std::min((double) ceil(currentT + S), (double) maxt));
        
        //Add new count to current time
        for (int i = floor(currentT); i < maxt; i++){
            Ncases(i) = Ncases(i) + 1;
            
        }
        
        //Measure the amount of days contagious
        double measure = daysNotQuarantined(parent_time, parent_time + S, l, m, quarantine_flag);
        
        
        NumericVector beta;
        
        beta = Environment::global_env()["beta.t"];
        
        lambda = beta[currentT];
        
        //Otros intentos:
        
        //lambda = (0.6052*std::exp(-(currentT)/30) + 0.01); //ecuación beta(t) de Ávila-Ponce de León et al.
        //https://www.medrxiv.org/content/10.1101/2020.05.11.20098517v1.full.pdf
        //Crece demasiado!!
        
        //lambda = 0.1288103;  // growth rate (estimación con paquete R0), no crece
        
        //lambda = (1 -  Ncases(currentT)/9025363)*lambda; //Hawkes
        
        
        //R0 en 2 puntos en el tiempo (este sale decente también):
        
        //First intervention
        // if(currentT <= 32){
        
        //lambda = 2.48/11.5; //R0 = 2.48 viene de Mena et al. https://arxiv.org/pdf/2005.02294.pdf
                              // 11.5 viene de: Lauer et al. https://doi.org/10.7326/M20-0504 :
                              //"The median incubation period was estimated to be 5.1 days (95% CI, 4.5 to 5.8 days),
                              //and 97.5% of those who develop symptoms will do so within 11.5 days
                              //(CI, 8.2 to 15.6 days) of infection."
        // }
        
        
        // Second intervention
        
        // if(currentT > 32 ){
        
        //lambda = 2.03/11.5; //R0 = 2.03 viene de Mena et al. https://arxiv.org/pdf/2005.02294.pdf
                              //"Actually from the parameters’ posterior
                              //distribution, one can show that prior the activation of the contingency plan, the basic reproduction rate R0 was 2.48, which
                              //was lowered to the value 2.03 once the plan was activated on March 23"
        // }
        
        //Initial cases
        int initCases;
        int hijos;
        initCases = 1; //Para empezar con 1 o más individuos
        
        if (currentT == 0){
           
            hijos = initCases;
        
        } else {
            hijos = R::rpois(measure*lambda);
        
        }
        
        
        if (hijos > 0){
            for (int k = 0; k < hijos; k++){
                
                int Uik;
                
                Uik = InfectionOnset(parent_time, S, l, m, quarantine_flag );
                
                
                L  = iterateChildren(Ncases,currentK + 1, ceil(Uik), L(0),
                Uik, lambda, maxk, maxsim, maxt,
                proba_autoquarantine, meanlog,
                sdlog, l , m, quarantine_flag);
            }
        }
    } else if (currentSim == maxsim){
        Rcout << "Number of simulations exceeds maxsim" << std::endl;
        L(0) = currentSim + 1;
    }
    
    return L;
}




#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix Model_l_times_m_cpp(int nsim, double lambda, int maxk, int maxsim,
int maxt, double proba_autoquarantine, double meanlog,
double sdlog, int l, int m, bool quarantine_flag){
    
    NumericMatrix Simulations(maxt, nsim);
    List L;
    for (int i = 0; i < nsim; i++){
        NumericVector Ncases(maxt);
        int initCases;
        initCases = 3;
        Ncases = Ncases + initCases;
        L = iterateChildren(Ncases, 0, 0, 0, 0, lambda, maxk, maxsim, maxt,
        proba_autoquarantine, meanlog,
        sdlog, l, m, quarantine_flag);
        Ncases = L(1);
        Simulations(_, i) = Ncases;
    }
    return Simulations;
}')


#ESTE MODELO EN C++
model_k_times_m <- function(nsim = 100,
                            params = list(lambda = 0.554902, maxk = 10, maxt = 10,
                                          maxsim = 100000,
                                          proba_autoquarantine = 1,
                                          meanlog = 1.621, sdlog = 0.418,
                                          l = 3, m = 4),
                            quarantine_flag = TRUE){
  
  return(Model_l_times_m_cpp(nsim, params$lambda,
                             floor(params$maxk),  floor(params$maxsim), floor(params$maxt),
                             params$proba_autoquarantine, params$meanlog, params$sdlog,
                             floor(params$l), floor(params$m),
                             quarantine_flag)
  )
  
}




model_k_times_m_summary <- function(qest = c(0.25, 0.5, 0.75),
                                    nsim = 100,
                                    lambda = 0.554902, maxk = 40,
                                    maxsim = 100000,
                                    maxt = 30, proba_autoquarantine = 1,
                                    meanlog = 1.621, sdlog = 0.418,
                                    l = 3, m = 4, quarantine_flag = TRUE){
  
  #Get model summary
  model.iteration <- model_k_times_m(nsim = nsim, params = list(lambda = lambda,
                                                                maxk = maxk, maxsim = maxsim,
                                                                maxt = maxt,
                                                                proba_autoquarantine = proba_autoquarantine,
                                                                meanlog = meanlog,
                                                                sdlog = sdlog, l = l, m = m),
                                     quarantine_flag = quarantine_flag)
  
  return(t(apply(model.iteration, 1, quantile, qest)))
  
}


#-------------------------------------------------------------------------------
#                             Run simulations for lxm 
#-------------------------------------------------------------------------------
#Para +31 días usar: , maxk = 1000000000, maxt = 51, maxsim = 1000000000 

dir <- "~/Dropbox/Modelo CDMX/Modelo en Github/Results/"

days <- 31 #días a simular

raw43 <- model_k_times_m(nsim = 5000,
                         params = list(lambda = 1, maxk = 1000000000, maxt = days,
                                       maxsim = 1000000000,
                                       proba_autoquarantine = 0.5,
                                       meanlog = 1.621, sdlog = 0.418,
                                       l = 4, m = 3),
                         quarantine_flag = TRUE)

save(raw43, file= paste0(dir, "raw.4x3.rda"))


qest = c(0.25, 0.5, 0.75)

t(apply(raw43, 1, quantile, qest))


