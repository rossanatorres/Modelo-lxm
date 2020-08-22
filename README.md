# Modelo-lxm
* run.model.final.R es el archivo funcional, trae comentados algunos intentos de lambda y descomentado el que hasta ahora ha sido el mejor (Rt/tiempo infeccioso)
* Auxiliary_functions contiene la función que utilizamos para calcular Rt, la función beta(t) en R para visualizar los resultados y una estimación para el growth rate con datos 
* Results trae los resultados más recientes con la lambda que mejor ha resultado (Rt/tiempo infeccioso), sólo corrimos 31 días.
* Visualization trae el código para comparar con datos reales y el código para ver reducciones en casos y en proporciones (esto nunca nos ha gustado pero es lo que hay)
* lambda_estimates trae las estimaciones dependientes del tiempo que se calculan con los archivos de Auxiliary functions
* Data contiene los datos reales para comparar
# Referencias principales
[Periodo infeccioso](https://doi.org/10.7326/M20-0504) 

[Estimación de beta dependiente al tiempo en México (Ávila-Ponce de león)](https://www.medrxiv.org/content/10.1101/2020.05.11.20098517v1.full.pdf)

[Estimación de beta dependiente al tiempo en México (Mena)](https://arxiv.org/pdf/2005.02294.pdf)
