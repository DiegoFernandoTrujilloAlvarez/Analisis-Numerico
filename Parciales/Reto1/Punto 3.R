library(Rmpfr)
#f1 es la funci�n dada en el enunciado 
f1 = function(x)
{
   return(exp(sin(x)-cos(x^2)))
}
#Intervalo en el cual se desea hacer la paroximaci�n polin�mica
LI<- mpfr(-2^(-8),120)
LS<- mpfr(2^(-8),120)
#iteraciones es el grado estimado del polinomio
iteraciones = 500
#error es el error con el se desea evaluar la aproximaci�n
error <- mpfr(2^(-90),120)
print(error)
#Funci�n que se encarga de elaborar el polinomio apoximado de acuerdo
#con la funci�n dada.
#
#Los par�metros son los l�mites del itevalo sobre el que se desea hacer
#la aproximaci�n, e es el error, y n es el n�mero de iteraciones.
aproximacionPolinomica = function(limiteInferior,limiteSuperior,e,n)
{
  iteracion = 0
  T0 <- mpfr(1,120)
  T1 <- mpfr(0,120)
  T2 <- mpfr(0,120)
  while(iteracion<n)
  {
    if(iteracion==0) #Por si el Polinomio es T_0
    {
      T0=1
      print(T0)
    }
    if(iteracion==1) #Por si el Polinomio es T_1
    {
      T1=f1(limiteSuperior)
      print(T1)
    }
    if(iteracion>1) #De 1 en adelante 
    {
      T2=(2*limiteSuperior)*T1-T0
      T0=T1
      print(T2)
      if((T2-T1)<e)
      {
        return(T2)
      }
      T1=T2
    }
    return(T2)
    iteracion=iteracion+1
  }
}

#Llamado a la aproximaci�n polin�mica
print(aproximacionPolinomica(limiteInferior=LI,limiteSuperior=LS,e=error,n=iteraciones))