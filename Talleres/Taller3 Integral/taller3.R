funcion <- function(x){log(x)}

romberg(funcion, 1,2,tol = 1e-16)
resultado = NULL
resultadototal = 0

funcionI <- function(n,h,a,b){
  i = NULL
  if(n <= 1)
  {
    resultadofinal = 0
  }
  else{
    for(i in 1:n-1){
      resultado <- c(resultado, funcion(a+i*h))
    }
    resultadototal = sum(resultado)-1   
  }
  return(h/2*(funcion(a)+(2*resultadototal) +funcion(b)))
}
  
#n = 1,2,3,4,5,7 se toma 7 iteraciones, esto lo encontre debido a la función romberg()
#que dice que para era funcion con una tolerancia de 1e-16 se consigue con 7 iteraciones
#h = 2-1/n
#nivel1
final = NULL
final <- c(final,funcionI(1,1,1,2))
final <- c(final,funcionI(2,1/2,1,2))
final <- c(final,funcionI(3,1/3,1,2))
final <- c(final,funcionI(4,1/4,1,2))
final <- c(final,funcionI(5,1/5,1,2))
final <- c(final,funcionI(6,1/6,1,2))
final <- c(final,funcionI(7,1/7,1,2))
final
finalorganizada= NULL
finalorganizada <- c(finalorganizada,final[2])
finalorganizada <- c(finalorganizada,final[3] )
finalorganizada <- c(finalorganizada,final[4] )
finalorganizada <- c(finalorganizada,final[5] )
finalorganizada <- c(finalorganizada,final[6] )
finalorganizada <- c(finalorganizada,final[7] )
finalorganizada <- c(finalorganizada,final[1] )
#nivel2
#el valor de los coeficientes se consiguen de la siguiente formula 4^(k-1)/4^(k-1)-1*Im - 1/4^(k-1)-1*Il
final2 = NULL
final2 <- c(final2, (4/3*finalorganizada[2])-(1/3*finalorganizada[1]))
final2 <- c(final2, (4/3*finalorganizada[3])-(1/3*finalorganizada[2]))
final2 <- c(final2, (4/3*finalorganizada[4])-(1/3*finalorganizada[3]))
final2 <- c(final2, (4/3*finalorganizada[5])-(1/3*finalorganizada[4]))
final2 <- c(final2, (4/3*finalorganizada[6])-(1/3*finalorganizada[5]))
final2 <- c(final2, (4/3*finalorganizada[7])-(1/3*finalorganizada[6]))
final2
#nivel3
final3 = NULL
final3 <- c(final3,(16/15*final2[2])-(1/15*final2[1]))
final3 <- c(final3,(16/15*final2[3])-(1/15*final2[2]))
final3 <- c(final3,(16/15*final2[4])-(1/15*final2[3]))
final3 <- c(final3,(16/15*final2[5])-(1/15*final2[4]))
final3 <- c(final3,(16/15*final2[6])-(1/15*final2[5]))
final3
#nivel4
final4 = NULL
final4 <- c(final4,(64/63*final3[2])-(1/63*final3[1]))
final4 <- c(final4,(64/63*final3[3])-(1/63*final3[2]))
final4 <- c(final4,(64/63*final3[4])-(1/63*final3[3]))
final4 <- c(final4,(64/63*final3[5])-(1/63*final3[4]))
final4
#nivel5
final5 = NULL
final5 <- c(final5,(256/255*final4[2])-(1/255*final4[1]))
final5 <- c(final5,(256/255*final4[3])-(1/255*final4[2]))
final5 <- c(final5,(256/255*final4[4])-(1/255*final4[3]))
final5
#nivel6
final6 = NULL
final6 <- c(final6,(1024/1023*final5[2])-(1/1023*final5[1]))
final6 <- c(final6,(1024/1023*final5[3])-(1/1023*final5[2]))
final6
#nivel7
final7 = NULL
final7 <- c(final7,(4096/4095*final6[2])-(1/4095*final6[1]))
final7
print("El resultado de la integral es: ")
print(final7)