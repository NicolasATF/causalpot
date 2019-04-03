
program kplot1 

syntax anything [,TIPO(string)]

qui set obs `1'

if ("`tipo'" == "omitida") {
qui {

  g w = 10*runiform()
  g u = 0
  replace u = 1 if w > 5
  g x = 10 + 2*u + rnormal(10,1)
  g y = -0.5*x + 4*u + 1+ rnormal(10,1)
  
  local vars y x 
  foreach x of local vars {

  egen m`x' = mean(`x') , by(u)
  g `x'2 = `x' - m`x'
  
        forvalues u = 0(1)1 {
                sum `x' if u == `u'
                        global `x'`u' = r(mean)
                                }       
                                }
 cor y x
 mat cor1 = r(C)
 global h = round(cor1[2,1],0.001)
 
 cor y2 x2
 mat cor1 = r(C)
 global ha2 = round(cor1[2,1],0.001)
  
 twoway (scatter y x if u == 0) ///
 (scatter y x if u == 1) (lfit y x) , name(a, replace) ///
 graphregion(fcolor(white)) ytitle(Y) xtitle(X) ///
 legend(order(1 " u = 0" 2 "u = 1")) ///
 title("Datos en Bruto" "Correlacion de $h ")
 graph export correlation1.png,as(png) replace

 twoway (scatter y x if u == 0) ///
 (scatter y x if u == 1) , name(b, replace) xline($x0) xline($x1) ///
 graphregion(fcolor(white)) ytitle(Y) xtitle(X) ///
 legend(off)  ///
 title("Que diferencias en X" "son explicadas por U" )
 graph export correlation2.png,as(png) replace

 twoway (scatter y x2 if u == 0) ///
 (scatter y x2 if u == 1), name(c, replace)  ///
 graphregion(fcolor(white)) ytitle(Y) xtitle(X) ///
 legend(off)  ///
 title("Remover las diferencias" "en X explicadas por U") 
 graph export correlation3.png,as(png) replace

twoway (scatter y x2 if u == 0) ///
(scatter y x2 if u == 1), name(d, replace)  yline($y0) yline($y1)  ///
graphregion(fcolor(white)) ytitle(Y) xtitle(X) ///
legend(off) ///
title("Que diferencias en Y" "son explicadas por U")
 graph export correlation4.png,as(png) replace
 
twoway (scatter y2 x2 if u == 0) ///
(scatter y2 x2 if u == 1)(lfit y2 x2)  , name(e, replace) ///
graphregion(fcolor(white)) ytitle(Y) xtitle(X) legend(off) ///
title("Remover las diferencias" "en Y explicadas por U" "Corr: $ha2")
 graph export correlation5.png,as(png) replace


drop *
}
}

if ("`tipo'" == "psm") {
 qui{

  g w = 10*runiform()
  g u = 0
  replace u = 1 if w > 9.3
  g x = 10 + rnormal(10,5)

  g y = 1.5*x + 0.5*w+ 5*u + 5 + rnormal(10,4)
  g k = 0
  g m=.
  local con  " "
  local line " "
  gsort -u x
  sum x if u == 1
  forvalues i = 1(1)`r(N)'{
  local u`i' = x[`i'] + `2'
  local l`i' = x[`i'] - `2'
  local con`i' = " (x < `u`i'' & x > `l`i'')"
  local line "`line' xline(`u`i'' , lcolor(black) lpattern(shortdash) lwidth(vvvthin) ) xline(`l`i'' , lpattern(shortdash) lwidth(vvvthin) lcolor(black))"
  replace m = `i' if `con`i'' 
  }

 twoway (scatter y x if u == 1, msize(vsmall)) ///
 (scatter y x if u == 0, msize(vsmall)) , name(a, replace)  ///
 graphregion(fcolor(white)) legend(order(1 "Tratado" 2 "Control")) ///
 title("Diseño Matching" "Datos en Bruto") ytitle(" ") xtitle(" ")
 graph export correlation1.png,as(png) replace

 twoway (scatter y x if u == 1, msize(vsmall)) ///
 (scatter y x if u == 0, msize(vsmall)), `line' name(b, replace) ///
  graphregion(fcolor(white))  legend(off) ///
  title("Obs similares" "En terminos de X") ytitle(" ") ///
  xtitle(" ")
  graph export correlation2.png,as(png) replace

keep if m != .

g u2 = 1
replace u2 = 0 if u == 1
egen  z = total(u + u2), by(m)

twoway (scatter y x if u == 1, msize(vsmall)) ///
(scatter y x if u == 0, msize(vsmall)), `line' ///
name(c, replace) graphregion(fcolor(white)) ///
legend(off)  ///
title("Nos enfocamos en" "las Obs similares" "En un radio de `2'")
graph export correlation3.png,as(png) replace

egen my = mean(y),by(u) 
  
  forvalues i = 0(1)1{
  sum y if u == `i' 
  local m`i' = `r(mean)'
  }
  keep if z > 1 

  sum z
  
twoway (scatter y x if u == 1, msize(vsmall)  yline(`m1' , lcolor(black))) ///
(scatter y x if u == 0, msize(vsmall) yline(`m0' , lcolor(black))), graphregion(fcolor(white)) ///
 name(d, replace) legend(off)  ///
 title("Mantemos `r(N)' Obs" "Comparables" "Calculamos E(y) Para" "cada grupo") ///
 ytitle(" ") xtitle(" ")
 graph export correlation4.png,as(png) replace

 twoway   (line my x if u == 1, msize(vsmall) lcolor(navy)) ///
 (line my x if u == 0, msize(vsmall) lcolor(maroon)) ///
 , graphregion(fcolor(white)) ///
 name(e, replace)  legend(off) ///
 title("El ATE es" "E(Y|D=1) - E(Y|D=0)")
 graph export correlation5.png,as(png) replace

drop *

}
}
if ("`tipo'" == "dd") {

qui {
 
        g w = 10*runiform()
        g t = 10*runiform()
        g u = 0
        replace u = 1 if w > 5
        g d = 0 
        replace d = 1 if t > 5

        g y = 3*u + 5*t^0.5+ d*2+ 5*u*d + rnormal(5,2)
        g k = 0
        
        forvalues i = 0(1)1{
                forvalues j = 0(1)1{
                        qui sum y if d == `i' & u == `j'
                        local m`i'`j' = r(mean)
                        }
                }
                
        local m1 = `m10' - `m00'
        local m0 = `m11' - `m01'
        
        g y2 = y
        replace y2 = y - `m1' if u == 0 & d == 1
        replace y2 = y - `m1' if u == 1 & d == 1
        
        egen my = mean(y), by(u d)
        egen my2 = mean(y2), by(u d)

        global op1 mcolor(gs7) msize(vsmall)
        global op2 mcolor(black) msize(vsmall)
        global op3 msize(vsmall)
        global op4 mcolor(maroon) msize(vsmall)
        global op5 mcolor(navy8) msize(vsmall) lcolor(navy8)
        global op6 lcolor(maroon) msize(vsmall)

        twoway (scatter y t if u == 0 , $op1 ) ///
        (scatter y t if u == 1, $op2 ) , name(a, replace) ///
        graphregion(fcolor(white)) legend(off) xtitle("Tiempo") ///
        xline(5, lpattern(longdash)) ytitle("Resultado") ///
        title("Diseño DIFDIF" "Datos en Bruto")
         graph export correlation1.png,as(png) replace

		
        twoway (scatter y t if u == 0, $op1 ) ///
        (scatter y t if u == 1, $op2 ) ///
        (line my t if u == 0 & d == 0 , $op5) ///
        (line my t if u == 0 & d == 1 , $op5) ///
        (line my t if u == 1 & d == 0, $op6) ///
        (line my t if u == 1 & d == 1, $op6), name(b, replace)  ///
        graphregion(fcolor(white)) legend(off) xtitle("Tiempo") ///
        xline(5, lpattern(longdash)) ytitle("Resultado")  ///
        title("Variacion explicada" "por Grupos y Tiempo")
        graph export correlation2.png,as(png) replace
		
        twoway (line  my t if u == 0 & d == 0, $op5) ///
        (line  my t if u == 0 & d == 1, $op5) ///
        (line  my t if u == 1 & d == 0 , $op6 ) ///
        (line  my t if u == 1 & d == 1 , $op6 ) , name(c, replace) ///
        graphregion(fcolor(white))   legend(off) xtitle("Tiempo") ///
        xline(5, lpattern(longdash)) ytitle("Resultado") ///
        title("Mantenemos los" "promedios" "en D y T")
        graph export correlation3.png,as(png) replace
		
        twoway (line  my2 t if u == 0 , $op3 ) ///
        (line  my2 t if u == 1 & d == 1, $op6 ) ///
        (line  my2 t if u == 1 & d == 0, $op6 ) , ///
        graphregion(fcolor(white)) name(d, replace) ///
        legend(off) xtitle("Tiempo") ///
        xline(5, lpattern(longdash)) ytitle("Resultado") ///
        title("ATE" "Doble Diferencia")
        graph export correlation4.png,as(png) replace
        
        drop *
        }
        }
if ("`tipo'" == "rd") {
        qui {
        g t = 10*runiform()
        
        g d = 0
        replace d = 1 if t > 5
        
        g y = 2 + t - 0.8*t^2 + 10*d + 0.1*t^3 + rnormal(5,5)
        xtile q = t, nquantiles(10)
        
        egen my0 = mean(y) if d == 0, by(q)
        egen my1 = mean(y) if d == 1, by(q)
        
        g my = my0 if d == 0
        replace my = my1 if d == 1
        
        scatter y t , xline(5, lpattern(longdash)) ///
        name(a, replace) msize(vsmall) xtitle("Puntaje") ///
        ytitle("Resultado") graphregion(fcolor(white)) ///
        title("Diseño RD" "Datos en Bruto")
         graph export correlation1.png,as(png) replace
		 
        twoway (scatter y t , xline(5, lpattern(longdash)) msize(vsmall)) ///
        (scatter my t , xline(5, lpattern(longdash)) ///
        msize(vsmall)) , name(b, replace) xtitle("Puntaje") ///
        ytitle("Resultado") graphregion(fcolor(white)) legend(off) ///
        title("Variacion Explicada por" "la variable Running")
         graph export correlation2.png,as(png) replace
		 
        scatter my t , msize(vsmall) ///
        xline(5, lpattern(longdash)) name(c, replace) ///
        msize(vsmall) mcolor(maroon) xtitle("Puntaje") ///
        ytitle("Resultado") graphregion(fcolor(white)) ///
        title("Nos quedamos" "la variacion" "explicada por R")
         graph export correlation3.png,as(png) replace
		 
        twoway (scatter  my t if t < 5  & t > 5 - `2', msize(vsmall) ) ///
        (scatter my t if t > 5  & t < 5 + `2', msize(vsmall) ) ,  ///
        xline(5, lpattern(longdash))  name(d, replace) ///
        xtitle("Puntaje")  ///
        ytitle("Resultado") graphregion(fcolor(white)) ///
        title("Efecto LATE" "Impacto Local") legend(off)
         graph export correlation4.png,as(png) replace
		 
      
        drop *
        }
        }
end     

