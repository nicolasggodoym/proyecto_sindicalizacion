use "SAIP AH007T0010295\OND2006.dta", clear

rename (sexo2 p07_2 p08a1_2 cse_2) (sexo p07 p08 cse)
destring p08, replace
label var sexo "Sexo"
label var p07 "Categoría laboral según versión chilena de CISE-93"
label var p08 "Rama de actividad económica según CIIU Rev. 2"
label var cse "Código sumario de empleo"
label drop sexo2 p07_2 p08a1_2 cse_2

#delimit;
label define sexo
1 "Hombre"
2 "Mujer", replace;
label define cise
1 "Empleador o patrón"
2 "Trabajador por cuenta propia, independiente"
3 "Asalariado del sector privado (empleado, obrero, jornalero)"
4 "Asalariado del sector público"
5 "Personal de servicio doméstico puertas adentro"
6 "Personal de servicio doméstico puertas afuera"
7 "Familiar o personal no remunerado", replace;
label define rama
1 "Agricultura, caza, silvicultura y pesca"
2 "Explotación de minas y canteras"
3 "Industrias manufactureras"
4 "Electricidad, gas y agua"
5 "Construcción"
6 "Comercio al por mayor y al por menor y restaurantes y hoteles"
7 "Transporte, almacenamiento y comunicaciones"
8 "Establecimientos financieros, seguros, bienes inmuebles y servicios prestados a las empresas"
9 "Servicios comunales, sociales y personales"
0 "Actividades no especificadas", replace;
#delimit cr

label values sexo sexo
label values p07 cise
label values p08 rama

tab p08 p07 [iw=fact_2] if (inlist(cse,1,2)) & sexo==1
tab p08 p07 [iw=fact_2] if (inlist(cse,1,2)) & sexo==1
tab p08 p07 [iw=fact_2] if (inlist(cse,1,2)) & sexo==2
