;-----------------------------------------------------------------
;   -----> PRACTICA 1
;   ASIGNATURA: Lenguajes de Programación
;   TRABAJO: Practica 1 - Spirograph con Lisp
;   AUTORES: Mario Ventura, Luis Miguel Vargas, Alberto Ruiz
;   CURSO: 2022-2023
;-----------------------------------------------------------------


;-----------------------------------------------------------------
;---------------------------< PARTE 1 >---------------------------
;-----------------------------------------------------------------
;
;   FUNCIONES:
;   1. (guarda-informacio)
;   2. (vermell), (blau), (verd), (negre)
;   3. (cercle x y radi n)
;   4. (radigran r)
;   5. (radipetit r)
;   6. (punt p),(inici angle),(escala e),(posicio x y)
;   7. (reduir m n)
;
;-----------------------------------------------------------------

;-----> 1.1 GUARDA-INFORMACIO
(defun guarda-informacio ()

    ;   Ha de guardar dins les propietats d’un àtom simbòlic anomenat
    ;   “spiro” la següent informació:

    ;   - grans: conté una llista on cada element és una llista amb la informació d’un dels
    ;     cercles grans (número de dents exteriors i interiors).
    ;   - petits: conté una llista on cada element és una llista amb la informació d’un dels
    ;     cercles petits (dents, número de forats i diàmetre).
    ;   - rgran: conté el valor del número de dents del cercle gran (per defecte 150).
    ;   - rpetit: conté el valor del número de dents del cercle petit (per defecte 50).
    ;   - punt: conté el número de punt a partir del que pintarem (per defecte 3).
    ;   - inici: conté l’angle en graus del cercle gran a partir del que començarem el dibuix.
    ;   - escala: valor que escalarà el dibuix (per defecte 1.8).
    ;   - interior: valor booleà que indica si esteim pintant per l’interior o no.
    ;   - x: coordenada x de la posició on es pintarà el dibuix.
    ;   - y: coordenada y de la posició on es pintarà el dibuix.
    ;   - pas: indica la variació de l’angle a cada passa dels càlculs (per defecte 0.2).
    ;
    ;   Aquesta funció, que inicialitza les dades del nostre spirograph, s’ha d’avaluar cada cop que
    ;   es llegeixi el fitxer que conté el codi font en LISP de la vostra pràctica.

    ;   Se usa putprop para darle valor al àtom simbólic llamado 'spiro'.
    ;   Para hacerlo, "crea" y da valor a todas las variables/propiedades
    ;   descritas anteriormente

    ; Se crea y da valor a la propiedad grans y se añade a 'spiro'
    (putprop 'spiro '(
        (150 105)   ; Círculo grande 1
        (144 96))   ; Círculo grande 2
    'grans)
    
    ; Se crea y da valor a la propiedad petits y se añade a 'spiro'
    (putprop 'spiro '(
        (84 35 56)  ; Círculo pequeño 1
        (80 33 53)  ; Círculo pequeño 2
        (75 31 50)  ; Círculo pequeño 3
        (72 29 48)  ; Círculo pequeño 4
        (63 25 42)  ; Círculo pequeño 5
        (60 23 40)  ; Círculo pequeño 6
        (56 21 37)  ; Círculo pequeño 7
        (52 19 35)  ; Círculo pequeño 8
        (48 17 32)  ; Círculo pequeño 9
        (45 16 30)  ; Círculo pequeño 10
        (42 14 28)  ; Círculo pequeño 11
        (40 13 27)  ; Círculo pequeño 12
        (32 9 21)   ; Círculo pequeño 13
        (30 8 20)   ; Círculo pequeño 14
        (24 5 16))  ; Círculo pequeño 15
    'petits)

    ; Se crea y da valor a la propiedad rgran y se añade a 'spiro'
    (putprop 'spiro '150 'rgran)

    ; Se crea y da valor a la propiedad rpetit y se añade a 'spiro'
    (putprop 'spiro '52 'rpetit)

    ; Se crea y da valor a la propiedad punt y se añade a 'spiro'
    (putprop 'spiro '3 'punt)

    ; Se crea y da valor a la propiedad inici y se añade a 'spiro'
    (putprop 'spiro '0 'inici)

    ; Se crea y da valor a la propiedad escala y se añade a 'spiro'
    (putprop 'spiro '1.8 'escala)

    ; Se crea y da valor a la propiedad interior y se añade a 'spiro'
    (putprop 'spiro 't 'interior)

    ; Se crea y da valor a la propiedad x y se añade a 'spiro'
    (putprop 'spiro '0 'x)

    ; Se crea y da valor a la propiedad y y se añade a 'spiro'
    (putprop 'spiro '0 'y)

    ; Se crea y da valor a la propiedad xauxiliar y se añade a 'spiro'
    (putprop 'spiro '0 'xauxiliar)

    ; Se crea y da valor a la propiedad yauxiliar y se añade a 'spiro'
    (putprop 'spiro '0 'yauxiliar) 

    ; Se crea y da valor a la propiedad pas y se añade a 'spiro'
    (putprop 'spiro '0.2 'pas)
)
(guarda-informacio)

;-----> 1.2.1 VERMELL
(defun vermell () (color 255 0 0))

;-----> 1.2.2 VERD
(defun verd () (color 0 255 0))

;-----> 1.2.3 BLAU
(defun blau () (color 0 0 255))

;-----> 1.2.4 NEGRE
(defun negre () (color 0 0 0))

;-----> 1.3 CERCLE (X Y RADI N)
(defun cercle (x y radi n)
    ;(igual que la de los apuntes)
    ; Se mueve la posición del lápiz mediante la función auxiliar mover-lapiz para que
    ; no deje una línea desde la esquina inferior izquierda hasta (x,y)
    (mover-lapiz (+ x radi) y)    ; lápiz en posición (x,y)
    ; Se dibuja el circulo con radio radi en la coordenadas (x,y)
    ; Dado que un circulo tiene 360 grados, para que los segmentos sean
    ; "simétricos", se obtendrá los segmentos haciendo 360/n
    (dibujarcercle x y radi (/ 360 n) 0)    ; Ángulo de paso será 0 por defecto
)

; Función auxiliar que llama a "move" de Lisp para desplazar la posición del lápiz a (x,y)
; (igual que la de los apuntes)
(defun mover-lapiz (x y)
    (move (realpart (round (+ 320 (* (get 'spiro 'escala) x))))
        (realpart (round (+ 187 (* (get 'spiro 'escala) y)))))
)

; Función auxiliar dibujarcercle (igual que la de los apuntes)
(defun dibujarcercle (x y radi pas angle)
    ; Comprobar que el ángulo es menor a 360
    (cond ((< angle 360)
        (pinta (+ x (* radi (cos (radians (+ angle pas))))) ; Formula para x
            (+ y (* radi (sin (radians (+ angle pas))))))   ; Formula para y
        (dibujarcercle x y radi pas (+ angle pas)))
        ; Caso trivial t=t
        (t t)
    )
)

; Función auxiliar pinta (igual que la de los apuntes)
(defun pinta (x y)
    ; Llamada a la función draw de Lisp
    (draw (realpart (round (+ 320 (* (get 'spiro 'escala) x))))
        (realpart (round (+ 187 (* (get 'spiro 'escala) y)))))
)

; Función auxiliar que convierte de grados a radianes
(defun radians (graus)
    (/ (* graus (* 2 pi)) 360)
)

;-----> 1.4 RADIGRAN
(defun radigran (r)
    ; Función que actúa como setter del radio grande
    ; Cambiamos el valor que tiene el radio grande por defecto y pintamos un círculo
    ; Spiro tendrá r con valor rgran
    (putprop 'spiro r 'rgran)
    ; Pinta el círculo con los valores x e y de spiro, que obtiene mediante un get 
    (cercle (get 'spiro 'x) (get 'spiro 'y) r 60) 
)

;-----> 1.5 RADIPETIT
(defun radipetit (r)
    ; Función que actúa como setter del radio pequeño
    ; Hacemos copia por seguridad
    (setq x (get 'spiro 'x))
    (setq y (get 'spiro 'y))
    ; Cambiamos el valor que tiene el radio pequño por defecto y pintamos un círculo
    ; en la posición inicial del círculo pequeño
    (putprop 'spiro r 'rpetit)
    ; Spiro tendrá r con valor rgran
    ; Al igual que con el radio grande, se obtiene la nueva posicion mediante getters del "objeto" spiro
    (posicio (calcularX (get 'spiro 'rgran) (get 'spiro 'rpetit) (radians (get 'spiro 'inici)))
            (calcularY (get 'spiro 'rgran) (get 'spiro 'rpetit) (radians (get 'spiro 'inici))))
    
    ; Se pinta el círculo mediante llamada a (cercle) que llama a (dibujarcercle)
    (cercle (get 'spiro 'x) (get 'spiro 'y) r 60)
    ; Recuperamos la posicion almacenada anteriormente
    (posicio x y)
)

; Función auxiliar para el cálculo de la X
(defun calcularX (rg rp a)
    (* (- rg rp) (sin a))
)

; Función auxiliar para el cálculo de la Y
(defun calcularY (rg rp a)
    (* (- rg rp) (cos a))
)

;-----> 1.6.1 INICI 
(defun inici (angle)
    ; Actúa como función setter de 'angle' en 'spiro'
    ; Modifica el valor del angulo angle en spiro
    (putprop 'spiro angle 'inici)
)

;-----> 1.6.2 PUNT
(defun punt (p)
    ; Actúa como función setter de 'punt' en 'spiro'
    ; Modifica el valor de punt en spiro
    (putprop 'spiro p 'punt)
)

;-----> 1.6.3.1 POSICIÓ
(defun posicio (x y)
    ; Actúa como función setter de 'x' e 'y' en 'spiro'
    ; Modifica el valor de la posición (x,y) en spiro
    (putprop 'spiro x'x)
    (putprop 'spiro y'y)
)

;-----> 1.6.3.2 POSICIÓ AUXILIAR
(defun posicioAux (x y)
    ; Actúa como setter de xauxiliar e yauxiliar en 'spiro'
    ; Modifica el valor de la posicion (xauxiliar,yauxiliar) en spiro 
    (putprop 'spiro x'xauxiliar)
    (putprop 'spiro y'yauxiliar)
)

;-----> 1.6.4 ESCALA
(defun escala (e)
    ; Actúa como función setter de 'escala' en 'spiro'
    ; Modifica el valor de escala en spiro
    (putprop 'spiro e 'escala)
)

;-----> 1.6.5 INTERIOR
(defun interior (x)
    ; Actúa como función setter de 'interior' en 'spiro'
    ; Modifica el valor de interior en spiro
    (putprop 'spiro x 'interior)

)

;-----> 1.7 REDUIR
(defun reduir (m n)
    ; "Calcula la fracció reduïda de m i n i torna una llista amb els dos valors de la nova fracció.
    ; Això es pot fer calculant el màxim comú divisor (mcd) de m i n i després dividint tant el
    ; numerador com el denominador pel mcd"

    ; Para devolverlo en forma de lista se usará la función list para crear una lista
    ; f = R/r = (R/mcd(R,r))/(r/mcd(R/r)) = R'/r'
    ; Se llamará a list con parametros R' y r' para añadirlos
    (list(/ m (gcd m n)) (/ n (gcd m n)))
)

; Función auxiliar que calcula el MCD de 2 parámetros dados
;(defun mcd (a b)
    ; Si b es zero se devuelve a
    ; Para saber si b es 0 se usa la función 'zerop' de Lisp, que devuelve t si el parametro que
    ; recibe es igual a 0 y nil en caso contrario
;    (if (zerop b)
;        a   ; mcd = a
        ;En caso contrario se hace llamada recursiva mcd con b y el resto de a/b.
;        (mcd b (mod a b)))
;)
; Se puede usar funcion gcd de Lisp


;-----------------------------------------------------------------
;---------------------------< PARTE 2 >---------------------------
;-----------------------------------------------------------------
;
;   FUNCIONES:
;   1. (spirograph p gran petit t inc inici)
;   2. (spiro gran petit p inc inici)
;
;-----------------------------------------------------------------

;-----> 2.1 SPIROGRAPH
(defun spirograph (p gran petit d inc inici)
    ; Simula el comportament d’un spirograph amb el número de passes p, amb els radis gran i
    ; petit, amb la distancia t (veure la figura 3), un increment inc a cada passa i amb l’inici del
    ; dibuixat a l’angle donat en graus

    ; Se usan las formulas 9 y 10 dependiendo de si se utiliza el exterior del circulo grande o el pequeño.
    
    ; La propiedad grans de spiro es una lista con listas. Esto implica un doble acceso. Hay que comparar
    ; 'gran' con el segundo elemento de grans[0] y grans[1] ya que es el interior del circulo
    (cond ((or (= gran (agafar-n 1(agafar-n 0(get 'spiro 'grans)))) (= gran (agafar-n 1(agafar-n 1(get 'spiro ' grans)))))
        ; ES INTERIOR ---> Usar formula 9
        ;(print "---> ES INTERIOR") ;debugging comment

        ; Modificar el valor de 'interior' con true para indicar que es interior
        (putprop 'spiro t 'interior)
        ; Modificar el inicio
        (inici inici)
        ; Modificar x e y segun la formula nº9
        (posicioAux (calcula-x-interior gran petit d inici) (calcula-y-interior gran petit d inici))
        ; Modificar x e y con las formulas del apartado 2
        (posicioAux (rotar-x (get 'spiro 'xauxiliar) (get 'spiro 'yauxiliar) inici) (rotar-y (get 'spiro 'xauxiliar) (get 'spiro 'yauxiliar) inici))
        ;(setq inici (+ inici inc))
        ; Mover el lapiz a la posicion (x+xauxiliar,y+yauxiliar)
        (mover-lapiz (+ (get 'spiro 'x) (get 'spiro 'xauxiliar)) (+ (get 'spiro 'y) (get 'spiro 'yauxiliar)))
        ; Llamar a funión recursiva con los mismos parámetros que spirograph pero con p decrementado inc veces y con
        ; inici incrementado para el ángulo de giro con el incremento inc
        (spirograph-recursiva-interior (- p inc) gran petit d inc (+ inici inc))


    ; Hay que comparar 'gran' con el primer elemento de grans[0] y grans[1] ya que es el exterior del circulo
    ) ((or (= gran (agafar-n 0 (agafar-n 0 (get 'spiro 'grans)))) (= gran (agafar-n 0 (agafar-n 1 (get 'spiro ' grans)))))
        ; ES EXTERIOR ---> Usar formula 10
        ;(print "---> ES EXTERIOR") ;debugging comment

        ; Modificar el valor de 'interior' con nil para indicar que es exterior
        (putprop 'spiro nil 'interior)
        ; Modificar el inicio
        (inici inici)
        ; Modificar x e y segun la formula nº10
        (posicioAux (calcula-x-exterior gran petit d inici) (calcula-y-exterior gran petit d inici))
        ; Modificar x e y con las formulas del apartado 2
        (posicioAux (rotar-x (get 'spiro 'xauxiliar) (get 'spiro 'yauxiliar) inici) (rotar-y (get 'spiro 'xauxiliar) (get 'spiro 'yauxiliar) inici))
        ; Mover el lapiz a la posicion (x+xauxiliar,y+yauxiliar)
        (mover-lapiz (+ (get 'spiro 'x) (get 'spiro 'xauxiliar)) (+ (get 'spiro 'y) (get 'spiro 'yauxiliar)))
        ; Llamar a funión recursiva con los mismos parámetros que spirograph pero con p decrementado inc veces y con
        ; inici incrementado para el ángulo de giro con el incremento inc
        (spirograph-recursiva-exterior (- p inc) gran petit d inc (+ inici inc))
    )
    ; Caso trivial: los parametros recibidos no coinciden con los valores de la tabla 1 del encunciado
    (t (print "ERROR! ---> VALOR NO VALIDO")))
)

;Función auxiliar feta a classe que agafa l'enèsim element d'una llista
;(agafar-n 3 '(a b c d e f))
(defun agafar-n (n l)
    (cond 
        ; Si la lista es null se devuelve nil
        ((null l) nil)
        ; Si n = 0 se quiere el primer elemento de la lista y se hace car de esta lista
        ((= n 0) (car l))
        ; Si no se quiere el primer elemento, se hara (agafar-n (n-1) (lista sin el primer elemento))
        (t (agafar-n (- n 1) (cdr l)))
    )
)

; Función auxiliar que permite obtener la 'x' aplicando la formula 9 del enunciado
(defun calcula-x-interior (rg rp desp a)
    ; x en formula 9:
    ; x = (R-r)·cos((r·a)/R) + t·cos((1-(r/R))·a)
    ; Dado que 't' hace que Lisp piense que nos referimos a true, llamaremos 'desp' a t
    (+ (* (- rg rp) (cos (/ (* rp a) rg)))
    (* desp (cos (* a (- 1 (/ rp rg))))))
)

; Función auxiliar que permite obtener la 'y' aplicando la formula 9 del enunciado
(defun calcula-y-interior (rg rp desp a)
    ; x en formula 9:
    ; x = (R-r)·sin((r·a)/R) - t·sin((1-(r/R))·a)
    ; Dado que 't' hace que Lisp piense que nos referimos a true, llamaremos 'desp' a t
    (- (* (- rg rp) (sin (/ (* rp a) rg)))
    (* desp (sin (* a (- 1 (/ rp rg))))))
)

; Función auxiliar que permite obtener la 'x' aplicando la formula 10 del enunciado
(defun calcula-x-exterior (rg rp desp a)
    ; x en formula 10:
    ; x = (R+r)·cos((r·a)/R) - t·cos((1+(r/R))·a)
    ; Dado que 't' hace que Lisp piense que nos referimos a true, llamaremos 'desp' a t
    (- (* (+ rg rp) (cos (/ (* rp a) rg)))
    (* desp (cos (* a (+ 1 (/ rp rg))))))
)

; Función auxiliar que permite obtener la 'y' aplicando la formula 10 del enunciado
(defun calcula-y-exterior (rg rp desp a)
    ; y en formula 10:
    ; x = (R-r)·sin((r·a)/R) - t·sin((1-(r/R))·a)
    ; Dado que 't' hace que Lisp piense que nos referimos a true, llamaremos 'desp' a t
    (- (* (+ rg rp) (sin (/ (* rp a) rg)))
    (* desp (sin (* a (+ 1 (/ rp rg))))))
)

; Función auxiliar que permite establecer la x del angulo inicial una vez calculado el
; punto que se debe pintar. Esta función rota el punto (x,y) segun el ángulo de inicio
(defun rotar-x (x y a)
    ; La fórmula a aplicar es: x' = x·cos(alfa) + y·sin(alfa)
    (+ (* x (cos a))
    (* y (sin a)))
)

; Función auxiliar que permite establecer la y del angulo inicial una vez calculado el
; punto que se debe pintar. Esta función rota el punto (x,y) segun el ángulo de inicio
(defun rotar-y (x y a)
    ; La fórmula a aplicar es: y' = -x·sin(alfa) + y·cos(alfa)
    (+ (* (sin a) (- x))
    (* y (cos a)))
)

; Función auxiliar recursiva que pinta un spirograph interior iterando un numero de pasos
(defun spirograph-recursiva-interior (p gran petit d inc inici)
    ; Comprobar que el número de pasos es mayor que 0
    (cond ((> p 0)
        ; Modificar x e y segun la formula nº9
        (posicioAux (calcula-x-interior gran petit d inici) (calcula-y-interior gran petit d inici))
        ; Modificar x e y con las formulas del apartado 2
        (posicioAux (rotar-x (get 'spiro 'xauxiliar) (get 'spiro 'yauxiliar) (get 'spiro 'inici)) (rotar-y (get 'spiro 'xauxiliar) (get 'spiro 'yauxiliar) (get 'spiro 'inici)))
        (setq inici (+ inici inc))
        ; Llamada a pinta con x e y
        (pinta (+ (get 'spiro 'xauxiliar) (get 'spiro 'x)) (+ (get 'spiro 'yauxiliar) (get 'spiro 'y)))
        ; Llamar a funión recursiva con los mismos parámetros que spirograph pero con p decrementado inc veces y con
        ; inici incrementado para el ángulo de giro con el incremento inc
        (setq p (- p inc))
        (spirograph-recursiva-interior p gran petit d inc inici)
    ))
)

; Función auxiliar recursiva que pinta un spirograph exterior iterando un numero de pasos
(defun spirograph-recursiva-exterior (p gran petit d inc inici)
    ; Comprobar que el número de pasos es mayor que 0
    (cond ((> p 0)
        ; Modificar x e y segun la formula nº10
        (posicioAux (calcula-x-exterior gran petit d inici) (calcula-y-exterior gran petit d inici))
        ; Modificar x e y con las formulas del apartado 2
        (posicioAux (rotar-x (get 'spiro 'xauxiliar) (get 'spiro 'yauxiliar) (get 'spiro 'inici)) (rotar-y (get 'spiro 'xauxiliar) (get 'spiro 'yauxiliar) (get 'spiro 'inici)))
        (setq inici (+ inici inc))
        ; Llamada a pinta con x e y
        (pinta (+ (get 'spiro 'xauxiliar) (get 'spiro 'x)) (+ (get 'spiro 'yauxiliar) (get 'spiro 'y)))
        ; Llamar a funión recursiva con los mismos parámetros que spirograph pero con p decrementado inc veces y con
        ; inici incrementado para el ángulo de giro con el incremento inc
        (setq p (- p inc))
        (spirograph-recursiva-exterior p gran petit d inc inici)
    ))
)

;-----> 2.2 SPIRO
(defun spiro (gran petit p inc inici)
    ; Esta función hace lo mismo que spirograph pero con un número de vueltas.
    ; Se usa 'petits' en lugar de 'grans'

    ; Se establece la posicion en la que se va a dibujar con el parametro 'petit'. Se tendrá que
    ; comprobar si este parametro existe en la lista petits y por tanto es válido
    (setq posicion (comprobar-si-existe petit (my-length (get 'spiro 'petits))))
    
    ; comprobar-si-existe devolverá nil o el numero de la posicion dependiendo de si 'petit' está en petits
    (cond ((null posicion) (print "ERROR ---> El valor del radio no es valido o no existe"))
        (t
            ; El valor es válido, comprobar-si-existe ha devuelto la posicion de petit en petits

            ; Se debe calcular el numero de pasos y el angulo t y llamar a la función spiro
            ; Calcular numero de pasos:
            ; numPasos = 2pi * (reduir rgran rpetit) * (rgran/rpetit)
            (setq numPasos (realpart(round(* 2 pi (* (cadr (reduir gran petit)) (/ gran petit))))))
            ; Calcular angulo t
            ; t = (num puntos-punto)*(radipetit/num puntos)
            (setq angulo (- (agafar-n 1 (agafar-n posicion (get 'spiro 'petits))) p))
            (setq angulo (* angulo (/ petit (agafar-n 1 (agafar-n posicion (get 'spiro 'petits))))))
            ; Llamada a spirograph con los valores calculados para pintar el spiro
            (spirograph numPasos gran petit angulo inc inici)
        )
    )
)

; Función auxiliar que calcula la longitud de una lista
(defun my-length (lista)
    ; Posible caso de lista vacia: longitud = 0
    (cond ((null lista) 0)
        ; Si la lista no esta vacía, se aumenta contador y se llama recursivamente para cdr lista
        (t (+ 1 ( my-length (cdr lista))))
    )
)

; Función auxiliar que comprueba si un elemento existe en la lista  de pequeños itertando "it" veces
(defun comprobar-si-existe (elemento it)
    ; Comprobamos que sigan quedando iteraciones
    (cond ((> it 0)
        ; Comprobación recursiva
        ; Va de 0 a n-1 por tanto se le restará 1 a 'iteraciones' para guardar en 'pos'
        (setq pos (- it 1))
        ; Comprobar si está en la lista. Si está se devuelve la posicion, es decir, el numero
        ; de circulo que este ocupa en la lista
        (cond ((= elemento (agafar-n 0(agafar-n pos (get 'spiro 'petits)))) pos)
        ; Si no esta en posicion pos, comprobacion con llamada recursiva con (pos-1)
        (t (setq it (- it 1)) (comprobar-si-existe elemento it)))
    ))
)

;-----------------------------------------------------------------
;---------------------------< PARTE 3 >---------------------------
;-----------------------------------------------------------------
;
;   FUNCIONES:
;   1. (roda)
;   2. (roda-voltes n)
;   3. (spiro-voltes voltes gran petit p in inici)
;   4. (spiros l)
;   5. (dibuix)
;
;-----------------------------------------------------------------

;-----> 3.1 RODA
(defun roda()
    ; Hace una simulación completa del spirograph cogiendo los valores que tiene
    ; el átomo 'spiro' en ese momento

    ; Llamar a (roda) es equivalente a llamar a (spiro), pero con la comodidad de 
    ; no tene que poner parámetros, ya que se usan los valores ya establecidos.
    ; Llamaremos a (spiro) con los 'get' de sus propiedades como parámetro
    ; (spiro (gran petit p inc inici))
    (spiro (get 'spiro 'rgran) (get 'spiro 'rpetit) (get 'spiro 'punt) (get 'spiro 'pas) (get 'spiro 'inici))
)

;-----> 3.2 RODA-VOLTES
(defun roda-voltes (n)
    ; Hace lo mismo que (roda) pero con n vueltas
    ; El funcionamiento es idéntico a spiro, ya que este llama a spirograph diciéndole
    ; cuántas vueltas quiere hacer, por tanto, usaremos el código de (spiro) con pequeñas
    ; modificaciones

    ; Esta función hace lo mismo que spirograph pero con un número de vueltas.
    ; Se usa 'petits' en lugar de 'grans'

    ; Se establece la posicion en la que se va a dibujar con el parametro 'petit'. Se tendrá que
    ; comprobar si este parametro existe en la lista petits y por tanto es válido
    (setq posicion (comprobar-si-existe (get 'spiro 'rpetit) (my-length (get 'spiro 'petits))))
    
    ; comprobar-si-existe devolverá nil o el numero de la posicion dependiendo de si 'petit' está en petits

    (cond ((null posicion) (print "ERROR ---> El valor del radio no es valido o no existe"))
        (t
            ; El valor es válido, comprobar-si-existe ha devuelto la posicion de petit en petits

            ; Se debe calcular el numero de pasos y el angulo t y llamar a la función spiro
            ; Calcular numero de pasos:
            ; numPasos = 2 * n * pi^2 / pi
            (setq numPasos (realpart(round(*(*(* 2 pi) n) (/ pi 2)))))
            ; Calcular angulo t
            ; t = (num puntos-punto)*(radipetit/num puntos)
            (setq angulo (- (agafar-n 1 (agafar-n posicion (get 'spiro 'petits))) (get 'spiro 'punt)))
            (setq angulo (* angulo (/ (get 'spiro 'rpetit) (agafar-n 1 (agafar-n posicion (get 'spiro 'petits))))))
            ; Llamada a spirograph con los valores calculados para pintar el spiro
            (spirograph numPasos (get 'spiro 'rgran) (get 'spiro 'rpetit) angulo (get 'spiro 'pas) (get 'spiro 'inici))
        )
    )
)

;-----> 3.3 SPIRO-VOLTES
(defun spiro-voltes (voltes gran petit p in inici)
    ; Es igual al anterior pero simula el comportamiento de un spirograph de
    ; con los argumentos dados: voltes, gran, petit, p, in, inici 

    ; Se establece la posicion en la que se va a dibujar con el parametro 'petit'. Se tendrá que
    ; comprobar si este parametro existe en la lista petits y por tanto es válido
    (setq posicion (comprobar-si-existe petit (my-length (get 'spiro 'petits))))
    
    ; comprobar-si-existe devolverá nil o el numero de la posicion dependiendo de si 'petit' está en petits
    (cond ((null posicion) (print "ERROR ---> El valor del radio no es valido o no existe"))
        (t
            ; El valor es válido, comprobar-si-existe ha devuelto la posicion de petit en petits

            ; Se debe calcular el numero de pasos y el angulo t y llamar a la función spiro
            ; Calcular numero de pasos:
            ; numPasos = 2 * n * pi^2 / pi
            (setq numPasos (realpart(round(*(*(* 2 pi) voltes) (/ pi 2)))))
            ; Calcular angulo t
            ; t = (num puntos-punto)*(radipetit/num puntos)
            (setq angulo (- (agafar-n 1 (agafar-n posicion (get 'spiro 'petits))) p))
            (setq angulo (* angulo (/ petit (agafar-n 1 (agafar-n posicion (get 'spiro 'petits))))))
            ; Llamada a spirograph con los valores calculados para pintar el spiro
            (spirograph numPasos gran petit angulo in inici)
        )
    )
)

; -----> 3.4 SPIROS
(defun spiros (l)
    ; Hace todas las simulaciones con las listas contenidas dentro de la lista l (el formato de cada
    ; elemento de la lista es una lista con los parametros correspondientes a la llamada a la funcion
    ; spiro: radigran, radipetit, p, incremento y angulo de inicio)

    ; El planteamiento será llamar a 'spiro' para cada elemento de la lista l. Podemos usar mapcar o
    ; hacer un recorrido de la lista, pero mapcar es la opción más eficiente computacionalmente
    ; Contemplar que se pueda recibir una lista vacía
    (cond ((null l) (print "La lista introducida está vacía"))
    ; Lista no vacía, se puede usar mapcar sobre l
    (t
        ; Se aplicará lambda(lista) para todo elemento de l
        (mapcar (lambda (lista)
            ; Cuerpo de la función lambda(lista)
            ; Aplicamos la función (spiro) a todos los elementos de la lista 'lista'
            (apply 'spiro lista))
    l)))
)

; -----> 3.5 DIBUIX
(defun dibuix ()
    ; Pinta un juego de pruebas de 12 figuras que se trazan a partir de diferentes combinaciones
    ; de circulos grandes, pequeños, interiores y exteriores usando todas las funciones de
    ; dibujo del spirograph definidas en la practica

    ; Se dibujan 12 figuras en organización matricial, haciendo 3 filas de 4 dibujos cada una
    ; Para ello tendremos que modificar valores, por tanto, haremos una copia por seguridad
    (setq escalaPrevio (get 'spiro 'escala))
    (setq inicioPrevio (get 'spiro 'inici))
    ; Ahora ya podemos hacer asignaciones a x e y sin riesgos
    (setq x (get 'spiro 'x))
    (setq y (get 'spiro 'y))

    ; Limpiamos la pantalla antes de comenzar con las simulaciones
    (cls)

    ; Establecemos una escala e inicio antes de dibujar
    (putprop 'spiro 0.5 'escala)
    (inici 0)
    ; Dibujamos las 12 figuras del juego de pruebas
    ; La posicion inicial del dibujo será la que se da por parámetro
    (dibujarFiguras -476 -240) 

    ; Ya se han hecho los dibujos, ya se puede recuperar la informacion previa sin modificar
    ; Recuperamos el inicio previo al juego de pruebas
    (inici inicioPrevio)
    ; Recuperamos la escala previa al juego de pruebas
    (escala escalaPrevio)
    ; Recuperamos la posicion previa al juego de pruebas
    (posicio x y)
    (negre)
)

(defun dibujarFiguras (x y)

    ; Dado que se sigue una organización matricial en cuanto a la distribución
    ; de los dibujos, se va a hacer estos 12 dibujos por filas

    ; Se dibuja la primera fila
    (primeraFila x y)
    ; Tras la primera fila, preparamos x e y para el dibujo de la siguiente fila
    (setq x -476)
    (setq y (+ 233.3 y))

    ; Se dibuja la segunda fila
    (segundaFila x y)
    ; Se actualizan x e y para el dibujo de la siguiente fila
    (setq x -476)
    (setq y (+ 233.3 y))

    (terceraFila x y)
)

; Funcion auxiliar que dibuja 4 figuras diferentes una al lado de otra todas en la misma fila
(defun primeraFila (x y)
    ; Se dibuja la primera fila del juego de pruebas
    ; La estrategia sera llamar a 4 funciones que hagan 1 dibujo diferente cada uno.
    ; Tras cada llamada a las funciones, se modificara la posicion (componente x, ya que
    ; nos desplazamos en la fila y por tanto es el eje X) de cara a el próximo dibujo

    ; Establecemos posicion inicial de primera figura
    (posicio x y)

    ; Se dibujan las figuras y se modifica su posicion
    ; Figura 1
    (figura1)
    (setq x (+ 317.5 x))
    (posicio x y)

    ; Figura 2
    (figura2)
    (setq x (+ 317.5 x))
    (posicio x y)

    ; Figura 3
    (figura3)
    (setq x (+ 317.5 x))
    (posicio x y)

    ; Figura 4
    (figura4)
)

; Funcion auxiliar que dibuja 4 figuras diferentes una al lado de otra todas en la misma fila
(defun segundaFila (x y)
    ; Se dibuja la segunda fila del juego de pruebas
    ; Misma estrategia que la primera fila

    ; Establecemos posicion inicial de la primera figura
    (posicio x y)

    ; Se dibujan las figuras y se modifica su posicion
    ; Figura 5
    (figura5)
    (setq x (+ 317.5 x))
    (posicio x y)

    ; Figura 6
    (figura6)
    (setq x (+ 317.5 x))
    (posicio x y)

    ; Figura 7
    (figura7)
    (setq x (+ 317.5 x))
    (posicio x y)

    ; Figura 8
    (figura8)
)

; Funcion auxiliar que dibuja 4 figuras diferentes una al lado de otra todas en la misma fila
(defun terceraFila (x y)
    ; Se dibuja la tercera fila del juego de pruebas
    ; Misma estrategia que la primera fila

    ; Establecemos posicion inicial de la primera figura
    (posicio x y)

    ; Se dibujan las figuras y se modifica su posicion
    ; Figura 9
    (figura9)
    (setq x (+ 317.5 x))
    (posicio x y)

    ; Figura 10
    (figura10)
    (setq x (+ 317.5 x))
    (posicio x y)

    ; Figura 11
    (figura11)
    (setq x (+ 317.5 x))
    (posicio x y)

    ; Figura 12
    (figura12)
)

; Función auxiliar que dibuja la figura 1/12 del juego de pruebas
(defun figura1 ()
    (vermell)
    (spiros '((105 80 1 0.4 0)(105 80 3 0.4 0)(105 80 5 0.4 0)))
    (verd)
    (spiros '((105 60 1 0.5 0)(105 60 3 0.5 0)(105 60 5 0.5 0)))
    (blau)
    (spiros '((105 75 1 0.5 0)(105 75 3 0.5 0)(105 75 5 0.5 0)))
)

; Función auxiliar que dibuja la figura 2/12 del juego de pruebas
(defun figura2 ()
    (verd)
    (spirograph 175 96 56 40 0.5 0)
    (negre)
    (spiros '((105 72 1 0.5 0)(105 72 3 0.5 0)))
    (blau)
    (spirograph 125 96 40 40 1.5 0)
)

; Función auxiliar que dibuja la figura 3/12 del juego de pruebas
(defun figura3 () 
    (blau)
    (spiros '((105 42 3 0.5 1)(105 42 5 0.5 1)(105 42 7 0.5 1)))
    (vermell)
    (spiros '((105 42 10 0.5 1)(105 42 13 0.5 1)(105 42 15 0.5 1)))
    (verd)
    (spirograph 32 105 42 8 0.5 1) 
)

; Función auxiliar que dibuja la figura 4/12 del juego de pruebas
(defun figura4 ()
    (negre)
    (spiros '((96 80 1 0.5 0)(96 75 3 0.5 0)(96 72 5 0.5 0)))
    (blau)
    (spiros '((96 80 7 0.5 0)(96 75 9 0.5 0)(96 72 11 0.5 0)))
    (vermell) 
    (putprop 'spiro 96 'rgran)
    (putprop 'spiro 56 'rpetit)
    (inici 0)
    (interior t)
    (punt 10)
    (roda)
)

; Función auxiliar que dibuja la figura 5/12 del juego de pruebas
(defun figura5 ()
    (vermell)
    (spirograph 300 96 80 35 1 1)
    (spirograph 300 96 84 35 1 0)
    (blau)
    (spiros '((96 72 1 0.5 0) (96 72 3 0.5 0) (96 72 5 0.5 0)))
    (verd)
    (putprop 'spiro 150 'radigran)
    (putprop 'spiro 40 'radipetit)
    (inici 0)
    (interior t)
    (punt 6)
    (roda-voltes 50)
)

; Función auxiliar que dibuja la figura 6/12 del juego de pruebas
(defun figura6 ()
    (blau)
    (spiros '((105 40 7 0.2 0)(105 40 11 0.2 0)))
    (vermell)
    (spiros '((105 45 7 1 0)(105 45 9 1 0)(105 45 11 1 0)))
    (verd)
    (roda-voltes 15)
)

; Función auxiliar que dibuja la figura 7/12 del juego de pruebas
(defun figura7 ()
    (blau)
    (spiro 105 42 5 0.5 6)
    (spiro 105 42 5 0.5 3)
    (negre)
    (spiro-voltes 100 105 72 2 0.5 180)
    (verd)
    (spiros '((96 72 1 0.5 0)(96 72 3 0.5 0)(96 72 5 0.5 0)))
)

; Función auxiliar que dibuja la figura 8/12 del juego de pruebas
(defun figura8 ()
    (blau)
    (spiros '((96 63 1 0.5 0)(96 63 3 0.5 0)(96 63 5 0.5 0)))
    (vermell)
    (spiros '((96 72 7 0.5 0)(96 72 9 0.5 0)(96 72 11 0.5 0)))
    (verd) 
    (spiro 96 75 13 0.5 0)
)

; Función auxiliar que dibuja la figura 9/12 del juego de pruebas
(defun figura9 ()
    (vermell)
    (spiros '((105 63 1 0.5 0)(105 63 7 0.5 0)(105 63 3 0.5 0)))
    (verd) 
    (spiros '((105 63 13 0.5 0)(105 63 7 0.5 0)(105 63 15 0.5 0))) 
    (putprop 'spiro 150 'radigran)
    (putprop 'spiro 56 'radipetit)
    (inici 0)
    (interior t)
    (blau)
    (punt 2)
    (roda-voltes 7)
)

; Función auxiliar que dibuja la figura 10/12 del juego de pruebas 
(defun figura10 ()
    (verd)
    (spiros '((105 40 1 0.2 0)(105 40 2 0.2 0)(105 40 3 0.2 0)))
    (blau)
    (spiros '((96 72 15 0.5 0)(96 72 13 0.5 0)(96 72 11 0.5 0)))
    (vermell)
    (spiros '((96 72 7 0.5 0)(96 72 9 0.5 0)(96 72 11 0.5 0)))
)

; Función auxiliar que dibuja la figura 11/12 del juego de pruebas
(defun figura11 ()
    (vermell)
    (spiros '((96 30 1 0.5 0)(96 30 3 0.5 0)(96 30 5 0.5 0)))
    (blau)
    (spiros '((105 60 7 0.5 0)(105 60 8 0.5 0)(105 60 9 0.5 0)))
    (verd)
    (spirograph 250 96 42 25 1 0)
)

; Función auxiliar que dibuja la figura 12/12 del juego de pruebas
(defun figura12 () 
    (blau)
    (spirograph 150 105 75 30 0.5 0)
    (spirograph 150 105 75 30 0.5 2)
    (vermell)
    (spirograph 150 105 75 30 0.5 4)
    (spirograph 150 105 75 30 0.5 6)
    (putprop 'spiro 96 'radigran)
    (putprop 'spiro 60 'radipetit)
    (inici 0)
    (interior t)
    (blau)
    (punt 10)
    (roda-voltes 50)
)


;-----------------------------------------------------------------
;---------------------------< PARTE 4 >---------------------------
;-----------------------------------------------------------------
;
;   FUNCIONES DEL JUEGO DE PRUEBAS:
;   1. prueba-p1
;   2. prueba-p2-spirograph1
;   3. prueba-p2-spirograph2
;   4. prueba-p2-spiro1
;   5. prueba-p2-spiro2
;   6. prueba-p3-roda
;   7. prueba-p3-rodavoltes
;   8. prueba-p3-spirovoltes
;   9. prueba-p3-spiros
;
;-----------------------------------------------------------------
; EL juego de pruebas consiste en una serie de funciones que hacen llamadas a las otras
; funciones del archivo, de forma que, cada una de las funciones que se mostrarán a
; continuación actuán como "script", ejecutando instrucciones y haciendo llamadas a funciones
; de igual manera que si las hicieramos nosotros desde la terminal
;-----------------------------------------------------------------

; Función auxiliar que nos permite comprobar la correcta implementación de la parte 1
(defun prueba-p1()
    ; Haremos llamadas a las funciones implementadas en la parte 1, creando un dibujo con
    ; diferentes círculos de distintos colores
    ; Crearemos dos circulos grandes, de los cuales el más pequeño de ellos tendrá una serie
    ; de 8 circulos pequeños inscritos

    ; Cargamos archivo
    (load 'spiro.lsp)
    ; Limpiamos pantalla
    (cls)
    (negre)

    ; Creamos el mayor de los circulos grandes
    (radigran 105)
    ; Creamos el menor de los circulos grandes para hacer circulos inscritos dentro
    (radigran 96)

    ; Circulo pequeño 1
    (inici 45)
    (vermell)
    (radipetit 32)

    ; Circulo pequeño 2
    (inici 90)
    (blau)
    (radipetit 32)

    ; Circulo pequeño 3
    (inici 135)
    (verd)
    (radipetit 32)

    ; Circulo pequeño 4
    (inici 180)
    (negre)
    (radipetit 32)

    ; Circulo pequeño 5
    (inici 225)
    (vermell)
    (radipetit 32)

    ; Circulo pequeño 6
    (inici 270)
    (blau)
    (radipetit 32)
    
    ; Circulo pequeño 7
    (inici 315)
    (verd)
    (radipetit 32)

    ; Circulo pequeño 8
    (inici 360)
    (negre)
    (radipetit 32)
)

; Función auxiliar que nos permite comprobar la correcta implementación de la función spirograph (1)
(defun prueba-p2-spirograph1 ()
    ; Crearemos un spirograph 
    (load 'spiro.lsp)
    (cls)
    (verd)
    (spirograph 100 105 60 10 0.5 0)
    (negre)
)

; Función auxiliar que nos permite comprobar la correcta implementación de la función spirograph (2)
(defun prueba-p2-spirograph2 ()
    ; Crearemos un spirograph diferente al anterior combinando 2 iguales pero con diferente rotacion
    (load 'spiro.lsp)
    (cls)
    (blau)
    (spirograph 150 105 40 25 0.5 0)
    (vermell)
    (spirograph 150 105 40 25 0.5 10)
    (negre)
)

; Función auxiliar que nos permite comprobar la correcta implementación de la función spiro (1)
(defun prueba-p2-spiro1 ()
    ; Crearemos un spiro
    (load 'spiro.lsp)
    (cls)
    (negre)
    (spiro 105 40 1 0.2 0)
)

; Función auxiliar que nos permite comprobar la correcta implementación de la función spiro (2)
(defun prueba-p2-spiro2 ()
    ; Crearemos un spiro diferente mediante la combinacion de 2
    (load 'spiro.lsp)
    (cls)
    (verd)
    (spiro 105 40 5 0.2 0)
    (blau)
    (spiro 105 40 7 0.4 0)
    (negre)
)

; Función auxiliar que nos permite comprobar la correcta implementación de la función roda
(defun prueba-p3-roda ()
    ; Haremos una llamada a la funcion roda
    (load 'spiro.lsp)
    (escala 1.5)
    (radipetit 72)
    (radigran 105)
    (putprop 'spiro 0.5 'pas)
    (interior t)
    (inici 0)
    (cls)
    ; Llamada a roda
    (verd)
    (roda)
    (negre)
)

; Función auxiliar que nos permite comprobar la correcta implementación de la función rodavoltes
(defun prueba-p3-rodavoltes ()
    ; Haremos una llamada a la funcion rodavoltes
    (load 'spiro.lsp)
    (escala 1.5)
    (radipetit 72)
    (radigran 105)
    (putprop 'spiro 0.5 'pas)
    (interior t)
    (inici 0)
    (cls)
    ; Llamada a roda
    (verd)
    (roda-voltes 10)
    (negre)
)

; Función auxiliar que nos permite comprobar la correcta implementación de la función spirovoltes
(defun prueba-p3-spirovoltes ()
    ; Haremos una llamada a spirovoltes
    (load 'spiro.lsp)
    (cls)
    ; Escala a 1 para cambiar
    (escala 2)
    (interior t)
    (vermell)
    (spiro-voltes 25 105 72 2 0.2 0)
    (negre)
)

; Función auxiliar que nos permite comprobar la correcta implementación de la función spiros
(defun prueba-p3-spiros ()
    ; Comprobaremos esta funcion creando una de las 12 figuras de 'dibuix'
    (load 'spiro.lsp)
    (cls)
    ; La figura 1 consiste en multiples llamadas a 'spiros'
    (figura1)
    (negre)
)

; -----> ADICIONAL (EQUIVALENTE A LAS QUE SALEN EN EL ENUNCIADO)
; Funcion hipo identica a la de la pagina 11 del enunciado para comprobar que funciona igual
(defun hipo ()
    (escala 1.2)
    (radigran 105)
    (radipetit 40)
    (inici 0)
    (interior t)
    (cls)
    (vermell)
    (punt 7)
    (roda)
    (negre)
)

; Funcion epi identica a la de la pagina 11 del enunciado para comprobar que funciona igual
(defun epi()
    (escala 0.8)
    (radigran 150)
    (radipetit 40)
    (inici 0)
    (interior nil)
    (cls)
    (vermell)
    (punt 5)
    (roda)
    (negre)
)