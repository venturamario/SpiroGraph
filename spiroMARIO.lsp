;-----------------------------------------------------------------
;   -----> PRACTICA 1
;   ASIGNATURA: Lenguajes de Programación
;   AUTORES: Mario Ventura Burgos & Luis Miguel Vargas Durán
;   FECHA: 03-2023
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

;-----> 1. GUARDA-INFORMACIO
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
    (putprop 'spiro '50 'rpetit)

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

    ; Se crea y da valor a la propiedad pas y se añade a 'spiro'
    (putprop 'spiro '0.2 'pas)
)
(guarda-informacio)

;-----> 2.1 VERMELL
(defun vermell () (color 255 0 0))

;-----> 2.2 VERD
(defun verd () (color 0 255 0))

;-----> 2.3 BLAU
(defun blau () (color 0 0 255))

;-----> 2.4 NEGRE
(defun negre () (color 0 0 0))

;-----> 3. CERCLE (X Y RADI N)
(defun cercle (x y radi n)
    ;(igual que la de los apuntes)
    ; Se mueve la posición del lápiz mediante la función auxiliar mover para que
    ; no deje una línea desde la esquina inferior izquierda hasta (x,y)
    (mover (+ x radi) y)    ; lápiz en posición (x,y)
    ; Se dibuja el circulo con radio radi en la coordenadas (x,y)
    ; Dado que un circulo tiene 360 grados, para que los segmentos sean
    ; "simétricos", se obtendrá los segmentos haciendo 360/n
    (dibujarcercle x y radi (/ 360 n) 0)    ; Ángulo de paso será 0 por defecto
)

; Función auxiliar que llama a "move" de Lisp para desplazar la posición del lápiz a (x,y)
; (igual que la de los apuntes)
(defun mover (x y)
    (move (realpart (round (+ 320 (* 1.8 x))))
        (realpart (round (+ 187 (* 1.8 y)))))
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
    (draw (realpart (round (+ 320 (* 1.8 x))))
        (realpart (round (+ 187 (* 1.8 y)))))
)

; Función auxiliar que convierte de grados a radianes
(defun radians (graus)
    (/ (* graus (* 2 pi)) 360)
)

;-----> 4. RADIGRAN
(defun radigran (r)
    ; Función que actúa como setter del radio grande
    ; Cambiamos el valor que tiene el radio grande por defecto y pintamos un círculo
    ; Spiro tendrá r con valor rgran
    (putprop 'spiro r 'rgran)
    ; Pinta el círculo con los valores x e y de spiro, que obtiene mediante un get 
    (cercle (get 'spiro 'x) (get 'spiro 'y) r 60) 
)

;-----> 5. RADIPETIT
(defun radipetit (r)
    ; Función que actúa como setter del radio pequeño
    ; Cambiamos el valor que tiene el radio pequño por defecto y pintamos un círculo
    ; en la posición inicial del círculo pequeño
    ; Spiro tendrá r con valor rgran
    (putprop 'spiro r 'rpetit)
    ; Al igual que con el radio grande, se obtiene la nueva posicion mediante getters del "objeto" spiro
    (posicio (calcularX (get 'spiro 'rgran) (get 'spiro 'rpetit) (radians (get 'spiro 'inici)))
    (calcularY (get 'spiro 'rgran) (get 'spiro 'rpetit) (radians (get 'spiro 'inici))))
    
    ; Se pinta el círculo mediante llamada a (cercle) que llama a (dibujarcercle)
    (cercle (get 'spiro 'x) (get 'spiro 'y) r 60)
)

; Función auxiliar para el cálculo de la X
(defun calcularX (rg rp a)
    (* (- rg rp) (sin a))
)

; Función auxiliar para el cálculo de la Y
(defun calcularY (rg rp a)
    (* (- rg rp) (cos a))
)

;-----> 6.1 INICI 
(defun inici (angle)
    ; Actúa como función setter de 'angle' en 'spiro'
    ; Modifica el valor del angulo angle en spiro
    (putprop 'spiro angle 'inici)
)

;-----> 6.2 PUNT
(defun punt (p)
    ; Actúa como función setter de 'punt' en 'spiro'
    ; Modifica el valor de punt en spiro
    (putprop 'spiro p 'punt)
)

;-----> 6.3 POSICIÓ
(defun posicio (x y)
    ; Actúa como función setter de 'x' e 'y' en 'spiro'
    ; Modifica el valor de la posición (x,y) en spiro
    (putprop 'spiro x'x)
    (putprop 'spiro y'y)
)

;-----> 6.4 ESCALA
(defun escala (e)
    ; Actúa como función setter de 'escala' en 'spiro'
    ; Modifica el valor de escala en spiro
    (putprop 'spiro e 'escala)
)

;-----> 7. REDUIR
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

(defun spirograph (p gran petit t inc inici)
    ; Simula el comportament d’un spirograph amb el número de passes p, amb els radis gran i
    ; petit, amb la distancia t (veure la figura 3), un increment inc a cada passa i amb l’inici del
    ; dibuixat a l’angle donat en graus

    ; Se usan las formulas 9 y 10 dependiendo de si se utiliza el exterior del circulo grande o el pequeño.
    ; Esto se puede deducir con la propiedad 'interior' de 'spiro'
    
    ; ---> interior: valor booleà que indica si esteim pintant per l’interior o no.
    (cond ((or (= gran 96) (= gran 105))
        ; Es interior ---> Usar formula 9
        

    ) ((or (= gran 144) (= gran 150))
        ; Es exterior ---> Usar formula 10


    )
    ; Caso trivial: los parametros recibidos no coinciden con los valores de la tabla 1 del encunciado
    (t (format t "ERROR! ---> VALOR NO VÁLIDO")))
)

(defun spiro (gran petit p inc inici)
    ; 
)