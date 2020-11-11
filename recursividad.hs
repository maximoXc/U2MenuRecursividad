main = do
    pregunta True


--Inicio de switch
pregunta seguir = do
    if seguir
        then do
           putStrLn "1.- Serie Fibonacci"
           putStrLn "2.- Presentar números del 1 al 10 (1 2 3 4 5 6 7 8 9 10)"
           putStrLn "3.- Factorial"
           putStrLn "4.- Desaparece números"
           putStrLn "5.- Palíndromos"
           putStrLn "6.- Menú de calculadora"
           putStrLn "7.- Salir"
           opcion <- getLine

           case opcion of
               "1" -> fibonacci
               "2" -> presentacion 1
               "3" -> factorial
               "4" -> desapareceNumeros [0,1,2,3,4,5,6,7,8,9,10]
               "5" -> palindromo
               "6" -> calculadora
               "7" -> pregunta False
    else
        putStr "Adios"

--Inicio de funciones

-- Serie Fibonacci (preguntar posición)
fibonacci = do
    putStrLn("¿Qué posición deseas conocer?")
    p <- getLine
    let pInt = read p::Int
    sucesion pInt

sucesion pInt = do
    let numeros = ["0","1","1","2","3","5","8","13","21","34","55","89","144","233","377","610","987","1597"]

    if pInt <= 17
        then
            print (numeros !! pInt)
    else

        pregunta True
    pregunta True


-- Presentar números del 1 al 10 (1 2 3 4 5 6 7 8 9 10)
presentacion n = do
    if n <= n
        then do
            print n
            presentacion(n-1)
    else do
        putStrLn("Terminar")
    pregunta True

-- Factorial (imprime únicamente el resultado final)
factorial = do
    print("Que factorial necesitas?")
    n <- getLine
    print ((product [1..(read n)]))
    pregunta True


-- Desaparecer números de un arreglo de 10 Números por la ultima posición
desapareceNumeros n = do
    if null n
        then do
            putStrLn("\n SE CUMPLIÓ LA CONDICIÓN CRACK \n")
            pregunta True
    else do
        print(n)
        let nl = init n
        desapareceNumeros(nl)

-- Palíndromos (pueden ingresar la cadena o pedir la palabra)
palindromo = do
    print("Si el resultado es True nuestra palabra si es palindromo y si el resultado es false pues no lo es xD")
    print("Ingresa un palindromo")
    pa <- getLine
    print ((pa == reverse pa))    


    
--Menú de calculadora

calculadora = do
    putStr "calculadora"
    pregunta2 True

pregunta2 seguir = do
    if seguir
        then do
        putStrLn "1.- Suma "
        putStrLn "2.- Resta "
        putStrLn "3.- Multiplicación "
        putStr "4.- División "
        putStr "5.- Salir"
        opcion <- getLine
        case opcion of
            "1" -> suma
            "2" -> resta
            "3" -> multi
            "4" -> division
            "5" -> pregunta True
    
    else
        putStr "Adios"

suma = do
    putStrLn ("numero 1")
    a <- getLine
    putStrLn ("numero 2")
    b <- getLine

    let aInt = read a::Int
    let bInt = read b::Int
    let resultado = aInt + bInt

    putStrLn ("El resultado es: "++ show resultado)
    pregunta2 True

resta = do
    putStrLn ("numero 1")
    a <- getLine
    putStrLn ("numero 2")
    b <- getLine

    let aInt = read a::Int 
    let bInt = read b::Int
    let resultado = aInt - bInt

    putStrLn ("El resultado es: "++ show resultado)
    pregunta2 True

multi = do
    putStrLn ("numero 1")
    a <- getLine
    putStrLn ("numero 2")
    b <- getLine

    let aInt = read a::Int 
    let bInt = read b::Int
    let resultado = aInt * bInt

    putStrLn ("El resultado es: "++ show resultado)
    pregunta2 True

division = do
    putStrLn ("numero 1")
    a <- getLine
    putStrLn ("numero 2")
    b <- getLine

    let aInt = read a::Int 
    let bInt = read b::Int
    let resultado = div aInt bInt

    putStrLn ("El resultado es: "++ show resultado)
    pregunta2 True

