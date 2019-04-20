       program juegos
       use randomnumber
       implicit none
       !Vamos a simular la teoría evolutiva de juegos. Primero
       !crearemos una matriz "m" nxn que representará el espacio
       !donde nos encontraremos.
       integer, dimension(:,:),allocatable::m !matriz espacial
       integer, dimension(:,:),allocatable::rac !matriz de recompensa
       !acumulada. Acompaña a la matriz m e indica que recompensa lleva
       !cada elemento de m
       integer rec(2,2) !matriz de recompensas
       integer n !tamaño matriz
       integer i,j,t,k !contadores
       integer s !número de iteracciones antes de la selección
       n = 50
       s = 10
       allocate (m(n,n))
       allocate (rac(n,n))
       
       !archivos de texto donde escribir los datos
       open(unit=10,file='matrizinicial.dat',status='unknown')
       open(unit=11,file='matrizfinal.dat',status='unknown')
       
       !La matriz m estará compuesta por 1 y 0. Los 0 corresponden
       !a los cooperadores y los 1 a los desertores.
       !los voy a hacer aleatoriamente como primera prueba.
       call dran_ini(175)
       do i=1,n 
           do j=1,n
               m(i,j)=nint(dran_u())
            end do
       end do
       
      
       !Vamos a usar la matriz de recompensas del dilema del prisionero 
       rec(1,1) = 3
       rec(1,2) = 0
       rec(2,1) = 5
       rec(2,2) = 1
       
       !Escribimos la matriz Inicial
       do i=1,n
           write(10,*) m(i,:)
       end do
       write(10,*)" "
       !El programa se ejecutará una serie de ciclos, que constan de
       !un número de iteraciones s seguidas de una reproducción
       t = 4
       !cada paso montecarlo son n^2 ciclos.
       do j = 1,t
            do k = 1,n**2
               !Inicializamos todas las recomensas acumuladas a 0
               rac(:,:) = 0
               !Iniciamos el juego por un tiempo s
               do i = 1,s*n**2
                   call recompensasMoran()
               end do
               !Una vez realizadas las recompensas realizamos la 
               !reproducción
               call reproduccion()
            end do
      end do
      
      !Escribimos la matriz final
       do i=1,n
           write(11,*) m(i,:)
       end do
       
       
       !  *******************     
       !  *******************    
       !      SUBRUTINAS 
       !  *******************
       !  *******************
       
       contains
       
       subroutine recompensasMoran()
       !En esta subrutina toma dos elementos al azar de la matriz "m"
       !y da las recompensas a los elementos de la matriz "rac" 
       !correspondientes
       integer:: fil1, col1, fil2,col2
       integer:: a,b
       !matriz 1
       fil1 = ceiling(dran_u()*n)
       col1 = ceiling(dran_u()*n)
       
       !matriz 2
       fil2 = ceiling(dran_u()*n)
       col2 = ceiling(dran_u()*n)
       
       if (m(fil1,col1).eq.0) then
           if (m(fil2,col2).eq.0) then
               a = rec(1,1) 
               b = rec(1,1)
           else 
               a = rec(1,2)
               b = rec(2,1)
           end if
       else
           if (m(fil2,col2).eq.0) then
               a = rec(2,1)
               b = rec(1,2)
           else 
               a = rec(2,2)
               b = rec(2,2)
           end if
       end if
       rac(fil1,col1) = rac(fil1,col1) + a
       rac(fil2,col2) = rac(fil2,col2) + b
       return
       end subroutine
       
       subroutine reproduccion()
       !Elegimos aleatoriamente de manera proporcional a su beneficio a 
       ! un sujeto para que se reproduzca
       integer i,j,hijo
       integer a,b
       a = 0
       do i =1,n
           do j = 1,n
               a = rac(i,j) + a
           end do
       end do
       b = ceiling(a*dran_u())
       do i = 1,n
           do j=1,n
               b = b-rac(i,j)
               if (b.le.0) then
                   hijo = m(i,j)
                   go to 10
               end if
           end do
       end do 
10     continue       
       !Ahora elegimos a un sujeto al azar que será reemplazado por el 
       ! nuevo individuo
       i = ceiling(dran_u()*n)
       j = ceiling(dran_u()*n)
       m(i,j) = hijo
       
       return
       end subroutine
       
                   
           
           
       end
        
       
