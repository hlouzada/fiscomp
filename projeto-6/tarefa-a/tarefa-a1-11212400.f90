program tarefa_a1
    implicit real(8) (a-h,o-z)
    character(70) filename
    character(70) name_planet
    parameter (pi=4d0*atan(1d0))
    dimension vel_planet(2), pos_i_planet(2), d2rdt2_planet(2)
    dimension pos_planet(2), raux_planet(2)
    
    T_earth = 1d0
    sun_mass = 2d30
    
    e = 1d-4 !delta t

    open(20, file='entrada-a-11212400')

    do j = 1, 9

        read(20,*) name_planet, planet_mass, radius_planet, exce_planet ! OBS: radius_planet = radius_planet_absolute/radius_earth

        write(filename, '(A,A,A)') 'saida-a1-', trim(name_planet), '-11212400'
        write(*,'(A,A,A)') "Calculations for planet ", trim(name_planet), ":"

        T_planet = T_earth*sqrt(radius_planet**3d0)
        GMs = ((2*pi)**2)*(radius_planet**3)/(T_planet**2)

        ! Calculo da velocidade considerando orbita circular (vel = cte)
        v0_planet = 2*pi*radius_planet/T_planet
        write(*,'(A,F0.8)')  'Circular Velocity: ', v0_planet
        
        ! Calculo da velocidade Inicial:
        v0_planet = sqrt(GMs)*sqrt((1/radius_planet) * (1 + planet_mass/sun_mass))
        write(*,'(A,F0.8)') 'Initial Velocity: ', v0_planet 
        
        vel_planet = (/0d0, v0_planet /)
        
        write(*,'(A,F0.8)') "Kepler's Constant:", (T_planet**2)/(radius_planet**3)

        pos_i_planet = (/ radius_planet, 0d0 /)
        pos_planet = pos_i_planet + vel_planet*e
        
        open(10, file=filename)

        write(10, *) pos_planet

        ! Metodo de Verlet
        do i = 1, int(T_planet/e)+1
            d2rdt2_planet = -GMs*pos_planet/dnorm(pos_planet)**3

            raux_planet = pos_planet
            pos_planet = 2*pos_planet - pos_i_planet + d2rdt2_planet*e**2
            pos_i_planet = raux_planet

            write(10, *) pos_planet

        end do

        close(10)

    end do

    close(20)

contains
    ! Calculo da distancia normal
    real(8) function dnorm(vector)
        implicit real(8) (a-h,o-z)
        dimension vector(2)

        dnorm = sqrt(vector(1)**2 + vector(2)**2)

    end function dnorm


end program tarefa_a1