program tarefa_b2
    call calc_pos(100) ! Generates planets position using jupter mass x100

    call calc_pos(1000) ! Generates planets position using jupter mass x1000

contains
    subroutine calc_pos(jupter_mass_mult)
        implicit real(8) (a-h,o-z)
        character(70) filename
        parameter (pi=4d0*atan(1d0))
        dimension vel_earth(2), pos_i_earth(2), d2rdt2_earth(2), pos_earth(2), raux_earth(2)
        dimension vel_jupter(2), pos_i_jupter(2), d2rdt2_jupter(2), pos_jupter(2), raux_jupter(2)

        T_earth = 1d0
        sun_mass = 2d30
        GMs = (2*pi)**2
        
        e = 1d-4 ! delta t
        tmax = 6d0

        earth_mass = 6d24
        radius_earth = 1d0 ! OBS: radius_planet = radius_planet_absolute/radius_earth
        exce_earth = 1.7d-2

        pjupter_mass = 1.9d27*jupter_mass_mult
        radius_jupter = 5.2d0 ! OBS: radius_planet = radius_planet_absolute/radius_earth
        exce_jupter = 4.8d-2

        write(filename, '(A,I0,A)') 'saida-b2-terra-', jupter_mass_mult, '-11212400'
        open(10, file=filename)

        write(filename, '(A,I0,A)') 'saida-b2-jupter-', jupter_mass_mult, '-11212400'
        open(11, file=filename)
        
        ! Calculo da velocidade Inicial terra:
        v0_earth = sqrt(GMs)*sqrt((1-exce_earth)/(radius_earth*(1+exce_earth)) * (1 +earth_mass/sun_mass))
        write(*,'(A,F0.8)') 'Earth Initial Velocity: ', v0_earth 
        
        vel_earth = (/0d0, v0_earth /)
        
        pos_i_earth = (/ radius_earth, 0d0 /)
        pos_earth = pos_i_earth + vel_earth*e
        
        write(10, *) pos_earth

        ! Calculo da velocidade Inicial jupter:
        v0_jupter = sqrt(GMs)*sqrt((1-exce_jupter)/(radius_jupter*(1+exce_jupter)) * (1 +pjupter_mass/sun_mass))
        write(*,'(A,F0.8)') 'Jupter Initial Velocity: ', v0_jupter 
        
        vel_jupter = (/0d0, v0_jupter /)
        
        pos_i_jupter = (/ radius_jupter, 0d0 /)
        pos_jupter = pos_i_jupter + vel_jupter*e
        
        write(11, *) pos_jupter


        ! Metodo de Verlet
        t = 0d0
        do while (t < tmax)
            d2rdt2_earth = -GMs*pos_earth/dnorm(pos_earth)**3 
            d2rdt2_earth = d2rdt2_earth -(GMs/sun_mass)*pjupter_mass*(pos_earth - pos_jupter)/dnorm(pos_earth - pos_jupter)**3 

            raux_earth = pos_earth
            pos_earth = 2*pos_earth - pos_i_earth + d2rdt2_earth*e**2
            pos_i_earth = raux_earth

            write(10, *) pos_earth

            t = t + e
        end do

        close(10)

        t = 0d0
        do while (t < tmax)
            d2rdt2_jupter = -GMs*pos_jupter/dnorm(pos_jupter)**3 
            d2rdt2_jupter = d2rdt2_jupter -(GMs/sun_mass)*earth_mass*(pos_jupter - pos_earth)/dnorm(pos_jupter - pos_earth)**3

            raux_jupter = pos_jupter
            pos_jupter = 2*pos_jupter - pos_i_jupter + d2rdt2_jupter*e**2
            pos_i_jupter = raux_jupter

            write(11, *) pos_jupter

            t = t + e
        end do

        close(11)

    end subroutine

    ! Calculo da distancia normal
    real(8) function dnorm(vector)
        implicit real(8) (a-h,o-z)
        dimension vector(2)

        dnorm = sqrt(vector(1)**2 + vector(2)**2)

    end function dnorm


end program tarefa_b2