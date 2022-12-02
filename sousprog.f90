! Lecture de données depuis le fichier 

subroutine read_data (filename, data_phys, data_num)
    use m_type

    implicit none
    
    character(len=*), intent(in) :: filename

    type (phys), intent(out) :: data_phys
    type (num), intent(out) :: data_num

    open (unit=10, file=filename)
    read (10, *)
    read (10, *)

    read (10, *) data_phys%L
    read (10, *) data_phys%D
    read (10, *) data_phys%C0
    read (10, *) data_phys%tf
    read (10, *) data_phys%xd
    read (10, *) data_phys%xf
    
    read (10, *) 
    read (10, *)
    read (10, *) 

    read (10, *) data_num%N_x
    read (10, *) data_num%N_t

    close (10)

end subroutine read_data

! Fonction de Heavyside 

function heavyside(x)

    implicit none
    real :: x
    integer :: heavyside

    if (x < 0) then
        heavyside = 0
    else
        heavyside = 1
    end if

end function heavyside

! Fonction maillage

subroutine L_maill(N,L,L_m)
    Implicit None

    Real,intent(in)::L
    Integer,intent(in)::N
    Integer::i
    Real,dimension(N),intent(out)::L_m

    do i=1,N
       L_m(i)=L*(i-1)/(N-1)
    end do
    
end subroutine L_maill

! Calcul de la concentration initiale

subroutine C_initiale(C_init,L_maill,N_x,xf,xd,C0)
    Implicit None

    Real,dimension(N_x),intent(in)::L_maill
    Real,dimension(N_x),intent(out)::C_init
    Integer,intent(in)::N_x,xf,xd
    Real,intent(in)::C0
    Integer::i, heavyside
    Real::L_x

    do i=1,N_x
       L_x=L_maill(i)
       C_init(i)=C0*(heavyside(L_x-xd)-heavyside(L_x-xf))
    end do

end subroutine C_initiale

! Calcul de la conncentration en fonction du temps et de la position

subroutine Concentration(C_t,C_dt,N_x,R,C0)
    implicit None
    real,intent(in)::R,C0
    integer,intent(in)::N_x
    real,dimension(N_x),intent(in)::C_t
    real,dimension(N_x),intent(out)::C_dt
    integer::i

    C_dt(1)=C0
    C_dt(N_x)=0
    do i=2,N_x-1
        C_dt(i)=R*C_t(i-1)+(1-2*R)*C_t(i)+R*C_t(i+1)
    end do

end subroutine Concentration

