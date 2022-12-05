subroutine read_file(filename, data, N_x, N_t)
    implicit none
    character(len=*), intent(in) :: filename
    real, dimension(N_t+1,N_x), intent(out) :: data
    integer, intent(in) :: N_x, N_t
    integer :: i, j
    open(11, file=filename)
    do i = 1, N_t+1
        read(11,*)(data(i,j),j=1, N_x)
    end do
    close(11)
end subroutine read_file

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

! Reproduction du maillage
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

! Calcul de la concentration à partir de la formule
subroutine Conc(C,L_m,N_x,C0,D,t)
    implicit None
    real,intent(in)::C0,D,t
    integer,intent(in)::N_x
    real,dimension(N_x),intent(in)::L_m
    real,dimension(N_x),intent(out)::C
    integer::i

    do i=1,N_x
        C(i)=C0*(1-erf(L_m(i)/(2*sqrt(D*t))))
    end do

end subroutine Conc