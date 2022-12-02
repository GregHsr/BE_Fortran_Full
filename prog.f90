program BE
    use m_type
    implicit none
    type (phys):: data_phys
    type (num):: data_num
    real :: R
    real,dimension(:),allocatable:: L_m, C_init, C_dt, C_t
    integer :: i

    
    call read_data("data_BE.txt",data_phys,data_num)
    R= data_phys%D*(data_phys%tf/data_num%N_t)/((data_phys%L/data_num%N_x)**2)

    allocate(L_m(data_num%N_x))
    call L_maill(data_num%N_x,data_phys%L,L_m)
    
    allocate(C_init(data_num%N_x))
    call C_initiale(C_init,L_m,data_num%N_x,data_phys%xf,data_phys%xd,data_phys%C0)

    allocate(C_t(data_num%N_x))

    C_t=C_init
    open(10,file="resultats.txt")
    write(10,*)C_t

    do i=1,data_num%N_t
        allocate(C_dt(data_num%N_x))
        call concentration(C_t,C_dt,data_num%N_x,R,data_phys%C0)
        write(10,*)C_dt
        C_t=C_dt
        deallocate(C_dt)
    end do  

    deallocate(L_m)
    deallocate(C_init)
    deallocate(C_t)
    close(10)

end program BE
