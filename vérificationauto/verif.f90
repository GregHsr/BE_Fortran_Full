program verif
    use m_type
    implicit none
    type (num):: data_num
    type (phys):: data_phys
    real,dimension(:,:),allocatable:: data
    real,dimension(:),allocatable:: Con_tfix,L_m
    integer:: i,j,k,l,dt
    real:: t
    
    call read_data("/home/gregoire/Documents/PROG_IMP/BE_full/data_BE.txt",data_phys,data_num)
    allocate(data(data_num%N_t+1,data_num%N_x))
    call read_file("/home/gregoire/Documents/PROG_IMP/BE_full/resultats.txt",data,data_num%N_x,data_num%N_t)
    
    open(20,file="lecture.txt")
    do i=1,data_num%N_t+1
        write(20,*) (data(i,j), j=1,data_num%N_x)
    end do
    close(20)
    open(21,file="res.csv")
    do dt=1,data_num%N_t
        t=dt*(data_phys%tf/data_num%N_t)

        allocate(L_m(data_num%N_x))
        call L_maill(data_num%N_x,data_phys%L,L_m)
        allocate(Con_tfix(data_num%N_x))
    
        call Conc(Con_tfix,L_m,data_num%N_x,data_phys%C0,data_phys%D,t)
        
        do k=1,data_num%N_x
            write(21,*) L_m(k), data(dt,k)
        end do
        do l=1,data_num%N_x
            write(21,*) L_m(l), Con_tfix(l)
        end do
        deallocate(L_m)
        deallocate(Con_tfix)
    end do
    deallocate(data)
    close(21)
end program verif