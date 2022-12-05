program verif
    use m_type
    implicit none
    type (num):: data_num
    type (phys):: data_phys
    real,dimension(:,:),allocatable:: data
    real,dimension(:),allocatable:: Con_tfix,L_m
    integer:: i,j,k,l,t_test
    real:: t
    t_test=1
    call read_data("/home/gregoire/Documents/PROG_IMP/BE_full/data_BE.txt",data_phys,data_num)
    allocate(data(data_num%N_t+1,data_num%N_x))
    call read_file("/home/gregoire/Documents/PROG_IMP/BE_full/resultats.txt",data,data_num%N_x,data_num%N_t)
    
    open(20,file="lecture.txt")
    do i=1,data_num%N_t+1
        write(20,*) (data(i,j), j=1,data_num%N_x)
    end do
    close(20)

    t=t_test*(data_phys%tf/data_num%N_t)

    allocate(L_m(data_num%N_x))
    call L_maill(data_num%N_x,data_phys%L,L_m)
    allocate(Con_tfix(data_num%N_x))
    call Conc(Con_tfix,L_m,data_num%N_x,data_phys%C0,data_phys%D,t)
    
    open(21,file="res.csv")
    do k=1,data_num%N_x
        write(21,*) L_m(k), data(t_test,k)
    end do
    do l=1,data_num%N_x
        write(21,*) L_m(l), Con_tfix(l)
    end do
    close(21)
    deallocate(L_m)
    deallocate(data)
    deallocate(Con_tfix)
end program verif