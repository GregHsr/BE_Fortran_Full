module m_type
    implicit none
    
    type phys
        real :: L, D, C0, tf
        integer :: xd, xf
    end type phys

    type num
        integer :: N_x, N_t
    end type num

end module m_type