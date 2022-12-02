FC = gfortran
OPT = -g -O0 -fbounds-check

OBJ = m_type.o verif.o fct_verif.o

verif.exe :	$(OBJ)
	$(FC) $(OPT) $(OBJ) -o verif.exe

m_type.o :	m_type.f90
	$(FC) $(OPT) m_type.f90 -c

verif.o :	verif.f90
	$(FC) $(OPT) verif.f90 -c

fct_verif.o :	fct_verif.f90
	$(FC) $(OPT) fct_verif.f90 -c

clean :
	/bin/rm -f $(OBJ) *.mod *.exe

