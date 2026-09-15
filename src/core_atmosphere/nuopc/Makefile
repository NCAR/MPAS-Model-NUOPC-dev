.SUFFIXES: .F90 .o

OBJS = mpas_nuopc_atm.o

all: $(OBJS)

clean:
	$(RM) *.o *.mod
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

%.o: %.F90
	$(RM) $@ $*.mod
	$(FC) $(CPPFLAGS) $(PHYSICS) $(FFLAGS) -c $< -o $@ -I.. -I../../framework -I../../driver -I../../operators $(CPPINCLUDES) $(FCINCLUDES) $(MPAS_ESMF_INC)
