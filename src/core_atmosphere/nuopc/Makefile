.SUFFIXES: .F90 .o

OBJS = mpas_nuopc_atm.o \
	mpas_nuopc_fields.o \
	mpas_nuopc_utils.o

all: $(OBJS)

mpas_nuopc_fields.o: mpas_nuopc_utils.o
mpas_nuopc_atm.o: mpas_nuopc_fields.o

clean:
	$(RM) *.o *.mod
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

%.o: %.F90
	$(RM) $@ $*.mod
	$(FC) $(CPPFLAGS) $(PHYSICS) $(FFLAGS) -c $< -o $@ -I$(MPAS_MODDIR) -I.. -I../physics -I../physics/physics_noahmp/utility $(CPPINCLUDES) $(FCINCLUDES) $(MPAS_ESMF_INC)
