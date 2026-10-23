.SUFFIXES: .F90 .o

OBJS = mpas_nuopc_atm.o \
	mpas_nuopc_field.o \
	mpas_nuopc_fieldlist.o \
	mpas_nuopc_geom.o

all: $(OBJS)

mpas_nuopc_geom.o: mpas_nuopc_macros.h
mpas_nuopc_field.o: mpas_nuopc_macros.h mpas_nuopc_geom.o
mpas_nuopc_fieldlist.o: mpas_nuopc_macros.h mpas_nuopc_field.o mpas_nuopc_geom.o
mpas_nuopc_atm.o: mpas_nuopc_macros.h mpas_nuopc_field.o mpas_nuopc_fieldlist.o mpas_nuopc_geom.o

clean:
	$(RM) *.o *.mod
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

%.o: %.F90
	$(RM) $@ $*.mod
	$(FC) $(CPPFLAGS) $(PHYSICS) $(FFLAGS) -c $< -o $@ -I.. -I../../framework -I../../driver -I../../operators $(CPPINCLUDES) $(FCINCLUDES)
