###############################################################################
#                                                                             #
#                       Generic Makefile for Elatools_lapw                        #
#                                                                             #  
#  make              ... generate executable                                  #
#  make clean        ... delete unnecessary files                             #
#                                                                             #
###############################################################################

###############################################################################
#                           User defined variables                            #
###############################################################################

#---------------- compiler -----------------

FC = ifort
#------------- compiler options ------------

FOPT     = -O2 -FR -mp1 -w -prec_div -pc80 -pad -ip -DINTEL_VML -traceback -assume buffered_io  -pthread -I$(MKLROOT)/include
FGEN     = -O2 -FR -mp1 -w -prec_div -pc80 -pad -ip -DINTEL_VML -traceback 
#-------------- loader options -------------

LDFLAGS  = $(FOPT) -L$(MKLROOT)/lib/$(MKL_TARGET_ARCH) -pthread -lmkl_lapack95_lp64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -qopenmp -lpthread


###############################################################################
#                       End of user defined variables                         #
###############################################################################

DESTDIR  = ../bin/
EXECNAME =  Elatools.x
EXECNAME1 = dat2wrl.x
EXECNAME2 = dat2agr.x
EXECNAME3 = dat2gnu.x
EXECNAME4 = dens_lapw

EXEC   = $(DESTDIR)$(EXECNAME)
EXEC1   = $(DESTDIR)$(EXECNAME1)
EXEC2   = $(DESTDIR)$(EXECNAME2)
EXEC3   = $(DESTDIR)$(EXECNAME3)
EXEC4   = $(DESTDIR)$(EXECNAME4)

OBJS =  Eatools_main.o	 Eatools_2Dyoung.o Eatools_2Dpoisson.o  Eatools_2Dshear.o \
        Eatools_2DMaxMin.o Eatools_2Danalyz.o Eatools_2D_proelast.o Eatools_2dPL.o \
        Eatools_3dPLbul.o Eatools_3dPLpoi.o Eatools_3dPLyou.o Eatools_hkl.o Eatools_cijinv.o \
        Eatools_cijinv2D.o  Eatools_poi.o Eatools_so.o Eatools_2dcal.o Eatools_2dPLv2.o Eatools_aniso.o \
        Eatools_3dPLcom.o Eatools_stability.o Eatools_db.o	Eatools_3dPLsh.o Eatools_ang2car.o Eatools_dsyec3.o \
             	Eatools_dutester.o Eatools_proelast.o Eatools_sh.o Eatools_welc.o Eatools_pugh.o Eatools_3dPLpugh.o \
        Eatools_wave_start.o Eatools_wave_vgvf.o Eatools_wave_rot.o Eatools_wave_cal.o Eatools_wave_main.o Eatools_2dcal_wave.o
OBJS1 =	wrl_calylm.o wrl_colorsign.o wrl_end.o wrl_main.o 	\
	       wrl_mesh.o wrl_shape.o wrl_spherappear.o wrl_spher.o wrl_start.o wrl_setcolor.o
OBJS2 = agr_autoset.f90 agr_curvef.f90 agr_main.f90 agr_minmax.f90 	\
       	agr_polar.f90 agr_setplot.f90 agr_start.f90 agr_winend.f90 agr_win.f90 agr_fzero.o 	
OBJS3 = gnu_main.o gnu_subs.o gnu_3dmap.o
OBJS4 = rho_main.o rho_mass.o

$(EXEC):	$(OBJS) $(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4)  Makefile
	$(FC) -o $(EXEC) $(OBJS) $(FOPT) $(LDFLAGS) 
	@echo	Eatools_lapw Created
	$(FC) -o $(EXEC1) $(OBJS1) $(FOPT) $(LDFLAGS) 
	@echo	dat2wrl_lapw Created
	$(FC) -o $(EXEC2) $(OBJS2) $(FOPT) $(LDFLAGS) 
	@echo	dat2agr_lapw Created
	$(FC) -o $(EXEC3) $(OBJS3) $(FOPT) $(LDFLAGS) 
	@echo	dat2gnu_lapw Created
	$(FC) -o $(EXEC4) $(OBJS4) $(FOPT) $(LDFLAGS) 
	@echo	dens_lapw Created

.SUFFIXES: .o .f90

.f90.o :
	$(FC) -O1 -c $(FGEN) $*.f90

clean :
	rm -f *.o *.mod *~ Elatools.x
