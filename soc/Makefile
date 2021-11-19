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

#FC = ifort
 FC = gfortran
#------------- compiler options ------------

#FOPT     = -O1 -FR -mp1 -w -prec_div -pc80 -pad -ip -DINTEL_VML -traceback -assume buffered_io -I$(MKLROOT)/include   # for ifort 
#FGEN = -O1 -FR -mp1 -w -prec_div -pc80 -pad -ip -DINTEL_VML -traceback                                                  # for ifort 

   FOPT = -ffree-form -O2 -ffree-line-length-none     # for gfortran
   FGEN = -ffree-form -O2 -ffree-line-length-none     # for gfortran

#-------------- loader options -------------

   LDFLAGS  = $(FOPT)  -lblas -llapack -lpthread           # for gfortran
#LDFLAGS  = $(FOPT) -L$(MKLROOT)/lib/$(MKL_TARGET_ARCH) -pthread -lmkl_lapack95_lp64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -qopenmp -lpthread  # for ifort 
 
 


###############################################################################
#                       End of user defined variables                         #
###############################################################################

DESTDIR  = ../bin/
EXECNAME =  Elatools.x
EXECNAME1 = dat2wrl.x
EXECNAME2 = dat2agr.x
EXECNAME3 = dat2gnu.x
EXECNAME4 = dens.x
EXECNAME5 = dat2html.x

EXEC    = $(DESTDIR)$(EXECNAME)
EXEC1   = $(DESTDIR)$(EXECNAME1)
EXEC2   = $(DESTDIR)$(EXECNAME2)
EXEC3   = $(DESTDIR)$(EXECNAME3)
EXEC4   = $(DESTDIR)$(EXECNAME4)
EXEC5   = $(DESTDIR)$(EXECNAME5)

OBJS =  Eatools_main.o	 Eatools_2Dyoung.o Eatools_2Dpoisson.o  Eatools_2Dshear.o \
        Eatools_2DMaxMin.o Eatools_2Danalyz.o Eatools_2D_proelast.o Eatools_2dPL.o \
        Eatools_3dPLbul.o Eatools_3dPLpoi.o Eatools_3dPLyou.o Eatools_hkl.o Eatools_cijinv.o \
        Eatools_cijinv2D.o  Eatools_poi.o Eatools_so.o Eatools_2dcal.o Eatools_2dPLv2.o Eatools_aniso.o \
        Eatools_3dPLcom.o Eatools_stability.o Eatools_db.o	Eatools_3dPLsh.o Eatools_ang2car.o Eatools_dsyec3.o \
        Eatools_dutester.o Eatools_proelast.o Eatools_sh.o Eatools_welc.o Eatools_pugh.o Eatools_3dPLpugh.o  Eatools_hardness.o \
        Eatools_km.o Eatools_wave_start.o Eatools_wave_vgvf.o Eatools_wave_rot.o Eatools_wave_cal.o Eatools_wave_main.o Eatools_2dcal_wave.o \
        Eatools_pro_wave.o Eatools_2Dadv.o Eatools_api.o Eatools_3dslic.o
OBJS1 =	wrl_calylm.o wrl_colorsign.o wrl_end.o wrl_main.o \
	       wrl_mesh.o wrl_shape.o wrl_spherappear.o wrl_spher.o wrl_start.o wrl_setcolor.o
	       
OBJS2 = agr_autoset.f90 agr_curvef.f90 agr_main.f90 agr_minmax.f90 agr_automake.o	\
       	agr_polar.f90 agr_setplot.f90 agr_start.f90 agr_winend.f90 agr_win.f90 agr_fzero.o 	
       	
OBJS3 = gnu_main.o gnu_subs.o gnu_3dmap.o gnu_copyri.o
OBJS4 = rho_main.o rho_mass.o 
OBJS5 = html_colorscale.o html_gerdata.o html_scord.o html_strace.o html_elayout.o html_main.o html_setpare.o html_swin.o \
        html_etrace.o html_mincord.o html_stitle.o html_gerdata_velop.o  html_gerdata_velog.o html_gerdata_polar.o html_minpolar.o \
        html_spolar.o html_colorsettings.o


all:	$(OBJS) $(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4) $(OBJS5)  Makefile
	$(FC) -o $(EXEC) $(OBJS) $(FOPT) $(LDFLAGS) 
	@echo	====Eatools.x Created====
	$(FC) -o $(EXEC1) $(OBJS1) $(FOPT) $(LDFLAGS) 
	@echo	====dat2wrl.x Created====
	$(FC) -o $(EXEC2) $(OBJS2) $(FOPT) $(LDFLAGS) 
	@echo	====dat2agr.x Created====
	$(FC) -o $(EXEC3) $(OBJS3)  
	@echo	====dat2gnu.x Created====
	$(FC) -o $(EXEC4) $(OBJS4) $(FOPT) $(LDFLAGS) 
	@echo	====dens.x Created====
	$(FC) -o $(EXEC5) $(OBJS5) $(FOPT) $(LDFLAGS) 
	@echo	====dat2html.x Created====
 
elatools:	$(OBJS)  Makefile
	$(FC) -o $(EXEC) $(OBJS) $(FOPT) $(LDFLAGS) 
	@echo	====Eatools.x Created====	
dat2wrl:	$(OBJS1)  Makefile
	$(FC) -o $(EXEC1) $(OBJS1) $(FOPT) $(LDFLAGS) 
	@echo	====dat2wrl.x Created====
dat2agr:	$(OBJS2)  Makefile
	$(FC) -o $(EXEC2) $(OBJS2) $(FOPT) $(LDFLAGS) 
	@echo	====dat2wrl.x Created====
dat2gnu:	$(OBJS3)  Makefile
	$(FC) -o $(EXEC3) $(OBJS3)  
	@echo	====dat2wrl.x Created====
dens:	$(OBJS4)  Makefile	
	$(FC) -o $(EXEC4) $(OBJS4) $(FOPT) $(LDFLAGS) 
	@echo	====dens.x Created====	
dat2html:	$(OBJS5)  Makefile
	$(FC) -o $(EXEC5) $(OBJS5) $(FOPT) $(LDFLAGS) 
	@echo	====dat2html.x Created====
		
.SUFFIXES: .o .f90

.f90.o :
	$(FC) -O1 -c $(FGEN) $*.f90

clean:
	rm -f *.o *.mod *~ Elatools.x dat2html.x dat2wrl.x dat2agr.x dat2gnu.x dens.x
