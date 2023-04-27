# Choix du compilateur
FC=ifort

# Lahey and G95 both use the same #define
#FLAGS=-DG95=1 -g
FLAGS=-mp -double_size 64 -fltconsistency
#FLAGS=/Op /double_size:128 /fltconsistency

#definition de l'executable
EXEC = mcgravi

all : comp link

comp : Port_routines.o util_str.o sys_utils.o str_const.o raw_data.o param.o lect_param.o lect_rawdata.o mc_data.o write_matr.o rempli_matMC.o mc_inversion.o mc_simulation.o ecriture.o gmt.o write_Html.o mc_statistic.o mcgravi.o mc_gravi.o
	
link : 
	$(FC) -o $(EXEC) mc_gravi.o Port_routines.o util_str.o sys_utils.o str_const.o raw_data.o param.o lect_param.o lect_rawdata.o mc_data.o write_matr.o rempli_matMC.o mc_inversion.o mc_simulation.o ecriture.o gmt.o write_Html.o mc_statistic.o mcgravi.o 

Port_routines.o : Port_routines.f90
	$(FC)  $(FLAGS) -c   Port_routines.f90

util_str.o : util_str.f90
	$(FC) $(FLAGS) -c util_str.f90
	
sys_utils.o : sys_utils.f90
	$(FC) $(FLAGS) -c sys_utils.f90
	
str_const.o : str_const.f90
	$(FC) -c $(FLAGS) str_const.f90
	
raw_data.o : raw_data.f90
	$(FC) -c $(FLAGS) raw_data.f90
	
param.o : param.f90
	$(FC) -c $(FLAGS) param.f90
	
lect_param.o : lect_param.f90
	$(FC) -c $(FLAGS) lect_param.f90
	
lect_rawdata.o : lect_rawdata.f90
	$(FC) -c $(FLAGS) lect_rawdata.f90
	
mc_data.o : mc_data.f90
	$(FC) -c $(FLAGS) mc_data.f90
	
write_matr.o : write_matr.f90
	$(FC) -c $(FLAGS) write_matr.f90
	
rempli_matMC.o : rempli_matMC.f90
	$(FC) -c $(FLAGS) rempli_matMC.f90
	
mc_inversion.o : mc_inversion.f90
	$(FC) -c $(FLAGS) mc_inversion.f90
	
mc_simulation.o : mc_simulation.f90
	$(FC) -c $(FLAGS) mc_simulation.f90
	
ecriture.o : ecriture.f90
	$(FC) -c $(FLAGS) ecriture.f90
	
gmt.o : gmt.f90
	$(FC) -c $(FLAGS) gmt.f90
	
write_Html.o : write_Html.f90
	$(FC) -c $(FLAGS) write_Html.f90
	
mc_statistic.o : mc_statistic.f90
	$(FC) -c $(FLAGS) mc_statistic.f90
	
mcgravi.o : mcgravi.f90
	$(FC) -c $(FLAGS) mcgravi.f90
	
mc_gravi.o : mc_gravi.f90
	$(FC) -c $(FLAGS) mc_gravi.f90
		
	
clean :
	rm -f  *.o 
	rm -f *.mod
	rm -f  *.obj
	rm -f *.*~
