
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; calculatet ATOMIC MASS main version. 

	 SUBROUTINE find_mass(name,ma,ma2)
      CHARACTER(len=2) :: name
      CHARACTER(len=2) :: nam
	  REAL(8)          :: ma
	  
!!!!!!!!!!!!!!! The unit of ATOMIC MASS (ma) is gr/mol
!!!!!!!!!!!!!!! 1 mol = 6.022045 *10^23 atoms

         nam = name(1:2)
		if(nam .eq. 'H ') ma = 1.0079d0
		if(nam .eq. 'He') ma = 4.0026d0
		if(nam .eq. 'Li') ma = 6.941d0
		if(nam .eq. 'Be') ma = 9.0122d0
		if(nam .eq. 'B ') ma = 10.811d0
		if(nam .eq. 'C ') ma = 12.0107d0
		if(nam .eq. 'N ') ma = 14.0067d0
		if(nam .eq. 'O ') ma = 15.9994d0
		if(nam .eq. 'F ') ma = 18.9984d0
		if(nam .eq. 'Ne') ma = 20.1797d0
		if(nam .eq. 'Na') ma = 22.9897d0
		if(nam .eq. 'Mg') ma = 24.305d0
		if(nam .eq. 'Al') ma = 26.9815d0
		if(nam .eq. 'Si') ma = 28.0855d0
		if(nam .eq. 'P ') ma = 30.9738d0
		if(nam .eq. 'S ') ma = 32.065d0
		if(nam .eq. 'Cl') ma = 35.453d0
		if(nam .eq. 'K ') ma = 39.0983d0
		if(nam .eq. 'Ar') ma = 39.948d0
		if(nam .eq. 'Ca') ma = 40.078d0
		if(nam .eq. 'Sc') ma = 44.9559d0
		if(nam .eq. 'Ti') ma = 47.867d0
		if(nam .eq. 'V ') ma = 50.9415d0  
		if(nam .eq. 'Cr') ma = 51.9961d0
		if(nam .eq. 'Mn') ma = 54.938d0
		if(nam .eq. 'Fe') ma = 55.845d0
		if(nam .eq. 'Ni') ma = 58.6934d0
		if(nam .eq. 'Co') ma = 58.9332d0
		if(nam .eq. 'Cu') ma = 63.546d0
		if(nam .eq. 'Zn') ma = 65.39d0
		if(nam .eq. 'Ga') ma = 69.723d0
		if(nam .eq. 'Ge') ma = 72.64d0
		if(nam .eq. 'As') ma = 74.9216d0
		if(nam .eq. 'Se') ma = 78.96d0
		if(nam .eq. 'Br') ma = 79.904d0
		if(nam .eq. 'Kr') ma = 83.8d0
		if(nam .eq. 'Rb') ma = 85.4678d0
		if(nam .eq. 'Sr') ma = 87.62d0
		if(nam .eq. 'Y ') ma = 88.9059d0
		if(nam .eq. 'Zr') ma = 91.224d0
		if(nam .eq. 'Nb') ma = 92.9064d0
		if(nam .eq. 'Mo') ma = 95.94d0
		if(nam .eq. 'Tc') ma = 98.0d0
		if(nam .eq. 'Ru') ma = 101.07d0
		if(nam .eq. 'Rh') ma = 102.9055d0
		if(nam .eq. 'Pd') ma = 106.42d0
		if(nam .eq. 'Ag') ma = 107.8682d0
		if(nam .eq. 'Cd') ma = 112.411d0
		if(nam .eq. 'In') ma = 114.818d0
		if(nam .eq. 'Sn') ma = 118.71d0 
		if(nam .eq. 'Sb') ma = 121.76d0
		if(nam .eq. 'I ') ma = 126.9045d0
		if(nam .eq. 'Te') ma = 127.6d0
		if(nam .eq. 'Xe') ma = 131.293d0
		if(nam .eq. 'Cs') ma = 132.9055d0
		if(nam .eq. 'Ba') ma = 137.327d0
		if(nam .eq. 'La') ma = 138.9055d0
		if(nam .eq. 'Ce') ma = 140.116d0
		if(nam .eq. 'Pr') ma = 140.9077d0
		if(nam .eq. 'Nd') ma = 144.24d0
		if(nam .eq. 'Pm') ma = 145.d0
		if(nam .eq. 'Sm') ma = 150.36d0
		if(nam .eq. 'Eu') ma = 151.964d0
		if(nam .eq. 'Gd') ma = 157.25d0
		if(nam .eq. 'Tb') ma = 158.9253d0
		if(nam .eq. 'Dy') ma = 162.5d0
		if(nam .eq. 'Ho') ma = 164.9303d0
		if(nam .eq. 'Er') ma = 167.259d0
		if(nam .eq. 'Tm') ma = 168.9342d0
		if(nam .eq. 'Yb') ma = 173.04d0
		if(nam .eq. 'Lu') ma = 174.967d0
		if(nam .eq. 'Hf') ma = 178.49d0
		if(nam .eq. 'Ta') ma = 180.9479d0
		if(nam .eq. 'W ') ma = 183.84d0
		if(nam .eq. 'Re') ma = 186.207d0
		if(nam .eq. 'Os') ma = 190.23d0
		if(nam .eq. 'Ir') ma = 192.217d0
		if(nam .eq. 'Pt') ma = 195.078d0
		if(nam .eq. 'Au') ma = 196.9665d0
		if(nam .eq. 'Hg') ma = 200.59d0
		if(nam .eq. 'Tl') ma = 204.3833d0
		if(nam .eq. 'Pb') ma = 207.2d0
		if(nam .eq. 'Bi') ma = 208.9804d0
		if(nam .eq. 'Po') ma = 209.0d0
		if(nam .eq. 'At') ma = 210.0d0
		if(nam .eq. 'Rn') ma = 222.0d0
		if(nam .eq. 'Fr') ma = 223.0d0
		if(nam .eq. 'Ra') ma = 226.0d0
		if(nam .eq. 'Ac') ma = 227.0d0
		if(nam .eq. 'Pa') ma = 231.0359d0
		if(nam .eq. 'Th') ma = 232.0381d0
		if(nam .eq. 'Np') ma = 237.0d0
		if(nam .eq. 'U ') ma = 238.0289d0
		if(nam .eq. 'Am') ma = 243.0d0
		if(nam .eq. 'Pu') ma = 244.0d0
		if(nam .eq. 'Cm') ma = 247.0d0
		if(nam .eq. 'Bk') ma = 247.0d0
		if(nam .eq. 'Cf') ma = 251.0d0
		if(nam .eq. 'Es') ma = 252.0d0
		if(nam .eq. 'Fm') ma = 257.0d0
		if(nam .eq. 'Md') ma = 258.0d0
		if(nam .eq. 'No') ma = 259.0d0
		if(nam .eq. 'Rf') ma = 261.0d0
		if(nam .eq. 'Lr') ma = 262.0d0
		if(nam .eq. 'Db') ma = 262.0d0
		if(nam .eq. 'Bh') ma = 264.0d0
		if(nam .eq. 'Sg') ma = 266.0d0
		if(nam .eq. 'Mt') ma = 268.0d0
		if(nam .eq. 'Rg') ma = 272.0d0
		if(nam .eq. 'Hs') ma = 277.0d0
!!!!!!!!!!!!!!!! Now atomic mass in unit of gr is
!!!!!!!!!!!!!!!! PS: the value of 10^23,  to convert volume from Ang^3 to Cm^3 remove.
!!!!!!!!!!!!!!!! 1 Ang^3 =10^(-24) Cm^3 
                ma = ma/6.022045d0
                ma2 = ma*1.661D0*(10**-24)
       
      END
      
