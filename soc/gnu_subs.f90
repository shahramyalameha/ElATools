
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D and 2D matereials; other format for GNUPLOT sqripts: dat2gnu program.

subroutine setreset()
   WRITE(2,'(a)')'unset terminal '
   WRITE(2,'(a)')'unset  output '
   WRITE(2,'(a)')'reset'
  end subroutine setreset
   
   
  subroutine unset1()   
   WRITE(2,'(a)')'set size ratio 1'
   WRITE(2,'(a)')'unset border'
   WRITE(2,'(a)')'unset xtics '
   WRITE(2,'(a)')'unset ytics'
   WRITE(2,'(a)')'set angle degrees'
  end subroutine unset1  
  
 subroutine set1()
  ChARACTER(len=2) :: e1,e2
  Real(8)          :: h,k,l
   call plan(e1,e2,h,k,l)  
   WRITE(2,'(a)')' print maxi'
   WRITE(2,'(a)')'unset key'
   WRITE(2,'(a)')'print ARG1'
   WRITE(2,'(a)')'set polar'
   WRITE(2,'(a)')'set grid polar'
   WRITE(2,'(a)')'set rrange  [0: ARG2] # chenge ARG2*1 to ARG2*0.5 or other...'
   WRITE(2,'(a)')'set rtics  0,ARG1 '
   WRITE(2,'(a)')'set arrow 1 from first 0,0 to first 0,ARG2*1 filled'
   WRITE(2,'(a,A1,A)')"set label 1 '",e1,"' at first 0,ARG2 offset 1,-.5 "
   WRITE(2,'(a)')'set arrow 2 from first 0,0 to first ARG2 filled'
   WRITE(2,'(a,A2,A)')"set label 2 '",e2,"' at first ARG2*1,0 offset -2.25,1"
 end subroutine set1  



  subroutine set1_2d()  
    ChARACTER(len=2) :: e1,e2
    integer          :: h,k,l
    !call plan(e1,e2,h,k,l)
    e1=""
    e2=""
   WRITE(2,'(a)')' print ARG2'
   WRITE(2,'(a)')'unset key'
   WRITE(2,'(a)')'print ARG1'
   WRITE(2,'(a)')'set polar'
   WRITE(2,'(a)')'set grid polar'
   WRITE(2,'(a)')'set rrange  [0:ARG2] # chenge ARG2*1 to ARG2*0.5 or other...'
   WRITE(2,'(a)')'set rtics  0,ARG1 '
   WRITE(2,'(a)')'set arrow 1 from first 0,0 to first 0,ARG2*1 filled'
   WRITE(2,'(a,A1,A)')" set label 1 '",e1,"' at first 0,ARG2 offset 1,-.5 "
   WRITE(2,'(a)')'set arrow 2 from first 0,0 to first ARG2 filled'
   WRITE(2,'(a,A1,A)')" set label 2 '",e2,"' at first ARG2*1,0 offset -2.25,1"
 end subroutine set1_2d 
 subroutine setterm()  
   WRITE(2,'(a)')'  reset'
   WRITE(2,'(a)')'unset output '
   WRITE(2,'(a)')'set term postscript eps enhanced color "Times-Roman, 15"'
 end subroutine setterm  


  subroutine setoutput(val0)
   ChARACTER(len=10) :: val0
   if (val0=='poi') then; WRITE(2,'(a)')'set output "Poissons.ps"';                                        endif
     if (val0=='com') then; WRITE(2,'(a)')'set output "Compressibiliy.ps"';                               endif  
       if (val0=='she') then; WRITE(2,'(a)')'set output "Shear.ps"';                                     endif
         if (val0=='pug') then; WRITE(2,'(a)')'set output "Pugh.ps"';                                   endif
	          if (val0=='you') then; WRITE(2,'(a)')'set output "Young.ps"';                               endif
	            if (val0=='bul') then; WRITE(2,'(a)')'set output "Bulk.ps"';                             endif
	              if (val0=='sou') then; WRITE(2,'(a)')'set output "Sound.ps';                          endif
	                if (val0=='2poi') then; WRITE(2,'(a)')'set output "Poissons_2D.ps"';               endif   
                    if (val0=='2you') then; WRITE(2,'(a)')'set output "Young_2D.ps"';              endif
                      if (val0=='2she') then; WRITE(2,'(a)')'set output "Shear_2D.ps"';           endif
                        if (val0=='pp') then; WRITE(2,'(a)')'set output "Phase-P.ps"';             endif
                          if (val0=='ps') then; WRITE(2,'(a)')'set output "Phase-Slow.ps"';         endif
                            if (val0=='pf') then; WRITE(2,'(a)')'set output "Phase-Fast.ps"';        endif
                              if (val0=='gp') then; WRITE(2,'(a)')'set output "Group-P.ps"';          endif
                                if (val0=='gs') then; WRITE(2,'(a)')'set output "Group-Slow.ps"';      endif
                                  if (val0=='gf') then; WRITE(2,'(a)')'set output "Group-Fast.ps"';     endif
                                   if (val0=='pall') then; WRITE(2,'(a)')'set output "Phase.ps"';        endif
                                    if (val0=='gall') then; WRITE(2,'(a)')'set output "Group.ps"';        endif
                                      if (val0=='pfall') then; WRITE(2,'(a)')'set output "Power-flow.ps"'; endif
    end subroutine setoutput
   
   subroutine settit(val0)  
      ChARACTER(len=10) :: val0
      ChARACTER(len=2) :: e1,e2
      integer          :: h,k,l
      call plan(e1,e2,h,k,l)
     if (val0=='poi') then; WRITE(2,'(a,3I1,a)')'set title "Poissons Ratio \n (', h,k,l,')-plane"';                                                   endif
        if (val0=='com') then; WRITE(2,'(a,3I1,a)')'set title "Linear Compressibiliy (1/TPa)\n (', h,k,l,')-plane"';                                   endif 
           if (val0=='she') then; WRITE(2,'(a,3I1,a)')'set title "Shear Modulus (GPa) \n (', h,k,l,')-plane"';                                          endif
             if (val0=='puh') then; WRITE(2,'(a,3I1,a)')'set title "Pugh Ratio \n (', h,k,l,')-plane"';                                                  endif
	              if (val0=='you') then; WRITE(2,'(a,3I1,a)')'set title "Young Modulus (GPa) \n (', h,k,l,')-plane"';                                        endif
	                if (val0=='bul') then; WRITE(2,'(a,3I1,a)')'set title "Bulk Modulus (GPa) \n (', h,k,l,')-plane"';                                        endif
		                 if (val0=='sou') then; WRITE(2,'(a,3I1,a)')'set title "Sound \n (', h,k,l,')-plane"';                                                    endif
                       if (val0=='2poi') then; WRITE(2,'(a,3I1,a)')'set title "Poissons Ratio"';                                                             endif
                         if (val0=='2you') then;  WRITE(2,'(a,3I1,a)')'set title "Young Modulus (N/m)';                                                       endif
                           if (val0=='2she') then;  WRITE(2,'(a,3I1,a)')'set title "Shear Modulus (N/m)';                                                      endif
                             if (val0=='pp') then; WRITE(2,'(a,3I1,a)')'set title "Phase-P (km/s)\n (', h,k,l,')-plane"';                                     endif
                               if (val0=='ps') then; WRITE(2,'(a,3I1,a)')'set title "Phase-Slow (km/s)\n (', h,k,l,')-plane"';                               endif
                                 if (val0=='pf') then; WRITE(2,'(a,3I1,a)')'set title "Phase-Fast (km/s) \n (', h,k,l,')-plane"';                           endif
                                   if (val0=='gp') then; WRITE(2,'(a,3I1,a)')'set title "Group-P (km/s) \n (', h,k,l,')-plane"';                           endif
                                     if (val0=='gs') then; WRITE(2,'(a,3I1,a)')'set title "Group-Slow (km/s)\n (', h,k,l,')-plane"';                      endif
                                       if (val0=='gf') then; WRITE(2,'(a,3I1,a)')'set title "Group-Fast (km/s) \n (', h,k,l,')-plane"';                  endif
                                         if (val0=='pall') then; WRITE(2,'(a,3I1,a)')'set title "Phase velocity (km/s)\n (', h,k,l,')-plane"';          endif
                                           if (val0=='gall') then; WRITE(2,'(a,3I1,a)')'set title "Group velocity (km/s) \n (', h,k,l,')-plane"';      endif
                                             if (val0=='pfall') then; WRITE(2,'(a,3I1,a)')'set title "Power flow angle (Deg) \n (', h,k,l,')-plane"'; endif
	   end subroutine settit

     subroutine plan(e1,e2,h,k,l)

      ChARACTER(len=2) :: e1,e2,yn_veloc
      integer          :: h,k,l
      open(8,file="HKL")
      read(8,*) e1
      read(8,*) e2
      read(8,*) h
      read(8,*) k
      read(8,*) l
      read(8,*)yn_veloc
      !write(*,*)e1,e2
       if (e1.EQ."X" .or. e1.EQ."Y" .or. e1.EQ."Z") then
         !write(*,*)e1,e2 
         
       else
        e1=""
        e2=""
       endif
       close(8)

    end subroutine plan
