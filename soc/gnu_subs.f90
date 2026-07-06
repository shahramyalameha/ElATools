
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D and 2D matereials; other format for GNUPLOT sqripts: dat2gnu program.

subroutine setreset()
   WRITE (2, '(a)') 'unset terminal '
   WRITE (2, '(a)') 'unset  output '
   WRITE (2, '(a)') 'reset'
end subroutine setreset

subroutine unset1()
   WRITE (2, '(a)') 'set size ratio 1'
   WRITE (2, '(a)') 'unset border'
   WRITE (2, '(a)') 'unset xtics '
   WRITE (2, '(a)') 'unset ytics'
   WRITE (2, '(a)') 'set angle degrees'
end subroutine unset1

subroutine setphm(val)
   ChARACTER(len=10) :: val
   WRITE (2, '(a)') '#set multiplot  '
   If (val == "phmall") then
      WRITE (2, '(a)') 'set multiplot layout 2,2'
   else
      WRITE (2, '(a)') 'set multiplot layout 1,2'
   end if
   WRITE (2, '(a)') 'set grid polar 30. lt -1 dt 2 lw 1 lc "#a9a9a9"'
   WRITE (2, '(a)') 'set polar'
   WRITE (2, '(a)') 'set angle degrees'
   WRITE (2, '(a)') ' set size ratio 1'
end subroutine setphm

subroutine unset2()
   WRITE (2, '(a)') 'unset key'
   WRITE (2, '(a)') 'unset raxis'
   WRITE (2, '(a)') 'unset rrange'
   WRITE (2, '(a)') 'unset rtics '
   WRITE (2, '(a)') 'unset border'
   WRITE (2, '(a)') 'unset xtics '
   WRITE (2, '(a)') 'unset ytics  '
end subroutine unset2

subroutine set1()
   ChARACTER(len=2) :: e1, e2
   integer          :: h, k, l
   call plan(e1, e2, h, k, l)
   WRITE (2, '(a)') ' print maxi'
   WRITE (2, '(a)') '#unset key'
   WRITE (2, '(a)') 'print ARG1'
   WRITE (2, '(a)') 'set polar'
   WRITE (2, '(a)') '#set grid polar'
   WRITE (2, '(a)') 'set ttics rotate'
   WRITE (2, '(a)') 'set ttics 0,30 format "%g".GPVAL_DEGREE_SIGN font ":Italic"'
   WRITE (2, '(a)') 'set mttics 3'
   WRITE (2, '(a)') 'set grid r polar 60'
   WRITE (2, '(a)') 'set border polar'
   WRITE (2, '(a)') 'set size square'
   WRITE (2, '(a)') 'set rrange  [0: ARG2] # chenge ARG2*1 to ARG2*0.5 or other...'
   WRITE (2, '(a)') 'set rtics  0,ARG1 '
   WRITE (2, '(a)') 'set arrow 1 from first 0,0 to first 0,ARG2*1 filled'
   WRITE (2, '(a,A1,A)') "set label 1 '", e1, "' at first 0,ARG2 offset 1,-.5 "
   WRITE (2, '(a)') 'set arrow 2 from first 0,0 to first ARG2 filled'
   WRITE (2, '(a,A2,A)') "set label 2 '", e2, "' at first ARG2*1,0 offset -2.25,1"
end subroutine set1

subroutine set1_2d()
   ChARACTER(len=2) :: e1, e2
   integer          :: h, k, l
   !call plan(e1,e2,h,k,l)
   e1 = ""
   e2 = ""
   WRITE (2, '(a)') ' print ARG2'
   WRITE (2, '(a)') '#unset key'
   WRITE (2, '(a)') 'print ARG1'
   WRITE (2, '(a)') 'set polar'
   WRITE (2, '(a)') '#set grid polar'
   WRITE (2, '(a)') 'set ttics rotate'
   WRITE (2, '(a)') 'set ttics 0,30 format "%g".GPVAL_DEGREE_SIGN font ":Italic"'
   WRITE (2, '(a)') 'set mttics 3'
   WRITE (2, '(a)') 'set grid r polar 60'
   WRITE (2, '(a)') 'set border polar'
   WRITE (2, '(a)') 'set size square'
   WRITE (2, '(a)') 'set rrange  [0:ARG2] # chenge ARG2*1 to ARG2*0.5 or other...'
   WRITE (2, '(a)') 'set rtics  0,ARG1 '
   WRITE (2, '(a)') 'set arrow 1 from first 0,0 to first 0,ARG2*1 filled'
   WRITE (2, '(a,A1,A)') " set label 1 '", e1, "' at first 0,ARG2 offset 1,-.5 "
   WRITE (2, '(a)') 'set arrow 2 from first 0,0 to first ARG2 filled'
   WRITE (2, '(a,A1,A)') " set label 2 '", e2, "' at first ARG2*1,0 offset -2.25,1"
end subroutine set1_2d

subroutine setterm()
   WRITE (2, '(a)') '  reset'
   WRITE (2, '(a)') 'unset output '
   WRITE (2, '(a)') 'set term postscript eps enhanced color ", 13"'
end subroutine setterm

subroutine setterm_sp()
   WRITE (2, '(a)') 'reset'
   WRITE (2, '(a)') 'unset output '
   WRITE (2, '(a)') 'set term pngcairo enhanced font "Arial, 22"  size 2300,700 crop  lw 3 #transparent'
end subroutine setterm_sp

subroutine setterm_phm()
   WRITE (2, '(a)') 'reset'
   WRITE (2, '(a)') 'unset output '
   WRITE (2, '(a)') '#set term postscript eps enhanced color ", 13"'
   WRITE (2, '(a)') 'set term pngcairo enhanced dashed font "Arial, 19" size 1600,700 nocrop lw 2'
end subroutine setterm_phm

subroutine set_3dsp()
   WRITE (2, '(a)') 'set multiplot layout 1,3'
   WRITE (2, '(a)') 'set pm3d depthorder interpolate 3,3 '
   WRITE (2, '(a)') 'set mapping spherical'
   WRITE (2, '(a)') 'set angle degrees'
   WRITE (2, '(a)') 'set surface'
   WRITE (2, '(a)') 'set xyplane at 0'
   WRITE (2, '(a)') 'set view ,120.,1.05,1.25'
   WRITE (2, '(a)') 'set pm3d lighting primary 0.4 specular 0.2'
   WRITE (2, '(a)') 'unset xtics'
   WRITE (2, '(a)') 'unset ytics'
   WRITE (2, '(a)') 'unset ztics'
   WRITE (2, '(a)') 'unset border'
   WRITE (2, '(a)') 'set arrow from first 0,0,1 to first 0,0,1.39 front filled lc "#3D3D3D"'
   WRITE (2, '(a)') 'set arrow from first 0,1,0 to first 0,1.42,0 front filled lc "#3D3D3D"'
   WRITE (2, '(a)') 'set arrow from first 1,0,0 to first 1.9,0,0 front filled lc "#3D3D3D"'
   WRITE (2, '(a)') 'set label "+X" at first 2.18,0.19,0.05 front'
   WRITE (2, '(a)') 'set label "+Y" at first 0,1.25,0.11 front'
   WRITE (2, '(a)') 'set label "+Z" at first 0,0.05,1.33 front'
end subroutine set_3dsp
subroutine set_3dsp_map()
   WRITE (2, '(a)') 'set multiplot layout 1,3'
   WRITE (2, '(a)') 'set pm3d depthorder explicit interpolate 3,3'
   WRITE (2, '(a)') 'set mapping spherical'
   WRITE (2, '(a)') 'set angle degrees'
   WRITE (2, '(a)') 'set surface'
   WRITE (2, '(a)') 'set view map'
   WRITE (2, '(a)') 'set size ratio 1'
   WRITE (2, '(a)') 'set xyplane at -1.0'
   WRITE (2, '(a)') 'unset xtics'
   WRITE (2, '(a)') 'unset ytics'
   WRITE (2, '(a)') 'unset border'
   WRITE (2, '(a)') 'set arrow from graph 0,0 to graph 0,0.1 filled lc "#3D3D3D"'
   WRITE (2, '(a)') 'set arrow from graph 0,0 to graph 0.1,0 filled lc "#3D3D3D"'
   WRITE (2, '(a)') 'set label "+X" at graph 0.12,0 '
   WRITE (2, '(a)') 'set label "+Y" at graph -0.025,0.14 '
end subroutine set_3dsp_map

subroutine endset_3dsp()
   WRITE (2, '(a)') 'reset'
   WRITE (2, '(a)') 'unset multiplot'
   WRITE (2, '(a)') 'unset output'
end subroutine endset_3dsp

subroutine spliter_sp(numsplit)
   integer  :: numsplit

   IF (numsplit == 1) Then
      WRITE (2, '(a)') 'set colorbox horiz user origin .074,.15 size .208,.05'
      WRITE (2, '(a)') 'set size 0.3916666,1.175'
      WRITE (2, '(a)') 'set origin -0.02,-0.02'
   END IF

   IF (numsplit == 2) Then
      WRITE (2, '(a)') 'set colorbox horiz user origin .39,.15 size .208,.05'
      WRITE (2, '(a)') 'set size 0.3916666,1.175'
      WRITE (2, '(a)') 'set origin 0.2999,-0.02'
   END IF

   IF (numsplit == 3) Then
      WRITE (2, '(a)') 'set colorbox horiz user origin .711,.15 size .208,.05'
      WRITE (2, '(a)') 'set size 0.3916666,1.175'
      WRITE (2, '(a)') 'set origin 0.6199,-0.02'
   END IF
end subroutine spliter_sp

subroutine spliter_sp_map(numsplit)
   integer  :: numsplit

   IF (numsplit == 1) Then
      WRITE (2, '(a)') 'set colorbox horiz user origin .0342,.15 size .218,.05'
      WRITE (2, '(a)') 'set size 0.38,1.14'
      WRITE (2, '(a)') 'set origin -0.05, 0.05'
   END IF

   IF (numsplit == 2) Then
      WRITE (2, '(a)') 'set colorbox horiz user origin .364,.15 size .218,.05'
      WRITE (2, '(a)') 'set size 0.38,1.14'
      WRITE (2, '(a)') 'set origin 0.2833,0.05'
   END IF

   IF (numsplit == 3) Then
      WRITE (2, '(a)') 'set colorbox horiz user origin .699,.15 size .218,.05'
      WRITE (2, '(a)') 'set size 0.38,1.14'
      WRITE (2, '(a)') 'set origin 0.6199,0.05'
   END IF
end subroutine spliter_sp_map

subroutine setoutput(val0, out_format)
   ChARACTER(len=10) :: val0
   if (val0 == 'poi') then; WRITE (2, '(a)') 'set output "Poissons.ps"'; end if
   if (val0 == 'com') then; WRITE (2, '(a)') 'set output "Compressibiliy.ps"'; end if
   if (val0 == 'she') then; WRITE (2, '(a)') 'set output "Shear.ps"'; end if
   if (val0 == 'pug') then; WRITE (2, '(a)') 'set output "Pugh.ps"'; end if
   if (val0 == 'you') then; WRITE (2, '(a)') 'set output "Young.ps"'; end if
   if (val0 == 'bul') then; WRITE (2, '(a)') 'set output "Bulk.ps"'; end if
   if (val0 == 'sou') then; WRITE (2, '(a)') 'set output "Sound.ps'; end if
   if (val0 == '2poi') then; WRITE (2, '(a)') 'set output "Poissons_2D.ps"'; end if
   if (val0 == '2you') then; WRITE (2, '(a)') 'set output "Young_2D.ps"'; end if
   if (val0 == '2she') then; WRITE (2, '(a)') 'set output "Shear_2D.ps"'; end if
   if (val0 == '2dtr') then; WRITE (2, '(a)') 'set output "Transverse_2D.ps'; end if
   if (val0 == '2dlo') then; WRITE (2, '(a)') 'set output "Longitudinal_2D.ps'; end if     
   if (val0 == 'pp') then; WRITE (2, '(a)') 'set output "Phase-P.ps"'; end if
   if (val0 == 'ps') then; WRITE (2, '(a)') 'set output "Phase-Slow.ps"'; end if
   if (val0 == 'pf') then; WRITE (2, '(a)') 'set output "Phase-Fast.ps"'; end if
   if (val0 == 'gp') then; WRITE (2, '(a)') 'set output "Group-P.ps"'; end if
   if (val0 == 'gs') then; WRITE (2, '(a)') 'set output "Group-Slow.ps"'; end if
   if (val0 == 'gf') then; WRITE (2, '(a)') 'set output "Group-Fast.ps"'; end if
   if (val0 == 'pfp') then; WRITE (2, '(a)') 'set output "Power-flow-p.ps"'; end if
   if (val0 == 'pff') then; WRITE (2, '(a)') 'set output "Power-flow-fast.ps"'; end if
   if (val0 == 'pfs') then; WRITE (2, '(a)') 'set output "Power-flow-slow.ps"'; end if
   if (val0 == 'pall') then; WRITE (2, '(a)') 'set output "Phase.ps"'; end if
   if (val0 == 'gall') then; WRITE (2, '(a)') 'set output "Group.ps"'; end if
   if (val0 == 'pfall') then; WRITE (2, '(a)') 'set output "Power-flow.ps"'; end if
   if (val0 == 'hard') then; WRITE (2, '(a)') 'set output "Hardness.ps"'; end if
   
   if (val0 == 'phmpoi') then; WRITE (2, '(a)') 'set output "Poissons_2Dphm.png"'; end if
   if (val0 == 'phmyon') then; WRITE (2, '(a)') 'set output "Young_2Dphm.png"'; end if
   if (val0 == 'phmshe') then; WRITE (2, '(a)') 'set output "Shear_2Dphm.png"'; end if
   if (val0 == 'phmtran') then; WRITE (2, '(a)') 'set output "Transverse_2Dphm.png"'; end if
   if (val0 == 'phmlong') then; WRITE (2, '(a)') 'set output "Longitudinal_2Dphm.png"'; end if   
   if (val0 == 'phmall') then; WRITE (2, '(a)') 'set output "All_2Dphm.png"'; end if
   
   if (val0 == 'km') then; WRITE (2, '(a)') 'set output "mthconductivity.ps'; end if
end subroutine setoutput

subroutine setoutput_sp(val0)
   ChARACTER(len=10) :: val0
   if (val0 == 'sppoi') then; WRITE (2, '(a)') 'set output "Poissons-3dsp.png"'; end if
   if (val0 == 'spmpoi') then; WRITE (2, '(a)') 'set output "Poissons-xysp.png"'; end if
   
   if (val0 == 'sppugh') then; WRITE (2, '(a)') 'set output "Pugh-3dsp.png"'; end if
   if (val0 == 'spmpugh') then; WRITE (2, '(a)') 'set output "Pugh-xysp.png"'; end if
   
   if (val0 == 'spshear') then; WRITE (2, '(a)') 'set output "Shear-3dsp.png"'; end if
   if (val0 == 'spmshear') then; WRITE (2, '(a)') 'set output "Shear-xysp.png"'; end if
   
   if (val0 == 'spcomp') then; WRITE (2, '(a)') 'set output "Compressibiliy-3dsp.png"'; end if
   if (val0 == 'spmcomp') then; WRITE (2, '(a)') 'set output "Compressibiliy-xysp.png"'; end if
   
   if (val0 == 'spyou') then; WRITE (2, '(a)') 'set output "Young-3dsp.png"'; end if
   if (val0 == 'spmyou') then; WRITE (2, '(a)') 'set output "Young-xysp.png"'; end if
   
   if (val0 == 'spbulk') then; WRITE (2, '(a)') 'set output "Bulk-3dsp.png"'; end if
   if (val0 == 'spmbulk') then; WRITE (2, '(a)') 'set output "Bulk-xysp.png"'; end if
   
   if (val0 == 'sppall') then; WRITE (2, '(a)') 'set output "Phase_all-3dsp.png"'; end if
   if (val0 == 'spmpall') then; WRITE (2, '(a)') 'set output "Phase_all-xysp.png"'; end if  
   
   if (val0 == 'spgall') then; WRITE (2, '(a)') 'set output "Group_all-3dsp.png"'; end if
   if (val0 == 'spmgall') then; WRITE (2, '(a)') 'set output "Group_all-xysp.png"'; end if 
   
   if (val0 == 'sppfall') then; WRITE (2, '(a)') 'set output "PFA_all-3dsp.png"'; end if
   if (val0 == 'spmpfall') then; WRITE (2, '(a)') 'set output "PFA_all-xysp.png"'; end if 

   if (val0 == 'spkm') then; WRITE (2, '(a)') 'set output "Km-3dsp.png"'; end if
   if (val0 == 'spmkm') then; WRITE (2, '(a)') 'set output "Km-xysp.png"'; end if    
end subroutine setoutput_sp
!====
subroutine set_cblabel(val0, npro)
   ChARACTER(len=10) :: val0
   integer           :: npro
   if (val0 == "spyou" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "E (GPa)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "spmyou" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "E (GPa)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "spbulk" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "B (GPa)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "spmbulk" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "B (GPa)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "sppoi" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "{/Symbol n}_{ Max}", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "sppoi" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "{/Symbol n}_{ Min}", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "sppoi" .and. npro == 3) then
      WRITE (2, '(3a)') 'set cblabel "', "{/Symbol n}_{ Negative}", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "spmpoi" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "{/Symbol n}_{ Max}", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "spmpoi" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "{/Symbol n}_{ Min}", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "spmpoi" .and. npro == 3) then
      WRITE (2, '(3a)') 'set cblabel "', "{/Symbol n}_{ Negative}", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "sppugh" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "{k}_{ Max}", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "sppugh" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "{k}_{ Min}", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "spmpugh" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "{k}_{ Max}", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "spmpugh" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "{k}_{ Min}", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "spshear" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "{G}_{ Max} (GPa)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "spshear" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "{G}_{ Min} (GPa)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "spmshear" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "{G}_{ Max} (GPa)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "spmshear" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "{G}_{ Min} (GPa)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "spcomp" .or. val0 == "spmcomp" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "{LC}_{ Max} x 10^{2} (TPa^{-1})", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "spcomp" .or. val0 == "spmcomp" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "{LC}_{ Min} x 10^{2} (TPa^{-1})", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "spcomp" .or. val0 == "spmcomp" .and. npro == 3) then
      WRITE (2, '(3a)') 'set cblabel "', "{LC}_{ Negative} x 10^{2} (TPa^{-1})", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if
!!!
   if (val0 == "spmcomp" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "{LC}_{ Max} x 10^{2} (TPa^{-1})", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "spmcomp" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "{LC}_{ Min} x 10^{2} (TPa^{-1})", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   elseif (val0 == "spmcomp" .and. npro == 3) then
      WRITE (2, '(3a)') 'set cblabel "', "{LC}_{ Negative} x 10^{2} (TPa^{-1})", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title " " offset -10,-2.5'
   end if

!!!
   if (val0 == "sppall" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{p} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Primary" offset -10,-2.5'
   elseif (val0 == "sppall" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{p} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Fast Secondary" offset -10,-2.5'
   elseif (val0 == "sppall" .and. npro == 3) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{p} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Slow Secondary" offset -10,-2.5'
   end if   
 
 !!!
   if (val0 == "spgall" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{g} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Primary" offset -10,-2.5'
   elseif (val0 == "spgall" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{g} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Fast Secondary" offset -10,-2.5'
   elseif (val0 == "spgall" .and. npro == 3) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{g} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Slow Secondary" offset -10,-2.5'
   end if    
 !!!
   if (val0 == "sppfall" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "Power flow angle (Deg.)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Primary" offset -10,-2.5'
   elseif (val0 == "sppfall" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "Power flow angle (Deg.)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Fast Secondary" offset -10,-2.5'
   elseif (val0 == "sppfall" .and. npro == 3) then
      WRITE (2, '(3a)') 'set cblabel "', "Power flow angle (Deg.)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Slow Secondary" offset -10,-2.5'
   end if    
   
 !!!
    if (val0 == "spmpall" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{p} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Primary" offset -15,-2.2'
   elseif (val0 == "spmpall" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{p} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Fast Secondary" offset -15,-2.2'
   elseif (val0 == "spmpall" .and. npro == 3) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{p} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Slow Secondary" offset -15,-2.2'
   end if   
 
 !!!  
   if (val0 == "spmgall" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{g} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Primary" offset -15,-2.2'
   elseif (val0 == "spmgall" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{g} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Fast Secondary" offset -15,-2.2'
   elseif (val0 == "spmgall" .and. npro == 3) then
      WRITE (2, '(3a)') 'set cblabel "', "v_{g} (km/s)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Slow Secondary" offset -15,-2.2'
   end if   
 
 !!!
   if (val0 == "spmpfall" .and. npro == 1) then
      WRITE (2, '(3a)') 'set cblabel "', "Power flow angle (Deg.)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Primary" offset -15,-2.2'
   elseif (val0 == "spmpfall" .and. npro == 2) then
      WRITE (2, '(3a)') 'set cblabel "', "Power flow angle (Deg.)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Fast Secondary" offset -15,-2.2'
   elseif (val0 == "spmpfall" .and. npro == 3) then
      WRITE (2, '(3a)') 'set cblabel "', "Power flow angle (Deg.)", '" offset 0.5,0.5'
      WRITE (2, '(a)') 'set title "Slow Secondary" offset -15,-2.2'
   end if    
end subroutine set_cblabel
!====

subroutine pal_color(color_name)
   implicit none
   ChARACTER(len=10) :: color_name
   if (color_name .EQ. "n") then
      color_name = 'plasma'
  WRITE(2,"(3A)") 'call "/home/shahram/Desktop/Cubelast/code/programMY/AAEP/soc/eatools_v1.7.0/db/gnu_pal/',trim(color_name),'.pal"'
   else
  WRITE(2,"(3A)") 'call "/home/shahram/Desktop/Cubelast/code/programMY/AAEP/soc/eatools_v1.7.0/db/gnu_pal/',trim(color_name),'.pal"'
   end if
end subroutine pal_color
!====
subroutine settit(val0)
   ChARACTER(len=10) :: val0
   ChARACTER(len=2) :: e1, e2
   integer          :: h, k, l
   call plan(e1, e2, h, k, l)
   write (*, "(3I2)") h, k, l
   if (val0 == 'poi') then; WRITE (2, '(a,3I2,a)') 'set title "Poissons Ratio \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'com') then; WRITE (2, '(a,3I2,a)') 'set title "Linear Compressibiliy (1/TPa)\n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'she') then; WRITE (2, '(a,3I2,a)') 'set title "Shear Modulus (GPa) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'hard') then; WRITE (2, '(a,3I2,a)') 'set title "Hardness (GPa) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'pug') then; WRITE (2, '(a,3I2,a)') 'set title "Pugh Ratio \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'you') then; WRITE (2, '(a,3I2,a)') 'set title "Young Modulus (GPa) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'bul') then; WRITE (2, '(a,3I2,a)') 'set title "Bulk Modulus (GPa) \n (', h, k, l, ')-plane"'; end if
   if (val0 == 'sou') then; WRITE (2, '(a,3I2,a)') 'set title "Sound \n (', h, k, l, ' )-plane"'; end if
   if (val0 == '2poi') then; WRITE (2, '(a,3I2,a)') 'set title "Poissons Ratio"'; end if
   if (val0 == '2you') then; WRITE (2, '(a,3I2,a)') 'set title "Young Modulus (N/m)'; end if
   if (val0 == '2she') then; WRITE (2, '(a,3I2,a)') 'set title "Shear Modulus (N/m)'; end if
   if (val0 == '2dtr') then; WRITE (2, '(a,3I2,a)') 'set title "Transverse Wave (km/s)'; end if
   if (val0 == '2dlo') then; WRITE (2, '(a,3I2,a)') 'set title "Longitudinal (km/s)'; end if   
   if (val0 == 'pp') then; WRITE (2, '(a,3I2,a)') 'set title "Phase-P (km/s)\n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'ps') then; WRITE (2, '(a,3I2,a)') 'set title "Phase-Slow (km/s)\n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'pf') then; WRITE (2, '(a,3I2,a)') 'set title "Phase-Fast (km/s) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'gp') then; WRITE (2, '(a,3I2,a)') 'set title "Group-P (km/s) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'gs') then; WRITE (2, '(a,3I2,a)') 'set title "Group-Slow (km/s)\n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'gf') then; WRITE (2, '(a,3I2,a)') 'set title "Group-Fast (km/s) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'pall') then; WRITE (2, '(a,3I2,a)') 'set title "Phase velocity (km/s)\n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'gall') then; WRITE (2, '(a,3I2,a)') 'set title "Group velocity (km/s) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'pfp') then; WRITE (2, '(a,3I2,a)') 'set title "Power flow angle P (Deg) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'pff') then; WRITE (2, '(a,3I2,a)') 'set title "Power flow angle fast (Deg) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'pfs') then; WRITE (2, '(a,3I2,a)') 'set title "Power flow angle slow (Deg) \n (', h, k, l, ' )-plane"'; end if

   if (val0 == 'pfall') then; WRITE (2, '(a,3I2,a)') 'set title "Power flow angle (Deg) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'phmpoi') then; WRITE (2, '(a,3I2,a)') 'set title "Poissons Ratio \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'phmyou') then; WRITE (2, '(a,3I2,a)') 'set title "Young Modulus (N/m) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'phmshe') then; WRITE (2, '(a,3I2,a)') 'set title "Shear Modulus (N/m) \n (', h, k, l, ' )-plane"'; end if
   if (val0 == 'km') then; WRITE (2, '(a,3I2,a)') 'set title "Min. thermal conductivity (W/K.m) \n (', h, k, l, ' )-plane"'; end if

end subroutine settit

subroutine plan(e1, e2, h, k, l)

   ChARACTER(len=2) :: e1, e2, yn_veloc
   integer          :: h, k, l
   open (8, file="HKL")
   read (8, *) e1
   read (8, *) e2
   read (8, *) h
   read (8, *) k
   read (8, *) l
   read (8, *) yn_veloc
   close (8)
   ! write(*,*)e1,e2,h,k,l
   if (e1 .EQ. "X" .or. e1 .EQ. "Y" .or. e1 .EQ. "Z") then
      !write(*,*)e1,e2

   else
      e1 = ""
      e2 = ""
   end if

end subroutine plan
