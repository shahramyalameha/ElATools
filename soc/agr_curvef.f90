!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; start format for creat agr file.

SUBROUTINE curve_frame1_agr(N_frame)
      IMPLICIT  NONE
   integer:: N_frame
       WRITE(2,'(a,I1,a)') '@g',N_frame,' on' !>>>>
       WRITE(2,'(a,I1,a)') '@g',N_frame,' hidden false'
       WRITE(2,'(a,I1,a)') '@g',N_frame,' type XY'
       WRITE(2,'(a,I1,a)') '@g',N_frame,' stacked false'
       WRITE(2,'(a,I1,a)') '@g',N_frame,' bar hgap 0.000000'
       WRITE(2,'(a,I1,a)') '@g',N_frame,' fixedpoint off'
       WRITE(2,'(a,I1,a)') '@g',N_frame,' fixedpoint type 0'
       WRITE(2,'(a,I1,a)') '@g',N_frame,' fixedpoint xy 0.000000, 0.000000'
       WRITE(2,'(a,I1,a)') '@g',N_frame,' fixedpoint format general general'
       WRITE(2,'(a,I1,a)') '@g',N_frame,' fixedpoint prec 6, 6'
       WRITE(2,'(a,I1,a)') '@with g',N_frame,' ' !>>>>
END SUBROUTINE


   SUBROUTINE set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)
      IMPLICIT  NONE
    REAL(8)::xticmin,yticmin,xticmax,yticmax,tmajor_yy,tmajor_xx,x1,y1,x2,y2
    ChARACTER(30) :: nlabel
    ChARACTER(4) ::	onoff
    ChARACTER(6) ::	Xlabel,FT
 WRITE(2,'(a,F10.2 ,a,F10.2 ,a,F10.2 ,a,F10.2 )') '@  world ',xticmin,',',yticmin,',', xticmax,',', yticmax !>>>
 WRITE(2,'(a)') '@  stack world 0, 0, 0, 0'
 WRITE(2,'(a)') '@  znorm 1'
 WRITE(2,'(a,F11.6,a,F11.6,a,F11.6,a,F11.6)') ' @  view ',x1,',',y1,',',x2,',',y2 !0.150000, 0.531818, 0.442388, 0.850000' !>>>
 WRITE(2,'(a)') '@  title ""'
 WRITE(2,'(a)') '@  title font 0'
 WRITE(2,'(a)') '@  title size 1.500000' !>>>
 WRITE(2,'(a)') '@  title color 1' !>>>
 WRITE(2,'(a)') '@  subtitle ""' !>>>
 WRITE(2,'(a)') '@  subtitle font 0'!>>>
 WRITE(2,'(a)') '@  subtitle size 1.000000' !>>
 WRITE(2,'(a)') '@  subtitle color 1'!>>>
 WRITE(2,'(a)') '@  xaxes scale Normal'
 WRITE(2,'(a)') '@  yaxes scale Normal'
 WRITE(2,'(a)') '@  xaxes invert off'
 WRITE(2,'(a)') '@  yaxes invert off'
 WRITE(2,'(a)') '@  xaxis  on'
 WRITE(2,'(2a)') '@  xaxis  type zero ',FT
 WRITE(2,'(a)') '@  xaxis  offset 0.000000 , 0.000000'
 WRITE(2,'(a)') '@  xaxis  bar on'
 WRITE(2,'(a)') '@  xaxis  bar color 1'
 WRITE(2,'(a)') '@  xaxis  bar linestyle 1'
 WRITE(2,'(a)') '@  xaxis  bar linewidth 1.0'
 WRITE(2,'(3a)') '@  xaxis  label "',Xlabel,'"' !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 WRITE(2,'(a)') '@  xaxis  label layout para'
 WRITE(2,'(a)') '@  xaxis  label place auto'
 WRITE(2,'(a)') '@  xaxis  label char size 0.75000'
 WRITE(2,'(a)') '@  xaxis  label font 0'
 WRITE(2,'(a)') '@  xaxis  label color 1'
 WRITE(2,'(a)') '@  xaxis  label place normal'
 WRITE(2,'(a)') '@  xaxis  tick on'
 WRITE(2,'(a,F11.6)') '@  xaxis  tick major ', tmajor_xx
 WRITE(2,'(a)') '@  xaxis  tick minor ticks 1'
 WRITE(2,'(a)') '@  xaxis  tick default 6'
 WRITE(2,'(a)') '@  xaxis  tick place rounded true'
 WRITE(2,'(a)') '@  xaxis  tick in'
 WRITE(2,'(a)') '@  xaxis  tick major size 1.000000'
 WRITE(2,'(a)') '@  xaxis  tick major color 1'
 WRITE(2,'(a)') '@  xaxis  tick major linewidth 1.0'
 WRITE(2,'(a)') '@  xaxis  tick major linestyle 1'
 WRITE(2,'(a)') '@  xaxis  tick major grid off'
 WRITE(2,'(a)') '@  xaxis  tick minor color 1'
 WRITE(2,'(a)') '@  xaxis  tick minor linewidth 1.0'
 WRITE(2,'(a)') '@  xaxis  tick minor linestyle 1'
 WRITE(2,'(a)') '@  xaxis  tick minor grid off'
 WRITE(2,'(a)') '@  xaxis  tick minor size 0.500000'
 WRITE(2,'(2a)') '@  xaxis  ticklabel  ', onoff !>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 WRITE(2,'(a)') '@  xaxis  ticklabel format general'
 WRITE(2,'(a)') '@  xaxis  ticklabel prec 5'
 WRITE(2,'(a)') '@  xaxis  ticklabel formula ""'
 WRITE(2,'(a)') '@  xaxis  ticklabel append ""'
 WRITE(2,'(a)') '@  xaxis  ticklabel prepend ""'
 WRITE(2,'(a)') '@  xaxis  ticklabel angle 90'
 WRITE(2,'(a)') '@  xaxis  ticklabel skip 0'
 WRITE(2,'(a)') '@  xaxis  ticklabel stagger 0'
 WRITE(2,'(a)') '@  xaxis  ticklabel place normal'
 WRITE(2,'(a)') '@  xaxis  ticklabel offset auto'
 WRITE(2,'(a)') '@  xaxis  ticklabel offset 0.000000 , 0.010000'
 WRITE(2,'(a)') '@  xaxis  ticklabel start type auto'
 WRITE(2,'(a)') '@  xaxis  ticklabel start 0.000000'
 WRITE(2,'(a)') '@  xaxis  ticklabel stop type auto'
 WRITE(2,'(a)') '@  xaxis  ticklabel stop 0.000000'
 WRITE(2,'(a)') '@  xaxis  ticklabel char size 0.500000'
 WRITE(2,'(a)') '@  xaxis  ticklabel font 0'
 WRITE(2,'(a)') '@  xaxis  ticklabel color 1'
 WRITE(2,'(a)') '@  xaxis  tick place both'
 WRITE(2,'(a)') '@  xaxis  tick spec type none'
 WRITE(2,'(a)') '@  yaxis  on'
 WRITE(2,'(2a)') '@  yaxis  type zero ',FT
 WRITE(2,'(a)') '@  yaxis  offset 0.000000 , 0.000000'
 WRITE(2,'(a)') '@  yaxis  bar on' !>>>
 WRITE(2,'(a)') '@  yaxis  bar color 1' !>>>
 WRITE(2,'(a)') '@  yaxis  bar linestyle 1' !>>>
 WRITE(2,'(a)') '@  yaxis  bar linewidth 1.0'!>>>
 WRITE(2,'(3a)') '@  yaxis  label "',nlabel,'"'!>>>
 WRITE(2,'(a)') '@  yaxis  label layout para'
 WRITE(2,'(a)') '@  yaxis  label place auto'
 WRITE(2,'(a)') '@  yaxis  label char size 0.600000'
 WRITE(2,'(a)') '@  yaxis  label font 0'!>>>
 WRITE(2,'(a)') '@  yaxis  label color 1'!>>
 WRITE(2,'(a)') '@  yaxis  label place normal'
 WRITE(2,'(a)') '@  yaxis  tick on' !>>>
 WRITE(2,'(a,F11.6)') '@  yaxis  tick major ',tmajor_yy  ! !!!!!!!!!!!!!!!!!!!>>>
 WRITE(2,'(a)') '@  yaxis  tick minor ticks 1'!>>>
 WRITE(2,'(a)') '@  yaxis  tick default 6'
 WRITE(2,'(a)') '@  yaxis  tick place rounded true'
 WRITE(2,'(a)') '@  yaxis  tick in'!>>>
 WRITE(2,'(a)') '@  yaxis  tick major size 1.000000' !>>>
 WRITE(2,'(a)') '@  yaxis  tick major color 1'!>>>
 WRITE(2,'(a)') '@  yaxis  tick major linewidth 1.0'!>>
 WRITE(2,'(a)') '@  yaxis  tick major linestyle 1'!>>>
 WRITE(2,'(a)') '@  yaxis  tick major grid off'
 WRITE(2,'(a)') '@  yaxis  tick minor color 1' !>>>
 WRITE(2,'(a)') '@  yaxis  tick minor linewidth 1.0'!>>>
 WRITE(2,'(a)') '@  yaxis  tick minor linestyle 1'!>>>
 WRITE(2,'(a)') '@  yaxis  tick minor grid off'
 WRITE(2,'(a)') '@  yaxis  tick minor size 0.500000'!>>>
 WRITE(2,'(a)') '@  yaxis  ticklabel on'!>>>
 WRITE(2,'(a)') '@  yaxis  ticklabel format general'
 WRITE(2,'(a)') '@  yaxis  ticklabel prec 5'
 WRITE(2,'(a)') '@  yaxis  ticklabel formula ""'
 WRITE(2,'(a)') '@  yaxis  ticklabel append ""'
 WRITE(2,'(a)') '@  yaxis  ticklabel prepend ""'
 WRITE(2,'(a)') '@  yaxis  ticklabel angle 90' !>>>
 WRITE(2,'(a)') '@  yaxis  ticklabel skip 0'
 WRITE(2,'(a)') '@  yaxis  ticklabel stagger 0'
 WRITE(2,'(a)') '@  yaxis  ticklabel place normal'
 WRITE(2,'(a)') '@  yaxis  ticklabel offset auto'
 WRITE(2,'(a)') '@  yaxis  ticklabel offset 0.000000 , 0.010000'
 WRITE(2,'(a)') '@  yaxis  ticklabel start type auto'
 WRITE(2,'(a)') '@  yaxis  ticklabel start 0.000000'
 WRITE(2,'(a)') '@  yaxis  ticklabel stop type auto'
 WRITE(2,'(a)') '@  yaxis  ticklabel stop 0.000000'
 WRITE(2,'(a)') '@  yaxis  ticklabel char size 0.500000' !>>>
 WRITE(2,'(a)') '@  yaxis  ticklabel font 0' !>>>
 WRITE(2,'(a)') '@  yaxis  ticklabel color 1' !>>>
 WRITE(2,'(a)') '@  yaxis  tick place both' !>>>
 WRITE(2,'(a)') '@  yaxis  tick spec type none'
 WRITE(2,'(a)') '@  altxaxis  off'
 WRITE(2,'(a)') '@  altyaxis  off'
 WRITE(2,'(a)') '@  legend on'
 WRITE(2,'(a)') '@  legend loctype view'
 WRITE(2,'(a)') '@  legend 0.85, 0.8'
 WRITE(2,'(a)') '@  legend box color 1'
 WRITE(2,'(a)') '@  legend box pattern 1'
 WRITE(2,'(a)') '@  legend box linewidth 1.0'
 WRITE(2,'(a)') '@  legend box linestyle 1'
 WRITE(2,'(a)') '@  legend box fill color 0'
 WRITE(2,'(a)') '@  legend box fill pattern 1'
 WRITE(2,'(a)') '@  legend font 0'
 WRITE(2,'(a)') '@  legend char size 1.000000'
 WRITE(2,'(a)') '@  legend color 1'
 WRITE(2,'(a)') '@  legend length 4'
 WRITE(2,'(a)') '@  legend vgap 1'
 WRITE(2,'(a)') '@  legend hgap 1'
 WRITE(2,'(a)') '@  legend invert false'
 WRITE(2,'(a)') '@  frame type 0'
 WRITE(2,'(a)') '@  frame linestyle 1'
 WRITE(2,'(a)') '@  frame linewidth 1.0 ' 
 WRITE(2,'(a)') '@  frame color 1'
 WRITE(2,'(a)') '@  frame pattern 1'
 WRITE(2,'(a)') '@  frame background color 0'
 WRITE(2,'(a)') '@  frame background pattern 0'
 
END SUBROUTINE

   SUBROUTINE set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)
      IMPLICIT  NONE
    REAL(8)::xticmin,yticmin,xticmax,yticmax,tmajor_yy,tmajor_xx,x1,y1,x2,y2,char_z_x,char_z_y,frame_style
    ChARACTER(30) :: nlabel
    ChARACTER(4) ::	onoff
    ChARACTER(6) ::	Xlabel,FT
 WRITE(2,'(a,F10.2 ,a,F10.2 ,a,F10.2 ,a,F10.2 )') '@  world ',xticmin,',',yticmin,',', xticmax,',', yticmax !>>>
 WRITE(2,'(a)') '@  stack world 0, 0, 0, 0'
 WRITE(2,'(a)') '@  znorm 1'
 WRITE(2,'(a,F11.6,a,F11.6,a,F11.6,a,F11.6)') ' @  view ',x1,',',y1,',',x2,',',y2 !0.150000, 0.531818, 0.442388, 0.850000' !>>>
 WRITE(2,'(a)') '@  title ""'
 WRITE(2,'(a)') '@  title font 0'
 WRITE(2,'(a)') '@  title size 1.500000' !>>>
 WRITE(2,'(a)') '@  title color 1' !>>>
 WRITE(2,'(a)') '@  subtitle ""' !>>>
 WRITE(2,'(a)') '@  subtitle font 0'!>>>
 WRITE(2,'(a)') '@  subtitle size 1.000000' !>>
 WRITE(2,'(a)') '@  subtitle color 1'!>>>
 WRITE(2,'(a)') '@  xaxes scale Normal'
 WRITE(2,'(a)') '@  yaxes scale Normal'
 WRITE(2,'(a)') '@  xaxes invert off'
 WRITE(2,'(a)') '@  yaxes invert off'
 WRITE(2,'(a)') '@  xaxis  on'
 WRITE(2,'(2a)') '@  xaxis  type zero ',FT
 WRITE(2,'(a)') '@  xaxis  offset 0.000000 , 0.000000'
 WRITE(2,'(a)') '@  xaxis  bar on'
 WRITE(2,'(a)') '@  xaxis  bar color 1'
 WRITE(2,'(a)') '@  xaxis  bar linestyle 1'
 WRITE(2,'(a)') '@  xaxis  bar linewidth 1.0'
 WRITE(2,'(3a)') '@  xaxis  label "',Xlabel,'"' !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 WRITE(2,'(a)') '@  xaxis  label layout para'
 WRITE(2,'(a)') '@  xaxis  label place auto'
 WRITE(2,'(a,F11.6)') '@  xaxis  label char size',char_z_x  
 WRITE(2,'(a)') '@  xaxis  label font 0'
 WRITE(2,'(a)') '@  xaxis  label color 1'
 WRITE(2,'(a)') '@  xaxis  label place normal'
 WRITE(2,'(a)') '@  xaxis  tick on'
 WRITE(2,'(a,F11.6)') '@  xaxis  tick major ', tmajor_xx
 WRITE(2,'(a)') '@  xaxis  tick minor ticks 1'
 WRITE(2,'(a)') '@  xaxis  tick default 6'
 WRITE(2,'(a)') '@  xaxis  tick place rounded true'
 WRITE(2,'(a)') '@  xaxis  tick in'
 WRITE(2,'(a)') '@  xaxis  tick major size 1.000000'
 WRITE(2,'(a)') '@  xaxis  tick major color 1'
 WRITE(2,'(a)') '@  xaxis  tick major linewidth 1.0'
 WRITE(2,'(a)') '@  xaxis  tick major linestyle 1'
 WRITE(2,'(a)') '@  xaxis  tick major grid off'
 WRITE(2,'(a)') '@  xaxis  tick minor color 1'
 WRITE(2,'(a)') '@  xaxis  tick minor linewidth 1.0'
 WRITE(2,'(a)') '@  xaxis  tick minor linestyle 1'
 WRITE(2,'(a)') '@  xaxis  tick minor grid off'
 WRITE(2,'(a)') '@  xaxis  tick minor size 0.500000'
 WRITE(2,'(2a)') '@  xaxis  ticklabel  ', onoff !>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 WRITE(2,'(a)') '@  xaxis  ticklabel format general'
 WRITE(2,'(a)') '@  xaxis  ticklabel prec 5'
 WRITE(2,'(a)') '@  xaxis  ticklabel formula ""'
 WRITE(2,'(a)') '@  xaxis  ticklabel append ""'
 WRITE(2,'(a)') '@  xaxis  ticklabel prepend ""'
 WRITE(2,'(a)') '@  xaxis  ticklabel angle 0'
 WRITE(2,'(a)') '@  xaxis  ticklabel skip 0'
 WRITE(2,'(a)') '@  xaxis  ticklabel stagger 0'
 WRITE(2,'(a)') '@  xaxis  ticklabel place normal'
 WRITE(2,'(a)') '@  xaxis  ticklabel offset auto'
 WRITE(2,'(a)') '@  xaxis  ticklabel offset 0.000000 , 0.010000'
 WRITE(2,'(a)') '@  xaxis  ticklabel start type auto'
 WRITE(2,'(a)') '@  xaxis  ticklabel start 0.000000'
 WRITE(2,'(a)') '@  xaxis  ticklabel stop type auto'
 WRITE(2,'(a)') '@  xaxis  ticklabel stop 0.000000'
 WRITE(2,'(a)') '@  xaxis  ticklabel char size 1.100000'
 WRITE(2,'(a)') '@  xaxis  ticklabel font 0'
 WRITE(2,'(a)') '@  xaxis  ticklabel color 1'
 WRITE(2,'(a)') '@  xaxis  tick place both'
 WRITE(2,'(a)') '@  xaxis  tick spec type none'
 WRITE(2,'(a)') '@  yaxis  on'
 WRITE(2,'(2a)') '@  yaxis  type zero ',FT
 WRITE(2,'(a)') '@  yaxis  offset 0.000000 , 0.000000'
 WRITE(2,'(a)') '@  yaxis  bar on' !>>>
 WRITE(2,'(a)') '@  yaxis  bar color 1' !>>>
 WRITE(2,'(a)') '@  yaxis  bar linestyle 1' !>>>
 WRITE(2,'(a)') '@  yaxis  bar linewidth 1.0'!>>>
 WRITE(2,'(3a)') '@  yaxis  label "',nlabel,'"'!>>>
 WRITE(2,'(a)') '@  yaxis  label layout para'
 WRITE(2,'(a)') '@  yaxis  label place auto'
 WRITE(2,'(a,F11.6)') '@  yaxis  label char size',char_z_y  
 WRITE(2,'(a)') '@  yaxis  label font 0'!>>>
 WRITE(2,'(a)') '@  yaxis  label color 1'!>>
 WRITE(2,'(a)') '@  yaxis  label place normal'
 WRITE(2,'(a)') '@  yaxis  tick on' !>>>
 WRITE(2,'(a,F11.6)') '@  yaxis  tick major ',tmajor_yy  ! !!!!!!!!!!!!!!!!!!!>>>
 WRITE(2,'(a)') '@  yaxis  tick minor ticks 1'!>>>
 WRITE(2,'(a)') '@  yaxis  tick default 6'
 WRITE(2,'(a)') '@  yaxis  tick place rounded true'
 WRITE(2,'(a)') '@  yaxis  tick in'!>>>
 WRITE(2,'(a)') '@  yaxis  tick major size 1.000000' !>>>
 WRITE(2,'(a)') '@  yaxis  tick major color 1'!>>>
 WRITE(2,'(a)') '@  yaxis  tick major linewidth 1.0'!>>
 WRITE(2,'(a)') '@  yaxis  tick major linestyle 1'!>>>
 WRITE(2,'(a)') '@  yaxis  tick major grid off'
 WRITE(2,'(a)') '@  yaxis  tick minor color 1' !>>>
 WRITE(2,'(a)') '@  yaxis  tick minor linewidth 1.0'!>>>
 WRITE(2,'(a)') '@  yaxis  tick minor linestyle 1'!>>>
 WRITE(2,'(a)') '@  yaxis  tick minor grid off'
 WRITE(2,'(a)') '@  yaxis  tick minor size 0.500000'!>>>
 WRITE(2,'(a)') '@  yaxis  ticklabel on'!>>>
 WRITE(2,'(a)') '@  yaxis  ticklabel format general'
 WRITE(2,'(a)') '@  yaxis  ticklabel prec 5'
 WRITE(2,'(a)') '@  yaxis  ticklabel formula ""'
 WRITE(2,'(a)') '@  yaxis  ticklabel append ""'
 WRITE(2,'(a)') '@  yaxis  ticklabel prepend ""'
 WRITE(2,'(a)') '@  yaxis  ticklabel angle 0' !>>>
 WRITE(2,'(a)') '@  yaxis  ticklabel skip 0'
 WRITE(2,'(a)') '@  yaxis  ticklabel stagger 0'
 WRITE(2,'(a)') '@  yaxis  ticklabel place normal'
 WRITE(2,'(a)') '@  yaxis  ticklabel offset auto'
 WRITE(2,'(a)') '@  yaxis  ticklabel offset 0.000000 , 0.010000'
 WRITE(2,'(a)') '@  yaxis  ticklabel start type auto'
 WRITE(2,'(a)') '@  yaxis  ticklabel start 0.000000'
 WRITE(2,'(a)') '@  yaxis  ticklabel stop type auto'
 WRITE(2,'(a)') '@  yaxis  ticklabel stop 0.000000'
 WRITE(2,'(a)') '@  yaxis  ticklabel char size 1.100000' !>>>
 WRITE(2,'(a)') '@  yaxis  ticklabel font 0' !>>>
 WRITE(2,'(a)') '@  yaxis  ticklabel color 1' !>>>
 WRITE(2,'(a)') '@  yaxis  tick place both' !>>>
 WRITE(2,'(a)') '@  yaxis  tick spec type none'
 WRITE(2,'(a)') '@  altxaxis  off'
 WRITE(2,'(a)') '@  altyaxis  off'
 WRITE(2,'(a)') '@  legend on'
 WRITE(2,'(a)') '@  legend loctype view'
 WRITE(2,'(a)') '@  legend 0.85, 0.8'
 WRITE(2,'(a)') '@  legend box color 1'
 WRITE(2,'(a)') '@  legend box pattern 1'
 WRITE(2,'(a)') '@  legend box linewidth 1.0'
 WRITE(2,'(a)') '@  legend box linestyle 1'
 WRITE(2,'(a)') '@  legend box fill color 0'
 WRITE(2,'(a)') '@  legend box fill pattern 1'
 WRITE(2,'(a)') '@  legend font 0'
 WRITE(2,'(a)') '@  legend char size 1.000000'
 WRITE(2,'(a)') '@  legend color 1'
 WRITE(2,'(a)') '@  legend length 4'
 WRITE(2,'(a)') '@  legend vgap 1'
 WRITE(2,'(a)') '@  legend hgap 1'
 WRITE(2,'(a)') '@  legend invert false'
 WRITE(2,'(a)') '@  frame type 0'
 WRITE(2,'(a, F7.3)') '@  frame linestyle',frame_style
 WRITE(2,'(a)') '@  frame linewidth 1.0'
 WRITE(2,'(a)') '@  frame color 1'
 WRITE(2,'(a)') '@  frame pattern 1'
 WRITE(2,'(a)') '@  frame background color 0'
 WRITE(2,'(a)') '@  frame background pattern 0'
 
END SUBROUTINE
