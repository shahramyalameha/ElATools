
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE   end_layout_web(namepro,width,height)
  IMPLICIT NONE 
        integer                          :: height,width
        ChARACTER(len=30)                :: title 
        ChARACTER(len=10)                :: namepro 
     if (namepro == "poi"  )    title="Poisson\'s ratio"
     if (namepro == "young")    title="Young\'s modulus"
     if (namepro == "bulk")    title="Bulk modulus"
     if (namepro == "shear")    title="Shear modulus"
     if (namepro == "comp" )    title="Linear compressibility"
     if (namepro == "hard" )    title="Hardness"
     if (namepro == "pp" )    title="P-mode of Phase velocity"
     if (namepro == "pf" )    title="SF-mode of Phase velocity"
     if (namepro == "ps" )    title="SS-mode of Phase velocity"
     if (namepro == "gp" )    title="P-mode of Group velocity"
     if (namepro == "gf" )    title="SF-mode of Group velocity"
     if (namepro == "gs" )    title="SS-mode of Group velocity"

     if (namepro == "pfp" )    title="Power Flow angle_P-mode"
     if (namepro == "pff" )    title="Power Flow angle_Slow-mode"
     if (namepro == "pfs" )    title="Group velocity_Fast-mode"
     if (namepro == "km" )    title="Min. thermal conductivity"
     
    WRITE(66,"(a)")"var layout ="
    WRITE(66,"(a)")"{"
    WRITE(66,"(3a)")"   title: '",title,"',"
    WRITE(66,"(a,I7,a)")"   width: ",width,","
    WRITE(66,"(a,I7,a)")"   height: ",height,","
    WRITE(66,"(a)")"   autosize: true,"
    WRITE(66,"(a)")"   autorange: true,"
    WRITE(66,"(a)")"   margin: {l: 65, r: 50, b: 65, t: 90}"
    WRITE(66,"(a)")"};"
    WRITE(66,"(a)")"Plotly.newPlot('Elatools',data,layout);"
    WRITE(66,"(a)")"</script>  " 
 END SUBROUTINE   
