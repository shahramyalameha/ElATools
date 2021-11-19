!-------------------------------------------------------------------------------------------
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, -
!               Please report bugs or suggestions to:  yalameha93@gmail.com                -
!                                                                                          -
!-------------------------------------------------------------------------------------------
SUBROUTINE swin_web(namepro)
  IMPLICIT NONE
  ChARACTER(len=10)                :: namepro

 WRITE(66,"(a)")'<html><link type="text/css" rel="stylesheet" id="dark-mode-custom-link">'
  CALL system("cp /home/shahram/Desktop/Cubelast/code/programMY/AAEP/soc/eatools_v1.7.0/db/plotly.js .")
 if (namepro=="young"  )WRITE(66,"(3a)")"</style><title> Spatial dependence of Young's modulus </title>"
 if (namepro=="bulk"   )WRITE(66,"(3a)")"</style><title> Spatial dependence of Bulk's modulus </title>"
 if (namepro=="poisson")WRITE(66,"(3a)")"</style><title> Spatial dependence of Poisson's ratio </title>"
 if (namepro=="pugh"   )WRITE(66,"(3a)")"</style><title> Spatial dependence of Phug's ratio </title>"
 if (namepro=="shear"  )WRITE(66,"(3a)")"</style><title> Spatial dependence of Shear modulus </title>"
 if (namepro=="comp"   )WRITE(66,"(3a)")"</style><title> Spatial dependence of linear compressibility </title>"
 if (namepro=="hard"   )WRITE(66,"(3a)")"</style><title> Spatial dependence of Hardness </title>"
 if (namepro=="pp"     )WRITE(66,"(3a)")"</style><title> Spatial dependence of Phase-P velocity </title>"
 if (namepro=="pf"     )WRITE(66,"(3a)")"</style><title> Spatial dependence of Phase-Fast velocity </title>"
 if (namepro=="ps"     )WRITE(66,"(3a)")"</style><title> Spatial dependence of Phase-Slow velocity </title>"
 if (namepro=="pfp"    )WRITE(66,"(3a)")"</style><title> Spatial dependence of Power Flow angle_P-mode </title>"
 if (namepro=="pff"    )WRITE(66,"(3a)")"</style><title> Spatial dependence of Power Flow angle_Fast-mode </title>"
 if (namepro=="pfs"    )WRITE(66,"(3a)")"</style><title> Spatial dependence of Power Flow angle_Slow-mode </title>"
 if (namepro=="gp"     )WRITE(66,"(3a)")"</style><title> Spatial dependence of Group-P velocity </title>"
 if (namepro=="gf"     )WRITE(66,"(3a)")"</style><title> Spatial dependence of Group-Fast velocity </title>"
 if (namepro=="gs"     )WRITE(66,"(3a)")"</style><title> Spatial dependence of Group-Slow velocity </title>"
 if (namepro=="km"  )WRITE(66,"(3a)")"</style><title> Spatial dependence of of Min. thermal conductivity </title>" 
 !--------------------------------------------- 
  if (namepro=="young2d")WRITE(66,"(3a)")"</style><title> Orientation dependence of Young's modulus </title>" 
  if (namepro=="bulk2d" )WRITE(66,"(3a)")"</style><title> Orientation dependence of Bulk modulus </title>" 
  if (namepro=="pugh2d" )WRITE(66,"(3a)")"</style><title> Orientation dependence of Pugh's ratio </title>"  
  if (namepro=="shear2d")WRITE(66,"(3a)")"</style><title> Orientation dependence of Shear modulus </title>"  
  if (namepro=="com2d"  )WRITE(66,"(3a)")"</style><title> Orientation dependence of linear compressibility </title>"  
  if (namepro=="poi2d"  )WRITE(66,"(3a)")"</style><title> Orientation dependence of Poisson's ratio </title>"  
  if (namepro=="hard2d" )WRITE(66,"(3a)")"</style><title> Orientation dependence of Hardness </title>"  
  if (namepro=="pp2d"   )WRITE(66,"(3a)")"</style><title> Orientation dependence of Phase-P velocity </title>"  
  if (namepro=="ps2d"   )WRITE(66,"(3a)")"</style><title> Orientation dependence of Phase-Fast velocity </title>"  
  if (namepro=="pf2d"   )WRITE(66,"(3a)")"</style><title> Orientation dependence of Phase-Slow velocity </title>"  

  if (namepro=="gp2d"   )WRITE(66,"(3a)")"</style><title> Orientation dependence of Group-P velocity </title>"  
  if (namepro=="gs2d"   )WRITE(66,"(3a)")"</style><title> Orientation dependence of Group-Fast velocity </title>"  
  if (namepro=="gf2d"   )WRITE(66,"(3a)")"</style><title> Orientation dependence of Group-Slow velocity </title>" 
  
  if (namepro=="pfpd"   )WRITE(66,"(3a)")"</style><title> Orientation dependence of Power Flow angle_P-mode </title>"  
  if (namepro=="pffd"   )WRITE(66,"(3a)")"</style><title> Orientation dependence of Power Flow angle Fast-mode </title>"  
  if (namepro=="pfs2d"  )WRITE(66,"(3a)")"</style><title> Orientation dependence of Power Flow angle Slow-mode </title>"  
   if (namepro=="km2d"  )WRITE(66,"(3a)")"</style><title> Orientation dependence of Min. thermal conductivity </title>"  
 WRITE(66,"(a)")'<script src="plotly.js"></script>'
 WRITE(66,"(a)")'</style><style type="text/css">'
 WRITE(66,"(a)")'  @media print {'
 WRITE(66,"(a)")'    .ms-editor-squiggler {'
 WRITE(66,"(a)")'        display:none !important;'
 WRITE(66,"(a)")'    }	'
 WRITE(66,"(a)")'  }'
 WRITE(66,"(a)")'  .ms-editor-squiggler {'
 WRITE(66,"(a)")'    all: initial;'
 WRITE(66,"(a)")'   display: block !important;'
 WRITE(66,"(a)")'   height: 0px !important;'
 WRITE(66,"(a)")' }</style>'
 WRITE(66,"(a)")'</head>'
END SUBROUTINE
!===============================
