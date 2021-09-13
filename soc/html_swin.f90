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
 if (namepro=="young")WRITE(66,"(3a)")"</style><title> Spatial dependence of Young's modulus </title>"
 if (namepro=="bulk")WRITE(66,"(3a)")"</style><title> Spatial dependence of Bulk's modulus </title>"
 if (namepro=="poisson")WRITE(66,"(3a)")"</style><title> Spatial dependence of Poisson's ratio </title>"
 if (namepro=="shear")WRITE(66,"(3a)")"</style><title> Spatial dependence of shear modulus </title>"
 if (namepro=="comp")WRITE(66,"(3a)")"</style><title> Spatial dependence of linear compressibility </title>"
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
