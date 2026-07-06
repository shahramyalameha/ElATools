!-------------------------------------------------------------------------------------------
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, -
!               Please report bugs or suggestions to:  yalameha93@gmail.com                -
!                                                                                          -
!-------------------------------------------------------------------------------------------
SUBROUTINE swin_web(namepro)
  IMPLICIT NONE
  ChARACTER(len=10)                :: namepro

! WRITE(66,"(a)")'<html><link type="text/css" rel="stylesheet" id="dark-mode-custom-link">'
  CALL system("cp /home/shahram/Desktop/Cubelast/code/programMY/AAEP/soc/eatools_v1.7.0/db/gnu_pal//plotly.js .")
WRITE(66,"(a)")'<!DOCTYPE html>'
WRITE(66,"(a)")'<html lang="en">'
WRITE(66,"(a)")'<head>'
WRITE(66,"(a)")'    <meta charset="UTF-8">'
WRITE(66,"(a)")'    <meta name="viewport" content="width=device-width, initial-scale=1.0">'
  
! if (namepro=="young")WRITE(66,"(3a)")"</style><title> Spatial dependence of Young's modulus </title>"
! if (namepro=="bulk")WRITE(66,"(3a)")"</style><title> Spatial dependence of Bulk's modulus </title>"
! if (namepro=="poisson")WRITE(66,"(3a)")"</style><title> Spatial dependence of Poisson's ratio </title>"
! if (namepro=="shear")WRITE(66,"(3a)")"</style><title> Spatial dependence of shear modulus </title>"
! if (namepro=="comp")WRITE(66,"(3a)")"</style><title> Spatial dependence of linear compressibility </title>"
 
 if (namepro=="young")WRITE(66,"(3a)")"<title> Spatial dependence of Young's modulus </title>"
 if (namepro=="bulk")WRITE(66,"(3a)")"<title> Spatial dependence of Bulk's modulus </title>"
 if (namepro=="poisson")WRITE(66,"(3a)")"<title> Spatial dependence of Poisson's ratio </title>"
 if (namepro=="shear")WRITE(66,"(3a)")"<title> Spatial dependence of shear modulus </title>"
 if (namepro=="comp")WRITE(66,"(3a)")"<title> Spatial dependence of linear compressibility </title>" 

WRITE(66,"(a)")'    <script src="plotly.js"></script>'
WRITE(66,"(a)")'    <style type="text/css">'
WRITE(66,"(a)")'        body {'
WRITE(66,"(a)")'            margin: 0;'
WRITE(66,"(a)")'            padding: 20px;'
WRITE(66,"(a)")"            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;"
WRITE(66,"(a)")'            background-color: #121212;'
WRITE(66,"(a)")'            display: flex;'
WRITE(66,"(a)")'            flex-direction: column;'
WRITE(66,"(a)")'            align-items: center;'
WRITE(66,"(a)")'        }'
WRITE(66,"(a)")'        h2 {'
WRITE(66,"(a)")'            color: #ffffff;'
WRITE(66,"(a)")'            margin-bottom: 20px;'
WRITE(66,"(a)")'        }'
WRITE(66,"(a)")'        /* Modern container with box-shadow */'
WRITE(66,"(a)")'        .plot-container {'
WRITE(66,"(a)")'            width: 100%;'
WRITE(66,"(a)")'            max-width: 900px;'
WRITE(66,"(a)")'            background-color: #1e1e1e;'
WRITE(66,"(a)")'            border-radius: 12px;'
WRITE(66,"(a)")'            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.5);'
WRITE(66,"(a)")'            padding: 20px;'
WRITE(66,"(a)")'            box-sizing: border-box;'
WRITE(66,"(a)")'        }'
WRITE(66,"(a)")'        @media print {'
WRITE(66,"(a)")'            .ms-editor-squiggler {'
WRITE(66,"(a)")'                display: none !important;'
WRITE(66,"(a)")'            }	'
WRITE(66,"(a)")'        }'
WRITE(66,"(a)")'        .ms-editor-squiggler {'
WRITE(66,"(a)")'            all: initial;'
WRITE(66,"(a)")'            display: block !important;'
WRITE(66,"(a)")'            height: 0px !important;'
WRITE(66,"(a)")'        }'
WRITE(66,"(a)")'    </style>'
WRITE(66,"(a)")'</head>'
WRITE(66,"(a)")'<body>'
 
! WRITE(66,"(a)")'<script src="plotly.js"></script>'
! WRITE(66,"(a)")'</style><style type="text/css">'
! WRITE(66,"(a)")'  @media print {'
! WRITE(66,"(a)")'    .ms-editor-squiggler {'
! WRITE(66,"(a)")'        display:none !important;'
! WRITE(66,"(a)")'    }	'
! WRITE(66,"(a)")'  }'
! WRITE(66,"(a)")'  .ms-editor-squiggler {'
! WRITE(66,"(a)")'    all: initial;'
! WRITE(66,"(a)")'   display: block !important;'
! WRITE(66,"(a)")'   height: 0px !important;'
! WRITE(66,"(a)")' }</style>'
! WRITE(66,"(a)")'</head>'
END SUBROUTINE
!===============================

