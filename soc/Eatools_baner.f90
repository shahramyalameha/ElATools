Subroutine call_thD_code(without_color)
CHARACTER(LEN=1) :: without_color

    WRITE(*,*)" > Select the desired method (code, file and databank):"
   IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 33;tput bold; echo "==============================================================";tput sgr0')
   IF (without_color .eq. "N") CALL SYSTEM('echo "=============================================================="')
    !WRITE(*,*)"============================================================" 
    WRITE(*,*)" IRelast-----------------------(       WEIN2k         )-=> 1"
    WRITE(*,*)" Elast-------------------------(       WEIN2k         )-=> 2"
    WRITE(*,*)" AELAS-------------------------(        VASP          )-=> 3"
    WRITE(*,*)" ElaStic-----------------------(  QE,WEIN2k,Exciting  )-=> 4"
    WRITE(*,*)" Using Cij Tensor in Cij.dat---(     Other codes      )-=> 5" 
    WRITE(*,*)" Using EC Databank-------------(      Offline MP      )-=> 6"
    WRITE(*,*)" Using EC Databank-------------(      Online  MP      )-=> 7"
    WRITE(*,*)" Using OUTCAR------------------(     VASP Output      )-=> 8"    
    WRITE(*,*)" Back --------------------------------------------------=> 0"
    !WRITE(*,*)"============================================================" 
    IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 33;tput bold; echo "=============================================================";tput sgr0') 
    IF (without_color .eq. "N") CALL SYSTEM('echo "============================================================="')
END subroutine 

Subroutine call_thD_bulkmethod(without_color)
CHARACTER(LEN=1) :: without_color
    WRITE(*,*)" > Select the desired method to calculate the Bulk modulus:"
   IF (without_color .eq. "Y")  CALL SYSTEM('tput setaf 12;tput bold; echo "==============================================================";tput sgr0')
   IF (without_color .eq. "N")  CALL SYSTEM('echo "=============================================================="')
    !WRITE(*,*)"============================================================" 
    WRITE(*,*)" Method I.  B=1/beta ------------------------------------=> 1"
    WRITE(*,*)" Method II. B=1/(3I_v' S d_v) ---------------------------=> 2"
    WRITE(*,*)" Back ---------------------------------------------------=> 0"
    !WRITE(*,*)"============================================================"
    IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 12;tput bold; echo "==============================================================";tput sgr0') 
    IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 1;tput bold; echo "> Note: Be careful in using Method I;";tput sgr0')
    IF (without_color .eq. "Y")  CALL SYSTEM('tput setaf 1;tput bold; echo "        Method II is recommended in most applications.";tput sgr0')
    
    IF (without_color .eq. "N") CALL SYSTEM('echo "=============================================================="') 
    IF (without_color .eq. "N") CALL SYSTEM('echo "> Note: Be careful in using Method I;"')
    IF (without_color .eq. "N")  CALL SYSTEM('echo "        Method II is recommended in most applications."')    
END subroutine 

Subroutine call_thD_Hardnessmethod(without_color)
CHARACTER(LEN=1) :: without_color
    CALL SYSTEM('clear')
    WRITE(*,*)" > Select the desired modele to calculate the Vickers Hardness:"
    IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 20;tput bold; echo "==============================================";tput sgr0')
    IF (without_color .eq. "N") CALL SYSTEM('echo "=============================================="')
    !WRITE(*,*)"=============================================" 
    WRITE(*,*)" Mazhnik's model -------------------------=> 1"
    WRITE(*,*)" Chen's model ----------------------------=> 2"
    WRITE(*,*)" Tian's model ----------------------------=> 3"
    WRITE(*,*)" Back ------------------------------------=> 0"
    !WRITE(*,*)"============================================="
    IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 20;tput bold; echo "==============================================";tput sgr0') 
    IF (without_color .eq. "N") CALL SYSTEM('echo "=============================================="')
END subroutine    

Subroutine call_thD_thermalmethod(without_color)
CHARACTER(LEN=1) :: without_color
        WRITE(99,*) " > Calculate elastic wave properties: On"
        WRITE(*,*)" > Select the desired option:"
        IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 63;tput bold; echo "================================================================";tput sgr0')
        IF (without_color .eq. "V") CALL SYSTEM('echo "================================================================"')
        WRITE(*,"(a)")" Phase, group and PFA without Min. thermal conductivity-----=> 1"
        WRITE(*,"(a)")" Phase, group and PFA  with   Min. thermal conductivity-----=> 2"
        WRITE(*,"(a)")" Back ------------------------------------------------------=> 0"
        IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 63;tput bold; echo "================================================================";tput sgr0')
        IF (without_color .eq. "N") CALL SYSTEM('echo "================================================================"')
END subroutine    
 
Subroutine call_unstable(without_color) 
CHARACTER(LEN=1) :: without_color
      IF (without_color .eq. "Y") CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
      IF (without_color .eq. "Y") CALL system ('tput setaf 9;tput bold; echo " > Elastic Stability Conditions:  Unstable; STOP";tput sgr0')
      IF (without_color .eq. "Y") CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0') 
      
      IF (without_color .eq. "N") CALL system ('echo " ======================================="')
      IF (without_color .eq. "N") CALL system ('echo " > Elastic Stability Conditions:  Unstable; STOP"')
      IF (without_color .eq. "N") CALL system ('echo " ======================================="')       
END subroutine    
 
Subroutine call_stable(without_color)  
CHARACTER(LEN=1) :: without_color
      IF (without_color .eq. "Y") CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
      IF (without_color .eq. "Y") CALL system ('tput setaf 10;tput bold; echo " > Elastic Stability Conditions:  Stable";tput sgr0')
      IF (without_color .eq. "Y") CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
      
      IF (without_color .eq. "N") CALL system ('echo " ======================================="')
      IF (without_color .eq. "N") CALL system ('echo " > Elastic Stability Conditions:  Stable"')
      IF (without_color .eq. "N") CALL system ('echo " ======================================="')       
END subroutine   

Subroutine call_twD_code(without_color)
CHARACTER(LEN=1) :: without_color
      WRITE(*,*)" > Select the desired method (code or file):"
      IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 12;tput bold; echo " =====================================================";tput sgr0')
      IF (without_color .eq. "N") CALL SYSTEM('echo " ====================================================="')      
      !WRITE(*,*)"====================================================="
      WRITE(*,*)" AELAS                            (    VASP   ) => 1"
      WRITE(*,*)" IRelast for 2D                   (   WIEN2K  ) => 2"    
      WRITE(*,*)" Using Cij Tensor in Cij-2D.dat   (other codes) => 3"
      WRITE(*,*)" Back ----------------------------------------- => 0"    
      !WRITE(*,*)"====================================================="
      IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 12;tput bold; echo " =====================================================";tput sgr0')
      IF (without_color .eq. "N") CALL SYSTEM('echo " ====================================================="')

END subroutine     

Subroutine call_twD_sysmethod(without_color)
CHARACTER(LEN=1) :: without_color
        WRITE(*,*)" > Select the type of two-dimensional system:"    
        IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 41;tput bold; echo " ====================================================";tput sgr0')  
        IF (without_color .eq. "N") CALL SYSTEM('echo " ===================================================="')
        WRITE(*,*) " Default  option (Hex., Squ., and Rec. systems) => 1  "
        WRITE(*,*) " Advanced option (       Oblique systems      ) => 2  "
        WRITE(*,*) " Back ------------------------------------------=> 0  "      
        IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 41;tput bold; echo " ====================================================";tput sgr0') 
        IF (without_color .eq. "N") CALL SYSTEM('echo " ===================================================="') 
END subroutine   

Subroutine call_oneD_code(without_color)
CHARACTER(LEN=1) :: without_color
      WRITE(*,*)" > Select the desired method (code or file):"
      IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 12;tput bold; echo " =====================================================";tput sgr0')
      IF (without_color .eq. "N") CALL SYSTEM('echo " ====================================================="')
      !WRITE(*,*)"====================================================="  
      WRITE(*,*)" Using Cij Tensor in Cij-1D.dat (e.g. Nanotube) => 1"
      WRITE(*,*)" Manual input of Cij Tensor                     => 2"      
      WRITE(*,*)" Back ----------------------------------------- => 0"    
      !WRITE(*,*)"====================================================="
      IF (without_color .eq. "Y") CALL SYSTEM('tput setaf 12;tput bold; echo " =====================================================";tput sgr0')
      IF (without_color .eq. "N") CALL SYSTEM('echo " ====================================================="')

END subroutine  
