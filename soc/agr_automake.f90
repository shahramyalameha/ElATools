  subroutine automakescal(tmajor_yy,Minimum_tot, Maximum_tot,nameinp)
   ChARACTER(30)      :: nameinp
   DOUBLE PRECISION           :: Minimum1, Maximum1,Minimum2, Maximum2,Minimum3, Maximum3 
   DOUBLE PRECISION           ::   tmajor_yy  ,Maximum_tot   ,Minimum_tot              ! max and min
   ChARACTER(len=10) :: val  
   DOUBLE PRECISION, dimension(364)  :: theta=0,x1=0,x2=0,x3=0, t1=0,t2=0
   integer           :: io1,io2,Count,i,j

  
    val=nameinp
 !------------------------------------  
   IF ( val=="poisson") then
    open(11, file="2dcut_poisson.dat")
     Do Count=0,360
      
      read(11,* ) theta(Count),x1(Count),x2(Count),x3(Count)
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
         
         Maximum3 =  x3(Count)                           
         Minimum3 =  x3(Count)                       
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
         
         IF ( x2(Count) > Maximum2)  Maximum2 = x2(Count)    
         IF ( x2(Count) < Minimum2)  Minimum2 = x2(Count)  
         
         IF ( x3(Count) > Maximum3)  Maximum3 = x3(Count)    
         IF ( x3(Count) < Minimum3)  Minimum3 = x3(Count)  
      END IF

     ENDDO
     close(11)
j=0
     t1(1) = Maximum1; t1(4) = Minimum1
     t1(2) = Maximum2; t1(5) = Minimum2
     t1(3) = Maximum3; t1(6) = Minimum3
     Do i=1, 6
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
    tmajor_yy= ( (Maximum_tot)- (Minimum_tot))
    write(*,*) "Max. and Min. of poissons", Maximum_tot, Minimum_tot 
                
endif 

!------------------------------------
   IF ( val=="young") then
    open(12, file="2dcut_young.dat")
     Do Count=0,360
 
      read(12,* ) theta(Count),x1(Count),x2(Count)
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
           
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
         
         IF ( x2(Count) > Maximum2)  Maximum2 = x2(Count)    
         IF ( x2(Count) < Minimum2)  Minimum2 = x2(Count)  
  
      END IF

     ENDDO
     close(12)
j=0
     t1(1) = Maximum1; t1(3) = Minimum1
     t1(2) = Maximum2; t1(4) = Minimum2
 
     Do i=1, 4
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
       tmajor_yy= ( (Maximum_tot)- (Minimum_tot))
    write(*,*) "Max. and Min. of young   ", Maximum_tot, Minimum_tot
endif 
 !------------------------------------
   IF ( val=="bulk") then
    open(12, file="2dcut_bulk.dat")
     Do Count=0,360
 
      read(12,* ) theta(Count),x1(Count),x2(Count)
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
           
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
         
         IF ( x2(Count) > Maximum2)  Maximum2 = x2(Count)    
         IF ( x2(Count) < Minimum2)  Minimum2 = x2(Count)  
  
      END IF

     ENDDO
     close(12)
j=0
     t1(1) = Maximum1; t1(3) = Minimum1
     t1(2) = Maximum2; t1(4) = Minimum2
 
     Do i=1, 4
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
       tmajor_yy= (NINT(Maximum_tot)-NINT(Minimum_tot))
    write(*,*) "Max. and Min. of bulk    ", Maximum_tot, Minimum_tot
endif
 !------------------------------------ 
   IF ( val=="shear") then
    open(11, file="2dcut_shear.dat")
     Do Count=0,360
      
      read(11,* ) theta(Count),x1(Count),x2(Count),x3(Count)
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
         
         Maximum3 =  x3(Count)                           
         Minimum3 =  x3(Count)                       
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
         
         IF ( x2(Count) > Maximum2)  Maximum2 = x2(Count)    
         IF ( x2(Count) < Minimum2)  Minimum2 = x2(Count)  
         
         IF ( x3(Count) > Maximum3)  Maximum3 = x3(Count)    
         IF ( x3(Count) < Minimum3)  Minimum3 = x3(Count)  
      END IF

     ENDDO
     close(11)
j=0
     t1(1) = Maximum1; t1(4) = Minimum1
     t1(2) = Maximum2; t1(5) = Minimum2
     t1(3) = Maximum3; t1(6) = Minimum3
     Do i=1, 6
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
       tmajor_yy= (NINT(Maximum_tot)-NINT(Minimum_tot))
    write(*,*) "Max. and Min. of shear   ", Maximum_tot, Minimum_tot
endif 

!------------------------------------ 
   IF ( val=="comp") then
    open(11, file="2dcut_comp.dat")
     Do Count=0,360
      
      read(11,* ) theta(Count),x1(Count),x2(Count) 
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
                        
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
         
         IF ( x2(Count) > Maximum2)  Maximum2 = x2(Count)    
         IF ( x2(Count) < Minimum2)  Minimum2 = x2(Count)  
  
      END IF

     ENDDO
     close(11)
j=0
     t1(1) = Maximum1; t1(3) = Minimum1
     t1(2) = Maximum2; t1(4) = Minimum2
 
     Do i=1, 4
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
       tmajor_yy= (NINT(Maximum_tot)-NINT(Minimum_tot))/2.0
    write(*,*) "Max. and Min. of comp    ", Maximum_tot, Minimum_tot
endif 
!------------------------------------  
   IF ( val=="pugh") then
    open(11, file="2dcut_pugh.dat")
     Do Count=0,360
      
      read(11,* ) theta(Count),x1(Count),x2(Count),x3(Count)
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
         
         Maximum3 =  x3(Count)                           
         Minimum3 =  x3(Count)                       
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
         
         IF ( x2(Count) > Maximum2)  Maximum2 = x2(Count)    
         IF ( x2(Count) < Minimum2)  Minimum2 = x2(Count)  
         
         IF ( x3(Count) > Maximum3)  Maximum3 = x3(Count)    
         IF ( x3(Count) < Minimum3)  Minimum3 = x3(Count)  
      END IF

     ENDDO
     close(11)
j=0
     t1(1) = Maximum1; t1(4) = Minimum1
     t1(2) = Maximum2; t1(5) = Minimum2
     t1(3) = Maximum3; t1(6) = Minimum3
     Do i=1, 6
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
       tmajor_yy= (NINT(Maximum_tot)-NINT(Minimum_tot))
    write(*,*) "Max. and Min. of pugh    ", Maximum_tot, Minimum_tot
endif 
!------------------------------------
   IF ( val=="hard") then
    open(12, file="2dcut_hardness.dat")
     Do Count=0,360
 
      read(12,* ) theta(Count),x1(Count),x2(Count)
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
           
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
         
         IF ( x2(Count) > Maximum2)  Maximum2 = x2(Count)    
         IF ( x2(Count) < Minimum2)  Minimum2 = x2(Count)  
  
      END IF

     ENDDO
     close(12)
j=0
     t1(1) = Maximum1; t1(3) = Minimum1
     t1(2) = Maximum2; t1(4) = Minimum2
 
     Do i=1, 4
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
       tmajor_yy= (NINT(Maximum_tot)-NINT(Minimum_tot))
    write(*,*) "Max. and Min. of hardness", Maximum_tot, Minimum_tot
endif
!------------------------------------ 
!------------------------------------
   IF ( val=="km") then
    open(12, file="2dcut_km.dat")
     Do Count=0,360
 
      read(12,* ) theta(Count),x1(Count) 
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
           
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
  
  
      END IF

     ENDDO
     close(12)
j=0
     t1(1) = Maximum1; t1(2) = Minimum1
 
 
     Do i=1, 2
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
       tmajor_yy= (NINT(Maximum_tot)-NINT(Minimum_tot))
    write(*,*) "Max. and Min. of conductivity", Maximum_tot, Minimum_tot
endif
!------------------------------------ 
!------------------------------------  
   IF ( val=="gall") then
    open(11, file="2dcut_gveloc.dat")
     Do Count=0,360
      
      read(11,* ) theta(Count),x1(Count),x2(Count),x3(Count)
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
         
         Maximum3 =  x3(Count)                           
         Minimum3 =  x3(Count)                       
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
         
         IF ( x2(Count) > Maximum2)  Maximum2 = x2(Count)    
         IF ( x2(Count) < Minimum2)  Minimum2 = x2(Count)  
         
         IF ( x3(Count) > Maximum3)  Maximum3 = x3(Count)    
         IF ( x3(Count) < Minimum3)  Minimum3 = x3(Count)  
      END IF

     ENDDO
     close(11)
j=0
     t1(1) = Maximum1; t1(4) = Minimum1
     t1(2) = Maximum2; t1(5) = Minimum2
     t1(3) = Maximum3; t1(6) = Minimum3
     Do i=1, 6
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
       tmajor_yy= (NINT(Maximum_tot)-NINT(Minimum_tot))
    write(*,*) "Max. and Min. of groups  ", Maximum_tot, Minimum_tot
endif 

!------------------------------------
   IF ( val=="pall") then
    open(11, file="2dcut_pveloc.dat")
     Do Count=0,360
      
      read(11,* ) theta(Count),x1(Count),x2(Count),x3(Count)
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
         
         Maximum3 =  x3(Count)                           
         Minimum3 =  x3(Count)                       
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
         
         IF ( x2(Count) > Maximum2)  Maximum2 = x2(Count)    
         IF ( x2(Count) < Minimum2)  Minimum2 = x2(Count)  
         
         IF ( x3(Count) > Maximum3)  Maximum3 = x3(Count)    
         IF ( x3(Count) < Minimum3)  Minimum3 = x3(Count)  
      END IF

     ENDDO
     close(11)
j=0
     t1(1) = Maximum1; t1(4) = Minimum1
     t1(2) = Maximum2; t1(5) = Minimum2
     t1(3) = Maximum3; t1(6) = Minimum3
     Do i=1, 6
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
       tmajor_yy= (NINT(Maximum_tot)-NINT(Minimum_tot))
    write(*,*) "Max. and Min. of phases  ", Maximum_tot, Minimum_tot
endif 

!----------------------------------------------------------------------
!------------------------------------
   IF ( val=="pfall") then
    open(11, file="2dcut_pfaveloc.dat")
     Do Count=0,360
      
      read(11,* ) theta(Count),x1(Count),x2(Count),x3(Count)
 
 
      IF (Count == 1) THEN                         
         Maximum1 =  x1(Count)                           
         Minimum1 =  x1(Count)
           
         Maximum2 =  x2(Count)                           
         Minimum2 =  x2(Count)
         
         Maximum3 =  x3(Count)                           
         Minimum3 =  x3(Count)                       
      ELSE                                         
         IF ( x1(Count) > Maximum1)  Maximum1 = x1(Count)    
         IF ( x1(Count) < Minimum1)  Minimum1 = x1(Count)     
         
         IF ( x2(Count) > Maximum2)  Maximum2 = x2(Count)    
         IF ( x2(Count) < Minimum2)  Minimum2 = x2(Count)  
         
         IF ( x3(Count) > Maximum3)  Maximum3 = x3(Count)    
         IF ( x3(Count) < Minimum3)  Minimum3 = x3(Count)  
      END IF

     ENDDO
     close(11)
j=0
     t1(1) = Maximum1; t1(4) = Minimum1
     t1(2) = Maximum2; t1(5) = Minimum2
     t1(3) = Maximum3; t1(6) = Minimum3
     Do i=1, 6
       if (t1(i) .NE. 0) then 
        j=j+1
        t2(j)=t1(i)
       endif
     enddo
     Do i=1,j
           IF (i == 1) THEN                         
         Maximum_tot =  t2(i)                           
         Minimum_tot =  t2(i)
                        
      ELSE                                         
         IF ( t2(i) > Maximum_tot)  Maximum_tot = abs(t2(i))  
         IF ( t2(i) < Minimum_tot)  Minimum_tot = abs(t2(i))      
   endif
   enddo
       tmajor_yy= (NINT(Maximum_tot)-NINT(Minimum_tot))
    write(*,*) "Max. and Min. of power flow angle  ", Maximum_tot, Minimum_tot
endif 

!----------------------------------------------------------------------
end subroutine 
    
   
    
   
   
   
   
   
   
   
   
