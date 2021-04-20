!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the sound calculation..

SUBROUTINE Csound(vec,v11,v12,v22,v13,v23,v33,EVa,Lm,MaxTm,MinTm)
		DOUBLE PRECISION, DIMENSION(3,3) :: DM=0D0,EVe=0D0
		DOUBLE PRECISION, DIMENSION(6,6) :: S,C
		DOUBLE PRECISION, DIMENSION(3)   :: EVa,longMod= 0,vec
		DOUBLE PRECISION                 :: MaxlongMod = 0,v11,v12,v22,v13,v23,v33
		INTEGER, DIMENSION(2)            :: tm
		INTEGER                          :: lm,MinTm,MaxTm,i1=0,j1=0

	OPEN(61,FILE="Cij.dat")
	READ(61,*) C(1,1),C(1,2),C(1,3),C(1,4),C(1,5),C(1,6)
	READ(61,*) C(2,1),C(2,2),C(2,3),C(2,4),C(2,5),C(2,6)
	READ(61,*) C(3,1),C(3,2),C(3,3),C(3,4),C(3,5),C(3,6)
	READ(61,*) C(4,1),C(4,2),C(4,3),C(4,4),C(4,5),C(4,6)
	READ(61,*) C(5,1),C(5,2),C(5,3),C(5,4),C(5,5),C(5,6)
	READ(61,*) C(6,1),C(6,2),C(6,3),C(6,4),C(6,5),C(6,6)
	CLOSE(61)

		DM(1,1)=C(1,1)*v11            &
		       +C(6,6)*v22            &
		       +C(5,5)*v33            &
		       +2D0*( C(1,6)*v12+C(1,5)*v13+C(5,6)*v23 )

		DM(2,2)= C(6,6)*v11           &
		         +C(2,2)*v22          &
		         +C(4,4)*v33          &
		         +2D0*( C(2,6)*v12+C(4,6)*v13+C(2,4)*v23 )

		DM(3,3)=C(5,5)*v11            &
		         +C(4,4)*v22          &
		         +C(3,3)*v33          &
		         +2D0*( C(4,5)*v12+C(3,5)*v13+C(3,4)*v23 ) 
		    
		DM(1,2)=C(1,6)*v11            &
		       +C(2,6)*v22            &
		       +C(4,5)*v33            &
		       +( C(1,2)+C(6,6) )*v12 &
		       +( C(1,4)+C(5,6) )*v13 &
		       +( C(2,5)+C(2,6) )*v23

		DM(2,1)=DM(1,2)

		DM(1,3)=C(1,5)*v11            &
		       +C(4,6)*v22            &
		       +C(3,5)*v33            &
		       +( C(1,4)+C(5,6) )*v12 &
		       +( C(1,3)+C(5,5) )*v13 &
		       +( C(3,6)+C(4,5) )*v23

		DM(3,1)=DM(1,3)

		DM(2,3)=C(5,6)*v11            &
		       +C(2,4)*v22            &
		       +C(3,4)*v33            &
		       +( C(2,5)+C(4,6) )*v12 &
		       +( C(3,6)+C(4,5) )*v13 &
		       +( C(2,3)+C(4,4) )*v23

		DM(3,2)=DM(2,3)

		CALL DSYEVH3(DM,EVe,EVa)

		DO i1=1,3
		  longMod(i1)=0D0
		  Do j1=1,3
		    longMod(i1)=longMod(i1)+vec(j1)*EVe(j1,i1)
		  ENDDO
		 longMod(i1)=ABS(longMod(i1))
		ENDDO
		!
		!
		MaxlongMod=0
		 
		Do i1=1,2
		  Do j1=i1+1,3
		    IF ( ( longMod(i1) .GT.  longMod(j1) ) .AND. ( longMod(i1)  .GT.  MaxlongMod ) ) THEN
		     lM=i1
		     MaxlongMod=longMod(i1)
		    ENDIF
		    IF ( ( longMod(i1) .LT.  longMod(j1) ) .AND. ( longMod(j1)  .GT.  MaxlongMod ) ) THEN
		     lM=j1
		     MaxlongMod=longMod(j1)
		    ENDIF
		  ENDDO
		ENDDO
		!
		!
		j1=0

		Do i1=1,3
		  IF (i1 .NE. lM) THEN
		    j1=j1+1
		    tm(j1)=i1
		  ENDIF
		ENDDO    
		IF ( EVa(tm(1)) .GT. EVa(tm(2)) ) THEN
		  MaxTm=(tm(1))
		  MinTm=(tm(2))
		  else
		  MaxTm=(tm(2))
		  MinTm=(tm(1))    
		ENDIF
 C=0
end
