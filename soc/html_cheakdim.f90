SUBROUTINE CheckDimension(sysdim)
  character(255) :: directory
  character(100)  :: filename1, filename2, sysdim
  LOGICAL        :: status1, status2
  ! Specify the directory path
  directory = "./" ! Replace with the actual path

  ! Specify the filenames to check for
  filename1 = "2dcut_young.dat"
  filename2 = "3d_young.dat"

  ! Check if the files exist
  inquire(file=trim(filename1), exist=status1)
  inquire(file=trim(filename2), exist=status2)
 ! write(*,*)status1,status2
  ! Output based on file existence
  if ((status1 .eqv. .true.) .and. (status2 .eqv. .true.)) then
      sysdim ="3D"
  else if ((status1 .eqv. .false.) .and. (status2 .eqv. .false.)) then
      sysdim ="2D"
 
  end if
 
end SUBROUTINE CheckDimension

 
