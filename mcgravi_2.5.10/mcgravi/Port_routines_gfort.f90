module Portability_routines
! module de d'interface des fonctions non standards

!use IFPORT

contains

integer(4) function Rpos (string, substr) 
    ! Locates the index of the last occurrence of a substring within a string. 
    ! remplace result = RINDEX (string, substr)
    
    implicit none
    character (len=*) string, substr
    integer i,l,lsub
    Rpos = 0
    l = len_trim(string)
    !write(0,*)l
    lsub = len_trim(substr)
    !write(0,*)lsub
    if (lsub>l) return
    
    do i=l-lsub,0,-1
        !write(0,*)i,' ',string(i+1:i+lsub),' ',substr
        if (string(i+1:i+lsub)==substr) then
            Rpos = i+1
            return
        end if
    
    end do
    
    return
end function Rpos

integer(2) function run_system (filename, commandline) 
    implicit none
    character (len=*) :: filename, commandline
    integer(2) resultat
	character (len=255) :: cmd
	
	cmd = filename // "  " //commandline
    call system(cmd)
    !resultat = RUNQQ(filename, commandline)
    run_system = resultat
    return

end function run_system

logical(4) function run_system2 (commandline) 
	  implicit none
	  character (len=*) :: commandline
	  logical(4) resultat
      call system(commandline)
	  !resultat = SYSTEMQQ(commandline)
      run_system2 = resultat
      return

end function run_system2

logical(4) function mk_dir (rep) 
    implicit none
    character (len=*) :: rep
    logical(4) resultat
	character (len=255) :: cmd
    cmd = "md " // rep
	call system(cmd)
    !resultat = MAKEDIRQQ(rep)
    mk_dir = resultat
    return

end function mk_dir

CHARACTER (len=8) function Heure () 
    implicit none
    CHARACTER (len=8) HOUR
	
	CHARACTER (len=30) :: dt
	
	!dt = fdate()
    HOUR = "00000000"
	!HOUR = dt(12:20)
    heure = HOUR
    return

end function Heure

real*8 function HeureF () 
    implicit none
    real*8 h
	REAL :: tarray(2)
	integer ::  hh,mm,ss
	CHARACTER (len=30) :: dt
	character (len=2) :: shh, smm, sss
    
	!dt = fdate()
	shh = dt(12:13)
	smm = dt(15:16)
	sss = dt(18:19)

	shh = "00"
	smm = "00"
	sss = "00"
	
	!write(0,*)shh,' ',smm,' ',sss
	read(shh,'(I2)')hh
	read(smm,'(I2)')mm
	read(sss,'(I2)')ss
	!write(0,*)hh,mm,ss
	
	h = hh + mm / 60.0d0 + ss / 3600d0
    HeureF = h
	!write(0,*)h
    return

end function HeureF









end module Portability_routines