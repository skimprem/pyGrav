module Portability_routines
! module de d'interface des fonctions non standards

use IFPORT

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
    
    resultat = RUNQQ(filename, commandline)
    run_system = resultat
    return

end function run_system

logical(4) function run_system2 (commandline) 
    implicit none
    character (len=*) :: commandline
    logical(4) resultat
    
    resultat = SYSTEMQQ(commandline)
    run_system2 = resultat
    return

end function run_system2

logical(4) function mk_dir (rep) 
    implicit none
    character (len=*) :: rep
    logical(4) resultat
    
    resultat = MAKEDIRQQ(rep)
    mk_dir = resultat
    return

end function mk_dir

CHARACTER (len=8) function Heure () 
    implicit none
    CHARACTER (len=8) HOUR
    
    HOUR = "      "
    CALL TIME (HOUR)
    heure = HOUR
    return

end function Heure

real*8 function HeureF () 
    implicit none
    real*8 h
    
    h = 0.0d0
    HeureF = h
    HeureF = TIMEF ()
    return

end function HeureF

CHARACTER (len=5) function OS_NAME () 
    implicit none
    CHARACTER (len=5) OS
    character (len=25) :: commandline
    logical(4) resultat
    LOGICAL exists
    character (len=255) line
    OS = 'WINNT'
	! OS = 'LINUX'

    commandline = 'uname > os.txt'
    resultat = run_system2(commandline)
    
    INQUIRE (FILE = 'os.txt', EXIST = exists)
    IF (exists) THEN
      open(20,file='os.txt')
      read(20,'(A255)')line
      
      if (line .EQ. 'Linux') OS = 'LINUX'
      if (line .EQ. 'WindowsNT') OS = 'WINNT'
      close(20)
    END IF

    OS_NAME = OS
    return

end function OS_NAME

CHARACTER (len=5) function OS_NAME_POSIX () 
    implicit none
    CHARACTER (len=5) OS
    character (len=25) :: commandline
    logical(4) resultat
    LOGICAL exists
    character (len=255) line
    OS = 'WINNT'
    
    open (150,file='uname.pl')
    write(150,*)'use POSIX;'
    write(150,*)'my ($sysname, $nodename, $release, $version, $machine ) = POSIX::uname();'
    write(150,*)'open (FIC,">os.txt");'
    write(150,*)'print FIC "$sysname\n";'
    write(150,*)'close(FIC);'
    close(150)
    
    commandline = 'perl uname.pl'
    resultat = run_system2(commandline)
    
    INQUIRE (FILE = 'os.txt', EXIST = exists)
    IF (exists) THEN
      open(20,file='os.txt')
      read(20,'(A255)')line
      
      if (line .EQ. 'Linux') OS = 'LINUX'
      if (line .EQ. 'Windows NT') OS = 'WINNT'
      close(20)
    END IF

    OS_NAME_POSIX = OS
    return

end function OS_NAME_POSIX



end module Portability_routines
