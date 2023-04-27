module Portability_routines
! module de d'interface des fonctions non standards

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
    character (len=*),  INtENT(IN) :: filename, commandline
    integer(2) resultat    
    character (len=255) :: cmd
    !write(0,*) '>>',commandline,'<<'
    cmd = filename // "  " //commandline
    call system(cmd)
    run_system = resultat
    return

end function run_system

logical(4) function run_system2 (commandline)     
    implicit none    
    character (len=*) ,  INtENT(IN) :: commandline    
    logical(4) resultat
    !write(0,*) '>>',commandline,'<<'
    call system(commandline)
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
    mk_dir = resultat
    return

end function mk_dir

CHARACTER (len=8) function Heure () 
    implicit none
    CHARACTER (len=8) HOUR        
    CHARACTER (len=30) :: dt        
    dt = fdate()
    HOUR = dt(12:20)
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
        dt = fdate()    
	shh = dt(12:13)    
	smm = dt(15:16)    
	sss = dt(18:19)    !write(0,*)shh,' ',smm,' ',sss    read(shh,'(I2)')hh    read(smm,'(I2)')mm    read(sss,'(I2)')ss    !write(0,*)hh,mm,ss        h = hh + mm / 60.0d0 + ss / 3600d0
    HeureF = h    !write(0,*)h
    return

end function HeureF


 character (len=5) function OS_NAME () 
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
    
    if (resultat ) then 
      OS = 'LINUX'
    endif
    
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
    write(150,*)'#!/usr/bin/perl'
    write(150,*)'use POSIX;'    
    write(150,*)'unlink "os.txt";'
    write(150,*)'my ($sysname, $nodename, $release, $version, $machine ) = POSIX::uname();'    
    !write(150,*)'#print " $sysname, $nodename, $release, $version, $machine\n";'
    write(150,*)'if ($sysname =~ m/cygwin/i) {$sysname = "LINUX"}'    
    write(150,*)'elsif ($sysname =~ m/linux/i) {$sysname = "LINUX"}'
    write(150,*)'elsif ($sysname =~ m/windows/i) {$sysname = "WINNT"}'
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
      
      if (line .EQ. 'LINUX') OS = 'LINUX'
      if (line .EQ. 'WINNT') OS = 'WINNT'
      close(20)
    END IF

    OS_NAME_POSIX = OS
    return

end function OS_NAME_POSIX

end module Portability_routines
