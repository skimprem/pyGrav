module sys_utils
! Module de fonctions systèmes :
! copy
! move
! mkdir
! rm


implicit none

contains

integer function copy(file1,file2)
    implicit none
    character (LEN=*) , intent(in) :: file1
    character (LEN=*) , intent(in) :: file2
    
    copy = copy2(file1,file2)

    return
end function copy 


integer function copy1(file1,file2)
    use Portability_routines
    implicit none
    character (LEN=*) , intent(in) :: file1
    character (LEN=*) , intent(in) :: file2
    logical(4) res
    logical(4) debug
    integer d,f,code
    debug = .false.
    
    open (99,file = 'copy.pl')
    Write(99,*)'use File::Copy;'
    call deb_fin(file1,d,f)
    write(99,*)'$file1 = "',file1(d:f),'";'
    call deb_fin(file2,d,f)
    write(99,*)'$file2 = "',file2(d:f),'";'
    write(99,*)'copy $file1, $file2;'
    close(99)
    res = run_system2 ('perl copy.pl')
    code = rm('copy.pl')
    copy1 = 0
    return
end function copy1


integer function copy2(file1,file2)
    use Portability_routines
    implicit none
    character (LEN=*) , intent(in) :: file1
    character (LEN=*) , intent(in) :: file2
    character (LEN=600) :: cmd
    integer d1,f1,d2, f2, code
    logical(4) res
    logical(4) debug
    
    debug = .false.
    call deb_fin(file1,d1,f1)
    call deb_fin(file2,d2,f2)
    cmd = 'perl -e "use File::Copy;copy('''//file1(d1:f1)//''','''//file2(d2:f2)//''');"'
   !if (debug) write(0,*) '>',file1,'<','>',file2,'<'
    if (debug) then
	call deb_fin(cmd,d1,f1)
        write(0,*) '>',cmd(d1:f1),'<'
    endif
    res = run_system2 (cmd)

    copy2 = 0
    return
end function copy2

integer function move(file1,file2)
    implicit none
    character (LEN=*) , intent(in) :: file1
    character (LEN=*) , intent(in) :: file2

    move = move2(file1,file2)
    
    return
end function move

integer function move1(file1,file2)
    use Portability_routines
    implicit none
    character (LEN=*) , intent(in) :: file1
    character (LEN=*) , intent(in) :: file2
    logical(4) res
    logical(4) debug
    integer d,f,code
    debug = .false.
    
    open (99,file = 'move.pl')
    Write(99,*)'use File::Copy;'
    call deb_fin(file1,d,f)
    write(99,*)'$file1 = "',file1(d:f),'";'
    call deb_fin(file2,d,f)
    write(99,*)'$file2 = "',file2(d:f),'";'
    write(99,*)'copy $file1, $file2;'
    close(99)
    res = run_system2 ('perl move.pl')
    code = rm('move.pl')    
    move1 = 0
    return
end function move1

integer function move2(file1,file2)
    use Portability_routines
    implicit none
    character (LEN=*) , intent(in) :: file1
    character (LEN=*) , intent(in) :: file2
    character (LEN=255)  :: cmd
    integer d, f, d1, f1, d2, f2
    logical(4) res
    logical(4) debug
    debug = .false.
    
    call deb_fin(file1,d1,f1)
    call deb_fin(file2,d2,f2)
    cmd = 'perl -e "use File::Copy;move('''//file1(d1:f1)//''','''//file2(d2:f2)//''');"'
    
    if (debug) then
	call deb_fin(cmd,d,f)
        write(0,*) '>',cmd(d:f),'<'
    endif
    
    res = run_system2 (cmd)     

    move2 = 0
    return
end function move2

integer function mkdir(dir)
    use Portability_routines
    implicit none
    character (LEN=*) , intent(in) :: dir
    character (LEN=500)  :: cmd
    logical(4) res
    logical(4) debug
    integer :: d,f
    debug = .false.
    
    call deb_fin(dir,d,f)
    cmd =  'perl -e "mkdir('''//dir(d:f)//''');"'
    
    if (debug) write(0,*) cmd
    res = run_system2 (cmd)     

    mkdir = 0
    return
end function mkdir

integer function rm(file1)
    use Portability_routines
    implicit none
    character (LEN=*) , intent(in) :: file1
    character (LEN=500)  :: cmd
    logical(4) res
    logical(4) debug
    integer :: d,f
    debug = .false.
    
    call deb_fin(file1,d,f)
    cmd =  'perl -e "unlink ('''//file1(d:f)//''');"'
    
    if (debug) write(0,*) cmd
    res = run_system2 (cmd)     

    rm = 0
    return
end function rm

subroutine deb_fin(str,d,f)
    implicit none
    character (LEN=*) , intent(in) :: str
    integer, INTENT(INOUT) :: d,f
    integer :: i,l
    character (LEN=500)  :: cmd
    logical(4) res
    logical(4) debug
    debug = .false.
    
    l = len(str)
    d = 1
    f = l   
    do i=1,l
        if (str(i:i) .eq. ' ') then
		d= i
        else
		exit
	endif
    end do
    
        do i=1,l
        if (str(l-i:l-i) .eq. ' ') then
		f= l-i-1
        else
		exit
	endif
    end do
   !write(0,*) d, f
    
   ! str = str(d:f)
    return
end subroutine deb_fin

end module sys_utils