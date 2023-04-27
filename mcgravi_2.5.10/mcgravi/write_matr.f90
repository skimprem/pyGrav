module write_matr
! Export texte des matrices du calcul (déboggage)

implicit none

contains

subroutine write_mat(A,nl,nc,nomfic)
    ! Procedure d'export des matrices de calcul vers des fichiers textes
    use param_data

    implicit none

    integer:: i, j, nl, nc, len_nomfic,len_dossier
    character (len=*) nomfic

    character (len=255) s
    real*8::A(nl,nc)
    real*8::val
    
    nomfic=trim(nomfic)
    write(0,*)'Writing ',nomfic
    open (150,file=nomfic)
    !write(0,*)s
    write(150,*)nl,' lignes ',nc,' colonnes'

    do i=1,nl
        write(150,*)'ligne = ',i
        do j=1,nc
            !val = int(1000*A(i,j))/1000
            val = A(i,j)
            write(150,*)val
        end do

    end do
    close(150)
    
    return
end subroutine write_mat

subroutine write_vec(A,nl,nomfic)
    ! Procedure d'export des matrices de calcul vers des fichiers textes
    use param_data

    implicit none

    integer:: i, j, nl, len_nomfic,len_dossier
    character (len=*) nomfic

    character (len=255) s
    real*8::A(nl)
    real*8::val
    
    nomfic=trim(nomfic)
    write(0,*)'Writing ',nomfic
    open (150,file=nomfic)
    write(150,*)nl,' lignes '

    do i=1,nl
        val = A(i)
        write(150,*)'ligne = ',i, val
    end do
    
    close(150)
    
    return
end subroutine write_vec

subroutine write_mat_Scilab(A,nl,nc,nomfic)
    ! Procedure d'export des matrices de calcul vers des fichiers textes

    implicit none
    integer::i,j,nl,nc
    character (len=*) nomfic
    character (len=255) :: ligne
    real*8::A(nl,nc)
    real*8::val
    open (150,file=nomfic)

    nomfic=trim(nomfic)
    write(0,*)'Writing ',nomfic

    write(150,*)'//',nl,' lignes ',nc,' colonnes'
    
    write(150,*)nomfic,'=['
    do i=1,nl 
        write(150,*)'ligne = ',i
        do j=1,nc
            !val = int(1000*A(i,j))/1000
            val = A(i,j)
            !ligne  = ligne//val
            write(150,*)val
        end do

    end do
    close(150)
    
    return
end subroutine write_mat_Scilab

end module
