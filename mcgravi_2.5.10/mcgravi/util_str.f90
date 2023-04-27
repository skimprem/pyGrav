module util_str
! utilitaires de traitement de chaines

implicit none

contains


integer function char2int(c)
    !converti un caractère en entier
    !renvoie -1 si ce n'est pas un chiffre

    implicit none
    character*1::c
    integer::ascval

    ascval=IACHAR(c)
    if ((ascval<48) .or. (ascval>57)) then
        char2int = -1
        RETURN
    end if
    char2int = ascval - 48

    return
end function char2int

real*8 function str2double(str)
    ! converti une chaine en double
    ! procedure destinéé à remplacer DNUM qui n'existe pas ssous g95

    implicit none
    character (len=*)::str
    integer :: n
    
    101 format (g16.8) 
    102 format (I8)
    
    str2double = -1.0
    
    if (index(str,'.')>0) then
        read(str,101)str2double
    else
        read(str,102)n
        str2double = DBLE(n)
    endif

    return
end function str2double

character (len=1) function int2char(n)
    !converti un entier <10 en un caractère
 
    implicit none
    integer n 
    
    if ((n>=0) .and. (n<10)) then
        int2char(1:1) = char(48+n)
    else  
        int2char(1:1) = ''
    end if   

    return
end function int2char

logical function StrIsNumber(c)
    ! Strisnumber prend une chaine en paramètre et renvoie true si elle représente un nombre
    ! Utilisée pour éviter les erreurs à l'execution lors de la lecture des observations

    implicit none
    character (LEN=*) c
    integer::i,long,ascval,ndot
    character*1::car
    c=trim(c)
    long=len_trim(c)

    !write(0,*)c
    !write(0,*)'longueur =',long
    StrisNumber=.true.
    ndot=0
    car=c(1:1)
    !write(0,*)car
    ascval=IACHAR(car)
    if ( ((ascval.GE.48) .and. (ascval.Le.57))  .or. (car=='+') .or. (car=='-')) then
    ! le premier caractère peut être - ou + ou un chiffre
    else
        StrisNumber=.false.
        RETURN
    end if

    do i=2,long
        car=c(i:i)
        ascval=IACHAR(car)
        if (((ascval.GE.48) .and. (ascval.LE.57)) .or. (car=='.')) then
            if (car=='.') then
                ndot=ndot+1
            end if
        else
            StrisNumber=.false.
            RETURN
        end if
    end do

    if (ndot>1) then
        StrisNumber=.false.
        RETURN
    end if

    StrisNumber=.true.
    return
end function StrIsNumber


subroutine decoupe_ligne(ligne,tabligne,nchamps)
    ! Procedure visant à recupérer sous forme de tableau de chaines
    ! les différents champs lus dans une chaine

    ! J. BEILIN - juin 2004
    ! adaptation en f90 d'une procedure Delphi de Comp3D (Y.EGELS)+ ajout de quelques protections

    ! Les champs sont donnés en format libre séparés par des blancs ou des tabulations
    ! Le séparateur décimal est '.' ou ','
    ! * et # : ligne désactivée
    ! on autorise 20 champs au maximum

    implicit none
    character (len=*) ligne
    character (len=1) c
    integer nchamps,long,i,ndifblanc,j
    CHARACTER (len=*) tabligne(20)
    logical(4) blankline
    
    nchamps=0
    blankline=.true.
    
    if (len_trim(ligne)==0)	then
	    return !chaine vide
    end	if
    
    ! nettoyage de la chaine (blancs, tabulations, séparateur décimal...
    ! transformation des tabulations en espaces
    do i=1,len_trim(ligne)
        if (IACHAR(ligne(i:i))==9) then
            ligne(i:i)=' '
        end if
        if (ligne(i:i)==',') then
            ligne(i:i)='.'
        end if
    end do
    
    do i=1,len_trim(ligne)
        if (ligne(i:i)/=' ') then
            blankline=.false.
        end if
    end do
    
    if (blankline) then
    !if (blankline==.true.) then

	    return !ligne désactivée
    end	if
    
    ! bloc bonux on vire les blancs en  fin et début de chaine
    do while ( (ligne(len_trim(ligne):len_trim(ligne))==' ') .and. (len_trim(ligne).GT.0) )
        ligne = ligne(1:len_trim(ligne)-1)
    end do

    do while ( (ligne(1:1)==' ') .and. (len_trim(ligne).GT.0) )
        ligne = ligne(2:len_trim(ligne))
    end do
    
    ! on vire toutes les '*' hormis en début de ligne
    ! utile pour les fichiers bruts du scintrex
    ! 499.8975* veut dire mesure corrigée de l'inclinaison
    do i=2,len_trim(ligne)  
        if (ligne(i:i)=='*') then
            ligne(i:i)=' '
        end if
    end do


    if ((ligne(1:1)=='*') .or. (ligne(1:1)=='#')) then
	    return !ligne désactivée
    end	if  

    ndifblanc=0
    j=0
    long=len_trim(ligne)+1
    ligne=ligne//' ' ! on ajoute un blanc pour récupérer le dernier champs

    ! principe de recherche :
    ! on cherche le prochain séparateur et on copie la chaine jusque là
    do i=1,long
        if (ligne(i:i)/=' ') then
            ndifblanc=ndifblanc+1
            if (ndifblanc==1) then
                j=i
            end if
        else if (ndifblanc/=0) then
            nchamps=nchamps+1
            if (nchamps>20) then
                nchamps = 0
                return
            end if
            tabligne(nchamps)=ligne(j:j+ndifblanc)
            ndifblanc=0
        end if
    end do
end subroutine decoupe_ligne

subroutine verif_nomfic(nomfic)

    character (len=255), intent(inout):: nomfic

    character (len=255) nom
    integer n 
    nom = nomfic
    n = len_trim(nom)
    
    if (nom(1:1)=='"' .and. nom(n:n)=='"') then
        nomfic = nom(2:n-1)
        n = n-2
    end if
    
end subroutine verif_nomfic

integer function nomfic2jour(nomfic)
    !use IFPORT
    use Portability_routines
    
    implicit none

    character (len=*), intent(in):: nomfic

    character (len=255) nom,s_annee,s_jour
    integer n,p, annee, mjd
    nom = trim(nomfic)
    n = len_trim(nom)
    
    p = Rpos (nom,".") 
    !p = RINDEX (nom,".") 

    if (p/=9) then
        nomfic2jour = 0
        return
    end if
    
    s_annee = nom(p-2:p-1)
    s_jour = nom(p+1:p+3)
        
    if (strisnumber(s_annee) .and. strisnumber(s_jour)) then
        read(s_annee,'(I8)')annee
        !annee = JNUM(s_annee)
        
        read(s_jour,'(I8)')mjd
        !mjd = JNUM(s_jour)
    else
        nomfic2jour = 0
        return      
    end if
    
    if (annee<50) then
        annee = annee + 2000
    else
        annee = annee + 1900
    end if  
    
    nomfic2jour = annee * 1000 + mjd  
    return
    
end function nomfic2jour

subroutine get_rep(nomfic,rep)
! routine qui sépare un nom de fichier de son nom de répertoire 

    character (len=255), intent(inout):: nomfic
    character (len=255), intent(out):: rep

    character (len=255) nom
    integer n,i 
    nom = nomfic
    n = len_trim(nom)
    
    i = index(nom,'\',BACK=.true.)
    if (i>0 .and. i<n) then
        rep = nom(1:i)
        nomfic = nom(i+1:len_trim(nom)) 
    else 
        rep = ''
    end if

end subroutine



end module util_str