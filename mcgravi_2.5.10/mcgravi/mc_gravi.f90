program MC_gravi
! programme principal d'appel à MCgravi

use mcgravi_main
use ecriture
use Portability_routines

implicit none

character (len=80),parameter :: titreA = 'Least squares software for absolute and relative gravity measurements'
character (len=70),parameter :: titreF = 'Compensation des observations de gravimetrie absolue et relative'

! variables de gestion des paramètres en entree
character (len=80) line
character (len=255) nomfic
integer  nargs
integer code,w
character lg
logical resultat
LOGICAL exists
CHARACTER (len=5) systeme

lg = 'F'

systeme = OS_NAME_POSIX()
write(0,*)systeme


!write(0,*)'MC_gravi'
write(0,*)'------------------------------------------------------------------------------'
write(0,*)''
if (lg=='F') then 
write(0,*)titreF 
else 
write(0,*)titreA 
end if
write(0,*)'Beilin Jacques - IPGP/IGN'
write(0,*)''
write(0,*)version()
write(0,*)''
write(0,*)'------------------------------------------------------------------------------'

! Lecture des paramètres en ligne de commande ***********************
write(0,*)''

nargs=iargc()
if(nargs/=1) then
    call W_error
    stop
end if

call getarg(1,nomfic)


INQUIRE (FILE = nomfic, EXIST = exists)

IF (.NOT. exists) THEN
    WRITE(0,*) 'Fichier introuvable'
    stop
END IF

write(0,*)''

code = MCgravi(nomfic)
write(0,*)'Code resultat : ',code

end program MC_gravi

