module MC_data
! Module de definition et de stockage des variables des moindres carrés. 
implicit none

type Tmc
    integer Nb_obsRel
    integer Nb_obsAbs
    integer Nb_sta
    integer Nb_inc
    integer Nb_profil
    integer ngravi_Cal
    integer degre_k
    integer degre_t
    integer dof
    real*8  sigma0
    integer NIterMax
    real*8  crit_khi2 ! valeur critique du test du khi2
    real*8  crit_tau  ! valeur critique du tau test
    
    character (len=8),pointer,dimension(:)      :: stat ! liste des stations observées
    integer, pointer, dimension(:)              :: stp  ! indice du point de départ pour chaque obs relative
    integer, pointer, dimension(:)              :: enp  ! indice du point d'arrivée pour chaque obs relative
    integer, pointer, dimension(:)              :: pos  ! indice du point concerné pour chaque obs absolue

    real*8, pointer, dimension(:,:)             :: A    ! matrice modèle
    real*8, pointer, dimension(:)               :: sig  ! variance de l'estimateur
    real*8, pointer, dimension(:,:)             :: Sx   ! inverse de la matrice normale
    real*8, pointer, dimension(:,:)             :: AtPA ! matrice normale
    real*8, pointer, dimension(:)               :: AtPB ! 
    real*8, pointer, dimension(:)               :: B    ! Vecteurs des observations
    real*8, pointer, dimension(:)               :: P    ! vecteur des poids
    real*8, pointer, dimension(:)               :: X    ! estimateur des MC
    real*8, pointer, dimension(:)               :: X0   ! Valeurs approchées
    real*8, pointer, dimension(:)               :: V    ! vecteur des résidus
    
    real*8, pointer, dimension(:)               :: sol_sans_calib    ! solution sans estimation de calibration
    
    real*8,dimension(18)                        :: histo ! histogramme des résidus 18 classes
    ! 16 classes de 0.5 de large de -4sigma à 4 sigma et deux classes de -inf à -4s et +4s à +inf
    
    real*8 vmax !Résidu maximum
    real*8 vmin !Résidu minimum
    real*8 vmean !Résidu moyen
    real*8 vrms !RMS des résidus
    
    ! inconnues de gravité
    real*8 sgmax ! sigma max 
    real*8 sgmin ! sigma min
    real*8 sgmean ! sigma moyen
    real*8 sgrms ! rms
    
    ! inconnues de gravité
    real*8 sdmax ! sigma max 
    real*8 sdmin ! sigma min
    real*8 sdmean ! sigma moyen
    real*8 sdrms ! rms
       
end type Tmc

type (Tmc) MCc ! moindres carrés contraints
type (Tmc) MCf ! moindres carrés libres

! Tableau utilisé par le module statistique pour gerer plus facilement les tests statistiques
Type TResid
    integer             profil      ! numéro du fichier profil
    real*8              obs         ! observation
    real*8              resid       ! residu
    real*8              SD_ini      ! SD a priori
    real*8              Sd_fin      ! SD a posteriori
    real*8              Norm_res    ! residu normalise
    real*8              Std_res     ! residu standard
    character (len=8)   ini         ! Point initial 
    character (len=8)   fin         ! Point final
    logical             abs         ! vrai si obs absolue, faux si obs relative
    logical             tautest     ! vrai si passe, faux sinon

end Type TResid

type (Tresid) ,  allocatable , dimension(:) :: TabResid,TabResid_trie


end module MC_data
