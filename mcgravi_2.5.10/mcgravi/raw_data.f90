module raw_data
! Definition et stockage des observations brutes

implicit none

integer, parameter :: datablock=100 
! Les tableaux sont alloués par blocs de 'datablock' enregistrements
! Si on se rend compte qu'on dépasse la taille limite on copie les éléments 
! dans un autre tableau et on réalloue un "datablock" de plus

real*8, parameter :: SD_min=0.000001
! On n'autorise pas ds sd < 1e-6 mgal
  
character (len=1) lg 

real*8 pi

! variable de calibration des gravimètres *************************************
type Tgravi
    character (len=8) Serial
    logical Estimate ! calibration estimée ou non 
    integer pos ! position de l'inconnue 
    integer Num
    character(len=1) N !indice du gravi ; destiné à remplacer Num
    real*8 Cf
end type Tgravi

type (Tgravi), allocatable, dimension(:) :: TabGravi
integer  ngravimeter

! variables de definition des observations absolues ***************************
type TObsAbs
    character (len=8) nomsta
    integer numsta
    real*8 grav
    real*8 SD
end type TObsAbs

type (TObsAbs) ObsAbs
type (TObsAbs), allocatable, dimension(:) :: TabObsAbs

! variables de lecture des observations dans les fichiers *********************
logical SD_too_Small

! Observations absolues (ancienne méthode -> à changer) ***********************
CHARACTER*8,allocatable,dimension(:)::fixstn,fixstn2
real*8,allocatable,dimension(:)::fixgra,stdx,fixgra2,stdx2
integer, allocatable, dimension(:)::pos, fichier, fichier2

! Observations absolues ******************************************************   
type Tobs_A10
    character (len=8) nomsta
    real*8 grav,sd
    integer numsta   
end type Tobs_A10

type (Tobs_A10) obs_A10
type (Tobs_A10) ,allocatable,dimension(:)::TabObsA10,TabObsA102
integer nTabObsA10

! Observation relative individuelle *******************************************
type Tobs 
    character (len=8) nomsta
    real*8 grav,sd,tempK,mjd,Cf,h
    ! le champ h sert à gérer la hauteur d'instrument provenant du fichier "s"
    integer numsta,date,heure,profil
end type Tobs 
type (Tobs) obs, obs_AR, obs_AV   
type (Tobs) ,allocatable,dimension(:)::TabObs,TabObs2 
! TabObs2 sert de tableau temporaire si on doit réallouer de la mémoire à TabObs
! Il en est de même pour les autres tableaux numérotés '2'
integer nTabObs
    
! observations relatives ******************************************************   
type Tobs_rel
    character (len=8) nomsta_AV, nomsta_AR
    real*8 grav_AR,grav_AV,sd_AR,sd_AV,tempK_AR,tempK_AV,mjd_AR,mjd_AV,Cf,mjd0,temp_K0
    integer numsta_AR,numsta_AV,profil   
end type Tobs_rel

type (Tobs_rel) obs_rel
type (Tobs_Rel) ,allocatable,dimension(:)::TabObsRel,TabObsRel2
integer nTabObsRel

! Liste des profils ***********************************************************
! utile pour donner le numéro du gravi avec les termes de dérive
Type Tprofil
    integer num
    integer posInc
    integer jour
    character (len=40) nomfic
    character (len=8) Serial
    character (len=1) Ngravi 
end type Tprofil

type (Tprofil) , allocatable, dimension(:):: TabProfil
integer Nb_profil 

! position des stations dans la liste des stations
integer,allocatable,dimension(:)::stp,enp


! Stations ********************************************************************   
type TStation
    character (len=8) nomsta
    real*8 lon, lat
    integer numsta   
end type TStation

type (TStation) Station
type (TStation) ,allocatable,dimension(:)::TabStation,TabStation2
integer NTabStation

! liste des stations
CHARACTER*8,allocatable,dimension(:)::stat,sta


 



end module raw_data
