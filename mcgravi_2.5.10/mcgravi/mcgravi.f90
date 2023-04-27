module mcgravi_main
!
! Least squares software for absolute and relative gravity measurements
! Author : Beilin Jacques, june 2004
! Calls some gravnet routines (C Hwang) (calculation subroutines)
!
! Software distributed under GNU GENERAL PUBLIC LICENSE
!
! Last update: 02, 2006, by JBL
!
! NAME
!	MCgravi - adjust relative ands absolute gravity measurements and estimate gravities and
!             gravimeter parameters, in FORTRAN 90
!
!
! CONVENTION OF FILE UNIT
! 10-59: input files
! 60-99: output files
!
!
! HISTORIQUE DES VERSIONS

! 1.0.11 30/06/2004 :   première version fonctionnelle.
! 1.0.12 01/07/2004 :   correction du signe des paramètres de dérives dans mat modèle
! 1.0.13 02/07/2004 :   verification de la valeur du SD sur les obs relatives (>0.000001 mgal). 
!                       Pour les observations absolues, 0 vaut dire point fixé.
! 1.0.14 05/07/2004 :   Ajout de la virgule en séparateur décimal
!                       Les lignes avec seulement des blancs ou des tabulations ne font plus planter le programme   
!                       Dessin d'un histogramme des résidus (tourné d'un quart de tour)
! 1.0.15 05/07/2004 :   uh renommé en sigma0, u2h renommé en sigma0_2
!                       Controle d'allocation avant de désallouer TabObsRel
! 1.0.16 06/07/2004 :   Déplacement des procedures de lecture dans util_str.f90
!                       Paramètres de calcule lus dans un fichier texte lu par Read_config_file (lect_param.f90)
!                       Paramètres de calcul stockés dans une structure de type TParam
!                       Ecriture des matrices déplacée vers write_matr.f90
!                       Procedures d'inversion déplacées vers MC_inversion.f90
!                       Procédures statistiques déplacées vers MC_statistic.f90
! 1.0.17 07/07/2004 :   Modification de l'histogramme des résidus normalisés (type geolab)
! 1.0.18 07/07/2004 :   Modifications mineures (formats de chaines)
! 1.0.19 07/07/2004 :   Création d'une procedure statistique dans MC_statistique.f90
! 1.0.20 07/07/2004 :   Déplacement des procedures freenet et constraint vers MC_inversion.f90

! 1.1.1 08/07/2004  :   Changement au niveau des modes de calcul
!                       1 = libre (calculé avec solution contrainte avec 1 point fixé)suppression de freenet
!                       2 = contraint 
! 1.1.2 08/07/2004  :   Refonte de la construction de la matrice modèle et de la matrice normale
!                       La construction de ces matrices est sortie de la procedure constraint
! 1.1.3 08/07/2004  :   Création d'un module ecriture pour generer le fichier lst
!                       nouveau mode de calcul : on calcule d'abord une solution libre
!                       puis une solution contrainte. On peut ainsi comparer les résultats plus rapidement
!                       Code "pas très propre". A suivre...

! 1.2.1 09/07/2004  :   Ajout de l'heure du calcul dans le fichier résultat
!                       Restructuration des données (non fonctionnel) 
! 1.2.2 09/07/2004  :   Ajout d'une structure Tmc contenant toutes les matrices des MC
!                       Ajout d'un module contenant les données brutes (raw_data)
! 1.2.3 09/07/2004  :   On tient compte des résidus sur les observations absolues dans l'histogramme
! 1.2.4 10/07/2004  :   Lecture des données en entrée déplacée vers lect_raw_data
!                       Restructuration des données en entrée (tableaux dynamiques de structures)
! 1.2.5 11/07/2004  :   Nettoyage dans les variables 
! 1.2.6 11/07/2004  :   Implémentation d'un import depuis le scintrex (manque fonction de tri 
! 1.2.7 12/07/2004  :   Ajout du tri dans lect_fic_o 
! 1.2.8 12/07/2004  :   Petites améliorations dans le fichier résultat 
!                       Ajout d'un tableau de structures Tresid pour simplifier les calculs statistiques. 
!                       Possibilité d'afficher l'histogramme des résidus standards ou des résidus normalisés
! 1.2.9 13/07/2004  :   la valeur lue de la gravité dans le fichier c est maintenant la valeur brute (col 2) 
!                       + ETC (col 9).
!                       Serial du gravimetre fourni avec les dérives
! 1.2.10 13/07/2004 :   Ajout d'une liste des 20 plus gros résidus (standards ou normalisés) 
! 1.2.11 15/07/2004 :   Correction d'un bug de lecture des fichiers "o" (il faut diviser les SD par dur^0.5 
!                       pour être cohérent avec les SD des fichiers "c"  
!                       Correction d'un bug de lecture de l'heure dans les fichiers "o"  
!                       Ajout d'un fichier de sortie contenant les gravités compensées
! 1.2.12 16/07/2004 :   Recupération du jour pour les fichiers "o" à partir de mjd et de l'année
!                       Ajout d'un écart-type SIGMA_ADD en plus du bruit sismique fourni par les SD 
!                       Ecriture des observation en face des résidus (aide pour retouver les observations problèmatiques)
!                       Correction d'un bug dans le calcul des variances sur les résidus dans le cas d'obs absolues
!                       Correction d'un bug dans le calcul des résidus mini et maxi
!                       Séparation des RMS gravi et dérive dans MC_statistic
!
! 1.3.1 19/07/2004  :   Refonte de l'ecriture des matrices des moindres carrés
!                       Implémentation du calcul de simulations
! 1.3.2 19/07/2004  :   Le code principal est isolé dans un module à part (MC_gravi)
!                       Les paramètres et la procedure de lecture sont isolés.
! 1.3.3 22/07/2004  :   MCgravi et de nombreuses sub transformées en functions pour la remontee des codes d'erreur
!                       Remplacement de tous les STOP par des return MCgravi  
!                       le fichier de configuration accepte maintenant les chemins relatifs ou absolus 
!                       pour les noms de fichiers. Les espaces dans les noms des répertoires ne sont pas tolérés. 
! 1.3.4 10/08/2004  :   Passage en dll
!                       desallocation avant d'allouer (pour pouvoir appeler la dll plusieurs fois)
!                       sd_too_small activé
! 1.3.5 11/08/2004  :   version de deboggage de la dll (plantage sur le calcul stat)
! 1.3.6 13/08/2004  :   bug calcul stat corrigé (écart-type négatif sur le point fixé en solution libre
!                       cette obs n'est plus prise en compte dans le calcul stat
! 1.3.7 26/08/2004  :   correction d'une mauvaise initialisation de la variable SD_too_small
! 1.3.8 08/09/2004  :   allongement des noms de fichiers à 255 caractères
! 1.3.9 13/09/2004  :   Problème de débordement de pile lié aux fonctions intrinsèques MATMUL et TRANSPOSE
! 1.3.10 15/09/2004 :   Valeurs par defaut de certaines variables (noms de fichiers...)

! 1.4.1  26/05/2005 :   Et c'est reparti ! 
!                       Intégration des données du gravimètre A10
! 1.4.2  30/05/2005 :   Intégration des fichiers de site CG3TOOL
! 1.4.3  01/06/2005 :   fixation de bugs mineurs
!                       Suppression du dernier MatMul (fait planter sur les grosses matrices)
! 1.4.4  03/06/2005 :   Allocation dynamique du tableau de fichiers relatifs (on n'est plus limité à 100 profils)
!                       Modification du calcul de l'estimateur de la variance sur les résidus non activé dans cette version
! 1.4.5  03/06/2005 :   Modification du calcul de l'estimateur de la variance sur les résidus
!                       Procedure solution dupliquée pour tester l'allocation dynamique du vecteur local work
! 1.4.6  06/06/2005 :   desactivation du calcul de la variance sur les résidus si on choisi résidus normalisés
!                       Reste un bug dans le calcul de la variance de V (matrice non définie positive)
!
!
! 2.0.1  06/06/2005 :   Debut de developpement d'une nouvelle version avec estimation de la calibration
!                       des gravimètres relatifs
!                       Lecture d'un champ supplémentaire dans le fichier de calibration [Y/N]
!                       ajout d'un paramètre : nombre de gravis dont on souhaite estimer la calibration
! 2.0.2  06/06/2005 :   Modification du calcul statistique
! 2.0.3  08/06/2005 :   2ème modification du calcul statistique suite à problème d'inversion de VarV
! 2.0.3  08/06/2005 :   Protection - les itération ne sont lancées que si param.NgraviCal>0
! 2.0.4  08/06/2005 :   Modification de la création du modèle de l'estimation 
! 2.0.5  09/06/2005 :   Modification du stockage des noms de fichiers d'entree
! 2.0.6  09/06/2005 :   Prise en compte des sigma_f et sigma_a par fichier
! 2.0.7  13/06/2005 :   Ecriture des résultats apres estimation de la calibration
!                       Lancement de statistique
! 2.0.8  13/06/2005 :   Normalisation par enrichissement
! 2.0.9  13/06/2005 :   Normalisation par enrichissement (debut de la version avec calibration)
!                       Correction d'un bug si on n'estime pas toutes les calibrations
! 2.0.10 14/06/2005 :   Normalisation par enrichissement (version avec calibration)
! 2.0.11 15/06/2005 :   Ecriture des fichiers résultats dans un sous-dossier

! 2.1.1  15/06/2005 :   Ajout d'un module d'export GMT (The Generic Mapping Tools)
!                       lecture des fichiers de points
! 2.1.2  15/06/2005 :   Lecture des coordonnées 
! 2.1.3  16/06/2005 :   Lecture des coordonnées 
! 2.1.4  16/06/2005 :   Dessin GMT des écarts-type 
! 2.1.5  17/06/2005 :   l'impression du tau-test devient facultative 
! 2.1.6  20/06/2005 :   suppression de qq bugs
! 2.1.7  21/06/2005 :   dessin du reseau avant d'inverser
!                       correction d'un bug dans la détermination de la position des inconnues de calibration
! 2.1.8  22/06/2005 :   pb calibration quand cf/=1.0 en entrée
! 2.1.9  22/06/2005 :   pb calibration quand cf/=1.0 en entrée corrigé pour la normalisation pousse la brouette
! 2.1.10 22/06/2005 :   pb calibration quand cf/=1.0 en entrée corrigé pour normalisation par enrichissement
!                       Correction d'un bug dans l'ecriture des résidus (tau-test) dans le fichier listing
! 2.1.11 30/06/2005 :   Modifications mineures dans le listingresultat = run_system2 ('move /Y hist2.png '//param%dossier)
! 2.1.12 30/06/2005 :   Modifications mineures dans les sorties ecran

! 2.2.1  01/07/2005 :   Listing HTML
! 2.2.2  08/07/2005 :   Listing HTML
! 2.2.3  11/07/2005 :   Listing HTML
! 2.2.4  20/07/2005 :   Histogramme GMT
! 2.2.5  20/07/2005 :   Histogramme GMT
! 2.2.6  21/07/2005 :   fichier html reordonné pour mode 2 (reste à faire mode 1 et 3
!                       tau test et synthese passés dans MC_data
! 2.2.7  13/08/2005 :   Correction d'un problème d'écriture du test du khi2
! 2.2.8  13/09/2005 :   Paramétrage du contenu du fichier HTML
! 2.2.9  13/09/2005 :   Echelle sous GMT
! 2.2.10 21/09/2005 :   affichage du numéro du profil avec les résidus
! 2.2.11 26/09/2005 :   affichage du sigma0 à l'écran
! 2.2.12 26/09/2005 :   portage des scripts GMT sous perl pour rendre ces scripts indépendant de la plateformepb enrichissement
! 2.2.13 27/09/2005 :   correction site non obligatoire avec fichier c
!                       problème sur l'affichage des residus normalisés sous gmt
! 2.2.14 27/09/2005 :   correction site lue dans fichier c
! 2.2.15 29/09/2005 :   pb export gmt mode 1

! 2.3.1  01/10/2005 :   abandon dll 
! 2.3.2  01/10/2005 :   portabilité g95 
! 2.3.3  01/10/2005 :   portabilité g95 
! 2.3.4  01/10/2005 :   portabilité g95  lect_rawdata.f90
! 2.3.5  01/10/2005 :   portabilité g95  rempli_matMC.f90
! 2.3.6  01/10/2005 :   portabilité g95  MC_inversion.f90
! 2.3.7  01/10/2005 :   portabilité g95  MC_simulation.f90
! 2.3.8  05/10/2005 :   portabilité g95  ecriture.f90 reste à écrite la compilation conditionelle
! 2.3.9  08/10/2005 :   portabilité g95  procédures RUNQQ, SYSYEMQQ et MAKEDIRQQ isolées dans portability routines
!                       pour supprimer les appels directs à IFPORT                         
! 2.3.10 08/10/2005 :   portabilité g95  gmt.f90
! 2.3.11 08/10/2005 :   portabilité g95  gmt.f90 (fin) et MC-statistic.f90
! 2.3.12 08/10/2005 :   portabilité g95  MCGRAVI.f90 et MC_gravi.f90 
!                       résultats numériques identiques à la version 2.2.15
!                       code entièrement porté sauf reste RUNQQ, SYSYEMQQ et MAKEDIRQQ, TIME et TIMEF (Port_routines.f90)
! 2.3.13 08/10/2005 :   routines de portabilité 
! 				        en intel fortran, le module est port_routine.f90
!				        en g95, le module est  port_routine_g95.f90 (makefile modifié en conséquence)
! 				        reste à gérer le temps de calcul
! 2.3.14 08/10/2005 :   temps de calcul géré sous g95
! 2.3.15 07/11/2005 :   problème de paramétrage de region sous GMT
! 2.3.16 14/01/2006 :   re problème de paramétrage de region sous GMT
!                       problème non resolu. Les limites de region doivent être données à 0.1° près (valeurs entières mal interprétées)
! 2.3.17 14/01/2006 :   le numéro de gravi est maintenant une chaine de 1 caractère pour permettre la calib de + de gravis
! 2.3.18 14/01/2006 :   differentiation des gravis par numéro de série et (j'insiste) lettre dans le nom de fichier CG3TOOL
!                       ceci a pour but de permettre l'estimation d'une calibration par an et par instrument
! 2.4.1  06/02/2006 :   2 paramètres pour gérer l'affichage des résidus
!                       WRITE_RESID pour demander l'affichage des résidus
!                       WRITE_TAU pour n'afficher que les résidus qui ne passent pas le tau-test
! 2.4.2  07/02/2006 :   encapsulation de la procedure de conversion character vers double pour palier l'abscence de DNUM sous g95 (util_str.f90 - str2double)
!                       permet en particulier d'éviter des problèmes lorsqu'un flottant est fourni sans partie décimale dans un fichier d'entrée
! 2.4.3  15/05/2006 :   portage des scripts GMT sous perl pour rendre ces scripts indépendant de la plateforme
! 2.4.4  18/05/2006 :   ajouts de cartes GMT (residus non normalises)
! 2.4.5  18/05/2006 :   ajouts de cartes GMT (tautest)
! 2.4.6  19/05/2006 :   ajouts de cartes GMT (tautest)
! 2.4.7  19/05/2006 :   suppression des fichiers gmtdefaults
! 2.4.8  19/05/2006 :   test real*16 mais abandon pour cause de débordement
! 2.4.9  20/06/2006 :   correction d'un bug dans la desallocation puis réallocation dans Normalise_cal (rempli_matMC.f90)
!                       ajout d'une boucle de 0 à 1E6 pour ralentir la CPU entre les deux actions.
! 2.4.10 12/07/2006 :   ajout d'une option (MEASURE_UNIT cm) dans GMT.f90
! 2.4.11 14/08/2006 :   detection automatique du système
! 2.4.12 10/10/2006 :   detection automatique du système par perl POSIX
! 2.5.1  19/04/2007 :   ajout d'un module dans lect_param pour lire les fichiers 'r'
!                       ajouts de différents messages d'erreurs lors d'un problème de lecture
! 2.5.2  23/05/2007 :   Création d'un module permettant d'écrire des fichiers r artificiellement après un calcul libre.
!                       Ces fichiers r peuvent être utilisés par la suite pour un autre calcul contraint.
! 2.5.3  21/06/2007 :   Ajout d'un module pour réduire les séries de mesures consécutives des fichiers 'c' en une seule mesure.
!                       Cette réduction a pour but d'éviter que des calculs de dérives sur des dt très courts viennent polluer le résultat final.
! 2.5.4  25/06/2007 :   Le calcul de la matrice Qll dans la procédure calc_resid comportait des erreurs qui ont été corrigée.
! 2.5.5  08/01/2008 :   Les fichiers projects du A10 comportent manitenant la lettre µ pour µGal (au lieu de uGal)
! 2.5.6  09/01/2008 :   J'ai 33 ans, ça vaut bien une nouvelle version
!                       Bon, en fait, elle sert surtout à régler quelques problème de compatibilié g95
! 2.5.7  11/01/2008 :   version test pour assurer le compatibilité avec cygwin
! 2.5.8  13/01/2008 :   fonction systèmes (copy, move, rm, mkdir) portées en perl pour éviter les soucis systèmes
!                       ajout dun module sys_utils.f90 pour efectuer ces tâches
! 2.5.9  14/01/2008 :   modifications mineures dans sys_utils.f90
! 2.5.10 14/01/2008 :   élimitation des fuites de mémoire dans la partie MC
!
!
! note ifort : Le compilateur initialise mal les variables. Dans ce cas, il faut aller dans Projet\Propriétés de MC Gravi\Propriétés
! de configuration\Fortran\Data\Local Variable Storage\ et sélectionner : All variables SAVE

contains

character(len=40) function VERSION()

    implicit none
    
    ! Numéro de version du logiciel ***********************************************
    
    character (len=40),parameter :: num_version='mcgravi 2.5.10 14/01/2008'
    
    ! *****************************************************************************
    
    VERSION = num_version

    return
end function VERSION

integer function MCGRAVI(nomfic_conf)

use str_const
use sys_utils
use raw_data
use lect_rawdata
use MC_data
use ecriture 
use util_str
use param_data
use lect_param
use write_matr
use write_html
use rempli_matMC
use MC_inversion
use MC_statistic
use GMT
use Portability_routines

implicit none

character (len=255) f_result1,f_result2,f_result3,f_result4,ch,s
character (len=255) nomfic_conf ! nom du fichier de paramètres
character (len=50) f_result5
integer code,ls,i,iter
integer d,f
character (len=2) ch2

logical(4) resultat
integer(2) resultatI

real*8 sigma00
REAL(8) elapsed_time
elapsed_time = heureF()

lg = 'F'

!ch = 'Debut du programme'
!call W_time(ch)
write(0,*)''

if (lg=='F') then ; write(0,*)readconfF ; else ; write(0,*)readconfA ; end if
ch = 'Lecture des fichiers'
call W_time(ch)

code = Read_config_file(nomfic_conf,param)
if (code == 101) then
    if (lg=='F') then ; write(0,*)invalidconfF ; else ; write(0,*)invalidconfA ; end if
    call W_code_erreur(code)
    MCgravi = code 
    return
end if

resultat = Cree_dossier_resultat()

Nb_profil=param%ntabnomficrel
param%Nb_obsAbs=0
ls=len_trim(param%nomficout)
f_result1=''
f_result2=''
f_result3=''
f_result4=''

f_result1(1:4+ls) = param%nomficout(1:ls)//'.lst'
f_result2(1:6+ls) = param%nomficout(1:ls)//'_f.gra'
f_result3(1:6+ls) = param%nomficout(1:ls)//'_c.gra'
f_result4(1:4+ls) = param%nomficout(1:ls)//'.htm'

call deb_fin(f_result1,d,f)
!write(0,*)f_result1,d,f
open(67,file=f_result1(d:f)) ! Fichier de sortie
call deb_fin(f_result4,d,f)
!write(0,*)f_result4,d,f
open(70,file=f_result4(d:f)) ! Fichier de sortie html


call W_entete(version())

! Lecture du fichier de calibration des gravimètres ***************************
if (param%calf) then
    if (lg=='F') then ; write(0,*)readcalF ; else ; write(0,*)readcalA ; end if
    code = lect_ficCal_gravi(param%nomficcal)
    if (code == 102) then
        if (lg=='F') then ; write(0,*)invalidcalF ; else ; write(0,*)invalidcalA ; end if
        call W_code_erreur(code)
        MCgravi = code 
        return   
    end if 
    call W_cal_gravi()
    
end if

call W_sigma_coeff()

call  W_liste_nomficrel()
call  W_liste_nomficabs()
call  W_liste_nomficA10()


! Lecture des observations absolues *******************************************
param%Nb_obsAbs=0
if(param%lfix) then
    if (lg=='F') then ; write(0,*)readabsF ; else ; write(0,*)readabsA ; end if
    code = lect_obsAbs()
    if (code == 103) then
        if (lg=='F') then ; write(0,*)invalidabsF ; else ; write(0,*)invalidabsA ; end if
        call W_code_erreur(code)
        MCgravi = code 
        return
    end if 
    
    code = lect_obsA10()
    if (code == 103) then
        if (lg=='F') then ; write(0,*)invalidabsF ; else ; write(0,*)invalidabsA ; end if
        call W_code_erreur(code)
        MCgravi = code 
        return
    end if

    if (allocated(pos)) deallocate(pos)
    allocate (pos(param%Nb_obsAbs)) 
     
end if

!write(0,*)'Nb obs A10 : ',param%Nb_obsAbs

! Lectures des observations relatives *****************************************
Ntabobs=0	!nombre d'observations
if (lg=='F') then ; write(0,*)readrelF ; else ; write(0,*)readrelA ; end if
code = lect_obsrel()
if (code /= 0) then
    if (lg=='F') then ; write(0,*)invalidrelF ; else ; write(0,*)invalidrelA ; end if
    call W_code_erreur(code)
    MCgravi = code 
    return
end if 

ch = 'Organisation des donnees'
call W_time(ch)


! Recherche des stations ******************************************************
code = cherche_station()
if (code == 105) then 
    if (lg=='F') then ; write(0,*)invalid_cherche_staF ; else ; write(0,*)invalid_cherche_staA ; end if
    call W_code_erreur(code)
    MCgravi = code 
    return
end if 

code = Lect_Coord()
    

! Recherche du numéro du point dans le tableau stat ***************************
! Cette numérotation sert à retrouver les inconnues lors de l'inversion
code = rempli_numpoint()
if (code == 106) then 
    if (lg=='F') then ; write(0,*)invalid_num_staF ; else ; write(0,*)invalid_num_staA ; end if
    call W_code_erreur(code)
    MCgravi = code 
    return
end if 

if (code == 125) then ! changer le txt d'erreur
    if (lg=='F') then ; write(0,*)invalid_num_staF ; else ; write(0,*)invalid_num_staA ; end if
    call W_code_erreur(code)
    MCgravi = code 
    return
end if 
if (param%print_obs) then
    call W_observation_rel() ! ecriture des observations dans le fichier résultat
end if


if (SD_too_Small) then ! on ne tolère pas d'écart-type trop petit pour les obs relatives
    if (lg=='F') then ; write(0,*)adj_impossibleF ; else ; write(0,*)adj_impossibleA ; end if
    call W_code_erreur(code)
    MCgravi = 107 
    return
end if

call W_liste_station2()

! TabObs contient la liste des observations individuelles avec le profil
! Il faut donc générer TabObsRel contenant les différences de gravités. *******
code = cree_difference_gravite()
if (code == 108) then 
    if (lg=='F') then ; write(0,*)error_cree_diff_graviF;  else ; write(0,*)error_cree_diff_graviA ; end if
    call W_code_erreur(code)
    MCgravi = code
    return
end if 

if (param%print_obs) then
    call W_difference_gravite()
end if

! Controle des numéros de stations ********************************************
! On verifie que toutes les stations de gravi absolue ont bien été observées en gravi relative
! Les stations sont stockées dans le tableau stat

if (param%lfix) then

    code = controle_num_point()
    if (code == 112) then 
        if (lg=='F') then ; write(0,*)error_cree_diff_graviF ; else ; write(0,*)error_cree_diff_graviA ; end if
        call W_code_erreur(code)
        MCgravi = code
        return
    end if
    
    if (param%print_obs) then
        call W_gravi_abs()
    end if

end if

! Position des inconnues de calibration

code = Rempli_pos_Inc_Calib()
 
ch = Cree_modeleF
call W_time(ch)

if (param%mode>=2 .and. param%Nb_ObsAbs==1) then
    param%mode=1
    if (lg=='F') then ; write(0,*)not_enough_fixed_pointF ; else ; write(0,*)not_enough_fixed_pointA ; end if
end if

Param%Nb_inc = Param%Nb_sta + Nb_profil * (param%drift_t + param%drift_k)

call Whtml_entete(version())
call Whtml_menu(param%mode)
call WHTML_cal_gravi()
call WHTML_sigma_coeff()

if (param%write_list_fic) then
    call WHTML_liste_nomficrel()
    call WHTML_liste_nomficabs()
    call WHTML_liste_nomficA10()
end if
if (param%write_list_station) call WHTML_liste_station2()

if (param%print_obs) call WHTML_observation_rel()
if (param%print_obs) call WHTML_difference_gravite()
if (param%print_obs) call WHTML_gravi_abs()
!deallocate(TabObs)

! Switch de gestion des différents modes de calcul (libre, contraint, libre + contraint

! ##################################################################
! mode = 1 
! Solution libre 
! ##################################################################
if ((param%mode==1)) then
    
    call create_Mc(Mcf,1)
    
    open(68,file=f_result2) ! Fichier de sortie
    
    call WGMT_bat('gmt.pl')
    call WGMT_pts_rel("pts_rel.txt")
    call WGMT_pts_abs("pts_abs.txt")
    call WGMT_profil("profils.txt",MCf)
    
    call dessine_reseau(MCf,.true.)
    
    call W_sol_libre();call WHTML_sol_libre()
    code = calcul(MCf)
    if (code == 110) then
        Mcgravi = code 
        return
    end if
    write(0,'(1x,A12,f20.3)')'Sigma0 : ',MCf%sigma0
    call W_param(Mcf);call WHTML_param(Mcf)
    
    code = statistique(MCf,1)
    if (code /= 0) then
        close(67)
        Mcgravi = code 
        return
    end if
    
    call WHTML_khi2(MCf)
    call WGMT_fic_histo("histo1.txt",MCf)
    call WGMT_bat_histo('hist1.pl',"histo1.txt",'hist1.ps','hist1.png')
    call WHTML_histo('hist1.png')
    call WHTML_synthese(MCf,1,'synthese1')
    if (MCf%Nb_obsRel+MCf%Nb_obsabs>20) call WHTML_20_plus_gros_resid(MCf,1,'20grosresid1')
    if (param%write_resid)  call WHTML_tautest(MCf,1,'tautest1')
        
    call W_gravity(Mcf,67)
    if (param%write_gravity) call WHTML_gravity(Mcf) 
    call W_gravity(Mcf,68) 
    if (param%write_drift) call WHTML_drift(Mcf) 
    call W_drift(Mcf)
    
    if (Param%create_r) call creer_fic_r (MCf)
    deallocate(TabObs)
    
    call run_GMT(MCf,.true.) 
    
    if (allocated(TabObsRel)) deallocate (TabObsRel)
    code = libere_MC(MCf)
    close(68)
    
! ##################################################################    
! mode = 2
! solution contrainte sur la gravi absolue
! possibilité d'estimer la calibration
! ##################################################################
elseif (param%mode==2) then
    ! 1ere passe de calcul avec les Cf du fichier de calibration 
    ! Permat de générer des gravités approchées
    
    write(0,*)""
    
    call create_Mc(Mcc,2)

    open(69,file=f_result3) ! Fichier de sortie

    call W_sol_contrainte();call WHTML_sol_contrainte()
    
    if (param%writemat) then
        call write_mat(MCc%AtPA,MCc%Nb_inc,MCc%Nb_inc,'AtPA_'//'00'//'.txt')
        call write_mat(MCc%AtPB,MCc%Nb_inc,1,'AtPB_'//'00'//'.txt')
        call write_mat(MCc%A,MCc%Nb_inc,MCc%Nb_obsRel+MCc%Nb_obsabs,'A_'//'00'//'.txt')
        call write_mat(MCc%P,MCc%Nb_obsRel+MCc%Nb_obsabs,1,'P_'//'00'//'.txt')
        call write_mat(MCc%B,MCc%Nb_obsRel+MCc%Nb_obsabs,1,'B_'//'00'//'.txt')
    endif    
        
    !ch = 'Inversion'
    !call W_time(ch)
    
    call WGMT_bat('gmt.pl')
    call WGMT_pts_rel("pts_rel.txt")
    call WGMT_pts_abs("pts_abs.txt")
    call WGMT_profil("profils.txt",MCc)
    
    call dessine_reseau(MCc,.true.) ! on dessine le reseau avant de calculer pour permettre de trouver les erreurs

    
    code = calcul(MCc)
    if (code == 110) then
        Mcgravi = code
        call dessine_reseau(MCc,.true.)
        return
    end if
    write(0,'(1x,A12,f20.3)')'Sigma0 : ',MCc%sigma0
    
    if (param%writemat) then
        call write_mat(MCc%X,MCc%Nb_inc,1,'X_'//'00'//'.txt')   
    endif 
    
    
    call W_param(Mcc);call WHTML_param(Mcc)
    
    
    if (associated(Mcc%sol_sans_calib)) deallocate(Mcc%sol_sans_calib)
    allocate(Mcc%sol_sans_calib(Mcc%Nb_inc))
    do i=1,Mcc%NB_inc
        Mcc%sol_sans_calib(i) = Mcc%X(i)
    end do
    
    ch = 'Statistique'
    !call W_time(ch)
    write(0,*)'Statistique'
    code = statistique(Mcc,2)
    if (code /= 0) then
        close(67)
        Mcgravi = code 
        return
    end if
    call WHTML_khi2(MCc)
    call WGMT_fic_histo("histo1.txt",MCc)
    call WGMT_bat_histo('hist1.pl',"histo1.txt",'hist1.ps','hist1.png')
    call WHTML_histo('hist1.png')
    call WHTML_synthese(MCc,2,'synthese1')
    if (MCc%Nb_obsRel+MCc%Nb_obsabs>20) call WHTML_20_plus_gros_resid(MCc,2,'20grosresid1')
    if (param%write_resid) call WHTML_tautest(MCc,2,'tautest1')
    
    call W_gravity(Mcc,67)
    if (param%write_gravity)  call WHTML_gravity(Mcc) 
    !call W_gravity(Mcc,69)
    call W_drift(Mcc)
    if (param%write_drift) call WHTML_drift(Mcc)
    
    if (param%ngravi_Cal>0) then 
    
        ! Reecriture du systeme pour estimer la calibration
        
        write(0,*)""
        ch = 'Initialisation de l''estimation des calibrations'
        call W_time(ch)

        ! Initialisation des matrices
        code = Init_Mc_Cal(MCc,2)
           
        sigma00 = 1d20
        iter = 1
        !MCc%NIterMax = 2
        do while (iter < MCc%NIterMax) 
        
            ch = 'Remplissage des matrices'
            call W_time(ch)
            !write(0,'(A9,x,I2)')'Iteration',iter
            
            code = Rempli_Mc_Cal(MCc,2)
            
            if (param%writemat) then
                write(ch2,'(I2)')iter
                call write_mat(MCc%AtPA,MCc%Nb_inc,MCc%Nb_inc,'AtPA_'//ch2//'.txt')
                call write_mat(MCc%AtPB,MCc%Nb_inc,1,'AtPB_'//ch2//'.txt')
                call write_mat(MCc%A,MCc%Nb_inc,MCc%Nb_obsRel+MCc%Nb_obsabs,'A_'//ch2//'.txt')
                call write_mat(MCc%P,MCc%Nb_obsRel+MCc%Nb_obsabs,1,'P_'//ch2//'.txt')
                call write_mat(MCc%B,MCc%Nb_obsRel+MCc%Nb_obsabs,1,'B_'//ch2//'.txt')
                call write_mat(MCc%X0,MCc%Nb_inc,1,'X0_'//ch2//'.txt')
                call write_mat(MCc%X,MCc%Nb_inc,1,'X_'//ch2//'.txt')
            endif
        
            code = calcul(MCc)
            if (code == 110) then
                Mcgravi = code
                return
            end if   

            if (param%writemat) then
                write(ch2,'(I2)')iter
                call write_mat(MCc%X0,MCc%Nb_inc,1,'X0bis_'//ch2//'.txt')
                call write_mat(MCc%X,MCc%Nb_inc,1,'Xbis_'//ch2//'.txt')   
            endif      
            
            ! Mise à jour du vecteur des inconnues

            code = Mise_A_Jour(MCc)
            
            if (param%writemat) then
                write(ch2,'(I2)')iter
                call write_mat(MCc%X0,MCc%Nb_inc,1,'X0ter_'//ch2//'.txt')
                call write_mat(MCc%X,MCc%Nb_inc,1,'Xter_'//ch2//'.txt')   
            endif      
            
            write(0,'(1x,A9,1x,I2,A12,f20.3)')'Iteration',iter,' ; Sigma0 : ',MCc%sigma0
        
            if (convergence(MCc,sigma00)) then
                code = Mise_A_Jour_inv(MCc) 
                ! permet de récupérer les paramètres dans MC.X pour le calcul stat
                exit
            end if
        
            iter = iter + 1    
        end do 
        
        call W_sol_calib();call WHTML_sol_calib()
        call W_param(Mcc);call WHTML_param(Mcc)
        
        ch = 'Statistique'
        !call W_time(ch)
        write(0,*)'Statistique'
        code = statistique(Mcc,2)
        if (code /= 0) then
            close(67)
            Mcgravi = code 
            return
        end if
        call WHTML_khi2(MCc)
        call WGMT_fic_histo("histo2.txt",MCc)
        call WGMT_bat_histo('hist2.pl',"histo2.txt",'hist2.ps','hist2.png')
        call WHTML_histo('hist2.png')
        call WHTML_synthese(MCc,2,'synthese2')
        if (MCc%Nb_obsRel+MCc%Nb_obsabs>20) call WHTML_20_plus_gros_resid(MCc,2,'20grosresid2')
        
        if (param%write_resid) call WHTML_tautest(MCc,2,'tautest2')
        
        call W_Ecart(Mcc,67)
        if (param%write_gravity) call WHTML_Ecart(MCc)
        call W_gravity(Mcc,69)

        call W_calibration(MCc)
        if (param%write_drift) call WHTML_calibration(MCc)
        call W_drift(Mcc)
        if (param%write_drift) call WHTML_drift(Mcc)

    
    end if
    write(0,*)''
    call run_GMT(MCc,.true.) 
    code = libere_MC(MCc)
    close(69)
        
! ##################################################################
! mode = 3
! Attention : le mode 3 ne permet pas l'estimation de la calibration 
! MODE TRES FORTEMENT DECONSEILLE
! ##################################################################
else if(param%mode==3) then
 
     
    call create_Mc(Mcf,1)
    call create_Mc(Mcc,2)
    if (allocated(TabObsRel)) deallocate (TabObsRel)
    
    open(68,file=f_result2) ! Fichier de sortie
    open(69,file=f_result3) ! Fichier de sortie
    
    call W_sol_libre();call WHTML_sol_libre()
    code = calcul(MCf)
    if (code == 110) then
        Mcgravi = code
        return
    end if
    call W_param(Mcf)
    call W_gravity(Mcf,67);call WHTML_gravity(Mcf)  
    call W_gravity(Mcf,68)
    call W_drift(Mcf);call WHTML_drift(Mcf)
    code = statistique(MCf,1)
    if (code /= 0) then
        close(67)
        Mcgravi = code 
        return
    end if
    call WHTML_khi2(MCf)

    call W_sol_contrainte();call WHTML_sol_contrainte()
    code = calcul(MCc)
    if (code == 110) then
        Mcgravi = code 
        return
    end if
    call W_param(Mcc);call WHTML_param(Mcc)
    call W_gravity(Mcc,67);call WHTML_gravity(Mcc) 
    call W_gravity(Mcc,69)
    call W_drift(Mcc);call WHTML_drift(Mcc)
    code = statistique(Mcc,2)
    if (code /= 0) then
        close(67)
        Mcgravi = code 
        return
    end if
    call WHTML_khi2(MCc)
    
    call W_comparaison()
    
    close(68)
    close(69)

    code = libere_MC(MCf)
    code = libere_MC(MCc)

! ##################################################################
! mode = 4 simulation
! ##################################################################
else if(param%mode==4) then

    call create_Mc(Mcc,4)
    call W_Simul()
    code = simul(Mcc)
    if (code == 110) then
        Mcgravi = code 
        return
    end if
    call W_param(Mcc);call WHTML_param(Mcc)
    call statistique_simul(Mcc)
    if (allocated(TabObsRel)) deallocate (TabObsRel)
    
end if

close(67)
call Whtml_finfichier()
close(70)

call Whtml_cpfic()

if (allocated(TabGravi)) deallocate (TabGravi)
code = libere_param(param) 
code = libere_rawdata()
if (allocated(tabresid)) deallocate(tabresid) 
if (allocated(TabResid_trie)) deallocate(TabResid_trie) 
write(0,*)'Fin du programme'
write(0,*)''
elapsed_time = heureF()
write(0,'(1x,A,f10.2)')'Temps de calcul (s) : ',elapsed_time


Mcgravi = 0

end function MCgravi

end module mcgravi_main
