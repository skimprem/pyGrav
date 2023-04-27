module param_data

implicit none

    ! Variables
    type TParam

        character(len=5) systeme
        character(len=8) ch_mkdir
        character(len=8) ch_copy
        character(len=8) ch_move
        character(len=8) ch_rm
        character(len=1) dsep

        character (len=1) lg 
        integer mode
        integer drift_t
        integer drift_k
        integer ngravi_Cal
        real*8 gradstd 
        real*8 siglevel
        real*8 critere_convergence
        real*8 sigma_factor ! facteur appliqué sur tous les sigma a priori 
        real*8 sigma_add ! facteur additif sur la variance
        logical Type_resid ! vrai si residu standard et faux si résidus normalisés
        
        character (len=255) nomficcal
        character (len=255) nomficout
        character (len=80) dossier
        integer len_dossier
        integer, pointer, dimension(:) :: ptab
        
        integer ntabnomficrel
        integer ntabnomficabs
        integer ntabnomficA10
        
        type (TDataFic), pointer , dimension(:) :: TabDataFic
        integer NDataFic
        
        type (TCoordFic), pointer , dimension(:) :: TabCoordFic
        integer NCoordFic
         
        logical lfix
        logical calf
        
        ! Gestion des éléments à afficher dans le fichier HTML
        logical write_list_fic
        logical write_list_station
        logical print_obs 
        logical writemat
        logical write_resid
        logical write_only_failed_tau_test
        logical write_gravity
        logical write_drift
        logical create_r
        
        integer Nb_obsAbs
        integer Nb_obsRel
        integer Nb_Sta
        integer Nb_inc
        
        real*8 lon1
        real*8 lon2
        real*8 lat1
        real*8 lat2
        
        integer delai_max        
    end type TParam
    
    type TDataFic
        character (len=255) nom
        character (len=255) rep 
        integer   typ                   ! 0 : relatif, 1 : absolu, 2 : A10
        real*8 sigma_f                  ! facteur appliqué sur tous les sigma a priori 
        real*8 sigma_a                  ! facteur additif sur la variance
    end type TDataFic
    
    type TCoordFic
        character (len=255) nom
        character (len=255) rep 
    end type TCoordFic
    
    type (TParam) Param ! paramètres du calcul
    
    character (len=3), parameter :: FORT = 'g95' ! ou INT pour intel	
    
contains

subroutine Init_param(p)
    implicit none
    type (TParam) p

    p%mode=1   
    p%drift_t = 1
    p%drift_k = 0
    p%ngravi_Cal = 0
    p%calf = .false.
    p%lfix = .false.
    p%dossier = ''
    p%ntabnomficrel=0
    p%ntabnomficabs=0
    p%ntabnomficA10=0
    p%nomficout=''
    p%nomficcal=''
    p%siglevel = 0.05 ! pour le moment on l'impose
    
    p%write_list_fic=.true.
    p%write_list_station=.true.
    p%writemat=.false.   
    p%write_gravity=.true.
    p%write_drift=.true.
    p%write_resid=.true.
    p%write_only_failed_tau_test=.false.
    
    p%create_r=.false.
    
    p%Nb_obsAbs=0
    p%Nb_obsRel=0
    p%nb_sta = 0
    p%Nb_inc = 0
    p%type_resid = .false.
    p%sigma_factor = 1
    P%sigma_add = 0
    p%lg='F'
    p%print_obs=.true.
    p%gradstd = -0.3086
    p%NDataFic = 0 
    p%NCoordFic = 0
    p%lon1 = -5
    p%lon2 = 10
    p%lat1 = 41
    p%lat2 = 52
    p%critere_convergence=0.000001
    p%systeme = 'LINUX'
    p%dsep = '/'
    p%ch_mkdir = 'mkdir  '
    p%ch_copy  = 'cp -f  '
    p%ch_move  = 'mv -f  '
    p%ch_rm    = 'rm -f  '
    
    p%delai_max=15
end subroutine Init_param
    

end module param_data

