module ecriture
! Ecriture dans le fichier résultat
contains

! Procedure qui permet de creer un dossier pour stocker les résultats du calcul
logical function Cree_dossier_resultat()
    use Portability_routines
    use param_data
	use sys_utils

    implicit none
    
    logical resultat
    integer(4) err
	integer(2) resultatI
    integer LENficout, len_s, len_dossier

    character (len=10) date_ , time_ , zone
    character (len=20) s 
    !CHARACTER (len=80) cmd
	CHARACTER (len=80) CHFMT
    INTEGER DATE_TIME (8)
    
    CALL DATE_AND_TIME ( date_ , time_ , zone , DATE_TIME )
    write(s,'(A8,A1,A6)')date_,"_",time_
    len_s = len_trim(s)
    lenficout = len_trim(param%nomficout)
    len_dossier = len_s+1+LENficout

    param%dossier(1:len_dossier) = param%nomficout(1:LENficout)//"_"//s(1:len_s)
    
   	resultatI = mkdir(param%dossier(1:len_trim(param%dossier)))
    
    if (resultatI .eq. 0) then
        WRITE (CHFMT,500)'(A11,1x,A',len_dossier,',1x,A4)'
        500  FORMAT (A,I2,A) 
        write(0,FMT=CHFMT)'Repertoire',param%dossier,'cree'
    end if
    
    if (param%systeme == 'LINUX') then
       param%nomficout(1:2+len_s+1+LENficout+LENficout+1) = &
	    &'./'//param%nomficout(1:LENficout)//"_"//s(1:len_s)//param%dsep//param%nomficout(1:LENficout)
    else
       param%nomficout(1:len_s+1+LENficout+LENficout+1) = &
	    &param%nomficout(1:LENficout)//"_"//s(1:len_s)//param%dsep//param%nomficout(1:LENficout)	
    endif

    Cree_dossier_resultat = resultat
    return
end function Cree_dossier_resultat

subroutine W_error
    write(0,*)'Usage: mcgravi config_file'
    return
end subroutine W_error

subroutine W_entete(num_vers)
    implicit none
    
    character (len=8) date
    character (len=10) heure
    character (len=40) num_vers
    call W_ligne()
    write(67,*)num_vers

    write(67,*)''
    !write(67,*)'Least squares software for absolute and relative gravity measurements'
    write(67,*)'Compensation des observations de gravimétrie absolue et relative'
    write(67,*)'Méthode des moindres carrés'
    write(67,*)'Beilin Jacques - 2004-06'
    write(67,*)'IGN - IPGP'
    call W_ligne()
    
    call date_and_time(date,heure)
    write(67,*)'Calcul effectué le ',date(7:8),'/',date(5:6),'/',date(1:4),' à ',heure(1:2),':',heure(3:4),':',heure(5:6)

end subroutine W_entete

subroutine W_code_erreur(c)
    implicit none
    integer c
    call W_ligne()
    write(67,*)'Calcul terminé avec erreur'
    write(67,'(a7,I3)')'Code : ',c
end subroutine W_code_erreur

subroutine W_ligne()
    write(67,*)''
    write(67,*)'=============================================================================='
    write(67,*)''
end subroutine W_ligne

subroutine W_sigma_coeff()
    use param_data
    !use raw_data
    implicit none
    integer k
    call W_ligne()
    write(67,*)'Modification des écarts-types a priori sur les fichiers relatifs'
    write(67,'(1x,a10,f6.3)')'Facteur = ',param%sigma_factor
    write(67,'(1x,a16,f6.4)')'Terme additif = ',param%sigma_add 
end subroutine W_sigma_coeff

subroutine W_cal_gravi()
    use param_data
    use raw_data
    implicit none
    integer k
    ! Ecriture des observations dans le fichier résultat
    call W_ligne()
    write(67,*)'Coefficients d''étalonnage des gravimètres '
    write(67,*)'Valeurs initiales '
    write(67,*)'Numero de série, numero dans CG3TOOL.init, Coefficient d''étalonnage'
    do k=1,ngravimeter
        if (TabGravi(k)%Estimate) then
            write(67,'(1x,A8,I2,2x,F12.10,1x,A6)')TabGravi(k)%Serial, TabGravi(k)%Num, TabGravi(k)%Cf,'estimé'
        else
            write(67,'(1x,A8,I2,2x,F12.10,1x,A5)')TabGravi(k)%Serial, TabGravi(k)%Num, TabGravi(k)%Cf,'connu'
        end if
    end do

end subroutine W_cal_gravi

subroutine W_liste_nomficrel()
    use param_data
    implicit none
    integer k,LENrep, LENnomfic,w,z
    type (TDataFic) Dfic
    character (len=60) nom_complet,rep,nomfic
    character (len=25) ch
    call W_ligne()
    write(67,*)'Liste des fichiers d''observations relatives  '
    write(67,*)"Fichier, facteur, terme additif"
    do k=1,param%nDataFic
        Dfic = param%TabDataFic(k)
        
        if (Dfic%typ == 0) then
        
            rep = Dfic%rep       
            LENrep = len_trim(rep)
            nomfic = Dfic%nom
            LENnomfic = len_trim(nomfic)
            nom_complet = ''        
            nom_complet(1:LENnomfic+LENrep) = rep(1:LENrep)//nomfic(1:LENnomfic)
            w = len_trim(nom_complet)
            z = 50 - w
            
            !WRITE (CHFMT,500) '(1X,',N,'F5.2)' 

			WRITE (CH,500) '(1x,A',w,',',z,'x,f6.1,1x,f6.3)'
			!write(0,*)CH 
            500  FORMAT (A,I2,A,I2,A) 
            WRITE (67,FMT=CH)nom_complet,Dfic%sigma_f,Dfic%sigma_a
            !write(67,55)nom_complet,Dfic%sigma_f,Dfic%sigma_a
            !55 format (1x,A<w>,<z>x,f6.1,1x,f6.3)
            
        end if
    end do
    write(67,*)' '
end subroutine W_liste_nomficrel

subroutine W_liste_nomficabs()
    use param_data
    implicit none
    integer k,w,z
    character (len=60) ch
    type (TDataFic) Dfic
    write(67,*)'Liste des fichiers d''observations absolues'
    write(67,*)"Fichier, facteur, terme additif"
    do k=1,param%nDataFic
    
        Dfic = param%TabDataFic(k)
        
        if (Dfic%typ == 1) then
            !write(67,'(x,A80)')Dfic%nom
            w = len_trim(Dfic%nom)
            z = 50 - w
            WRITE (CH,500) '(1x,A',w,',',z,'x,f6.1,1x,f6.3)' 
            500  FORMAT (A,I2,A,I2,A) 
            WRITE (67,FMT=CH)Dfic%nom,Dfic%sigma_f,Dfic%sigma_a

            
            !write(67,55)Dfic%nom,Dfic%sigma_f,Dfic%sigma_a
            !55 format (1x,A<w>,<z>x,f6.1,1x,f6.3)
            
        end if
        
    end do
    write(67,*)' '
end subroutine W_liste_nomficabs

subroutine W_liste_nomficA10()
    use param_data
    implicit none
    integer k,w,z
    type (TDataFic) Dfic
    character (len=60) ch
    write(67,*)'Liste des fichiers d''observations de gravimètre A10'
    write(67,*)"Fichier, facteur, terme additif"
    do k=1,param%nDataFic
    
        Dfic = param%TabDataFic(k)
        
        if (Dfic%typ == 2) then
            !write(67,'(x,A80)')Dfic%nom
            w = len_trim(Dfic%nom)
            z = 50 - w
            WRITE (CH,500) '(1x,A',w,',',z,'x,f6.1,1x,f6.3)' 
            500  FORMAT (A,I2,A,I2,A) 
            WRITE (67,FMT=CH)Dfic%nom,Dfic%sigma_f,Dfic%sigma_a
            
            !write(67,55)Dfic%nom,Dfic%sigma_f,Dfic%sigma_a
            !55 format (1x,A<w>,<z>x,f6.1,1x,f6.3)
        end if
        
    end do
    write(67,*)' '
end subroutine W_liste_nomficA10

subroutine W_observation_rel()
    use param_data
    use raw_data
    implicit none
    integer k,nprofil
    
    ! Ecriture des observations dans le fichier résultat
    call W_ligne()
    write(67,*)'Observations Relatives'
    write(67,*)'nom, gravite(mGal), SD(mGal), Temp(mK), t (jour decimal), date, heure'
    nprofil = 0
    do k=1,nTabObs
        obs=TabObs(k)
        
        if (obs%profil/=nprofil) then
            write(67,*)' '
            write(67,'(1x,A7,1x,I2,4x,A9,A20,1x,A6,F20.6)')'Profil ',&
			&obs%profil,' fichier ',tabprofil(obs%profil)%nomfic,' Cf = ',obs%Cf
            nprofil = obs%profil
        end if 
        
        write(67,'(1x,A8,1x,f9.3,1x,f6.3,1x,f5.2,1x,f9.5,1x,I6,1x,I6)')&
            &obs%nomsta,obs%grav,obs%sd,obs%tempK,obs%mjd,obs%date,obs%heure
    end do

end subroutine W_observation_rel

subroutine W_liste_station()
    use param_data
    use raw_data
    implicit none
    integer k

    call W_ligne()
    write(67,*)'Liste des stations observées'
    do k=1,param%Nb_sta
        write(67,'(1x,A8)')stat(k)
    end do
end subroutine W_liste_station

subroutine W_liste_station2()
    use param_data
    use raw_data
    implicit none
    integer k

    call W_ligne()
    ! fichier log
    write(67,*)'Liste des stations observées'
    write(67,*)'Nom, longitude, latitude, numero de l''inconnue'
    do k=1,NTabStation
        !write(67,'(x,A8,x,f14.8,x,f14.8)')TabStation(k).nomsta,TabStation(k).lon,TabStation(k).lat
        write(67,'(1x,A8,1x,f14.8,1x,f14.8,1x,I12)')TabStation(k)%nomsta,TabStation(k)%lon,TabStation(k)%lat,TabStation(k)%numsta

    end do
end subroutine W_liste_station2

subroutine W_difference_gravite()
    use param_data
    use raw_data
    implicit none
    integer k,nprofil
    real*8 sigma
    !Fichier log
    call W_ligne()
    nprofil = 0
    write(67,*)'Différences de gravités'
  
    do k=1,param%Nb_obsRel

        Obs_Rel=TabObsRel(k)
        if (obs_Rel%profil/=nprofil) then
            write(67,*)' '
            write(67,'(1x,A7,1x,I2,2x,A7,1x,A12,2x,A5,F10.8,2x,A5,F9.5,2x,A5,F5.2)')&
                &'Profil ',obs_rel%profil,&
                &'fichier',tabprofil(obs_rel%profil)%nomfic,&
                &'Cf = ',obs_rel%Cf,&
                &'t0 = ',obs_rel%mjd0,&
                &'K0 = ',obs_rel%temp_K0
            nprofil = obs_Rel%profil
        end if 
        
        sigma = SQRT(obs_rel%SD_AR**2+obs_rel%SD_AV**2)
        write(67,'(1x,A8,1x,A8,1x,f9.3,1x,f9.3,1x,f9.3,1x,f6.3,1x,F5.2,1x,f5.2,1x,F9.5,1x,F9.5)')&
                        &obs_rel%nomsta_AR,&
                        &obs_rel%nomsta_AV,&
                        &obs_rel%grav_AR-obs_rel%grav_AV,&
                        &obs_rel%grav_AR,&
                        &obs_rel%grav_AV,&
                        &sigma,&
                        &obs_rel%tempK_AR,&
                        &obs_rel%tempK_AV,&
                        &obs_rel%mjd_Ar,&
                        &obs_rel%mjd_AV
        
    end do

end subroutine W_difference_gravite

subroutine W_gravi_abs()
    use param_data
    use raw_data
    implicit none
    integer k

    call W_ligne()
    write(67,*)'Observations absolues'
    write(67,*)'Nom, Gravite (mgal), SD (mgal)'
	do k=1,param%Nb_obsAbs
	    write(67,'(1x,A8,1x,f11.3,1x,F6.3)')fixstn(k),fixgra(k),stdx(k)
	end do

end subroutine W_gravi_abs

subroutine W_sol_contrainte()
    use str_const
    use param_data
    call W_ligne()
    write(67,*)'Solution contrainte'
    !call W_ligne()
    if (param%lg=='F') then ; write(0,*)constraintF ; else ; write(0,*)constraintA ; end if
end subroutine W_sol_contrainte

subroutine W_simul()
    use str_const
    use param_data
    call W_ligne()
    write(67,*)'Simulation'
    !call W_ligne()
    if (param%lg=='F') then ; write(0,*)simulF ; else ; write(0,*)simulA ; end if
end subroutine W_simul

subroutine W_sol_libre()
    use str_const
    use param_data
    call W_ligne()
    write(67,*)'Solution Libre'
    call W_ligne()
    if (param%lg=='F') then ; write(0,*)datumfreeF ; else ; write(0,*)datumfreeA ; end if
    WRITE(67,*)'Information : le calcul en solution libre cale les gravités'
    WRITE(67,*)'sur le premier point du premier fichier d''observations absolues'
    !write(67,*)''
end subroutine W_sol_libre

subroutine W_sol_calib()
    use str_const
    use param_data
    call W_ligne()
    WRITE(67,*)'Calcul avec estimation des calibrations'
    write(67,*)''
end subroutine W_sol_calib

subroutine W_param(MC)
    use MC_data
    use param_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i
    
    call W_ligne()
    write(67,103)'Nombre de stations                                : ',MC%Nb_sta
    write(67,103)'Nombre de profils                                 : ',MC%Nb_profil
    write(67,103)'Degre du polynome de derive liee au temps         : ',MC%degre_t
    write(67,103)'Degre du polynome de derive liee à la temperature : ',MC%degre_k
    write(67,*)''
    write(67,103)'Nombre d''inconnues                                : ',MC%Nb_inc
    write(67,*)''
    write(67,103)'Nombre d''observations relatives                   : ',MC%Nb_obsRel
    if (param%lfix) then 
        write(67,103)'Nombre d''observations absolues                    : ',MC%Nb_obsAbs
    end if 
    write(67,*)''
                   
    write(67,103)'Degré de liberté du système                       : ',Mc%dof
    write(67,*)''

    103 format(1x,a52,I4)
    
end subroutine W_param

subroutine W_gravity(MC,unite)
    use str_const
    use param_data

    use MC_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i,unite
    
    write(67,*)''
    
    if (unite==67) then 
        if (param%lg=='F') then ; write(0,*)WgravF ; else ; write(0,*)WgravA ; end if
        WRITE(unite,*)'Nom, Gravite compensee (mgal), SD (mgal)' 
    end if
    !write(0,*)MC%sigma0
    DO I=1,MC%Nb_sta
        !WRITE(unite,'(x,a8,3X,F12.3,3x,f9.3)')MC%stat(i),MC%X(i),DSQRT(MC%SX(i,i))*MC%sigma0
        WRITE(unite,'(1x,a8,3X,F12.3,3x,f9.3)')MC%stat(i),MC%X(i),MC%Sig(i)
    END DO
    
    write(67,*)''
end subroutine W_gravity

subroutine W_Ecart(MC,unite)
    use str_const
    use param_data

    use MC_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i,unite
    
    write(67,*)''
    if (unite==67) then 
        if (param%lg=='F') then ; write(0,*)WgravF ; else ; write(0,*)WgravA ; end if
        WRITE(unite,*)'Nom, Gravite compensee (mGal), Ecart avec la solution sans calibration (mGal), SD (mGal)' 
    end if
    !write(0,*)MC%sigma0
    DO I=1,MC%Nb_sta
        !WRITE(unite,'(1x,a8,3X,F12.3,3x,f9.3)')MC.stat(i),MC.X(i),DSQRT(MC.SX(i,i))*MC.sigma0
        WRITE(unite,'(1x,a8,3X,F12.3,3x,F12.3,3x,f9.3)')MC%stat(i),MC%X(i),MC%sol_sans_calib(i)-MC%X(i),MC%Sig(i)
    END DO
    
    write(67,*)''
end subroutine W_Ecart


subroutine W_drift(MC)
    use MC_data
    use param_data
    use raw_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i,k,row
    
    if (param%drift_t==0 .and. param%drift_k==0) then 
        return
    end if
            ! Write drift parameters
    do k=1,MC%Nb_profil
        write(67,'(1x,A7,1x,I3,1x,A7,1x,A8)')'Profil ',k,'gravi ',tabprofil(k)%serial

        ! dérive liée au temps
        if(param%drift_t.gt.0) then
            write(67,*)'Temps'
            row=MC%Nb_sta+(k-1)*(param%drift_t+param%drift_k)
            do i=1,param%drift_t
                row=row+1
                !write(67,'(1x,A7,1x,I1,1x,2f7.3)')'degré ',i,MC.X(row),DSQRT(MC%SX(row,row))*MC%sigma0
                write(67,'(1x,A7,1x,I1,1x,2f7.3)')'degré ',i,MC%X(row),MC%Sig(row)
            end do
        end if

        ! dérive liée à la température
        if(param%drift_k.gt.0) then
            write(67,*)'Température'
            row=MC%Nb_sta+(k-1)*(param%drift_t+param%drift_k) + param%drift_t
            do i=1,param%drift_k
                row=row+1
                !write(67,'(1x,A7,1x,I1,1x,2f7.3)')'degré ',i,MC%X(row),DSQRT(MC%SX(row,row))*MC%sigma0
                write(67,'(1x,A7,1x,I1,1x,2f7.3)')'degré ',i,MC%X(row),MC%Sig(row)
            end do
        end if

        write(67,*)' '
    end do

    write(67,*)''
    
end subroutine W_drift

subroutine W_calibration(MC)
    use MC_data
    use param_data
    use raw_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i,k,row,n
    
    
    row = MC%Nb_sta + (MC%Nb_profil) * (MC%degre_k + MC%degre_t)
    
    if (MC%ngravi_Cal==0) then 
        return
    end if
    
    write(67,*)'Coefficients de calibration'
    do k=1,ngravimeter
    
        if (TabGravi(k)%Estimate) then
            row = row + 1
            !write(67,'(A7,x,A8,x,f20.8,x,f20.8)')'gravi ',TabGravi(k)%serial,MC%X0(row),DSQRT(MC%SX(row,row))*MC%sigma0
            write(67,'(A7,1x,A8,1x,f20.6,1x,f20.6)')'gravi ',TabGravi(k)%serial,MC%X0(row),MC%Sig(row)

        end if

    end do

    write(67,*)''
    
end subroutine W_calibration

subroutine W_20_plus_gros_resid(MC,mode)
    use MC_data
    use param_data
    implicit none
    type (Tmc), intent(inout):: MC
    integer mode
    real*8 resNorm,resnorm2
    integer i,nb_oa
    
    if (mode==1) then
        nb_oa = 0;
    else
        nb_oa = MC%nb_obsAbs
    end if

    write(67,*)' '
    if (param%type_resid) then
        write(67,*)'20 plus gros résidus standards' 
        write(67,*)'Point initial, Point final, observation (mgal), résidu (mgal), résidu standard' 
    else 
        write(67,*)'20 plus gros résidus normalisés'  
        write(67,*)'Point initial, Point final, observation (mgal), résidu (mgal), résidu normalisé' 
    end if
    
    do i=MC%Nb_obsRel+Nb_oa,MC%Nb_obsRel+Nb_oa-20,-1
        if (param%type_resid) then
            resNorm = Tabresid_trie(i)%std_res
        else
            resNorm = Tabresid_trie(i)%Norm_res
        end if
        if (Tabresid_trie(i)%abs) then
            write(67,'(1x,A8,10x,f16.3,1x,f8.3,1x,f8.3)')&
			&Tabresid_trie(i)%ini,Tabresid_trie(i)%obs,Tabresid_trie(i)%resid, resNorm
        else
            write(67,'(1x,A8,1x,A8,1x,f16.3,1x,f8.3,1x,f8.3)')&
			&Tabresid_trie(i)%ini,Tabresid_trie(i)%fin,Tabresid_trie(i)%obs,Tabresid_trie(i)%resid, resNorm
        end if
    
    end do
        
end subroutine W_20_plus_gros_resid
    
subroutine W_comparaison()
    use MC_data
    implicit none
    integer i    
    call W_ligne()
    write(67,*)'Comparaison des solutions contraintes et libres'
    call W_ligne()
    
    
    WRITE(67,*)'C : solution contrainte (mgal)'
    WRITE(67,*)'L : solution libre (mgal)'
    WRITE(67,*)'C-L : solution contrainte - solution libre (mgal)'
    WRITE(67,*)
    WRITE(67,*)' Num  Nom           L           C           C-L ' 
    WRITE(67,*)
    do I=1,Mcc%Nb_sta
        WRITE(67,'(I3,3x,a8,3X,F9.3,3x,f9.3,3x,f9.3)')I,MCc%stat(i),Mcf%X(i),Mcc%X(i),Mcc%X(i)-Mcf%X(i)
    end do
end subroutine W_comparaison

subroutine W_time(ch)

    use Portability_routines
    implicit none
    
    CHARACTER*8 HOUR
    character (len=100) :: ch, CHFMT
    integer width
    width = len_trim(ch)
    width = 8 + 3 + width
    WRITE (CHFMT,500) '(1x,A',width,')' 
    500  FORMAT (A,I2,A) 
    HOUR = heure()
    ch = hour // " : "//ch
    WRITE (0,FMT=CHFMT)ch
    
end subroutine W_time

!________________________________________________________Module d'ecriture de fichiers r artificiels
subroutine creer_fic_r (MC)

    use param_data
    use MC_data
    use raw_data
    implicit none
    integer i,j,k,npts
    character (len=40) nom_fic_c_courant,nom_fic_r_courant
    type (Tmc), intent(in):: MC
    integer, dimension (20) :: tab_temp ! tableau temporaire servant à éviter d'écrire plusieurs fois le même point
    logical ok
    
    do i=1,nTabObs
        if (i==1) then
            nom_fic_c_courant = tabprofil(TabObs(i)%profil)%nomfic
            nom_fic_r_courant = nom_fic_c_courant(1:5)//"r"//nom_fic_c_courant(7:12)
            open (21,file=nom_fic_r_courant,action='write',status='replace',position='rewind')
            write (21,*)'# Fichier de mesure r artificiellement cree par MCGravi'
            npts=0
            tab_temp=0
        end if
        if (i>1) then
            if (TabObs(i)%profil /= TabObs(i-1)%profil) then
                do k=1,npts
                    write(21,'(1x,A8,3x,f15.4,3x,f15.4,3x,i1,3x,i1)') &
                       & MC%stat(tab_temp(k)),MC%X(tab_temp(k))-980000,MC%sig(tab_temp(k)),0,0
                end do
                close (21)
                nom_fic_c_courant = tabprofil(TabObs(i)%profil)%nomfic
                nom_fic_r_courant = nom_fic_c_courant(1:5)//"r"//nom_fic_c_courant(7:12)
                open (21,file=nom_fic_r_courant,action='write',status='replace',position='rewind')
                write (21,*)'# Fichier de mesure r artificiellement cree par MCGravi'
                npts=0
                tab_temp=0
            end if
        end if
        do j=1,MC%Nb_sta
            if (TabObs(i)%nomsta==MC%stat(j)) then
                if (npts==0) then
                    npts=npts+1
                    tab_temp(npts)=j
                end if
                ok=.true.
                do k=1,npts
                    if (j==tab_temp(k)) then
                        ok=.false.
                        exit
                    end if
                end do
                if (ok) then
                    npts=npts+1
                    tab_temp(npts)=j
                end if
            end if
        end do
        if (i==nTabObs) then
            do k=1,npts
                write(21,'(1x,A8,3x,f15.4,3x,f15.4,3x,i1,3x,i1)')&
                   & MC%stat(tab_temp(k)),MC%X(tab_temp(k))-980000,MC%sig(tab_temp(k)),0,0
            end do
        end if
    end do
    
    

end subroutine creer_fic_r



!___________________________________________________________________________________________________




end module ecriture
