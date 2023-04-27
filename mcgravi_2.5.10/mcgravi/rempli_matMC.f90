module rempli_MatMC
! construction du système des moindres carrés à partir des observations
implicit none

contains

! Test de la convergence du système 
! Basé sur la variation de l'estimateur du facteur de variance
logical function Convergence(MC,s)
    use MC_data
    use param_data
    
    implicit none
    type (Tmc), intent(inout):: MC
    real*8, intent(inout):: s
            
    if ((abs(MC%sigma0 - s))<param%critere_convergence) then 
        Convergence = .true.
    else
        Convergence = .false.
        s = MC%sigma0
    end if
    
    return
    
end function Convergence

integer function libere_MC(MC)
    use Raw_Data
    use MC_data
    use param_data
    
    implicit none
    type (Tmc), intent(inout):: MC
            
    if (Associated(MC%B)) deallocate(MC%B)
    if (Associated(MC%A)) deallocate(MC%A)
    if (Associated(MC%P)) deallocate(MC%P)
    if (Associated(MC%V)) deallocate(MC%V)
    if (Associated(MC%AtPA)) deallocate(MC%AtPA)
    if (Associated(MC%AtPB)) deallocate(MC%AtPB)
    if (Associated(MC%X)) deallocate(MC%X)
    if (Associated(MC%X0)) deallocate(MC%X0)
    if (Associated(MC%stp)) deallocate(MC%stp)
    if (Associated(MC%enp)) deallocate(MC%enp)
    if (Associated(MC%pos)) deallocate(MC%pos)
    if (Associated(MC%stat)) deallocate(MC%stat)
    if (Associated(MC%sx)) deallocate(MC%sx)
    if (Associated(MC%sig)) deallocate(MC%sig)
    if (Associated(MC%sol_sans_calib)) deallocate(MC%sol_sans_calib)
    
    libere_MC = 0
    
end function libere_MC

integer function Init_Mc_Cal(MC,type_calc)
    use Raw_Data
    use MC_data
    use param_data
    
    implicit none
    type (Tmc), intent(inout):: MC
    integer type_calc ! correspond au mode  : 1 pour libre, 2 pour contraint
    integer k,nb,code
    
    MC%NIterMax = 20
    
    MC%Nb_obsRel = param%Nb_obsRel
    MC%Nb_sta = param%Nb_sta
    MC%Nb_profil = Nb_profil
    MC%degre_k = param%drift_k
    MC%degre_t = param%drift_t
    MC%ngravi_Cal = param%ngravi_Cal
    MC%Nb_inc = MC%Nb_sta + MC%Nb_profil * (MC%degre_k + MC%degre_t) + MC%ngravi_Cal
    param%Nb_inc = MC%Nb_inc

    ! Allocation des matrices *************************************************
    
    if (type_calc==2) then
    
        if (Associated(MC%B)) deallocate(MC%B)
        if (Associated(MC%A)) deallocate(MC%A)
        if (Associated(MC%P)) deallocate(MC%P)
        if (Associated(MC%V)) deallocate(MC%V)
	if (Associated(MC%AtPA)) deallocate(MC%AtPA)
        if (Associated(MC%AtPB)) deallocate(MC%AtPB)
        if (Associated(MC%X0)) deallocate(MC%X0)
        if (Associated(MC%sx)) deallocate(MC%sx)
	if (Associated(MC%sig)) deallocate(MC%sig)
        if (Associated(MC%sol_sans_calib)) deallocate(MC%sol_sans_calib)
    
        MC%Nb_obsAbs = param%Nb_obsAbs
        MC%dof = MC%Nb_obsRel + MC%Nb_obsAbs - MC%Nb_inc
        
        allocate (MC%B(param%Nb_obsRel+param%Nb_obsAbs))
        allocate (MC%A(param%Nb_obsRel+param%Nb_obsAbs,MC%Nb_inc))
        allocate (MC%P(param%Nb_obsRel+param%Nb_obsAbs))
        allocate (MC%V(param%Nb_obsRel+param%Nb_obsAbs))
        allocate (MC%atpa(MC%Nb_inc,MC%Nb_inc))
        allocate (MC%atpB(MC%Nb_inc))
        allocate (MC%X0(MC%Nb_inc))
        
        allocate (MC%sx(MC%Nb_inc,MC%Nb_inc))
        allocate (MC%sig(MC%Nb_inc))
                
    else
        Init_Mc_Cal = 120 
        return      
    end if
    
    ! Initialisation des matrices *********************************************

    MC%A=0.d0
    MC%atpa=0.d0
    MC%atpB=0.d0
    
    
    ! Recuperation des stations ***********************************************

    do k=1,param%Nb_sta
        MC%stat(k) = stat(k)
    end do
    
    ! Recuperation de la solution generee sans estimation de la calibration
   
    nb = MC%Nb_sta + MC%Nb_profil * (MC%degre_k + MC%degre_t)
    do k=1,nb
        MC%X0(k) = MC%X(k)
    end do
    
    ! valeur initiale des inconnues de calibration
    do k=1,ngravimeter
        if (TabGravi(k)%Estimate) then 
            !write(0,*)TabGravi(k)%serial,TabGravi(k)%pos
            MC%X0(TabGravi(k)%pos) = TabGravi(k)%Cf 
            ! a modifier jbl 2007-136  ************************************************    
            MC%X0(TabGravi(k)%pos) = 1.0d0     
        end if
    end do
    
    ! maintenant qu'on a récupéré les valeurs approchées, on peut réallouer MC%X
    if (Associated(MC%X)) deallocate(MC%X)
    allocate (MC%X(MC%Nb_inc))
    
    Init_Mc_Cal = 0
   
end function Init_Mc_Cal

integer function Mise_A_Jour(MC)
    use MC_data
    
    implicit none
    type (Tmc), intent(inout):: MC
    integer i 
        
    do i=1,MC%nb_Inc
        !write(0,*)i,MC%X0(i),MC%X(i),MC%X0(i) + MC%X(i)   
        MC%X0(i) = MC%X0(i) + MC%X(i) 
    end do
    
    Mise_A_Jour = 0
    
end function Mise_A_Jour

integer function Mise_A_Jour_inv(MC)
    use MC_data
    
    implicit none
    type (Tmc), intent(inout):: MC
    integer i 
           
    do i=1,MC%nb_Inc
        MC%X(i) = MC%X0(i)
    end do
    
    Mise_A_Jour_inv = 0
    
end function Mise_A_Jour_inv


! Procedure qui initialise un certain nombre de variables de l'objet MC 
! en vue du calcul avec estimation de la calibration des appareils
! Construction de la matrice modèle A, du vecteur des observations B, du vecteur des poids P, de AtPA, AtPB
integer function Rempli_Mc_Cal(MC,type_calc)
    use Raw_Data
    use MC_data
    use param_data
    use write_matr
    
    implicit none
    type (Tmc), intent(inout):: MC
    integer type_calc ! correspond au mode  : 1 pour libre, 2 pour contraint

    real*8 ka,kb,grav,ta,tb,std_obs,Ck
    real*8 drk, drt, gAR, gAV,fX0
    integer mode ! on charge differemment A si on est en solution libre ou contrainte
    integer row,k,i,j,PosIncCal
   
    ! Initialisation des matrices *********************************************

    MC%A=0.0D0
    MC%AtPA=0.0D0
    MC%AtPB=0.0D0 
    
    row=param%Nb_sta ! sert à placer correctement les inconnues de drift après les inconnues de points
    
    ! Remplissage du modèle pour les observations relatives *******************
    
    do k=1,param%Nb_obsRel
        !if(mod(k,100)==0)write(0,*)'k =',k

        Obs_Rel = TabObsRel(k)
        
        ! Recuperation de la calibration approchée 
        PosIncCal = TabProfil(obs_rel%profil)%posInc
        if (PosIncCal == 0) then 
            do i=1,NGravimeter
                if ((TabGravi(i)%serial == TabProfil(obs_rel%profil)%serial) .and. &
                     & (TabGravi(i)%N .EQ. TabProfil(obs_rel%profil)%Ngravi)) then
                    Ck = TabGravi(i)%cf    
                end if
            end do
        else
            Ck = MC%X0(PosIncCal)
        end if 
        
        ! numeros des points (en fait numéro des inconnues)
        MC%stp(k) = obs_rel%numsta_AR
        MC%enp(k) = obs_rel%numsta_AV

        ! les temps sont ramenés à 0 en début de profil
        tb = obs_rel%mjd_AR - obs_rel%mjd0
        ta = obs_rel%mjd_AV - obs_rel%mjd0

        ! Les températires sont ramenées à 0 pour le premier point du profil
        kb = obs_rel%tempK_AR - obs_rel%temp_K0
        ka = obs_rel%tempK_AV - obs_rel%temp_K0

        ! gravity difference
        if (obs_rel%numsta_AR/=obs_rel%numsta_AV) then
            MC%A(k,obs_rel%numsta_AR) =  1.D0 / Ck
            MC%A(k,obs_rel%numsta_AV) = -1.D0 / Ck
        end if
        
        ! dérive liée au temps
        if(param%drift_t.gt.0) then
            row = MC%Nb_sta+(obs_rel%profil-1)*(param%drift_t+param%drift_k)
            drt = 0.0
            do i=1,param%drift_t
                row = row+1
                MC%A(k,row) = (tb - ta)**i 
                drt = drt + MC%X0(row) * ((tb - ta)**i)
            end do
        end if

        ! dérive liée à la température
        if(param%drift_k.gt.0) then
            row=MC%Nb_sta+(obs_rel%profil-1)*(param%drift_t+param%drift_k) + param%drift_t
            drk = 0.0
            do i=1,param%drift_k
                row=row+1
                MC%A(k,row)=(kb - ka)**i 
                drk = drk + MC%X0(row) * ((kb - ka)**i)
            end do
        end if
        
        ! inconnue de calibration du gravimètre relatif
        if (PosIncCal /= 0) then ! on cherche le profil pour savoir si on estime la calib du gravi en question
            gAR = MC%X0(obs_rel%numsta_AR)
            gAV = MC%X0(obs_rel%numsta_AV)
            !MC%A(k,PosIncCal) = - (gAR - gAV + drt + drk) / (Ck**2)
            MC%A(k,PosIncCal) = - (gAR - gAV) / (Ck**2)
        end if

        ! Matrice de poids (en fait sa diagonale)
        std_obs = SQRT(obs_rel%SD_AR**2+obs_rel%SD_AV**2)
        MC%p(k)=1/std_obs**2

        ! vecteur des observations
        fX0 = ((MC%X0(obs_rel%numsta_AR) - MC%X0(obs_rel%numsta_AV)) / Ck )+ drt + drk
        MC%B(k)= (obs_rel%grav_AR - obs_rel%grav_AV) - fX0
    end do
    
    ! Ajout des observations absolues *****************************************
    ! on ajoute la première observation absolue (suppression du défaut de rang)
    ! ou plusieurs si on fait un calcul contraint 
    do k = 1, Mc%Nb_obsAbs
        row = pos(k)
        MC%A(MC%Nb_obsRel+k,row)=1 
        MC%P(MC%Nb_obsRel+k)= 1 / stdx(k)**2
        MC%B(MC%Nb_obsRel+k) = fixgra(k) - MC%X0(row) 
        MC%pos(k)=pos(k)  
    end do
    
       
    ! Normalisation lourde et extreeeeeeeeeeeemement longue sur les gros systèmes
    !call AtPA_AtPB_pousselabrouette(MC)
    
    ! Normalisation par enrichissement
    call Normalise_cal(MC)
    
    Rempli_Mc_Cal = 0

end function Rempli_Mc_Cal



! Procedure qui initialise un certain nombre de variables de l'objet MC
! Construction de la matrice modèle A, du vecteur des observations B, du vecteur des poids P, de AtPA, AtPB%%%
subroutine create_Mc(MC,type_calc)
    use Raw_Data
    use MC_data
    use param_data
    use write_matr
    
    implicit none
    type (Tmc), intent(inout):: MC
    integer type_calc ! correspond au mode  : 1 pour libre, 2 pour contraint

    real*8 ka,kb,grav,ta,tb,std_obs
    integer mode ! on charge differemment A si on est en solution libre ou contrainte
    integer row,k,i,j
    
    MC%Nb_obsRel = param%Nb_obsRel
    MC%Nb_sta = param%Nb_sta
    MC%Nb_profil = Nb_profil
    MC%degre_k = param%drift_k
    MC%degre_t = param%drift_t
    MC%ngravi_Cal = 0 ! Pour l'instant cette subroutine ne gère que le cas sans estimation de la calibration
    MC%Nb_inc = MC%Nb_sta + MC%Nb_profil * (MC%degre_k + MC%degre_t) + MC%ngravi_Cal
    param%Nb_inc = MC%Nb_inc
    
    ! Controle de la desallocation pour pouvoir lancer deux fois la dll *******
    
    if (Associated(MC%B)) deallocate(MC%B)
    if (Associated(MC%A)) deallocate(MC%A)
    if (Associated(MC%P)) deallocate(MC%P)
    if (Associated(MC%V)) deallocate(MC%V)
    if (Associated(MC%AtPA)) deallocate(MC%AtPA)
    if (Associated(MC%AtPB)) deallocate(MC%AtPB)
    if (Associated(MC%X)) deallocate(MC%X)
    if (Associated(MC%stp)) deallocate(MC%stp)
    if (Associated(MC%enp)) deallocate(MC%enp)
    if (Associated(MC%pos)) deallocate(MC%pos)
    if (Associated(MC%stat)) deallocate(MC%stat)
        
    ! Allocation des matrices *************************************************

    if (type_calc==1) then
    
        MC%Nb_obsAbs = 1
        MC%dof = MC%Nb_obsRel + 1 - MC%Nb_inc
        
        allocate (MC%B(param%Nb_obsRel+1))
        allocate (MC%A(param%Nb_obsRel+1,param%Nb_inc))
        allocate (MC%P(param%Nb_obsRel+1))
        allocate (MC%V(param%Nb_obsRel+1))
        allocate (MC%atpa(param%Nb_inc,param%Nb_inc))
        allocate (MC%atpB(param%Nb_inc))
        allocate (MC%X(param%Nb_inc))
        
        allocate (MC%stp(param%Nb_obsRel))
        allocate (MC%enp(param%Nb_obsRel))
        allocate (MC%pos(1))
        allocate (MC%stat(param%Nb_sta))

    else if (type_calc>=2) then
    
        MC%Nb_obsAbs = param%Nb_obsAbs
        MC%dof = MC%Nb_obsRel + MC%Nb_obsAbs - MC%Nb_inc
		
		!write(0,*)param%Nb_obsRel+param%Nb_obsAbs, param%Nb_inc, (param%Nb_obsRel+param%Nb_obsAbs)* param%Nb_inc
        
        allocate (MC%B(param%Nb_obsRel+param%Nb_obsAbs))
        allocate (MC%A(param%Nb_obsRel+param%Nb_obsAbs,param%Nb_inc))
        allocate (MC%P(param%Nb_obsRel+param%Nb_obsAbs))
        allocate (MC%V(param%Nb_obsRel+param%Nb_obsAbs))
        allocate (MC%atpa(param%Nb_inc,param%Nb_inc))
        allocate (MC%atpB(param%Nb_inc))
        allocate (MC%X(param%Nb_inc))
        
        allocate (MC%stp(param%Nb_obsRel))
        allocate (MC%enp(param%Nb_obsRel))
        allocate (MC%pos(param%Nb_obsAbs))
        allocate (MC%stat(param%Nb_sta))
    
    end if
    
    ! Initialisation des matrices *********************************************

    MC%A=0.d0
    MC%atpa=0.d0
    MC%atpB=0.d0
    
    do k=1,param%Nb_sta
        MC%stat(k) = stat(k)
    end do
    
    row=param%Nb_sta ! sert à placer correctement les inconnues de drift après les inconnues de points
    
    ! Remplissage du modèle pour les observations relatives *******************
    
    do k=1,param%Nb_obsRel
        !if(mod(k,100)==0)write(0,*)'k =',k

        Obs_Rel = TabObsRel(k)
        
        ! numeros des points (en fait numéro des inconnues)
        MC%stp(k) = obs_rel%numsta_AR
        MC%enp(k) = obs_rel%numsta_AV

        ! les temps sont ramenés à 0 en début de profil
        tb = obs_rel%mjd_AR - obs_rel%mjd0
        ta = obs_rel%mjd_AV - obs_rel%mjd0

        ! Les températires sont ramenées à 0 pour le premier point du profil
        kb = obs_rel%tempK_AR - obs_rel%temp_K0
        ka = obs_rel%tempK_AV - obs_rel%temp_K0

        ! gravity difference
        if (obs_rel%numsta_AR/=obs_rel%numsta_AV) then
            MC%A(k,obs_rel%numsta_AR) =  1.D0
            MC%A(k,obs_rel%numsta_AV) = -1.D0
        end if
        
        ! dérive liée au temps
        if(param%drift_t.gt.0) then
            row = MC%Nb_sta+(obs_rel%profil-1)*(param%drift_t+param%drift_k)
            do i=1,param%drift_t
                row = row+1
                !write(67,*)'degre_t ',row
                MC%A(k,row) = (tb - ta)**i
            end do
        end if

        ! dérive liée à la température
        if(param%drift_k.gt.0) then
            row=MC%Nb_sta+(obs_rel%profil-1)*(param%drift_t+param%drift_k) + param%drift_t
            do i=1,param%drift_k
                row=row+1
                !write(67,*)'degre_k ',row
                MC%A(k,row)=(kb - ka)**i
            end do
        end if

        ! Matrice de poids (en fait sa diagonale)
        std_obs = SQRT(obs_rel%SD_AR**2+obs_rel%SD_AV**2)
        MC%p(k)=1/std_obs**2

        ! vecteur des observations
        MC%B(k)= obs_rel%Cf * (obs_rel%grav_AR - obs_rel%grav_AV)

    end do
    
    ! Ajout des observations absolues *****************************************
    ! on ajoute la première observation absolue (suppression du défaut de rang) 
    do k = 1, Mc%Nb_obsAbs
        row = pos(k)
        MC%A(MC%Nb_obsRel+k,row)=1 
        MC%P(MC%Nb_obsRel+k)= 1 / stdx(k)**2
        MC%B(MC%Nb_obsRel+k) = fixgra(k)  
        MC%pos(k)=pos(k)  
    end do
    
    !Normalisation lourde et extreeeeeeeeeeeemement longue sur les gros systèmes
    !call AtPA_AtPB_pousselabrouette(MC)
    
    ! Normalisation par enrichissement
    call Normalise(MC)
    
    if (param%writemat) call write_mat(MC%A,MC%Nb_obsRel+1,MC%Nb_inc,'Matrice_A.txt')
    
    
    ! Allocation de l'inverse de AtPA *****************************************
    allocate (MC%sx(param%Nb_inc,param%Nb_inc),MC%sig(param%Nb_inc))


end subroutine create_Mc


! Remplissage de AtPA et AtPB par enrichissement ******************************
subroutine Normalise(MC)
    use Raw_Data
    use MC_data
    use param_data
    use write_matr
    
    implicit none
    type (Tmc), intent(inout):: MC
    real*8 ka,kb,grav,ta,tb,std_obs,wg,Cf,observ
    integer mode ! on charge differemment A si on est en solution libre ou contrainte
    integer row,k,i,j,l

    real*8,allocatable,dimension(:) :: vect_drift
    real*8,allocatable,dimension(:,:) :: bloc_drift
    integer Nbloc_drift
    
    Nbloc_drift = MC%degre_k + MC%degre_t
  
    if (allocated(vect_drift)) deallocate(vect_drift)
    if (allocated(bloc_drift)) deallocate(bloc_drift)
    allocate(vect_drift(Nbloc_drift))
    allocate(bloc_drift(Nbloc_drift,Nbloc_drift))
    
    MC%AtPA = 0.0
    MC%AtPB = 0.0
    
    do k=1,param%Nb_obsRel
        !if(mod(k,100)==0)write(0,*)'k =',k

        Obs_Rel = TabObsRel(k)
        
        ! numeros des points (en fait numéro des inconnues)
        MC%stp(k) = obs_rel%numsta_AR
        MC%enp(k) = obs_rel%numsta_AV

        ! les temps sont ramenés à 0 en début de profil
        tb = obs_rel%mjd_AR - obs_rel%mjd0
        ta = obs_rel%mjd_AV - obs_rel%mjd0

        ! Les températires sont ramenées à 0 pour le premier point du profil
        kb = obs_rel%tempK_AR - obs_rel%temp_K0
        ka = obs_rel%tempK_AV - obs_rel%temp_K0
        
        cf = Obs_Rel%Cf
      
        ! Remplissage du vecteur des paramètres de dérive
        ! dérive liée au temps
        if(MC%degre_t.gt.0) then
            row = MC%Nb_sta+(obs_rel%profil-1)*(MC%degre_t+MC%degre_k)
            do i=1,MC%degre_t
                row = row+1
                vect_drift(i) = (tb - ta)**i
            end do
        end if

        ! dérive liée à la température
        if(MC%degre_k.gt.0) then
            row=MC%Nb_sta+(obs_rel%profil-1)*(MC%degre_t+MC%degre_k)+ MC%degre_t
            do i=1,MC%degre_k
                row=row+1
                vect_drift(i+MC%degre_t) = (kb - ka)**i
            end do
        end if
        
        ! Matrice de poids (en fait sa diagonale)
        std_obs = SQRT(obs_rel%SD_AR**2+obs_rel%SD_AV**2)
        MC%p(k) = 1/std_obs**2
        wg = MC%p(k)
        
        ! On calcule le bloc carré correspondant aux dérives
        
        do i=1,Nbloc_drift 
            do j=1,Nbloc_drift
                bloc_drift(i,j) = wg * vect_drift(i) * vect_drift(j)    
            end do
        end do
        
        ! On affecte le bloc à AtPA
        row = MC%Nb_sta+(obs_rel%profil-1)*(MC%degre_t+MC%degre_k)
        do i=1,Nbloc_drift 
            do j=1,Nbloc_drift
                MC%AtPA(row+i,row+j) = MC%AtPA(row+i,row+j) + bloc_drift(i,j) !/Cf
            end do
        end do
        
        do i=1,Nbloc_drift
            MC%AtPA(row+i,MC%stp(k)) = MC%AtPA(row+i,MC%stp(k)) + wg * vect_drift(i) !/Cf 
            MC%AtPA(row+i,MC%enp(k)) = MC%AtPA(row+i,MC%enp(k)) - wg * vect_drift(i) !/Cf
            
            MC%AtPA(MC%stp(k),row+i) = MC%AtPA(row+i,MC%stp(k))
            MC%AtPA(MC%enp(k),row+i) = MC%AtPA(row+i,MC%enp(k)) 
        end do 
        
        if (MC%stp(k)/=MC%enp(k)) then 
            MC%AtPA(MC%stp(k),MC%stp(k)) = MC%AtPA(MC%stp(k),MC%stp(k)) + wg !/Cf
            MC%AtPA(MC%enp(k),MC%enp(k)) = MC%AtPA(MC%enp(k),MC%enp(k)) + wg !/Cf
            MC%AtPA(MC%stp(k),MC%enp(k)) = MC%AtPA(MC%stp(k),MC%enp(k)) - wg !/Cf
            MC%AtPA(MC%enp(k),MC%stp(k)) = MC%AtPA(MC%stp(k),MC%enp(k))
        end if
        
        ! Calcul de AtPB
        observ = obs_rel%Cf * (obs_rel%grav_AR - obs_rel%grav_AV) 
        !observ = (obs_rel%grav_AR - obs_rel%grav_AV) !/ obs_rel%Cf  
        MC%AtPB(MC%stp(k)) = MC%AtPB(MC%stp(k)) + wg * observ !/Cf
        MC%AtPB(MC%enp(k)) = MC%AtPB(MC%enp(k)) - wg * observ !/Cf
        do i=1,Nbloc_drift 
            MC%AtPB(row+i) = MC%AtPB(row+i) +  wg * vect_drift(i) * observ !/Cf  
        end do 

    end do
    
    ! Ajout des observations absolues *****************************************
    ! on ajoute la première observation absolue (suppression du défaut de rang) 
    
    do k = 1, Mc%Nb_obsAbs
        row = pos(k)
        MC%P(MC%Nb_obsRel+k)= 1 / stdx(k)**2
        wg = MC%P(MC%Nb_obsRel+k)
        MC%pos(k)=pos(k) 
        MC%AtPA(row,row) = MC%AtPA(row,row) + wg
        MC%AtPB(row) = MC%AtPB(row) + wg * fixgra(k)   
    end do
    
    if (param%writemat) then
        call write_mat(MC%AtPA,MC%Nb_inc,MC%Nb_inc,'AtPA_enrichissement.txt')
        call write_mat(MC%AtPB,MC%Nb_inc,1,'AtPB_enrichissement.txt')
    end if
     
    if (allocated(vect_drift)) deallocate(vect_drift)
    if (allocated(bloc_drift)) deallocate(bloc_drift)
    
end subroutine Normalise

! Normalisation par enrichissement avec estimation de la calibration **********
subroutine Normalise_cal(MC)
    use Raw_Data
    use MC_data
    use param_data
    use write_matr
    
    implicit none
    type (Tmc), intent(inout):: MC
    integer type_calc ! correspond au mode  : 1 pour libre, 2 pour contraint

    real*8 ka,kb,grav,ta,tb,std_obs ,Ck,dCal
    real*8 wg, observ, fX0, drt, drk
    integer mode ! on charge differemment A si on est en solution libre ou contrainte
    integer row,i,j,PosIncCal
    integer*8 k
    
    real*8,allocatable,dimension(:) :: vect_drift
    real*8,allocatable,dimension(:,:) :: bloc_drift
    integer Nbloc_drift
    
    Nbloc_drift = MC%degre_k + MC%degre_t
  
    if (allocated(vect_drift)) deallocate(vect_drift)
    if (allocated(bloc_drift)) deallocate(bloc_drift)
    
    i = 0
    do k=1,100000000000
        i = i +1 
    end do 
    
    allocate(vect_drift(Nbloc_drift))
    allocate(bloc_drift(Nbloc_drift,Nbloc_drift))
    
    if (param%writemat) then 
        call write_vec(MC%P,param%Nb_obsRel+param%Nb_obsAbs,'Pcal_enrichissement.txt')
    end if
    
    ! Initialisation des matrices *********************************************
    MC%AtPA = 0.0
    MC%AtPB = 0.0

    row=param%Nb_sta ! sert à placer correctement les inconnues de drift après les inconnues de points
    
    ! Remplissage du modèle pour les observations relatives *******************
    
    do k=1,param%Nb_obsRel
        !if(mod(k,100)==0)write(0,*)'k =',k

        Obs_Rel = TabObsRel(k)
        
        ! Recuperation de la calibration approchée 
        PosIncCal = TabProfil(obs_rel%profil)%posInc
        
        if (PosIncCal == 0) then 
            do i=1,NGravimeter
                if (TabGravi(i)%serial == TabProfil(obs_rel%profil)%serial) then
                    Ck = TabGravi(i)%cf    
                end if
            end do
        else
            Ck = MC%X0(PosIncCal)
        end if 
   
        ! numeros des points (en fait numéro des inconnues)
        MC%stp(k) = obs_rel%numsta_AR
        MC%enp(k) = obs_rel%numsta_AV

        ! les temps sont ramenés à 0 en début de profil
        tb = obs_rel%mjd_AR - obs_rel%mjd0
        ta = obs_rel%mjd_AV - obs_rel%mjd0

        ! Les températires sont ramenées à 0 pour le premier point du profil
        kb = obs_rel%tempK_AR - obs_rel%temp_K0
        ka = obs_rel%tempK_AV - obs_rel%temp_K0

        ! gravity difference
        !if (obs_rel%numsta_AR/=obs_rel%numsta_AV) then
        !    MC%A(k,obs_rel%numsta_AR) =  1%D0 / Ck
        !    MC%A(k,obs_rel%numsta_AV) = -1%D0 / Ck
        !end if
        
        ! dérive liée au temps
        if(param%drift_t.gt.0) then
            drt = 0.0d0
            row = MC%Nb_sta+(obs_rel%profil-1)*(param%drift_t+param%drift_k)
            do i=1,param%drift_t
                row = row+1
                vect_drift(i) = ((tb - ta)**i) / Ck
                !MC%A(k,row) = (tb**i - ta**i) / Ck
                drt = drt + MC%X0(row) * ((tb - ta)**i)
            end do
        end if

        ! dérive liée à la température
        if(param%drift_k.gt.0) then
            drk = 0.0d0
            row=MC%Nb_sta+(obs_rel%profil-1)*(param%drift_t+param%drift_k) + param%drift_t
            do i=1,param%drift_k
                row=row+1
                vect_drift(i+MC%degre_t) = ((kb - ka)**i) / Ck
                drk = drk + MC%X0(row) * ((kb - ka)**i)
                !MC%A(k,row)=(kb**i - ka**i) / Ck
            end do
        end if
        
        ! inconnue de calibration du gravimètre relatif
        if (PosIncCal /= 0) then ! on cherche le profil pour savoir si on estime la calib du gravi en question
            dCal = - (obs_rel%grav_AR - obs_rel%grav_AV) / Ck
            !MC%A(k,PosIncCal) = - (obs_rel%grav_AR - obs_rel%grav_AV) / Ck
        end if
       
        ! Matrice de poids (en fait sa diagonale)
        std_obs = SQRT(obs_rel%SD_AR**2+obs_rel%SD_AV**2)
        MC%p(k) = 1/std_obs**2
        wg = MC%p(k)
        
        ! On calcule le bloc carré correspondant aux dérives
        
        do i=1,Nbloc_drift 
            do j=1,Nbloc_drift
                bloc_drift(i,j) = wg * vect_drift(i) * vect_drift(j)    
            end do
        end do
        
        ! On affecte le bloc à AtPA
        row = MC%Nb_sta+(obs_rel%profil-1)*(MC%degre_t+MC%degre_k)
        do i=1,Nbloc_drift 
            do j=1,Nbloc_drift
                !write(0,*)k,row,i,j,bloc_drift(i,j) 
                MC%AtPA(row+i,row+j) = MC%AtPA(row+i,row+j) + bloc_drift(i,j) 
            end do
        end do
        
        do i=1,Nbloc_drift
        
            MC%AtPA(row+i,MC%stp(k)) = MC%AtPA(row+i,MC%stp(k)) + wg * vect_drift(i) 
            MC%AtPA(row+i,MC%enp(k)) = MC%AtPA(row+i,MC%enp(k)) - wg * vect_drift(i) 
            
            MC%AtPA(MC%stp(k),row+i) = MC%AtPA(row+i,MC%stp(k))
            MC%AtPA(MC%enp(k),row+i) = MC%AtPA(row+i,MC%enp(k)) 
            
            if (PosIncCal /= 0) then 
                MC%AtPA(PosIncCal,row+i) = MC%AtPA(PosIncCal,row+i) + wg * dCal * vect_drift(i)
                MC%AtPA(row+i,PosIncCal) = MC%AtPA(PosIncCal,row+i)       
            end if
            
        end do 
        
        if (PosIncCal /= 0) then   
                MC%AtPA(PosIncCal,MC%stp(k)) = MC%AtPA(PosIncCal,MC%stp(k)) + wg * dCal / Ck
                MC%AtPA(PosIncCal,MC%enp(k)) = MC%AtPA(PosIncCal,MC%enp(k)) - wg * dCal / Ck
                
                MC%AtPA(MC%stp(k),PosIncCal) = MC%AtPA(PosIncCal,MC%stp(k))
                MC%AtPA(MC%enp(k),PosIncCal) = MC%AtPA(PosIncCal,MC%enp(k))          
                
                MC%AtPA(PosIncCal,PosIncCal) = MC%AtPA(PosIncCal,PosIncCal) + wg * dCal * dCal
        end if 
        
        
        if (MC%stp(k)/=MC%enp(k)) then 
            MC%AtPA(MC%stp(k),MC%stp(k)) = MC%AtPA(MC%stp(k),MC%stp(k)) + wg / Ck / Ck
            MC%AtPA(MC%enp(k),MC%enp(k)) = MC%AtPA(MC%enp(k),MC%enp(k)) + wg / Ck / Ck
            MC%AtPA(MC%stp(k),MC%enp(k)) = MC%AtPA(MC%stp(k),MC%enp(k)) - wg / Ck / Ck
            MC%AtPA(MC%enp(k),MC%stp(k)) = MC%AtPA(MC%stp(k),MC%enp(k))
        end if
        
        ! Calcul de AtPB
        fX0 = ((MC%X0(obs_rel%numsta_AR) - MC%X0(obs_rel%numsta_AV)) / Ck )+ drt + drk

        observ = (obs_rel%grav_AR - obs_rel%grav_AV) - fX0
        !write(0,*)observ,wg,Ck
        MC%AtPB(MC%stp(k)) = MC%AtPB(MC%stp(k)) + wg * observ / Ck
        MC%AtPB(MC%enp(k)) = MC%AtPB(MC%enp(k)) - wg * observ / Ck
        do i=1,Nbloc_drift 
            MC%AtPB(row+i) = MC%AtPB(row+i) +  wg * vect_drift(i) * observ  
            !write(0,*)obs,wg,Ck 
        end do 
        
        if (PosIncCal /= 0) then  
            MC%AtPB(PosIncCal) = MC%AtPB(PosIncCal) + dCal * wg * observ
        end if

        ! vecteur des observations
        !MC%B(k)= (obs_rel%grav_AR - obs_rel%grav_AV) - (MC%X0(obs_rel%numsta_AR) - MC%X0(obs_rel%numsta_AV))

    end do
    
    ! Ajout des observations absolues *****************************************
    ! on ajoute la première observation absolue (suppression du défaut de rang)
    ! ou plusieurs si on fait un calcul contraint 
   
    do k = 1, Mc%Nb_obsAbs
        row = pos(k)
        !MC%A(MC%Nb_obsRel+k,row)=1 
        MC%P(MC%Nb_obsRel+k)= 1 / stdx(k)**2
        wg = MC%P(MC%Nb_obsRel+k)
        !MC%B(MC%Nb_obsRel+k) = fixgra(k)  
        MC%pos(k)=pos(k) 
        MC%AtPA(row,row) = MC%AtPA(row,row) + wg
        MC%AtPB(row) = MC%AtPB(row) + wg * (fixgra(k) - MC%X0(row))
    end do
    
    if (param%writemat) then
        call write_mat(MC%AtPA,MC%Nb_inc,MC%Nb_inc,'AtPACal_enrichissement.txt')
        call write_mat(MC%AtPB,MC%Nb_inc,1,'AtPBCal_enrichissement.txt')
    end if

    if (allocated(vect_drift)) deallocate(vect_drift)
    if (allocated(bloc_drift)) deallocate(bloc_drift)
    
end subroutine Normalise_Cal

! Calcul de AtPA et AtPL ******************************************************
! Pas très elegant et long en calcul : à modifier
subroutine AtPA_AtPB_pousselabrouette(MC)
    use MC_data
    use param_data
    use write_matr
    
    implicit none
    type (Tmc), intent(inout):: MC
    integer i,k,j
    
    write(0,*)"Normalisation 'Pousse la brouette'"

    do k = 1,MC%Nb_obsRel+MC%Nb_obsAbs    
	    do i=1,MC%Nb_inc
		    do j=1,i
			    MC%AtPA(i,j)=MC%AtPA(i,j)+MC%A(k,i)*MC%A(k,j)*MC%P(k)
		    end	do
		    MC%atpB(i)=MC%atpB(i)+MC%A(k,i)*MC%B(k)*MC%p(k)
	    end	do
    end do 
    
    ! on rend AtPA symétrique *************************************************
    do i=1,MC%Nb_inc
        do j=1,i
            MC%AtPA(j,i)=MC%AtPA(i,j)
        end do
    end do
    
    if (param%writemat) then
        call write_mat(MC%AtPA,MC%Nb_inc,MC%Nb_inc,'AtPACal_pousse_la_brouette.txt')
        call write_mat(MC%AtPB,MC%Nb_inc,1,'AtPBCal_pousse_la_brouette.txt')
    end if
    
end subroutine AtPA_AtPB_pousselabrouette

end module rempli_MatMC
