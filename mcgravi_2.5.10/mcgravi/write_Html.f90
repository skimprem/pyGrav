module write_html

contains

subroutine Whtml_cpfic()
    use sys_utils
    use param_data
    implicit none

    integer lcmd, ls
    character (len=100) f_result
    logical(4) resultat
    integer(4) resultatI  
    
    ls=len_trim(param%nomficout)

    if (param%systeme=='LINUX') then
       f_result(1:6+ls) = param%nomficout(1:ls)//'.htm .'
       !resultatI = copy(f_result)
    else
       f_result(1:5+ls) = param%nomficout(1:ls)//'.htm'
       !resultatI = copy(f_result)
    endif
    
    !write(0,*)f_result

end subroutine Whtml_cpfic


subroutine Whtml_entete(num_vers)
    implicit none
    
    character (len=8) date
    character (len=10) heure
    character (len=40) num_vers
    !call W_ligne()
    
    write(70,*)'<HTML>'
    write(70,*)'<TITLE>Compensation des observations de gravimétrie absolue et relative</TITLE>'
    write(70,*)'<BODY bgcolor="#ffffff">'
    !write(70,*)'<CENTER><H2>Compensation des observations de gravimétrie absolue et relative</H2></CENTER><P><BR><HR>'
    
    write(70,*)"<P><CENTER><H2><A NAME=""Haut_Page"">Compensation des observations &
    &de gravimétrie absolue et relative</A></H2></CENTER></P><BR><HR>"
    
    write(70,*)'<H3><A NAME="anchor_theme">'
    write(70,*)num_vers
    write(70,*)'</H3>'
    !write(70,*)'Compensation des observations de gravimétrie absolue et relative<BR>'
    !write(70,*)'Méthode des moindres carrés<BR>'
    write(70,*)'Beilin Jacques - 2004-06<BR>'
    write(70,*)'IGN - IPGP<BR>'
   
    call date_and_time(date,heure)
    write(70,*)'Calcul effectué le ',date(7:8),'/',date(5:6),'/',date(1:4),' à ',heure(1:2),':',heure(3:4),':',heure(5:6),'<BR>'

end subroutine Whtml_entete

subroutine Whtml_menu(mode)
    use param_data
    implicit none
    logical trouve
    integer k,mode
    
    80 format (A130)
    
    ! tester si les données sont vraiment écrites (en fonction des options)
    write(70,*)"<P><A HREF=""#Coef_etal"">Coefficients d'étalonnage des gravimètres</A></P>"
    if (param%write_list_fic) write(70,*)"<P><A HREF=""#List_ficRel"">Fichiers d'observations relatives</A></P>"
    
    if (param%write_list_fic) then
    trouve = .false.
    do k=1,param%nDataFic
       if (param%TabDataFic(k)%typ == 1) trouve=.true.
    end do
    if (trouve) write(70,*)"<P><A HREF=""#List_ficAbs"">Fichiers d'observations absolues</A></P>"
    
    trouve = .false.
    do k=1,param%nDataFic
       if (param%TabDataFic(k)%typ == 2) trouve=.true.
    end do
    if (trouve) write(70,*)"<P><A HREF=""#List_ficA10"">Fichiers d'observations de gravimètre A10</A></P>"
    end if
    if (param%write_list_station) then
    write(70,*)"<P><A HREF=""#list_station"">Stations observées</A></P>"
    if (param%print_obs) then
        write(70,*)"<P><A HREF=""#obs_rel"">Observations Relatives</A></P>"
    end if
    end if 
        
    if (param%print_obs) then
        write(70,*)"<P><A HREF=""#diff_gravi"">Différences de pesanteur</A></P>"
        write(70,*)'<P><A HREF="#obs_abs">Observations absolues</A></P>'
    end if
    
    if (mode==1) then
        write(70,*)'<P><A HREF="#sol_libre">Solution libre</A></P>'
        write(70,*)"<TABLE CELLPADDING=10 BORDER=""0"">"
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#hist1.png">Histogramme</A></P></TD></TR>'
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#synthese1">Eléments statistiques</A></P></TD></TR>'
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#20grosresid1">20 plus gros résidus</A></P></TD></TR>'
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#tautest1">Tau-test sur les résidus</A></P></TD></TR>'
        if (param%write_gravity) write(70,80)'<TR><TD> </TD><TD><P><A HREF="#grav_comp1">Pesanteur compensée</A></P></TD></TR>'
        write(70,*)"</TABLE>"
    end if
    
    if (mode==2) then
        write(70,*)'<P><A HREF="#sol_contrainte">Solution contrainte</A></P>'
        write(70,*)"<TABLE CELLPADDING=10 BORDER=""0"">"
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#hist1.png">Histogramme</A></P></TD></TR>'
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#synthese1">Eléments statistiques</A></P></TD></TR>'
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#20grosresid1">20 plus gros résidus</A></P></TD></TR>'
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#tautest1">Tau-test sur les résidus</A></P></TD></TR>'
        if (param%write_gravity) write(70,80)'<TR><TD> </TD><TD><P><A HREF="#grav_comp1">Pesanteur compensée</A></P></TD></TR>'
        write(70,*)"</TABLE>"
    end if
    
    if (mode==2 .and. param%ngravi_Cal>0) then
        write(70,*)'<P><A HREF="#Sol_calib">Calcul avec estimation des calibrations</A></P>'
        write(70,*)"<TABLE CELLPADDING=10 BORDER=""0"">"
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#hist2.png">Histogramme</A></P>'
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#synthese2">Eléments statistiques</A></P></TD></TR>'
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#20grosresid2">20 plus gros résidus</A></P></TD></TR>'
        write(70,80)'<TR><TD> </TD><TD><P><A HREF="#tautest2">Tau-test sur les résidus</A></P></TD></TR>'
        if (param%write_gravity) write(70,80)'<TR><TD> </TD><TD><P><A HREF="#grav_comp2"&
        &>Pesanteur compensée avec estimation de la calibration</A></P></TD></TR>'
        if (param%write_drift) write(70,80)'<TR><TD> </TD><TD><P><A HREF=&
        &"#coeff_calib_out">Coefficients de calibration</A></P></TD></TR>'
        write(70,*)"</TABLE>"
    end if
      
end subroutine Whtml_menu


subroutine WHTML_cal_gravi()
    use param_data
    use raw_data
    implicit none
    integer k
    ! Ecriture des observations dans le fichier résultat
    
    write(70,*)"<BR><HR><A NAME=""Coef_etal""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><STRONG>Coefficients d'étalonnage des gravimètres (Valeurs initiales) </STRONG></CAPTION>"
    write(70,*)"<TR><TD>Numéro de série</TD><TD>Numéro dans CG3TOOL.init</TD><TD>Coefficient d'étalonnage</TD><TD></TD></TR>"
    101 format (A8,A8,A9,I2,A9,F12.6,A9,1x,A6,A10)
    do k=1,ngravimeter
        if (TabGravi(k)%Estimate) then
            write(70,101)"<TR><TD>",TabGravi(k)%Serial,"</TD><TD>", TabGravi(k)%Num,"</TD><TD>",&
            & TabGravi(k)%Cf,"</TD><TD>",'estimé',"</TD></TR>"
        else
            write(70,101)"<TR><TD>",TabGravi(k)%Serial,"</TD><TD>", TabGravi(k)%Num,"</TD><TD>",&
            & TabGravi(k)%Cf,"</TD><TD>",'connu',"</TD></TR>"
        end if
    end do
     
    write(70,*)"</TABLE>"

end subroutine WHTML_cal_gravi

subroutine WHTML_sigma_coeff()
    use param_data
    implicit none
    
    write(70,*)'<HR><H4>Modification des écarts-types a priori sur les fichiers relatifs</H4><BR>'
    write(70,'(1x,a10,f6.3,A4)')'Facteur = ',param%sigma_factor,'<BR>'
    write(70,'(1x,a16,f6.4,A4)')'Terme additif = ',param%sigma_add,'<BR>' 
end subroutine WHTML_sigma_coeff

subroutine WHTML_liste_nomficrel()
    use param_data
    implicit none
    integer k,LENrep, LENnomfic,w,z
    type (TDataFic) Dfic
    character (len=100) :: CHFMT
    character (len=60) nom_complet,rep,nomfic
    
    write(70,*)"<BR><HR><A NAME=""List_ficRel""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Fichiers d'observations relatives</H4></CAPTION>"
    write(70,*)"<TR><TD>Fichier</TD><TD>Facteur</TD><TD>Terme additif</TD><TD></TD></TR>"

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
            WRITE (CHFMT,500) '(A8,A',w,',A9,f6.1,A9,f6.3,A10)' 
            500  FORMAT (A,I2,A) 
            
            write(70,FMT=CHFMT)"<TR><TD>",nom_complet,"</TD><TD>", Dfic%Sigma_f,"</TD><TD>", Dfic%sigma_a,"</TD></TR>"
            !55 format (A8,A<w>,A9,f6.1,A9,f6.3,A10)
            
        end if
    end do
    write(70,*)"</TABLE>"
end subroutine WHTML_liste_nomficrel

subroutine WHTML_liste_nomficabs()
    use param_data
    implicit none
    integer k,w
    type (TDataFic) Dfic
    character (len=100) :: CHFMT
    logical trouve
    
    trouve = .false.
    do k=1,param%nDataFic
       Dfic = param%TabDataFic(k)
       if (Dfic%typ == 1) trouve=.true.
    end do
    
    if (.not. trouve) return
    
    
    write(70,*)"<BR><HR><A NAME=""List_ficAbs""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Fichiers d'observations absolues</H4></CAPTION>"
    write(70,*)"<TR><TD>Fichier</TD><TD>Facteur</TD><TD>Terme additif</TD><TD></TD></TR>"

    do k=1,param%nDataFic
    
        Dfic = param%TabDataFic(k)
        
        if (Dfic%typ == 1) then
            w = len_trim(Dfic%nom)
            WRITE (CHFMT,500) '(A10,A',w,',A10,f6.1,A10,f6.3,A10)' 
            500  FORMAT (A,I2,A) 
            write(70,FMT=CHFMT)"<TR><TD>",Dfic%nom,"</TD><TD>",Dfic%sigma_f,"</TD><TD>",Dfic%sigma_a,"</TD></TR>"
            !55 format (A10,A<w>,A10,f6.1,A10,f6.3,A10)        
        end if    
    end do
    write(70,*)"</TABLE>"
end subroutine WHTML_liste_nomficabs

subroutine WHTML_liste_nomficA10()
    use param_data
    implicit none
    integer k,w
    type (TDataFic) Dfic
    character (len=100) :: CHFMT
    
    logical trouve
    
    trouve = .false.
    do k=1,param%nDataFic
       Dfic = param%TabDataFic(k)
       if (Dfic%typ == 2) trouve=.true.
    end do 
    if (.not. trouve) return
    
    write(70,*)"<BR><HR><A NAME=""List_ficA10""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Fichiers d'observations de gravimètre A10</H4></CAPTION>"
    write(70,*)"<TR><TD>Fichier</TD><TD>Facteur</TD><TD>Terme additif</TD><TD></TD></TR>"

    do k=1,param%nDataFic
    
        Dfic = param%TabDataFic(k)
        
        if (Dfic%typ == 2) then
            w = len_trim(Dfic%nom)
            WRITE (CHFMT,500) '(A10,A',w,',A10,f6.1,A10,f6.3,A10)' 
            500  FORMAT (A,I2,A) 
            write(70,FMT=CHFMT)"<TR><TD>",Dfic%nom,"</TD><TD>",Dfic%sigma_f,"</TD><TD>",Dfic%sigma_a,"</TD></TR>"
            !55 format (A10,A<w>,A10,f6.1,A10,f6.3,A10)        
        end if
        
    end do
    write(70,*)"</TABLE>"
end subroutine WHTML_liste_nomficA10

subroutine WHTML_observation_rel()
    use param_data
    use raw_data
    implicit none
    integer k,nprofil,w
    character (len=80) :: chfmt
    
    ! Ecriture des observations dans le fichier résultat
    
    write(70,*)"<BR><HR><A NAME=""obs_rel""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<H4>Observations Relatives</H4>"
   
    
    nprofil = 0
    do k=1,nTabObs
        obs=TabObs(k)
        
        if (obs%profil/=nprofil) then
            if (k>0)  write(70,*)"</TABLE>"
            w = len_trim(tabprofil(obs%profil)%nomfic)
            write(chfmt,500)'(A13,A',w,',A6)'
            500 format(A,I2,A)

            write(70,FMT=chfmt)"<BR><A NAME=""",tabprofil(obs%profil)%nomfic,"""></A>"
            write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
            write(70,'(A13,A7,1x,I2,4x,A9,A20,1x,A6,F10.6,A15)')"<CAPTION><H4>",'Profil ',obs%profil,' fichier ',&
            &tabprofil(obs%profil)%nomfic,' Cf = ',obs%Cf,"</H4></CAPTION>"
            write(70,*)'<TR><TD>Nom</TD><TD> Pesanteur(mGal)</TD><TD> SD(mGal)&
            &</TD><TD> Temp(mK)</TD><TD> t (jour decimal)&
            &</TD><TD> Date</TD><TD> Heure</TD></TR>'
            nprofil = obs%profil
        end if 
        
        write(70,'(A12,A8,A12,f9.3,A12,f6.3,A12,f5.2,A12,f9.5,A12,I6,A12,I6,A12)')&
            &"<TR><TD>",obs%nomsta,"</TD><TD>",obs%grav,"</TD><TD>",obs%sd,"</TD><TD>",obs%tempK,"</TD><TD>",&
            &obs%mjd,"</TD><TD>",obs%date,"</TD><TD>",obs%heure,"</TD></TR>"
    end do
    
    write(70,*)"</TABLE>"
end subroutine WHTML_observation_rel

subroutine WHTML_liste_station2()
    use param_data
    use raw_data
    implicit none
    integer k
    
    write(70,*)"<BR><HR><A NAME=""list_station""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Stations observées</H4></CAPTION>"
    write(70,*)"<TR><TD>Nom</TD><TD>Longitude</TD><TD>Latitude</TD><TD>Numéro de l'inconnue</TD></TR>"

    do k=1,NTabStation
        write(70,'(A12,A8,A12,f14.8,A12,f14.8,A12,I12,A12)')"<TR><TD>",TabStation(k)%nomsta,&
        &"</TD><TD>",TabStation(k)%lon,"</TD><TD>",TabStation(k)%lat&
        &,"</TD><TD>",TabStation(k)%numsta,"</TD></TR>"
    end do
    write(70,*)"</TABLE>"
end subroutine WHTML_liste_station2

subroutine WHTML_difference_gravite()
    use param_data
    use raw_data
    implicit none
    integer k,nprofil,w
    character(len=80) :: chfmt
    real*8 sigma
    nprofil = 0
    
    write(70,*)"<BR><HR><A NAME=""diff_gravi""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<H4>Différences de Pesanteur</H4>"
  
    do k=1,param%Nb_obsRel

        Obs_Rel=TabObsRel(k)
        if (obs_Rel%profil/=nprofil) then
            if (k>0)  write(70,*)"</TABLE>"
            w = len_trim(tabprofil(obs%profil)%nomfic)
            
            write(chfmt,500)'(A13,A',w,',A6)'
            500 format(A,I2,A)
            write(70,FMT=chfmt)"<BR><A NAME=""",tabprofil(obs%profil)%nomfic,"""></A>"
            write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"

            101 format (A15,A7,1x,I2,2x,A7,1x,A12,2x,A5,F10.6,2x,A5,F9.5,2x,A5,F5.2,A15)
            write(70,101)"<CAPTION><H4>",&
                &'Profil ',obs_rel%profil,&
                &'fichier',tabprofil(obs_rel%profil)%nomfic,&
                &'Cf = ',obs_rel%Cf,&
                &'t0 = ',obs_rel%mjd0,&
                &'K0 = ',obs_rel%temp_K0,"</H4></CAPTION>"
                
            write(70,*)'<TR><TD>Point 1</TD><TD>Point 2</TD><TD>Dg (mGal)</TD><TD>g1 (mGal)</TD><TD>g2 (mGal)&
            &</TD><TD>Ecart-type</TD><TD>T1</TD><TD>T2</TD><TD> Date 1</TD><TD> Date 2</TD></TR>'

            nprofil = obs_Rel%profil
        end if 
        
        sigma = SQRT(obs_rel%SD_AR**2+obs_rel%SD_AV**2)
        102 format (A8,A8,A9,A8,A9,f9.3,A9,f9.3,A9,f9.3,A9,f6.3,A9,F5.2,A9,f5.2,A9,F9.5,A9,F9.5,A10)
        write(70,102)&
                        &"<TR><TD>",obs_rel%nomsta_AR,"</TD><TD>",&
                        &obs_rel%nomsta_AV,"</TD><TD>",&
                        &obs_rel%grav_AR-obs_rel%grav_AV,"</TD><TD>",&
                        &obs_rel%grav_AR,"</TD><TD>",&
                        &obs_rel%grav_AV,"</TD><TD>",&
                        &sigma,"</TD><TD>",&
                        &obs_rel%tempK_AR,"</TD><TD>",&
                        &obs_rel%tempK_AV,"</TD><TD>",&
                        &obs_rel%mjd_Ar,"</TD><TD>",&
                        &obs_rel%mjd_AV,"</TD></TR>"
        
    end do
    write(70,*)"</TABLE>"

end subroutine WHTML_difference_gravite

subroutine WHTML_sol_contrainte()   
    write(70,*)"<BR><HR><P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)'<H3><A NAME="sol_contrainte"></A>'

    write(70,*)"Solution contrainte"
    write(70,*)'</H3>'  
end subroutine WHTML_sol_contrainte

subroutine WHTML_sol_libre()
    write(70,*)"<BR><HR><P><H4><A HREF=""#Haut_Page"">Sommaire</A><H4></P>"
    write(70,*)'<H3><A NAME="sol_libre"></A>'
    write(70,*)'Solution Libre'
    write(70,*)'</H3>'  
    write(67,*)'Solution Libre'
    WRITE(70,*)'Information : le calcul en solution libre cale les pesanteurs'
    WRITE(70,*)'sur le premier point du premier fichier d''observations absolues'
end subroutine WHTML_sol_libre

subroutine WHTML_sol_calib()
    write(70,*)"<BR><HR><P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)'<H3><A NAME="Sol_calib"></A>'

    write(70,*)"Calcul avec estimation des calibrations"
    write(70,*)'</H3>'  
end subroutine WHTML_sol_calib

subroutine WHTML_gravi_abs()
    use param_data
    use raw_data
    implicit none
    integer k
    
    write(70,*)"<BR><HR><A NAME=""obs_abs""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Observations absolues</H4></CAPTION>"
    write(70,*)"<TR><TD>Nom</TD><TD>Pesanteur (mGal)</TD><TD>SD (mgal)</TD></TR>"

	do k=1,param%Nb_obsAbs
	    write(70,'(A8,A8,A9,f11.3,A9,F6.3,A10)')"<TR><TD>",fixstn(k),&
	    &"</TD><TD>",fixgra(k),"</TD><TD>",stdx(k),"</TD></TR>"
	end do
    write(70,*)"</TABLE>"
end subroutine WHTML_gravi_abs


subroutine WHTML_param(MC)
    use MC_data
    use param_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i
    
    write(70,*)"<BR><A NAME=""param_calc""></A>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Paramètres de calcul</H4></CAPTION>"
   
    write(70,103)"<TR><TD>",'Nombre de stations                                 ',&
    &"</TD><TD>",MC%Nb_sta,"</TD></TR>"
    write(70,103)"<TR><TD>",'Nombre de profils                                  ',&
    &"</TD><TD>",MC%Nb_profil,"</TD></TR>"
    write(70,103)"<TR><TD>",'Degré du polynome de dérive liée au temps          ',&
    &"</TD><TD>",MC%degre_t,"</TD></TR>"
    write(70,103)"<TR><TD>",'Degré du polynome de dérive liée à la température  ',&
    &"</TD><TD>",MC%degre_k,"</TD></TR>"

    write(70,103)"<TR><TD>",'Nombre d''inconnues                    ',&
    &"</TD><TD>",MC%Nb_inc,"</TD></TR>"

    write(70,103)"<TR><TD>",'Nombre d''observations relatives       ',&
    &"</TD><TD>",MC%Nb_obsRel,"</TD></TR>"
    if (param%lfix) then 
        write(70,103)"<TR><TD>",'Nombre d''observations absolues    ',&
        &"</TD><TD>",MC%Nb_obsAbs,"</TD></TR>"
    end if 

                   
    write(70,103)"<TR><TD>",'Degré de liberté du système            ',"</TD><TD>",Mc%dof,"</TD></TR>"

    103 format(A8,a52,A9,I4,A10)
    write(70,*)"</TABLE>"    
end subroutine WHTML_param


 
 subroutine WHTML_histo(nomfic_png)
    implicit none
    character (len=*) nomfic_png
    integer w
    character (len=80)chfmt 
    
    w = len_trim(nomfic_png)
    write(chfmt,500)'(A17,A',w,',A5)'
    500 format (A,I3,A)
    write(70,FMT=chfmt)"<BR><HR><A NAME=""",nomfic_png,"""></A>"
    
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    
    write(chfmt,500)'(A45,A',w,',A1)'
    write(70,FMT=chfmt)'<IMG WIDTH="600" HEIGHT="800" BORDER="0" SRC=',nomfic_png,'>'
        
end subroutine WHTML_histo


subroutine WHTML_gravity(MC)
    use str_const
    use param_data

    use MC_data
    implicit none
    
    type (Tmc), intent(in):: MC
    integer i
    
    write(70,*)"<BR><HR><A NAME=""grav_comp1""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Pesanteur compensée</H4></CAPTION>"
    write(70,*)"<TR><TD>Nom</TD><TD>Pesanteur compensée (mGal)</TD><TD>SD (mGal)</TD></TR>"

    DO I=1,MC%Nb_sta
        WRITE(70,101)"<TR><TD>",MC%stat(i),"</TD><TD>",MC%X(i),"</TD><TD>",MC%Sig(i),"</TD></TR>"
        101 format (A8,a8,A9,F12.3,A9,f9.3,A10)
    END DO
    
    write(70,*)"</TABLE>"    
end subroutine WHTML_gravity

subroutine WHTML_Ecart(MC)
    use str_const
    use param_data

    use MC_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i
    
    write(70,*)"<BR><HR><A NAME=""grav_comp2""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Pesanteur compensée</H4></CAPTION>"
    write(70,*)"<TR><TD>Nom</TD><TD>Pesanteur compensée (mGal)</TD><TD>",&
    &"Ecart avec la solution sans calibration (mGal)</TD><TD>SD (mGal)</TD></TR>"

    DO I=1,MC%Nb_sta
        WRITE(70,101)"<TR><TD>",MC%stat(i),"</TD><TD>",MC%X(i),&
        &"</TD><TD>",MC%sol_sans_calib(i)-MC%X(i),"</TD><TD>",MC%Sig(i),"</TD></TR>"
        101 format (A8,a8,A9,F12.3,A9,F12.3,A9,f9.3,A10)
    END DO
    
    write(70,*)"</TABLE>"    
end subroutine WHTML_Ecart

subroutine WHTML_drift(MC)
    use MC_data
    use param_data
    use raw_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i,k,row,n
    
    if (param%drift_t==0 .and. param%drift_k==0) then 
        return
    end if
    
    n = param%drift_t + param%drift_k
    
    write(70,*)"<BR><HR><A NAME=""drift""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Dérives</H4></CAPTION>"
    write(70,*)"<TR><TD>","Profil","</TD><TD>","Serial","</TD><TD>"
    if(param%drift_t.gt.0) then
        do i=1,param%drift_t
            write(70,'(A1,I1,A9)')"T",i,"</TD><TD>"
        end do
    end if
    if(param%drift_k.gt.0) then
        do i=1,param%drift_k
            if (i==param%drift_k) then 
                write(70,'(A1,I1)')"K",i
            else
                write(70,'(A1,I1,A9)')"K",i,"</TD><TD>"
            end if
        end do
    end if
    write(70,*)"</TD></TR>"
    
            ! Write drift parameters
    do k=1,MC%Nb_profil
        write(70,*)"<TR><TD>",k,"</TD><TD>",tabprofil(k)%serial,"</TD><TD>"

        ! dérive liée au temps
        if(param%drift_t.gt.0) then
            row=MC%Nb_sta+(k-1)*(param%drift_t+param%drift_k)
            do i=1,param%drift_t
                row=row+1
                write(70,'(f7.3,A2,f7.3,A1,A10)')MC%X(row)," (",MC%Sig(row),")","</TD><TD>"
            end do
        end if

        ! dérive liée à la température
        if(param%drift_k.gt.0) then
            row=MC%Nb_sta+(k-1)*(param%drift_t+param%drift_k) + param%drift_t
            do i=1,param%drift_k
                row=row+1
                if (i==param%drift_k) then
                    write(70,'(f7.3,A2,f7.3,A1)')MC%X(row),"(",MC%Sig(row),")"
                else
                    write(70,'(f7.3,A2,f7.3,A1,A10)')MC%X(row)," (",MC%Sig(row),")","</TD><TD>"
                end if
            end do
        end if

        write(70,*)"</TD></TR>"
    end do
    write(70,*)"</TABLE>"    
    
end subroutine WHTML_drift

subroutine WHTML_calibration(MC)
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
    
    write(70,*)"<BR><HR><A NAME=""coeff_calib_out""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Coefficients de calibration</H4></CAPTION>"
    write(70,*)"<TR><TD>Numéro de série</TD><TD>Calibration</TD><TD>SD</TD></TR>"

    do k=1,ngravimeter
    
        if (TabGravi(k)%Estimate) then
            row = row + 1
            write(70,101)"<TR><TD>",TabGravi(k)%serial,"</TD><TD>",&
            &MC%X0(row),"</TD><TD>",MC%Sig(row),"</TD></TR>"
            101 format (A8,A8,A9,f20.6,A9,f20.6,A10)

        end if

    end do

    write(70,*)"</TABLE>"    

    
end subroutine WHTML_calibration

subroutine WHTML_20_plus_gros_resid(MC,mode,nom)
    use MC_data
    use param_data
    use raw_data
    implicit none
    type (Tmc), intent(inout):: MC
    integer mode
    real*8 resNorm,resnorm2
    integer i,nb_oa,w,k,nobs
    character (len=*) nom 
    character (len=40) profile
    character (len=40) chfmt
    character (len=255) profile_abs
    
    if (mode==1) then
        nb_oa = 0;
    else
        nb_oa = MC%nb_obsAbs
    end if
    
    w = len_trim(nom)
    write(chfmt,500)'(A17,A',w,',A6)'
    500 format (A,I2,A)
    write(70,FMT=chfmt)"<BR><HR><A NAME=""",nom,"""></A>"
    
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    if (param%type_resid) then
        write(70,*)"<CAPTION><H4>20 plus gros résidus standards</H4></CAPTION>"
        write(70,*)"<TR><TD>Point initial</TD><TD>Point final</TD><TD>",&
        &"observation (mGal)</TD><TD>résidu (mGal)</TD><TD>",&
        &"Résidu standard</TD><TD>Fichier</TD></TR>"
    else
        write(70,*)"<CAPTION><H4>20 plus gros résidus normalisés</H4></CAPTION>"
        write(70,*)"<TR><TD>Point initial</TD><TD>Point final</TD><TD>",&
        &"observation (mGal)</TD><TD>résidu (mGal)</TD><TD>",&
        &"Résidu normalisé</TD><TD>Fichier</TD></TR>"
    end if
    
    
    nobs = MC%Nb_obsRel+Nb_oa
    if (nobs >= 20) then
        nobs = 19
    end if 
    
    do i=MC%Nb_obsRel+Nb_oa,MC%Nb_obsRel+Nb_oa-nobs,-1
        if (param%type_resid) then
            resNorm = Tabresid_trie(i)%std_res
        else
            resNorm = Tabresid_trie(i)%Norm_res
        end if
        if (Tabresid_trie(i)%abs) then
            write(70,101)"<TR><TD>",Tabresid_trie(i)%ini,"</TD><TD></TD><TD>",&
            &Tabresid_trie(i)%obs,"</TD><TD>",&
            &Tabresid_trie(i)%resid,"</TD><TD>", resNorm,"</TD></TR>"
            101 format (A8,A8,A20,f16.3,A9,f8.3,A9,f8.3,A10)
        else
        
            k = Tabresid_trie(i)%profil
            profile = TabProfil(k)%nomfic
            
            write(70,103)"<TR><TD>",Tabresid_trie(i)%ini,"</TD><TD>",&
            &Tabresid_trie(i)%fin,"</TD><TD>",Tabresid_trie(i)%obs,"</TD><TD>",&
            &Tabresid_trie(i)%resid,"</TD><TD>", resNorm,"</TD><TD>",profile,"</TD></TR>"
            
            103 format (A8,A8,A9,A8,A9,f16.3,A9,f8.3,A9,f8.3,A10,A40,A10)
        end if
    
    end do
    
    write(70,*)"</TABLE>"    
        
end subroutine WHTML_20_plus_gros_resid

subroutine WHTML_tautest(MC,mode,nom)
    use MC_data
    use param_data
    use raw_data
    implicit none
    type (Tmc), intent(inout):: MC
    integer mode
    character (len=*) nom
    character (len=80) chfmt
    real*8 resNorm,resnorm2,tau
    integer nb_oa,w,k,i,l
    character (len=10) test
    logical W_only_failed_tau_test
    character (len=40) profile
    
    W_only_failed_tau_test = param%write_only_failed_tau_test
    
    if (mode==1) then
        nb_oa = 0;
    else
        nb_oa = MC%nb_obsAbs
    end if
    
    w = len_trim(nom)
    write(chfmt,500)'(A17,A',w,',A6)'
    500 format (A,I2,A)
    write(70,*)"<BR><HR><A NAME=""",nom,"""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<TABLE CELLPADDING=3 BORDER=""1"">"
    
    56 format (A40,f6.2,A16)
    write(70,56)"<CAPTION><H4>Tau-test (valeur critique = ",MC%crit_tau,")</H4></CAPTION>"
    write(70,*)"<TR><TD>Point initial</TD><TD>Point final</TD><TD>",&
    &"Observation (mGal)</TD><TD>Résidu (mGal)",&
    &"</TD><TD>Test</TD><TD>Statut</TD><TD>Fichier</TD></TR>"
        
    do k=1,MC%Nb_obsRel
        if (param%Type_resid) then 
            tau = abs(Tabresid(k)%std_res) 
        else
            tau = abs(Tabresid(k)%norm_res) 
        end if 
        if (tau>MC%crit_tau)then
            test='reject'
        else
            test='accept'
        end if
        57 format (A10,A8,A10,A8,A10,f16.3,A10,f9.3,A10,f9.3,A10,A10,A10)
        !58 format (A10,A8,A10,A8,A10,f16.3,A10,f9.3,A10,f9.3,A10,A10,A10,A40,A10)
        58 format (A10,A8,A10,A8,A10,f16.3,A10,f16.6,A10,f16.6,A10,A10,A10,A40,A10)
        
        l = Tabresid(k)%profil
        profile = TabProfil(l)%nomfic
        
        if (W_only_failed_tau_test) then
            if (tau>MC%crit_tau) then
!                WRITE(70,57)"<TR><TD>",Tabresid(k)%ini,"</TD><TD>",&
!                &Tabresid(k)%fin,"</TD><TD>",Tabresid(k)%obs,"</TD><TD>",&
!                &Tabresid(k)%resid,"</TD><TD>",tau,"</TD><TD>",test,"</TD></TR>"
                WRITE(70,58)"<TR><TD>",Tabresid(k)%ini,"</TD><TD>",&
                &Tabresid(k)%fin,"</TD><TD>",Tabresid(k)%obs,"</TD><TD>",&
                &Tabresid(k)%resid,"</TD><TD>",tau,"</TD><TD>",test,"</TD><TD>",profile,"</TD></TR>"

            endif
        else
            !WRITE(70,57)"<TR><TD>",Tabresid(k)%ini,"</TD><TD>",&
            !&Tabresid(k)%fin,"</TD><TD>",Tabresid(k)%obs,"</TD><TD>",&
            !&Tabresid(k)%resid,"</TD><TD>",tau,"</TD><TD>",test,"</TD></TR>"
            WRITE(70,58)"<TR><TD>",Tabresid(k)%ini,"</TD><TD>",&
                &Tabresid(k)%fin,"</TD><TD>",Tabresid(k)%obs,"</TD><TD>",&
                &Tabresid(k)%resid,"</TD><TD>",tau,"</TD><TD>",test,"</TD><TD>",profile,"</TD></TR>"

        endif
        
        
    end do


    if(param%lfix .and. param%mode>1 ) then
        do k=MC%Nb_obsRel+1,MC%Nb_obsRel+Nb_oa
            if (param%Type_resid) then 
                tau = abs(Tabresid(k)%std_res) 
            else
                tau = abs(Tabresid(k)%norm_res) 
            end if 
            if (tau>MC%crit_tau)then
                test='reject'
            else
                test='accept'
            end if
            if (W_only_failed_tau_test) then
                if (tau>MC%crit_tau) then
                    !WRITE(70,57)"<TR><TD>",Tabresid(k)%ini,"</TD><TD>","  ",&
                    !&"</TD><TD>",Tabresid(k)%obs,"</TD><TD>",Tabresid(k)%resid,&
                    !&"</TD><TD>",tau,"</TD><TD>",test,"</TD></TR>"
                    WRITE(70,58)"<TR><TD>",Tabresid(k)%ini,"</TD><TD>","  ",&
                    &"</TD><TD>",Tabresid(k)%obs,"</TD><TD>",Tabresid(k)%resid,&
                    &"</TD><TD>",tau,"</TD><TD>",test,"</TD><TD>"," ","</TD></TR>"

                endif
            else
                !WRITE(70,57)"<TR><TD>",Tabresid(k)%ini,"</TD><TD>","  ",&
                !&"</TD><TD>",Tabresid(k)%obs,"</TD><TD>",Tabresid(k)%resid,&
                !&"</TD><TD>",tau,"</TD><TD>",test,"</TD></TR>" 
                WRITE(70,58)"<TR><TD>",Tabresid(k)%ini,"</TD><TD>","  ",&
                    &"</TD><TD>",Tabresid(k)%obs,"</TD><TD>",Tabresid(k)%resid,&
                    &"</TD><TD>",tau,"</TD><TD>",test,"</TD><TD>"," ","</TD></TR>"   
            endif
                        
        end do

    end if
    
    write(70,*)"</TABLE>"    
        
end subroutine WHTML_tautest

subroutine WHTML_synthese(MC,mode,nom)
    use MC_data
    use param_data
    implicit none
    type (Tmc), intent(inout):: MC
    integer mode
    character (len=*) nom
    real*8 resNorm,resnorm2,tau
    integer nb_oa,w,k,i
    character (len=10) test
    
    if (mode==1) then
        nb_oa = 1;
    else
        nb_oa = MC%nb_obsAbs
    end if
    
    w = len_trim(nom)
    
    56 format (A10,A40,A10,f9.3,A10)
    write(70,*)"<BR><HR><A NAME=""",nom,"""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    
    write(70,*)"<BR><TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Résidus</H4></CAPTION>" 
    write(70,56)"<TR><TD>",'Résidu maximum',"</TD><TD>",MC%vmax,"</TD></TR>"
    write(70,56)"<TR><TD>",'Résidu minimum',"</TD><TD>",MC%vmin,"</TD></TR>"
    write(70,56)"<TR><TD>",'Résidu moyen',"</TD><TD>",MC%vmean,"</TD></TR>"
    write(70,56)"<TR><TD>",'RMS des résidu',"</TD><TD>",MC%vrms,"</TD></TR>"   
    write(70,*)"</TABLE>"  
    
    write(70,*)"<BR><TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Inconnues de Pesanteur</H4></CAPTION>"   
    write(70,56)"<TR><TD>",'Ecart-type maximum sur les inconnues',&
    &"</TD><TD>",MC%sgmax,"</TD></TR>"
    write(70,56)"<TR><TD>",'Ecart-type minimum sur les inconnues',&
    &"</TD><TD>",MC%sgmin,"</TD></TR>"
    write(70,56)"<TR><TD>",'Ecart-type moyen sur les inconnues',&
    &"</TD><TD>",MC%sgmean,"</TD></TR>"
    write(70,56)"<TR><TD>",'RMS de l''écart-type sur les inconnues',&
    &"</TD><TD>",MC%sgrms,"</TD></TR>"
    write(70,*)"</TABLE>"  
    
    write(70,*)"<BR><TABLE CELLPADDING=3 BORDER=""1"">"
    write(70,*)"<CAPTION><H4>Inconnues de dérive</H4></CAPTION>"
    write(70,56)"<TR><TD>",'Ecart-type maximum sur les inconnues',&
    &"</TD><TD>",MC%sdmax,"</TD></TR>"
    write(70,56)"<TR><TD>",'Ecart-type minimum sur les inconnues',&
    "</TD><TD>",MC%sdmin,"</TD></TR>"
    write(70,56)"<TR><TD>",'Ecart-type moyen sur les inconnues',&
    &"</TD><TD>",MC%sdmean,"</TD></TR>"
    write(70,56)"<TR><TD>",'RMS de l''écart-type sur les inconnues',&
    &"</TD><TD>",MC%sdrms,"</TD></TR>"
    write(70,*)"</TABLE>"    
          
end subroutine WHTML_synthese



subroutine WHTML_khi2(MC)
    use MC_data
    use param_data
    use raw_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i,k,row,n
    real*8 ski2
    character*6 test
    
 
    
    write(70,*)"<BR><HR><A NAME=""khi2""></A>"
    write(70,*)"<P><A HREF=""#Haut_Page"">Sommaire</A></P>"
    write(70,*)"<H4>Test du khi2</H4>"
    
    ski2 = MC%dof * MC%sigma0**2

    if (ski2>MC%crit_khi2)then
        test='REJECT'
    else
        test='ACCEPT'
    end if
     
    write(70,'(A41,A23,1x,f10.3,A4)')"Racine carrée de l'estimateur du facteur ",&
    &"unitaire de variance : ",MC%sigma0,"<BR>"
    WRITE(70,'(1x,A7,f10.2,A4)')'Test : ',ski2,"<BR>"
    WRITE(70,'(1x,A35,f10.2,A4)')'Valeur critique du test du khi-2 : ',MC%crit_khi2,"<BR>"
    WRITE(70,*)'Résultat du test : ',test,"<BR>"
        
    
    
    
end subroutine WHTML_khi2

subroutine Whtml_finfichier()
    write(70,*)'</BODY>'
    write(70,*)'</HTML>'
end subroutine Whtml_finfichier

end module write_html


