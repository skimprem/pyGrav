module GMT
! Creation des fichiers pour la cr�ation des dessins sous GMT

contains

! Numeros des fichiers 
! 80 : script
! 81 : points relatifs
! 82 : points absolus
! 83 : barres d'erreur sur les gravit�s
! 84 : profils
! 85 : residus relatifs et absolus

! creation et lancement du script GMT de dessin des profils
subroutine dessine_reseau(MC,run)
    use MC_data 
    use param_data
    use Portability_routines
	use sys_utils
    implicit none
    type (Tmc), intent(inout):: MC
    logical(4) resultat
    logical run
	integer(2) resultatI
    
    call WGMT_bat_profils('profils.pl')
    call WGMT_pts_rel("pts_rel.txt")
    call WGMT_pts_abs("pts_abs.txt")
    call WGMT_profil("profils.txt",MC)

    if (run) then
         !write(0,*)'debut profil'
         !resultat = run_system2(param%ch_rm//' .gmt*')
		 resultatI = rm('.gmtdefaults')
		 resultatI = rm('.gmtdefaults4')
         resultat = run_system2('perl  profils.pl')
         !write(0,*)'fin profil'
         !stop
    end if

end subroutine dessine_reseau


! creation et lancement du script GMT
subroutine run_GMT(MC,run)
    use MC_data
    use param_data
    use Portability_routines
    use sys_utils
    implicit none
    type (Tmc), intent(inout):: MC
    character (len=80) :: cmd
    integer(2) resultatI
    logical(4) resultat
    character(5) systeme
    logical run

    call WGMT_bat('gmt.pl')
    call WGMT_sigma("error_bars.txt",MC)

    call WGMT_resid_rel("residus_std_rel.txt",MC,'NORM','ALL')
    call WGMT_resid_rel("residus_brut_rel.txt",MC,'BRUT','ALL')
    call WGMT_resid_abs("residus_std_abs.txt",MC,'NORM','ALL')
    call WGMT_resid_abs("residus_brut_abs.txt",MC,'BRUT','ALL')
    call WGMT_resid_rel("residus_tau_rel.txt",MC,'NORM','TAU')
    call WGMT_resid_abs("residus_tau_abs.txt",MC,'NORM','TAU')
    
    call WGMT_leg_resid("legende_resid_std.txt",'NORM')
    call WGMT_leg_resid("legende_resid_brut.txt",'BRUT')
    call WGMT_leg_sigma("legende_sigma.txt")
    
    if (run) then
    
         resultatI = run_system('perl','gmt.pl')

         resultatI = move('gmt.pl',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('profils.pl',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('pts_rel.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('pts_abs.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('error_bars.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('profils.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_std_rel.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_std_abs.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_tau_rel.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_tau_abs.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_brut_rel.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_brut_abs.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('legende_resid_brut.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('legende_resid_std.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('legende_sigma.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy('Profils.ps',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy('Error_bars.ps',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy('Residus_norm.ps',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy('Residus_bruts.ps',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy('Residus_tautest.ps',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy(param%nomficcal,param%dossier(1:len_trim(param%dossier)))
      
      
    end if

end subroutine run_GMT

! Ecriture du script GMT
subroutine WGMT_bat_profils(nomfic)
    use param_data
    implicit none
    character (len=*) nomfic
    character(350) s
    CHARACTER string*(20)

    !write(0,*)"Ecriture du script GMT"
    open(80,file = nomfic)
     
    call Wfic(80,"`gmtset PLOT_DEGREE_FORMAT +ddd:mm`;")
    call Wfic(80,"`gmtset PAGE_ORIENTATION  landscape`;")
    call Wfic(80,"`gmtset PAPER_MEDIA a3`;")
    call Wfic(80,"`gmtset ANNOT_FONT_SIZE_PRIMARY 10`;")
    call Wfic(80,"`gmtset LABEL_FONT_SIZE 8`;")
    call Wfic(80,"`gmtset CHAR_ENCODING  Standard+`;")
    call Wfic(80,"`gmtset FRAME_WIDTH 0.1c`;")
    call Wfic(80,"`gmtset MEASURE_UNIT cm`;")

    write(s,17)' -R',param%lon1,'/',param%lat1,'/',param%lon2,'/',param%lat2,"r"
    17 format(A3,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1)
    
    s= "`pscoast "//s(1:45)//" -JM33c &
    &-B1.00g1.00f1.00:.""Observations relatives et absolues"":&
    & -Df -K -A0/1 -N1 -S210 &
    & -W1 -Lf6.5/42.5/45/50k -G255/220/150 -S150/255/255 -I1/0.5p/0/0/255 &
    & -I2/0.5p/0/0/255 -I3/0.5p/0/0/255 -Y2c -X3.5c > Profils.ps`;"
   
    call Wfic(80,s)
    call Wfic(80,"`psxy -R  -JM -M -W1.5/120/120/120 -K -O profils.txt >> Profils.ps`;")
    
    call Wfic(80,"`psxy -R  -JM -Sc0.075 -G0/0/0 -K -O pts_rel.txt >> Profils.ps`;")
    call Wfic(80,"`pstext -R -JM -S5/255/220/150 -O -K -G94/151/106 -Dj0.05  pts_rel.txt >> Profils.ps`;")
        
    call Wfic(80,"`psxy -R   -JM -St0.25 -G255/0/0 -K -O  pts_abs.txt >> Profils.ps`;")
    call Wfic(80,"`pstext -R -JM -S5/255/220/150 -O  -G0 -Dj0.05 pts_abs.txt >> Profils.ps`;")
    
    close(80)
    
end subroutine WGMT_bat_profils

! Ecriture du script GMT
subroutine WGMT_bat_histo(nomfic,nomfic_histo,nomfic_ps,nomfic_png)
    use param_data
    use Portability_routines
    use sys_utils
    implicit none
    character (len=*) nomfic,nomfic_ps,nomfic_histo,nomfic_png
    character(255) s
    CHARACTER string*(20)
    integer(2) resultatI
    logical(4) resultat

    !write(0,*)"Ecriture du script GMT"
    open(80,file = nomfic)
    
    call Wfic(80,"`gmtset PLOT_DEGREE_FORMAT +ddd:mm`;")
    call Wfic(80,"`gmtset PAGE_ORIENTATION  portrait`;")
    call Wfic(80,"`gmtset PAPER_MEDIA a4`;")
    call Wfic(80,"`gmtset ANNOT_FONT_SIZE_PRIMARY 12`;")
	call Wfic(80,"`gmtset HEADER_FONT_SIZE 10p`;")
    call Wfic(80,"`gmtset LABEL_FONT_SIZE 8`;")
    call Wfic(80,"`gmtset CHAR_ENCODING  Standard+`;")
    call Wfic(80,"`gmtset FRAME_WIDTH 0.1c`;")
    call Wfic(80,"`gmtset MEASURE_UNIT cm`;")
    
    if (param%Type_resid) then         
        call Wfic(80,'`psbasemap -R-4.5/+4.5/0/60 -JX16c/20c -Bf0.5a0.5:""&
        &:/f10a10:"\045":WS:."Histogramme des r\345sidus standards": -K > '//nomfic_ps//'`;')
    else
        call Wfic(80,'`psbasemap -R-4.5/+4.5/0/60 -JX16c/20c -Bf0.5a0.5:""&
        &:/f10a10:"\045":WS:."Histogramme des r\345sidus normalis\345s": -K > '//nomfic_ps//'`;')
    end if
    s = "`psxy   -R  -JX -G0/0/255 -W0.5p  -Sb0.9c -N -O -K " // nomfic_histo //' >> '//nomfic_ps//'`;'
    call Wfic(80,s) 
    call Wfic(80,'`psxy   -R  -JX -W0.8p/255/0/0  gauss.txt -N -O >> '//nomfic_ps//'`;')
   
    close(80)
    
    resultatI = run_system('perl',nomfic)
    resultatI = run_system ('convert',nomfic_ps//' '//nomfic_png)

    resultatI = move(nomfic_histo(1:len_trim(nomfic_histo)),param%dossier(1:len_trim(param%dossier)))
    resultatI = move(nomfic,param%dossier(1:len_trim(param%dossier)))
    resultatI = move(nomfic_ps,param%dossier(1:len_trim(param%dossier)))
    resultatI = move('gauss.txt',param%dossier(1:len_trim(param%dossier)))
    resultatI = copy(nomfic_png,param%dossier(1:len_trim(param%dossier)))
       
end subroutine WGMT_bat_histo


! Ecriture du script GMT
subroutine WGMT_bat(nomfic)
    use param_data
    implicit none
    character (len=*) nomfic   
    character(300) s
    CHARACTER string*(20)

    17 format(A3,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1)
    18 format(A3,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1)

    write(0,*)"Ecriture du script GMT"
    open(80,file = nomfic)

    call Wfic(80,"`gmtset PLOT_DEGREE_FORMAT +ddd:mm`;")
    call Wfic(80,"`gmtset PAGE_ORIENTATION  landscape`;")
    call Wfic(80,"`gmtset PAPER_MEDIA a3`;")
    call Wfic(80,"`gmtset ANNOT_FONT_SIZE_PRIMARY 10`;")
    call Wfic(80,"`gmtset LABEL_FONT_SIZE 8`;")
    call Wfic(80,"`gmtset CHAR_ENCODING  Standard+`;")
    call Wfic(80,"`gmtset FRAME_WIDTH 0.1c`;")
    call Wfic(80,"`gmtset MEASURE_UNIT cm`;")
 
    ! carte des ecarts-types sur les pesanteurs 
      
    write(s,17)' -R',param%lon1,'/',param%lat1,'/',param%lon2,'/',param%lat2,"r"   
    s= "`pscoast "//s(1:45)//" -JM30c -B1.00g1.00f1.00:.""Ecarts-types sur les pesanteurs esitm\345es"": &
    & -Df -K -A0/1 -N1 -S210 &
    & -W1 -Lf6.5/42.5/45/50k -G255/220/150 -S150/255/255 -I1/0.5p/0/0/255 &
    & -I2/0.5p/0/0/255 -I3/0.5p/0/0/255 -Y2 -X4.5 > Error_bars.ps`;"        
    call Wfic(80,s)
    call Wfic(80,"`psxy error_bars.txt -R -JM  -W1/255/0/0  -G255/0/0 &
    & -Sc0.001c -Ey0.3c/255/0/0 -O -K >> Error_bars.ps`;")  
    call Wfic(80,"`pstext -R -JM -S5/255/220/150 -O  -K  -G0 -Dj0.05   legende_sigma.txt >> Error_bars.ps`;")    
    call Wfic(80,"`psxy -R  -JM -Sc0.075 -G0/0/0 -K -O pts_rel.txt >> Error_bars.ps`;")
    call Wfic(80,"`pstext -R -JM -S5/255/220/150  -O -K -G94/151/106 -Dj0.05  pts_rel.txt >> Error_bars.ps`;")      
    call Wfic(80,"`psxy -R   -JM  -St0.25 -G255/0/0 -K -O  pts_abs.txt >> Error_bars.ps`;")
    call Wfic(80,"`pstext -R -JM -S5/255/220/150 -O  -G0 -Dj0.05 pts_abs.txt >> Error_bars.ps`;")
  
    ! carte des residus normalises

    write(s,17)' -R',param%lon1,'/',param%lat1,'/',param%lon2,'/',param%lat2,"r"
    s= "`pscoast "//s(1:45)//" -JM33c -B1.00g1.00f1.00:.""R\345sidus normalis\345s"": -Df -K -A0/1 -N1 -S210 &
    & -W1 -Lf6.5/42.5/45/50k -G255/220/150 -S150/255/255 -I1/0.5p/0/0/255 &
    & -I2/0.5p/0/0/255 -I3/0.5p/0/0/255 -Y2 -X4.5 > Residus_norm.ps`;"      
    call Wfic(80,s)   
    call Wfic(80,"`psxy -R  -JM -M -W1.5/120/120/120 -K -O profils.txt >> Residus_norm.ps`;")
    call Wfic(80,"`psxy  -R -JM  -W1/0/255/0 -G0/255/0 -Sv0.02c/0.04c/0.04c &
    & -O -K residus_std_rel.txt >> Residus_norm.ps`;")
    call Wfic(80,"`psxy  -R -JM  -W1/255/0/255  -G255/0/255 -Sv0.02c/0.04c/0.04c &
    & -O -K residus_std_abs.txt >> Residus_norm.ps`;")   
    call Wfic(80,"`pstext -R -JM -S5/255/220/150 -O -K -G0 -Dj0.05   legende_resid_std.txt >> Residus_norm.ps`;")  
    call Wfic(80,"`psxy -R  -JM -Sc0.075 -G0/0/0 -K -O pts_rel.txt >> Residus_norm.ps`;")
    call Wfic(80,"`pstext -R -JM -S5/255/220/150 -O -K -G94/151/106 -Dj0.05  pts_rel.txt >> Residus_norm.ps`;")     
    call Wfic(80,"`psxy -R   -JM -St0.25 -G255/0/0 -K -O  pts_abs.txt >> Residus_norm.ps`;")
    call Wfic(80,"`pstext -R -JM -S5/255/220/150  -O  -G0 -Dj0.05 pts_abs.txt >> Residus_norm.ps`;")


    ! carte des residus normalises ne passant pas le tau-test

    write(s,17)' -R',param%lon1,'/',param%lat1,'/',param%lon2,'/',param%lat2,"r"
    s= "`pscoast "//s(1:45)//" -JM33c -B1.00g1.00f1.00:.""R\345sidus normalis\345s \345chouant au tau-test"": &
    &-Df -K -A0/1 -N1 -S210 &
    & -W1 -Lf6.5/42.5/45/50k -G255/220/150 -S150/255/255 -I1/0.5p/0/0/255 &
    & -I2/0.5p/0/0/255 -I3/0.5p/0/0/255 -Y2 -X4.5 > Residus_tautest.ps`;"      
    call Wfic(80,s)   
    call Wfic(80,"`psxy -R  -JM -M -W1.5/120/120/120 -K -O profils.txt >> Residus_tautest.ps`;")
    call Wfic(80,"`psxy  -R -JM  -W1/0/255/0 -G0/255/0 -Sv0.02c/0.04c/0.04c  -O -K &
    & residus_tau_rel.txt >> Residus_tautest.ps`;")
    call Wfic(80,"`psxy  -R -JM  -W1/255/0/255  -G255/0/255 -Sv0.02c/0.04c/0.04c  -O -K &
    & residus_tau_abs.txt >> Residus_tautest.ps`;")   
    call Wfic(80,"`pstext -R -JM -S5/255/220/150 -O -K -G0 -Dj0.05  &
    & legende_resid_std.txt >> Residus_tautest.ps`;")  
    call Wfic(80,"`psxy -R  -JM -Sc0.075 -G0/0/0 -K -O pts_rel.txt >> Residus_tautest.ps`;")
    call Wfic(80,"`pstext -R -JM -S5/255/220/150 -O -K -G94/151/106 -Dj0.05  pts_rel.txt >> Residus_tautest.ps`;")     
    call Wfic(80,"`psxy -R   -JM -St0.25 -G255/0/0 -K -O  pts_abs.txt >> Residus_tautest.ps`;")
    call Wfic(80,"`pstext -R -JM -S5/255/220/150  -O  -G0 -Dj0.05 pts_abs.txt >> Residus_tautest.ps`;")

    ! carte des residus bruts

    write(s,17)' -R',param%lon1,'/',param%lat1,'/',param%lon2,'/',param%lat2,"r"  
    s= "`pscoast "//s(1:45)//" -JM33c -B1.00g1.00f1.00:.""R\345sidus"": -Df -K -A0/1 -N1 -S210 &
    & -W1 -Lf6.5/42.5/45/50k -G255/220/150 -S150/255/255 -I1/0.5p/0/0/255 &
    & -I2/0.5p/0/0/255 -I3/0.5p/0/0/255 -Y2 -X4.5 > Residus_bruts.ps`;"   
    call Wfic(80,s)   
    call Wfic(80,"`psxy -R  -JM -M -W1.5/120/120/120 -K -O profils.txt >> Residus_bruts.ps`;")
    call Wfic(80,"`psxy  -R -JM  -W1/0/255/0 -G0/255/0 -Sv0.02c/0.04c/0.04c &
    & -O -K residus_brut_rel.txt >> Residus_bruts.ps`;")
    call Wfic(80,"`psxy  -R -JM  -W1/255/0/255  -G255/0/255 -Sv0.02c/0.04c/0.04c &
    & -O -K residus_brut_abs.txt >> Residus_bruts.ps`;")  
    call Wfic(80,"`pstext -R -JM -S5/255/220/150 -O -K -G0 -Dj0.05 &
    &  legende_resid_brut.txt >> Residus_bruts.ps`;") 
    call Wfic(80,"`psxy -R  -JM -Sc0.075 -G0/0/0 -K -O pts_rel.txt >> Residus_bruts.ps`;")
    call Wfic(80,"`pstext -R -JM -S5/255/220/150 -O -K -G94/151/106 -Dj0.05  pts_rel.txt >> Residus_bruts.ps`;")     
    call Wfic(80,"`psxy -R   -JM -St0.25 -G255/0/0 -K -O  pts_abs.txt >> Residus_bruts.ps`;")
    call Wfic(80,"`pstext -R -JM -S5/255/220/150  -O  -G0 -Dj0.05 pts_abs.txt >> Residus_bruts.ps`;")

    close(80)

end subroutine WGMT_bat

subroutine WGMT_pts_rel(nomfic)
    use Raw_data
    use param_data
    implicit none
    character (len=*) nomfic
    character(255) s
    integer i
    !write(0,*)"Ecriture des points observes en relatif"
    open(81,file = nomfic)

    do i=1,NTabStation
        write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)')TabStation(i)%lon,TabStation(i)%lat,"8 0 1 0",TabStation(i)%nomsta
        call Wfic(81,s)
    end do
    
    close(81)
end subroutine WGMT_pts_rel

subroutine WGMT_leg_sigma(nomfic)
    use Raw_data
    use param_data
    implicit none
    character (len=*) nomfic
    character(255) s
    real*8 dlat,dlon,sigma,lat, lon
    integer i
    !write(0,*)"Ecriture des points observes en relatif"
    open(81,file = nomfic)

    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    lat = param%lat1 + dlat / 10
    lon = param%lon1 + dlon / 10 + 0.1
    write(s,'(f12.6,1x,f12.6,1x,A7,1x,A20)')lon,lat,"8 0 1 0","0.025 mGal"
    call Wfic(81,s)
    
    close(81)
end subroutine WGMT_leg_sigma

subroutine WGMT_leg_resid(nomfic,BRUTouNORM)
    use Raw_data
    use param_data
    implicit none
    character (len=*) nomfic
    character (len=4) BRUTouNORM ! BRUT ou NORM ; on appelle la procedure 2 fois
    character(255) s
    real*8 dlat,dlon,sigma,lat, lon
   
    open(81,file = nomfic)

    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    lat = param%lat1 + dlat / 10
    lon = param%lon1 + dlon / 10 + 0.1
    if (BRUTouNORM=='BRUT') then
      write(s,'(f12.6,1x,f12.6,1x,A7,1x,A10)')lon,lat,"8 0 1 0","10 \225Gal"
    else
	   write(s,'(f12.6,1x,f12.6,1x,A7,1x,A3)')lon,lat,"8 0 1 0","1"
    endif
   
    call Wfic(81,s)
    
    close(81)
end subroutine WGMT_leg_resid

subroutine WGMT_pts_abs(nomfic)
    use Raw_data
    use param_data
    implicit none
    character (len=*) nomfic
    character(255) s
    integer i,j
    !write(0,*)"Ecriture des points observes en absolu"
    open(82,file = nomfic)

    do i=1,NTabStation
        do j=1,param%Nb_obsAbs
            if (TabStation(i)%nomsta == fixstn(j)) then
                write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)')TabStation(i)%lon,TabStation(i)%lat,"8 0 1 0",TabStation(i)%nomsta
                call Wfic(82,s)
            end if
        
        end do
    end do
    
    close(82)
end subroutine WGMT_pts_abs

subroutine WGMT_sigma(nomfic,MC)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout):: MC

    real*8 dlat,dlon,sigma,lat, lon
    character (len=*) nomfic
    character(255) s
    integer i,j
    !write(0,*)"Ecriture des ecarts-type"
    open(83,file = nomfic)
    
    do i=1,NTabStation
        write(s,'(f12.6,1x,f12.6,1x,f12.6,1x,A8)')TabStation(i)%lon,TabStation(i)%Lat,MC%sig(TabStation(i)%numsta),"0 0 0 0"
        call Wfic(83,s)
    end do
    
    
    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    lat = param%lat1 + dlat / 10
    lon = param%lon1 + dlon / 10
    sigma = 0.025d0
    write(s,'(f12.6,1x,f12.6,1x,f12.4,1x,A8)')lon,lat,sigma,"0 0 0 0"
    call Wfic(83,s)

    
    close(83)
end subroutine WGMT_sigma

! Dessin des profils pour detecter les zones de faiblesse du r�seau
subroutine WGMT_profil(nomfic,MC)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout):: MC

    character (len=*) nomfic
    character(255) s
    integer i,j
    !write(0,*)"Ecriture des profils"
    open(84,file = nomfic)
    
    obs_rel = TabObsRel(1)
    write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)')TabStation(obs_rel%numsta_AR)%lon,&
    &TabStation(obs_rel%numsta_AR)%lat,"8 0 1 0",TabStation(obs_rel%numsta_AR)%nomsta
    call Wfic(84,s)
    do i=2,nTabObsRel
        if (TabObsRel(i)%profil/=obs_rel%profil) then
            call Wfic(84,">")
            write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)')TabStation(TabObsRel(i)%numsta_AR)%lon,&
            &TabStation(TabObsRel(i)%numsta_AR)%lat,"8 0 1 0",TabStation(TabObsRel(i)%numsta_AR)%nomsta
            call Wfic(84,s)
        else
            if (TabObsRel(i)%numsta_AV/=TabObsRel(i)%numsta_AR) then
                write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)')TabStation(TabObsRel(i)%numsta_AV)%lon,&
                &TabStation(TabObsRel(i)%numsta_AV)%lat,"8 0 1 0",TabStation(TabObsRel(i)%numsta_AV)%nomsta
                call Wfic(84,s)
            end if

        end if 
        obs_rel = TabObsRel(i)
    end do
    
    close(84)
end subroutine WGMT_profil

! Dessin des residus 
subroutine WGMT_resid_rel(nomfic,MC,BRUTouNORM,ONLYTAU)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout):: MC
    real*8 lon, lat, v, moy, dlat, dlon
    character (len=*) nomfic
    character (len=4) BRUTouNORM ! BRUT ou NORM ; on appelle la procedure 2 fois
    character (len=3) ONLYTAU ! TAU ou ALL ; on appelle la procedure 2 fois
    character (len=3) Az
    character(255) s
    integer i,j
    
    !write(0,*)"Ecriture des Residus relatifs"
    open(85,file = nomfic)
    moy = 0
    do i=1,nTabObsRel
    
        ! milieu de la liaison
        lon = 0.5 * (TabStation(TabObsRel(i)%numsta_AR)%lon + TabStation(TabObsRel(i)%numsta_AV)%lon)
        lat = 0.5 * (TabStation(TabObsRel(i)%numsta_AR)%lat + TabStation(TabObsRel(i)%numsta_AV)%lat)
        if (BRUTouNORM=='BRUT') then
            v = 100.0 * TabResid(i)%resid
        else
            if (param%Type_resid) then
               v = TabResid(i)%std_res
            else
               v = TabResid(i)%Norm_res
            endif
        endif
        !3.11056    45.76361  90 3.59 0 0 0
        if (v>0) then
            Az = '90'
        else
            Az = '-90'
        end if
        v = abs(v)
        write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.4,1x,A5,A10,A10)')lon,lat,Az,v,"0 0 0",TabResid(i)%ini,TabResid(i)%fin

        if (ONLYTAU=='TAU') then
!		    if ( TabResid(i)%tautest==.false.) then
		    if (.not. TabResid(i)%tautest) then
				call Wfic(85,s)	
          endif 
        else 
			 call Wfic(85,s)
        endif
        
        !write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.4,1x,A5)')lon,lat,Az,v,"0 0 0"
        
            
    end do
    
    !moy = moy /nTabObsRel
    moy = 1
    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.4,1x,A5)')param%lon1+dlon/10,param%lat1+dlat/10,"90",moy,"0 0 0"
    call Wfic(85,s)
      
    close(85)

end subroutine WGMT_resid_rel

! Dessin des residus 
subroutine WGMT_resid_abs(nomfic,MC,BRUTouNORM,ONLYTAU)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout):: MC
    real*8 lon, lat, v, moy
    character (len=*) nomfic
    character (len=4) BRUTouNORM ! BRUT ou NORM ; on appelle la procedure 2 fois
    character (len=3) ONLYTAU ! TAU ou ALL ; on appelle la procedure 2 fois
    character (len=3) Az
    character(255) s
    integer i,j,num_pt
    
    !write(0,*)"Ecriture des Residus absolus"
    open(85,file = nomfic)
    moy = 0
    
    !write(0,*)nTabObsRel,MC%Nb_obsAbs
    do i=nTabObsRel+1,nTabObsRel+MC%Nb_obsAbs
        
        num_pt = pos(i-nTabObsRel)

        lon = TabStation(num_pt)%lon
        lat = TabStation(num_pt)%lat
        if (BRUTouNORM=='BRUT') then
            v = 100.0 * TabResid(i)%resid
        else
            if (param%Type_resid) then
               v = TabResid(i)%std_res
            else
               v = TabResid(i)%Norm_res
            endif
        endif

        if (v>0) then
            Az = '90'
        else
            Az = '-90'
        end if
        v = abs(v) 
        !3.11056    45.76361  90 3.59 0 0 0
        write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.6,1x,A5,A10)')lon,lat,Az,v,"0 0 0",TabResid(i)%ini
        if (ONLYTAU=='TAU') then
!		    if (TabResid(i)%tautest==.false.) then
		    if (.not. TabResid(i)%tautest) then
				 call Wfic(85,s)	
          endif 
        else 
			  call Wfic(85,s)
        endif

        !write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.6,1x,A5)')lon,lat,Az,v,"0 0 0"
       
            
    end do
    
    close(85)
end subroutine WGMT_resid_abs

! Dessin des drifts lin�aires
! Pour l'instant on remplit le tableau
! pour le dessin on verra plus tard 
subroutine WGMT_drift(nomfic,MC)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout):: MC
    real*8 lon, lat, v
    character (len=*) nomfic
    character(255) s
    integer i,j,dt,dk,np,ns,ng,row,g
    real*8 mini,maxi
    real*8,allocatable,dimension(:,:,:)::Tabdrift
    real*8,allocatable,dimension(:)::TabNdrift
    
    !write(0,*)"Ecriture des derives"
    
    dt = MC%degre_t
    dk = MC%degre_k
    np = MC%Nb_profil
    ns = MC%Nb_sta
    ng = ngravimeter
    row = ns + np * (dt + dk)
    
    if (allocated(Tabdrift)) deallocate(Tabdrift)
    if (allocated(TabNdrift)) deallocate(TabNdrift)
    allocate (Tabdrift(ng,np,(dt + dk)))
    allocate (TabNdrift(ng))
    
    TabNdrift = 0.0
    Tabdrift = 0.0
    
    mini = -1d20
    maxi = 1d20
    
    if (dt>0 .or. dk>0) then
        do i=1,np
            !write(0,*)i
            do j=1,ng
                if (TabProfil(i)%serial==TabGravi(j)%serial) then
                    g = j
                    exit
                end if
            end do 
            
            TabNdrift(g) = TabNdrift(g) + 1
            
            if (dt>0) then
                row = ns + ( i - 1 )*( dt + dk ) 
                do j=1,dt
                    row = row + 1
                    Tabdrift(g,INT(TabNdrift(g)),j) = MC%X(row)
                    if (j==1) then
                        if (MC%X(row)<mini) mini = MC%X(row)
                        if (MC%X(row)>maxi) maxi = MC%X(row)
                    end if
                end do 
            end if
            
            if (dk>0) then
                row = ns + ( i - 1 )*( dt + dk ) + dt
                do j=1,dk
                    Tabdrift(g,INT(TabNdrift(g)),j+dt) = MC%X(row)
                end do
            end if         
    
        end do
        
    end if
    
    
    open(85,file = nomfic)
    
    do i=1,nTabObsRel
    
        ! milieu de la liaison
        lon = 0.5 * (TabStation(TabObsRel(i)%numsta_AR)%lon + TabStation(TabObsRel(i)%numsta_AV)%lon)
        lat = 0.5 * (TabStation(TabObsRel(i)%numsta_AR)%lat + TabStation(TabObsRel(i)%numsta_AV)%lat)
        v = 10*MC%V(i)
        !3.11056    45.76361  90 3.59 0 0 0
        write(s,'(f12.6,1x,f12.6,1x,A2,1x,f12.6,1x,A5)')lon,lat,"90",v,"0 0 0"
        call Wfic(85,s)
            
    end do
   
    
    close(85)
    
    if (allocated(Tabdrift)) deallocate(Tabdrift)
    if (allocated(TabNdrift)) deallocate(TabNdrift)
    
end subroutine WGMT_drift

! Dessin des residus 
subroutine WGMT_fic_histo(nomfic,MC)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout):: MC
    real*8 valeur, gauss, pi1 ,Norm
    character (len=*) nomfic
    real*8,dimension(18)::hist
    character(255) s
    integer i
    
    pi1 = 4d0 * DATAN(1d0)
    
    !write(0,*)"Ecriture du fichier d'histogramme"
    do i=1,18
	    hist(i) = 2d0 * 100d0 * MC%histo(i) 
	    if (hist(i)>55) hist(i)=55d0
    end do
    
   
    open(85,file = nomfic)
    do i=1,18
	    valeur = i/2d0 -5d0 
	    valeur = valeur + 0.25d0       
        write(s,'(f12.6,1x,f12.6)')valeur,hist(i)
        call Wfic(85,s)           
    end do
    close(85)
    
    ! ecriture de la loi th�orique
    open(86,file = 'gauss.txt')
    do i = -45,45
      Norm = DBLE(i) / 10d0;
      gauss = 100d0 * (DEXP(-0.5d0 *(Norm**2)))   / (DSQRT(2d0*pi1));
      write(86,'(f12.6,1x,f12.6)')Norm,gauss
    end do 
           
    close(86)
end subroutine WGMT_fic_histo

! Ecrit dans un fichier 
! Permet d'eviter les probl�mes de longueur de ligne
! num est le num�ro du fichier ou on veut ecrire
subroutine Wfic(num,line)
    implicit none
    integer l,num
    character (len=*) :: line
    character (len=40) :: CHFMT 
    l = len_trim(line)
    WRITE (CHFMT,500)'(A',l,')'
    500  FORMAT (A,I4,A) 
    write(num,FMT=CHFMT)line
    !write(num,'(A<l>)')line
end subroutine Wfic


end module GMT
