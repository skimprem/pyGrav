module Lect_param
! module de lecture de paramètres de calcul dans un fichier 

    use util_str
    use Str_const
    
    
    implicit none
    
contains    

integer function Read_config_file(nomfic,p) 
! fonction qui lit les paramètre de calcul dans un fichier texte (lsgravi.conf par exemple)  

    use param_data
    use Raw_data
    use util_str
    use portability_routines

    implicit none                 
      
    character (len=*) nomfic 
    type (TParam) p
    integer nc,i,nfic_grav_abs,k
    character (len=255) line
    character (len=255) nom,rep
    character (len=255) tabline(20)
    !character (len=255), pointer, dimension(:) :: tabnomficrel2
    !character (len=255), pointer, dimension(:) :: tabrepficRel2
    type (TDataFic), pointer , dimension(:) :: TabDataFic2
    type (TDataFic) Dfic
    
    ! definition des formats de lecture de nombres
    101 format (f16.8) 
    102 format (I8)
    103 format (G16.8)
    
    logical valid
    
    valid = .true.
    Read_config_file = 0


    call Init_param(p)
    
    p%systeme = OS_NAME_POSIX()

    if (p%systeme == 'LINUX') then
      p%ch_mkdir = 'mkdir   '
      p%ch_copy  = 'cp -f   '
      p%ch_move  = 'mv -f   '
      p%ch_rm    = 'rm -f   '
      p%dsep     = '/'
    else
      p%ch_mkdir = 'md      '
      p%ch_copy  = 'copy /Y '
      p%ch_move  = 'move /Y '
      p%ch_rm    = 'del /Q  '
      p%dsep     = '\'
    endif

    
    allocate(p%tabDataFic(datablock))
    allocate(p%tabCoordFic(datablock))

    open(20,file=nomfic)     
    
    do while(.true.)
    
        read(20,'(A255)',end=668)line
       
        call decoupe_ligne(line,tabline,nc)
        
        if (nc==2) then
        
            if ((tabline(1)=='DRIFT_T').and.StrIsnumber(tabline(2))) then
                read(tabline(2),102)p%drift_t
                !p%drift_t = JNUM(tabline(2))
                if (p%drift_t<0) then
                    write(0,*)'Time related drift. Polynomial degree must be >=0. But requested degree is:',p%drift_t
                    valid=.false.
                end if
                
            else if ((tabline(1)=='DRIFT_K').and.StrIsnumber(tabline(2))) then
                read(tabline(2),102)p%drift_k
                !p%drift_k = JNUM(tabline(2))
                if (p%drift_k<0) then
                    write(0,*)'Temperature related drift. Polynomial degree must be >=0. But requested degree is:',p%drift_k
                    valid=.false.
                end if
                
            else if ((tabline(1)=='SIGMA_FACTOR').and.StrIsnumber(tabline(2))) then
                !read(tabline(2),101)p%sigma_factor
                !p%sigma_factor = DNUM(tabline(2))
                p%sigma_factor = str2double(tabline(2))
                
                if (p%sigma_factor<0) then
                    p%sigma_factor = 1.0
                end if
                
            else if ((tabline(1)=='CONV').and.StrIsnumber(tabline(2))) then
                !read(tabline(2),101)p%critere_convergence
                !p%critere_convergence = DNUM(tabline(2))
                p%critere_convergence = str2double(tabline(2))
                if (p%critere_convergence<0) then
                    p%critere_convergence = 0.000001
                end if
                
            else if ((tabline(1)=='SIGMA_ADD').and.StrIsnumber(tabline(2))) then
                !read(tabline(2),101)p%sigma_add
                !p%sigma_add = DNUM(tabline(2))
                p%sigma_add = str2double(tabline(2))
                if (p%sigma_add<0) then
                    p%sigma_add = 0.0
                end if
                
            else if ((tabline(1)=='MODE').and.StrIsnumber(tabline(2))) then
                read(tabline(2),102)p%mode
                !p%mode = JNUM(tabline(2))
                if (P%mode<0 .or. p%mode>4) then
                    write(0,*)'No such adjustment model. Requested model is:', p%mode
                    valid=.false.
                end if
                
            else if ((tabline(1)=='TYPE_HISTO').and.StrIsnumber(tabline(2))) then
                read(tabline(2),102)k
                if (k==1) then
                    P%type_resid=.true.
                    !write(0,*)line
                else 
                    P%type_resid=.false.
                end if
                
            else if (tabline(1)=='WRITE_OBS') then
                if (tabline(2)=='N') then
                    P%print_obs = .false.
                else if (tabline(2)=='Y') then
                    P%print_obs = .true.   
                end if
   
            else if (tabline(1)=='OUTF') then
                p%nomficout=tabline(2)
                
            else if (tabline(1)=='CALF') then
                p%nomficcal=tabline(2)
                call verif_nomfic(p%nomficcal)
                
            else if (tabline(1)=='FCOR') then
                p%NCoordFic = p%NCoordFic + 1
                nom = tabline(2)
                call get_rep(nom,rep)
                p%tabCoordFic(p%NCoordFic)%nom = nom
                p%tabCoordFic(p%NCoordFic)%rep = rep 
                
            else if (tabline(1)=='WRITE_MAT' ) then  
                if (tabline(2)=='N') then
                    p%writemat=.false.
                else if (tabline(2)=='Y') then
                    p%writemat=.true.
                end if  
                
            else if (tabline(1)=='WRITE_RESID' ) then  
                if (tabline(2)=='N') then
                    p%write_resid=.false.
                else
                    p%write_resid=.true. 
                end if 
                
            else if (tabline(1)=='WRITE_TAU' ) then  
                if (tabline(2)=='Y') then
                    p%write_only_failed_tau_test=.true.
                else 
                    p%write_only_failed_tau_test=.false.
                end if  
                
            else if (tabline(1)=='WRITE_GRAV' ) then  
                if (tabline(2)=='N') then
                    p%write_gravity=.false.
                end if 
                
            else if (tabline(1)=='WRITE_DRIFT' ) then  
                if (tabline(2)=='N') then
                    p%write_drift=.false.
                end if  
                
            else if (tabline(1)=='WRITE_LIST_FIC' ) then  
                if (tabline(2)=='N') then
                    p%write_list_fic=.false.
                end if  
                
            else if (tabline(1)=='WRITE_LIST_STATION' ) then  
                if (tabline(2)=='N') then
                    p%write_list_station=.false.
                end if
            !_____________________________________________________contrainte de temps pour la réduction des fichiers c    
            else if (tabline(1)=='delai_max'.or.tabline(1)=='DELAI_MAX' .and. StrIsnumber(tabline(2))) then  
                read(tabline(2),102)p%delai_max
            !______________________________________________________création ou non de fichiers r ?  
            else if (tabline(1)=='CREATE_R') then
                if (tabline(2)=='Y') P%create_r = .true.
            !_____________________________________________________________________________________
            end if
        end if
        
        if (nc==5) then
             if  (tabline(1)=='REGION') then
                if (strisnumber(tabline(2)) .and. strisnumber(tabline(3)) .and.strisnumber(tabline(4))&
                & .and.strisnumber(tabline(5)) ) then
                
                    !read(tabline(2),103)p%lon1
                    !p%lon1 = DNUM(tabline(2))
                    p%lon1 = str2double(tabline(2))
                    
                    !read(tabline(4),103)p%lon2
                    !p%lon2 = DNUM(tabline(4))
                    p%lon2 = str2double(tabline(4))
                    
                    !read(tabline(3),103)p%lat1
                    !p%lat1 = DNUM(tabline(3))
                    p%lat1 = str2double(tabline(3))
                    
                    !read(tabline(5),103)p%lat2
                    !p%lat2 = DNUM(tabline(5))
                    p%lat2 = str2double(tabline(5))
                    
                end if
                
                !WRITE(0,*)p%lon1,p%lon2,p%lat1,p%lat2
             end if
        end if
        
        if (nc>=2 .and. nc<=4) then
        
            if  (tabline(1)=='RELF' .or. tabline(1)=='ABSF' .or. tabline(1)=='A10F') then  
            
                if (mod(p%NDataFic,datablock)==0) then
                        ! on atteint la fin du datablock, il faut réallouer de la place
                        allocate (TabDataFic2(p%NDataFic))
                        do i=1,p%NDataFic
                            TabDataFic2(i) = p%TabDataFic(i)
                        end do
                        deallocate (p%TabDataFic)
                        allocate (p%TabDataFic(p%NDataFic+datablock))
                        do i=1,p%NDataFic
                            p%TabDataFic(i) = TabDataFic2(i)
                        end do
                        deallocate (TabDataFic2)
                end if
            
            
                p%NDataFic = p%NDataFic + 1
                nom = tabline(2)
                call get_rep(nom,rep)
                p%TabDataFic(p%NDataFic)%nom = nom
                p%TabDataFic(p%NDataFic)%rep = rep
                p%TabDataFic(p%NDataFic)%sigma_f = 1d0
                p%TabDataFic(p%NDataFic)%sigma_a = 0.0
            
                if (StrIsnumber(tabline(3))) then
                    !read(tabline(3),101)p%TabDataFic(p%NDataFic)%sigma_f
                    !p%TabDataFic(p%NDataFic)%sigma_f = DNUM(tabline(3))
                    p%TabDataFic(p%NDataFic)%sigma_f = str2double(tabline(3)) 
                end if
            
                if (StrIsnumber(tabline(4))) then
                    !read(tabline(4),101)p%TabDataFic(p%NDataFic)%sigma_a
                    !p%TabDataFic(p%NDataFic)%sigma_a = DNUM(tabline(4))
                    p%TabDataFic(p%NDataFic)%sigma_a = str2double(tabline(4)) 
                end if
            
                if (tabline(1)=='RELF') then
                    p%ntabnomficrel=p%ntabnomficrel+1
                    p%TabDataFic(p%NDataFic)%typ = 0
                
                else if (tabline(1)=='ABSF') then  
                    p%ntabnomficabs=p%ntabnomficabs+1  
                    p%TabDataFic(p%NDataFic)%typ = 1
                
                else
                    p%ntabnomficA10=p%ntabnomficA10+1 
                    p%TabDataFic(p%NDataFic)%typ = 2
                end if
            
            end if
            
        end if
        
         
    end do
    668 continue 
    
    !do i=1,p.NDataFic
    !    Dfic = p.TabDataFic(i)
    !    write(0,'(A8,x,A80,x,I1,x,f6.3,x,f6.3)')Dfic.nom, Dfic.rep, Dfic.typ, Dfic.sigma_f, Dfic.sigma_a
    !end do
    
    nfic_grav_abs = p%ntabnomficabs + p%ntabnomficA10
    
    if (P%mode>=2 .and. nfic_grav_abs<=0) then
	    write(0,*)'Must provide fixed stations if weighted constraint solution is wanted'
	    valid = .false.
    end if
    
    if (p%ntabnomficabs>0 .OR. p%ntabnomficA10>0) then
	    p%lfix=.true.
    end if

    if (p%nomficcal/='') p%calf=.true.
    
    if  ((p%ntabnomficrel>0) .and. (p%nomficout/='') .and. (nfic_grav_abs>0) .and. valid) then
        Read_config_file = 0
    else 
        Read_config_file = 101        
    end if
    
    close(20)                                          
    return                               
                                      
end function Read_config_file

integer function libere_param(p) 

    use param_data
    implicit none     
    type (TParam) , intent(inout):: p            
            
    if (Associated(p%TabDataFic)) deallocate(p%TabDataFic)
    if (Associated(p%TabCoordFic)) deallocate(p%TabCoordFic)

    
    libere_param = 0
    
end function libere_param



end module Lect_param
