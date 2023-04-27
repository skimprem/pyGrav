Module MC_inversion
! Module d'inversion du système des moindres carrés
implicit none

contains

integer function calcul(MC)
    ! Minimum constraint or weighted constraint solution
    use write_matr
    use param_data
    use str_const
    use MC_data
    
    implicit none

    type (Tmc), intent(inout):: MC

    integer i,j,hh,k,ij,il
    integer code
    real*8 sigma0,vtpv,sigma0_2,vtmp
   
    ! Automatic arrays, but by allocation to avoid stack overflow, make sure
    ! to deallocate them
    real*8,allocatable,dimension(:)::c,nn,uvec
    if (allocated(c)) deallocate(c)
    if (allocated(nn)) deallocate(nn)
    if (allocated(uvec)) deallocate(uvec)
    allocate (c((MC%Nb_inc+1)*MC%Nb_inc/2))
    allocate (nn((MC%Nb_inc+1)*MC%Nb_inc/2))
    allocate (uvec(MC%Nb_inc))
    
    code = 0
   
    !if (param%writemat) call write_mat(MC%atpa,MC%Nb_inc,MC%Nb_inc,'Matrice_AtPA.txt')

    hh=0
    do k=1,MC%Nb_inc
        do j=1,k
            hh=hh+1
            nn(hh)=MC%AtPA(k,j)
        end do
    end do
    uvec=MC%AtPB
    
    !do I=1,MC%Nb_inc
    !    write(0,*)I,uvec(I)
    !end do
    
    if (param%lg=='F') then 
        write(0,*)inversionF
    else 
        write(0,*)inversionA 
    end if

    code = solution(nn,uvec,MC%Nb_inc,1,c)
    if (code == 110) then
        calcul = code 
        return
    end if
        
    MC%X = uvec
    

    
    vtpv=0
    do k=1,MC%Nb_obsRel+MC%Nb_obsAbs
        Vtmp=0.D0
        DO I=1,MC%Nb_inc
            Vtmp=Vtmp+MC%A(k,i)*uvec(i)
        ENDDO
        MC%V(k)=Vtmp-MC%B(k)
        vtpv=vtpv+MC%V(k)**2*MC%P(k)
    end do
    sigma0_2=VTPV/MC%dof
    MC%sigma0=DSQRT(sigma0_2)

    ! conversion de C (colonne) en SX = (AtPA)^-1 carrée

    do I=1,MC%Nb_inc
        IL=(I-1)*I/2
            do J=1,I
                IJ=IL+J
                MC%SX(I,J)=c(IJ)
                MC%SX(J,I)=MC%SX(I,J)
            end do
    end do
    
    !write(0,*)MC%sigma0
    ! Calcul de la variance sur les parametres ********************************    
    do i=1,MC%Nb_inc
        if (MC%SX(i,i)<0) write(0,*)'Negative variance : ',i
        MC%sig(i) = DSQRT(MC%SX(i,i) * MC%sigma0**2)
    end do

    if (param%writemat) call write_mat(MC%SX,MC%Nb_inc,MC%Nb_inc,'Matrice_invAtPA.txt')

    deallocate (c,nn)
    return
    
end function calcul


integer function simul(MC)
    ! Minimum constraint or weighted constraint solution
    use write_matr
    use param_data
    use str_const
    use MC_data
    
    implicit none

    type (Tmc), intent(inout):: MC

    integer i,j,hh,k,ij,il
    integer code
    real*8 sigma0,vtpv,sigma0_2,vtmp
   
    real*8,allocatable,dimension(:)::c,nn,uvec
    if (allocated(c)) deallocate(c)
    if (allocated(nn)) deallocate(nn)
    if (allocated(uvec)) deallocate(uvec)
    allocate (c((MC%Nb_inc+1)*MC%Nb_inc/2))
    allocate (nn((MC%Nb_inc+1)*MC%Nb_inc/2))
    allocate (uvec(MC%Nb_inc))
    
    code = 0
   
    if (param%writemat) call write_mat(MC%atpa,MC%Nb_inc,MC%Nb_inc,'Matrice_AtPA.txt')

    hh=0
    do k=1,MC%Nb_inc
        do j=1,k
            hh=hh+1
            nn(hh)=MC%atpa(k,j)
        end do
    end do
    uvec=MC%atpB
    
    if (param%lg=='F') then ; write(0,*)inversionF ; else ; write(0,*)inversionA ; end if

    code =  solution(nn,uvec,MC%Nb_inc,2,c)
    if (code == 110) then
        simul = code 
        return
    end if
    
    
    MC%X = uvec

    vtpv=0d0
    do k=1,MC%Nb_obsRel+MC%Nb_obsAbs
        Vtmp=0.D0
        DO I=1,MC%Nb_inc
            Vtmp=Vtmp+MC%A(k,i)*uvec(i)
        ENDDO
        MC%V(k)=Vtmp-MC%B(k)
        vtpv=vtpv+MC%V(k)**2*MC%P(k)
    end do
    sigma0_2=VTPV/MC%dof
    MC%sigma0=DSQRT(sigma0_2)

    ! conversion de C (colonne) en SX = (AtPA)^-1 carrée

    DO I=1,MC%Nb_inc
        IL=(I-1)*I/2
            DO J=1,I
                IJ=IL+J
                MC%SX(I,J)=c(IJ)
                MC%SX(J,I)=MC%SX(I,J)
            ENDDO
    ENDDO
    
    ! Calcul de la variance sur les parametres ********************************    
    do i=1,MC%Nb_inc
        MC%sig(i) = MC%SX(i,i) * MC%sigma0**2
    end do
    
    if (param%writemat) call write_mat(MC%SX,MC%Nb_inc,MC%Nb_inc,'Matrice_invAtPA.txt')
    
    
    
    deallocate (c,nn)
    deallocate (MC%atpB,MC%atpa)
    simul = code
    return
    
end function simul

  

integer function solution(A,X,N,mode,C)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C PROGRAM TO SOLVE FOR A LINEAR SYSTEM USING CHOLESKY DECOMPOSTION%
!C
!C    A %%% A psd matrix, or the normal matrix% On output, it
!c          becomes a lower triangular matrix from the Cholesky
!c          decomposition or its inverse, C
!C    X %%% on input it is the observation vector; on output
!c          it is the solution vector
!c    C %%% inverse of A
!c    work %% a work array with dim=N
!C    N %%% order of A
!c    mode %% 0, solve for x only
!c            1, invert A (the inverse is C) and solve for x
!c            2, invert A only% X is not used in this mode
!C
!C              CHEINWAY HWANG
!c 	            Feb 20, 1999
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      use param_data
      use str_const
      
      integer N
      real*8  A(*),SUM !,work(N)
      real*8, intent(out):: C(N*(N+1)/2)
      real*8, intent(inout):: X(N)
      integer order,nsing,mode,K,I,J,KK,II,IK,II1
      ! CHOLESKY DECOMPOSITION of A
      
      real*8,allocatable,dimension(:):: work
      if (allocated(work)) deallocate(work)
      allocate(work(N))
      
      nsing=0
      if(mode.lt.0 .or. mode.gt.3) stop 'incorrect mode'
      
      !write(0,*)'debut Cholesky'
      
      DO 1 K=1,N
        kk=k*(k-1)/2
        DO 2 I=1,K-1
            II=I*(I-1)/2
            SUM=0.D0
            DO 3 J=1,I-1
            3    SUM=SUM+A(II+J)*A(KK+J)
            II1=KK+I
        2    A(II1)=(A(II1)-SUM)/A(II+I)
        SUM=0.D0
        DO 4 J=1,K-1
        IK=KK+J
    4    SUM=SUM+A(IK)*A(IK)
        IK=KK+K
        A(IK)=A(IK)-SUM
        IF(A(IK).LE.0.0) THEN
            if (param%lg=='F') then
             
                write(0,*)neg_diag_elemF,k,a(ik) 
                write(67,*)neg_diag_elemF,k,a(ik) 
            else 
                write(0,*)neg_diag_elemA,k,a(ik) 
                write(67,*)neg_diag_elemA,k,a(ik)
            end if
            if (param%lg=='F') then 
                write(0,*)matrix_not_psdF 
                write(67,*)matrix_not_psdF 
            else 
                write(0,*)matrix_not_psdA 
                write(67,*)matrix_not_psdA
            end if
            solution = 110
            return
        END IF
1     A(IK)=DSQRT(A(IK))
      if(mode.eq.0 .or.(mode.eq.1)) then
        call LTRISOL(A,X,N)
        call UTRISOL(A,X,N)
      end if
!c Invert A
      if((mode.eq.1) .or. (mode.eq.2)) then
        do order=1,n
            do i=1,n
                work(i)=0
            end do
            work(order)=1
            call LTRISOL(A,work,N)
            Call UTRISOL(A,work,N)
            do i=order,n
                ii=i*(i-1)/2
                c(ii+order)=work(i)
            end do
        end do
        do i=1,n*(n+1)/2
        a(i)=c(i)
        end do
      end if
!c      write(0,*)'Nsing=',nsing
      if (allocated(work)) deallocate(work)
      RETURN
END function solution



SUBROUTINE LTRISOL(A,X,N)
!C SUBROUTINE TO SOLVE A LINEAR SYSTEM% A IS A LOWER TRIANGULAR MATRIX%
!C INPUT :
!C        A ----- A NONSINGULAR LOWER TRIANGULAR MATRIX
!C        N ----- ORDER OF T
!C        X  ---- OBSERVATION VECTOR
!C OUTPUT :
!C        X ----- SOLUTION VECTOR
!C
       REAL*8 A(*),X(*)
       REAL*8 SUM
       integer N,K,I,II,J
       DO 1 I=1,N
            II=I*(I-1)/2
            SUM=0.D0
            DO 2 J=1,I-1
 2          SUM=SUM+A(II+J)*X(J)
1           X(I)=(X(I)-SUM)/A(II+I)
       RETURN
END SUBROUTINE LTRISOL

SUBROUTINE UTRISOL(A,X,N)
!C SUBROUTINE TO SOLVE A LINEAR SYSTEM% A IS AN UPPER
!C TRIANGULAR MATRIX FROM THE CHOLESKY DECOMPOSITION%
!C INPUT :
!C        A ----- A NONSINGULAR LOWER TRIANGULAR MATRIX
!C        N ----- ORDER OF A
!C        X  ---- OBSERVATION VECTOR
!C OUTPUT :
!C        X ----- SOLUTION VECTOR
!C
        integer N,K,I,J,II,nsta
       REAL*8 A(*),X(*)
       REAL*8 SUM
!C BACKWARD SUBSTITUTION
      DO J=N,1,-1
        SUM=0.D0
        DO I=J+1,N
            II=I*(I-1)/2
            SUM=SUM+A(II+J)*X(I)
        END DO
        nsta=J*(J+1)/2
        X(J)=(X(J)-SUM)/A(nsta)
      END DO
      RETURN
END SUBROUTINE UTRISOL


end module MC_inversion
