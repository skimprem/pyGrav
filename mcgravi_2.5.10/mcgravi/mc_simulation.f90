module MC_simulation


implicit none

contains

subroutine create_obs_rel
end subroutine create_obs_rel

REAL FUNCTION BruitG(iter,sd)
! Generation d'un bruit gaussien  
! entrees : 
!     iter, nombre d'iterations
!     sd,  ecart-type                        
! Sortie  : variable aleatoire gaussienne centree                             
    IMPLICIT none
    INTEGER iter,k
    REAL sd,r,theta,z1,z2,x,y,RAND,pi


    k=0
    pi=4.0*ATAN(1.0 )
    DO WHILE(k.LE.iter)

        k=k+1
        z1=RAND(0)
        z2=RAND(0)
        theta=2*pi*z2
        r=SQRT(-2*(sd**2)*ALOG(1-z1))
        x=r*COS(theta)
        y=r*SIN(theta)
        
    ENDDO

    BruitG=y

    RETURN
END FUNCTION BruitG



end module MC_simulation
