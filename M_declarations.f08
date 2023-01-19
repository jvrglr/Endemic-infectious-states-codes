module declarations_module
  !Module for public variables to be used in the rest of the modules
  implicit none
  public

  integer*8 :: ios,V,N,realiz,jjcontrol
  double precision :: t,mu,R0,beta,tmax,h,alpha
  double precision, dimension (:),allocatable :: S,I


contains

  subroutine assingments()
    implicit none
    double precision,dimension(2,21):: dum,attack_rates
    integer*4 :: ii
    !! SOLUCION CUTRE: since I don't know how to compute Lambert function in FORTRAN, I use values from Python
    !!attack_rates(1,:)--> values of R0 (independent variable)
    !!attack_rates(2,:)--> values of the attack rate (dependent variable)
    !! R0 is an input for the program, I have to loock for the value of the attack rate linked with the input R0
    !! alpha=attack_rates(2,:)(input R0)
    read(*,*) jjcontrol,h,R0

    if ( R0.ge.1.0 ) then
      open(unit=1001, file="Attack_rates.dat", status="old", action="read")
          read(1001,*) attack_rates
      close(1001)

      do ii = 1, 21, 1
        if (abs(attack_rates(1,ii)-R0).le.0.01) then
          alpha=attack_rates(2,ii)
          print *, attack_rates(1,ii),alpha
          exit
        endif
      end do
    else
      alpha=0
    end if

    V=32 !22 !Number of independent populations
    !tmax=4*10**4 !maximum simulation time
    tmax=140 !1000 !365
    allocate(S(V));allocate(I(V))
    N=7000 !Number of inhabitants in every population
    S=N*(1-alpha);S(1)=N*(1-alpha)-1
    I=0;I(1)=1
    t=0.0d0
    mu=1.0d0/3.7d0 !Recovery rate

    realiz=1



  end subroutine assingments

end module declarations_module
