module Subroutines

  !Module with public subroutines.
  !Depends on declarations_module and functions.

  use declarations_module
  use functions

contains

  subroutine Draw_trajectory(tf,Rf)
    implicit none
    double precision, intent(out) :: tf,Rf
    double precision :: W,pb,pm,u,dum
    double precision, dimension(V) :: q
    integer*8 :: sumI,sumSI,pos,ii,count,iterations,inc,StopI
    double precision :: dran_u
    integer*8 :: i_dran

    StopI=-1 !When Sum(I)==StopI simulation stops

    open(unit=1001, file="history/Fig/R0_"//trim(str(nint(R0*100)))// &
    "_h_"//trim(str(nint(h*1000)))// &
    "_V_32_jj"// &
    trim(str(int(jjcontrol)))//".dat", iostat=ios, status="unknown", action="write")

    ! open(unit=1001, file="inc_Bea/R0_"//trim(str(nint(R0*100)))// &
    ! "_h_"//trim(str(nint(h*1000)))// &
    ! "_V_32_jj"// &
    ! trim(str(int(jjcontrol)))//".dat", iostat=ios, status="unknown", action="write")

    ! open(unit=1001, file="Linear_growth/R0_"//trim(str(nint(R0*100)))// &
    ! "_h_"//trim(str(nint(h*1000)))// &
    ! "_V_"//trim(str(int(V)))//"_jj"// &
    ! trim(str(int(jjcontrol)))//".dat", iostat=ios, status="unknown", action="write")

    !open(unit=1001, file="history/R0_0d85_h_0_V_32.dat", iostat=ios, status="unknown", action="write")
    ! if ( ios /= 0 ) stop "Error opening file "
    ! print*,"I(start)=", sum(I),size(I)

    t=0.0d0
    sumI=sum(I);sumSI=sum(S*I)
    count=0;iterations=0
    inc=0 !Daily incidence
    do while (( sumI.gt.StopI ).and.(t.lt.tmax))
      pb=beta*sumSI/N;pm=mu*sumI
      W=pm+pb+h
      u=0.0d0
      do while ( u.eq.0.0d0 ) !Sometimes I obtain u=0
        u=dran_u()
      end do
      t=t-log(u)/W;iterations=iterations+1
      pb=pb/W;pm=pm/W
      ! print *, "pb=",pb,"pm=", pm,"h=",h,"W=",W
      ! print*, "t=",t,"I=",I,"S=",S
      if (( abs(pb)>1 ).or.(abs(pm)>1)) then
        print*, "FATAL ERROR, probabilities >1"
        exit
      end if
      u=dran_u()
      if ( u.lt.pb ) then
        !Create I due to contact with infected
        q=S*I/sumSI
        u=dran_u()
        call search_list_binary_algoritm(q,pos,u)
        ! print*, "pos=",pos
        I(pos)=I(pos)+1;S(pos)=S(pos)-1
        inc=inc+1
      elseif ( u.lt.(pb+pm) ) then
        !Destroy one infected
        q=I/sumI
        u=dran_u()
        call search_list_binary_algoritm(q,pos,u)
        I(pos)=I(pos)-1 !R=R+1
      else
        !Create I due to external seeding
        pos=i_dran(V)
        u=dran_u()
        dum=(dble(S(pos))/N)
        if ( u<dum) then
          !remove S
          S(pos)=S(pos)-1;I(pos)=I(pos)+1
        elseif (  u<(dum+(N-I(pos)-S(pos))/N) ) then
          !remove R
          I(pos)=I(pos)+1
          inc=inc+1
        end if
      end if

      sumI=sum(I);sumSI=sum(S*I)
      if ( t.gt.count ) then
        count=count+1
        write(1001,*) t,nint(t),count,inc,sumI,V*N-sum(I+S)
        inc=0
        ! do ii = 1, V, 1
        !   if ( I(ii)>0 ) then
        !     write(1001,*) ii,t,dble(S(ii))/N,dble(I(ii))/N,dble(N-I(ii)-S(Ii))/N,iterations
        !   end if
        ! end do

      end if

      !print*, pos,t, sumI,sum(S),dble(V*N-sumI-sum(S))/(N*V)
       ! write(1001,*) t,sum(S),sumI,dble(V*N-sumI-sum(S))/(N*V)

    end do
      close(1001)

    tf=t
    Rf=(V*N-sum(I+S))/(V*N)
    !print *, "final t=",tf,"final count=",count,"final R=",Rf
  end subroutine Draw_trajectory

  subroutine search_list_binary_algoritm(list,position,p)
    !Rafle event:
    !Given a list of probabilities called "list" such that sum(list)=1 and a probability p.
    !Look for "position" such that C(position)>=p and C(j)<p for all j in [1,position[.
    !Where C is the cumulative of list: C(i)=list(1)+list(2)+...+list(i)-
    !REFERENCE:Brainerd, W. S. (2015). Guide to Fortran 2008 programming (p. 141). Berlin: Springer.

    implicit none
    double precision, dimension(:), intent (in) :: list
    double precision, intent (in) :: p
    integer*8, intent(out) :: position
    double precision, dimension(size(list)) :: C
    integer*8 ii,N,first,last,half


    N=size(list) !It would be cool to define this as a parameter (constant), I don't know how...
    C(1)=list(1)
    do ii = 2, N, 1 !Compute cumulative of list
      c(ii)=C(ii-1)+list(ii)
    end do

    first=1;last=N
    do while ( first.ne.last )
      half=(first+last)/2
      if ( p>C(half) ) then
        first=half+1
      else
        last=half
      end if
    end do

    position=first

  end subroutine search_list_binary_algoritm

  subroutine read_xF_tf()
    !Example of subroutine, read data file
    implicit none
    integer*8 :: ios,i,dum,datapoints
    double precision :: dummy

    open(unit=1001, file="data/F_Target_test3.dat", iostat=ios, status="old", action="read")
    if ( ios /= 0 ) stop "Error opening file "
    do i = 1, datapoints, 1
      read(1001,*) dummy,dummy,dummy,dum
    end do


  end subroutine read_xF_tf

end module subroutines
