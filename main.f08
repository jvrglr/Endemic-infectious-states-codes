program Main_SIS
  !Main program, depends on the rest of modules (declarations_module, functions and subroutines))
  use declarations_module
  use functions
  use subroutines
  implicit none
  integer*8 :: ii,seeds,jj,dum
  double precision :: tf,R0min,R0max,tm,tm2,err,rf,rm,rm2,M,hname
  character (len=10) :: addname
  integer*8 :: i_dran
  call assingments()
  call dran_ini(time())


  print *, "n=",jjcontrol,"h=",h,"R0=",R0,"V=",V
  call dran_ini(time()+jjcontrol)
  hname=h !;h=h/dble(400)
  beta=R0*mu
  M=0
  addname=""
  open(unit=9009, file="data/t_h"//trim(str(nint(hname*1000)))// &
  "_R0"//trim(str(nint(R0*100)))// &
  "_M"//trim(str(nint(M*10**6)))// &
  "_n"//trim(str(int(jjcontrol)))// &
  trim(addname)//".dat", &
   status="unknown", action="write") ! writes h,b and final iteration STATISTICS OF THE WHOLE

  if ( ios /= 0 ) stop "Error opening file "


  tm=0.0d0;tm2=0.0d0
  rm=0.0d0;rm2=0.0d0
  do ii = 1, realiz, 1
    seeds=126 !100 !186
    S=N*(1-alpha);I=0
    do jj = 1, seeds, 1
      dum=i_dran(V)
      S(dum)=S(dum)-1
      I(dum)=I(dum)+1
    end do
    !print *, "Total number of initial seeds=",sum(I)

    t=0.0d0
    call Draw_trajectory(tf,rf)
    !write(*,*) "ii=",ii,"t=",tf,"rf=",rf
    tm=tm+tf;tm2=tm2+tf*tf
    rm=rm+rf;rm2=rm2+rf*rf
    write(9009,*) jjcontrol,H,R0,tf,tf,rf,rf
  end do
  close(9009)
  !print*, tm,tm

  tm=tm/realiz;tm2=tm2/realiz
  rm=rm/realiz;rm2=rm2/realiz
  !write(9009,*) R0,h,tm,sqrt(abs(tm2-tm*tm)/realiz),rm,sqrt(abs(rm2-rm*rm)/realiz)
  print*, jjcontrol,H,R0,tm,sqrt(abs(tm2-tm*tm)/realiz),rm,sqrt(abs(rm2-rm*rm)/realiz)

  open(unit=9009, file="data/t_h"//trim(str(nint(hname*1000)))// &
  "_R0"//trim(str(nint(R0*100)))// &
  "_M"//trim(str(nint(M*10**6)))// &
  "_n"//trim(str(int(jjcontrol)))// &
  trim(addname)//".dat", &
   status="unknown", action="write") ! writes h,b and final iteration STATISTICS OF THE WHOLE


  write(9009,*) jjcontrol,H,R0,tm,sqrt(abs(tm2-tm*tm)/realiz),rm,sqrt(abs(rm2-rm*rm)/realiz)


  !Deallocate and close files
  deallocate(S);deallocate(I)
  close(9009)

101 end program Main_SIS
