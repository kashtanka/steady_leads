      subroutine balance
      use steady_mod
      implicit none
      real A,B,C
      integer i,iu,in,ifx
      real f,fp
      real Ch,uvs,Chi,Chw
      real dz, T1,CF,CF2,Bprime,LW
      real K_th
      real nn, aKL, bKL ! nn-total cloud amount 0-1; aKL,bKL Konig-Langlo koefficients 
      nn=0.
      aKL=0.765
      bKL=0.22
 
      ro = 1.4

!--------ice or snow on top-------------!
!      if (hs.ne.0.) then
!         T1 = Tis
!         K_th = Ks
!         dz = hs
!      else
!         T1 = Tb
!         K_th = Ki
!         dz = hi
!      endif
      open(15,file='simple.txt')
!      do iu = 1,20
!         U = 1+0.25*iu
      do in = 1,19
         z0 = 0.0001 + (in-1)*0.002
         do ifx = 1, 39
            U = 1.+(ifx-1)*1.
!-------first guess---------------------!      
      Tsi = 245.
      Ta =250.
      Chi = 1.0e-4
      Chw = 1.3e-3
 !     LW =  50. !eps*sig*(Tsi**4. - (aKL+bKL*nn**3.)*Ta**4)
   ! write(0,*) 'LW=', LW
!--------begin iteration----------------!
      do i = 1,50

         
         A = eps*sig
         CF = (1.-frac)*Chw/((1.-frac)*Chw + frac*Chi)
         
         B = ro*cp*Chi*U*CF + Ks/hs*(Ki*hs)/(Ki*hs + Ks*hi)
!         
         C = -B*Tb - (aKL+bKL*nn
     :       **3.)*eps*sig*(Ta)**4.
         f = A*Tsi**4. + B*Tsi + C
         fp = 4.*A*Tsi**3. + B
         Tsi = Tsi - f/fp    
!         Tsi = 245.
!         write(0,*) i, Ta, Tsi
!         Ta = ((1.-frac)*Chw*Tb + frac*Chi*Tsi) /
!     :        ((1.-frac)*Chw + frac*Chi)
         call surf_layer_t
         Chi = - tst_s*ust_s/(U*(Tsi-Ta))
         Chw = - tst_s2*ust_s2/(U*(Tb-Ta))
         Ta = ((1.-frac)*Chw*Tb + frac*Chi*Tsi) /
     :        ((1.-frac)*Chw + frac*Chi)
!         CF = (1.-frac)*Chw/((1.-frac)*Chw + frac*Chi)
!         CF2 = ((1.-frac)*Chw + frac*Chi)/(frac*Chi)
!         Bprime = (ro*cp*Chi*U*CF + Ks/hs*(Ki*hs)/(Ki*hs + Ks*hi))*CF2
!         Ta = Tb - LW/Bprime 
!         Tsi = Ta*CF2 -(1.-frac)*Chw/frac/Chi*Tb
         
         
      enddo  
      write(15,'(f10.5,3f10.3)')z0,U,Ta,Tsi !U, Ta, Tsi, frac*Chi/(1-frac)/Chw
       write(0,*) z0, U,Ta, Tsi
 !     enddo
      enddo
      enddo

      end
