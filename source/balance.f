      subroutine balance
      use steady_mod
      implicit none
      real A,B,C
      integer i,iu,in,ifx
      real f,fp
      real Ch,uvs,Chi,Chw
      real dz, T1,CF,CF2,Bprime,LW
      real R
      real K_th
      real g,Rib_l
      real xB,xC,xD,Chi2,Chw2,Ta_l,Tsi_l,T0
      real nn, aKL, bKL ! nn-total cloud amount 0-1; aKL,bKL Konig-Langlo koefficients 
      nn=0.
      aKL=0.765
      bKL=0.22
      g = 9.81
      ro = 1.4
      R = 0 !-2./86400.*40

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
      open(15,file='reference_U_hdb.txt')
      open(16,file='linear_U.txt')
!      do iu = 1,20
!         hi = 0.2+0.1*iu

         do ifx = 1,100 !79
            U = 0.1+(ifx-1)*0.1
            !nn = 0. + ifx/10.
            !frac = 0.89999 + (ifx-1)/100.
!-------first guess---------------------!      
      Tsi = 245.
      Ta =250.
      Chi = 1.0e-4
      Chw = 1.3e-3
      LW =  eps*sig*(Tsi**4. - (aKL+bKL*nn**3.)*Ta**4)
   ! write(0,*) 'LW=', LW
!--------begin iteration----------------!
      do i = 1,50

         
         A = eps*sig
         CF = (1.-frac)*Chw/((1.-frac)*Chw + frac*Chi)
c         
         B = ro*cp*Chi*U*CF + Ks/hs*(Ki*hs)/(Ki*hs + Ks*hi)
         C = -B*Tb - R*ro*cp*Chi/((1.-frac)*Chw + frac*Chi)
     :   - (aKL+bKL*nn**3.)*eps*sig*(Ta)**4.
         f = A*Tsi**4. + B*Tsi + C
         fp = 4.*A*Tsi**3. + B
         Tsi = Tsi - f/fp    
!         Tsi = 240.
         call surf_layer_t
         Chi = - tst_s*ust_s/(U*(Tsi-Ta))
         Chw = - tst_s2*ust_s2/(U*(Tb-Ta))
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         Chi = - tst_s*ust_s/(U*(Tsi-Ta))
c         Chw = - tst_s2*ust_s2/(U*(Tb-Ta))
c         A = eps*sig
c         CF = (1.-frac)*Chw/((1.-frac)*Chw + frac*Chi)
c         
c         B = ro*cp*Chi*U*CF + Ks/hs*(Ki*hs)/(Ki*hs + Ks*hi)   
c         C = LW - B*Tb - R*ro*cp*Chi/((1.-frac)*Chw + frac*Chi)
c         Tsi = - C/B
c         Ta = ((1.-frac)*Chw*Tb + frac*Chi*Tsi + R) /
c     :        ((1.-frac)*Chw + frac*Chi)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         Ta = ((1.-frac)*Chw*Tb + frac*Chi*Tsi + R) /
     :        ((1.-frac)*Chw + frac*Chi)

         
         
      enddo  
cccccccccccccccccc  LINEAR cccccccccccccccccccccccc
      Chw2 = 1.8e-3
      Chi2 = 1.4e-3
      T0 = 235.
      xB = eps*sig*(1. - (aKL+bKL*nn**3))
      CF2 = (1.-frac)*Chw2/((1.-frac)*Chw2 + frac*Chi2)
      xC = ro*cp*Chi2*U*CF2 + Ks/hs*(Ki*hs)/(Ki*hs + Ks*hi)
      xD = -xC*Tb - R*ro*cp*Chi2/((1.-frac)*Chw2 + frac*Chi2)
      Tsi_l = (3.*xB*T0**4. - xD)/(4.*xB*T0**3. + xC)
      Ta_l = ((1.-frac)*Chw2*Tb + frac*Chi2*Tsi_l + R) /
     :        ((1.-frac)*Chw2 + frac*Chi2)

      Rib_l = (g/(0.5*(Ta_l+Tsi_l)))*(Ta_l - Tsi_l)*10/U**2.

      write(15,'(3f10.3,f12.5,2f10.4)')
     :               U,Ta,Tsi,frac*Chi/(1-frac)/Chw,
     :                          Chi*1000,Chw*1000
       write(0,*) U,Ta, Tsi
    
      write(16,'(3f10.3,f10.4)')U,Ta_l, Tsi_l, Rib_l
      enddo
 !     enddo


      end
