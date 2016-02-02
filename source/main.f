      program steady_leads
      use steady_mod
      implicit none

      U = 5.
      Tb = 271.35  ! sea ice bottom temperature
      Ki = 2.2     ! thermal conductivity of ice (Wm-1K-1)
      Ks = 0.21    ! thermal conductivity of snow (Wm-1K-1)
      roi = 916.   ! sea ice density (kgm-3)
      ci = 2100.   ! sea ice heat capacity
      rosn = 290.    ! snow density (kgm-3)
      hi = 2.       ! ice thickness (m)
      hs = 0.3    ! snow thickness (m)
      frac = 0.95     ! sea ice fraction
      z_sl = 5.
      z0 = 0.001

      write (0,*) 'START!'
      
      call balance
      
      
      end

      
