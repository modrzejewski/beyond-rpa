module rpa_CCD_Corrections_Experimental
      use arithmetic      
      use real_linalg
      use rpa_definitions
      use clock
      use cmpidx
      
      implicit none

contains


    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !--------------------------------------------------          Calculation of Taibj amplitudes         --------------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine amplitudes_aibj(Taibj, Dacij, NOcc, NVirt, NVecsT2, Uaim, Am)


                integer, intent(in)                                             :: NOcc
                integer, intent(in)                                             :: NVirt
                integer, intent(in)                                             :: NVecsT2
                real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in)          :: Uaim
                real(F64), dimension(:), intent(in)                             :: Am
                             
                real(F64), dimension(:, :, :), allocatable                      :: Uami
                real(F64), dimension(:, :, :), allocatable                      :: AUmai               
                real(F64), dimension(NVirt, NVirt, NOcc, NOcc), intent(out)     :: Taibj
                real(F64), dimension(NVirt**2, NOcc, NOcc), intent(out)         :: Dacij
                
                integer :: a, i, j, mu
                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "--------------------- Amplitudes calculation in progress --------------------"
                
                
            !------------------------- Amplitudes calculation intermediates -------------------------
            allocate(Uami(NVirt, NVecsT2, NOcc))
            allocate(AUmai(NVecsT2, NVirt, NOcc))
            Uami = ZERO
            AUmai = ZERO
            do i = 1, NOCc
                do a = 1, NVirt
                    do mu = 1, NVecsT2
                        Uami(a, mu, i) = Uaim(a, i, mu)
                        AUmai(mu, a, i) = Uaim(a, i, mu) * Am(mu)
                    end do
                end do
            end do
            
            !------------------------- Amplitudes Taibj(a, b, i, j) -----------------------
            Taibj = ZERO
            Dacij = ZERO
            do j = 1, NOcc
                do i = 1, NOcc
                    Taibj(:, :, i, j) = matmul(Uami(:, :, i), AUmai(:, :, j)) 
                    Dacij(:, i, j) = reshape(Taibj(:, :, i, j), [NVirt**2])
                end do
            end do
            
            
            call msg("T2 amplitudes computed in " // str(clock_readwall(timer),d=1) // " seconds")
    end  subroutine amplitudes_aibj
    
    
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !------------------------------------------------          Calculation of ERIs intermediates          -------------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine ERI_intermediates(ZXXkij, ZYXkai, ZYYkab, NOcc, NVirt, NGridTHC, NCholesky, Zgk, Yga, Xgi)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NGridTHC
                integer, intent(in)                                         :: NCholesky
                real(F64), dimension(:, :), intent(in)                      :: Zgk
                real(F64), dimension(NGridTHC, NVirt), intent(in)           :: Yga
                real(F64), dimension(NGridTHC, NOcc), intent(in)            :: Xgi
                
                real(F64), dimension(:), allocatable                        :: XXg
                real(F64), dimension(:), allocatable                        :: YXg
                real(F64), dimension(:), allocatable                        :: YYg
                real(F64), dimension(NCholesky, NOcc, NOcc), intent(out)    :: ZXXkij 
                real(F64), dimension(NCholesky, NVirt, NOcc), intent(out)   :: ZYXkai   
                real(F64), dimension(NCholesky, NVirt, NVirt), intent(out)  :: ZYYkab        
                
                integer :: i, j, a, b         
                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "---------------- ERIs intermediates calculation in progress -----------------"
                
                
!            !$omp parallel do collapse(2) private(d, b) default(shared)
!            !$omp end parallel do
                
            !------------------------- ZXXkij(k, i, j) calculation procedure -------------------------
            
            !$omp parallel private(XXg) &
            !$omp private(i, j)
            allocate(XXg(NGridTHC))
            !$omp do collapse(2)
            do i = 1, NOcc                               
                  do j = 1, NOcc
                        XXg(:) = Xgi(:, i) * Xgi(:, j)
                        call real_ATv(ZXXkij(:, i, j), Zgk, XXg)
                  end do
            end do
            !$omp end do
            !$omp end parallel
                
                
            !------------------------- ZYXkai(k, a, i) calculation procedure -------------------------
            
            !$omp parallel private(YXg) &
            !$omp private(a, i)
            allocate(YXg(NGridTHC))
            !$omp do collapse(2)
            do a = 1, NVirt                                
                  do i = 1, NOcc
                        YXg(:) = Yga(:, a) * Xgi(:, i)
                        call real_ATv(ZYXkai(:, a, i), Zgk, YXg)
                  end do
            end do
            !$omp end do
            !$omp end parallel
                
                
            !------------------------- ZYYkab(k, a, b) calculation procedure -------------------------
            
            !$omp parallel private(YYg) &
            !$omp private(a, b)
            allocate(YYg(NGridTHC))
            !$omp do collapse(2)
            do a = 1, NVirt                                
                  do b = 1, NVirt
                        YYg(:) = Yga(:, a) * Yga(:, b)
                        call real_ATv(ZYYkab(:, a, b), Zgk, YYg)
                  end do
            end do
            !$omp end do
            !$omp end parallel
                
            
            call msg("ERIs intermediates computed in " // str(clock_readwall(timer),d=1) // " seconds")
    end  subroutine ERI_intermediates
    
    
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !--------------------------------------------------          Calculation of Vaibj integrals          --------------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine ERI_aibj(Vaibj, NOcc, NVirt, NCholesky, ZYXkai)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NCholesky
                real(F64), dimension(NCholesky, NVirt, NOcc), intent(in)    :: ZYXkai
                   
                real(F64), dimension(NVirt, NVirt, NOcc, NOcc), intent(out) :: Vaibj 
                
                integer :: i, j
                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "------------------- ERIs (ai|bj) calculation in progress --------------------"
                
                
            !------------------------- ERIs type Vaibj(a, b, i, j) -----------------------
            
            !$omp parallel private(i, j)
            !$omp do collapse(2)
            do j = 1, NOcc
                do i = 1, NOcc
                    call real_aTb(Vaibj(:, :, i, j), ZYXkai(:, :, i), ZYXkai(:, :, j))      
                end do
            end do
            !$omp end do
            !$omp end parallel
            
            call msg("ERIs (ai|bj) computed in " // str(clock_readwall(timer),d=1) // " seconds")
    end  subroutine ERI_aibj
    
    
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !--------------------------------------------------          Calculation of Vijkl integrals          --------------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine ERI_ijkl(Vijkl, NOcc, NVirt, NCholesky, ZXXkij)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NCholesky
                real(F64), dimension(NCholesky, NOcc, NOcc), intent(in)     :: ZXXkij 
                  
                real(F64), dimension(NOcc, NOcc, NOcc, NOcc), intent(out)   :: Vijkl
                
                integer :: i, j, l    
                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "------------------- ERIs (ij|kl) calculation in progress --------------------"
                
                
            !------------------------- ERIs type Vijkl(i, k, j, l) -----------------------
            
            !$omp parallel private(j, l)
            !$omp do collapse(2)
            do l = 1, NOcc
                do j = 1, NOcc
                    call real_aTb(Vijkl(:, :, j, l), ZXXkij(:, :, j), ZXXkij(:, :, l))      
                end do
            end do
            !$omp end do
            !$omp end parallel
            
            
            call msg("ERIs (ij|kl) computed in " // str(clock_readwall(timer),d=1) // " seconds")
      end  subroutine ERI_ijkl


    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !--------------------------------------------------          Calculation of Vijab integrals          --------------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine ERI_ijab(Vijab, NOcc, NVirt, NCholesky, ZXXkij, ZYYkab)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NCholesky
                real(F64), dimension(NCholesky, NOcc**2), intent(in)        :: ZXXkij 
                real(F64), dimension(NCholesky, NVirt**2), intent(in)       :: ZYYkab  
                  
                real(F64), dimension(NVirt**2, NOcc**2), intent(out)        :: Vijab
                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "------------------- ERIs (ij|ab) calculation in progress --------------------"
                
                
            !------------------------- ERIs type Vijab(a, b, i, j) -----------------------
            
            call real_aTb(Vijab, ZYYkab, ZXXkij)      
            
            
            call msg("ERIs (ij|ab) computed in " // str(clock_readwall(timer),d=1) // " seconds")
      end  subroutine ERI_ijab


    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !----------------------------------------          Calculation of Ec1b, Ec2b, Ec2c, Ec2d corrections          -----------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine rpa_Ec2b_corection(Energy, NOcc, NVirt, Taibj, Vaibj)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                real(F64), dimension(:), intent(inout)                      :: Energy
                real(F64), dimension(:, :, :, :), intent(in)                :: Taibj
                real(F64), dimension(NVirt, NVirt, NOcc, NOcc), intent(in)  :: Vaibj 
                
                real(F64) :: Ec1b, Ec2b, Ec2c, Ec2d
                real(F64), dimension(:, :), allocatable                     :: VTac
                
                integer :: i, j, k, a, c
                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "-------------- Ec1b, Ec2b, Ec2c, Ec2d calculation in progress ---------------"
                
                
            !------------------------- CCD Ec1b main calculation -------------------------
            Ec1b = ZERO
            do j = 1, NOcc                                
                do i = 1, NOcc
                    do a = 1, NVirt
                    Ec1b = Ec1b + DOT_PRODUCT(Vaibj(:, a, j, i), Taibj(:, a, i, j))
                    end do
                end do
            end do
            
            allocate(VTac(NVirt, NVirt))
            Ec2b = ZERO
            Ec2d = ZERO
            do j = 1, NOcc                                
                do k = 1, NOcc
                    do i = 1, NOcc
                    VTac = ZERO
                    VTac = matmul(Vaibj(:, :, i, j), Taibj(:, :, k, j))
                        do c = 1, NVirt
                        Ec2b = Ec2b + DOT_PRODUCT(VTac(:, c), Taibj(:, c, i, k))
                        Ec2d = Ec2d + DOT_PRODUCT(VTac(:, c), Taibj(:, c, k, i))
                        end do
                    end do
                end do
            end do
            
            Ec1b = -TWO * Ec1b
            Ec2b = -FOUR * Ec2b
            Ec2c = Ec2b
            Ec2d = TWO * Ec2d
            Energy(RPA_ENERGY_CUMULANT_1B) = Ec1b
            Energy(RPA_ENERGY_CUMULANT_2B) = Ec2b
            Energy(RPA_ENERGY_CUMULANT_2C) = Ec2c
            Energy(RPA_ENERGY_CUMULANT_2D) = Ec2d
            
            
            call msg("Ec1b, Ec2b, Ec2c, Ec2d corrections computed in " // str(clock_readwall(timer),d=1) // " seconds")
    end  subroutine rpa_Ec2b_corection
    
    
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !----------------------------------------          Calculation of Ec2g, Ec2h, Ec2i, Ec2j corrections          -----------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine rpa_Ec2g_corection(Energy, NOcc, NVirt, Taibj, Vijab)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                real(F64), dimension(:), intent(inout)                      :: Energy
                real(F64), dimension(:, :, :, :), intent(in)                :: Taibj
                real(F64), dimension(NVirt, NVirt, NOcc, NOcc), intent(in)  :: Vijab
                
                real(F64) :: Ec2g, Ec2h, Ec2i, Ec2j
                real(F64), dimension(:, :), allocatable                     :: VTbc
                
                integer :: i, j, k, c
                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "-------------- Ec2g, Ec2h, Ec2i, Ec2j calculation in progress ---------------"
                
                
            !------------------------- CCD Ec2g, Ec2h, Ec2i, Ec2j main calculation -------------------------
            allocate(VTbc(NVirt, NVirt))
            Ec2g = ZERO
            Ec2i = ZERO
            do i = 1, NOcc                                
                do k = 1, NOcc
                    do j = 1, NOcc
                    VTbc = ZERO
                    VTbc = matmul(Vijab(:, :, j, i), Taibj(:, :, j, k))
                        do c = 1, NVirt
                        Ec2g = Ec2g + DOT_PRODUCT(VTbc(:, c), Taibj(:, c, i, k))
                        Ec2i = Ec2i + DOT_PRODUCT(VTbc(:, c), Taibj(:, c, k, i))
                        end do
                    end do
                end do
            end do
            
            Ec2j = ZERO
            Ec2h = ZERO
            do i = 1, NOcc                                
                do k = 1, NOcc
                    do j = 1, NOcc
                    VTbc = ZERO
                    VTbc = matmul(Vijab(:, :, j, i), Taibj(:, :, k, j))
                        do c = 1, NVirt
                        Ec2h = Ec2h + DOT_PRODUCT(VTbc(:, c), Taibj(:, c, k, i))
                        end do
                    end do
                end do
            end do
            
            Ec2g = -FOUR * Ec2g
            Ec2i = TWO * Ec2i
            Ec2h = -FOUR * Ec2h
            Ec2j = Ec2i
            Energy(RPA_ENERGY_CUMULANT_2g) = Ec2g
            Energy(RPA_ENERGY_CUMULANT_2i) = Ec2i
            Energy(RPA_ENERGY_CUMULANT_2h) = Ec2h
            Energy(RPA_ENERGY_CUMULANT_2j) = Ec2j
            
            call msg("Ec2g, Ec2h, Ec2i, Ec2j corrections computed in " // str(clock_readwall(timer),d=1) // " seconds")
    end  subroutine rpa_Ec2g_corection
    
    
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !----------------------------------------------          Calculation of Ec2e, Ec2f corrections          -----------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine rpa_Ec2e_corection(Energy, NOcc, NVirt, Taibj, Vijkl)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                real(F64), dimension(:), intent(inout)                      :: Energy
                real(F64), dimension(:, :, :, :), intent(in)                :: Taibj
                real(F64), dimension(NOcc, NOcc, NOcc, NOcc), intent(in)    :: Vijkl
                
                integer :: i, j, k, l, b
                real(F64) :: Ec2e, Ec2f
                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "-------------------- Ec2e, Ec2f calculation in progress ---------------------"
                
                
            !------------------------- CCD Ec2e, Ec2f main calculation -------------------------
            Ec2e = ZERO
            Ec2f = ZERO
            do l = 1, NOcc                                
                do k = 1, NOcc
                    do j = 1, NOcc
                        do i = 1, NOcc
                            do b = 1, NVirt
                            Ec2e = Ec2e + Vijkl(i, k, j, l) * DOT_PRODUCT(Taibj(:, b, j, l), Taibj(:, b, i, k))
                            Ec2f = Ec2f + Vijkl(i, k, j, l) * DOT_PRODUCT(Taibj(:, b, l, j), Taibj(:, b, i, k))
                            end do
                        end do
                    end do
                end do
            end do
            
            
            Ec2e = TWO * Ec2e
            Ec2f = -ONE * Ec2f
            Energy(RPA_ENERGY_CUMULANT_2e) = Ec2e
            Energy(RPA_ENERGY_CUMULANT_2f) = Ec2f
            
            call msg("Ec2e, Ec2f corrections computed in " // str(clock_readwall(timer),d=1) // " seconds")
    end  subroutine rpa_Ec2e_corection
    
    
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !----------------------------------------------          Calculation of Ec2k, Ec2k corrections          -----------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine rpa_Ec2k_corection(Energy, NOcc, NVirt, NGridTHC, NCholesky, Taibj, Dacij, ZYYkab)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NGridTHC
                integer, intent(in)                                         :: NCholesky
                real(F64), dimension(NVirt, NVirt, NOcc, NOcc), intent(in)  :: Taibj
                real(F64), dimension(NVirt**2, NOcc, NOcc), intent(in)      :: Dacij
                real(F64), dimension(NCholesky, NVirt, NVirt), intent(in)   :: ZYYkab
                real(F64), dimension(:), intent(inout)                      :: Energy
                
                real(F64), dimension(:), allocatable                        :: Vac_bd
                
                integer :: i, j, b, d, bd
                real(F64) :: Ec2k, Ec2k_d, Ec2l, Ec2l_d, V_T, V_T_T_ij, V_T_T_ji
                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "-------------------- Ec2k, Ec2l calculation in progress ---------------------"
                
                
            !------------------------- CCD Ec2k, Ec2l main calculation -------------------------
            
            allocate(Vac_bd(NVirt**2))
            
            Ec2k = ZERO
            Ec2k_d = ZERO
            Ec2l = ZERO
            Ec2l_d = ZERO
            do bd = 1, NVirt * (NVirt + 1)/2
            call pq2p_ge_q(bd, NVirt, b, d)
            call Vac_bd_sub(Vac_bd, NVirt, b, d, ZYYkab)
            !$omp parallel do collapse(2) &
            !$omp private(i, j, V_T, V_T_T_ij, V_T_T_ji) &
            !$omp reduction(+:Ec2k, Ec2k_d, Ec2l, Ec2l_d)
                do i = 1, NOcc
                    do j = 1, NOcc
                        call real_vw_x(V_T, Vac_bd, Dacij(:, i, j), NVirt**2)
                        V_T_T_ij = V_T * Taibj(b, d, i, j)
                        V_T_T_ji = V_T * Taibj(b, d, j, i)
                        Ec2k = Ec2k + V_T_T_ij
                        Ec2l = Ec2l + V_T_T_ji
                            if (b == d) then
                            Ec2k_d = Ec2k_d + V_T_T_ij
                            Ec2l_d = Ec2l_d + V_T_T_ji
                            endif
                    end do
                end do
                !$omp end parallel do
            end do
            
            
            Ec2k = TWO * (TWO * Ec2k - ONE * Ec2k_d)
            Ec2l = -ONE * (TWO * Ec2l - ONE * Ec2l_d)
            Energy(RPA_ENERGY_CUMULANT_2k) = Ec2k
            Energy(RPA_ENERGY_CUMULANT_2l) = Ec2l
            
            
            call msg("Ec2k, Ec2l corrections computed in " // str(clock_readwall(timer),d=1) // " seconds")
            
            contains
            subroutine Vac_bd_sub(Vac_bd, NVirt, b, d, ZYYkab)
                           
                real(F64), dimension(NVirt, NVirt), intent(out)             :: Vac_bd
                real(F64), dimension(NCholesky, NVirt, NVirt), intent(in)   :: ZYYkab
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: b, d 

            call real_aTb(Vac_bd, ZYYkab(:, :, b), ZYYkab(:, :, d))      
            end subroutine Vac_bd_sub


    end  subroutine rpa_Ec2k_corection
    
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !----------------------------------------------          Beyond RPA corrections main procedure          -----------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
      subroutine rpa_CCD_corrections_FullSet(Energy, Zgh, Zgk, Yga, Xgi, OccEnergies, VirtEnergies, &
            Uaim, Am, NOcc, NVirt, NVecsT2, NGridTHC, NCholesky)
            
            
                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NVecsT2
                integer, intent(in)                                         :: NGridTHC
                integer, intent(in)                                         :: NCholesky
                real(F64), dimension(:), intent(inout)                      :: Energy
                real(F64), dimension(:, :), intent(in)                      :: Zgh
                real(F64), dimension(:, :), intent(in)                      :: Zgk
                real(F64), dimension(NGridTHC, NVirt), intent(in)           :: Yga
                real(F64), dimension(NGridTHC, NOcc), intent(in)            :: Xgi
                real(F64), dimension(NOcc), intent(in)                      :: OccEnergies
                real(F64), dimension(NVirt), intent(in)                     :: VirtEnergies
                real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in)      :: Uaim
                real(F64), dimension(:), intent(in)                         :: Am
            

                real(F64), dimension(:, :, :, :), allocatable               :: Taibj
                real(F64), dimension(:, :, :), allocatable                  :: Dacij
                real(F64), dimension(:, :, :), allocatable                  :: ZXXkij 
                real(F64), dimension(:, :, :), allocatable                  :: ZYXkai   
                real(F64), dimension(:, :, :), allocatable                  :: ZYYkab 
                real(F64), dimension(:, :, :, :), allocatable               :: Vaibj
                real(F64), dimension(:, :, :, :), allocatable               :: Vijab
                real(F64), dimension(:, :, :, :), allocatable               :: Vijkl

            
            
                        print *, "NOcc      =", NOcc
                        print *, "NVirt     =", NVirt
                        print *, "NVecsT2   =", NVecsT2
                        print *, "NGridTHC  =", NGridTHC
                        print *, "NCholesky =", NCholesky

!                  !$omp parallel do private(h) default(shared)
!                  !$omp end parallel do

            ! T2 amplitudes calculation block
            allocate(Taibj(NVirt, NVirt, NOcc, NOcc))
            allocate(Dacij(NVirt**2, NOcc, NOcc))
            call amplitudes_aibj(Taibj, Dacij, NOcc, NVirt, NVecsT2, Uaim, Am)
            
            ! ERIs intermediates calculation block
            allocate(ZXXkij(NCholesky, NOcc, NOcc))
            allocate(ZYXkai(NCholesky, NVirt, NOcc))
            allocate(ZYYkab(NCholesky, NVirt, NVirt))
            call ERI_intermediates(ZXXkij, ZYXkai, ZYYkab, NOcc, NVirt, NGridTHC, NCholesky, Zgk, Yga, Xgi) 
            
            ! Vaibj based corrections calculation block
            allocate(Vaibj(NVirt, NVirt, NOcc, NOcc))
            call ERI_aibj(Vaibj, NOcc, NVirt, NCholesky, ZYXkai)
            deallocate(ZYXkai)
            call rpa_Ec2b_corection(Energy, NOcc, NVirt, Taibj, Vaibj)
            deallocate(Vaibj)
            
            ! Vijkl based corrections calculation block
            allocate(Vijkl(NOcc, NOcc, NOcc, NOcc))
            call ERI_ijkl(Vijkl, NOcc, NVirt, NCholesky, ZXXkij)
            call rpa_Ec2e_corection(Energy, NOcc, NVirt, Taibj, Vijkl)
            deallocate(Vijkl)
            
            ! Vijab based corrections calculation block
            allocate(Vijab(NVirt, NVirt, NOcc, NOcc))
            call ERI_ijab(Vijab, NOcc, NVirt, NCholesky, ZXXkij, ZYYkab)
            deallocate(ZXXkij)
            call rpa_Ec2g_corection(Energy, NOcc, NVirt, Taibj, Vijab)
            deallocate(Vijab)
            
            ! Vabcd based corrections calculation block
            call rpa_Ec2k_corection(Energy, NOcc, NVirt, NGridTHC, NCholesky, Taibj, Dacij, ZYYkab)    

            ! ----------- odkomentować 
            ! block
            !       type(TClock) :: timer
            !       integer :: NCholesky
                  
            !       call clock_start(timer)
            !       NCholesky = size(Zgk, dim=2)
            !       call ERI_ijab_ijkl_abcd_v2(Vabcd, NOcc, NVirt, NGridTHC, NCholesky, Zgk, Yga, Xgi)
            !       call msg("v2 integrals completed in " // str(clock_readwall(timer), d=1) // " seconds")
            ! end block

            Energy(RPA_ENERGY_CUMULANT_1B) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_1B)
            Energy(RPA_ENERGY_CUMULANT_2B) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_2B)
            Energy(RPA_ENERGY_CUMULANT_2C) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_2C)
      end subroutine rpa_CCD_corrections_FullSet
end module rpa_CCD_Corrections_Experimental
