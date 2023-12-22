module rpa_CCD_Corrections_Experimental
      use arithmetic      
      use real_linalg
      use rpa_definitions
      use clock
      
      implicit none

contains


    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !--------------------------------------------------          Calculation of Taibj amplitudes         --------------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine amplitudes_aibj(Taibj, NOcc, NVirt, NVecsT2, Uaim, Am)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NVecsT2
                real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in)      :: Uaim
                real(F64), dimension(:), intent(in)                         :: Am
                
                integer :: a, i, j, mu                
                real(F64), dimension(:, :, :), allocatable                  :: Uami
                real(F64), dimension(:, :, :), allocatable                  :: AUmai               
                real(F64), dimension(:, :, :, :), intent (out)              :: Taibj
                
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
            do j = 1, NOcc
                do i = 1, NOcc
                    Taibj(:, :, i, j) = matmul(Uami(:, :, i), AUmai(:, :, j)) 
                end do
            end do
            
            
            call msg("T2 amplitudes computed in " // str(clock_readwall(timer),d=1) // " seconds")
    end  subroutine amplitudes_aibj
    
    
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !--------------------------------------------------          Calculation of Vaibj integrals          --------------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine ERI_aibj(Vaibj, NOcc, NVirt, NGridTHC, Zgh, Yga, Xgi)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NGridTHC
                real(F64), dimension(:, :), intent(in)                      :: Zgh
                real(F64), dimension(NGridTHC, NVirt), intent(in)           :: Yga
                real(F64), dimension(NGridTHC, NOcc), intent(in)            :: Xgi
                
                integer :: a, i, j, g             
                real(F64), dimension(:, :), allocatable                     :: ZYXgai                      
                real(F64), dimension(:, :, :), allocatable                  :: YXgai      
                real(F64), dimension(:, :, :), allocatable                  :: YXagi       
                real(F64), dimension(:, :, :, :), intent(out)               :: Vaibj 

                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "------------------- ERIs (ai|bj) calculation in progress --------------------"
                
            !------------------------- ERIs type YX calculation intermediates -------------------------
            allocate(YXgai(NGridTHC, NVirt, NOcc))
            allocate(YXagi(NVirt, NGridTHC, NOcc))
            YXgai = ZERO
            YXagi = ZERO
            do i = 1, NOcc    
                do a = 1, NVirt
                    !$omp parallel do private(g) default(shared)
                    do g = 1, NGridTHC
                        YXgai(g, a, i) = Yga(g, a) * Xgi(g, i)
                    end do
                    !$omp end parallel do
                end do
                YXagi(:, :, i) = transpose(YXgai(:, :, i))    
            end do        
            
            !------------------------- ERIs type Vaibj(a, b, i, j) -----------------------
            allocate(ZYXgai(NGridTHC, NVirt))
            Vaibj = ZERO
            do j = 1, NOcc
                ZYXgai = ZERO
                do i = 1, NOcc
                    ZYXgai = matmul(Zgh, YXgai(:, :, j))
                    Vaibj(:, :, i, j) = matmul(YXagi(:, :, i), ZYXgai) 
                end do
            end do
            
            
            call msg("ERIs (ai|bj) computed in " // str(clock_readwall(timer),d=1) // " seconds")
    end  subroutine ERI_aibj
    
    
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !-------------------------------------------          Calculation of Vijab, Vijkl, Vabcd integrals          -------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine ERI_ijab_ijkl_abcd(Vijab, Vijkl, Vabcd, NOcc, NVirt, NGridTHC, Zgh, Yga, Xgi)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NGridTHC
                real(F64), dimension(:, :), intent(in)                      :: Zgh
                real(F64), dimension(NGridTHC, NVirt), intent(in)           :: Yga
                real(F64), dimension(NGridTHC, NOcc), intent(in)            :: Xgi
                
                integer :: i, j, a, b, g            
                real(F64), dimension(:, :), allocatable                     :: ZXXgij
                real(F64), dimension(:, :, :), allocatable                  :: ZYYgab                      
                real(F64), dimension(:, :, :), allocatable                  :: XXgij
                real(F64), dimension(:, :, :), allocatable                  :: XXigj
                real(F64), dimension(:, :, :), allocatable                  :: YYgab  
                real(F64), dimension(:, :, :), allocatable                  :: YYagb    
                real(F64), dimension(:, :, :, :), allocatable               :: Wijab
                real(F64), dimension(:, :, :, :), intent(out)               :: Vijab
                real(F64), dimension(:, :, :, :), intent(out)               :: Vijkl
                real(F64), dimension(:, :, :, :), intent(out)               :: Vabcd

                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "----------- ERIs (ij|ab) (ij|kl) (ab|cd) calculation in progress ------------"
                
            allocate(XXgij(NGridTHC, NOcc, NOcc))
            allocate(XXigj(NOcc, NGridTHC, NOcc))
            allocate(YYgab(NGridTHC, NVirt, NVirt))
            allocate(YYagb(NVirt, NGridTHC, NVirt))
            allocate(ZXXgij(NGridTHC, NOcc))
            allocate(ZYYgab(NGridTHC, NVirt, NVirt))
            allocate(Wijab(NOcc, NVirt, NVirt, NOcc))
            
            !------------------------- ERIs type XX calculation intermediates -------------------------
            XXgij = ZERO
            XXigj = ZERO
            do j = 1, NOcc                                
                do i = 1, NOcc
                    !$omp parallel do private(g) default(shared)
                    do g = 1, NGridTHC
                        XXgij(g, i, j) = Xgi(g, i) * Xgi(g, j)
                    end do
                    !$omp end parallel do
                end do
                XXigj(:, :, j) = transpose(XXgij(:, :, j))    
            end do        
            
            !------------------------- ERIs type YY calculation intermediates -------------------------
            YYgab = ZERO
            YYagb = ZERO
            do b = 1, NVirt                                
                do a = 1, NVirt
                    !$omp parallel do private(g) default(shared)
                    do g = 1, NGridTHC
                        YYgab(g, a, b) = Yga(g, a) * Yga(g, b)
                    end do
                    !$omp end parallel do
                end do
                YYagb(:, :, b) = transpose(YYgab(:, :, b))    
            end do
            
            !------------------------- ERIs type ZYY calculation intermediate -------------------------
            ZYYgab = ZERO
            !$omp parallel do private(b) default(shared)
            do b = 1, NVirt                                
                ZYYgab(:, :, b) = matmul(Zgh, YYgab(:, :, b))
            end do
            !$omp end parallel do
            
            !------------------------- ERIs type Vijkl(i, k, j, l) -----------------------
            Vijkl = ZERO
            do j = 1, NOcc
                ZXXgij = ZERO
                do i = 1, NOcc
                    ZXXgij = matmul(Zgh, XXgij(:, :, j))
                    Vijkl(:, :, i, j) = matmul(XXigj(:, :, i), ZXXgij) 
                end do
            end do
            
            !------------------------- ERIs type Vijab(a, b, i, j) -----------------------
            Wijab = ZERO
            Vijab = ZERO
            !$omp parallel do collapse(2) private(j, b) default(shared)
            do j = 1, NOcc
                do b = 1, NVirt
                    Wijab(:, :, b, j) = matmul(XXigj(:, :, j), ZYYgab(:, :, b)) 
                end do
            end do
            !$omp end parallel do
            
            !$omp parallel do collapse(2) private(i, a) default(shared)
            do i = 1, NOcc
                do a = 1, NVirt
                    Vijab(:, a, :, i) = transpose(Wijab(:, a, :, i))
                end do
            end do
            !$omp end parallel do
            
            !------------------------- ERIs type Vabcd(a, c, b, d) -----------------------
            Vabcd = ZERO
            !$omp parallel do collapse(2) private(a, b) default(shared)
            do a = 1, NVirt
                do b = 1, NVirt
                    Vabcd(:, :, a, b) = matmul(YYagb(:, :, a), ZYYgab(:, :, b)) 
                end do
            end do
            !$omp end parallel do
            
            
            call msg("ERIs (ij|ab) (ij|kl) (ab|cd) computed in " // str(clock_readwall(timer),d=1) // " seconds")
      end  subroutine ERI_ijab_ijkl_abcd


      subroutine ERI_ijab_ijkl_abcd_v2(Vabcd, NOcc, NVirt, NGridTHC, NCholesky, Zgk, Yga, Xgi)

            real(F64), dimension(:, :, :, :), intent(out)                   :: Vabcd
                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NGridTHC
                integer, intent(in)                                         :: NCholesky
                real(F64), dimension(:, :), intent(in)                      :: Zgk
                real(F64), dimension(NGridTHC, NVirt), intent(in)           :: Yga
                real(F64), dimension(NGridTHC, NOcc), intent(in)            :: Xgi                

                real(F64), dimension(:), allocatable :: YYg
                real(F64), dimension(:, :, :), allocatable :: ZYYkab
                real(F64), dimension(:), allocatable :: VabcdBlock
                integer :: a, b
                integer :: ab0, ab1, cd0, cd1, Nab, Ncd
                integer :: k, l, NBlocks
                integer, parameter :: BlockDim = 100

                allocate(ZYYkab(NCholesky, NVirt, NVirt))
                !$omp parallel private(YYg) &
                !$omp private(a, b)
                allocate(YYg(NGridTHC)) ! private YYk array for each thread
                !$omp do collapse(2)
                do b = 1, NVirt                                
                      do a = 1, NVirt
                            YYg(:) = Yga(:, a) * Yga(:, b) ! vector operation
                            call real_ATv(ZYYkab(:, a, b), Zgk, YYg) ! matrix-vector multiplication w = A**T*v
                      end do
                end do
                !$omp end do
                !$omp end parallel

                allocate(VabcdBlock(BlockDim**2)) ! ------------ blok zaalokowany jako jednowymiarowy pasek ----------
                NBlocks = NVirt**2 / BlockDim
                if (modulo(NVirt**2, BlockDim) > 0) NBlocks = NBlocks + 1 ! ------ blok na krawędzi może mieć wymiar < BlockDim
                do l = 1, NBlocks ! ----- pętla po blokach macierzy V; granice bloku to (ab0:ab1, cd0:cd1), wymiar to Nab x Ncd -----
                      do k = 1, NBlocks
                            cd0 = 1 + BlockDim * (l - 1)
                            cd1 = min(NVirt**2, BlockDim * l)
                            ab0 = 1 + BlockDim * (k - 1) 
                            ab1 = min(NVirt**2, BlockDim * k)
                            Nab = ab1 - ab0 + 1
                            Ncd = cd1 - cd0 + 1
                            call ZYY_ZYY_Block(Vabcd, VabcdBlock(1:Nab*Ncd), ZYYkab, &
                                  ab0, ab1, cd0, cd1, NVirt)
                      end do
                end do
                
          contains

                subroutine ZYY_ZYY_Block(Vacbd, VabcdBlock, ZYYkab, ab0, ab1, cd0, cd1, NVirt)
                      integer, intent(in)                                    :: ab0, ab1, cd0, cd1
                      real(F64), dimension(:, :, :, :), intent(inout)       :: Vacbd
                      real(F64), dimension(ab0:ab1, cd0:cd1), intent(out)    :: VabcdBlock
                      real(F64), dimension(NCholesky, NVirt**2), intent(in)  :: ZYYkab
                      integer, intent(in)                                    :: NVirt
                      
                      integer :: ab, cd, a, b, c, d
                      
                      call real_aTb(VabcdBlock, ZYYkab(:, ab0:ab1), ZYYkab(:, cd0:cd1)) ! sklejone indeksy ab, cd
                      do cd = cd0, cd1
                            do ab = ab0, ab1
                                  !
                                  ! ab = a + NVirt * (b - 1)
                                  ! cd = c + NVirt * (d - 1)
                                  !
                                  b = (ab - 1) / NVirt + 1
                                  a = ab - NVirt * (b - 1)
                                  d = (cd - 1) / NVirt + 1
                                  c = cd - NVirt * (d - 1)
                                  Vacbd(a, c, b, d) = VabcdBlock(ab, cd)
                            end do
                      end do
                end subroutine ZYY_ZYY_BLOCK
      end subroutine ERI_ijab_ijkl_abcd_v2

            
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !----------------------------------------          Calculation of Ec1b, Ec2b, Ec2c, Ec2d corrections          -----------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine rpa_Ec2b_corection(Energy, NOcc, NVirt, Taibj, Vaibj)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                real(F64), dimension(:), intent(inout)                      :: Energy
                real(F64), dimension(:, :, :, :), intent(in)                :: Taibj
                real(F64), dimension(:, :, :, :), intent(in)                :: Vaibj
                
                integer :: i, j, k, a, c
                real(F64) :: Ec1b, Ec2b, Ec2c, Ec2d
                real(F64), dimension(:, :), allocatable                     :: VTac
                
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
                real(F64), dimension(:, :, :, :), intent(in)                :: Vijab
                
                integer :: i, j, k, c
                real(F64) :: Ec2g, Ec2h, Ec2i, Ec2j
                real(F64), dimension(:, :), allocatable                     :: VTbc
                
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
                real(F64), dimension(:, :, :, :), intent(in)                :: Vijkl
                
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
    !----------------------------------------------          Calculation of Ec2k, Ec2l corrections          -----------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    subroutine rpa_Ec2k_corection(Energy, NOcc, NVirt, Taibj, Vabcd)


                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                real(F64), dimension(:), intent(inout)                      :: Energy
                real(F64), dimension(:, :, :, :), intent(in)                :: Taibj
                real(F64), dimension(:, :, :, :), intent(in)                :: Vabcd
                
                integer :: i, j, b, c, d
                real(F64) :: VT, Ec2k, Ec2l
                
                type(TClock) :: timer
                call clock_start(timer)
                
                print *, "-------------------- Ec2k, Ec2l calculation in progress ---------------------"
                
            !------------------------- CCD Ec2k, Ec2l main calculation -------------------------
            Ec2k = ZERO
            Ec2l = ZERO
            do i = 1, NOcc
                do j = 1, NOcc
                    do d = 1, NVirt
                        do b = 1, NVirt
                        VT = ZERO
                            do c = 1, NVirt
                            VT = VT + DOT_PRODUCT(Vabcd(:, c, b, d), Taibj(:, c, i, j))
                            end do
                            Ec2k = Ec2k + Taibj(b, d, i, j) * VT
                            Ec2l = Ec2l + Taibj(b, d, j, i) * VT
                        end do
                    end do
                end do
            end do
            
!            !$omp parallel do collapse(2) private(d, b) default(shared)
!            !$omp end parallel do
            
            Ec2k = TWO * Ec2k
            Ec2l = -ONE * Ec2l
            Energy(RPA_ENERGY_CUMULANT_2k) = Ec2k
            Energy(RPA_ENERGY_CUMULANT_2l) = Ec2l
            
            call msg("Ec2k, Ec2l corrections computed in " // str(clock_readwall(timer),d=1) // " seconds")
    end  subroutine rpa_Ec2k_corection
    
    
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
    !----------------------------------------------          Beyond RPA corrections main procedure          -----------------------------------------------!
    !------------------------------------------------------------------------------------------------------------------------------------------------------!
      subroutine rpa_CCD_corrections_FullSet(Energy, Zgh, Zgk, Yga, Xgi, OccEnergies, VirtEnergies, &
            Uaim, Am, NOcc, NVirt, NVecsT2, NGridTHC)
            
            
                integer, intent(in)                                         :: NOcc
                integer, intent(in)                                         :: NVirt
                integer, intent(in)                                         :: NVecsT2
                integer, intent(in)                                         :: NGridTHC
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
                real(F64), dimension(:, :, :, :), allocatable               :: Vaibj
                real(F64), dimension(:, :, :, :), allocatable               :: Vijab
                real(F64), dimension(:, :, :, :), allocatable               :: Vijkl
                real(F64), dimension(:, :, :, :), allocatable               :: Vabcd
            
            
            allocate(Taibj(NVirt, NVirt, NOcc, NOcc))
            allocate(Vaibj(NVirt, NVirt, NOcc, NOcc))
            
            
!                  !$omp parallel do private(h) default(shared)
!                  !$omp end parallel do

            call amplitudes_aibj(Taibj, NOcc, NVirt, NVecsT2, Uaim, Am)
            call ERI_aibj(Vaibj, NOcc, NVirt, NGridTHC, Zgh, Yga, Xgi)
            call rpa_Ec2b_corection(Energy, NOcc, NVirt, Taibj, Vaibj)
            deallocate(Vaibj)
            allocate(Vijab(NVirt, NVirt, NOcc, NOcc))
            allocate(Vijkl(NOcc, NOcc, NOcc, NOcc))
            allocate(Vabcd(NVirt, NVirt, NVirt, NVirt))
            call ERI_ijab_ijkl_abcd(Vijab, Vijkl, Vabcd, NOcc, NVirt, NGridTHC, Zgh, Yga, Xgi)

            ! ----------- odkomentować 
            ! block
            !       type(TClock) :: timer
            !       integer :: NCholesky
                  
            !       call clock_start(timer)
            !       NCholesky = size(Zgk, dim=2)
            !       call ERI_ijab_ijkl_abcd_v2(Vabcd, NOcc, NVirt, NGridTHC, NCholesky, Zgk, Yga, Xgi)
            !       call msg("v2 integrals completed in " // str(clock_readwall(timer), d=1) // " seconds")
            ! end block

            
            call rpa_Ec2g_corection(Energy, NOcc, NVirt, Taibj, Vijab)
            call rpa_Ec2e_corection(Energy, NOcc, NVirt, Taibj, Vijkl)
            call rpa_Ec2k_corection(Energy, NOcc, NVirt, Taibj, Vabcd)

            Energy(RPA_ENERGY_CUMULANT_1B) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_1B)
            Energy(RPA_ENERGY_CUMULANT_2B) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_2B)
            Energy(RPA_ENERGY_CUMULANT_2C) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_2C)
      end subroutine rpa_CCD_corrections_FullSet
     
     
end module rpa_CCD_Corrections_Experimental
