module MolproInterface
      use arithmetic
      use basis_sets
      use Auto2e
      use display
      use string
      use OneElectronInts
      use sys_definitions
      use linalg
      use spherh
      
      implicit none

contains

      subroutine molpro_AngFuncTransf(X_out, X_in, FromMolproAO, TwoIndexTransf, AOBasis)
            real(F64), dimension(:, :), intent(out) :: X_out
            real(F64), dimension(:, :), intent(in)  :: X_in
            logical, intent(in)                     :: FromMolproAO            
            logical, intent(in)                     :: TwoIndexTransf
            type(TAOBasis), intent(in)              :: AOBasis

            integer, dimension(:), allocatable :: Map
            integer, dimension(:), allocatable :: Map_ao2mao
            integer :: p, n
            integer :: NAO

            NAO = size(X_in, dim=1)
            allocate(Map(NAO))
            allocate(Map_ao2mao(NAO))
            call molpro_AOMap(Map, Map_ao2mao, AOBasis)
            if (.not. FromMolproAO) then
                  Map = Map_ao2mao
            end if            
            if (TwoIndexTransf) then
                  !
                  ! Examples: overlap matrix, one-electron matrices of the Fock operator, ...
                  !
                  call molpro_TransfMatrix(X_out, X_in, Map)
            else
                  !
                  ! Examples: MO coefficients stored in columns, e.g., C(p,i), where p is AO and i is MO
                  !
                  n = size(X_in, dim=2)
                  do p = 1, n
                        call molpro_TransfVector(X_out(:, p), X_in(:, p), Map)
                  end do
            end if
      end subroutine molpro_AngFuncTransf
      

      subroutine molpro_TransfMatrix(X_out, X_in, Map)
            real(F64), dimension(:, :), intent(out) :: X_out
            real(F64), dimension(:, :), intent(in)  :: X_in
            integer, dimension(:), intent(in)       :: Map
            
            integer :: q
            integer :: NAO
            real(F64), dimension(:, :), allocatable :: W
            
            NAO = size(Map)
            allocate(W(NAO, NAO))
            do q = 1, NAO
                  call molpro_TransfVector(W(:, q), X_in(:, q), Map)
            end do
            do q = 1, NAO
                  X_out(:, Map(q)) = W(:, q)
            end do
      end subroutine molpro_TransfMatrix


      subroutine molpro_TransfVector(X_out, X_in, Map)
            real(F64), dimension(:), intent(out) :: X_out
            real(F64), dimension(:), intent(in)  :: X_in
            integer, dimension(:), intent(in)    :: Map
            
            integer :: p
            integer :: NAO
            
            NAO = size(Map)
            do p = 1, NAO
                  X_out(Map(p)) = X_in(p)
            end do
      end subroutine molpro_TransfVector


      subroutine molpro_TransfMOCoeffs(X_out, X_in, Map)
            real(F64), dimension(:, :), intent(out) :: X_out
            real(F64), dimension(:, :), intent(in)  :: X_in
            integer, dimension(:), intent(in)       :: Map

            integer :: m, k

            m = size(X_in, dim=1)
            do k = 1, m
                  call molpro_TransfVector(X_out(:, k), X_in(:, k), Map)
            end do
      end subroutine molpro_TransfMOCoeffs
            
      
      subroutine molpro_AOMap(Map_mao2ao, Map_ao2mao, AOBasis)
            integer, dimension(:), intent(out) :: Map_mao2ao
            integer, dimension(:), intent(out) :: Map_ao2mao
            type(TAOBasis), intent(in)         :: AOBasis

            integer :: k
            !
            ! Ordering of real solid harmonics in Molpro
            !
            integer, parameter :: MaxL = AUTO2E_MAXL
            integer, parameter :: MaxAngFuncSpher = 2 * MaxL + 1
            integer, parameter :: MaxAngFuncCart = ((MaxL+1)*(MaxL+2))/2
            !                                                             s
            integer, dimension(MaxAngFuncSpher), parameter :: MOLPRO_S = [0, (-1, k=1,10)] + 1
            !
            ! Note the exception: p orbitals are not transformed to the spherical basis by the subroutines
            ! in the Auto2e module, see the SPHER_TRANSF_LMIN parameter. The Cartesian ordering, (px, py, pz),
            ! is the same as in Molpro. (Otherwise, the pz orbital corresponding to m=0 would be stored in
            ! the second element of the array instead of the third.)
            !
            !                                                             px py  pz
            integer, dimension(MaxAngFuncSpher), parameter :: MOLPRO_P = [1, 2, 3, (0,k=1,8)] 
            !                                                             d0  d2- d1+ d2+ d1-
            integer, dimension(MaxAngFuncSpher), parameter :: MOLPRO_D = [0,  -2,  1, 2, -1, (-3,k=1,6)] + 3
            !                                                             f1+  f1-  f0  f3+  f2-  f3- f2+
            integer, dimension(MaxAngFuncSpher), parameter :: MOLPRO_F = [1,   -1,   0,  3,  -2,  -3,  2, (-4,k=1,4)] + 4
            !                                                             g0  g2-  g1+  g4+  g1-  g2+  g4-  g3+  g3-
            integer, dimension(MaxAngFuncSpher), parameter :: MOLPRO_G = [0,  -2,  1,   4,   -1,  2,   -4,  3,   -3, (-5,k=1,2)] + 5
            !                                                             h1+  h1-  h2+  h3+  h4-  h3-  h4+  h5-  h0  h5+  h2-
            integer, dimension(MaxAngFuncSpher), parameter :: MOLPRO_H = [1,   -1,  2,   3,   -4,  -3,   4,  -5,   0,  5,   -2] + 6

            integer, dimension(MaxAngFuncSpher, 0:MaxL), parameter :: AngFuncMap &
                  = reshape([MOLPRO_S, MOLPRO_P, MOLPRO_D,  MOLPRO_F, MOLPRO_G, MOLPRO_H], [MaxAngFuncSpher,MaxL+1])

            integer, dimension(MaxAngFuncCart), parameter :: MOLPRO_CART_S = [1,(0,k=1,MaxAngFuncCart-1)]
            !                                                                  x  y  z
            integer, dimension(MaxAngFuncCart), parameter :: MOLPRO_CART_P = [1, 2, 3, (0,k=1,MaxAngFuncCart-3)]
            !
            ! Auto2e ordering
            ! 1  xx
            ! 2  xy
            ! 3  xz
            ! 4  yy
            ! 5  yz
            ! 6  zz
            !                                                                  xx yy zz xy xz yz
            integer, dimension(MaxAngFuncCart), parameter :: MOLPRO_CART_D = [1, 4, 6, 2, 3, 5, (0,k=1,MaxAngFuncCart-6)]
            !
            ! Auto2e ordering
            !
            ! 1  xxx
            ! 2  xxy
            ! 3  xxz
            ! 4  xyy
            ! 5  xyz
            ! 6  xzz
            ! 7  yyy
            ! 8  yyz
            ! 9  yzz
            ! 10 zzz
            !
            integer, dimension(MaxAngFuncCart), parameter :: MOLPRO_CART_F = &
                  ! xxx yyy zzz xxy xxz xyy yyz xzz yzz xyz
                  [ 1,  7,  10, 2,  3,  4,  8,  6,  9,  5, (0,k=1,MaxAngFuncCart-10)]
            !
            ! Auto2e ordering
            !
            ! 1  xxxx
            ! 2  xxxy
            ! 3  xxxz
            ! 4  xxyy
            ! 5  xxyz
            ! 6  xxzz
            ! 7  xyyy
            ! 8  xyyz
            ! 9  xyzz
            ! 10 xzzz
            ! 11 yyyy
            ! 12 yyyz
            ! 13 yyzz
            ! 14 yzzz
            ! 15 zzzz
            !
            integer, dimension(MaxAngFuncCart), parameter :: MOLPRO_CART_G = &
                  ! xxxx yyyy zzzz xxxy xxxz xyyy yyyz xzzz yzzz xxyy xxzz yyzz xxyz xyyz xyzz
                  [ 1,   11,  15,  2,   3,   7,   12,  10,  14,  4,   6,   13,  5,   8,   9, (0,k=1,MaxAngFuncCart-15)]
            !
            ! And finally, the ordring of h functions in Molpro is systematic and the same as in Auto2e.
            !
            integer, dimension(MaxAngFuncCart), parameter :: MOLPRO_CART_H = [(k,k=1,MaxAngFuncCart)]

            integer, dimension(MaxAngFuncCart, 0:MaxL), parameter :: CartAngFuncMap &
                  = reshape([MOLPRO_CART_S, MOLPRO_CART_P, MOLPRO_CART_D,  MOLPRO_CART_F, MOLPRO_CART_G, MOLPRO_CART_H], [MaxAngFuncCart,MaxL+1])
           
            integer :: kk, L, n
            integer :: p0, p1, p
            integer :: NAO

            if (AOBasis%SpherAO) then
                  NAO = AOBasis%NAOSpher
            else
                  NAO = AOBasis%NAOCart
            end if
            Map_mao2ao = -1
            if (AOBasis%SpherAO) then
                  associate ( &
                        ShellLoc => AOBasis%ShellLocSpher, &
                        ShellParamsIdx => AOBasis%ShellParamsIdx, &
                        ShellMomentum => AOBasis%ShellMomentum, &
                        NAngFunc => AOBasis%NAngFuncSpher, &
                        NShells => AOBasis%NShells, &
                        NAO => AOBasis%NAOSpher &
                        )
                        do kk = 1, NShells
                              k = ShellParamsIdx(kk)
                              L = ShellMomentum(k)
                              n = NAngFunc(k)
                              p0 = ShellLoc(kk)
                              p1 = ShellLoc(kk) + n - 1
                              do p = p0, p1
                                    Map_mao2ao(p) = p0 + AngFuncMap(p-p0+1, L) - 1
                              end do
                        end do
                  end associate
            else
                  associate ( &
                        ShellLoc => AOBasis%ShellLocCart, &
                        ShellParamsIdx => AOBasis%ShellParamsIdx, &
                        ShellMomentum => AOBasis%ShellMomentum, &
                        NAngFunc => AOBasis%NAngFuncCart, &
                        NShells => AOBasis%NShells, &
                        NAO => AOBasis%NAOCart &
                        )
                        do kk = 1, NShells
                              k = ShellParamsIdx(kk)
                              L = ShellMomentum(k)
                              n = NAngFunc(k)
                              p0 = ShellLoc(kk)
                              p1 = ShellLoc(kk) + n - 1
                              do p = p0, p1
                                    Map_mao2ao(p) = p0 + CartAngFuncMap(p-p0+1, L) - 1
                              end do
                        end do
                  end associate
            end if
            do p = 1, NAO
                  Map_ao2mao(Map_mao2ao(p)) = p
            end do            
      end subroutine molpro_AOMap


      subroutine molpro_Test(AOBasis, Rho_cao)
            type(TAOBasis), intent(in)             :: AOBasis
            real(F64), dimension(:, :), intent(in) :: Rho_cao

            real(F64), dimension(:, :), allocatable :: S_cao, S_sao, S_mao
            real(F64), dimension(:, :), allocatable :: Rho_sao, Rho_mao
            real(F64), dimension(:), allocatable :: TransfWork
            integer :: NAOSpher, NAOCart, NAO
            logical, parameter :: SpherAO = .true.

            NAOSpher = AOBasis%NAOSpher
            NAOCart = AOBasis%NAOCart
            NAO = NAOSpher
            allocate(S_cao(NAOCart, NAOCart))
            allocate(S_sao(NAO, NAO))
            allocate(S_mao(NAO, NAO))
            allocate(Rho_sao(NAO, NAO))
            allocate(Rho_mao(NAO, NAO))
            call ints1e_OverlapMatrix(S_cao, AOBasis)
            call smfill(S_cao)
            allocate(TransfWork(NAOSpher*NAOCart))
            call SpherGTO_TransformMatrix_U(S_sao, S_cao, &
                  AOBasis%LmaxGTO, &
                  AOBasis%NormFactorsSpher, &
                  AOBasis%NormFactorsCart, &
                  AOBasis%ShellLocSpher, &
                  AOBasis%ShellLocCart, &
                  AOBasis%ShellMomentum, &
                  AOBasis%ShellParamsIdx, &
                  AOBasis%NAOSpher, &
                  AOBasis%NAOCart, &
                  AOBasis%NShells, TransfWork)
            call SpherGTO_TransformMatrix(Rho_sao, Rho_cao, &
                  AOBasis%LmaxGTO, &
                  AOBasis%NormFactorsSpher, &
                  AOBasis%NormFactorsCart, &
                  AOBasis%ShellLocSpher, &
                  AOBasis%ShellLocCart, &
                  AOBasis%ShellMomentum, &
                  AOBasis%ShellParamsIdx, &
                  AOBasis%NAOSpher, &
                  AOBasis%NAOCart, &
                  AOBasis%NShells, TransfWork)

            call molpro_AngFuncTransf(S_mao, S_sao, .false., .true., AOBasis)
            call molpro_AngFuncTransf(Rho_mao, Rho_sao, .false., .true., AOBasis)
            
            call msg("Testing Molpro interface", underline=.true.)
            call blankline()
            call msg("Spherical AO basis loaded from " // AOBasis%FilePath)
            call msg("Overlap matrix in spherical AO basis with Molpro ordering of shells and angular functions")
            call geprn(S_mao)
            call blankline()
            call msg("Density matrix in spherical AO basis with Molpro ordering of shells and angular functions")
            call geprn(Rho_mao)
            call msg("Koniec drukowania")
            stop
      end subroutine molpro_Test
end module MolproInterface
