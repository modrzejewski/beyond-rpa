module symmetry

      use gparam
      use parser
      use string
      use math_constants
      use gridfunc
      use orbextension
      use sort
      use arithmetic
      use display
      use linalg
      use blas1
      use blas2
      use blas3
      use blas4
      use blas5
      use blas6
      use blas_olenkifer


      implicit none
      real(F64), parameter :: MOVALEPS = 1.d-5
      real(F64), parameter :: AOVALEPS = 1.d-9
      real(F64), parameter :: COORDEPS = 1.d-5
      real(F64), parameter :: NORMEPS  = 1.d-5

      integer, parameter :: SYM_E = 1
      integer, parameter :: SYM_C2z = 2
      integer, parameter :: SYM_C2y = 3
      integer, parameter :: SYM_C2x = 4
      integer, parameter :: SYM_I = 5
      integer, parameter :: SYM_Oxy = 6
      integer, parameter :: SYM_Oxz = 7
      integer, parameter :: SYM_Oyz = 8
      integer, parameter :: maxop = 8

      integer, dimension(maxop) :: charac
   
      real(F64), parameter :: LINDEPCOND = 1.d-15      
      real(F64), parameter :: EPS = 1.d-9
      
      integer, dimension(:), allocatable :: morep

      integer, parameter, private :: z0 = 0, z1 = 1, z2 = 2, z3 = 3, z4 = 4, z5 = 5, z6 = 6, z7 = 7, z8 =8, z9 = 9, z10 = 10

contains

      subroutine symmetry_free()
!            deallocate(morep)
      end subroutine symmetry_free

      subroutine check_cc_symmetry_irrep(iexci, cc_iexci)
            integer, dimension(:, :), intent(out) :: iexci
            character(len=DEFLEN), intent(in) :: cc_iexci

            integer :: indx
            character(:), allocatable :: key, rest1, allstates       
            character(:), allocatable :: rep, n_states, s7, s8, state
            integer :: idx_start, idx_stop
            logical :: finisher



            call split(cc_iexci, key, allstates)

            finisher = .false.


            do while (finisher .eqv. .false.)
                  call split(allstates, state, rest1, ",")


                  call split(state, rep, n_states)
                  rep = uppercase(rep)

                  indx = index(n_states, "-")
                  if (indx .ne. 0) then
                        if (indx .eq. 1) then
                              idx_start = 1
                              read(n_states, *) idx_stop
                              if(idx_stop .lt. 0 ) idx_stop = -idx_stop
                        else
                              call split(n_states, s7, s8, '-')
                              read(s7, *) idx_start
                              read(s8, *) idx_stop
                        end if
                  else
                        read(n_states, *) idx_start
                        idx_stop = idx_start
                  end if
                  
                  call fill_exci(iexci, rep, idx_start, idx_stop)


                  allstates = rest1

                  indx = index(n_states, ",")
                  if (indx .eq. 0) then
                        if (rest1 .eq. "")then
                              finisher = .true.
                        end if
                  end if
            end do


      end subroutine check_cc_symmetry_irrep
      

      subroutine fill_exci(iexci, rep, idx_start, idx_stop)

            integer, dimension(:, :), intent(inout) :: iexci
            character(*), intent(in) :: rep
            integer, intent(in) :: idx_start, idx_stop
            
            print*, 'rep', rep
            if (POINT_GROUP .eq. D2h)then
                  select case (rep)
                        
                  case ('AG')
                        iexci(1, 1) = idx_start
                        iexci(2, 1) = idx_stop
                  case('B1G')
                        iexci(1, 2) = idx_start
                        iexci(2, 2) = idx_stop
                  case ('B2G')
                        iexci(1, 3) = idx_start
                        iexci(2, 3) = idx_stop
                  case ('B3G')
                        iexci(1, 4) = idx_start
                        iexci(2, 4) = idx_stop
                  case ('AU')
                        iexci(1, 5) = idx_start
                        iexci(2, 5) = idx_stop
                  case ('B1U')
                        iexci(1, 6) = idx_start
                        iexci(2, 6) = idx_stop
                  case ('B2U')
                        iexci(1, 7) = idx_start
                        iexci(2, 7) = idx_stop
                  case ('B3U')
                        iexci(1, 8) = idx_start
                        iexci(2, 8) = idx_stop
                  end select

            else if (POINT_GROUP .eq. C2v)then

                  select case (rep)

                  case ('A1')
                        iexci(1, 1) = idx_start
                        iexci(2, 1) = idx_stop
                  case('A2')
                        iexci(1, 2) = idx_start
                        iexci(2, 2) = idx_stop
                  case ('B1')
                        iexci(1, 3) = idx_start
                        iexci(2, 3) = idx_stop
                  case ('B2')
                        iexci(1, 4) = idx_start
                        iexci(2, 4) = idx_stop
                  end select
            else
                  call msg('SYMMETRY NOT YET IMPLEMENTED', MSG_ERROR)
                  stop
            end if
                  
      end  subroutine fill_exci

      subroutine D2htransform(r, rtrans)
            real(F64), dimension(:), intent(in) :: r
            real(F64), dimension(:, :), intent(out) :: rtrans

            call rotate('Z', 2, r, rtrans(:, 2))
            call rotate('Y', 2, r, rtrans(:, 3))
            call rotate('X', 2, r, rtrans(:, 4))
            call invert(r, rtrans(:, 5))
            call reflect('XY', r, rtrans(:, 6))
            call reflect('XZ', r, rtrans(:, 7))
            call reflect('YZ', r, rtrans(:, 8))
            
      end subroutine D2htransform

      subroutine C2vtransform(r, rtrans)
            real(F64), dimension(:), intent(in) :: r
            real(F64), dimension(:, :), intent(out) :: rtrans

            call rotate('Z', 2, r, rtrans(:, 2))
            call reflect('XZ', r, rtrans(:, 3))
            call reflect('YZ', r, rtrans(:, 4))

      end subroutine C2vtransform

      subroutine C2vrepr(charac, repr)
            !
            ! Returns the representation of the group
            ! repr, according to the characters of the
            ! representation given in charac.
            !
            !       |  E   SYM_C2z  Oxz  Oyz
            !       +--------------------
            !   A1  |   1    1    1    1
            !   A2  |   1    1   -1   -1
            !   B1  |   1   -1    1   -1
            !   B2  |   1   -1   -1    1

            integer, dimension(:), intent(in) :: charac
            integer, intent(out) :: repr

            repr = 0
            if (repr_diff(charac, C2v_order, C2v_A1))  repr = REP_A1
            if (repr_diff(charac, C2v_order, C2v_A2))  repr = REP_A2
            if (repr_diff(charac, C2v_order, C2v_B1))  repr = REP_B1
            if (repr_diff(charac, C2v_order, C2v_B2))  repr = REP_B2

            if (charac(symmpos(SYM_E, 1)) .ne. 1)then
                  repr = 0
            end if

            if (repr == 0) then
                  call msg("SYMMETRY ERROR: NON EXISTING CHARACTER IN SYM_C2v GROUP", MSG_ERROR)
                  stop
            end if

      end subroutine C2vrepr

      subroutine D2hrepr(charac, repr)
            
             ! |     |  E   C2z   C2y  C2x   i   Oxy  Oxz  Oyz
             ! +-----+----------------------------------------
             ! | Ag  |   1    1    1    1    1    1    1    1
             ! | B1g |   1    1   -1   -1    1    1   -1   -1
             ! | B2g |   1   -1    1   -1    1   -1    1   -1
             ! | B3g |   1   -1   -1    1    1   -1   -1    1
             ! | Au  |   1    1    1    1   -1   -1   -1   -1
             ! | B1u |   1    1   -1   -1   -1   -1    1    1
             ! | B2u |   1   -1    1   -1   -1    1   -1    1
             ! | B3u |   1   -1   -1    1   -1    1    1   -1

            integer, dimension(:), intent(in) :: charac
            integer, intent(out) :: repr

            repr = 0
            if (repr_diff(charac, D2h_order, D2h_Ag))   repr = REP_Ag
            if (repr_diff(charac, D2h_order, D2h_B1g))  repr = REP_B1g
            if (repr_diff(charac, D2h_order, D2h_B2g))  repr = REP_B2g
            if (repr_diff(charac, D2h_order, D2h_B3g))  repr = REP_B3g
            if (repr_diff(charac, D2h_order, D2h_Au))   repr = REP_Au
            if (repr_diff(charac, D2h_order, D2h_B1u))  repr = REP_B1u
            if (repr_diff(charac, D2h_order, D2h_B2u))  repr = REP_B2u
            if (repr_diff(charac, D2h_order, D2h_B3u))  repr = REP_B3u
            
            if (repr == 0) then
                  call msg("SYMMETRY ERROR: NON EXISTING CHARACTER IN D2h GROUP", MSG_ERROR)
                  stop
            end if

      end subroutine D2hrepr

      function repr_diff(charac, order, repref)
            logical :: repr_diff
            integer, dimension(:), intent(in) :: charac
            integer, intent(in) :: order
            integer, dimension(:), intent(in) :: repref
            integer :: aux

            integer :: i

            aux = 0
            repr_diff = .false.
            do i = 1, order
                  aux = aux + (charac(i) - repref(i))**2
            end do

            if (aux == 0) repr_diff = .true.

      end function repr_diff

      subroutine rotate(axis, k, r, rtrans)
            !
            ! Rotates point r(x, y, z) around
            ! k-fold C axis. The result is stored 
            ! in rtrans(x, y, z)
            ! ------------------------------------
            ! axis   - character, intent(in) X, Y, or Z
            ! k      - integer, intent(in), defines the
            !          rotational angle theta = 2pi/k
            ! r      - coordinates of the point
            ! rtrans - transformed coordinates of the point
            !          r
            !
            character(*), intent(in) :: axis
            integer, intent(in) :: k
            real(F64), dimension(:), intent(in) :: r
            real(F64), dimension(:), intent(out) :: rtrans

            real(F64) :: theta

            theta = TWO * Pi / real(k, F64)

            rtrans = r
            
            if (uppercase(axis) == 'Z') then
                  rtrans(1) = r(1) * cos(theta) + r(2) * sin(theta)
                  rtrans(2) = r(1) * sin(theta) + r(2) * cos(theta)
            else if (uppercase(axis) == 'X') then
                  rtrans(2) = r(2) * cos(theta) + r(3) * sin(theta)
                  rtrans(3) = r(2) * sin(theta) + r(3) * cos(theta)
            else if (uppercase(axis) == 'Y')then
                  rtrans(1) = r(1) * cos(theta) + r(3) * sin(theta)
                  rtrans(3) = r(1) * sin(theta) + r(3) * cos(theta)
            else
                  call msg("SYMMETRY ERROR: DIRECTION OF AXIS NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

      end subroutine rotate

      subroutine reflect(plane, r, rtrans)
            !
            ! Reflects the point r(x, y, z)
            ! in the plane, and stores the
            ! output in the rtrans.
            !
            character(*), intent(in) :: plane
            real(F64), dimension(:), intent(in) :: r
            real(F64), dimension(:), intent(out) :: rtrans

            rtrans = r

            if (uppercase(plane) == 'XY')then
                  rtrans(3) = -r(3)
            else if (uppercase(plane) == 'XZ') then
                  rtrans(2) = -r(2)
            else if (uppercase(plane) == 'YZ') then
                  rtrans(1) = -r(1)
            else
                  call msg("SYMMETRY ERROR: SYMMETRY PLANE NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if
            
      end subroutine reflect
      
      subroutine invert(r, rtrans)
            !
            ! Inverts the point r(x, y, z) and
            ! stores the output in rtrans.
            !
            real(F64), dimension(:), intent(in) :: r
            real(F64), dimension(:), intent(out) :: rtrans

            rtrans = -r
            
      end subroutine invert


      subroutine degeneracy_scan(eexcit, n, guess_deg, guess_deg_idx, guess_noflev, eps)
            
            real(F64), dimension(:), intent(in) :: eexcit
            integer, intent(in) :: n
            integer, dimension(:), intent(out) :: guess_deg
            integer, dimension(:), intent(out) :: guess_deg_idx
            integer, intent(out)               :: guess_noflev
            real(F64), intent(in)              :: eps
            
            integer :: l, k, i
            integer :: degeneracy
            integer :: mx

            guess_deg = 0
            degeneracy = 0
            k = 1
            lloop: do l = 1, n
                  degeneracy = degeneracy + 1
                  if (l == n) then
                        guess_deg(k) = degeneracy
                        k = k + 1
                  else
                        if (abs(eexcit(l)-eexcit(l+1)) > abs(eexcit(l)*eps)) then
                              guess_deg(k) = degeneracy
                              k = k + 1
                              degeneracy = 0
                        end if
                  end if
            end do lloop

            guess_noflev = k - 1

            guess_deg_idx = 1

            mx = maxval(guess_deg(1:guess_noflev))
            do l = 1, mx
                  k = 1
                  do i = 1, guess_noflev
                        if (guess_deg(i) == l)then
                              guess_deg_idx(i) = k
                              k = k + 1
                        end if
                  end do
            end do


      end subroutine degeneracy_scan

      subroutine irrep_scan(ci_rep, npair, nirrep, order)
            integer, dimension(:), intent(in) :: ci_rep
            integer, intent(in) :: npair
            integer, dimension(:), intent(out) :: nirrep
            integer, intent(in) :: order
            
            integer :: i, j
            
            nirrep = 0

            do i = 1, order
                  do j = 1, npair
                        if (ci_rep(j) == i) nirrep(i) = nirrep(i) + 1
                  end do
            end do

      end subroutine irrep_scan

      subroutine symmetrize_arbitrary(m, n_sym)

            real(F64), dimension(:,:), intent(inout)  :: m
            integer, dimension(:), intent(out) :: n_sym

            integer, dimension(:,:), allocatable :: aosymm
            real(F64), dimension(:,:), allocatable :: p_mo            
            real(F64), dimension(:,:), allocatable :: m_symm
            integer, dimension(:), allocatable :: rep_list
            integer :: order
            integer :: i, j, k
            integer, dimension(:), allocatable :: ao
            integer :: nvec


            if (POINT_GROUP == C2v) then
                  order = C2v_order
            else if (POINT_GROUP == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            allocate(aosymm(CC_NORB, order))
            ! call aosymmetrize2(aosymm, order, POINT_GROUP)
            ! stop
            call aosymmetrize(aosymm, order, POINT_GROUP)
            print*, '           ', '   E  ', '  C2z ', '  C2y ',  '  C2x ', '  I   ', '  Oxy ', '  Oxz ', '  Oyz '
            ! print*, aosymm
            ! do i = 1, CC_NORB
            !       print*, 'i', i
            !       write(*,'(A5, I3, A3, 8I6)') "phi", i,  '|', aosymm(i, 1:order)
            ! end do

            nvec = size(m, dim=2)
            allocate(m_symm(CC_NORB, nvec))
            allocate(p_mo(CC_NORB, order))                
            allocate(rep_list(nvec))

            i = 1
            j = 1
            do while (i .le. nvec)
                  call project_mo2(m(:, j), p_mo, order, aosymm)
                  
                  j = j + 1
                  call add_symmetrized_vec(i, p_mo, m_symm, order, rep_list)
            end do

            k = 1
            n_sym = 0
            do i = 1, order
                  do j = 1, nvec
                        if (rep_list(j) == i) then
                              m(:, k) = m_symm(:, j)
                              n_sym(i) = n_sym(i) + 1
                              k = k + 1
                        end if
                  end do
            end do
            deallocate(m_symm)
            deallocate(rep_list)


      end subroutine symmetrize_arbitrary

     subroutine project_mo2(mo, p_mo, h, aosymm)
            real(F64), dimension(:), intent(in) :: mo
            real(F64), dimension(:,:), intent(out) :: p_mo
            integer, intent(in) :: h
            integer, dimension(:, :), intent(in) :: aosymm

            integer :: gamma, R
            real(F64) :: h_real_inv
            real(F64), dimension(:), allocatable :: tr_mo

            allocate(tr_mo(CC_NORB))

            if (POINT_GROUP == D2h)then
                  h_real_inv = one / eight
            else if (POINT_GROUP == C2v) then
                  h_real_inv = one / four
            end if

            p_mo = zero

            do gamma = 1, h
                  do R = 1, h
                        call trans_mo2(mo, tr_mo, R, aosymm)
                        p_mo(:, gamma) = p_mo(:, gamma) +  h_real_inv * char_table_real(POINT_GROUP, gamma, R) * tr_mo
                  end do
            end do

            deallocate(tr_mo)

      end subroutine project_mo2

      subroutine trans_mo2(mo, tr_mo, R, aosymm)
            real(F64), dimension(:), intent(in) :: mo
            real(F64), dimension(:), intent(out) :: tr_mo
            integer, intent(in) :: R
            integer, dimension(:,:), intent(in) :: aosymm

            real(F64) :: phase
            integer :: k, i

            tr_mo = zero

            do i = 1, CC_NORB
                  if (aosymm(i, R) .lt. 0)then
                        k = abs(aosymm(i, R))
                        phase = -one
                  else
                        k = aosymm(i, R)
                        phase = one
                  end if
 
                  tr_mo(k) = tr_mo(k) + mo(i) * phase
            end do
      end subroutine trans_mo2


      function char_table_real(symm, gamma, R)

            real(F64) :: char_table_real
            integer, intent(in) :: symm
            integer, intent(in) :: gamma
            integer, intent(in) :: R
            integer :: chi_gamma
            
            if(symm == D2h) then
                  chi_gamma = D2h_char_table(gamma, R)
            else if(POINT_GROUP == C2v) then
                  chi_gamma = C2v_char_table(gamma, R)
            end if
            if (chi_gamma .gt. 0)then
                  char_table_real = one
            else
                  char_table_real = -one
            end if

      end function char_table_real

      ! subroutine mosymmetrize(eorb, c, c_symm, c_rep, symm, nactive, overlap, &
      !       nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, c_idx)

      !       real(F64), dimension(:), intent(inout)             :: eorb
      !       real(F64), dimension(:,:), intent(in)              :: c
      !       real(F64), dimension(:,:), contiguous, intent(out) :: c_symm
      !       integer, dimension(:), intent(out)     :: c_rep
      !       integer, intent(in)                    :: symm
      !       integer, intent(in)                    :: nactive
      !       real(F64), dimension(:,:), intent(in)  :: overlap
      !       integer, intent(in)                    :: nocc0, nocc1
      !       integer, intent(in)                    :: nvirt0, nvirt1
      !       integer, dimension(:, :), intent(out)  :: irrep0
      !       integer, dimension(:, :), intent(out)  :: irrep1
      !       integer, dimension(:),intent(out)      :: c_idx

      !       integer, dimension(:, :), allocatable    :: aosymm
      !       real(F64), dimension(:, :), allocatable  :: cs
      !       integer, dimension(:), allocatable       :: c_rep_nocc
      !       integer, dimension(:), allocatable       :: c_rep_nvirt
      !       integer, dimension(:, :), allocatable    :: c_symm_nocc
      !       integer, dimension(:, :), allocatable    :: c_symm_nvirt

      !       real(F64), dimension(:,:), allocatable   :: trans_mo
      !       real(F64), dimension(:,:), allocatable   :: project_mo
            
      !       integer :: nocc, nvirt
      !       integer :: i
      !       integer :: order

      !       order = 0

      !       if (symm == C2v) then
      !             order = C2v_order
      !       else if (symm == D2h) then
      !             order = D2h_order
      !       else
      !             call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
      !             stop
      !       end if

      !       nocc = nocc1 - nocc0 + 1
      !       nvirt = nvirt1 - nvirt0 + 1


      !       allocate(c_rep_nocc(nocc0:nocc1))
      !       allocate(c_rep_nvirt(nvirt0:nvirt1))
      !       allocate(c_symm_nocc(CC_NORB, nocc))
      !       allocate(c_symm_nvirt(CC_NORB, nvirt))
      !       allocate(aosymm(CC_NORB, order))
      !       allocate(cs(CC_NORB, CC_NORB*order))
      !       allocate(trans_mo(CC_NORB, order))
      !       allocate(project_mo(CC_NORB, order))

      !       !
      !       ! Find AO symmetries, and store them in aosymm matrix:
      !       ! 
      !       ! Example of AOSYMM for C2v group
      !       !     |E     |C2(z)    |Oxz     | Oyz   |
      !       ! chi1|p*idx1| p*idx2  | p*idx3 | p*idx4|
      !       ! ...
      !       ! where chi1 transforms to chi_idx2 under the C2(z)
      !       ! symmetry operator. p is the phase factor
      !       ! -1 if the sign of orbital changes, and 1 if
      !       ! the sign doesn't change

      !       call aosymmetrize(aosymm, order, symm)
      !       ! print*, '           ', '   E  ', '  C2z ', '  C2y ',  '  C2x ', '  I   ', '  Oxy ', '  Oxz ', '  Oyz ' 
      !       ! do i = 1, CC_NORB
      !       !       write(*,'(A5, I3, A3, 8I6)') "phi", i,  '|', aosymm(i, :)
      !       ! end do


      !       cs = zero
      !       c_symm = zero
      !       irrep0 = 0
      !       irrep1 = 0

      !       do i = 1, nactive
      !             call transform_phi_one(aosymm, c(:, i), trans_mo, order)
      !             call project_phi_one(trans_mo, project_mo, order)
      !             call add_symmetrized_mo(i, project_mo, c_symm, order, c_rep, overlap)
      !       end do

      !       call sort_mo(c_symm, eorb, c_rep, 1, nocc, order, 1, irrep0, irrep1, c_idx)
      !       call sort_mo(c_symm, eorb, c_rep, nocc+1, nactive, order, 2, irrep0, irrep1, c_idx)

      !       allocate(morep(CC_NORB))

      !       call moorthogonalize(c_symm, c_rep, 1, nocc, order, 1, overlap)
      !       call moorthogonalize(c_symm, c_rep, nocc+1, nactive, order, 2, overlap)
      !       call monormalize(c_symm, nactive, overlap)


      !       do i = 1, nactive
      !             call msg_state_irrep(eorb(i), c_rep(i), POINT_GROUP)
      !       end do

      !       deallocate(cs)

      !       deallocate(c_rep_nocc)
      !       deallocate(c_rep_nvirt)
      !       deallocate(c_symm_nocc)
      !       deallocate(c_symm_nvirt)
            
      ! end subroutine mosymmetrize


      subroutine project_phi_one(trans_mo, project_mo, order)
            real(F64), dimension(:,:), intent(in)  :: trans_mo
            real(F64), dimension(:,:), intent(out) :: project_mo
            integer, intent(in)                    :: order

            real(F64), dimension(:), allocatable :: project_mo_R
            integer :: R, gamma

            allocate(project_mo_R(CC_NORB))

            project_mo = zero

            do gamma = 1, order
                  do R = 1, order
                        call R_contrb_to_project_mo(gamma, R, trans_mo(:, R), project_mo_R)
                        project_mo(:, gamma) = project_mo(:, gamma) +  project_mo_R
                  end do
            end do
            project_mo = project_mo / dble(order)

            deallocate(project_mo_R)

      end subroutine project_phi_one

      subroutine R_contrb_to_project_mo(gamma, R, trans_mo, project_mo_R)
            integer, intent(in) :: gamma
            integer, intent(in) :: R
            real(F64), dimension(:), intent(in) :: trans_mo
            real(F64), dimension(:), intent(out) :: project_mo_R
            integer :: chi_gamma

            chi_gamma = -1
            if(POINT_GROUP == D2h) then
                  chi_gamma = D2h_char_table(gamma, R)
            else if(POINT_GROUP == C2v) then
                  chi_gamma = C2v_char_table(gamma, R)
            end if

            project_mo_R = chi_gamma * trans_mo

      end subroutine R_contrb_to_project_mo

      subroutine transform_phi_one(aosymm, mo, trans_mo, order)
            !                                                                                                                                                
            ! Transforms molecular orbital (mo) through all the                                                                                           
            ! symmetry operators in the POINT GROUP                                                                                                    
            ! The result is stored in trans_mo                                                                                                            
            !                                                                                                                                            
            ! Example of trans_mo in C2v group                                                                                                           
            !     |E     |C2(z)    |Oxz     |Oyz    |                                                                                                      
            !     |E(mo) |C2(z)(mo)|Oxz(mo) |Oyz(mo)|                                                                                                   
            ! trans_mo(CC_NORB, POINT_GROUP_ORDER)                                                                                                
            !                                                                                                                                              
            integer, dimension(:,:), intent(in)    :: aosymm
            real(F64), dimension(:), intent(in)    :: mo
            real(F64), dimension(:,:), intent(out) :: trans_mo
            integer, intent(in)                    :: order

            integer :: i
            
            do i = 1, order
                  call trans_phi_through_R(mo, i, aosymm, trans_mo(:,i))                  
            end do

      end subroutine transform_phi_one

      subroutine trans_phi_through_R(mo, R, aosymm, trans_mo_R)
            !                                                                                                                                              
            ! Transforms molecular orbital (mo) through                                                                                               
            ! symmetry operator R                                                                                                                       
            ! The result R(mo) is returned in trans_mo_R                                                                                           
            !                                                                                                                                         
            real(F64), dimension(:), intent(in)  :: mo
            integer, intent(in)                  :: R
            integer, dimension(:,:), intent(in)  :: aosymm
            real(F64), dimension(:), intent(out) :: trans_mo_R

            integer :: i, k
            real(F64) :: phase

            trans_mo_R = zero

            !                                                                                                                                            
            ! R(mo_a) = R(\sum_k mo_a(k) * ao(k))                                                                                                     
            !                                                                                                                                    

            do i = 1, CC_NORB
                  call project_ao(R, i, aosymm, k, phase)
                  trans_mo_R(k) = trans_mo_R(k) + mo(i) * phase
            end do

      end subroutine trans_phi_through_R

      function sym_op(i)
            !                                                                                                                             
            ! Returns the integer corresponding to the given                                                                                        
            ! symmetry operation for the POINT GROUP                                                                                          
            !                                                                                                                               
            ! For egzample for i = 3                                                                                                    
            ! In D2h sym_op = 3  (3 <= C2y)                                                                                   
            ! In C2v sym_op = 7  (7 <= Oxz)                                                                                                
            !                                                                                                                                      
            integer :: sym_op
            integer, intent(in) :: i
            sym_op = -1
            if (POINT_GROUP == D2h) then
                  select case (i)

                  case (1)
                        sym_op = SYM_E
                  case (2)
                        sym_op = SYM_C2z
                  case (3)
                        sym_op = SYM_C2y
                  case (4)
                        sym_op = SYM_C2x
                  case (5)
                        sym_op = SYM_I
                  case (6)
                        sym_op = SYM_Oxy
                  case (7)
                        sym_op = SYM_Oxz
                  case (8)
                        sym_op = SYM_Oyz
                  end select

            else if (POINT_GROUP== C2v) then
                  select case (i)
                  case (1)
                        sym_op = SYM_E
                  case (2)
                        sym_op = SYM_C2z
                  case (3)
                        sym_op = SYM_Oxz
                  case (4)
                        sym_op = SYM_Oyz
                  end select
            end if

      end function sym_op

      function print_sym_op(i)
            character(len=8) :: print_sym_op
            integer, intent(in) :: i
            print_sym_op = ""

            if (POINT_GROUP == D2h) then
                  select case (i)
                        
                  case (1)
                        print_sym_op = "SYM_E"
                  case (2)
                        print_sym_op = "SYM_C2z"
                  case (3)
                        print_sym_op = "SYM_C2y"
                  case (4)
                        print_sym_op = "SYM_C2x"
                  case (5)
                        print_sym_op = "SYM_I"
                  case (6)
                        print_sym_op = "SYM_Oxy"
                  case (7)
                        print_sym_op = "SYM_Oxz"
                  case (8)
                        print_sym_op = "SYM_Oyz"
                  end select

            else if (POINT_GROUP== C2v) then
                  select case (i)
                  case (1)
                        print_sym_op = "SYM_E"
                  case (2)
                        print_sym_op = "SYM_C2z"
                  case (3)
                        print_sym_op = "SYM_Oxz"
                  case (4)
                        print_sym_op = "SYM_Oyz"
                  end select
            end if

      end function print_sym_op

      function return_rep(i)
            !                                                                                                                                 
            ! Returns the integer corresponding to the given                                                                                           
            ! representation for the POINT GROUP                                                                                                      
            !                                                                                                                                     
            ! For egzample for i = 3                                                                                                           
            ! In D2h return_rep = 3  (3 <= B2g)                                        
            ! In C2v return_rep = 3  (3 <= B1 )        

            integer :: return_rep
            integer, intent(in) :: i

            return_rep = -1
            if (POINT_GROUP == D2h) then
                  select case (i)

                  case (1)
                        return_rep = REP_Ag
                  case (2)
                        return_rep = REP_B1g
                  case (3)
                        return_rep = REP_B2g
                  case (4)
                        return_rep = REP_B3g
                  case (5)
                        return_rep = REP_Au
                  case (6)
                        return_rep = REP_B1u
                  case (7)
                        return_rep = REP_B2u
                  case (8)
                        return_rep = REP_B3u
                  end select

            else if (POINT_GROUP== C2v) then
                  select case (i)
                  case (1)
                        return_rep = REP_A1
                  case (2)
                        return_rep = REP_A2
                  case (3)
                        return_rep = REP_B1
                  case (4)
                        return_rep = REP_B2
                  end select
            end if
      end function return_rep

      subroutine add_symmetrized_mo(i, project_mo, c_symm, order, c_rep, overlap)

            integer, intent(in) :: i
            real(F64), dimension(:,:), intent(in)    :: project_mo
            real(F64), dimension(:,:), intent(inout) :: c_symm
            integer, intent(in) :: order
            integer, dimension(:), intent(out) :: c_rep
            real(F64), dimension(:,:), intent(in) :: overlap

            real(F64), dimension(:,:), allocatable :: lin_ind_project_mo            
            integer, dimension(:), allocatable :: lin_ind_idx
            real(F64) :: nrm0, nrm
            integer   :: k
            integer   :: l
            integer   :: candidate_idx
            logical   :: cond


            allocate(lin_ind_project_mo(CC_NORB, order))
            allocate(lin_ind_idx(order))
            
            candidate_idx = -1

            if (i.ne.1) then
                  l = 0
                  do k = 1, order
                        c_symm(:, i) = project_mo(:, k)
                        cond = lincheck(c_symm, i, overlap)
                        if (cond .eqv. .false.) then
                              l = l + 1
                              lin_ind_project_mo(:, l) = project_mo(:,k)
                              lin_ind_idx(l) = k
                        end if
                  end do
            end if

            if (i.ne.1) then
                  nrm0 = zero
                  do k = 1, l
                        nrm = dot_with_overlap(lin_ind_project_mo(:, k), lin_ind_project_mo(:, k), overlap, CC_NORB)
                        if(nrm.gt.nrm0)then
                              candidate_idx = lin_ind_idx(k)
                              nrm0 = nrm
                        end if
                  end do
            else
                  nrm0 = zero
                  do k = 1, order
                        nrm = dot_with_overlap(project_mo(:, k), project_mo(:, k), overlap, CC_NORB)
                        if(nrm.gt.nrm0)then
                              candidate_idx = k
                              nrm0 = nrm
                        end if
                  end do
            end if

            c_symm(:, i) = project_mo(:, candidate_idx)
            c_rep(i) = return_rep(candidate_idx)


      end subroutine add_symmetrized_mo

      function dot_with_overlap(c1, c2, overlap, n)
            real(F64) :: dot_with_overlap
            real(F64), dimension(:), intent(in) :: c1, c2
            real(F64), dimension(:,:), intent(in) :: overlap
            integer, intent(in) :: n

            integer :: i, j
            
            dot_with_overlap = zero
            do i = 1, n
                  do j = 1, n
                        dot_with_overlap = dot_with_overlap + c1(i) * c2(j) * overlap(i, j)
                  end do
            end do

      end function dot_with_overlap

      ! subroutine sort_mo(c_symm, eorb, c_rep, s0, s1, order, n, irrep0, irrep1, c_idx)
      !       real(F64), dimension(:,:), intent(inout)  :: c_symm
      !       real(F64), dimension(:), intent(inout)    :: eorb
      !       integer, dimension(:), intent(inout) :: c_rep
      !       integer, intent(in) :: s0
      !       integer, intent(in) :: s1
      !       integer, intent(in) :: order
      !       integer, intent(in) :: n
      !       integer, dimension(:,:), intent(out) :: irrep0
      !       integer, dimension(:,:), intent(out) :: irrep1
      !       integer, dimension(:), intent(inout) :: c_idx 


      !       real(F64), dimension(:,:), allocatable :: c_temp
      !       real(F64), dimension(:), allocatable :: e_temp

      !       integer :: dim
      !       integer :: i
      !       integer :: rep_dim
      !       integer :: i0
      !       integer :: i1

      !       dim = s1 - s0 + 1

      !       allocate(c_temp(CC_NORB, s0:s1))
      !       allocate(e_temp(s0:s1))

      !       do i = s0, s1
      !             c_idx(i) = i
      !       end do

      !       call isort(c_rep(s0:s1), c_idx(s0:s1), dim, 2)

      !       call irrep_bound(c_rep, s0, s1, irrep0, irrep1, order, n)

      !       do i = 1, order
      !             i0 = irrep0(n, i)
      !             i1 = irrep1(n, i)
      !             rep_dim = i1 - i0 + 1
      !             if (i0 .ne. 0) call isort(c_idx(i0:i1), c_rep(i0:i1), rep_dim, 2)
      !       end do


      !       do i = s0, s1
      !             c_temp(:, i) = c_symm(:, c_idx(i))
      !             e_temp(i) = eorb(c_idx(i))
      !       end do

      !       eorb(s0:s1) = e_temp(s0:s1)
      !       c_symm(:, s0:s1) = c_temp(:, s0:s1)


      !       deallocate(c_temp)
      !       deallocate(e_temp)

      ! end subroutine sort_mo

      subroutine gen_frozen_irrep(irrep0, irrep1, eorb)
            integer, dimension(:,:), intent(inout) :: irrep0
            integer, dimension(:,:), intent(inout) :: irrep1
            real(F64), dimension(:), intent(in)    :: eorb
            integer, dimension(:), allocatable :: rep
            integer, dimension(:), allocatable :: rep_frozen
            integer :: order
            integer :: i
            integer :: sum_occ, sum_virt
            
            order = size(irrep0, dim=2)
            allocate(rep(order))
            allocate(rep_frozen(order))

            do i = 1, order
                  if (irrep0(1, i) .ne. 0) then
                        sum_occ = irrep1(1, i) - irrep0(1, i) + 1
                  else
                        sum_occ = 0
                  end if

                  if (irrep0(2, i) .ne. 0) then
                        sum_virt = irrep1(2, i) - irrep0(2, i) + 1 
                  else
                        sum_virt = 0
                  end if

                  rep(i) = sum_occ + sum_virt
            end do
            write(*, '(8I5)') rep

!            call frozen_irrep(eorb, rep, rep_frozen)
            call frozen_irrep(eorb, irrep0, irrep1, rep_frozen)

            do i = 1, order
                  if (rep_frozen(i) .gt. 0)then
                        sum_occ = irrep1(1, i) - irrep0(1, i) + 1
                        print*, sum_occ, rep_frozen(i)
                        if (rep_frozen(i) .gt. sum_occ) then
                              call msg("SYMMETRY ERROR: WRONG NUMBER OF FROZEN ORBITALS:", &
                                    priority=MSG_ERROR)
                              stop
                        else if (rep_frozen(i) == sum_occ) then
                              irrep0(1, i) = 0
                              irrep1(1, i) = 0
                        else if(rep_frozen(i) .lt. sum_occ) then
                              irrep0(1, i) = irrep0(1, i) + rep_frozen(i)
                        end if
                  end if
            end do

            print*, 'irrep w frozen'
            write(*, '(8I5)') irrep0(1, :)
            write(*, '(8I5)') irrep1(1, :)
            write(*, '(8I5)') irrep0(2, :)
            write(*, '(8I5)') irrep1(2, :)

            
      end subroutine gen_frozen_irrep

      subroutine frozen_irrep(eorb, irrep0, irrep1, rep_frozen)
!      subroutine frozen_irrep(eorb, rep, rep_frozen)
            
            real(F64), dimension(:), intent(in) :: eorb
            integer, dimension(:,:), intent(in) :: irrep0, irrep1
!            integer, dimension(:), intent(in) :: rep
            integer, dimension(:), intent(out) :: rep_frozen
            integer, dimension(:), allocatable :: rep_all
            real(F64), dimension(:), allocatable :: eorb_copy
            integer :: order
            integer :: n
            integer :: i, j, k


            order = size(rep_frozen)
            n = 0
            do i = 1, order
                  if (irrep0(1, i) .ne. 0) then
                        n = n +  irrep1(1, i) - irrep0(1, i) + 1
                  end if

                  if (irrep1(2, i) .ne. 0) then
                        n = n +  irrep1(2, i) -irrep0(2, i) + 1
                  end if

            end do
!            n = sum(rep)
            
            
            allocate(rep_all(n))
            allocate(eorb_copy(n))
            eorb_copy = eorb(1:n)

            k = 1
            do i = 1, order
                  if (irrep0(1, i) .ne. 0) then
                        do j = irrep0(1, i), irrep1(1, i)
                              rep_all(k) = i
                              k = k +1
                        end do
                  end if
            end do

           do i = 1, order
                  if (irrep0(2, i) .ne. 0) then
                        do j = irrep0(2, i), irrep1(2, i)
                              rep_all(k) = i
                              k= k +1
                        end do
                  end if
            end do

            do i = 1, n
                  print*, rep_all(i)
            end do


            ! k = 1
            ! do i = 1, order
            !       do j = k, k+rep(i)-1
            !             rep_all(j) = i
            !             print*, eorb(j), j, i
            !       end do
            !       k = k + rep(i)
            ! end do

            call dsort(eorb_copy, rep_all, n)
            do i = 1, n
                  print*, eorb_copy(i), rep_all(i)
            end do

            rep_frozen = 0
            do i = 1, CC_FROZEN
                  rep_frozen(rep_all(i)) = rep_frozen(rep_all(i)) + 1
            end do
            write(*, '(8I5)') rep_frozen
            
            deallocate(rep_all)
            deallocate(eorb_copy)

      end subroutine frozen_irrep

      subroutine sort_mo_symm(mocoeff, eorb, rep, irrep0, irrep1, nactive)
            !
            ! Sort mocoeff in symmetries, nocc first, 
            !     nocc            nvirt  
            ! Ag B1g B2g....   Ag B1g B2g...
            !
            real(F64), dimension(:,:), intent(inout) :: mocoeff
            real(F64), dimension(:), intent(inout)   :: eorb
            integer, dimension(:), intent(in)        :: rep
            integer, dimension(:,:), intent(out)     :: irrep0
            integer, dimension(:,:), intent(out)     :: irrep1
            integer, intent(out) :: nactive

            real(F64), dimension(:,:), allocatable   :: mocoeff_work
            real(F64), dimension(:), allocatable     :: eorb_work
            integer, dimension(:), allocatable       :: work
            integer, dimension(:), allocatable       :: rep_occ

            integer :: i, idx0, idx1, k, j, l
            integer :: rep_dim
            integer :: rep_nocc
            integer :: order

            if (POINT_GROUP == D2h) then
                  order = 8
            else if (POINT_GROUP ==C2v) then
                  order = 4
            end if

            allocate(mocoeff_work(CC_NORB, CC_NORB))
            allocate(eorb_work(CC_NORB))
            allocate(work(CC_NORB))
            allocate(rep_occ(order))

            mocoeff_work = mocoeff
            eorb_work = eorb

            ! print*, 'na wejsciu'
            ! do i = 1, CC_NORB
            !       print*, i, eorb(i)
            ! end do

            do i = 1, CC_NORB
                  work(i) = i
            end do

            call dsort(eorb_work, work, CC_NORB)
            
            rep_occ = 0
            do i = 1, NE/2
                  do j = 1, order
                        if (work(i) .ge. sum(rep(1:j-1))+ 1 .and. work(i) .le. sum(rep(1:j))) then
                              print*, work(i)
                              rep_occ(j) = rep_occ(j) + 1
                        end if
                  end do
            end do
            
            write(*, '(8I4)') rep_occ

            eorb_work = eorb

            ! print*, 'przed sortowaniem'
            ! do i = 1, size(eorb)
            !       print*, eorb(i)
            ! end do

            do i = 1, CC_NORB
                  work(i) = i
            end do

            irrep0 = 0
            irrep1 = 0
            
            k = 1
            do i = 1, order
                  idx0 = sum(rep(1:i-1)) + 1
                  idx1 = idx0 + rep(i) - 1
                  rep_dim = idx1 - idx0 + 1
                  
                  call dsort(eorb(idx0:idx1), work(idx0:idx1), rep_dim)

                  ! rep_nocc = 0
                  ! do j = idx0, idx1
                  !       if (eorb(j) .lt. zero)then
                  !             rep_nocc = rep_nocc + 1
                  !       end if
                  ! end do
                  

                  if (rep_occ(i) .ne. 0) then
                        irrep0(1, i) = k

                        do j = idx0, idx0 + rep_occ(i) - 1
                              eorb_work(k) = eorb(j)
                              mocoeff_work(:, k) = mocoeff(:, work(j))
                              irrep1(1, i) = k
                              k = k + 1
                        end do
                  end if
            end do

            do i = 1, order
                  idx0 = sum(rep(1:i-1)) + 1
                  idx1 = idx0 + rep(i) - 1
                  rep_dim = idx1 - idx0 + 1
                  ! rep_nocc = 0
                  ! do j = idx0, idx1
                  !       if (eorb(j) .lt. zero)then
                  !             rep_nocc = rep_nocc + 1
                  !       end if
                  ! end do
                  
                  if (rep_dim - rep_occ(i) .ne. 0) then
                        irrep0(2, i) = k

                        do j = idx0 + rep_occ(i), idx1
                              eorb_work(k) = eorb(j)
                              mocoeff_work(:, k) = mocoeff(:, work(j))
                              irrep1(2, i) = k
                              k = k +1
                        end do
                  end if
            end do
            
!            print*, 'irrep poza frozen'
!            write(*, '(8I5)') irrep0(1, :)
!            write(*, '(8I5)') irrep1(1, :)
!            write(*, '(8I5)') irrep0(2, :)
!            write(*, '(8I5)') irrep1(2, :)

            mocoeff = mocoeff_work
            eorb = eorb_work

            nactive = sum(rep)

            deallocate(mocoeff_work)
            deallocate(eorb_work)
            deallocate(work)


      end subroutine sort_mo_symm

      subroutine project_ao(sym_op, ao_idx, aosymm, proj_ao, phase)
            integer, intent(in)                 :: sym_op
            integer, intent(in)                 :: ao_idx
            integer, dimension(:,:), intent(in) :: aosymm
            integer, intent(out)                :: proj_ao
            real(F64), intent(out)              :: phase

            proj_ao = aosymm(ao_idx, sym_op)

            phase = zero

            if(proj_ao.lt.0)then
                  phase = -one
            else
                  phase = one
            end if

            proj_ao = abs(proj_ao)

      end subroutine project_ao

      function print_morep2(i, irrep0, irrep1, order)
            character(len=5) :: print_morep2
            integer, intent(in) :: i
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, intent(in) :: order
            integer :: j

            do j = 1, order
                  if (i .ge. irrep0(1, j) .and. i .le. irrep1(1, j))then
                        print_morep2 = print_rep(j)
                  else if (i .ge. irrep0(2, j) .and. i .le. irrep1(2, j))then
                        print_morep2 = print_rep(j)
                  end if
            end do

      end function print_morep2

      function morep_direct_product(moidx)
            integer :: morep_direct_product
            integer, dimension(:), intent(in) :: moidx
            integer :: n
            integer, dimension(:), allocatable :: input_irreps
            integer :: i

            n = size(moidx)
            allocate(input_irreps(n))

            do i = 1, n
                  input_irreps(i) = morep(moidx(i))
            end do

            call direct_product(input_irreps, POINT_GROUP, morep_direct_product)

            deallocate(input_irreps)

      end function morep_direct_product
      
!       subroutine moorthogonalize(c_symm, c_rep, s0, s1, order, n, overlap)
            
!             real(F64), dimension(:,:), contiguous, intent(inout) :: c_symm
!             integer, dimension(:), intent(in)        :: c_rep
!             integer, intent(in)                      :: s0
!             integer, intent(in)                      :: s1
!             integer, intent(in)                      :: order
!             integer, intent(in)                      :: n
!             real(F64), dimension(:,:), intent(in)    :: overlap

!             real(F64), dimension(:,:), allocatable   :: sq, invsq
!             real(F64), dimension(:,:), allocatable   :: c_symm_t
!             real(F64), dimension(:,:), allocatable   :: over_copy

!             integer :: i, j
!             integer :: nrep
!             integer :: a0, a1
!             integer :: m
!             integer, dimension(:, :), allocatable :: rep_dim


!             m = size(overlap, dim=1)

!             allocate(rep_dim(2, order))
!             allocate(sq(m, m))
!             allocate(invsq(m, m))
!             allocate(over_copy(m, m))
!             allocate(c_symm_t(m, m))

!             over_copy = overlap

!             call ssqinvsq(over_copy, invsq, sq)
!             call transform_orbitals(c_symm_t, sq, c_symm)
            
!             j = s0
!             do i = 1, order
!                   nrep = count_rep(i, c_rep, s0, s1)
!                   if(nrep .ne. 0)then
!                         rep_dim(1, i) = j
!                         rep_dim(2, i) = j + nrep - 1
!                   else
!                         rep_dim(1, i) = 0
!                         rep_dim(2, i) = 0
!                   end if
!                   j = j + nrep
!             end do

! !            do i = 1, order
! !                  if(rep_dim(1, i) .ne. 0) then
! !                        a0 = rep_dim(1, i)
! !                        a1 = rep_dim(2, i)
! !                        call la_gramschmidt(.true., c_symm_t(:, a0:a1), 0)
! !                  end if
! !            end do

!             call transform_orbitals(c_symm, invsq, c_symm_t)

!             deallocate(rep_dim)
!             deallocate(sq)
!             deallocate(invsq)
!             deallocate(over_copy)
!             deallocate(c_symm_t)


!       end subroutine moorthogonalize

      function count_rep(rep, c_rep, s0, s1)
            integer :: count_rep
            integer, intent(in) :: rep
            integer, dimension(:), intent(in) :: c_rep
            integer, intent(in) :: s0
            integer, intent(in) :: s1

            integer :: i

            count_rep = 0
            do i = s0, s1
                  if(c_rep(i)==rep)then
                        count_rep = count_rep + 1
                  end if
            end do

      end function count_rep

      ! subroutine monormalize(c_symm, nactive, overlap)
      !       real(F64), dimension(:,:), intent(inout) :: c_symm
      !       integer, intent(in) :: nactive
      !       real(F64), dimension(:,:), intent(in) :: overlap
      !       real(F64) :: nrm
      !       integer :: p
      !       integer :: q
      !       integer :: i

      !       do i = 1, nactive
      !             nrm = zero
      !             do p = 1, CC_NORB
      !                   do q = 1, CC_NORB
      !                         nrm = nrm + c_symm(p, i) * c_symm(q, i) * overlap(p, q)
      !                   end do
      !             end do
      !             c_symm(:, i) = c_symm(:, i) / sqrt(nrm)
      !       end do

      ! end subroutine monormalize

      function symmpos(elem, symm)
            integer :: symmpos
            integer, intent(in) :: elem
            integer, intent(in) :: symm

            symmpos = 0

            if (symm == D2h) then
                  if (elem == SYM_E) then
                        symmpos = 1
                  else if (elem == SYM_C2z) then
                        symmpos = 2
                  else if (elem == SYM_C2y) then
                        symmpos = 3
                  else if (elem == SYM_C2x) then
                        symmpos = 4
                  else if (elem == SYM_I)   then
                        symmpos = 5
                  else if (elem == SYM_Oxy) then 
                        symmpos = 6
                  else if (elem == SYM_Oxz) then
                        symmpos = 7
                  else if (elem == SYM_Oyz) then
                        symmpos = 8
                  end if
            else if (symm == C2v) then
                  if (elem == SYM_E) then
                        symmpos = 1
                  else if (elem == SYM_C2z) then
                        symmpos = 2
                  else if (elem == SYM_Oxz) then
                        symmpos = 3
                  else if (elem == SYM_Oxz) then
                        symmpos = 4
                  end if

            end if

      end function symmpos

      subroutine direct_product(input_irreps, symm, output_irrep)
            !
            ! INPUT_IRREPS 
            !       - vector, contains irreps for direct product
            !        
            ! SYMM 
            !       - point group
            ! 
            ! OUTPUT_IRREP
            !       - scalar, output irrep
            !
            integer, dimension(:), intent(in)  :: input_irreps
            integer, intent(in)                :: symm
            integer, intent(out)               :: output_irrep

            integer, dimension(:), allocatable :: charac

            integer :: i, j
            integer :: n
            integer :: order

            n = size(input_irreps)
            order = -1
            if (symm == D2h) then
                  order = D2h_order
            else if (symm == C2v) then
                  order = C2v_order
            end if

            allocate(charac(order))

            charac = 1

            do j = 1, n
                  do i = 1, order
                        charac(i) = charac(i) * rep_char(input_irreps(j), i, symm)
                  end do
            end do


            if (symm == D2h) then
                  call D2hrepr(charac, output_irrep)
            else if (symm == C2v) then
                  call C2vrepr(charac, output_irrep)
            else
                  call msg("SYMMETRY ERROR: POINT GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop                 
            end if
            
            deallocate(charac)
            
      end subroutine direct_product

      subroutine blocks_dimensions_s(iexci, irrep0, irrep1, hdim, order, ci_rep)
            integer, dimension(:), intent(in)   :: iexci
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            integer, intent(out)                :: hdim
            integer, intent(in)                 :: order
            integer, dimension(:), intent(out)  :: ci_rep
            
            integer :: oc0, oc1, vt0, vt1
            integer, parameter :: OC = 1
            integer, parameter :: VT = 2
            integer, dimension(:, :), allocatable :: ipairs

            integer :: i, j
            integer :: ocdim, vtdim
            integer :: hdimsmall

            allocate(ipairs(2, order))

            hdim = 0

            do i = 1, order

                  if (iexci(i) .ne. 0) then
                        hdimsmall = 0
                        call irrep_singles(i, ipairs, POINT_GROUP)

                        do j = 1, order
                              oc0 = irrep0(OC, ipairs(OC, j))
                              oc1 = irrep1(OC, ipairs(OC, j))
                              vt0 = irrep0(VT, ipairs(VT, j))
                              vt1 = irrep1(VT, ipairs(VT, j))


                              if (oc0 .ne. 0 .and. vt0 .ne. 0) then
                                    ocdim = (oc1-oc0+1)
                                    vtdim = (vt1-vt0+1)
                                    hdimsmall = hdimsmall + ocdim * vtdim
                              end if
                        end do
                        if (iexci(i) .gt. hdimsmall) then
                              call msg("NOT ENOUGH GUESS VECTORS FOR IRREP " // print_rep(i), MSG_ERROR)
                              stop
                        else
                              ci_rep(i) = hdimsmall
                              hdim = hdim + hdimsmall
                        end if
                  else
                        ci_rep(i) = 0
                  end if
            end do

            deallocate(ipairs)
            
      end subroutine blocks_dimensions_s


      subroutine blocks_dimensions_d(iexci, irrep0, irrep1, hdim, order, ci_rep)
            integer, dimension(:), intent(in)   :: iexci
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            integer, intent(out)                :: hdim
            integer, intent(in)                 :: order
            integer, dimension(:), intent(out)  :: ci_rep

            integer :: n0a, n1a, n0b, n1b, n0i, n1i, n0j, n1j
            integer, parameter :: OC = 1
            integer, parameter :: VT = 2
            integer, dimension(:, :), allocatable :: idoubles

            integer :: i, j
            integer :: hdimsmall
            integer :: idimd

            ! allocate(ipairs(2, order))
            ! allocate(idoubles(4, order))


            hdim = 0

            do i = 1, order
                  if (iexci(i) .ne. 0) then
                        hdimsmall = 0
                        call irrep_doubles(i, idoubles, POINT_GROUP, idimd, .true.)
                        allocate(idoubles(4,idimd)) 
                        call irrep_doubles(i, idoubles, POINT_GROUP, idimd, .false.)

                        do j = 1, idimd

                              n0i = irrep0(OC, idoubles(1, j))
                              n1i = irrep1(OC, idoubles(1, j))

                              n0j = irrep0(OC, idoubles(2, j))
                              n1j = irrep1(OC, idoubles(2, j))

                              n0a = irrep0(VT, idoubles(3, j))
                              n1a = irrep1(VT, idoubles(3, j))

                              n0b = irrep0(VT, idoubles(4, j))
                              n1b = irrep1(VT, idoubles(4, j))

                              if (n0i .ne. 0 .and. n0j .ne. 0 .and. n0a .ne. 0 .and. n0b .ne. 0) then
                                    hdimsmall = hdimsmall + (n1i-n0i+1) * (n1j-n0j+1) * (n1a-n0a+1) * (n1b-n0b+1)
                              end if
                        end do

                        deallocate(idoubles)
                        if (iexci(i) .gt. hdimsmall) then
                              call msg("NOT ENOUGH GUESS VECTORS FOR IRREP " // print_rep(i), MSG_ERROR)
                              stop
                        else
                              ci_rep(i) = hdimsmall
                              hdim = hdim + hdimsmall
                        end if
                  else
                        ci_rep(i) = 0
                  end if
            end do


      end subroutine blocks_dimensions_d


      subroutine trim_virtual(eorbactive, irrep0, irrep1, order, irrep0_s, irrep1_s, thresh_cisd)
            real(F64), dimension(:), intent(in) :: eorbactive
            integer, dimension(:,:), intent(in) :: irrep0, irrep1
            integer, intent(in) :: order
            integer, dimension(:,:), intent(out) :: irrep0_s, irrep1_s
            real(F64), intent(in) :: thresh_cisd

            integer :: i, j, s0, s1, s1_new
            logical :: tr
            
            irrep0_s = irrep0
            irrep1_s(1, :) = irrep1(1, :)
            irrep1_s(2, :) = irrep0(2, :)

            do i = 1, order
                  s0 = irrep0(2, i)
                  s1 = irrep1(2, i)
                  s1_new = -1
                  tr = .true.

                  if (s0 .ne. 0 .and. s1 .ne. 0)then

                        jloop: do j = s0, s1
                              if (eorbactive(j) .gt. thresh_cisd)then
                                    tr = .false.
                                    exit jloop
                              else
                                    s1_new = j
                              end if
                        end do jloop
                  else
                        s1_new = 0
                  end if
                  if (tr) then

                        irrep1_s(2, i) = s1_new
                  end if
            end do

      end subroutine trim_virtual

      subroutine sort_ci_coeff(ci_coeff, ci_rep, npair, s0, s1, wrci)
            real(F64), dimension(:,:), intent(inout) :: ci_coeff
            integer, dimension(:), intent(inout) :: ci_rep
            integer, intent(in) :: npair
            integer, intent(in) :: s0
            integer, intent(in) :: s1
            real(F64), dimension(:), intent(inout) :: wrci

            integer, dimension(:), allocatable :: ci_idx
            real(F64), dimension(:), allocatable :: ci_temp
            real(F64) :: wr_temp

            integer :: dim
            integer :: i
            
            dim = s1 - s0 + 1

            allocate(ci_idx(s0:s1))
            allocate(ci_temp(npair))
            
            do i = s0, s1
                  ci_idx(i) = i
            end do

            call isort(ci_rep(s0:s1), ci_idx(s0:s1), dim, 2)
            
            do i = s0, s1
                  ci_temp = ci_coeff(:, i)
                  ci_coeff(:, i) = ci_coeff(:, ci_idx(i))
                  ci_coeff(:, ci_idx(i)) = ci_temp

                  wr_temp = wrci(i)
                  wrci(i) = wrci(ci_idx(i))
                  wrci(ci_idx(i)) = wr_temp
            end do

            deallocate(ci_idx)
            deallocate(ci_temp)

      end subroutine sort_ci_coeff


      function print_rep(rep)
            character(len=5) :: print_rep
            integer, intent(in) :: rep

            if (POINT_GROUP == D2h) then
                  select case (rep)
                  case (rep_ag)
                        print_rep = "Ag"
                  case (rep_b1g)
                        print_rep = "B1g"
                  case (rep_b2g)
                        print_rep = "B2g"
                  case (rep_b3g)
                        print_rep = "B3g"
                  case (rep_au)
                        print_rep = "Au"
                  case (rep_b1u)
                        print_rep = "B1u"
                  case (rep_b2u)
                        print_rep = "B2u"
                  case (rep_b3u)
                        print_rep = "B3u"
                  end select
            else if (POINT_GROUP == C2v) then
                  select case (rep)
                  case (rep_a1)
                        print_rep = "A1"
                  case (rep_a2)
                        print_rep = "A2"
                  case (rep_b1)
                        print_rep = "B1"
                  case (rep_b2)
                        print_rep = "B2"
                  end select
            end if  

      end function print_rep

     function print_rep_mult(rep, multip)
            character(len=5) :: print_rep_mult
            integer, intent(in) :: rep
            integer, intent(in) :: multip
            character(len=1) :: cmult

            write(cmult, '(I1)') multip

            if (POINT_GROUP == D2h) then
                  select case (rep)
                  case (rep_ag)
                        print_rep_mult = "Ag"
                  case (rep_b1g)
                        print_rep_mult = "B1g"
                  case (rep_b2g)
                       print_rep_mult = "B2g"
                  case (rep_b3g)
                        print_rep_mult = "B3g"
                  case (rep_au)
                        print_rep_mult = "Au"
                  case (rep_b1u)
                        print_rep_mult = "B1u"
                  case (rep_b2u)
                        print_rep_mult = "B2u"
                  case (rep_b3u)
                        print_rep_mult = "B3u"
                  end select
            else if (POINT_GROUP == C2v) then
                  select case (rep)
                  case (rep_a1)
                        print_rep_mult = "A1"
                  case (rep_a2)
                        print_rep_mult = "A2"
                  case (rep_b1)
                        print_rep_mult = "B1"
                  case (rep_b2)
                        print_rep_mult = "B2"
                  end select
            end if

            print_rep_mult = cmult//print_rep_mult

      end function print_rep_mult



      subroutine msg_irrep(s, symm)
            integer, intent(in) :: s
            integer, intent(in) :: symm
            
            if (symm == D2h) then
                  if (s == rep_ag) then
                        call msg("SYMMETRY Ag")
                  else if (s == rep_b1g) then
                        call msg("SYMMETRY B1g")
                  else if (s == rep_b2g) then
                        call msg("SYMMETRY B2g")
                  else if (s == rep_b3g) then
                        call msg("SYMMETRY B3g")
                  else if (s == rep_au) then
                        call msg("SYMMETRY Au")
                  else if (s == rep_b1u) then
                        call msg("SYMMETRY B1u")
                  else if (s == rep_b2u) then
                        call msg("SYMMETRY B2u")
                  else if (s == rep_b3u) then
                        call msg("SYMMETRY B3u")
                  else
                        call msg("SYMMETRY ERROR: POINT GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                        stop   
                  end if
            else if (symm == C2v) then          
                  if (s == rep_a1) then
                        call msg("SYMMETRY A1")
                  else if (s == rep_a2) then
                        call msg("SYMMETRY A2")
                  else if (s == rep_b1) then
                        call msg("SYMMETRY B1")
                  else if (s == rep_b2) then
                        call msg("SYMMETRY B2")       
                  else
                        call msg("SYMMETRY ERROR: POINT GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                        stop   
                  end if
            else
                  call msg("SYMMETRY ERROR: POINT GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop                 
            end if

      end subroutine msg_irrep

      subroutine msg_state_irrep(e, s, symm)
            real(F64), intent(in) :: e
            integer, intent(in)   :: s
            integer, intent(in)   :: symm

            if (symm == D2h) then
                  if (s == rep_ag) then
                        call dmsg("SYMMETRY Ag", e)
                  else if (s == rep_b1g) then
                        call dmsg("SYMMETRY B1g", e)
                  else if (s == rep_b2g) then
                        call dmsg("SYMMETRY B2g", e)
                  else if (s == rep_b3g) then
                        call dmsg("SYMMETRY B3g", e)
                  else if (s == rep_au) then
                        call dmsg("SYMMETRY Au", e)
                  else if (s == rep_b1u) then
                        call dmsg("SYMMETRY B1u", e)
                  else if (s == rep_b2u) then
                        call dmsg("SYMMETRY B2u", e)
                  else if (s == rep_b3u) then
                        call dmsg("SYMMETRY B3u", e)
                  else
                        call msg("SYMMETRY ERROR: NO SUCH REPRESENTATION", MSG_ERROR)
                        stop   
                  end if
            else if (symm == C2v) then          
                  if (s == rep_a1) then
                        call dmsg("SYMMETRY A1", e)
                  else if (s == rep_a2) then
                        call dmsg("SYMMETRY A2", e)
                  else if (s == rep_b1) then
                        call dmsg("SYMMETRY B1", e)
                  else if (s == rep_b2) then
                        call dmsg("SYMMETRY B2", e)       
                  else
                        call msg("SYMMETRY ERROR: NO SUCH REPRESENTATION", MSG_ERROR)
                        stop   
                  end if
            else
                  call msg("SYMMETRY ERROR: POINT GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop                 
            end if

      end subroutine msg_state_irrep

      function rep_char(rep, i, symm)
            integer :: rep_char
            integer, intent(in) :: rep
            integer, intent(in) :: i
            integer, intent(in) :: symm

            rep_char = 1000
            if (symm == D2h) then
                  if (rep == rep_Ag) then
                        rep_char = D2h_Ag(i)
                  else if (rep == rep_B1g) then
                        rep_char = D2h_B1g(i)
                  else if (rep == rep_B2g) then
                        rep_char = D2h_B2g(i)
                  else if (rep == rep_B3g) then
                        rep_char = D2h_B3g(i)
                  else if (rep == rep_Au) then
                        rep_char = D2h_Au(i)
                  else if (rep == rep_B1u) then
                        rep_char = D2h_B1u(i)
                  else if (rep == rep_B2u) then
                        rep_char = D2h_B2u(i)
                  else if (rep == rep_B3u) then
                        rep_char = D2h_B3u(i)
                  end if
            else if (symm == C2v) then
                  if (rep == rep_A1) then
                        rep_char = C2v_A1(i)
                  else if (rep == rep_A2) then
                        rep_char = C2v_A2(i)
                  else if (rep == rep_B1) then
                        rep_char = C2v_B1(i)
                  else if (rep == rep_B2) then
                        rep_char = C2v_B2(i)
                  end if
            else
                  call msg("SYMMETRY ERROR: POINT GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

      end function rep_char

      subroutine aosymmetrize(aosymm, order, symm)
            integer, dimension(:, :), intent(out) :: aosymm
            integer, intent(in) :: order
            integer, intent(in) :: symm

            integer :: i, j, k, l
            integer :: m, s
            real(F64), dimension(3, order) :: rtrans
            real(F64), dimension(:), allocatable :: oext_orb
            
            allocate(oext_orb(CC_NORB))
            call orbext_orb(oext_orb)


            do i = 1, natom
                  rtrans(:, 1) = atomr(:, i)

                  if (symm == C2v) then
                        call C2vtransform(atomr(:, i), rtrans)
                  else if (symm == D2h) then
                        call D2htransform(atomr(:, i), rtrans)
                  else
                        call msg("SYMMETRY ERROR: POINT GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                        stop
                  end if
                  
                  do j = 1, order
                        kloop: do k = 1, natom
                              if (abs(atomr(1, k) - rtrans(1, j)).lt.COORDEPS .and. &
                                    abs(atomr(2, k) - rtrans(2, j)).lt.COORDEPS .and. &
                                    abs(atomr(3, k) - rtrans(3, j)).lt.COORDEPS) then
                                    do l = idx(i), idx(i+1)-1                                          
                                          s = symm_op(j, symm)

                                          call find_ao_phase(l, i, k, s, m, oext_orb(l))
                                          aosymm(l, j) = m
                                    end do
                                    exit kloop
                              end if
                        end do kloop
                  end do
            end do
            deallocate(oext_orb)

      end subroutine aosymmetrize

      subroutine orbext_orb(oext_orb)
            real(F64), dimension(:), intent(out) :: oext_orb

            real(F64), dimension(:), allocatable :: oext_shell

            integer :: i, j
            allocate(oext_shell(nshell))
            call orbext(oext_shell, 1)
            
            do i = 1, nshell
                  do j = shpos(i), shpos(i+1) -1
                        oext_orb(j) = oext_shell(i)
                  end do
            end do

            deallocate(oext_shell)

      end subroutine orbext_orb

      function symm_op(j, symm)
            integer :: symm_op
            integer, intent(in) :: j
            integer, intent(in) :: symm

            symm_op = 0
            if (symm == C2v) then
                  if (j == 1) then
                        symm_op = SYM_E
                  else if (j == 2) then
                        symm_op = SYM_C2z
                  else if (j == 3) then
                        symm_op = SYM_Oxz
                  else if (j == 4) then
                        symm_op = SYM_Oyz
                  end if


            else if (symm == D2h) then
                  if (j == 1) then
                        symm_op = SYM_E
                  else if (j == 2) then
                        symm_op = SYM_C2z
                  else if (j == 3) then
                        symm_op = SYM_C2y
                  else if (j == 4) then
                        symm_op = SYM_C2x
                  else if (j == 5) then
                        symm_op = SYM_I
                  else if (j == 6) then
                        symm_op = SYM_Oxy
                  else if (j == 7) then
                        symm_op = SYM_Oxz
                  else if (j == 8) then
                        symm_op = SYM_Oyz
                  end if
            else
                  call msg("SYMMETRY ERROR: POINT GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if
            
      end function symm_op

      subroutine find_ao_phase(l, i, k, s, idxm, ext)

            ! l AO from i-th atom idx
            ! i atom idx
            ! k atom idx

            integer, intent(in) :: l
            integer, intent(in) :: i
            integer, intent(in) :: k
            integer, intent(in) :: s
            integer, intent(out) :: idxm
            real(F64), intent(in) :: ext         

            integer :: rdim 

            real(F64), dimension(:, :), allocatable :: r
            real(F64), dimension(3)  :: rtrans
            real(F64) :: chi_l, chi_m

            integer :: j, p
            integer :: idxi
            integer :: compp, compm, compz
            integer, parameter :: n = 100
            real(F64) :: scale
            real(F64) :: step

            idxi = -320
            rdim = 26
            allocate(r(3, rdim))
            r = ZERO
            step = ext / dble(n)
            !
            ! Generate candidates for cooridnates
            ! around i-th atom center: ari, where
            ! the ao value is nonzero
            !
            scale = zero
            ploop: do p = 1, n
                  do j = 1, rdim
                        r(:, j) = atomr(:,i)
                  end do
                  scale = scale + step
                  call scalecoords(atomr(:, i), r, scale)
                  !
                  ! Find cooridinates where the value 
                  ! of j-th orbital is greater then EPS
                  ! Search among the candidates
                  !
                  
                  jloop: do j = 1, rdim
                        call aovalueone(chi_l, l, i,  r(1, j), r(2, j), r(3, j))                  
                        if (abs(chi_l) .gt. EPS) then
                              idxi = j
                              exit ploop
                        end if
                  end do jloop
            end do ploop

            ! j -index r który jest niezerowy dla naszego l-tego orbitala na i-tym atomie
            ! wiemy ze jego symetryczny odpowiednik jest na k-tym atomie

            ! nasz atom i-ty
            ! indeks pierwszego orbitala tam idx(i)
            ! indeks aktualnego orbitala to l
            ! ktory to orbital? l - idx(i)   idx(i) = 3  l 8  8-3 = 5 = 6   3 4 5 6 7 
            ! mamy atom k  3 + 5 = 8
            ! indeks pierwszego orbitala na k to idx(k)
            ! indeks naszego szukanego orbitala to idx(k) + (l - idx(i))

            idxm = idx(k) + (l - idx(i))


            ! mamy l-ty i m-ty orbital wybrane teraz trzeba tylko sprawdzic symetrie
            ! w r(:, 1:rdim) mamy zapisane wsp punktow wokol i-tego atomu, w ktorych
            ! policzymy wartosci l-tego orbitala

            compp = 0 
            compm = 0
            compz = 0
            do j = 1, rdim
                  call aovalueone(chi_l, l, i, r(1, j), r(2, j), r(3, j))
                  call coord_trans(r(:, j), rtrans, s)
                  call aovalueone(chi_m, idxm, k, rtrans(1), rtrans(2), rtrans(3))


                  if (abs(chi_l - chi_m) .lt. AOVALEPS .and. abs(chi_l + chi_m) .gt. AOVALEPS) then
                        compp = compp + 1
                  else if (abs(chi_l + chi_m) .lt. AOVALEPS .and. abs(chi_l - chi_m) .gt. AOVALEPS) then
                        compm = compm + 1
                  else if (abs(chi_l - chi_m) .lt. AOVALEPS .and. abs(chi_l + chi_m) .lt. AOVALEPS) then
                        compz = compz + 1
                  end if
            end do
            
            if (compp .gt. compm) then
                  compp = compp + compz
            else if (compm .gt. compp) then
                  compm = compm + compz
            end if

            if (compp == rdim .and. compm == 0) then
                  idxm = idxm
            else if (compp == 0 .and. compm == rdim)then
                  idxm = -idxm
            else
                  idxm = 0
            end if

      end subroutine find_ao_phase

      subroutine scalecoords(a, r, scale)
            real(F64), dimension(:), intent(in) :: a
            real(F64), dimension(:,:), intent(inout) :: r
            real(F64), intent(in) :: scale
            
            r(1, 1) = a(1) + scale
            r(2, 2) = a(2) + scale
            r(3, 3) = a(3) + scale
            r(1, 4) = a(1) - scale
            r(2, 5) = a(2) - scale
            r(3, 6) = a(3) - scale
            
            
            r(1, 7) = a(1) + scale
            r(2, 7) = a(2) + scale

            r(1, 8) = a(1) + scale
            r(2, 8) = a(2) - scale

            r(1, 9) = a(1) - scale
            r(2, 9) = a(2) + scale

            r(1, 10) = a(1) - scale
            r(2, 10) = a(2) - scale


            r(1, 11) = a(1) + scale
            r(3, 11) = a(3) + scale

            r(1, 12) = a(1) + scale
            r(3, 12) = a(3) - scale

            r(1, 13) = a(1) - scale
            r(3, 13) = a(3) + scale

            r(1, 14) = a(1) - scale
            r(3, 14) = a(3) - scale

            r(2, 15) = a(2) + scale
            r(3, 15) = a(3) + scale

            r(2, 16) = a(2) + scale
            r(3, 16) = a(3) - scale

            r(2, 17) = a(2) - scale
            r(3, 17) = a(3) + scale

            r(2, 18) = a(2) - scale
            r(3, 18) = a(3) - scale

            r(1, 19) = a(1) + scale
            r(2, 19) = a(2) + scale
            r(3, 19) = a(3) + scale

            r(1, 20) = a(1) + scale
            r(2, 20) = a(2) + scale
            r(3, 20) = a(3) - scale

            r(1, 21) = a(1) + scale
            r(2, 21) = a(2) - scale
            r(3, 21) = a(3) + scale

            r(1, 22) = a(1) - scale
            r(2, 22) = a(2) + scale
            r(3, 22) = a(3) + scale

            r(1, 23) = a(1) - scale
            r(2, 23) = a(2) - scale
            r(3, 23) = a(3) - scale

            r(1, 24) = a(1) - scale
            r(2, 24) = a(2) + scale
            r(3, 24) = a(3) - scale

            r(1, 25) = a(1) - scale
            r(2, 25) = a(2) - scale
            r(3, 25) = a(3) + scale

            r(1, 26) = a(1) + scale
            r(2, 26) = a(2) - scale
            r(3, 26) = a(3) - scale

      end subroutine scalecoords

      subroutine aovalueone(chi_k, k, i, x, y, z)
            real(F64), intent(out) :: chi_k
            integer,   intent(in) :: k
            integer,   intent(in) :: i
            real(F64), intent(in) :: x
            real(F64), intent(in) :: y
            real(F64), intent(in) :: z

            real(F64), dimension(:), allocatable :: chi
            integer :: chidim
            
            chidim = idx(i+1) - 1 - idx(i) + 1

            allocate(chi(chidim))
            
            call aovalue(chi, i, x, y, z)

            chi_k = chi(k-(idx(i)-1))
            
            deallocate(chi)

      end subroutine aovalueone

      subroutine coord_trans(r, rtrans, s)
            real(F64), dimension(3), intent(in) :: r
            real(F64), dimension(3), intent(out) :: rtrans
            integer, intent(in) :: s

            rtrans = ZERO
            
            if (s == SYM_E) then
                  rtrans = r
            else if (s == SYM_C2z) then
                  call rotate('Z', 2, r, rtrans)
            else if (s == SYM_C2y) then
                  call rotate('Y', 2, r, rtrans)
            else if (s == SYM_C2x) then
                  call rotate('x', 2, r, rtrans)
            else if (s == SYM_I) then
                  call invert(r, rtrans)
            else if (s == SYM_Oxy) then
                  call reflect('XY', r, rtrans)
            else if (s == SYM_Oxz) then
                  call reflect('XZ', r, rtrans)
            else if (s == SYM_Oyz) then
                  call reflect('YZ', r, rtrans)
            end if
            
      end subroutine coord_trans

      subroutine irrep_singles(targ, ipairs, symm)
            integer, intent(in) :: targ
            integer, dimension(:,:), intent(out) :: ipairs
            integer, intent(in) :: symm

            integer, dimension(2) :: input_irreps
            integer :: order
            integer :: i, j, l
            integer :: output_irrep

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if
       
            l = 1
            do i = 1, order
                  do j = 1, order
                        input_irreps(1) = i
                        input_irreps(2) = j

                        call direct_product(input_irreps, symm, output_irrep)
                        
                        if (output_irrep == targ) then

                              ipairs(1, l) = i
                              ipairs(2, l) = j
                              
                              l = l + 1
                        end if
                  end do
            end do
            
      end subroutine irrep_singles

      subroutine irrep_doubles(targ, idoubles, symm, idimd, dim)
            integer, intent(in) :: targ
            integer, dimension(:,:), intent(out) :: idoubles
            integer, intent(in) :: symm
            integer, intent(out) :: idimd
            logical, intent(in) :: dim

            integer, dimension(4) :: input_irreps
            integer :: order
            integer :: i, j, l, k, m
            integer :: output_irrep

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            l = 1
            do i = 1, order
            do j = 1, order
            do k = 1, order
            do m = 1, order
                  input_irreps(1) = i
                  input_irreps(2) = j
                  input_irreps(3) = k
                  input_irreps(4) = m

                  call direct_product(input_irreps, symm, output_irrep)

                  if (output_irrep == targ) then

                        if (dim .eqv. .true.)then
                              l = l + 1
                        else
                        
                              idoubles(1, l) = i
                              idoubles(2, l) = j
                              idoubles(3, l) = k
                              idoubles(4, l) = m
                        end if

                        l = l + 1
                  end if
            end do
            end do
            end do
            end do

            idimd = l - 1

      end subroutine irrep_doubles
      

      subroutine irrep_singless_pq(targ, irrep0, irrep1, isingles, symm, m, dim, q1, q2)
            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:,:), intent(out) :: isingles
            integer, intent(inout) :: m
            logical, intent(in) :: dim
            integer, intent(in) :: q1
            integer, intent(in) :: q2

            integer, dimension(2) :: input_irreps
            integer :: order
            integer :: a, i
            integer :: output_irrep
            integer :: n0i, n0a
            integer :: n1i, n1a

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
                do a = 1, order
                  do i = 1, order
                        input_irreps(1) = a
                        input_irreps(2) = i
                        call direct_product(input_irreps, symm, output_irrep)
                        if (output_irrep == targ) then

                              if (q1 == 1 .and. q2 == 1) then
                                    n0a = irrep0(1, a)
                                    n1a = irrep1(1, a)
                                    
                                    n0i = irrep0(1, i)
                                    n1i = irrep1(1, i)

                              else if (q1 == 2 .and. q2 == 1) then
                                    n0a = irrep0(2, a)
                                    n1a = irrep1(2, a)

                                    n0i = irrep0(1, i)
                                    n1i = irrep1(1, i)

                              else if (q1 == 1 .and. q2 == 2) then

                                    n0a = irrep0(1, a)
                                    n1a = irrep1(1, a)
                                    
                                    n0i = irrep0(2, i)
                                    n1i = irrep1(2, i)

                              else if (q1 == 2 .and. q2 == 2) then
                                    n0a = irrep0(2, a)
                                    n1a = irrep1(2, a)
                                    
                                    n0i = irrep0(2, i)
                                    n1i = irrep1(2, i)

                              end if

                              if (n0i .ne. 0 .and. n0a .ne. 0) then

                                    if (.not. dim) then
                                          isingles(1, m) = a
                                          isingles(2, m) = i
                                    end if

                                    m = m + 1

                              end if
                        end if
                  end do
            end do
            m = m -1

      end subroutine irrep_singless_pq

      subroutine irrep_singless_pq_exc_exc(targ1, targ2, irrep0, irrep1, isingles, symm, m, dim, q1, q2)
            integer, intent(in) :: targ1, targ2
            integer, intent(in) :: symm
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:,:), intent(out) :: isingles
            integer, intent(inout) :: m
            logical, intent(in) :: dim
            integer, intent(in) :: q1
            integer, intent(in) :: q2

            integer, dimension(2) :: input_irreps
            integer :: order
            integer :: a, i
            integer :: output_irrep
            integer :: n0i, n0a
            integer :: n1i, n1a

            integer :: irrep_out1, irrep_out2
            integer, dimension(2) :: work

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
                do a = 1, order
                  do i = 1, order
                        input_irreps(1) = a
                        input_irreps(2) = i
                        call direct_product(input_irreps, symm, output_irrep)
                        work(1) = output_irrep
                        work(2) = targ1
                        call direct_product(work, symm, irrep_out1)
                        work(1) = output_irrep
                        work(2) = targ2
                        call direct_product(work, symm, irrep_out2)
!                        if (output_irrep == targ1 .or. output_irrep == targ2) then
                        if (irrep_out1 == targ2 .or. irrep_out2 == targ1) then

                              if (q1 == 1 .and. q2 == 1) then
                                    n0a = irrep0(1, a)
                                    n1a = irrep1(1, a)
                                    
                                    n0i = irrep0(1, i)
                                    n1i = irrep1(1, i)

                              else if (q1 == 2 .and. q2 == 1) then
                                    n0a = irrep0(2, a)
                                    n1a = irrep1(2, a)

                                    n0i = irrep0(1, i)
                                    n1i = irrep1(1, i)

                              else if (q1 == 1 .and. q2 == 2) then

                                    n0a = irrep0(1, a)
                                    n1a = irrep1(1, a)
                                    
                                    n0i = irrep0(2, i)
                                    n1i = irrep1(2, i)

                              else if (q1 == 2 .and. q2 == 2) then
                                    n0a = irrep0(2, a)
                                    n1a = irrep1(2, a)
                                    
                                    n0i = irrep0(2, i)
                                    n1i = irrep1(2, i)

                              end if

                              if (n0i .ne. 0 .and. n0a .ne. 0) then

                                    if (.not. dim) then
                                          isingles(1, m) = a
                                          isingles(2, m) = i
                                    end if

                                    m = m + 1

                              end if
                        end if
                  end do
            end do
            m = m -1

      end subroutine irrep_singless_pq_exc_exc

      subroutine irrep_singless(targ, irrep0, irrep1, isingles, symm, m, dim)
            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:,:), intent(out) :: isingles
            integer, intent(inout) :: m
            logical, intent(in) :: dim

            integer, dimension(2) :: input_irreps
            integer :: order
            integer :: a, i
            integer :: output_irrep
            integer :: n0i, n0a
            integer :: n1i, n1a

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do i = 1, order
                        input_irreps(1) = a
                        input_irreps(2) = i
                        call direct_product(input_irreps, symm, output_irrep)
                        if (output_irrep == targ) then
                              
                              n0a = irrep0(2, a)
                              n1a = irrep1(2, a)
                              
                              n0i = irrep0(1, i)
                              n1i = irrep1(1, i)

                              if (n0i .ne. 0 .and. n0a .ne. 0) then

                                    if (.not. dim) then
                                          isingles(1, m) = a
                                          isingles(2, m) = i
                                    end if

                                    m = m + 1

                              end if
                        end if
                  end do
            end do
            m = m -1 

      end subroutine irrep_singless

      subroutine irrep_doubless(targ, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles, symm, m, dim, eom, mfold)
            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:,:), intent(out) :: idoubles
            integer, intent(inout) :: m
            logical, intent(in) :: dim
            logical, intent(in) :: eom
            integer, optional, intent(in) :: mfold

            integer, dimension(4) :: input_irreps
            integer :: order
            integer :: a, i, b, j
            integer :: output_irrep
            integer :: n0i, n0j, n0a, n0b
            integer :: n1i, n1j, n1a, n1b
            integer :: ai, bj
            logical :: trippar


            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do i = 1, order
                        do b = 1, order
                              do j = 1, order
                                    input_irreps(1) = a
                                    input_irreps(2) = i
                                    input_irreps(3) = b
                                    input_irreps(4) = j

                                    call direct_product(input_irreps, symm, output_irrep)

                                    if (output_irrep == targ) then

                                          n0a = irrep0(2, a)
                                          n1a = irrep1(2, a)

                                          n0i = irrep0(1, i)
                                          n1i = irrep1(1, i)

                                          n0b = irrep0(2, b)
                                          n1b = irrep1(2, b)

                                          n0j = irrep0(1, j)
                                          n1j = irrep1(1, j)

                                          if (n0i .ne. 0 .and. n0a .ne. 0 .and. n0j .ne. 0 .and. n0b .ne. 0) then

                                                ai = (n1a - nvirt0) * nocc + (n1i - nocc0) + 1
                                                bj = (n0b - nvirt0) * nocc + (n0j - nocc0) + 1
                                                
                                                

                                                if (present(mfold)) then
                                                      if (mfold == 1) then
                                                            trippar = .true.

                                                            if (n1a .le. n0b) then
                                                                  trippar = .false.
                                                            end if
                                                            
                                                            if (n1i .le. n0j) then
                                                                  trippar = .false.
                                                            end if
                                                                                                                       
                                                            if (trippar) then
                                                                  
                                                                  if (.not. dim) then
                                                                        idoubles(1, m) = a
                                                                        idoubles(2, m) = i
                                                                        idoubles(3, m) = b
                                                                        idoubles(4, m) = j
                                                                  end if
                                                                  m = m + 1
                                                            end if
                                                      else if (mfold == -1) then
                                                            
                                                            if (ai .gt. bj) then

                                                                  if (.not. dim) then
                                                                        idoubles(1, m) = a
                                                                        idoubles(2, m) = i
                                                                        idoubles(3, m) = b
                                                                        idoubles(4, m) = j
                                                                  end if
                                                                  m = m + 1
                                                            end if
                                                      end if
                                                else
                                                      if (eom) then
                                                            if (ai .ge. bj) then
                                                                  if (.not. dim) then
                                                                        idoubles(1, m) = a
                                                                        idoubles(2, m) = i
                                                                        idoubles(3, m) = b
                                                                        idoubles(4, m) = j
                                                                  end if

                                                                  m = m + 1

                                                            end if
                                                      else
                                                            if(n0b.le. n1a)then
                                                                  if (.not. dim) then
                                                                        idoubles(1, m) = a
                                                                        idoubles(2, m) = i
                                                                        idoubles(3, m) = b
                                                                        idoubles(4, m) = j
                                                                  end if

                                                                  m = m + 1
                                                            end if
                                                      end if
                                                end if
                                          end if
                                    end if
                              end do
                        end do
                  end do
            end do
            m = m -1 

      end subroutine irrep_doubless

      
      
      subroutine irrep_triples_C1(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: a, b, d, i, j

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do b = 1, order
                        do d = 1, order
                              do i = 1, order
                                    do j = 1, order

                                          call irrep_triples_inner(m, itriples, targ, a, i, b, i, d, j, irrep0, irrep1, &
                                                nocc0, nvirt0, nocc, dim, .false.)
                                    end do
                              end do
                        end do
                  end do
            end do
            m = m - 1

      end subroutine irrep_triples_C1

      subroutine irrep_triples_C2(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: a, b, d, i, j

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do b = 1, order
                        do d = 1, order
                              do i = 1, order
                                    do j = 1, order

                                          call irrep_triples_inner(m, itriples, targ, a, i, b, j, d, j, irrep0, irrep1, &
                                                nocc0, nvirt0, nocc, dim, .false.)
                                    end do
                              end do
                        end do
                  end do
            end do
            m = m - 1

      end subroutine irrep_triples_C2

      subroutine irrep_triples_B1(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: a, b, i, j, l

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do b = 1, order
                        do i = 1, order
                              do j = 1, order
                                    do l = 1, order

                                          call irrep_triples_inner(m, itriples, targ, a, i, a, j, b, l, irrep0, irrep1, &
                                                nocc0, nvirt0, nocc, dim, .false.)
                                    end do
                              end do
                        end do
                  end do
            end do
            m = m - 1

      end subroutine irrep_triples_B1

      subroutine irrep_triples_B2(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: a, b, i, j, l

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do b = 1, order
                        do i = 1, order
                              do j = 1, order
                                    do l = 1, order

                                          call irrep_triples_inner(m, itriples, targ, a, i, b, j, b, l, irrep0, irrep1, &
                                                nocc0, nvirt0, nocc, dim, .false.)
                                    end do
                              end do
                        end do
                  end do
            end do
            m = m - 1

      end subroutine irrep_triples_B2
      

      subroutine irrep_triples_D1(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: a, b, i, j

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do b = 1, order
                        do i = 1, order
                              do j = 1, order
                                    call irrep_triples_inner(m, itriples, targ, a, i, a, i, b, j, irrep0, irrep1, &
                                          nocc0, nvirt0, nocc, dim, .false.)
                              end do
                        end do
                  end do
            end do
            m = m - 1
            
      end subroutine irrep_triples_D1

      subroutine irrep_triples_D2(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: a, b, i, j

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do b = 1, order
                        do i = 1, order
                              do j = 1, order

                                    call irrep_triples_inner(m, itriples, targ, a, i, a, j, b, j, irrep0, irrep1, &
                                          nocc0, nvirt0, nocc, dim, .false.)
                              end do
                        end do
                  end do
            end do
            m = m - 1

      end subroutine irrep_triples_D2
      
      subroutine irrep_triples_D3(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: a, b, i, j

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do b = 1, order
                        do i = 1, order
                              do j = 1, order
                                    call irrep_triples_inner(m, itriples, targ, a, i, b, i, b, j, irrep0, irrep1, &
                                          nocc0, nvirt0, nocc, dim, .false.)
                              end do
                        end do
                  end do
            end do
            m = m - 1

      end subroutine irrep_triples_D3

      subroutine irrep_triples_D4(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: a, b, i, j

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do b = 1, order
                        do i = 1, order
                              do j = 1, order
                                    call irrep_triples_inner(m, itriples, targ, a, i, b, j, b, j, irrep0, irrep1, &
                                          nocc0, nvirt0, nocc, dim, .false.)
                              end do
                        end do
                  end do
            end do
            m = m - 1

      end subroutine irrep_triples_D4

     subroutine irrep_triples_inner_abc(m, itriples_abc, targ, a, b, c, irrep0, irrep1, &
             nocc0, nvirt0, nocc, dim)
             integer, intent(inout) :: m
            integer, dimension(:,:), intent(inout) :: itriples_abc
            integer, intent(in) :: targ
            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: c
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nocc
            logical, intent(in) :: dim

            integer, dimension(3) :: input_irreps
            integer :: output_irrep
            integer :: n0a, n0b, n0c
            integer :: n1a, n1b, n1c
            integer :: a0, b0, aa, bb, cc

            input_irreps(1) = a
            input_irreps(2) = b
            input_irreps(3) = c
            
            call direct_product(input_irreps, POINT_GROUP, output_irrep)

            if (output_irrep == targ) then
                  n0a = irrep0(2, a)
                  n1a = irrep1(2, a)

                  n0b = irrep0(2, b)
                  n1b = irrep1(2, b)

                  n0c = irrep0(2, c)
                  n1c = irrep1(2, c)

                  if (n0a .ne. 0 .and. n0b .ne. 0 .and. n0c .ne. 0) then
                     do cc = n0c, n1c
                              b0 = max(cc + 1, n0b)
                              do bb = b0, n1b
                                    a0 = max(bb + 1, n0a)
                                    do aa = a0, n1a
                                          if (dim)then
                                                m = m + 1
                                          else
                                             m = m + 1
                                                itriples_abc(1, m) = aa
                                                itriples_abc(2, m) = bb
                                                itriples_abc(3, m) = cc                          
                                          end if
                                    end do
                              end do
                        end do
                  end if
               end if
      end subroutine irrep_triples_inner_abc



      subroutine irrep_triples_abc_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_abc, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples_abc
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: a, i, b, j, c, k

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
               call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
               stop
            end if

            if (dim) then
               m = 0
            end if

            do a = 1, order
                  do b = 1, order
                        do c = 1, order
                              call irrep_triples_inner_abc(m, itriples_abc, targ, a, b, c, irrep0, irrep1, &
                                    nocc0, nvirt0, nocc, dim)
                        end do
                  end do
            end do

      end subroutine irrep_triples_abc_targ


      subroutine irrep_triples_abc(nbounds_abc, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_abc, symm, m, dim)

            integer, dimension(:, :), intent(inout) :: nbounds_abc
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples_abc
            integer, intent(inout) :: m
            logical, intent(in)    :: dim
            integer :: targ

            integer :: order, m_targ
            integer :: a, i, b, j, c, k

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            if (dim) then
                  m = 0
                  m_targ = 0
                  do targ = 1, order
                     call irrep_triples_abc_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_abc, symm, m_targ, dim)
                     nbounds_abc(1, targ) = m + 1
                     nbounds_abc(2, targ) = m + m_targ
                     m = m + m_targ
                  end do
            else
               do targ = 1, order
                  m_targ = nbounds_abc(1, targ) - 1
                  call irrep_triples_abc_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_abc, symm, m_targ, dim)
                  end do
            end if

          end subroutine irrep_triples_abc


       subroutine irrep_triples_inner_aac(m, itriples_aac, targ, a, c, irrep0, irrep1, &
             nocc0, nvirt0, nocc, dim, cs)
             integer, intent(inout) :: m
            integer, dimension(:,:), intent(inout) :: itriples_aac
            integer, intent(in) :: targ
            integer, intent(in) :: a
            integer, intent(in) :: c
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nocc
            logical, intent(in) :: dim
            integer, intent(in) :: cs

            integer, dimension(3) :: input_irreps
            integer :: output_irrep
            integer :: n0a, n0b, n0c
            integer :: n1a, n1b, n1c
            integer :: a0, b0, aa, bb, cc

            if (cs == 3 .or. cs == 4 .or. cs ==7 .or. cs == 8) then !z34, z7, z8,  aac a>c
               input_irreps(1) = a
               input_irreps(2) = a
               input_irreps(3) = c
            else if (cs == 5 .or. cs == 6 .or. cs ==9 .or. cs == 10) then !z56, z9, z10,  abb a>b
               input_irreps(1) = a
               input_irreps(2) = c
               input_irreps(3) = c
            end if
            
            call direct_product(input_irreps, POINT_GROUP, output_irrep)

            if (output_irrep == targ) then
                  n0a = irrep0(2, a)
                  n1a = irrep1(2, a)

                  n0c = irrep0(2, c)
                  n1c = irrep1(2, c)

                  if (n0a .ne. 0 .and. n0c .ne. 0) then
                     if (cs == 3 .or. cs == 4 .or. cs ==7 .or. cs == 8) then
                        do cc = n0c, n1c
                           a0 = max(cc + 1, n0a)
                           do aa = a0, n1a
                              if (dim)then
                                 m = m + 1
                              else
                                 m = m + 1
                                 itriples_aac(1, m) = aa
                                 itriples_aac(2, m) = aa
                                 itriples_aac(3, m) = cc                          
                              end if
                           end do
                        end do
                     else if (cs == 5 .or. cs == 6 .or. cs ==9 .or. cs == 10) then
                        do cc = n0c, n1c
                           a0 = max(cc + 1, n0a)
                           do aa = a0, n1a
                              if (dim)then
                                 m = m + 1
                              else
                                 m = m + 1
                                 itriples_aac(1, m) = aa
                                 itriples_aac(2, m) = cc
                                 itriples_aac(3, m) = cc
                              end if
                           end do
                        end do
                     end if
                  end if
               end if

                     
      end subroutine irrep_triples_inner_aac



      subroutine irrep_triples_aac_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_aac, symm, m, dim, cs)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples_aac
            integer, intent(inout) :: m
            logical, intent(in)    :: dim
            integer, intent(in) :: cs
            
            integer :: order
            integer :: a, c

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
               call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
               stop
            end if

            if (dim) then
               m = 0
            end if

            do a = 1, order
               do c = 1, order
                  call irrep_triples_inner_aac(m, itriples_aac, targ, a, c, irrep0, irrep1, &
                       nocc0, nvirt0, nocc, dim, cs)
               end do
            end do

      end subroutine irrep_triples_aac_targ


      subroutine irrep_triples_aac(nbounds_aac, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_aac, symm, m, dim, cs)

            integer, dimension(:, :), intent(inout) :: nbounds_aac
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples_aac
            integer, intent(inout) :: m
            logical, intent(in)    :: dim
            integer, intent(in) :: cs
            integer :: targ

            integer :: order, m_targ
            integer :: a, i, b, j, c, k

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            if (dim) then
                  m = 0
                  m_targ = 0
                  do targ = 1, order
                     call irrep_triples_aac_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_aac, symm, m_targ, dim, cs)
                     nbounds_aac(1, targ) = m + 1
                     nbounds_aac(2, targ) = m + m_targ
                     m = m + m_targ
                  end do
            else
               do targ = 1, order
                  m_targ = nbounds_aac(1, targ) - 1
                  call irrep_triples_aac_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_aac, symm, m_targ, dim, cs)
                  end do
            end if

      end subroutine irrep_triples_aac

     subroutine irrep_triples_inner_iji(m, itriples_iji, targ, i, j, irrep0, irrep1, &
             nocc0, nvirt0, nocc, dim, cs)
             integer, intent(inout) :: m
            integer, dimension(:,:), intent(inout) :: itriples_iji
            integer, intent(in) :: targ
            integer, intent(in) :: i, j
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nocc
            logical, intent(in) :: dim
            integer, intent(in) :: cs
            
            integer, dimension(3) :: input_irreps
            integer :: output_irrep
            integer :: n0i, n0j
            integer :: n1i, n1j
            integer :: i0, ii, jj, i1

            if (cs == z1 .or. cs == z7 .or. cs == z9) then ! z1 iji and z7 iji i > j and z9 iji i<j
               input_irreps(1) = i
               input_irreps(2) = j
               input_irreps(3) = i
            else if (cs == z2 .or. cs == z8) then ! z2 ijj and z8 ijj i>j 
               input_irreps(1) = i
               input_irreps(2) = j
               input_irreps(3) = j
            else if (cs == z10) then ! z10 iij i>j             
               input_irreps(1) = i
               input_irreps(2) = i
               input_irreps(3) = j               
            end if

            call direct_product(input_irreps, POINT_GROUP, output_irrep)

            if (output_irrep == targ) then
               n0i = irrep0(1, i)
               n1i = irrep1(1, i)
               
               n0j = irrep0(1, j)
               n1j = irrep1(1, j)

               if (n0i .ne. 0 .and. n0j .ne. 0) then
                  if (cs == z1) then ! z1 aibjcia>b>c (iji) i = k, j!=i                     
                     do jj = n0j, n1j
                        iloop: do ii = n0i, n1i
                           if (ii == jj) cycle iloop
                           if (dim)then
                              m = m + 1
                           else
                              m = m + 1
                              itriples_iji(1, m) = ii
                              itriples_iji(2, m) = jj
                           end if
                        end do iloop
                     end do
                  else if (cs == z2) then ! z2 aibjcj  a>b>c (ijj) j = k, j!=i
                     do jj = n0j, n1j
                        iloop2: do ii = n0i, n1i
                           if (ii == jj) cycle iloop2
                           if (dim)then
                              m = m + 1
                           else
                              m = m + 1
                              itriples_iji(1, m) = ii
                              itriples_iji(2, m) = jj
                           end if
                        end do iloop2
                     end do
                  else if (cs == z7) then ! z7 aac (iji) i = k, i > j
                     do jj = n0j, n1j
                        i0 = max(n0i, jj+1)
                        do ii = i0, n1i
                           if (dim)then
                              m = m + 1
                           else
                              m = m + 1
                              itriples_iji(1, m) = ii
                              itriples_iji(2, m) = jj
                           end if
                        end do
                     end do
                  else if (cs == z8) then ! z8 aac (ijj) j = k, i > j
                     do jj = n0j, n1j
                        i0 = max(n0i, jj+1)
                        do ii = i0, n1i
                           if (dim)then
                              m = m + 1
                           else
                              m = m + 1
                              itriples_iji(1, m) = ii
                              itriples_iji(2, m) = jj
                           end if
                        end do
                     end do
                  else if (cs == z9) then ! z9 abb (iji) a>b, i<j
                     do jj = n0j, n1j
                        i1 = min(n1i, jj-1)
                        do ii = n0i, i1
                           if (dim)then
                              m = m + 1
                           else
                              m = m + 1
                              itriples_iji(1, m) = ii
                              itriples_iji(2, m) = jj
                           end if
                        end do
                     end do
                  else if (cs == z10) then ! z10 abb (iij) a>b, i>j
                     do jj = n0j, n1j
                        i0 = max(n0i, jj+1)
                        do ii = i0, n1i
                           if (dim)then
                              m = m + 1
                           else
                              m = m + 1
                              itriples_iji(1, m) = ii
                              itriples_iji(2, m) = jj
                           end if
                        end do
                     end do
                  end if
               end if
            end if
       end subroutine irrep_triples_inner_iji

      subroutine irrep_triples_iji_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_iji, symm, m, dim, cs)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples_iji
            integer, intent(inout) :: m
            logical, intent(in)    :: dim
            integer, intent(in) :: cs
            
            integer :: order
            integer :: i, j

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
               call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
               stop
            end if
            
            if (dim) then
               m = 0
            end if


            do i = 1, order
                  do j = 1, order
                        call irrep_triples_inner_iji(m, itriples_iji, targ, i, j, irrep0, irrep1, &
                              nocc0, nvirt0, nocc, dim, cs)
                  end do
            end do
            
      end subroutine irrep_triples_iji_targ


      subroutine irrep_triples_iji(nbounds_iji, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_iji, symm, m, dim, cs)

            integer, dimension(:, :), intent(inout) :: nbounds_iji
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(inout)::   itriples_iji
            integer, intent(inout) :: m
            logical, intent(in)    :: dim
            integer :: targ

            integer :: order, m_targ
            integer :: a, i, b, j, c, k
            integer :: cs

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            if (dim) then
               m = 0
               m_targ = 0
               do targ = 1, order
                  call irrep_triples_iji_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_iji, symm, m_targ, dim, cs)
                        nbounds_iji(1, targ) = m + 1
                        nbounds_iji(2, targ) = m + m_targ
                        m = m + m_targ
                  end do
            else
                  do targ = 1, order
                        m_targ = nbounds_iji(1, targ) - 1
                        call irrep_triples_iji_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_iji, symm, m_targ, dim, cs)
                  end do
               end if

     end subroutine irrep_triples_iji

     !-------------------------------------------------------z34a--------------------------------------------------------

     subroutine irrep_triples_inner_ijk(m, itriples_ijk, targ, i, j, k, irrep0, irrep1, &
             nocc0, nvirt0, nocc, dim, cs)
             integer, intent(inout) :: m
            integer, dimension(:,:), intent(inout) :: itriples_ijk
            integer, intent(in) :: targ
            integer, intent(in) :: i, j, k
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nocc
            logical, intent(in) :: dim
            integer, intent(in) :: cs
            
            integer, dimension(3) :: input_irreps
            integer :: output_irrep
            integer :: n0i, n0j
            integer :: n1i, n1j
            integer :: n0k, n1k
            integer :: i0, j1, i1, j0, ii, jj, kk

            input_irreps(1) = i
            input_irreps(2) = j
            input_irreps(3) = k

            call direct_product(input_irreps, POINT_GROUP, output_irrep)

            if (output_irrep == targ) then
               n0i = irrep0(1, i)
               n1i = irrep1(1, i)
               
               n0j = irrep0(1, j)
               n1j = irrep1(1, j)

               n0k = irrep0(1, k)
               n1k = irrep1(1, k)

               
               if (n0i .ne. 0 .and. n0j .ne. 0 .and. n0k .ne. 0) then

                  if (cs == z0) then ! z34 aibjck abc (ijk) a>b>c, i>j>k

                     do kk = n0k, n1k
                           j0 = max(kk+1, n0j)
                           do jj = j0, n1j
                                 i0 = max(jj+1, n0i)                                 
                                 do ii = i0, n1i
                                       if (dim)then
                                             m = m + 1
                                       else
                                             m = m + 1
                                             itriples_ijk(1, m) = ii
                                             itriples_ijk(2, m) = jj
                                             itriples_ijk(3, m) = kk
                                       end if
                                 end do
                           end do
                     end do
                     

               else if (cs == z3) then ! z34 aiajck aac (ijk) a>c, j < k, i > (j, k)
                     
                     do kk = n0k, n1k
                        j1 = min(kk - 1, n1j)
                        do jj = n0j, j1
                           i0 = max(n0i, kk+1, jj+1)
                           do ii = i0, n1i
                              if (dim)then
                                 m = m + 1
                              else
                                 m = m + 1
                                 itriples_ijk(1, m) = ii
                                 itriples_ijk(2, m) = jj
                                 itriples_ijk(3, m) = kk
                              end if
                           end do
                        end do
                     end do

                  else if (cs == z4) then ! z34 aiajck  aac (ijk) a>c, j < k, i > j, i < k
                     
                     do kk = n0k, n1k
                        j1 = min(kk - 1, n1j)
                        do jj = n0j, j1
                           i0 = max(n0i, jj+1)
                           i1 = min(n1i, kk-1)
                           do ii = i0, i1
                              if (dim)then
                                 m = m + 1
                              else
                                 m = m + 1
                                 itriples_ijk(1, m) = ii
                                 itriples_ijk(2, m) = jj
                                 itriples_ijk(3, m) = kk
                              end if
                           end do
                        end do
                     end do

                  else if (cs == z5) then !z5 abb (ijk) a>b, j > k, i > k, i < j

                     do kk = n0k, n1k
                        j0 = max(kk + 1, n0j)
                        do jj = j0, n1j
                           i0 = max(n0i, kk+1)
                           i1 = min(n1i, jj-1)
                           do ii = i0, i1
                              if (dim)then
                                 m = m + 1
                              else
                                 m = m + 1
                                 itriples_ijk(1, m) = ii
                                 itriples_ijk(2, m) = jj
                                 itriples_ijk(3, m) = kk
                              end if
                           end do
                        end do
                     end do

                  else if (cs == z6) then ! z6 abb (ijk) a>b, j>k, i<j, i<k 
                     do kk = n0k, n1k
                        j0 = max(kk + 1, n0j)
                        do jj = j0, n1j
                           i1 = min(n1i, jj-1, kk-1)
                           do ii = n0i, i1
                              if (dim)then
                                 m = m + 1
                              else
                                 m = m + 1
                                 itriples_ijk(1, m) = ii
                                 itriples_ijk(2, m) = jj
                                 itriples_ijk(3, m) = kk
                              end if
                           end do
                        end do
                     end do
                  end if
               end if
            end if

      end subroutine irrep_triples_inner_ijk



      subroutine irrep_triples_ijk_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_ijk, symm, m, dim, cs)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples_ijk
            integer, intent(inout) :: m
            logical, intent(in)    :: dim
            integer, intent(in) :: cs
            
            integer :: order
            integer :: i, j, k

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
               call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
               stop
            end if
            
            if (dim) then
               m = 0
            end if


            do i = 1, order
               do j = 1, order
                  do k = 1, order
                     call irrep_triples_inner_ijk(m, itriples_ijk, targ, i, j, k, irrep0, irrep1, &
                          nocc0, nvirt0, nocc, dim, cs)
                  end do
               end do
            end do
            
      end subroutine irrep_triples_ijk_targ

      
      subroutine irrep_triples_ijk(nbounds_ijk, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_ijk, symm, m, dim, cs)

            integer, dimension(:, :), intent(inout) :: nbounds_ijk
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(inout)::   itriples_ijk
            integer, intent(inout) :: m
            logical, intent(in)    :: dim
            integer :: targ

            integer :: order, m_targ
            integer :: i, j, k
            integer :: cs

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            if (dim) then
               m = 0
               m_targ = 0
               do targ = 1, order
                  call irrep_triples_ijk_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_ijk, symm, m_targ, dim, cs)
                        nbounds_ijk(1, targ) = m + 1
                        nbounds_ijk(2, targ) = m + m_targ
                        m = m + m_targ
                  end do
            else
                  do targ = 1, order
                        m_targ = nbounds_ijk(1, targ) - 1
                        call irrep_triples_ijk_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples_ijk, symm, m_targ, dim, cs)
                  end do
               end if

     end subroutine irrep_triples_ijk


     subroutine irrep_doubles_inner_ij(m, idoubles_ij, targ, i, j, irrep0, irrep1, &
             nocc0, nvirt0, nocc, dim)
             integer, intent(inout) :: m
            integer, dimension(:,:), intent(inout) :: idoubles_ij
            integer, intent(in) :: targ
            integer, intent(in) :: i, j
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nocc
            logical, intent(in) :: dim

            integer, dimension(2) :: input_irreps
            integer :: output_irrep
            integer :: n0i, n0j
            integer :: n1i, n1j
            integer :: i0, ii, jj

            input_irreps(1) = i
            input_irreps(2) = j

            call direct_product(input_irreps, POINT_GROUP, output_irrep)

            if (output_irrep == targ) then
               n0i = irrep0(1, i)
               n1i = irrep1(1, i)
               
               n0j = irrep0(1, j)
               n1j = irrep1(1, j)
               
                  if (n0i .ne. 0 .and. n0j .ne. 0) then
                     do jj = n0j, n1j
                              iloop: do ii = n0i, n1i
                                 if (dim)then
                                    m = m + 1
                                 else
                                    m = m + 1
                                    idoubles_ij(1, m) = ii
                                    idoubles_ij(2, m) = jj
                                 end if
                              end do iloop
                           end do
                        end if
                     end if
      end subroutine irrep_doubles_inner_ij



      subroutine irrep_doubles_ij_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles_ij, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   idoubles_ij
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: i, j

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
               call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
               stop
            end if
            
            if (dim) then
               m = 0
            end if


            do i = 1, order
               do j = 1, order
                  call irrep_doubles_inner_ij(m, idoubles_ij, targ, i, j, irrep0, irrep1, &
                       nocc0, nvirt0, nocc, dim)
               end do
            end do
            
      end subroutine irrep_doubles_ij_targ


      subroutine irrep_doubles_ij(nbounds_ij, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles_ij, symm, m, dim)

            integer, dimension(:, :), intent(inout) :: nbounds_ij
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(inout)::   idoubles_ij
            integer, intent(inout) :: m
            logical, intent(in)    :: dim
            integer :: targ

            integer :: order, m_targ
            integer :: a, i, b, j, c, k

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            if (dim) then
               m = 0
               m_targ = 0
               do targ = 1, order
                  call irrep_doubles_ij_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles_ij, symm, m_targ, dim)
                  nbounds_ij(1, targ) = m + 1
                  nbounds_ij(2, targ) = m + m_targ
                  m = m + m_targ
               end do
            else
               do targ = 1, order
                  m_targ = nbounds_ij(1, targ) - 1
                  call irrep_doubles_ij_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles_ij, symm, m_targ, dim)
               end do
            end if
            
          end subroutine irrep_doubles_ij




             subroutine irrep_doubles_inner_ab(m, idoubles_ab, targ, a, b, irrep0, irrep1, &
                  nocc0, nvirt0, nocc, dim)
               integer, intent(inout) :: m
               integer, dimension(:,:), intent(inout) :: idoubles_ab
               integer, intent(in) :: targ
               integer, intent(in) :: a
               integer, intent(in) :: b
               integer, dimension(:,:), intent(in) :: irrep0
               integer, dimension(:,:), intent(in) :: irrep1
               integer, intent(in) :: nocc0
               integer, intent(in) :: nvirt0
               integer, intent(in) :: nocc
               logical, intent(in) :: dim

               integer, dimension(2) :: input_irreps
               integer :: output_irrep
               integer :: n0a, n0b
               integer :: n1a, n1b
               integer :: a0, b0, aa, bb

               input_irreps(1) = a
               input_irreps(2) = b

               call direct_product(input_irreps, POINT_GROUP, output_irrep)

               if (output_irrep == targ) then
                  n0a = irrep0(2, a)
                  n1a = irrep1(2, a)

                  n0b = irrep0(2, b)
                  n1b = irrep1(2, b)

                  if (n0a .ne. 0 .and. n0b .ne. 0 ) then
                        do bb = n0b, n1b
                           a0 = max(bb, n0a)
                           do aa = a0, n1a
                              if (dim)then
                                 m = m + 1
                              else
                                 m = m + 1
                                 idoubles_ab(1, m) = aa
                                 idoubles_ab(2, m) = bb
                              end if
                           end do
                        end do
                     end if
                  end if
                end subroutine irrep_doubles_inner_ab

             subroutine irrep_doubles_ab_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles_ab, symm, m, dim)

               integer, intent(in) :: targ
               integer, intent(in) :: symm
               integer, intent(in) :: nocc0
               integer, intent(in) :: nocc
               integer, intent(in) :: nvirt0
               integer, dimension(:, :), intent(in) :: irrep0
               integer, dimension(:, :), intent(in) :: irrep1
               integer, dimension(:, :), intent(out)::   idoubles_ab
               integer, intent(inout) :: m
               logical, intent(in)    :: dim

               integer :: order
               integer :: a, b

               order = -1
               if (symm == C2v) then
                  order = C2v_order
               else if (symm == D2h) then
                  order = D2h_order
               else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
               end if

               if (dim) then
                  m = 0
               end if

               do a = 1, order
                  do b = 1, order
                     call irrep_doubles_inner_ab(m, idoubles_ab, targ, a, b, irrep0, irrep1, &
                          nocc0, nvirt0, nocc, dim)
                  end do
               end do
             end subroutine irrep_doubles_ab_targ


             subroutine irrep_doubles_ab(nbounds_ab, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles_ab, symm, m, dim)

               integer, dimension(:, :), intent(inout) :: nbounds_ab
               integer, intent(in) :: symm
               integer, intent(in) :: nocc0
               integer, intent(in) :: nocc
               integer, intent(in) :: nvirt0
               integer, dimension(:, :), intent(in) :: irrep0
               integer, dimension(:, :), intent(in) :: irrep1
               integer, dimension(:, :), intent(out)::   idoubles_ab
               integer, intent(inout) :: m
               logical, intent(in)    :: dim
               integer :: targ
               integer :: order, m_targ
               integer :: a, b

               order = -1
               if (symm == C2v) then
                  order = C2v_order
               else if (symm == D2h) then
                  order = D2h_order
               else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
               end if

               if (dim) then
                  m = 0
                  m_targ = 0
                  do targ = 1, order
                     call irrep_doubles_ab_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles_ab, symm, m_targ, dim)
                     nbounds_ab(1, targ) = m + 1
                     nbounds_ab(2, targ) = m + m_targ
                     m = m + m_targ
                  end do
               else
                  do targ = 1, order
                     m_targ = nbounds_ab(1, targ) - 1
                     call irrep_doubles_ab_targ(targ, irrep0, irrep1, nocc0, nocc, nvirt0, idoubles_ab, symm, m_targ, dim)
                  end do
               end if
             end subroutine irrep_doubles_ab



      subroutine target_pairs(symm, targ, target_pair_idx)

            integer, intent(in) :: symm
            integer, intent(in) :: targ
            integer, dimension(:, :), intent(out) :: target_pair_idx
            integer :: i, j
            integer :: order
            integer, dimension(2) :: input_irreps
            integer :: output_irrep
            integer :: m

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do i = 1, order
                  do j = 1, order

                        input_irreps(1) = i
                        input_irreps(2) = j

                        call direct_product(input_irreps, POINT_GROUP, output_irrep)

                        if (output_irrep == targ) then
                              target_pair_idx(1, m) = i
                              target_pair_idx(2, m) = j
                              m = m + 1
                        end if
                              
                  end do
            end do
            
            
      end subroutine target_pairs


      

      subroutine irrep_triples_inner(m, itriples, targ, a, i, b, j, c, k, irrep0, irrep1, &
             nocc0, nvirt0, nocc, dim, all)
             integer, intent(inout) :: m
            integer, dimension(:,:), intent(inout) :: itriples
            integer, intent(in) :: targ
            integer, intent(in) :: a
            integer, intent(in) :: i
            integer, intent(in) :: b
            integer, intent(in) :: j
            integer, intent(in) :: c
            integer, intent(in) :: k
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            integer, intent(in) :: nocc0
            integer, intent(in) :: nvirt0
            integer, intent(in) :: nocc
            logical, intent(in) :: dim
            logical, intent(in) :: all      

            integer, dimension(6) :: input_irreps
            integer :: output_irrep
            integer :: n0i, n0j, n0k, n0a, n0b, n0c
            integer :: n1i, n1j, n1k, n1a, n1b, n1c
            integer :: ai, bjs, bjl, ck

            input_irreps(1) = a
            input_irreps(2) = i
            input_irreps(3) = b
            input_irreps(4) = j
            input_irreps(5) = c
            input_irreps(6) = k

            call direct_product(input_irreps, POINT_GROUP, output_irrep)

            if (output_irrep == targ) then

                  n0a = irrep0(2, a)
                  n1a = irrep1(2, a)

                  n0i = irrep0(1, i)
                  n1i = irrep1(1, i)

                  n0b = irrep0(2, b)
                  n1b = irrep1(2, b)

                  n0j = irrep0(1, j)
                  n1j = irrep1(1, j)

                  n0c = irrep0(2, c)
                  n1c = irrep1(2, c)

                  n0k = irrep0(1, k)
                  n1k = irrep1(1, k)

!                  write(*, '(12I5)') n0a, n1a, n0i, n1i, n0b, n1b, n0j, n1j, n0c, n1c, n0k, n1k

                  if (n0i .ne. 0 .and. n0a .ne. 0 .and. n0j&
                        .ne. 0 .and. n0b .ne. 0 &
                        .and. n0k .ne. 0 .and. n0c .ne. 0) then

                        ai  = (n1a - nvirt0) * nocc + (n1i - nocc0) + 1
                        bjs = (n0b - nvirt0) * nocc + (n0j - nocc0) + 1

                        bjl = (n1b - nvirt0) * nocc + (n1j - nocc0) + 1
                        ck  = (n0c - nvirt0) * nocc + (n0k - nocc0) + 1
 !                       print*, ai, bjs, bjl, ck
                        if (.not. all) then
  !                            print*, 'yes1'
                              if (ai .ge. bjs .and. bjl .ge. ck) then
   !                                 print*, 'yes2'
                                    if (.not. dim) then
                                          itriples(1, m) = a
                                          itriples(2, m) = i
                                          itriples(3, m) = b
                                          itriples(4, m) = j
                                          itriples(5, m) = c
                                          itriples(6, m) = k
                                    end if
                                    m = m + 1
                              end if
                        else
                              if (.not. dim) then
                                    itriples(1, m) = a
                                    itriples(2, m) = i
                                    itriples(3, m) = b
                                    itriples(4, m) = j
                                    itriples(5, m) = c
                                    itriples(6, m) = k
                              end if
                              m = m + 1
                        end if
                  end if
            end if
      end subroutine irrep_triples_inner


      subroutine irrep_triples(targ, irrep0, irrep1, nocc0, nocc, nvirt0, itriples, symm, m, dim, all)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples
            integer, intent(inout) :: m
            logical, intent(in)    :: dim
            logical, intent(in)    :: all

            integer :: order
            integer :: a, i, b, j, c, k

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do i = 1, order
                        do b = 1, order
                              do j = 1, order
                                    do c = 1, order
                                          do k = 1, order

                                                call irrep_triples_inner(m, itriples, targ, a, i, b, j, c, k, irrep0, irrep1, &
                                                      nocc0, nvirt0, nocc, dim, all)
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
            m = m - 1

      end subroutine irrep_triples

      subroutine irrep_triples_doubles(targ, idoubles, gidimd, &
            irrep0, irrep1, nocc0, nocc, nvirt0, &
            gitd1, gitd1_dim, gitd2, gitd2_dim, gitd3, gitd3_dim, &
            gitd1_idx, gitd2_idx, gitd3_idx, rev)

            integer, intent(in) :: targ
            integer, dimension(:,:), intent(in) :: idoubles
            integer, intent(in) :: gidimd
            integer, intent(in) :: nocc0
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:,:), allocatable, intent(out) :: gitd1, gitd2, gitd3
            integer, intent(out) :: gitd1_dim, gitd2_dim, gitd3_dim
            integer, dimension(:, :), allocatable, intent(out) :: gitd1_idx, gitd2_idx, gitd3_idx
            logical, intent(in) :: rev
            integer :: i1, i2, i3

            integer :: p, q, r
            logical :: add
            integer, dimension(3) :: work1, work2, work3, work4
            integer :: full_sym
            integer, dimension(:, :), allocatable :: itriples
            integer :: gidimt

            if (POINT_GROUP == D2h) then
                  full_sym = REP_Ag
            else if (POINT_GROUP == C2v) then
                  full_sym = REP_A1
            end if

            call irrep_triples(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  itriples, POINT_GROUP, gidimt, .true., all = .true.)
            allocate(itriples(6, gidimt))
            call irrep_triples(full_sym, irrep0, irrep1, nocc0, nocc, nvirt0, &
                  itriples, POINT_GROUP, gidimt, .false., all = .true.)

            gitd1_dim = 0
            gitd2_dim = 0
            gitd3_dim = 0

            allocate(gitd1_idx(gidimd, 2))
            allocate(gitd2_idx(gidimd, 2))
            allocate(gitd3_idx(gidimd, 2))


            print*, idoubles(:, 1)

            do p = 1, gidimd
                  
                  if (rev) then
                        work1(1) = idoubles(1, p)
                        work1(2) = idoubles(4, p)
                        work1(3) = idoubles(2, p)
                  else
                        work1(1) = idoubles(3, p)
                        work1(2) = idoubles(2, p)
                        work1(3) = idoubles(4, p)
                  end if

                  do q = 1, gidimt
                        work2(1) = itriples(1, q)
                        work2(2) = itriples(4, q)
                        work2(3) = itriples(2, q)

                        work3(1) = itriples(1, q)
                        work3(2) = itriples(4, q)
                        work3(3) = itriples(6, q)

                        work4(1) = itriples(1, q)
                        work4(2) = itriples(6, q)
                        work4(3) = itriples(2, q)

                        if (all(work2 == work1)) gitd1_dim = gitd1_dim + 1
                        if (all(work3 == work1)) gitd2_dim = gitd2_dim + 1
                        if (all(work4 == work1)) gitd3_dim = gitd3_dim + 1
                  end do
            end do

            allocate(gitd1(3, gitd1_dim))
            allocate(gitd2(3, gitd2_dim))
            allocate(gitd3(3, gitd3_dim))

            print*, gitd1_dim, gitd2_dim, gitd3_dim
            print*, ''
            gitd1_dim = 0
            gitd2_dim = 0
            gitd3_dim = 0

            i1 = 0
            i2 = 0
            i3 = 0

            gitd1_idx = 0
            gitd2_idx = 0
            gitd3_idx = 0


            do p = 1, gidimd

                  if (rev) then
                        work1(1) = idoubles(1, p)
                        work1(2) = idoubles(4, p)
                        work1(3) = idoubles(2, p)
                  else
                        work1(1) = idoubles(3, p)
                        work1(2) = idoubles(2, p)
                        work1(3) = idoubles(4, p)
                  end if

                  if (p == 1) then
                        gitd1_idx(p, 1) = 1
                        gitd2_idx(p, 1) = 1
                        gitd3_idx(p, 1) = 1
                  else 
                        gitd1_idx(p, 1) = gitd1_idx(p-1, 2) + 1
                        gitd2_idx(p, 1) = gitd2_idx(p-1, 2) + 1
                        gitd3_idx(p, 1) = gitd3_idx(p-1, 2) + 1
                  end if

                  do q = 1, gidimt
                        work2(1) = itriples(1, q)
                        work2(2) = itriples(4, q)
                        work2(3) = itriples(2, q)

                        work3(1) = itriples(1, q)
                        work3(2) = itriples(4, q)
                        work3(3) = itriples(6, q)

                        work4(1) = itriples(1, q)
                        work4(2) = itriples(6, q)
                        work4(3) = itriples(2, q)

                        if (all(work2 == work1)) then
                              gitd1_dim = gitd1_dim + 1
                              gitd1(1, gitd1_dim) = itriples(3, p)
                              gitd1(2, gitd1_dim) = itriples(6, p)
                              gitd1(3, gitd1_dim) = itriples(5, p)
                              add = .true.
                              do r = 1, gitd1_dim-1
                                    if (all(gitd1(:, gitd1_dim) == gitd1(:, r))) then
                                          add = .false.
                                    end if
                              end do
                              if (add .eqv. .false.) then
                                    gitd1_dim = gitd1_dim - 1
                              else
                                    i1 = i1 + 1
                              end if
                        end if
                        if (all(work3 == work1)) then
                              gitd2_dim = gitd2_dim + 1
                              gitd2(1, gitd2_dim) = itriples(3, p)
                              gitd2(2, gitd2_dim) = itriples(2, p)
                              gitd2(3, gitd2_dim) = itriples(5, p)
                              add = .true.
                              do r = 1, gitd2_dim-1
                                    if (all(gitd2(:, gitd2_dim) == gitd2(:, r))) then
                                          add = .false.
                                    end if
                              end do
                              if (add .eqv. .false.) then
                                    gitd2_dim = gitd2_dim - 1
                              else
                                    i2 = i2 + 1
                              end if
                        end if
                        if (all(work4 == work1)) then
                              gitd3_dim = gitd3_dim + 1
                              gitd3(1, gitd3_dim) = itriples(3, p)
                              gitd3(2, gitd3_dim) = itriples(4, p)
                              gitd3(3, gitd3_dim) = itriples(5, p)
                              add = .true.
                              do r = 1, gitd3_dim-1
                                    if (all(gitd3(:, gitd3_dim) == gitd3(:, r))) then
                                          add = .false.
                                    end if
                              end do
                              if (add .eqv. .false.) then
                                    gitd3_dim = gitd3_dim - 1
                              else
                                    i3 = i3 + 1
                              end if
                        end if

                  end do
                  gitd1_idx(p, 2) = gitd1_idx(p, 1) + i1 - 1
                  i1 = 0
                  
                  gitd2_idx(p, 2) = gitd2_idx(p, 1) + i2 - 1
                  i2 = 0

                  gitd3_idx(p, 2) = gitd3_idx(p, 1) + i3 - 1
                  i3 = 0

            end do
            print*, ''
            print*, gitd1_dim
            print*, gitd2_dim
            print*, gitd3_dim

            deallocate(itriples)

      end subroutine irrep_triples_doubles
 
      subroutine irrep_triples_triplet(targ, irrep0, irrep1, itriples, symm, m, dim)

            integer, intent(in) :: targ
            integer, intent(in) :: symm
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, dimension(:, :), intent(out)::   itriples
            integer, intent(inout) :: m
            logical, intent(in)    :: dim

            integer :: order
            integer :: a, i, b, j, c, k

            order = -1
            if (symm == C2v) then
                  order = C2v_order
            else if (symm == D2h) then
                  order = D2h_order
            else
                  call msg("SYMMETRY ERROR: GROUP NOT YET IMPLEMENTED", MSG_ERROR)
                  stop
            end if

            m = 1
            do a = 1, order
                  do i = 1, order
                        do b = 1, order
                              do j = 1, order
                                    do c = 1, order
                                          do k = 1, order

                                                call irrep_triples_triplet_inner(m, itriples, targ, a, i, b, j, c, k, &
                                                      irrep0, irrep1, dim)

                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
            m = m - 1

      end subroutine irrep_triples_triplet


      subroutine irrep_triples_triplet_inner(m, itriples, targ, a, i, b, j, c, k, irrep0, irrep1, dim)
             integer, intent(inout) :: m
            integer, dimension(:,:), intent(inout) :: itriples
            integer, intent(in) :: targ
            integer, intent(in) :: a
            integer, intent(in) :: i
            integer, intent(in) :: b
            integer, intent(in) :: j
            integer, intent(in) :: c
            integer, intent(in) :: k
            integer, dimension(:,:), intent(in) :: irrep0
            integer, dimension(:,:), intent(in) :: irrep1
            logical, intent(in) :: dim

            integer, dimension(6) :: input_irreps
            integer :: output_irrep
            integer :: n0i, n0j, n0k, n0a, n0b, n0c
            integer :: n1i, n1j, n1k, n1a, n1b, n1c
            logical :: trippar

            input_irreps(1) = a
            input_irreps(2) = i
            input_irreps(3) = b
            input_irreps(4) = j
            input_irreps(5) = c
            input_irreps(6) = k

            call direct_product(input_irreps, POINT_GROUP, output_irrep)

            if (output_irrep == targ) then

                  n0a = irrep0(2, a)
                  n1a = irrep1(2, a)

                  n0i = irrep0(1, i)
                  n1i = irrep1(1, i)

                  n0b = irrep0(2, b)
                  n1b = irrep1(2, b)

                  n0j = irrep0(1, j)
                  n1j = irrep1(1, j)

                  n0c = irrep0(2, c)
                  n1c = irrep1(2, c)

                  n0k = irrep0(1, k)
                  n1k = irrep1(1, k)

                  if (n0i .ne. 0 .and. n0a .ne. 0 .and. n0j&
                        .ne. 0 .and. n0b .ne. 0 &
                        .and. n0k .ne. 0 .and. n0c .ne. 0) then


                        
                        trippar = .true.
                        
                        if (n1b .le. n0c) then
                              trippar = .false.
                        end if
                        
                        if (n1j .le. n0k) then
                              trippar = .false.
                        end if
                        
                        if (trippar) then

                              if (.not. dim) then
                                    itriples(1, m) = a
                                    itriples(2, m) = i
                                    itriples(3, m) = b
                                    itriples(4, m) = j
                                    itriples(5, m) = c
                                    itriples(6, m) = k
                              end if
                              m = m + 1
                        end if
                  end if
            end if

      end subroutine irrep_triples_triplet_inner


      subroutine loop_boundaries_sp(ipairs, irrep0, irrep1, &
            n0i, n1i, n0a, n1a)
            integer, dimension(:), intent(in)                    :: ipairs
            integer, dimension(:,:), intent(in)                  :: irrep0
            integer, dimension(:,:), intent(in)                  :: irrep1
            integer, intent(out)                                 :: n0i
            integer, intent(out)                                 :: n1i
            integer, intent(out)                                 :: n0a
            integer, intent(out)                                 :: n1a
            
            n0a = irrep0(2, ipairs(1))
            n1a = irrep1(2, ipairs(1))    

            n0i = irrep0(1, ipairs(2))
            n1i = irrep1(1, ipairs(2))
            
      end subroutine loop_boundaries_sp

      subroutine loop_boundaries_sp_qq(ipairs, irrep0, irrep1, &
            n0i, n1i, n0a, n1a, q1, q2)
            integer, dimension(:), intent(in)                    :: ipairs
            integer, dimension(:,:), intent(in)                  :: irrep0
            integer, dimension(:,:), intent(in)                  :: irrep1
            integer, intent(out)                                 :: n0i
            integer, intent(out)                                 :: n1i
            integer, intent(out)                                 :: n0a
            integer, intent(out)                                 :: n1a
            integer, intent(in)                                  :: q1
            integer, intent(in)                                  :: q2    

            n0i = irrep0(q1, ipairs(1))
            n1i = irrep1(q1, ipairs(1))

            n0a = irrep0(q2, ipairs(2))
            n1a = irrep1(q2, ipairs(2))

            ! n0i = irrep0(q1, ipairs(2))
            ! n1i = irrep1(q1, ipairs(2))

            ! n0a = irrep0(q2, ipairs(1))
            ! n1a = irrep1(q2, ipairs(1))
            
      end subroutine loop_boundaries_sp_qq



      subroutine irrep_bound(c_rep, s0, s1, irrep0, irrep1, order, n)
            integer, intent(in) :: s0
            integer, intent(in) :: s1
            integer, dimension(:), intent(in) :: c_rep
            integer, dimension(:, :), intent(inout) :: irrep0
            integer, dimension(:, :), intent(inout) :: irrep1
            integer, intent(in) :: order
            integer, intent(in) :: n

            integer :: i, j

            do i = 1, order
                  do j = s0, s1
                        if (c_rep(j) == i) then
                              if (irrep0(n, i) == 0) then
                                    irrep0(n, i) = j
                              end if
                              irrep1(n, i) = j
                        end if
                  end do
            end do

      end subroutine irrep_bound



      function lindepcheck(c_symm, s0, i)
            logical :: lindepcheck
            real(F64), dimension(:,:), intent(in) :: c_symm
            integer, intent(in) :: s0
            integer, intent(in) :: i

            integer :: k, l
            real(F64), dimension(:,:), allocatable :: gram
            integer :: gdim
!            real(F64) :: ddot
!            external ddot

            gdim = i


            allocate(gram(gdim, gdim))
            
            do l = 1, gdim
                  do k = 1, gdim
                        call la_dot_olenkifer(gram(l, k), c_symm(:, s0+l-1), c_symm(:, s0+k-1))
                  end do
            end do

            lindepcheck = issingular(gram, gdim, LINDEPCOND)

            deallocate(gram)

      end function lindepcheck

      function lincheck(c_symm, i, overlap)
            logical :: lincheck
            real(F64), dimension(:,:), intent(in) :: c_symm
            integer, intent(in) :: i
            real(F64), dimension(:,:), intent(in) :: overlap

            integer :: k, l
            real(F64), dimension(:,:), allocatable :: gram

            allocate(gram(i, i))

            do l = 1, i
                  do k = 1, i
                        gram(l, k) = dot_with_overlap(c_symm(:, l), c_symm(:, k), overlap, CC_NORB)
!                        call la_dot(gram(l, k), c_symm(:, l), c_symm(:, k))
                  end do
            end do

            lincheck = issingular(gram, i, LINDEPCOND)

            deallocate(gram)

      end function lincheck

      subroutine symmetry_info(c_rep, symm)
            integer, dimension(:), intent(in) :: c_rep
            integer, intent(in) :: symm
            print*, 'sym info'
            if (symm == D2h) then
                  call msg("MOLECULE SYMMETRY: D2h")
                  call symmetry_info_small(c_rep, REP_Ag)
                  call symmetry_info_small(c_rep, REP_B1g)
                  call symmetry_info_small(c_rep, REP_B2g)
                  call symmetry_info_small(c_rep, REP_B3g)
                  call symmetry_info_small(c_rep, REP_Au)
                  call symmetry_info_small(c_rep, REP_B1u)
                  call symmetry_info_small(c_rep, REP_B2u)
                  call symmetry_info_small(c_rep, REP_B3u)
            else if (symm == C2v) then
                  call msg("MOLECULE SYMMETRY: C2v")
                  call symmetry_info_small(c_rep, REP_A1)
                  call symmetry_info_small(c_rep, REP_A2)
                  call symmetry_info_small(c_rep, REP_B1)
                  call symmetry_info_small(c_rep, REP_B2)
            end if
            print*, 'po info'
      end subroutine symmetry_info

      subroutine symmetry_info_small(c_rep, rep)
            integer, dimension(:), intent(in) :: c_rep
            integer, intent(in) :: rep

            integer :: i, k

            k = 0
            do i = 1, CC_NORB
                  if (c_rep(i) == rep)then
                        k = k + 1
                  end if
            end do
            if (k .ne. 0) then
                  if (POINT_GROUP == D2h) then
                        if (rep == REP_Ag) then
                              call imsg("Number of Orbitals in symmetry Ag: ", k)
                        else if (rep == REP_B1g) then
                              call imsg("Number of Orbitals in symmetry B1g: ", k)
                        else if (rep == REP_B2g) then
                              call imsg("Number of Orbitals in symmetry B2g: ", k)
                        else if (rep == REP_B3g) then
                              call imsg("Number of Orbitals in symmetry B3g: ", k)
                        else if (rep == REP_Au) then
                              call imsg("Number of Orbitals in symmetry Au: ", k)
                        else if (rep == REP_B1u) then
                              call imsg("Number of Orbitals in symmetry B1u: ", k)
                        else if (rep == REP_B2u) then
                              call imsg("Number of Orbitals in symmetry B2u: ", k)
                        else if (rep == REP_B3u) then
                              call imsg("Number of Orbitals in symmetry B3u: ", k)
                        end if
                  else if (POINT_GROUP == C2v) then
                        if (rep == REP_A1) then
                              call imsg("Number of Orbitals in symmetry A1: ", k)
                        else if (rep == REP_A2) then
                              call imsg("Number of Orbitals in symmetry A2: ", k)
                        else if (rep == REP_B1) then
                              call imsg("Number of Orbitals in symmetry B1: ", k)
                        else if (rep == REP_B2) then
                              call imsg("Number of Orbitals in symmetry B2: ", k)
                        end if
                  end if
                  k = 0
                  do i = 1, CC_NORB
                        if (c_rep(i) == rep)then
                              print*, i
                        end if
                  end do
            end if
            
            
      end subroutine symmetry_info_small


      subroutine add_symmetrized_vec(i, project_m, m_symm, order, rep_list)

            integer, intent(inout) :: i
            real(F64), dimension(:,:), intent(in)    :: project_m
            real(F64), dimension(:,:), intent(inout) :: m_symm
            integer, intent(in) :: order
            integer, dimension(:), intent(out) :: rep_list

            real(F64), dimension(:,:), allocatable :: lin_ind_project_mo            
            integer, dimension(:), allocatable :: lin_ind_idx
            real(F64) :: nrm
            integer   :: k
            logical   :: cond
            real(F64), parameter :: eps = 1.d-8


            allocate(lin_ind_project_mo(CC_NORB, order))
            allocate(lin_ind_idx(order))


            do k = 1, order
                  call la_dot_olenkifer(nrm, project_m(:, k), project_m(:, k))
                  nrm = sqrt(nrm)
                  if(nrm .gt. eps) then
                        m_symm(:, i) = project_m(:, k) / nrm
                        cond = lincheck_m(m_symm, i)
                        if (cond .eqv. .false.) then
                              rep_list(i) = k
                              i = i + 1
                        end if
                  end if
            end do

      end subroutine add_symmetrized_vec


      function lincheck_m(c_symm, i)
            logical :: lincheck_m
            real(F64), dimension(:,:), intent(in) :: c_symm
            integer, intent(in) :: i

            integer :: k, l
            real(F64), dimension(:,:), allocatable :: gram

            allocate(gram(i, i))

            do l = 1, i
                  do k = 1, i
                        call la_dot_olenkifer(gram(l, k), c_symm(:, l), c_symm(:, k))                                                                                                             
                  end do
            end do

            lincheck_m = issingular(gram, i, LINDEPCOND)

            deallocate(gram)

      end function lincheck_m

end module symmetry
