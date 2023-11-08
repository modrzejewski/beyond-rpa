
module eomccsdprop
      use math_constants
      use tmga
      use tmxi
      use tmga_ccsd
      use tmxi_ccsd
      use v0_tmxi
      use v1_tmxi
      use v2_tmxi
      use v3_tmxi
      use v4_tmxi
      use v5_tmxi
      use v6_tmxi

      implicit none

contains

      function biortonorm(l, r, nocc, nvirt, nidx)
            double precision :: biortonorm
            double precision, dimension(:), intent(in) :: l, r
            integer, intent(in)                        :: nocc, nvirt
            integer, intent(in)                        :: nidx

            double precision :: dot
            integer :: v
            integer :: npair

            npair = nocc * nvirt
            !
            ! NIDX = number of one electron indices +
            !        number of two electron indices
            !       (CC3 + number of three electron indices)
            !
            dot = ZERO
            do v = 1, nidx
                  dot = dot + l(v) * r(v)
                  ! print*, dot
            end do
            biortonorm = ONE / sqrt(dot)

      end function biortonorm


      subroutine motojac(a, movec, jacvec, nocc, nvirt, nidx)
            double precision, intent(out)               :: a
            double precision, dimension(:), intent(in)  :: jacvec, movec
            integer, intent(in)                         :: nocc, nvirt
            integer, intent(in)                         :: nidx

            integer :: npair
            integer :: v

            npair = nocc * nvirt
            !
            ! NIDX = number of one electron indices +
            !        number of two electron indices
            !        (CC3 + number of three electron indices)
            !
            a = ZERO

            do v = 1, nidx
                  a = a + jacvec(v) * movec(v)
            end do
      end subroutine motojac


      subroutine tmccsd(method, tm, n, lvec, rvec, t2, t1, s2, s1,  &
            nocc0, nocc1, nvirt0, nvirt1, xga, yxi, nocc, nvirt, nactive, nidx)
            ! ---------------------------------------------------------------------
            ! TM      - Output, requested transition moments (ordering consistent
            !           with TMIDX)
            ! N       - Number of requested transition moments (first N indices
            !           from TMIDX will be read)
            ! LVEC    - Matrix of left eigenvectors
            ! RVEC    - Matrix of right eigenvectors
            ! T2      - CCSD T2 amplitudes
            ! T1      - CCSD T1 amplitudes
            ! NOCC0   - First occupied orbital
            ! NOCC1   - Last occupied orbital
            ! NVIRT0  - First virtual orbital
            ! NVIRT1  - Last virtual orbital
            ! XGA     - Matrix of the one electron operator
            !           present in the gamma part
            ! YXI     - Matrix of the one electron operator
            !           present in the xi part
            ! NIDX = number of one electron indices +
            !        number of two electron indices
            !        (CC3 + number of three electron indices)
            !
            character(len=*), intent(in) :: method
            double precision, dimension(:), intent(out)         :: tm
            integer, intent(in)                                 :: n
            double precision, dimension(:, :), intent(in)       :: lvec
            double precision, dimension(:, :), intent(in)       :: rvec
            double precision, dimension(:, :, :, :), intent(in) :: t2
            double precision, dimension(:, :), intent(in)       :: t1
            double precision, dimension(:, :, :, :), intent(in) :: s2
            double precision, dimension(:, :), intent(in)       :: s1
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, intent(in)                                 :: nocc, nvirt, nactive
            double precision, dimension(:, :), intent(in)       :: xga
            double precision, dimension(:, :), intent(in)       :: yxi
            integer, intent(in)                                 :: nidx

            double precision, dimension(n) :: bnorm
            double precision :: ga, xi
            double precision, dimension(:), allocatable :: ganu, xinu
            integer :: i, a, j, b, c, k
            integer :: ai, bj, ck
            integer :: iket, ibra
            integer :: v, w
            integer ::  npair
            integer :: braoffset, ketoffset
            integer :: qbj, qbj2
            integer :: qck, qck2
            integer :: q00
            

            npair = nocc * nvirt

            qbj  = 3 + 6 * npair
            qbj2 = -3
            qck  = 2 + 3 * npair * (2 + npair)
            qck2 = -3 * (1 + npair)
            q00  = -3 * npair * (3 + npair)
            !
            ! Get normalization factor in biorthonormal basis
            !
            do w = 1, n
                  bnorm(w) = biortonorm(lvec(:, w), rvec(:, w), nocc, nvirt, nidx)
            end do
            !
            ! NIDX = number of one electron indices +
            !        number of two electron indices
            !
            allocate(ganu(nidx))
            allocate(xinu(nidx))
            ganu = ZERO
            xinu = ZERO
            tm = ZERO
            !
            ! Generate XI and GAMMA in MO basis
            !
            if(method.eq.'CCSD')then
                  
                  v = 1
                  do a = nvirt0, nvirt1
                        do i = nocc0, nocc1
                              ganu(v) = ganu_ccsd1_ai(xga, t2, t1, s2, s1, nocc, nactive, a, i)
                              xinu(v) = xinu_ccsd1_ai(yxi, t2, t1,  nocc, nactive, a, i)
                              v = v + 1
                        end do
                  end do
                  do b = nvirt0, nvirt1
                        do j = nocc0, nocc1
                              !
                              ! a == b, i == j
                              !
                              a = b
                              i = j
                              ganu(v) = ganu_ccsd2_aiai(xga, s2, s1, nocc, nactive, a, i)
                              xinu(v) = xinu_ccsd2_aiai(yxi, t2, nocc, nactive, a, i)
                              v = v + 1
                              !
                              ! a == b, i > j
                              !
                              a = b
                              do i = j + 1, nocc1
                                    ganu(v) = ganu_ccsd2_aiaj(xga, s2, s1, nocc, nactive, a, i, j)
                                    xinu(v) = xinu_ccsd2_aiaj(yxi, t2, nocc, nactive, a, i, j)
                                    v = v + 1
                              end do
                              !
                              ! a > b
                              !
                              do a = b + 1, nvirt1
                                    !
                                    ! a > b, i < j
                                    !
                                    do i = nocc0, j - 1
                                          ganu(v) = ganu_ccsd2_aibj(xga, s2, s1, nocc, nactive, a, i, b, j)
                                          xinu(v) = xinu_ccsd2_aibj(yxi, t2, nocc, nactive, a, i, b, j)
                                          v = v + 1
                                    end do
                                    !
                                    ! a > b, i == j
                                    !
                                    i = j
                                    ganu(v) = ganu_ccsd2_aibi(xga, s2, s1,  nocc, nactive, a, i, b)
                                    xinu(v) = xinu_ccsd2_aibi(yxi, t2, nocc, nactive, a, i, b)
                                    v = v + 1
                                    !
                                    ! a > b, i > j
                                    !
                                    do i = j + 1, nocc1
                                          ganu(v) = ganu_ccsd2_aibj(xga, s2, s1, nocc, nactive, a, i, b, j)
                                          xinu(v) = xinu_ccsd2_aibj(yxi, t2, nocc, nactive, a, i, b, j)
                                          v = v + 1
                                    end do
                              end do
                        end do
                  end do

            else if(method.eq.'CC3')then
                  v = 1
                  do a = nvirt0, nvirt1
                        do i = nocc0, nocc1
                              ganu(v) = ganu1_ai(xga, t2, t1, s2, s1, nocc, nactive, a, i)
                              xinu(v) = xinu1_ai(yxi, t2, t1,  nocc, nactive, a, i)
                              v = v + 1
                        end do
                  end do
                  do b = nvirt0, nvirt1
                        do j = nocc0, nocc1
                              !
                              ! a == b, i == j
                              !
                              a = b
                              i = j
                              ganu(v) = ganu2_aiai(xga, s2, s1, nocc, nactive, a, i)
                              xinu(v) = xinu2_aiai(yxi, t2, nocc, nactive, a, i)
                              v = v + 1
                              !
                              ! a == b, i > j
                              !
                              a = b
                              do i = j + 1, nocc1
                                    ganu(v) = ganu2_aiaj(xga, s2, s1, nocc, nactive, a, i, j)
                                    xinu(v) = xinu2_aiaj(yxi, t2, nocc, nactive, a, i, j)
                                    v = v + 1
                              end do
                              !
                              ! a > b
                              !
                              do a = b + 1, nvirt1
                                    !
                                    ! a > b, i < j
                                    !
                                    do i = nocc0, j - 1
                                          ganu(v) = ganu2_aibj(xga, s2, s1, nocc, nactive, a, i, b, j)
                                          xinu(v) = xinu2_aibj(yxi, t2, nocc, nactive, a, i, b, j)
                                          v = v + 1
                                    end do
                                    !
                                    ! a > b, i == j
                                    !
                                    i = j
                                    ganu(v) = ganu2_aibi(xga, s2, s1,  nocc, nactive, a, i, b)
                                    xinu(v) = xinu2_aibi(yxi, t2, nocc, nactive, a, i, b)
                                    v = v + 1
                                    !
                                    ! a > b, i > j
                                    !
                                    do i = j + 1, nocc1
                                          ganu(v) = ganu2_aibj(xga, s2, s1, nocc, nactive, a, i, b, j)
                                          xinu(v) = xinu2_aibj(yxi, t2, nocc, nactive, a, i, b, j)
                                          v = v + 1
                                    end do
                              end do
                        end do
                  end do
            
            braoffset = v-1
            ketoffset = v-1
            

            !
            ! Elementary loop 1
            ! --------------------
            ! Free virtual indices: a, c
            ! Free occupied indices: i, j, k
            ! Equalities: b == a
            ! No equalities independent of the above can hold.
            !
            c_aiajck: do c = nvirt0, nvirt1
                  k_aiajck: do k = nocc0, nocc1
                        ck = (c - nvirt0) * nocc + (k - nocc0) + 1
                        j_aiajck: do j = nocc0, k - 1
                              a_aiajck: do a = c + 1, nvirt1
                                    bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                                    i_aiajck: do i = j + 1, nocc1
                                          if (i == k) cycle i_aiajck
                                          ai = (a - nvirt0) * nocc + (i - nocc0) + 1

                                          iket = ketoffset + mu3tr(ai, bj, ck)
                                          ganu(iket) = ganu3_aiajck(xga, s2, nocc, nactive, a, i, j, c, k)

                                    end do i_aiajck
                              end do a_aiajck
                        end do j_aiajck
                  end do k_aiajck
            end do c_aiajck
            !
            ! Elementary loop 2
            ! --------------------
            ! Free virtual indices: a, b
            ! Free occupied indices: i, j, k
            ! Equalities: c == a
            ! No equalities independent of the above can hold.
            !
            k_aibjak: do k = nocc0, nocc1
                  b_aibjak: do b = nvirt0, nvirt1
                        j_aibjak: do j = nocc0, nocc1
                              if (j == k) cycle j_aibjak
                              bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                              a_aibjak: do a = b + 1, b - 1
                                    ck = (a - nvirt0) * nocc + (k - nocc0) + 1
                                    i_aibjak: do i = nocc0, nocc1
                                          if (i == j .or. i == k) cycle i_aibjak
                                          ai = (a - nvirt0) * nocc + (i - nocc0) + 1

                                          iket = ketoffset + mu3tr(ai, bj, ck)
                                          ganu(iket) = ganu3_aibjak(xga, s2, nocc, nactive, a, i, b, j, k)

                                    end do i_aibjak
                              end do a_aibjak
                        end do j_aibjak
                  end do b_aibjak
            end do k_aibjak
            !
            ! Elementary loop 3
            ! --------------------
            ! Free virtual indices: b, a
            ! Free occupied indices: i, j, k
            ! Equalities: c == b
            ! No equalities independent of the above can hold.
            !
            k_aibjbk: do k = nocc0, nocc1
                  b_aibjbk: do b = nvirt0, nvirt1
                        ck = (b - nvirt0) * nocc + (k - nocc0) + 1
                        j_aibjbk: do j = k + 1, nocc1
                              bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                              a_aibjbk: do a = b + 1, nvirt1
                                    i_aibjbk: do i = nocc0, j - 1
                                          if (i == k) cycle i_aibjbk
                                          ai = (a - nvirt0) * nocc + (i - nocc0) + 1

                                          iket = ketoffset + mu3tr(ai, bj, ck)
                                          ganu(iket) = ganu3_aibjbk(xga, s2, nocc, nactive, a, i, b, j, k)

                                    end do i_aibjbk
                              end do a_aibjbk
                        end do j_aibjbk
                  end do b_aibjbk
            end do k_aibjbk
            !
            ! Elementary loop 4
            ! --------------------
            ! Free virtual indices: a, b, c
            ! Free occupied indices: i, j
            ! Equalities: k == i
            ! No equalities independent of the above can hold.
            !
            c_aibjci: do c = nvirt0, nvirt1
                  b_aibjci: do b = c + 1, nvirt1
                        j_aibjci: do j = nocc0, nocc1
                              bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                              a_aibjci: do a = b + 1, nvirt1
                                    if (a == c) cycle a_aibjci
                                    i_aibjci: do i = nocc0, nocc1
                                          if (i == j) cycle i_aibjci
                                          ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                          ck = (c - nvirt0) * nocc + (i - nocc0) + 1

                                          iket = ketoffset + mu3tr(ai, bj, ck)
                                          ganu(iket) = ganu3_aibjci(xga, s2, nocc, nactive, a, i, b, j, c)

                                    end do i_aibjci
                              end do a_aibjci
                        end do j_aibjci
                  end do b_aibjci
            end do c_aibjci
            !
            ! Elementary loop 5
            ! --------------------
            ! Free virtual indices: a, b, c
            ! Free occupied indices: j, i
            ! Equalities: k == j
            ! No equalities independent of the above can hold.
            !
            c_aibjcj: do c = nvirt0, nvirt1
                  b_aibjcj: do b = c + 1, nvirt1
                        j_aibjcj: do j = nocc0, nocc1
                              bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                              ck = (c - nvirt0) * nocc + (j - nocc0) + 1
                              a_aibjcj: do a = b + 1, nvirt1
                                    if (a == c) cycle a_aibjcj
                                    i_aibjcj: do i = nocc0, nocc1
                                          if (i == j) cycle i_aibjcj
                                          ai = (a - nvirt0) * nocc + (i - nocc0) + 1

                                          iket = ketoffset + mu3tr(ai, bj, ck)
                                          ganu(iket) = ganu3_aibjcj(xga, s2, nocc, nactive, a, i, b, j, c)

                                    end do i_aibjcj
                              end do a_aibjcj
                        end do j_aibjcj
                  end do b_aibjcj
            end do c_aibjcj
            !
            ! Elementary loop 6
            ! --------------------
            ! Free virtual indices: a, c
            ! Free occupied indices: i, j
            ! Equalities: b == a, k == i
            ! No equalities independent of the above can hold.
            !
            c_aiajci: do c = nvirt0, nvirt1
                  j_aiajci: do j = nocc0, nocc1
                        a_aiajci: do a = c + 1, nvirt1
                              bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                              i_aiajci: do i = j + 1, nocc1
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (c - nvirt0) * nocc + (i - nocc0) + 1

                                    iket = ketoffset + mu3tr(ai, bj, ck)
                                    ganu(iket) = ganu3_aiajci(xga, s2, nocc, nactive, a, i, j, c)

                              end do i_aiajci
                        end do a_aiajci
                  end do j_aiajci
            end do c_aiajci
            !
            ! Elementary loop 7
            ! --------------------
            ! Free virtual indices: a, c
            ! Free occupied indices: j, i
            ! Equalities: b == a, k == j
            ! No equalities independent of the above can hold.
            !
            c_aiajcj: do c = nvirt0, nvirt1
                  j_aiajcj: do j = nocc0, nocc1
                        ck = (c - nvirt0) * nocc + (j - nocc0) + 1
                        a_aiajcj: do a = c + 1, nvirt1
                              bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                              i_aiajcj: do i = j + 1, nocc1
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1

                                    iket = ketoffset + mu3tr(ai, bj, ck)
                                    ganu(iket) = ganu3_aiajcj(xga, s2, nocc, nactive, a, i, j, c)

                              end do i_aiajcj
                        end do a_aiajcj
                  end do j_aiajcj
            end do c_aiajcj
            !
            ! Elementary loop 8
            ! --------------------
            ! Free virtual indices: a, b
            ! Free occupied indices: i, j
            ! Equalities: c == a, k == i
            ! No equalities independent of the above can hold.
            !
            b_aibjai: do b = nvirt0, nvirt1
                  j_aibjai: do j = nocc0, nocc1
                        bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                        a_aibjai: do a = b + 1, b - 1
                              i_aibjai: do i = nocc0, nocc1
                                    if (i == j) cycle i_aibjai
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (a - nvirt0) * nocc + (i - nocc0) + 1

                                    iket = ketoffset + mu3tr(ai, bj, ck)
                                    ganu(iket) = ganu3_aibjai(xga, s2, nocc, nactive, a, i, b, j)

                              end do i_aibjai
                        end do a_aibjai
                  end do j_aibjai
            end do b_aibjai
            !
            ! Elementary loop 9
            ! --------------------
            ! Free virtual indices: a, b
            ! Free occupied indices: i, k
            ! Equalities: c == a, j == i
            ! No equalities independent of the above can hold.
            !
            k_aibiak: do k = nocc0, nocc1
                  b_aibiak: do b = nvirt0, nvirt1
                        a_aibiak: do a = b + 1, b - 1
                              ck = (a - nvirt0) * nocc + (k - nocc0) + 1
                              i_aibiak: do i = nocc0, nocc1
                                    if (i == k) cycle i_aibiak
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1

                                    iket = ketoffset + mu3tr(ai, bj, ck)
                                    ganu(iket) = ganu3_aibiak(xga, s2, nocc, nactive, a, i, b, k)

                              end do i_aibiak
                        end do a_aibiak
                  end do b_aibiak
            end do k_aibiak
            !
            ! Elementary loop 10
            ! --------------------
            ! Free virtual indices: a, b
            ! Free occupied indices: j, i
            ! Equalities: c == a, k == j
            ! No equalities independent of the above can hold.
            !
            b_aibjaj: do b = nvirt0, nvirt1
                  j_aibjaj: do j = nocc0, nocc1
                        bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                        a_aibjaj: do a = b + 1, b - 1
                              ck = (a - nvirt0) * nocc + (j - nocc0) + 1
                              i_aibjaj: do i = nocc0, nocc1
                                    if (i == j) cycle i_aibjaj
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1

                                    iket = ketoffset + mu3tr(ai, bj, ck)
                                    ganu(iket) = ganu3_aibjaj(xga, s2, nocc, nactive, a, i, b, j)

                              end do i_aibjaj
                        end do a_aibjaj
                  end do j_aibjaj
            end do b_aibjaj
            !
            ! Elementary loop 11
            ! --------------------
            ! Free virtual indices: b, a
            ! Free occupied indices: i, j
            ! Equalities: c == b, k == i
            ! No equalities independent of the above can hold.
            !
            b_aibjbi: do b = nvirt0, nvirt1
                  j_aibjbi: do j = nocc0, nocc1
                        bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                        a_aibjbi: do a = b + 1, nvirt1
                              i_aibjbi: do i = nocc0, j - 1
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (b - nvirt0) * nocc + (i - nocc0) + 1

                                    iket = ketoffset + mu3tr(ai, bj, ck)
                                    ganu(iket) = ganu3_aibjbi(xga, s2, nocc, nactive, a, i, b, j)

                              end do i_aibjbi
                        end do a_aibjbi
                  end do j_aibjbi
            end do b_aibjbi
            !
            ! Elementary loop 12
            ! --------------------
            ! Free virtual indices: b, a
            ! Free occupied indices: i, k
            ! Equalities: c == b, j == i
            ! No equalities independent of the above can hold.
            !
            k_aibibk: do k = nocc0, nocc1
                  b_aibibk: do b = nvirt0, nvirt1
                        ck = (b - nvirt0) * nocc + (k - nocc0) + 1
                        a_aibibk: do a = b + 1, nvirt1
                              i_aibibk: do i = k + 1, nocc1
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1

                                    iket = ketoffset + mu3tr(ai, bj, ck)
                                    ganu(iket) = ganu3_aibibk(xga, s2, nocc, nactive, a, i, b, k)

                              end do i_aibibk
                        end do a_aibibk
                  end do b_aibibk
            end do k_aibibk
            !
            ! Elementary loop 13
            ! --------------------
            ! Free virtual indices: a, b, c
            ! Free occupied indices: i, j, k
            ! Equalities: none
            ! No equalities independent of the above can hold.
            !
            c_aibjck: do c = nvirt0, nvirt1
                  k_aibjck: do k = nocc0, nocc1
                        ck = (c - nvirt0) * nocc + (k - nocc0) + 1
                        b_aibjck: do b = c + 1, nvirt1
                              j_aibjck: do j = nocc0, nocc1
                                    if (j == k) cycle j_aibjck
                                    bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                                    a_aibjck: do a = b + 1, nvirt1
                                          if (a == c) cycle a_aibjck
                                          i_aibjck: do i = nocc0, nocc1
                                                if (i == j .or. i == k) cycle i_aibjck
                                                if (i > j .and. j > k) exit i_aibjck
                                                ai = (a - nvirt0) * nocc + (i - nocc0) + 1

                                    iket = ketoffset + mu3tr(ai, bj, ck)
                                    ganu(iket) = ganu3_aibjck(xga, s2, nocc, nactive, a, i, b, j, c, k)

                                          end do i_aibjck
                                    end do a_aibjck
                              end do j_aibjck
                        end do b_aibjck
                  end do k_aibjck
            end do c_aibjck

!
! Elementary loop 1
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a
! No equalities independent of the above can hold.
!
c2_aiajck: do c = nvirt0, nvirt1
k2_aiajck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j2_aiajck: do j = nocc0, k - 1
a2_aiajck: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i2_aiajck: do i = j + 1, nocc1
if (i == k) cycle i2_aiajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
    ibra = braoffset + mu3tr(ai, bj, ck)

    xinu(ibra) = v6_tmxi_aiajck(yxi, t2, nocc, nactive, a, i, j, c, k)
    
end do i2_aiajck
end do a2_aiajck
end do j2_aiajck
end do k2_aiajck
end do c2_aiajck
!
! Elementary loop 2
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b
! No equalities independent of the above can hold.
!
k2_aibjbk: do k = nocc0, nocc1
b2_aibjbk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j2_aibjbk: do j = k + 1, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a2_aibjbk: do a = b + 1, nvirt1
i2_aibjbk: do i = nocc0, j - 1
if (i == k) cycle i2_aibjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
    ibra = braoffset + mu3tr(ai, bj, ck)
    
    xinu(ibra) = v6_tmxi_aibjbk(yxi, t2, nocc, nactive, a, i, b, j, k)
    
end do i2_aibjbk
end do a2_aibjbk
end do j2_aibjbk
end do b2_aibjbk
end do k2_aibjbk
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: k == i
! No equalities independent of the above can hold.
!
c2_aibjci: do c = nvirt0, nvirt1
b2_aibjci: do b = c + 1, nvirt1
j2_aibjci: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a2_aibjci: do a = b + 1, nvirt1
if (a == c) cycle a2_aibjci
i2_aibjci: do i = nocc0, nocc1
if (i == j) cycle i2_aibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
    ibra = braoffset + mu3tr(ai, bj, ck)
    
    xinu(ibra) = v0_tmxi_aibjci(yxi, t2, nocc, nactive, a, i, b, j, c)
    
end do i2_aibjci
end do a2_aibjci
end do j2_aibjci
end do b2_aibjci
end do c2_aibjci
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: k == j
! No equalities independent of the above can hold.
!
c2_aibjcj: do c = nvirt0, nvirt1
b2_aibjcj: do b = c + 1, nvirt1
j2_aibjcj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a2_aibjcj: do a = b + 1, nvirt1
if (a == c) cycle a2_aibjcj
i2_aibjcj: do i = nocc0, nocc1
if (i == j) cycle i2_aibjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
    ibra = braoffset + mu3tr(ai, bj, ck)
    
    xinu(ibra) = v0_tmxi_aibjcj(yxi, t2, nocc, nactive, a, i, b, j, c)
    
end do i2_aibjcj
end do a2_aibjcj
end do j2_aibjcj
end do b2_aibjcj
end do c2_aibjcj
!
! Elementary loop 5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, k == i
! No equalities independent of the above can hold.
!
c2_aiajci: do c = nvirt0, nvirt1
j2_aiajci: do j = nocc0, nocc1
a2_aiajci: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i2_aiajci: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
    ibra = braoffset + mu3tr(ai, bj, ck)
    
    xinu(ibra) = v6_tmxi_aiajci(yxi, t2, nocc, nactive, a, i, j, c)
    
end do i2_aiajci
end do a2_aiajci
end do j2_aiajci
end do c2_aiajci
!
! Elementary loop 6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, k == j
! No equalities independent of the above can hold.
!
c2_aiajcj: do c = nvirt0, nvirt1
j2_aiajcj: do j = nocc0, nocc1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a2_aiajcj: do a = c + 1, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i2_aiajcj: do i = j + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
    ibra = braoffset + mu3tr(ai, bj, ck)
    
    xinu(ibra) = v0_tmxi_aiajcj(yxi, t2, nocc, nactive, a, i, j, c)
    
end do i2_aiajcj
end do a2_aiajcj
end do j2_aiajcj
end do c2_aiajcj
!
! Elementary loop 7
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, k == i
! No equalities independent of the above can hold.
!
b2_aibjbi: do b = nvirt0, nvirt1
j2_aibjbi: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a2_aibjbi: do a = b + 1, nvirt1
i2_aibjbi: do i = nocc0, j - 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
    ibra = braoffset + mu3tr(ai, bj, ck)
    
    xinu(ibra) = v6_tmxi_aibjbi(yxi, t2, nocc, nactive, a, i, b, j)
    
    
end do i2_aibjbi
end do a2_aibjbi
end do j2_aibjbi
end do b2_aibjbi
!
! Elementary loop 8
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, j == i
! No equalities independent of the above can hold.
!
k2_aibibk: do k = nocc0, nocc1
b2_aibibk: do b = nvirt0, nvirt1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a2_aibibk: do a = b + 1, nvirt1
i2_aibibk: do i = k + 1, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
    ibra = braoffset + mu3tr(ai, bj, ck)
    
    xinu(ibra) = v0_tmxi_aibibk(yxi, t2, nocc, nactive, a, i, b, k)
    
end do i2_aibibk
end do a2_aibibk
end do b2_aibibk
end do k2_aibibk
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: none
! No equalities independent of the above can hold.
!
c2_aibjck: do c = nvirt0, nvirt1
k2_aibjck: do k = nocc0, nocc1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b2_aibjck: do b = c + 1, nvirt1
j2_aibjck: do j = nocc0, nocc1
if (j == k) cycle j2_aibjck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a2_aibjck: do a = b + 1, nvirt1
if (a == c) cycle a2_aibjck
i2_aibjck: do i = nocc0, nocc1
if (i == j .or. i == k) cycle i2_aibjck
if (i > j .and. j > k) exit i2_aibjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
    ibra = braoffset + mu3tr(ai, bj, ck)
    
    
    if (i > j) then
          if (j > k) then
                exit i2_aibjck
          else if (k > i) then
                xinu(ibra) = v3_tmxi_aibjck(yxi, t2, nocc, nactive, a, i, b, j, c, k)
          else
                xinu(ibra) = v1_tmxi_aibjck(yxi, t2, nocc, nactive, a, i, b, j, c, k)
          end if
    else if (i > k) then
          xinu(ibra) = v2_tmxi_aibjck(yxi, t2, nocc, nactive, a, i, b, j, c, k)
    else if (k > j) then
          xinu(ibra) = v5_tmxi_aibjck(yxi, t2, nocc, nactive, a, i, b, j, c, k)
    else
          xinu(ibra) = v4_tmxi_aibjck(yxi, t2, nocc, nactive, a, i, b, j, c, k)
    end if
    
    
end do i2_aibjck
end do a2_aibjck
end do j2_aibjck
end do b2_aibjck
end do k2_aibjck
end do c2_aibjck
end if
            !
            ! Transform XI and GAMMA to the basis of the Jacobian eigenvectors
            !
            do w = 1, n
                  call motojac(ga, ganu, rvec(:, w), nocc, nvirt, nidx)
                  call motojac(xi, xinu, lvec(:, w), nocc, nvirt, nidx)
                  tm(w) = ga * xi * bnorm(w)**2
!                  print*, ga, xi, bnorm(w)
            end do

            deallocate(ganu)
            deallocate(xinu)

contains
      function mu3tr(ai, bj, ck)
            !
            ! Compute compound three-electron index
            ! (assumed that ai >= bj >= ck)
            !
            integer :: mu3tr
            integer, intent(in) :: ai, bj, ck
            integer :: mu31, mu32
            !
            ! Compound index is compouted relative
            ! to the first element of matrix block.
            ! Braoffset/ketoffset should be added
            ! to get the absolute position
            !
            mu31 = (qbj + qbj2 * bj) * bj 
            mu32 = (qck + (qck2 + ck) * ck) * ck
            mu3tr  = ai + (mu31 + mu32 + q00) / 6
      end function mu3tr
end subroutine tmccsd

end module eomccsdprop
