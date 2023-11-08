module cisd_block_22_mod_part4
      use cisd22
      use math_constants
      use arithmetic
      use cmpidx
      implicit none
      !
      ! File generated automatically on 2014-11-10 21:36:57 UTC.
      !
contains
      subroutine cisd_block_22_part4(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, &
            n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, bra0, ket0, eorb) 
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
                  n0d, n1d
            integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
                  n0l, n1l
            integer, intent(in)                                 :: bra0, ket0
            real(F64), dimension(:), intent(in)                 :: eorb
            !
            ! Local variables
            !
            real(F64), dimension(:,:), intent(inout) :: hci
            integer :: a, b, c, d
            integer :: i, j, k, l
            integer :: ai, bj, ck, dl
            integer :: a0, a1, b0, b1, c0, i0, i1
            integer :: n0ab, n0abc, n0abcd, n0abd, n0ac
            integer :: n0acd, n0ad, n0bc, n0bcd, n0bd
            integer :: n0cd, n0ij, n0ijk, n0ijkl, n0ijl
            integer :: n0ik, n0ikl, n0il, n0jk, n0jkl
            integer :: n0jl, n0kl
            integer :: n1ab, n1abc, n1abcd, n1abd, n1ac
            integer :: n1acd, n1ad, n1bc, n1bcd, n1bd
            integer :: n1cd, n1ij, n1ijk, n1ijkl, n1ijl
            integer :: n1ik, n1ikl, n1il, n1jk, n1jkl
            integer :: n1jl, n1kl
            integer :: nocc, nvirt
            integer :: npair, nactive
            integer :: ibra, iket
            integer :: braoffset, ketoffset
            !
            ! Offset of the jacobian blocks
            !
            braoffset = bra0 - 1
            ketoffset = ket0 - 1
            !
            ! Number of occupied and virtual orbitals
            ! present in calculations
            !
            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nactive = nocc + nvirt
            n0ab = max(n0a, n0b)
            n0abc = max(n0a, n0b, n0c)
            n0abcd = max(n0a, n0b, n0c, n0d)
            n0abd = max(n0a, n0b, n0d)
            n0ac = max(n0a, n0c)
            n0acd = max(n0a, n0c, n0d)
            n0ad = max(n0a, n0d)
            n0bc = max(n0b, n0c)
            n0bcd = max(n0b, n0c, n0d)
            n0bd = max(n0b, n0d)
            n0cd = max(n0c, n0d)
            n0ij = max(n0i, n0j)
            n0ijk = max(n0i, n0j, n0k)
            n0ijkl = max(n0i, n0j, n0k, n0l)
            n0ijl = max(n0i, n0j, n0l)
            n0ik = max(n0i, n0k)
            n0ikl = max(n0i, n0k, n0l)
            n0il = max(n0i, n0l)
            n0jk = max(n0j, n0k)
            n0jkl = max(n0j, n0k, n0l)
            n0jl = max(n0j, n0l)
            n0kl = max(n0k, n0l)
            n1ab = min(n1a, n1b)
            n1abc = min(n1a, n1b, n1c)
            n1abcd = min(n1a, n1b, n1c, n1d)
            n1abd = min(n1a, n1b, n1d)
            n1ac = min(n1a, n1c)
            n1acd = min(n1a, n1c, n1d)
            n1ad = min(n1a, n1d)
            n1bc = min(n1b, n1c)
            n1bcd = min(n1b, n1c, n1d)
            n1bd = min(n1b, n1d)
            n1cd = min(n1c, n1d)
            n1ij = min(n1i, n1j)
            n1ijk = min(n1i, n1j, n1k)
            n1ijkl = min(n1i, n1j, n1k, n1l)
            n1ijl = min(n1i, n1j, n1l)
            n1ik = min(n1i, n1k)
            n1ikl = min(n1i, n1k, n1l)
            n1il = min(n1i, n1l)
            n1jk = min(n1j, n1k)
            n1jkl = min(n1j, n1k, n1l)
            n1jl = min(n1j, n1l)
            n1kl = min(n1k, n1l)
            !
            ! Elementary loop  1
            ! --------------------
            ! Free virtual indices: a, b
            ! Free occupied indices: i, k
            ! Equalities: c == a, d == b, j == i, l == k
            ! No equalities independent of the above can hold.
            !
            k_aibiakbk: do k = n0kl, n1kl
                  b_aibiakbk: do b = n0bd, n1bd
                        dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                        a0 = max(b + 1, n0ac)
                        a_aibiakbk: do a = a0, n1ac
                              if (a == b) cycle a_aibiakbk
                              ck = (a - nvirt0) * nocc + (k - nocc0) + 1
                              i_aibiakbk: do i = n0ij, n1ij
                                    if (i == k) cycle i_aibiakbk
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibiakbk(i, k)
                              end do i_aibiakbk
                        end do a_aibiakbk
                  end do b_aibiakbk
            end do k_aibiakbk
            !
            ! Elementary loop  2
            ! --------------------
            ! Free virtual indices: a, b
            ! Free occupied indices: i, l
            ! Equalities: c == a, d == b, j == i, k == i
            ! No equalities independent of the above can hold.
            !
            l_aibiaibl: do l = n0l, n1l
                  b_aibiaibl: do b = n0bd, n1bd
                        dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                        a0 = max(b + 1, n0ac)
                        a_aibiaibl: do a = a0, n1ac
                              if (a == b) cycle a_aibiaibl
                              i_aibiaibl: do i = n0ijk, n1ijk
                                    if (i == l) cycle i_aibiaibl
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibiaibl(a, i, b, l)
                              end do i_aibiaibl
                        end do a_aibiaibl
                  end do b_aibiaibl
            end do l_aibiaibl
            !
            ! Elementary loop  3
            ! --------------------
            ! Free virtual indices: a, b, d
            ! Free occupied indices: i
            ! Equalities: c == a, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            d_aibiaidi: do d = n0d, n1d
                  b_aibiaidi: do b = n0b, n1b
                        if (b == d) cycle b_aibiaidi
                        a0 = max(b + 1, d + 1, n0ac)
                        a_aibiaidi: do a = a0, n1ac
                              if (a == b .or. a == d) cycle a_aibiaidi
                              i_aibiaidi: do i = n0ijkl, n1ijkl
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibiaidi(a, i, b, d)
                              end do i_aibiaidi
                        end do a_aibiaidi
                  end do b_aibiaidi
            end do d_aibiaidi
            !
            ! Elementary loop  4
            ! --------------------
            ! Free virtual indices: b, a
            ! Free occupied indices: i, k
            ! Equalities: c == b, d == b, j == i, l == i
            ! No equalities independent of the above can hold.
            !
            k_aibibkbi: do k = n0k, n1k
                  b_aibibkbi: do b = n0bcd, n1bcd
                        ck = (b - nvirt0) * nocc + (k - nocc0) + 1
                        a0 = max(b + 1, n0a)
                        a_aibibkbi: do a = a0, n1a
                              if (a == b) cycle a_aibibkbi
                              i1 = min(k - 1, n1ijl)
                              i_aibibkbi: do i = n0ijl, i1
                                    if (i == k) cycle i_aibibkbi
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibibkbi(a, i, b, k)
                              end do i_aibibkbi
                        end do a_aibibkbi
                  end do b_aibibkbi
            end do k_aibibkbi
            !
            ! Elementary loop  5
            ! --------------------
            ! Free virtual indices: b, a
            ! Free occupied indices: i, j
            ! Equalities: c == b, d == b, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            b_aibjbibi: do b = n0bcd, n1bcd
                  j_aibjbibi: do j = n0j, n1j
                        bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                        a0 = max(b + 1, n0a)
                        a_aibjbibi: do a = a0, n1a
                              if (a == b) cycle a_aibjbibi
                              i_aibjbibi: do i = n0ikl, n1ikl
                                    if (i == j) cycle i_aibjbibi
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibjbibi(a, i, b, j)
                              end do i_aibjbibi
                        end do a_aibjbibi
                  end do j_aibjbibi
            end do b_aibjbibi
            !
            ! Elementary loop  6
            ! --------------------
            ! Free virtual indices: b, a
            ! Free occupied indices: i, j
            ! Equalities: c == b, d == b, l == i, k == j
            ! No equalities independent of the above can hold.
            !
            b_aibjbjbi: do b = n0bcd, n1bcd
                  j_aibjbjbi: do j = n0jk, n1jk
                        bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                        ck = (b - nvirt0) * nocc + (j - nocc0) + 1
                        a0 = max(b + 1, n0a)
                        a_aibjbjbi: do a = a0, n1a
                              if (a == b) cycle a_aibjbjbi
                              i1 = min(j - 1, n1il)
                              i_aibjbjbi: do i = n0il, i1
                                    if (i == j) cycle i_aibjbjbi
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibjbjbi(a, i, b, j)
                              end do i_aibjbjbi
                        end do a_aibjbjbi
                  end do j_aibjbjbi
            end do b_aibjbjbi
            !
            ! Elementary loop  7
            ! --------------------
            ! Free virtual indices: b, a
            ! Free occupied indices: j, i
            ! Equalities: c == b, d == b, k == j, l == j
            ! No equalities independent of the above can hold.
            !
            b_aibjbjbj: do b = n0bcd, n1bcd
                  j_aibjbjbj: do j = n0jkl, n1jkl
                        bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                        ck = (b - nvirt0) * nocc + (j - nocc0) + 1
                        dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                        a0 = max(b + 1, n0a)
                        a_aibjbjbj: do a = a0, n1a
                              if (a == b) cycle a_aibjbjbj
                              i_aibjbjbj: do i = n0i, n1i
                                    if (i == j) cycle i_aibjbjbj
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibjbjbj(a, i, b, j)
                              end do i_aibjbjbj
                        end do a_aibjbjbj
                  end do j_aibjbjbj
            end do b_aibjbjbj
            !
            ! Elementary loop  8
            ! --------------------
            ! Free virtual indices: b, a
            ! Free occupied indices: i, j
            ! Equalities: c == b, d == b, k == i, l == j
            ! No equalities independent of the above can hold.
            !
            b_aibjbibj: do b = n0bcd, n1bcd
                  j_aibjbibj: do j = n0jl, n1jl
                        bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                        dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                        a0 = max(b + 1, n0a)
                        a_aibjbibj: do a = a0, n1a
                              if (a == b) cycle a_aibjbibj
                              i0 = max(j + 1, n0ik)
                              i_aibjbibj: do i = i0, n1ik
                                    if (i == j) cycle i_aibjbibj
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibjbibj(a, i, b, j)
                              end do i_aibjbibj
                        end do a_aibjbibj
                  end do j_aibjbibj
            end do b_aibjbibj
            !
            ! Elementary loop  9
            ! --------------------
            ! Free virtual indices: b, a
            ! Free occupied indices: i, l
            ! Equalities: c == b, d == b, j == i, k == i
            ! No equalities independent of the above can hold.
            !
            l_aibibibl: do l = n0l, n1l
                  b_aibibibl: do b = n0bcd, n1bcd
                        dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                        a0 = max(b + 1, n0a)
                        a_aibibibl: do a = a0, n1a
                              if (a == b) cycle a_aibibibl
                              i0 = max(l + 1, n0ijk)
                              i_aibibibl: do i = i0, n1ijk
                                    if (i == l) cycle i_aibibibl
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibibibl(a, i, b, l)
                              end do i_aibibibl
                        end do a_aibibibl
                  end do b_aibibibl
            end do l_aibibibl
            !
            ! Elementary loop  10
            ! --------------------
            ! Free virtual indices: a, c
            ! Free occupied indices: i, j
            ! Equalities: b == a, d == c, k == i, l == j
            ! No equalities independent of the above can hold.
            !
            c_aiajcicj: do c = n0cd, n1cd
                  j_aiajcicj: do j = n0jl, n1jl
                        dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                        a_aiajcicj: do a = n0ab, n1ab
                              if (a == c) cycle a_aiajcicj
                              bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                              i0 = max(j + 1, n0ik)
                              i_aiajcicj: do i = i0, n1ik
                                    if (i == j) cycle i_aiajcicj
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (c - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aiajcicj(a, c)
                              end do i_aiajcicj
                        end do a_aiajcicj
                  end do j_aiajcicj
            end do c_aiajcicj
            !
            ! Elementary loop  11
            ! --------------------
            ! Free virtual indices: c, a, b
            ! Free occupied indices: i
            ! Equalities: d == c, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            c_aibicici: do c = n0cd, n1cd
                  b_aibicici: do b = n0b, n1b
                        if (b == c) cycle b_aibicici
                        a0 = max(b + 1, n0a)
                        a_aibicici: do a = a0, n1a
                              if (a == b .or. a == c) cycle a_aibicici
                              i_aibicici: do i = n0ijkl, n1ijkl
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (c - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibicici(a, b, c)
                              end do i_aibicici
                        end do a_aibicici
                  end do b_aibicici
            end do c_aibicici
            !
            ! Elementary loop  12
            ! --------------------
            ! Free virtual indices: b, a, d
            ! Free occupied indices: i
            ! Equalities: c == b, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            d_aibibidi: do d = n0d, n1d
                  b0 = max(d + 1, n0bc)
                  b_aibibidi: do b = b0, n1bc
                        if (b == d) cycle b_aibibidi
                        a0 = max(b + 1, n0a)
                        a_aibibidi: do a = a0, n1a
                              if (a == b .or. a == d) cycle a_aibibidi
                              i_aibibidi: do i = n0ijkl, n1ijkl
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibibidi(a, i, b, d)
                              end do i_aibibidi
                        end do a_aibibidi
                  end do b_aibibidi
            end do d_aibibidi
            !
            ! Elementary loop  13
            ! --------------------
            ! Free virtual indices: a, c
            ! Free occupied indices: i, k
            ! Equalities: b == a, d == a, j == i, l == i
            ! No equalities independent of the above can hold.
            !
            c_aiaickai: do c = n0c, n1c
                  k_aiaickai: do k = n0k, n1k
                        ck = (c - nvirt0) * nocc + (k - nocc0) + 1
                        a1 = min(c - 1, n1abd)
                        a_aiaickai: do a = n0abd, a1
                              if (a == c) cycle a_aiaickai
                              i_aiaickai: do i = n0ijl, n1ijl
                                    if (i == k) cycle i_aiaickai
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aiaickai(a, i, c, k)
                              end do i_aiaickai
                        end do a_aiaickai
                  end do k_aiaickai
            end do c_aiaickai
            !
            ! Elementary loop  14
            ! --------------------
            ! Free virtual indices: a, c
            ! Free occupied indices: i, j
            ! Equalities: b == a, d == a, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            c_aiajciai: do c = n0c, n1c
                  j_aiajciai: do j = n0j, n1j
                        a1 = min(c - 1, n1abd)
                        a_aiajciai: do a = n0abd, a1
                              if (a == c) cycle a_aiajciai
                              bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                              i0 = max(j + 1, n0ikl)
                              i_aiajciai: do i = i0, n1ikl
                                    if (i == j) cycle i_aiajciai
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (c - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aiajciai(a, i, j, c)
                              end do i_aiajciai
                        end do a_aiajciai
                  end do j_aiajciai
            end do c_aiajciai
            !
            ! Elementary loop  15
            ! --------------------
            ! Free virtual indices: a, c
            ! Free occupied indices: i, j
            ! Equalities: b == a, d == a, l == i, k == j
            ! No equalities independent of the above can hold.
            !
            c_aiajcjai: do c = n0c, n1c
                  j_aiajcjai: do j = n0jk, n1jk
                        ck = (c - nvirt0) * nocc + (j - nocc0) + 1
                        a1 = min(c - 1, n1abd)
                        a_aiajcjai: do a = n0abd, a1
                              if (a == c) cycle a_aiajcjai
                              bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                              i0 = max(j + 1, n0il)
                              i_aiajcjai: do i = i0, n1il
                                    if (i == j) cycle i_aiajcjai
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aiajcjai(a, i, j, c)
                              end do i_aiajcjai
                        end do a_aiajcjai
                  end do j_aiajcjai
            end do c_aiajcjai
            !
            ! Elementary loop  16
            ! --------------------
            ! Free virtual indices: a, c
            ! Free occupied indices: j, i
            ! Equalities: b == a, d == a, k == j, l == j
            ! No equalities independent of the above can hold.
            !
            c_aiajcjaj: do c = n0c, n1c
                  j_aiajcjaj: do j = n0jkl, n1jkl
                        ck = (c - nvirt0) * nocc + (j - nocc0) + 1
                        a1 = min(c - 1, n1abd)
                        a_aiajcjaj: do a = n0abd, a1
                              if (a == c) cycle a_aiajcjaj
                              bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                              dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                              i0 = max(j + 1, n0i)
                              i_aiajcjaj: do i = i0, n1i
                                    if (i == j) cycle i_aiajcjaj
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aiajcjaj(a, i, j, c)
                              end do i_aiajcjaj
                        end do a_aiajcjaj
                  end do j_aiajcjaj
            end do c_aiajcjaj
            !
            ! Elementary loop  17
            ! --------------------
            ! Free virtual indices: a, c
            ! Free occupied indices: i, j
            ! Equalities: b == a, d == a, k == i, l == j
            ! No equalities independent of the above can hold.
            !
            c_aiajciaj: do c = n0c, n1c
                  j_aiajciaj: do j = n0jl, n1jl
                        a1 = min(c - 1, n1abd)
                        a_aiajciaj: do a = n0abd, a1
                              if (a == c) cycle a_aiajciaj
                              bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                              dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                              i0 = max(j + 1, n0ik)
                              i_aiajciaj: do i = i0, n1ik
                                    if (i == j) cycle i_aiajciaj
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (c - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aiajciaj(a, i, j, c)
                              end do i_aiajciaj
                        end do a_aiajciaj
                  end do j_aiajciaj
            end do c_aiajciaj
            !
            ! Elementary loop  18
            ! --------------------
            ! Free virtual indices: a, c
            ! Free occupied indices: i, l
            ! Equalities: b == a, d == a, j == i, k == i
            ! No equalities independent of the above can hold.
            !
            l_aiaicial: do l = n0l, n1l
                  c_aiaicial: do c = n0c, n1c
                        a1 = min(c - 1, n1abd)
                        a_aiaicial: do a = n0abd, a1
                              if (a == c) cycle a_aiaicial
                              dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                              i_aiaicial: do i = n0ijk, n1ijk
                                    if (i == l) cycle i_aiaicial
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (c - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aiaicial(a, i, c, l)
                              end do i_aiaicial
                        end do a_aiaicial
                  end do c_aiaicial
            end do l_aiaicial
            !
            ! Elementary loop  19
            ! --------------------
            ! Free virtual indices: a, b, c
            ! Free occupied indices: i
            ! Equalities: d == a, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            c_aibiciai: do c = n0c, n1c
                  b_aibiciai: do b = n0b, n1b
                        if (b == c) cycle b_aibiciai
                        a0 = max(b + 1, n0ad)
                        a1 = min(c - 1, n1ad)
                        a_aibiciai: do a = a0, a1
                              if (a == b .or. a == c) cycle a_aibiciai
                              i_aibiciai: do i = n0ijkl, n1ijkl
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (c - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibiciai(a, i, b, c)
                              end do i_aibiciai
                        end do a_aibiciai
                  end do b_aibiciai
            end do c_aibiciai
            !
            ! Elementary loop  20
            ! --------------------
            ! Free virtual indices: a, c, d
            ! Free occupied indices: i
            ! Equalities: b == a, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            d_aiaicidi: do d = n0d, n1d
                  c0 = max(d + 1, n0c)
                  c_aiaicidi: do c = c0, n1c
                        if (c == d) cycle c_aiaicidi
                        a_aiaicidi: do a = n0ab, n1ab
                              if (a == c .or. a == d) cycle a_aiaicidi
                              i_aiaicidi: do i = n0ijkl, n1ijkl
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (c - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aiaicidi(a, c, d)
                              end do i_aiaicidi
                        end do a_aiaicidi
                  end do c_aiaicidi
            end do d_aiaicidi
            !
            ! Elementary loop  21
            ! --------------------
            ! Free virtual indices: b, a, c
            ! Free occupied indices: i
            ! Equalities: d == b, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            c_aibicibi: do c = n0c, n1c
                  b1 = min(c - 1, n1bd)
                  b_aibicibi: do b = n0bd, b1
                        if (b == c) cycle b_aibicibi
                        a0 = max(b + 1, n0a)
                        a_aibicibi: do a = a0, n1a
                              if (a == b .or. a == c) cycle a_aibicibi
                              i_aibicibi: do i = n0ijkl, n1ijkl
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ck = (c - nvirt0) * nocc + (i - nocc0) + 1
                                    dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    iket = ketoffset + &
                                          ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                                    hci(ibra, iket) = cisd22_aibicibi(a, i, b, c)
                              end do i_aibicibi
                        end do a_aibicibi
                  end do b_aibicibi
            end do c_aibicibi
            !
            ! Elementary loop  22
            ! --------------------
            ! Free virtual indices: a
            ! Free occupied indices: i, k
            ! Equalities: b == a, c == a, d == a, j == i, l == i
            ! No equalities independent of the above can hold.
            !
            k_aiaiakai: do k = n0k, n1k
                  a_aiaiakai: do a = n0abcd, n1abcd
                        ck = (a - nvirt0) * nocc + (k - nocc0) + 1
                        i1 = min(k - 1, n1ijl)
                        i_aiaiakai: do i = n0ijl, i1
                              if (i == k) cycle i_aiaiakai
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              bj = (a - nvirt0) * nocc + (i - nocc0) + 1
                              dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              iket = ketoffset + &
                                    ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                              hci(ibra, iket) = cisd22_aiaiakai(a, i, k)
                        end do i_aiaiakai
                  end do a_aiaiakai
            end do k_aiaiakai
            !
            ! Elementary loop  23
            ! --------------------
            ! Free virtual indices: a
            ! Free occupied indices: i, j
            ! Equalities: b == a, c == a, d == a, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            j_aiajaiai: do j = n0j, n1j
                  a_aiajaiai: do a = n0abcd, n1abcd
                        bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                        i0 = max(j + 1, n0ikl)
                        i_aiajaiai: do i = i0, n1ikl
                              if (i == j) cycle i_aiajaiai
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ck = (a - nvirt0) * nocc + (i - nocc0) + 1
                              dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              iket = ketoffset + &
                                    ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                              hci(ibra, iket) = cisd22_aiajaiai(a, i, j)
                        end do i_aiajaiai
                  end do a_aiajaiai
            end do j_aiajaiai
            !
            ! Elementary loop  24
            ! --------------------
            ! Free virtual indices: a
            ! Free occupied indices: j, i
            ! Equalities: b == a, c == a, d == a, k == j, l == j
            ! No equalities independent of the above can hold.
            !
            j_aiajajaj: do j = n0jkl, n1jkl
                  a_aiajajaj: do a = n0abcd, n1abcd
                        bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                        ck = (a - nvirt0) * nocc + (j - nocc0) + 1
                        dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                        i0 = max(j + 1, n0i)
                        i_aiajajaj: do i = i0, n1i
                              if (i == j) cycle i_aiajajaj
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              iket = ketoffset + &
                                    ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                              hci(ibra, iket) = cisd22_aiajajaj(a, i, j)
                        end do i_aiajajaj
                  end do a_aiajajaj
            end do j_aiajajaj
            !
            ! Elementary loop  25
            ! --------------------
            ! Free virtual indices: a
            ! Free occupied indices: i, j
            ! Equalities: b == a, c == a, d == a, k == i, l == j
            ! No equalities independent of the above can hold.
            !
            j_aiajaiaj: do j = n0jl, n1jl
                  a_aiajaiaj: do a = n0abcd, n1abcd
                        bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                        dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                        i0 = max(j + 1, n0ik)
                        i_aiajaiaj: do i = i0, n1ik
                              if (i == j) cycle i_aiajaiaj
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ck = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              iket = ketoffset + &
                                    ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                              hci(ibra, iket) = cisd22_aiajaiaj(eorb, nocc, a, i, j)
                        end do i_aiajaiaj
                  end do a_aiajaiaj
            end do j_aiajaiaj
            !
            ! Elementary loop  26
            ! --------------------
            ! Free virtual indices: a
            ! Free occupied indices: i, k
            ! Equalities: b == a, c == a, d == a, j == i, l == k
            ! No equalities independent of the above can hold.
            !
            k_aiaiakak: do k = n0kl, n1kl
                  a_aiaiakak: do a = n0abcd, n1abcd
                        ck = (a - nvirt0) * nocc + (k - nocc0) + 1
                        dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                        i_aiaiakak: do i = n0ij, n1ij
                              if (i == k) cycle i_aiaiakak
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              bj = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              iket = ketoffset + &
                                    ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                              hci(ibra, iket) = cisd22_aiaiakak(i, k)
                        end do i_aiaiakak
                  end do a_aiaiakak
            end do k_aiaiakak
            !
            ! Elementary loop  27
            ! --------------------
            ! Free virtual indices: a
            ! Free occupied indices: i, l
            ! Equalities: b == a, c == a, d == a, j == i, k == i
            ! No equalities independent of the above can hold.
            !
            l_aiaiaial: do l = n0l, n1l
                  a_aiaiaial: do a = n0abcd, n1abcd
                        dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                        i0 = max(l + 1, n0ijk)
                        i_aiaiaial: do i = i0, n1ijk
                              if (i == l) cycle i_aiaiaial
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              bj = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ck = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              iket = ketoffset + &
                                    ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                              hci(ibra, iket) = cisd22_aiaiaial(a, i, l)
                        end do i_aiaiaial
                  end do a_aiaiaial
            end do l_aiaiaial
            !
            ! Elementary loop  28
            ! --------------------
            ! Free virtual indices: a, b
            ! Free occupied indices: i
            ! Equalities: c == a, d == a, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            b_aibiaiai: do b = n0b, n1b
                  a0 = max(b + 1, n0acd)
                  a_aibiaiai: do a = a0, n1acd
                        if (a == b) cycle a_aibiaiai
                        i_aibiaiai: do i = n0ijkl, n1ijkl
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                              ck = (a - nvirt0) * nocc + (i - nocc0) + 1
                              dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              iket = ketoffset + &
                                    ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                              hci(ibra, iket) = cisd22_aibiaiai(a, i, b)
                        end do i_aibiaiai
                  end do a_aibiaiai
            end do b_aibiaiai
            !
            ! Elementary loop  29
            ! --------------------
            ! Free virtual indices: a, d
            ! Free occupied indices: i
            ! Equalities: b == a, c == a, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            d_aiaiaidi: do d = n0d, n1d
                  a0 = max(d + 1, n0abc)
                  a_aiaiaidi: do a = a0, n1abc
                        if (a == d) cycle a_aiaiaidi
                        i_aiaiaidi: do i = n0ijkl, n1ijkl
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              bj = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ck = (a - nvirt0) * nocc + (i - nocc0) + 1
                              dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              iket = ketoffset + &
                                    ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                              hci(ibra, iket) = cisd22_aiaiaidi(a, i, d)
                        end do i_aiaiaidi
                  end do a_aiaiaidi
            end do d_aiaiaidi
            !
            ! Elementary loop  30
            ! --------------------
            ! Free virtual indices: a, b
            ! Free occupied indices: i
            ! Equalities: c == a, d == b, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            b_aibiaibi: do b = n0bd, n1bd
                  a0 = max(b + 1, n0ac)
                  a_aibiaibi: do a = a0, n1ac
                        if (a == b) cycle a_aibiaibi
                        i_aibiaibi: do i = n0ijkl, n1ijkl
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                              ck = (a - nvirt0) * nocc + (i - nocc0) + 1
                              dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              iket = ketoffset + &
                                    ((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
                              hci(ibra, iket) = cisd22_aibiaibi(eorb, nocc, a, i, b)
                        end do i_aibiaibi
                  end do a_aibiaibi
            end do b_aibiaibi
      end subroutine cisd_block_22_part4
end module cisd_block_22_mod_part4
