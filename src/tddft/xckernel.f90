module xckernel
      use gparam
      use grid

      implicit none

contains
      
      pure subroutine ugga_fxccontrib(fpqrs_aa, fpqrs_ab, fpqrs_bb, shellidx, orbval, n0, rhoa, rhob, &
            grhoa, grhob, vsigma, v2rho2, v2rhosigma, v2sigma2, weight, irs0, irs1)

            double precision, dimension(:), intent(inout) :: fpqrs_aa
            double precision, dimension(:), intent(inout) :: fpqrs_ab
            double precision, dimension(:), intent(inout) :: fpqrs_bb
            integer, dimension(:), intent(in)             :: shellidx
            double precision, dimension(:), intent(in)    :: orbval
            integer, intent(in)                           :: n0
            double precision, intent(in)                  :: rhoa
            double precision, intent(in)                  :: rhob
            double precision, dimension(:, :), intent(in) :: grhoa
            double precision, dimension(:, :), intent(in) :: grhob
            double precision, dimension(3), intent(in)    :: vsigma
            double precision, dimension(3), intent(in)    :: v2rho2
            double precision, dimension(6), intent(in)    :: v2rhosigma
            double precision, dimension(6), intent(in)    :: v2sigma2
            double precision, intent(in)                  :: weight
            integer, intent(in)                           :: irs0
            integer, intent(in)                           :: irs1

            double precision :: p_0, q_0, r_0, s_0
            double precision, dimension(3) :: gp, gq, gr, gs
            double precision :: pq, rs
            double precision, dimension(3) :: gpq, grs
            double precision :: gpq_grhoa, grs_grhoa, gpq_grhob, grs_grhob
            double precision, dimension(3) :: pq_grhoa, pq_grhob
            double precision :: faa1, faa2, faa3, faa4, faa5, faa6, faa7, faa8, faa9, faa10
            double precision :: fbb1, fbb2, fbb3, fbb4, fbb5, fbb6, fbb7, fbb8, fbb9, fbb10
            double precision :: fab1, fab2, fab3, fab4, fab5, fab6, fab7, fab8, fab9, fab10
            double precision, dimension(3) :: aa1, aa3, aa5
            double precision, dimension(3) :: bb1, bb3, bb5
            double precision, dimension(3) :: ab1, ab3, ab5
            double precision :: aa2, aa4, aa6, aa7, aa8, aa9, aa10
            double precision :: bb2, bb4, bb6, bb7, bb8, bb9, bb10
            double precision :: ab2, ab4, ab6, ab7, ab8, ab9, ab10
            double precision :: aa_rs, aa_grs_grhoa, aa_grs_grhob
            double precision :: bb_rs, bb_grs_grhob, bb_grs_grhoa
            double precision :: ab_rs, ab_grs_grhoa, ab_grs_grhob
            double precision, dimension(3) :: ab_grs
            double precision, dimension(3) :: aa_grs
            double precision, dimension(3) :: bb_grs
            double precision :: taa1, taa2, taa3, taa4
            double precision :: tbb1, tbb2, tbb3, tbb4
            double precision :: tab1, tab2, tab3, tab4
            integer :: k, l, m, n
            integer :: mm0, nn0
            integer :: u, uu, v, vv, x, xx, y, yy
            integer :: p, q, r, s
            integer :: ipq, irs
            integer :: maxp, maxq, maxr, maxs
            double precision :: weight2, weight4
            double precision :: vsigma_aa, vsigma_ab, vsigma_bb
            double precision :: v2rho2_aa, v2rho2_ab, v2rho2_bb
            double precision :: v2rhosigma_a_aa, v2rhosigma_b_aa, v2rhosigma_a_ab
            double precision :: v2rhosigma_b_ab, v2rhosigma_a_bb, v2rhosigma_b_bb
            double precision :: v2sigma2_aa_aa, v2sigma2_aa_bb, v2sigma2_aa_ab
            double precision :: v2sigma2_bb_ab, v2sigma2_ab_ab, v2sigma2_bb_bb
            integer :: maxipq, ipqrs
            integer, parameter :: nrecords = 4

            if ((rhoa+rhob) < GRID_RHOTHRESH .or. n0 == 0) then
                  return
            end if

            maxipq = fpqrs_ipq(NORB, NORB, NORB)

            vsigma_aa = vsigma(IVSIGMA_AA)
            vsigma_ab = vsigma(IVSIGMA_AB)
            vsigma_bb = vsigma(IVSIGMA_BB)

            v2rho2_aa = v2rho2(IV2RHO2_AA)
            v2rho2_ab = v2rho2(IV2RHO2_AB)
            v2rho2_bb = v2rho2(IV2RHO2_BB)

            v2rhosigma_a_aa = v2rhosigma(IV2RHOSIGMA_A_AA)
            v2rhosigma_b_bb = v2rhosigma(IV2RHOSIGMA_B_BB)
            v2rhosigma_b_aa = v2rhosigma(IV2RHOSIGMA_B_AA)
            v2rhosigma_a_ab = v2rhosigma(IV2RHOSIGMA_A_AB)
            v2rhosigma_b_ab = v2rhosigma(IV2RHOSIGMA_B_AB)
            v2rhosigma_a_bb = v2rhosigma(IV2RHOSIGMA_A_BB)

            v2sigma2_aa_aa = v2sigma2(IV2SIGMA2_AA_AA)
            v2sigma2_aa_bb = v2sigma2(IV2SIGMA2_AA_BB)
            v2sigma2_aa_ab = v2sigma2(IV2SIGMA2_AA_AB)
            v2sigma2_bb_ab = v2sigma2(IV2SIGMA2_AB_BB)
            v2sigma2_ab_ab = v2sigma2(IV2SIGMA2_AB_AB)
            v2sigma2_bb_bb = v2sigma2(IV2SIGMA2_BB_BB)

            weight2 = weight * 2.d+0
            weight4 = weight * 4.d+0
            !
            ! Premultiply alpha-alpha derivatives by constant factors
            !
            faa1  = vsigma_aa       * weight2
            faa2  = v2rho2_aa       * weight
            faa3  = v2rhosigma_a_aa * weight2
            faa4  = faa3
            faa5  = v2rhosigma_a_ab * weight
            faa6  = faa5
            faa7  = v2sigma2_aa_aa  * weight4
            faa8  = v2sigma2_aa_ab  * weight2
            faa9  = faa8
            faa10 = v2sigma2_ab_ab  * weight
            !
            ! Premultiply beta-beta derivatives by constant factors
            !
            fbb1  = vsigma_bb       * weight2
            fbb2  = v2rho2_bb       * weight
            fbb3  = v2rhosigma_b_bb * weight2
            fbb4  = fbb3
            fbb5  = v2rhosigma_b_ab * weight
            fbb6  = fbb5
            fbb7  = v2sigma2_bb_bb  * weight4
            fbb8  = v2sigma2_bb_ab  * weight2
            fbb9  = fbb8
            fbb10 = v2sigma2_ab_ab  * weight
            !
            ! Premultiply alpha-beta derivatives by constant factors
            !
            fab1  = vsigma_ab       * weight
            fab2  = v2rho2_ab       * weight
            fab3  = v2rhosigma_a_bb * weight2
            fab4  = v2rhosigma_b_aa * weight2
            fab5  = v2rhosigma_a_ab * weight
            fab6  = v2rhosigma_b_ab * weight
            fab7  = v2sigma2_aa_bb  * weight4
            fab8  = v2sigma2_aa_ab  * weight2
            fab9  = v2sigma2_bb_ab  * weight2
            fab10 = v2sigma2_ab_ab  * weight

            k = 1
            do uu = 1, n0
                  u = shellidx(uu)
                  maxp = shpos(u+1) - 1
                  do p =  shpos(u), maxp
                        p_0 = orbval(k)
                        gp(1) = orbval(k + 1)
                        gp(2) = orbval(k + 2)
                        gp(3) = orbval(k + 3)
                        k = k + nrecords
                        l = 1
                        do vv = 1, uu
                              v = shellidx(vv)
                              maxq = min(shpos(v+1)-1, p)
                              do q = shpos(v), maxq
                                    q_0 = orbval(l)
                                    gq(1) = orbval(l + 1)
                                    gq(2) = orbval(l + 2)
                                    gq(3) = orbval(l + 3)
                                    l = l + nrecords
                                    call utpq(p_0, gp, q_0, gq, grhoa, grhob, &
                                          pq, gpq, pq_grhoa, pq_grhob, gpq_grhoa, gpq_grhob)
                                    
                                    aa1  = gpq       * faa1
                                    aa2  = pq        * faa2
                                    aa3  = pq_grhoa  * faa3
                                    aa4  = gpq_grhoa * faa4
                                    aa5  = pq_grhob  * faa5
                                    aa6  = gpq_grhob * faa6
                                    aa7  = gpq_grhoa * faa7
                                    aa8  = gpq_grhoa * faa8
                                    aa9  = gpq_grhob * faa9
                                    aa10 = gpq_grhob * faa10

                                    bb1  = gpq       * fbb1
                                    bb2  = pq        * fbb2
                                    bb3  = pq_grhob  * fbb3
                                    bb4  = gpq_grhob * fbb4
                                    bb5  = pq_grhoa  * fbb5
                                    bb6  = gpq_grhoa * fbb6
                                    bb7  = gpq_grhob * fbb7
                                    bb8  = gpq_grhob * fbb8
                                    bb9  = gpq_grhoa * fbb9
                                    bb10 = gpq_grhoa * fbb10

                                    aa_grs       = aa1 + aa3 + aa5
                                    aa_rs        = aa2 + aa4 + aa6
                                    aa_grs_grhoa = aa7 + aa9
                                    aa_grs_grhob = aa8 + aa10

                                    bb_grs       = bb1 + bb3 + bb5
                                    bb_rs        = bb2 + bb4 + bb6
                                    bb_grs_grhob = bb7 + bb9
                                    bb_grs_grhoa = bb8 + bb10

                                    ab1  = gpq       * fab1
                                    ab2  = pq        * fab2
                                    ab3  = pq_grhob  * fab3
                                    ab4  = gpq_grhoa * fab4
                                    ab5  = pq_grhoa  * fab5
                                    ab6  = gpq_grhob * fab6
                                    ab7  = gpq_grhoa * fab7
                                    ab8  = gpq_grhoa * fab8
                                    ab9  = gpq_grhob * fab9
                                    ab10 = gpq_grhob * fab10

                                    ab_grs       = ab1 + ab3 + ab5
                                    ab_rs        = ab2 + ab4 + ab6
                                    ab_grs_grhoa = ab8 + ab10
                                    ab_grs_grhob = ab7 + ab9

                                    ipq = fpqrs_ipq(p, q, NORB)

                                    mm0 = 1
                                    xxloop: do xx = 1, uu
                                          x = shellidx(xx)
                                          if (x == u) then
                                                maxr = p
                                          else
                                                maxr = shpos(x+1) - 1
                                          end if
                                          nn0 = 1
                                          yyloop: do yy = 1, xx
                                                y = shellidx(yy)
                                                !
                                                ! Compute index of the first pair of orbitals belonging
                                                ! to the ket shell pair. Jump to the next shell if it does not
                                                ! belong to the requested set of AO orbital indices.
                                                ! This is the mechanism for incremantal building of 
                                                ! the orbital Hessian matrix with low memory cost
                                                !
                                                irs = fpqrs_ipq(shpos(x), shpos(y), NORB)
                                                if (irs > irs1) then
                                                      exit xxloop
                                                else if (irs < irs0) then
                                                      nn0 = nn0 + nrecords * (shpos(y+1)-shpos(y))
                                                      cycle yyloop
                                                end if
                                                
                                                m = mm0
                                                do r = shpos(x), maxr
                                                      r_0 = orbval(m)
                                                      gr(1) = orbval(m + 1)
                                                      gr(2) = orbval(m + 2)
                                                      gr(3) = orbval(m + 3)
                                                      m = m + nrecords
                                                      if (r == p) then
                                                            maxs = min(shpos(y+1)-1, q)
                                                      else
                                                            maxs = min(shpos(y+1)-1, r)
                                                      end if

                                                      n = n0
                                                      do s = shpos(y), maxs
                                                            irs = fpqrs_ipq(r, s, NORB)
                                                            irs = irs - irs0 + 1
                                                            s_0 = orbval(n)
                                                            gs(1) = orbval(n + 1)
                                                            gs(2) = orbval(n + 2)
                                                            gs(3) = orbval(n + 3)
                                                            n = n + nrecords
                                                            call utrs(r_0, gr, s_0, gs, grhoa, grhob, &
                                                                  rs, grs, grs_grhoa, grs_grhob)

                                                            taa1 = dotp3d(aa_grs, grs)
                                                            taa2 = aa_rs * rs
                                                            taa3 = aa_grs_grhoa * grs_grhoa
                                                            taa4 = aa_grs_grhob * grs_grhob

                                                            tbb1 = dotp3d(bb_grs, grs)
                                                            tbb2 = bb_rs * rs
                                                            tbb3 = bb_grs_grhob * grs_grhob
                                                            tbb4 = bb_grs_grhoa * grs_grhoa

                                                            tab1 = dotp3d(ab_grs, grs)
                                                            tab2 = ab_rs * rs
                                                            tab3 = ab_grs_grhoa * grs_grhoa
                                                            tab4 = ab_grs_grhob * grs_grhob

                                                            ipqrs = fpqrs_ipq(ipq, irs, maxipq)
                                                            fpqrs_aa(ipqrs) = fpqrs_aa(ipqrs) &
                                                                  + taa1 + taa2 + taa3 + taa4
                                                            fpqrs_bb(ipqrs) = fpqrs_bb(ipqrs) &
                                                                  + tbb1 + tbb2 + tbb3 + tbb4
                                                            fpqrs_ab(ipqrs) = fpqrs_ab(ipqrs) &
                                                                  + tab1 + tab2 + tab3 + tab4
                                                      end do
                                                end do
                                                nn0 = nn0 + nrecords * (shpos(y+1)-shpos(y))
                                          end do yyloop
                                          mm0 = mm0 + nrecords * (shpos(x+1)-shpos(x))
                                    end do xxloop
                              end do
                        end do
                  end do
            end do
      end subroutine ugga_fxccontrib


      pure function dotp3d(a, b)
            double precision :: dotp3d
            double precision, dimension(3), intent(in) :: a
            double precision, dimension(3), intent(in) :: b

            dotp3d = a(1) * b(1) + a(2) * b(2) + a(3) * b(3)
      end function dotp3d


      pure subroutine utpq(p_0, gp, q_0, gq, grhoa, grhob, &
            pq, gpq, pq_grhoa, pq_grhob, gpq_grhoa, gpq_grhob)
            !
            ! pq = \phi_p * \phi_q
            ! gpq = \nabla(\phi_p \phi_q)
            ! pq_grhoa = \phi_p \phi_q \nabla \rho_\alpha
            ! gpq_grhoa = \nabla(\phi_p \phi_q) \cdot \nabla \rho_\alpha
            !
            double precision, intent(in)                :: p_0
            double precision, dimension(3), intent(in)  :: gp
            double precision, intent(in)                :: q_0
            double precision, dimension(3), intent(in)  :: gq
            double precision, dimension(3), intent(in)  :: grhoa
            double precision, dimension(3), intent(in)  :: grhob
            double precision, intent(out)               :: pq
            double precision, dimension(3), intent(out) :: gpq
            double precision, dimension(3), intent(out) :: pq_grhoa
            double precision, dimension(3), intent(out) :: pq_grhob
            double precision, intent(out)               :: gpq_grhoa
            double precision, intent(out)               :: gpq_grhob

            pq = p_0 * q_0
            gpq = p_0 * gq + gp * q_0
            pq_grhoa = pq * grhoa
            pq_grhob = pq * grhob
            gpq_grhoa = dotp3d(gpq, grhoa)
            gpq_grhob = dotp3d(gpq, grhob)
      end subroutine utpq


      pure subroutine utrs(r_0, gr, s_0, gs, grhoa, grhob, rs, grs, grs_grhoa, grs_grhob)
            double precision, intent(in)                :: r_0
            double precision, dimension(3), intent(in)  :: gr
            double precision, intent(in)                :: s_0
            double precision, dimension(3), intent(in)  :: gs
            double precision, dimension(3), intent(in)  :: grhoa
            double precision, dimension(3), intent(in)  :: grhob
            double precision, intent(out)               :: rs
            double precision, dimension(3), intent(out) :: grs
            double precision, intent(out)               :: grs_grhoa
            double precision, intent(out)               :: grs_grhob

            rs = r_0 * s_0
            grs = r_0 * gs + gr * s_0
            grs_grhoa = dotp3d(grs, grhoa)
            grs_grhob = dotp3d(grs, grhob)
      end subroutine utrs


      pure function fpqrs_ipq(p, q, m)
            ! ----------------------------------------------------------------
            ! Compute compound 2-index, (pq), assuming that p is always
            ! greater or equal q. Min. index: 1, max index: M.
            ! Example: P and Q enumerate, respectively, rows and columns
            ! of a symmetric matrix. The compound 2-index enumerates 
            ! consecutive elements of the lower triangle of the 5x5 matrix.
            !
            ! M = 5
            !             Q 
            !    | 1                |
            !    | 2  6             |
            ! P  | 3  7  10         |
            !    | 4  8  11  13     |
            !    | 5  9  12  14  15 |
            !
            integer             :: fpqrs_ipq
            integer, intent(in) :: p
            integer, intent(in) :: q
            integer, intent(in) :: m

            integer :: i1, i2

            i1 = ((2 * m - q + 2) * (q - 1)) / 2
            i2 = p - q + 1
            fpqrs_ipq = i1 + i2
      end function fpqrs_ipq
end module xckernel
