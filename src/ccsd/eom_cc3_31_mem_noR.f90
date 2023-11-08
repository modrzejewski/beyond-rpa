
module eom_cc3_31_mem_noR
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    use arithmetic
    use cc3_intermediates
    use cc_gparams                                                                                                                                                    
    implicit none

    contains
                                                                                                         
        subroutine sub_eom_cc3_31_mem_noR_v0_z1(t2, nocc, nactive, a, i, b, j, c,  sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
        real(F64), dimension(:,:), intent(inout) :: d_small, sigrows
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0d,n1d,n0l,n1l
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                              
                integer, intent(in) :: ntrial, a, i, b, j, c                                                                                                        
                integer :: p1, p2, ai, bj, iket  ,e,k,l,d                                                                                                         
                
    if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,i) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))


sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 1
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,i,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,i) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 5
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,i,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,i) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,i) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 12
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,i) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,i,j) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 14
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,j) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,i) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 16
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,i) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,i,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 21
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,i) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,i) * read_ftvvvv(c, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,i) * read_ftvvvv(c, d, b, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,i) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,i,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,j) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 28
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,i) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 29
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,i,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,i) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 35
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,i) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,k,j) * tvoov(a, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,k,i) * tvoov(a, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,i,k) * tvoov(a, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,j,k) * tvoov(a, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,k,j) * tvoov(b, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 41
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,k,j) * tvoov(c, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 42
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,k) * tvoov(c, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 43
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,i,k) * tvoov(b, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,k,i) * tvoov(b, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,k,i) * tvoov(c, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 46
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,k) * tvoov(b, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,i,k) * tvoov(c, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 48
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,k,j) * tvvoo(a, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 49
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,k,i) * tvvoo(a, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 50
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,i,k) * tvvoo(a, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 51
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,j,k) * tvvoo(a, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 52
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,k,j) * tvvoo(b, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 53
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,k,j) * tvvoo(c, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 54
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,k) * tvvoo(c, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 55
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,i,k) * tvvoo(b, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 56
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,k,i) * tvvoo(b, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 57
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,k,i) * tvvoo(c, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 58
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,k) * tvvoo(b, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 59
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,i,k) * tvvoo(c, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 60
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,k,j) * toooo(l, i, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 61
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,k,i) * toooo(l, i, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 62
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,i,k) * toooo(l, i, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 63
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,j,k) * toooo(l, i, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 64
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,k,j) * toooo(l, i, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 65
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,k,j) * toooo(l, i, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 66
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,i,k) * toooo(l, i, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 67
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,k) * toooo(l, i, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 68
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,k,i) * toooo(l, j, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 69
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,k,i) * toooo(l, j, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 70
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,i,k) * toooo(l, j, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 71
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,k) * toooo(l, j, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 


    end subroutine sub_eom_cc3_31_mem_noR_v0_z1                                                                                                     
        subroutine sub_eom_cc3_31_mem_noR_v0_z2(t2, nocc, nactive, a, i, b, j, c, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0d,n1d,n0l,n1l
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                              
                integer, intent(in) :: ntrial, a, i, b, j, c                                                                                                        
                integer :: p1, p2, ai, bj, iket  ,e,k,l,d                                                                                                         
                
    if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,i,j) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 2
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,j) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,j) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,i,j) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 11
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 12
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,i,j) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 13
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,j) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,j) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 16
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,j) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 17
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,j) * read_ftvvvv(c, d, b, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 18
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,i,j) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,i) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 21
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,i) * read_ftvvvv(c, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,i,j) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,i) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,j) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 28
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,i,j) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 31
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,i) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,k,i) * tvoov(a, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 37
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,j,k) * tvoov(a, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 38
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,k,j) * tvoov(a, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,j,k) * tvoov(a, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,k,j) * tvoov(c, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,j,k) * tvoov(c, i, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,k,i) * tvoov(b, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,k,j) * tvoov(b, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,k,j) * tvoov(c, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,j,k) * tvoov(b, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 46
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,k) * tvoov(b, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 47
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,k) * tvoov(c, j, k, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 48
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,k,i) * tvvoo(a, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 49
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,j,k) * tvvoo(a, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 50
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,k,j) * tvvoo(a, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 51
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,j,k) * tvvoo(a, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 52
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,k,j) * tvvoo(c, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,j,k) * tvvoo(c, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 54
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,k,i) * tvvoo(b, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 55
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,k,j) * tvvoo(b, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 56
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,k,j) * tvvoo(c, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 57
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,j,k) * tvvoo(b, d, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 58
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,k) * tvvoo(b, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do k = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 59
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,k) * tvvoo(c, d, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 60
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,k,i) * toooo(l, j, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 61
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,c,j,k) * toooo(l, j, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 62
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,k,j) * toooo(l, i, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 63
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,c,j,k) * toooo(l, i, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 64
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,k,j) * toooo(l, i, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 65
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,j,k) * toooo(l, i, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 66
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,k,i) * toooo(l, j, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 67
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,k,j) * toooo(l, j, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 68
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,k,j) * toooo(l, j, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 69
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,j,k) * toooo(l, j, k, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 70
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,i,k) * toooo(l, j, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do k = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 71
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,i,k) * toooo(l, j, k, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 


    end subroutine sub_eom_cc3_31_mem_noR_v0_z2                                                                                                     
        subroutine sub_eom_cc3_31_mem_noR_v6_z34(t2, nocc, nactive, a, i, j, c, k, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0d,n1d,n0l,n1l
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                              
                integer, intent(in) :: ntrial, a, i, j, c, k                                                                                                        
                integer :: p1, p2, ai, bj, iket  ,e,m,l,d                                                                                                         
                
    if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 0
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,k,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 1
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,k) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,i) * tvoov(a, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,k) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 4
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,k,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * tvoov(a, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,k,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,k) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,k) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,k,i) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvoov(c, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 11
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,i) * tvoov(c, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,k,i) * read_ftvvvv(a, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,k) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 14
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,i) * read_ftvvvv(a, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 15
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,k) * read_ftvvvv(a, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,k,j) * read_ftvvvv(a, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 17
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 18
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,k,j) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,k) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 20
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,k) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 21
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,k,i) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 24
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,k,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,k) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,i) * tvvoo(a, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,k) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,k,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * tvvoo(c, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,k,j) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 31
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,k) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,k) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,k,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvvoo(a, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,i) * tvvoo(a, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 36
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,k) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,m) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,m,j) * tvoov(a, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 39
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,m,j) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 40
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,k) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,i,m) * tvoov(a, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,k,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,j,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 44
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,a,i,m) * tvoov(c, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,a,k,m) * tvoov(c, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 46
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,i,m) * tvoov(c, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 47
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,j,m) * tvoov(c, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 48
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,k) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 49
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,m) * tvvoo(a, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 50
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,m,j) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 51
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,m,j) * tvvoo(a, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 52
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,k) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,i,m) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 54
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,k,m) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 55
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,j,m) * tvvoo(a, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 56
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,a,i,m) * tvvoo(c, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 57
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,a,k,m) * tvvoo(c, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 58
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,i,m) * tvvoo(c, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 59
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,j,m) * tvvoo(c, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 60
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,m,k) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 61
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,i,m) * toooo(m, k, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 62
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,j) * toooo(m, i, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 63
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,j) * toooo(m, k, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 64
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,m,k) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 65
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,m) * toooo(m, j, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 66
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,k,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 67
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,j,m) * toooo(m, k, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 68
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,i,m) * toooo(m, k, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 69
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,k,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 70
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,a,i,m) * toooo(m, j, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 71
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,a,j,m) * toooo(m, i, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 


    end subroutine sub_eom_cc3_31_mem_noR_v6_z34                                                                                                     
        subroutine sub_eom_cc3_31_mem_noR_v6_z56(t2, nocc, nactive, a, i, b, j, k, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0d,n1d,n0l,n1l
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                              
                integer, intent(in) :: ntrial, a, i, b, j, k                                                                                                        
                integer :: p1, p2, ai, bj, iket  ,e,m,l,d                                                                                                         
                
    if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,k) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,k,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 2
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,k) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,k,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,k,j) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,k) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,j) * tvoov(b, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvoov(b, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,k,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,i) * tvoov(b, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvoov(b, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 11
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,k) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 12
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,k) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 13
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,k,i) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,k) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,k,j) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 16
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,k,j) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 17
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,k) * read_ftvvvv(b, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 18
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,j) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,k,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 21
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,i) * read_ftvvvv(b, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * read_ftvvvv(b, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,k) * read_ftvvvv(b, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,k) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,k,i) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,k) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,k,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 28
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,k,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,k) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,j) * tvvoo(a, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 31
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvvoo(a, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,k,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,i) * tvvoo(b, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvvoo(b, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,k) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,b,i,m) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 37
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,b,k,m) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 38
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,j,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,k,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,m,k) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,j,m) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,m,i) * tvoov(b, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,j) * tvoov(b, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,k) * tvoov(b, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,j,m) * tvoov(b, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 46
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvoov(b, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 47
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvoov(b, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 48
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,b,i,m) * tvvoo(a, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 49
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,b,k,m) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 50
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,j,m) * tvvoo(a, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 51
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,k,m) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 52
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,m,k) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,j,m) * tvvoo(b, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 54
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,m,i) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 55
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,j) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 56
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,k) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 57
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,j,m) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 58
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 59
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvvoo(b, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 60
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,i,m) * toooo(m, k, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 61
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,k,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 62
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,b,j,m) * toooo(m, k, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 63
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,b,k,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 64
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,k) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 65
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,j,m) * toooo(m, k, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 66
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,i) * toooo(m, j, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 67
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,m,j) * toooo(m, i, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 68
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,m,k) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 69
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,j,m) * toooo(m, i, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 70
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,i,m) * toooo(m, j, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 71
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,i,m) * toooo(m, k, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 


    end subroutine sub_eom_cc3_31_mem_noR_v6_z56                                                                                                     
        subroutine sub_eom_cc3_31_mem_noR_v06_z7(t2, nocc, nactive, a, i, j, c, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0d,n1d,n0l,n1l
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                              
                integer, intent(in) :: ntrial, a, i, j, c                                                                                                        
                integer :: p1, p2, ai, bj, iket  ,e,m,l,d                                                                                                         
                
    if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 0
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,i,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 1
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(c,e,j,i) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,i,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,i,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 5
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,i) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,i) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,i,i) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,i,i) * read_ftvvvv(a, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,i) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(c,e,j,i) * read_ftvvvv(a, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,i,j) * read_ftvvvv(a, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,i) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,i,i) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 18
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,i,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,i) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(c,e,j,i) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 21
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,i,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,i,j) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,i) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 24
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,i) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,i,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,i) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,m) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,m,j) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,i) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,i,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,j,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 33
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,i,m) * tvoov(c, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,j,m) * tvoov(c, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 35
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,a,i,m) * tvoov(c, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 36
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,i) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,m) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,m,j) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,i) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,i,m) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 41
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,j,m) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 42
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,i,m) * tvvoo(c, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,j,m) * tvvoo(c, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 44
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,a,i,m) * tvvoo(c, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,m,i) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 46
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,i,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,m,j) * toooo(m, i, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 48
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,m,i) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 49
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,i,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 50
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,j,m) * toooo(m, i, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 51
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,a,i,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 52
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,a,j,m) * toooo(m, i, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,a,i,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 


    end subroutine sub_eom_cc3_31_mem_noR_v06_z7                                                                                                     
        subroutine sub_eom_cc3_31_mem_noR_v06_z8(t2, nocc, nactive, a, i, j, c, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0d,n1d,n0l,n1l
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                              
                integer, intent(in) :: ntrial, a, i, j, c                                                                                                        
                integer :: p1, p2, ai, bj, iket  ,e,m,l,d                                                                                                         
                
    if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(c,e,i,j) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 1
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,j,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 4
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 5
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,j,j) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,i) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(c,e,i,j) * read_ftvvvv(a, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,i) * read_ftvvvv(a, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,j,i) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(c,e,j,j) * read_ftvvvv(a, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,j) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,j,j) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(c,e,i,j) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,j,i) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 21
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 22
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(c,e,j,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,j) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,j,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,j,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,m,i) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,j) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,j,m) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,m) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 31
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,j) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,j,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,a,j,m) * tvoov(c, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,i,m) * tvoov(c, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,j,m) * tvoov(c, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,m,i) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,j) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,j,m) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,i,m) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 40
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,m,j) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 41
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,c,j,m) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,a,j,m) * tvvoo(c, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,i,m) * tvvoo(c, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,a,j,m) * tvvoo(c, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,m,i) * toooo(m, j, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 46
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,m,j) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,j,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 48
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,i,m) * toooo(m, j, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 49
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,m,j) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 50
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,c,j,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 51
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,a,j,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 52
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,a,i,m) * toooo(m, j, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,a,j,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-8 .or. abs(tmp).gt.1.d-8) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-8) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 


    end subroutine sub_eom_cc3_31_mem_noR_v06_z8                                                                                                     
        subroutine sub_eom_cc3_31_mem_noR_v06_z9(t2, nocc, nactive, a, i, b, j, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0d,n1d,n0l,n1l
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                              
                integer, intent(in) :: ntrial, a, i, b, j                                                                                                        
                integer :: p1, p2, ai, bj, iket  ,e,m,l,d                                                                                                         
                
    if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 0
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,i,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr)), eorb, wr

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 1
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 2
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr)), eorb, wr, a, l

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 4
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,i,j) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 5
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,j,i) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr)), -t2(b,e,i,i) * tvoov(b, j, l, e)

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 9
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,i,i) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 10
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,i) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr)), t2(b,e,j,i) * read_ftvvvv(b, e, a, d)

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 11
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,j) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 12
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 13
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,i,j) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 14
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,j,i) * read_ftvvvv(b, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr)), 14, (-2.0_F64) ,  t2(a,e,j,i) , read_ftvvvv(b, e, b, d), e

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 15
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * read_ftvvvv(b, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 16
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 17
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,i) * read_ftvvvv(b, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 18
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,i,i) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 20
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 21
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 22
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,i,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 23
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,j,i) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 24
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,i) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 27
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,b,i,m) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,j,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 29
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,i,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,j) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 31
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,m,i) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 32
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,j,m) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 33
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,i) * tvoov(b, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvoov(b, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 36
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,b,i,m) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,j,m) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 38
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,i,m) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,j) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 40
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,m,i) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 41
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,j,m) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 42
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,i) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 45
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,b,i,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 46
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,b,j,m) * toooo(m, i, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 47
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,b,i,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 48
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,m,j) * toooo(m, i, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 49
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,m,i) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 50
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,j,m) * toooo(m, i, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 51
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,i,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 52
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,m,i) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

!print*, 'q', 53
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,i,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 


    end subroutine sub_eom_cc3_31_mem_noR_v06_z9                                                                                                     
        subroutine sub_eom_cc3_31_mem_noR_v06_z10(t2, nocc, nactive, a, i, b, j, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0d,n1d,n0l,n1l
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                              
                integer, intent(in) :: ntrial, a, i, b, j                                                                                                        
                integer :: p1, p2, ai, bj, iket  ,e,m,l,d                                                                                                         
                
    if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 

do p1 = 1, ntrial 

! 0
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 1
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,i,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,i,j) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 4
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 5
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,j,i) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,j) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,i) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,i,i) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,i,j) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,j,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,j) * read_ftvvvv(b, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,j,i) * read_ftvvvv(b, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,e,i,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,e,i,i) * read_ftvvvv(b, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 18
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,i,i) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,i,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 22
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,j,i) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,j,i) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,e,i,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,e,i,i) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,i,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,j,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,b,i,m) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,m,i) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 31
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,j) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,j,m) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,i) * tvoov(b, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvoov(b, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 36
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,i,m) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(b,b,j,m) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,b,i,m) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 39
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,m,i) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 40
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,j) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 41
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,j,m) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,m,i) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * t2(a,b,i,m) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,b,i,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 46
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(b,b,j,m) * toooo(m, i, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,b,i,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 48
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,m,i) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 49
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,m,j) * toooo(m, i, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 50
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,i,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 51
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,j,m) * toooo(m, i, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 52
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,m,i) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * t2(a,b,i,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 


    end subroutine sub_eom_cc3_31_mem_noR_v06_z10                                                                                                     
        subroutine sub_eom_cc3_31_mem_noR_vp_z0(t2, nocc, nactive, a, i, b, j, c, k, sigrows, d_small, sigma, vdav, ntrial, nocc0, nvirt0, n0d,n1d,n0l,n1l, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0d,n1d,n0l,n1l
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                              
                integer, intent(in) :: ntrial, a, i, b, j, c, k                                                                                                        
                integer :: p1, p2, ai, bj, iket  ,e,m,l,d                                                                                                         
                
    if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(c,e,i,k) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,k,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,k,i) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,i,k) * tvoov(a, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(c,e,j,i) * tvoov(a, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,i,j) * tvoov(a, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,j,k) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(c,e,k,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,i,j) * tvoov(a, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,k,j) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,j,i) * tvoov(a, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(b,e,j,k) * tvoov(a, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(c,e,j,k) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,k,j) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,k,j) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,j,k) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,j,k) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,k,j) * tvoov(c, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,k,j) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,j,k) * tvoov(b, i, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(c,e,i,j) * tvoov(b, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,j,i) * tvoov(b, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,i,k) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(c,e,k,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,i,k) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,k,i) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,i,j) * tvoov(c, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(b,e,j,i) * tvoov(c, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 28
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,j,i) * tvoov(b, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,k,i) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,k,i) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,j,i) * tvoov(c, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,i,j) * tvoov(b, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,e,i,k) * tvoov(b, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 34
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,i,k) * tvoov(c, j, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 35
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,e,i,j) * tvoov(c, k, l, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(c,e,i,k) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 37
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(c,e,k,i) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,k,i) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 39
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,e,i,k) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(c,e,j,i) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(c,e,i,j) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(c,e,j,k) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 43
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(c,e,k,j) * read_ftvvvv(b, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 44
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,i,j) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,e,k,j) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 46
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,e,j,i) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(b,e,j,k) * read_ftvvvv(c, e, a, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 48
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(c,e,j,k) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 49
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(c,e,k,j) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 50
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,k,j) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 51
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,e,j,k) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 52
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,j,k) * read_ftvvvv(c, d, b, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,e,k,j) * read_ftvvvv(c, d, b, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 54
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,k,j) * read_ftvvvv(c, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 55
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,e,j,k) * read_ftvvvv(c, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 56
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(c,e,i,j) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 57
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(c,e,j,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 58
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(c,e,i,k) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 59
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(c,e,k,i) * read_ftvvvv(b, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 60
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,e,i,k) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 61
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,e,k,i) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 62
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,e,i,j) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 63
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(b,e,j,i) * read_ftvvvv(c, d, a, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 64
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,j,i) * read_ftvvvv(c, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 65
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,e,k,i) * read_ftvvvv(c, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 66
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,e,k,i) * read_ftvvvv(c, d, b, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 67
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,e,j,i) * read_ftvvvv(c, d, b, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 68
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 69
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(a,e,i,k) * read_ftvvvv(c, e, b, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 70
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,e,i,k) * read_ftvvvv(c, d, b, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do e = nocc + 1, nactive 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 71
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 72
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(c,e,i,k) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 73
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,k,i) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 74
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,k,i) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 75
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,i,k) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 76
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(c,e,j,i) * tvvoo(b, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 77
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,i,j) * tvvoo(b, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 78
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,j,k) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 79
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(c,e,k,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 80
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,i,j) * tvvoo(c, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 81
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,k,j) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 82
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,j,i) * tvvoo(c, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 83
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(b,e,j,k) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 84
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(c,e,j,k) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 85
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,k,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 86
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,k,j) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 87
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,j,k) * tvvoo(a, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 88
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,j,k) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 89
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,k,j) * tvvoo(b, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 90
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,k,j) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 91
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,j,k) * tvvoo(c, e, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 92
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(c,e,i,j) * tvvoo(a, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 93
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,j,i) * tvvoo(a, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 94
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(c,e,i,k) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 95
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(c,e,k,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 96
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,e,i,k) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 97
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,k,i) * tvvoo(a, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 98
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,e,i,j) * tvvoo(a, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 99
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(b,e,j,i) * tvvoo(a, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 100
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,j,i) * tvvoo(c, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 101
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,k,i) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 102
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,e,k,i) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 103
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,j,i) * tvvoo(b, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 104
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,i,j) * tvvoo(c, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 105
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,e,i,k) * tvvoo(c, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 106
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,e,i,k) * tvvoo(b, e, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do e = nocc + 1, nactive 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 107
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,e,i,j) * tvvoo(b, e, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 108
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,c,m,i) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 109
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,m,k) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 110
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,c,k,m) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 111
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,i,m) * tvoov(a, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 112
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,c,m,j) * tvoov(a, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 113
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,m,i) * tvoov(a, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 114
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,m,j) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 115
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(b,c,m,k) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 116
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,c,i,m) * tvoov(a, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 117
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,k,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 118
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,j,m) * tvoov(a, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 119
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(b,c,j,m) * tvoov(a, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 120
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,m,j) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 121
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,m,k) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 122
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,m,k) * tvoov(c, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 123
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,m,j) * tvoov(c, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 124
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,j,m) * tvoov(c, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 125
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,k,m) * tvoov(c, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 126
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,k,m) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 127
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,j,m) * tvoov(b, i, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 128
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,m,i) * tvoov(b, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 129
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,m,j) * tvoov(b, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 130
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,m,i) * tvoov(b, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 131
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,c,m,k) * tvoov(b, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 132
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,m,i) * tvoov(c, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 133
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,m,k) * tvoov(c, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 134
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,m,i) * tvoov(c, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 135
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,b,m,j) * tvoov(c, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 136
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,j,m) * tvoov(b, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 137
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,k,m) * tvoov(b, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 138
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,k,m) * tvoov(c, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 139
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,j,m) * tvoov(c, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 140
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,i,m) * tvoov(b, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 141
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,c,i,m) * tvoov(b, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 142
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,i,m) * tvoov(c, j, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 143
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,b,i,m) * tvoov(c, k, m, d)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 144
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,c,m,i) * tvvoo(a, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 145
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,m,k) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 146
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,c,k,m) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 147
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,i,m) * tvvoo(a, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 148
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,c,m,j) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 149
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,m,i) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 150
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,m,j) * tvvoo(a, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 151
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(b,c,m,k) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 152
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(b,c,i,m) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 153
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,k,m) * tvvoo(a, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 154
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(b,c,j,m) * tvvoo(a, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 155
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(b,c,j,m) * tvvoo(a, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 156
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,m,j) * tvvoo(b, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 157
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,m,k) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 158
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,m,k) * tvvoo(c, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 159
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,m,j) * tvvoo(c, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 160
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,j,m) * tvvoo(c, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 161
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,k,m) * tvvoo(c, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 162
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,k,m) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (i - nocc0) + 1 
do p1 = 1, ntrial 

! 163
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,j,m) * tvvoo(b, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 164
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,m,i) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 165
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,m,j) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 166
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,m,i) * tvvoo(b, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 167
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,c,m,k) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 168
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,m,i) * tvvoo(c, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 169
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,m,k) * tvvoo(c, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 170
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,m,i) * tvvoo(c, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 171
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,b,m,j) * tvvoo(c, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 172
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,c,j,m) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 173
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,k,m) * tvvoo(b, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 174
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * t2(a,b,k,m) * tvvoo(c, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 175
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,j,m) * tvvoo(c, d, m, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 176
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,c,i,m) * tvvoo(b, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 177
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,c,i,m) * tvvoo(b, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (j - nocc0) + 1 
do p1 = 1, ntrial 

! 178
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * t2(a,b,i,m) * tvvoo(c, d, m, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
do m = 1, nocc 
iket = (d - nvirt0) * nocc + (k - nocc0) + 1 
do p1 = 1, ntrial 

! 179
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * t2(a,b,i,m) * tvvoo(c, d, m, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 180
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,c,m,i) * toooo(m, k, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 181
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,c,m,k) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 182
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,c,k,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 183
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,c,i,m) * toooo(m, k, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 184
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,c,m,j) * toooo(m, i, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 185
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,c,m,i) * toooo(m, j, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 186
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,c,m,j) * toooo(m, k, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 187
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(b,c,m,k) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 188
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(b,c,i,m) * toooo(m, j, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 189
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,c,k,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 190
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(b,c,j,m) * toooo(m, i, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (a - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 191
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(b,c,j,m) * toooo(m, k, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 192
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,m,j) * toooo(m, k, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 193
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,c,m,k) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 194
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,m,k) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 195
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,b,m,j) * toooo(m, k, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 196
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,j,m) * toooo(m, k, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 197
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,b,k,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 198
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,k,m) * toooo(m, j, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 199
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,c,j,m) * toooo(m, k, l, i)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 200
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,m,i) * toooo(m, j, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 201
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,c,m,j) * toooo(m, i, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 202
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,c,m,i) * toooo(m, k, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 203
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(a,c,m,k) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 204
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,m,i) * toooo(m, k, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 205
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,b,m,k) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 206
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,b,m,i) * toooo(m, j, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 207
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(a,b,m,j) * toooo(m, i, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 208
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,c,j,m) * toooo(m, i, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 209
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,c,k,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 210
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * t2(a,b,k,m) * toooo(m, i, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 211
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,b,j,m) * toooo(m, i, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 212
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,c,i,m) * toooo(m, j, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (b - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 213
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(a,c,i,m) * toooo(m, k, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 214
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * t2(a,b,i,m) * toooo(m, k, l, j)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
 do l = n0l, n1l 
do m = 1, nocc 
iket = (c - nvirt0) * nocc + (l - nocc0) + 1 
do p1 = 1, ntrial 

! 215
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * t2(a,b,i,m) * toooo(m, j, l, k)
do p2 = 1, ntrial
            !if (abs(vdav(iket, p2)).gt.1.d-12 .or. abs(tmp).gt.1.d-12) print*, 'plusztmp', vdav(iket, p2), tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            !if (iket == 11 .and. abs(tmp) .gt.1.d-12) print*, p1, iket, sigma(p1), tmp, (one/(eorb-wr))

sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end do 
end do 

end if 


    end subroutine sub_eom_cc3_31_mem_noR_vp_z0
    end module eom_cc3_31_mem_noR
    
