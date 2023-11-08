module eom_cc3_32_mem_noR
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    use arithmetic
    use cc3_intermediates
    use cc_gparams       
    implicit none

    contains
                                                                                                         
        subroutine sub_eom_cc3_32_mem_noR_v0_z1(a, i, b, j, c, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m, eorb, wr)
        real(F64), dimension(:,:), intent(inout) :: d_small, sigrows
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
                integer, intent(in) :: ntrial, npair, nocc, a, i, b, j, c
                integer :: p1, p2, dl, em, iket ,d,l,e,m
                integer :: n0de, n1de, n0lm, n1lm
                n0de = max(n0d, n0e)
                n1de = min(n1d, n1e)
                n0lm = max(n0l, n0m)
                n1lm = min(n1l, n1m)
                
    if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 0
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 2
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 4
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 9
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 11
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 13
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 15
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 17
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 18
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 20
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 22
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 24
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 31
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 33
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 34
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 41
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 42
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 43
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 46
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_noR_v0_z1                                                                                                     
        subroutine sub_eom_cc3_32_mem_noR_v0_z2(a, i, b, j, c, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
                integer, intent(in) :: ntrial, npair, nocc, a, i, b, j, c
                integer :: p1, p2, dl, em, iket ,d,l,e,m
                integer :: n0de, n1de, n0lm, n1lm
                n0de = max(n0d, n0e)
                n1de = min(n1d, n1e)
                n0lm = max(n0l, n0m)
                n1lm = min(n1l, n1m)
                
    if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 0
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 1
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 4
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 5
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 9
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, j, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 14
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 15
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, j, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 20
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 22
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, j, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 24
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 29
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 33
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 34
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 35
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 37
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 38
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, j, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, j, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, j, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 46
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 47
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_noR_v0_z2                                                                                                     
        subroutine sub_eom_cc3_32_mem_noR_v6_z34(a, i, j, c, k, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
                integer, intent(in) :: ntrial, npair, nocc, a, i, j, c, k
                integer :: p1, p2, dl, em, iket ,d,l,e,m
                integer :: n0de, n1de, n0lm, n1lm
                n0de = max(n0d, n0e)
                n1de = min(n1d, n1e)
                n0lm = max(n0l, n0m)
                n1lm = min(n1l, n1m)
                
    if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 2
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, d, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 5
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, d, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 9
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 12
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 13
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, k, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, i, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 16
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, k, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, j, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 22
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, k, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, k, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(c, e, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 28
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 29
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, e, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 33
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 34
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 35
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 36
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, k, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 39
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, k, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 40
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(c, j, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, k, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 44
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, k, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, i, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 46
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 47
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_noR_v6_z34                                                                                                     
        subroutine sub_eom_cc3_32_mem_noR_v6_z56(a, i, b, j, k, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
                integer, intent(in) :: ntrial, npair, nocc, a, i, b, j, k
                integer :: p1, p2, dl, em, iket ,d,l,e,m
                integer :: n0de, n1de, n0lm, n1lm
                n0de = max(n0d, n0e)
                n1de = min(n1d, n1e)
                n0lm = max(n0l, n0m)
                n1lm = min(n1l, n1m)
                
    if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 0
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 1
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 4
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 5
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, d, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 9
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, d, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, j, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 14
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 15
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, i, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, k, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, k, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 20
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, k, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 22
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, k, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 24
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, e, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 29
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, e, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(b, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 33
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvvo(a, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 34
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 35
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, k, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 37
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, i, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 38
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, k, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, j, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, k, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * tvooo(b, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 46
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 47
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, k, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_noR_v6_z56                                                                                                     
        subroutine sub_eom_cc3_32_mem_noR_v06_z7(a, i, j, c, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
                integer, intent(in) :: ntrial, npair, nocc, a, i, j, c
                integer :: p1, p2, dl, em, iket ,d,l,e,m
                integer :: n0de, n1de, n0lm, n1lm
                n0de = max(n0d, n0e)
                n1de = min(n1d, n1e)
                n0lm = max(n0l, n0m)
                n1lm = min(n1l, n1m)
                
    if (c >= n0e .and. c <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(c, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 9
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 12
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 14
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 15
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 16
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(c, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(c, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(c, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 33
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 35
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_noR_v06_z7                                                                                                     
        subroutine sub_eom_cc3_32_mem_noR_v06_z8(a, i, j, c, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
                integer, intent(in) :: ntrial, npair, nocc, a, i, j, c
                integer :: p1, p2, dl, em, iket ,d,l,e,m
                integer :: n0de, n1de, n0lm, n1lm
                n0de = max(n0d, n0e)
                n1de = min(n1d, n1e)
                n0lm = max(n0l, n0m)
                n1lm = min(n1l, n1m)
                
    if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(c, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, j, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 12
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 13
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 14
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(c, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 16
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, j, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 17
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(c, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(c, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(c, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 31
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(c, j, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, j, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_noR_v06_z8                                                                                                     
        subroutine sub_eom_cc3_32_mem_noR_v06_z9(a, i, b, j, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
                integer, intent(in) :: ntrial, npair, nocc, a, i, b, j
                integer :: p1, p2, dl, em, iket ,d,l,e,m
                integer :: n0de, n1de, n0lm, n1lm
                n0de = max(n0d, n0e)
                n1de = min(n1d, n1e)
                n0lm = max(n0l, n0m)
                n1lm = min(n1l, n1m)
                
    if (b >= n0e .and. b <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(b, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(b, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 11
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 12
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(b, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(b, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 15
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 16
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 17
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(b, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(b, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(b, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 29
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(b, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 33
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_noR_v06_z9                                                                                                     
        subroutine sub_eom_cc3_32_mem_noR_v06_z10(a, i, b, j, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
                integer, intent(in) :: ntrial, npair, nocc, a, i, b, j
                integer :: p1, p2, dl, em, iket ,d,l,e,m
                integer :: n0de, n1de, n0lm, n1lm
                n0de = max(n0d, n0e)
                n1de = min(n1d, n1e)
                n0lm = max(n0l, n0m)
                n1lm = min(n1l, n1m)
                
    if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(b, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(b, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 9
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(b, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 13
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 14
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(b, i, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 16
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 17
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(b, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(b, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvvvo(a, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvvo(a, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(b, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 31
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvooo(b, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(a, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvooo(b, i, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_noR_v06_z10                                                                                                     
        subroutine sub_eom_cc3_32_mem_noR_vp_z0(a, i, b, j, c, k, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
                integer, intent(in) :: ntrial, npair, nocc, a, i, b, j, c, k
                integer :: p1, p2, dl, em, iket ,d,l,e,m
                integer :: n0de, n1de, n0lm, n1lm
                n0de = max(n0d, n0e)
                n1de = min(n1d, n1e)
                n0lm = max(n0l, n0m)
                n1lm = min(n1l, n1m)
                
    if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(b, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(c, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, d, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(b, d, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, d, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(b, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(c, d, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, d, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(c, d, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(a, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(a, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(b, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, d, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(c, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, d, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(a, d, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, d, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(a, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(a, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, d, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(a, d, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 28
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(c, d, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(b, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, d, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, d, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(c, d, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 34
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, d, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 35
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(b, d, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(a, j, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 37
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(a, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 39
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, j, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(a, k, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, k, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, i, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 43
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 44
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(a, k, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 46
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, k, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(a, i, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 48
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(b, i, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 49
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 50
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(c, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 51
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, i, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 52
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(c, i, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 54
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(b, i, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 55
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, i, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 56
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(b, k, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 57
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, k, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 58
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, j, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (c - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 59
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 60
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(c, j, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 61
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 62
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, k, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
                em = (b - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 63
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(c, k, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 64
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(b, k, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 65
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 66
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(c, j, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 67
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, k, l, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 68
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, k, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 69
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(b, j, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 70
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, j, l, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
                em = (a - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 71
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(c, k, l, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 72
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(c, e, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 73
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 74
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(b, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 75
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, e, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 76
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(c, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 77
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 78
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, e, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 79
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(c, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 80
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(b, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 81
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 82
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 83
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(b, e, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 84
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(c, e, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 85
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 86
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(b, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 87
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, e, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 88
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(a, e, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 89
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 90
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(a, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 91
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, e, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 92
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(c, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 93
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 94
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(c, e, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 95
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(c, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 96
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(b, e, a, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 97
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 98
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(b, e, a, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 99
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(b, e, a, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 100
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(a, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 101
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, e, c, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (k - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 102
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvvo(a, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (j - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 103
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, e, b, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 104
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, e, c, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 105
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(a, e, c, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 106
 tmp = (one/(eorb-wr)) * sigma(p1) * (-4.0_F64) * tvvvo(a, e, b, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
 do e = n0e, n1e 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (e - nvirt0) * nocc + (i - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 107
 tmp = (one/(eorb-wr)) * sigma(p1) * 8.0_F64 * tvvvo(a, e, b, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 108
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(b, k, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 109
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, i, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 110
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(c, i, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 111
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, k, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 112
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(b, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 113
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 114
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, k, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 115
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(b, j, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 116
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(c, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 117
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, j, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 118
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 119
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(c, k, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 120
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(a, k, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 121
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, j, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 122
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(a, j, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 123
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, k, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 124
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(b, k, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 125
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, j, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 126
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(c, j, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 127
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, k, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 128
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 129
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 130
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, k, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (c - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 131
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(a, i, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 132
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(a, k, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 133
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, i, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 134
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(a, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (b - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 135
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(a, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 136
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(c, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 137
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, i, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 138
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tvooo(b, i, m, k)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 139
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, i, m, j)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 140
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(c, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 141
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(c, k, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 142
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tvooo(b, k, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
 do m = n0m, n1m 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
                em = (a - nvirt0) * nocc + (m - nocc0) + 1
                if (dl .ge. em) then
                iket = npair + ((2 * npair - em + 2) * (em -1)) / 2 + dl - em +1
                do p1 = 1, ntrial 

! 143
 tmp = (one/(eorb-wr)) * sigma(p1) * (-8.0_F64) * tvooo(b, j, m, i)
do p2 = 1, ntrial
            d_small(p2, p1) = d_small(p2, p1) + vdav(p2, iket) * tmp
            end do
            
sigrows(p1, iket) = sigrows(p1, iket) + tmp

            end do 
end if 
end do 

end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_noR_vp_z0
    end module eom_cc3_32_mem_noR
    
