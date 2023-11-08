 module eom_cc3_32_mem
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    use arithmetic
    use cc3_intermediates
    use cc_gparams                                                                                                                                                    
    implicit none

  contains
                
        subroutine sub_eom_cc3_32_mem_v0_z1(eom_cc3_32_mem_v0_z1, a, i, b, j, c,vdav, ntrial, npair, nocc, nocc0, nvirt0, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m) 
        real(F64), dimension(:), intent(out) :: eom_cc3_32_mem_v0_z1
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc, npair
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
        integer, intent(in) :: ntrial, a, i, b, j, c
        integer :: p, idx ,k,d,l,e,m
        integer :: n0de, n1de, n0lm, n1lm
        integer :: dl, em
        n0de = max(n0d, n0e)
        n1de = min(n1d, n1e)
        n0lm = max(n0l, n0m)
        n1lm = min(n1l, n1m)
        eom_cc3_32_mem_v0_z1 = ZERO
        
        if (i >= n0l .and. i <= n1l) then 
           if (c >= n0e .and. c <= n1e) then 
              if (j >= n0m .and. j <= n1m) then 
                 !aibjcidicj 
                 do d = n0d, n1d 
                    dl = (d - nvirt0) * nocc + (i - nocc0) + 1
                    em = (c - nvirt0) * nocc + (j - nocc0) + 1
                    if (dl .ge. em) then
                       idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
                       do p = 1, ntrial 
                          
                          eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(b, d, a, i)
                          
                          eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(a, d, b, i)
                       end do
                    end if
                 end do
                 
              end if
           end if
        end if
        
        if (i >= n0m .and. i <= n1m) then 
           if (c >= n0e .and. c <= n1e) then 
              if (j >= n0l .and. j <= n1l) then 
                 !aibjcidjci 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(b, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcidjbi 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(c, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcidibj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(c, d, a, i)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(a, d, c, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcidjai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(b, d, c, i)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(c, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0lm .and. i <= n1lm) then 
if (c >= n0e .and. c <= n1e) then 
!aibjcidici 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0lm .and. i <= n1lm) then 
if (b >= n0e .and. b <= n1e) then 
!aibjcidibi 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
!aibjcidiai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(c, d, b, j)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(b, d, c, j)
end do 
end if 
end do 

end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcibkcj 
 do k = n0l, n1l 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(a, i, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
!aibjcibkci 
 do k = n0l, n1l 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(a, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
!aibjcickbi 
 do k = n0l, n1l 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(a, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcickbj 
 do k = n0l, n1l 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(a, i, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjciakcj 
 do k = n0l, n1l 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(b, i, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjciakbj 
 do k = n0l, n1l 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(c, i, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
!aibjcibkai 
 do k = n0l, n1l 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(c, i, k, j)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(c, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
!aibjcickai 
 do k = n0l, n1l 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(b, i, k, j)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(b, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
!aibjciakci 
 do k = n0l, n1l 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(b, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
!aibjciakbi 
 do k = n0l, n1l 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(c, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjciaidj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(c, d, b, i)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(b, d, c, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
!aibjciaidi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(c, d, b, j)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(b, d, c, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcibidj 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(c, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcicidj 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(b, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0lm .and. i <= n1lm) then 
if (c >= n0d .and. c <= n1d) then 
!aibjcicidi 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0lm .and. i <= n1lm) then 
if (b >= n0d .and. b <= n1d) then 
!aibjcibidi 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcibjdi 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(c, d, a, i)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvvvo(a, d, c, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcicjdi 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
!aibjciaick 
 do k = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(b, i, k, j)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(b, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
!aibjciaibk 
 do k = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(c, j, k, i)

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(c, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
!aibjcibick 
 do k = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(a, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
!aibjcicibk 
 do k = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(a, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
!aibjciciak 
 do k = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(b, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
!aibjcibiak 
 do k = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(c, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcibjck 
 do k = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(a, i, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcicjbk 
 do k = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(a, i, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcibjak 
 do k = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) - vdav(p, idx) * tvooo(c, i, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcicjak 
 do k = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z1(p)= eom_cc3_32_mem_v0_z1(p) + vdav(p, idx) * tvooo(b, i, k, i)
end do 
end if 
end do 

end if 
end if 
end if 


        end subroutine sub_eom_cc3_32_mem_v0_z1   

           subroutine sub_eom_cc3_32_mem_v0_z2(eom_cc3_32_mem_v0_z2, a, i, b, j, c,vdav, ntrial, npair, nocc, nocc0, nvirt0, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m) 
        real(F64), dimension(:), intent(out) :: eom_cc3_32_mem_v0_z2
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc, npair
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
            integer, intent(in) :: ntrial, a, i, b, j, c
            integer :: p, idx ,k
            integer :: n0de, n1de, n0lm, n1lm, d, l, e, m
            integer :: dl, em
            n0de = max(n0d, n0e)
            n1de = min(n1d, n1e)
            n0lm = max(n0l, n0m)
            n1lm = min(n1l, n1m)
            eom_cc3_32_mem_v0_z2 = ZERO
                
    if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjdjci 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(b, d, a, j)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjdibj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(c, d, a, j)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
!aibjcjdjcj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(b, d, a, i)
end do 
end if 
end do 

end if 
end if 

if (b >= n0e .and. b <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
!aibjcjdjbj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(c, d, a, i)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(a, d, c, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
!aibjcjdjaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(b, d, c, i)
end do 
end if 
end do 

end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjdicj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjdiaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(c, d, b, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjdjai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(c, d, b, j)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(b, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
!aibjcjbkci 
 do k = n0l, n1l 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(a, j, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjckbj 
 do k = n0l, n1l 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(a, j, k, i)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(a, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjbkcj 
 do k = n0l, n1l 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(a, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjakbj 
 do k = n0l, n1l 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(c, i, k, j)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(c, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjbkaj 
 do k = n0l, n1l 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(c, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
!aibjcjakci 
 do k = n0l, n1l 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(b, j, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjakcj 
 do k = n0l, n1l 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(b, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjckaj 
 do k = n0l, n1l 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(b, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
!aibjcjckai 
 do k = n0l, n1l 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(b, j, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
!aibjcjbkai 
 do k = n0l, n1l 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(c, j, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjajdi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(c, d, b, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
!aibjcjajdj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(b, d, c, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjaidj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(c, d, b, j)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(b, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjcjcidj 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(b, d, a, j)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjbjdi 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(c, d, a, j)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0d .and. b <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
!aibjcjbjdj 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(c, d, a, i)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvvvo(a, d, c, i)
end do 
end if 
end do 

end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
!aibjcjcjdj 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(b, d, a, i)
end do 
end if 
end do 

end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjcjdi 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjajck 
 do k = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(b, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjajbk 
 do k = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(c, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
!aibjcjaick 
 do k = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(b, j, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
!aibjcjaibk 
 do k = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(c, j, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
!aibjcjcibk 
 do k = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(a, j, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
!aibjcjciak 
 do k = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(b, j, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjbjck 
 do k = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(a, j, k, i)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(a, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjcjbk 
 do k = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(a, i, k, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjbjak 
 do k = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) + vdav(p, idx) * tvooo(c, i, k, j)

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(c, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjcjcjak 
 do k = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v0_z2(p)= eom_cc3_32_mem_v0_z2(p) - vdav(p, idx) * tvooo(b, j, k, i)
end do 
end if 
end do 

end if 
end if 
end if 



end subroutine sub_eom_cc3_32_mem_v0_z2

        subroutine sub_eom_cc3_32_mem_v6_z34(eom_cc3_32_mem_v6_z34, a, i, j, c, k,vdav, ntrial, npair, nocc, nocc0, nvirt0, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m)
        real(F64), dimension(:), intent(out) :: eom_cc3_32_mem_v6_z34
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc, npair
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
            integer, intent(in) :: ntrial, a, i, j, c, k
            integer :: p, idx
            integer :: n0de, n1de, n0lm, n1lm, d, l, e, m
            integer :: dl, em
            n0de = max(n0d, n0e)
            n1de = min(n1d, n1e)
            n0lm = max(n0l, n0m)
            n1lm = min(n1l, n1m)
            eom_cc3_32_mem_v6_z34 = ZERO
                
    if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
!aiajckdick 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(a, d, a, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
!aiajckdkai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(c, d, a, j)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aiajckdicj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(a, d, a, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (k >= n0l .and. k <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aiajckdkcj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(a, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (k >= n0m .and. k <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aiajckdjck 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(a, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aiajckdjai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(c, d, a, k)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(a, d, c, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aiajckdjak 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(c, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aiajckdkaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(c, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
!aiajckdiak 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aiajckdiaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(a, d, c, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
!aiajckalck 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (c - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
!aiajckclai 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(a, j, l, k)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(a, k, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aiajckalcj 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(a, k, l, i)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(a, i, l, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
!aiajckclak 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aiajckclaj 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(a, i, l, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
!aiajckalai 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(c, j, l, k)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(c, k, l, j)
end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (k >= n0m .and. k <= n1m) then 
!aiajckalak 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(c, j, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
!aiajckalaj 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(c, k, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aiajckajdk 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(c, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aiajckajdi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(a, d, c, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aiajckakdj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(c, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aiajckaidj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(c, d, a, k)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(a, d, c, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
!aiajckaidk 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(c, d, a, j)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
!aiajckakdi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aiajckcjdi 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(a, d, a, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (k >= n0m .and. k <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aiajckcjdk 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvvvo(a, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
!aiajckckdi 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(a, d, a, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (k >= n0l .and. k <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aiajckckdj 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvvvo(a, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aiajckajcl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(a, i, l, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
!aiajckajal 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(c, k, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
!aiajckakcl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (c - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
!aiajckaicl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(a, k, l, j)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(a, j, l, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (k >= n0l .and. k <= n1l) then 
!aiajckakal 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(c, j, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
!aiajckaial 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(c, j, l, k)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(c, k, l, j)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aiajckcjal 
 do l = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(a, k, l, i)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) + vdav(p, idx) * tvooo(a, i, l, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
!aiajckckal 
 do l = n0m, n1m 
 dl = (c - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v6_z34(p)= eom_cc3_32_mem_v6_z34(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 



end subroutine sub_eom_cc3_32_mem_v6_z34

        subroutine sub_eom_cc3_32_mem_v6_z56(eom_cc3_32_mem_v6_z56, a, i, b, j, k,vdav, ntrial, npair, nocc, nocc0, nvirt0, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m) 
        real(F64), dimension(:), intent(out) :: eom_cc3_32_mem_v6_z56
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc, npair
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
            integer, intent(in) :: ntrial, a, i, b, j, k
            integer :: p, idx
            integer :: n0de, n1de, n0lm, n1lm, d, l, e, m
            integer :: dl, em
            n0de = max(n0d, n0e)
            n1de = min(n1d, n1e)
            n0lm = max(n0l, n0m)
            n1lm = min(n1l, n1m)
            eom_cc3_32_mem_v6_z56 = ZERO
                
    if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
!aibjbkdkbi 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(b, d, a, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
!aibjbkdibk 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(b, d, a, j)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (k >= n0l .and. k <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbkdkbj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(b, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (k >= n0m .and. k <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbkdjbk 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbkdkaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(b, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbkdjbi 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(a, d, b, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbkdibj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(a, d, b, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbkdiaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(b, d, b, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbkdjai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(b, d, b, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
!aibjbkdkai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(b, d, b, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0de .and. b <= n1de) then 
!aibjbkblbi 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(a, j, l, k)
end do 
end if 
end do 

end if 
end if 

if (k >= n0m .and. k <= n1m) then 
if (b >= n0de .and. b <= n1de) then 
!aibjbkblbk 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbkblbj 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(a, i, l, k)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
!aibjbkalbk 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(b, i, l, j)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(b, j, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbkblaj 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(b, i, l, k)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(b, k, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
!aibjbkalbi 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(b, k, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbkalbj 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(b, k, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
!aibjbkblai 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(b, k, l, j)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(b, j, l, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbkajdi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(b, d, b, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbkajdk 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(b, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbkaidj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(b, d, b, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
!aibjbkaidk 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(b, d, b, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
!aibjbkbidk 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(b, d, a, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbkbidj 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(a, d, b, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
!aibjbkbkdi 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(b, d, a, j)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (k >= n0l .and. k <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbkbkdj 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (k >= n0m .and. k <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbkbjdk 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (k - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(b, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbkbjdi 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvvvo(a, d, b, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbkajbl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(b, k, l, i)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(b, i, l, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
!aibjbkaibl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(b, k, l, j)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(b, j, l, k)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0de .and. b <= n1de) then 
!aibjbkbibl 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(a, j, l, k)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
!aibjbkbial 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(b, k, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (k >= n0l .and. k <= n1l) then 
if (b >= n0de .and. b <= n1de) then 
!aibjbkbkbl 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbkbjbl 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(a, i, l, k)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
!aibjbkbkal 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (k - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) + vdav(p, idx) * tvooo(b, i, l, j)

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(b, j, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbkbjal 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v6_z56(p)= eom_cc3_32_mem_v6_z56(p) - vdav(p, idx) * tvooo(b, k, l, i)
end do 
end if 
end do 

end if 
end if 
end if 



end subroutine sub_eom_cc3_32_mem_v6_z56

        subroutine sub_eom_cc3_32_mem_v06_z7(eom_cc3_32_mem_v06_z7, a, i, j, c,vdav, ntrial, npair, nocc, nocc0, nvirt0, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m)
        real(F64), dimension(:), intent(out) :: eom_cc3_32_mem_v06_z7
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc, npair
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
            integer, intent(in) :: ntrial, a, i, j, c
            integer :: p, idx
            integer :: n0de, n1de, n0lm, n1lm, d, l, e, m
            integer :: dl, em
            n0de = max(n0d, n0e)
            n1de = min(n1d, n1e)
            n0lm = max(n0l, n0m)
            n1lm = min(n1l, n1m)
            eom_cc3_32_mem_v06_z7 = ZERO
                
    if (i >= n0lm .and. i <= n1lm) then 
if (c >= n0e .and. c <= n1e) then 
!aiajcidici 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(a, d, a, j)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
!aiajcidiai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(c, d, a, j)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcidicj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcidjci 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(a, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcidjai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, i)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(a, d, c, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcidiaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(c, d, a, i)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(a, d, c, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
!aiajcialci 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
!aiajciclai 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcialcj 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aiajciclaj 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(a, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
!aiajcialai 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(c, i, l, j)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, j, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcialaj 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(c, i, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aiajciajdi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(c, d, a, i)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(a, d, c, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aiajciaidj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, i)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(a, d, c, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
!aiajciaidi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(c, d, a, j)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0lm .and. i <= n1lm) then 
if (c >= n0d .and. c <= n1d) then 
!aiajcicidi 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(a, d, a, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcicidj 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + vdav(p, idx) * tvvvo(a, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcicjdi 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, a, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aiajciajcl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(a, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
!aiajciajal 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(c, i, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
!aiajciaicl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, j)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(a, j, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
!aiajciaial 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, j, l, i)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(c, i, l, j)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
!aiajcicial 
 do l = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcicjal 
 do l = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z7(p)= eom_cc3_32_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 



end subroutine sub_eom_cc3_32_mem_v06_z7


        subroutine sub_eom_cc3_32_mem_v06_z8(eom_cc3_32_mem_v06_z8, a, i, j, c,vdav, ntrial, npair, nocc, nocc0, nvirt0, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m)
        real(F64), dimension(:), intent(out) :: eom_cc3_32_mem_v06_z8
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc, npair
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
            integer, intent(in) :: ntrial, a, i, j, c
            integer :: p, idx
            integer :: n0de, n1de, n0lm, n1lm, d, l, e, m
            integer :: dl, em
            n0de = max(n0d, n0e)
            n1de = min(n1d, n1e)
            n0lm = max(n0l, n0m)
            n1lm = min(n1l, n1m)
            eom_cc3_32_mem_v06_z8 = ZERO
                
    if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcjdjci 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, a, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcjdicj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(a, d, a, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcjdiaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, j)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcjdjai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(c, d, a, j)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
!aiajcjdjcj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(a, d, a, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (j >= n0lm .and. j <= n1lm) then 
!aiajcjdjaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(c, d, a, i)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
!aiajcjalci 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (c - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcjalcj 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (c - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcjclaj 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
!aiajcjclai 
 do l = n0l, n1l 
 dl = (c - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(a, j, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcjalaj 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, i, l, j)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(c, j, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0m .and. i <= n1m) then 
!aiajcjalai 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(c, j, l, j)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcjajdi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, j)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
!aiajcjajdj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(c, d, a, i)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcjaidj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(c, d, a, j)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(a, d, c, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aiajcjcidj 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, a, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcjcjdi 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(a, d, a, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (j >= n0lm .and. j <= n1lm) then 
!aiajcjcjdj 
 do d = n0e, n1e 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + vdav(p, idx) * tvvvo(a, d, a, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcjajcl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (c - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcjajal 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, i, l, j)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(c, j, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
!aiajcjaicl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (c - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(a, j, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0de .and. a <= n1de) then 
if (i >= n0l .and. i <= n1l) then 
!aiajcjaial 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(c, j, l, j)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
!aiajcjcial 
 do l = n0m, n1m 
 dl = (c - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aiajcjcjal 
 do l = n0m, n1m 
 dl = (c - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v06_z8(p)= eom_cc3_32_mem_v06_z8(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 



end subroutine sub_eom_cc3_32_mem_v06_z8



    subroutine sub_eom_cc3_32_mem_v06_z9(eom_cc3_32_mem_v06_z9, a, i, b, j,vdav, ntrial, npair, nocc,  nocc0, nvirt0, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m)
        real(F64), dimension(:), intent(out) :: eom_cc3_32_mem_v06_z9
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc, npair
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
            integer, intent(in) :: ntrial, a, i, b, j
            integer :: p, idx
            integer :: n0de, n1de, n0lm, n1lm, d, l, e, m
            integer :: dl, em
            n0de = max(n0d, n0e)
            n1de = min(n1d, n1e)
            n0lm = max(n0l, n0m)
            n1lm = min(n1l, n1m)
            eom_cc3_32_mem_v06_z9 = ZERO
                
    if (i >= n0lm .and. i <= n1lm) then 
if (b >= n0e .and. b <= n1e) then 
!aibjbidibi 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, j)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbidibj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbidjbi 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbidiaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbidjai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(b, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
!aibjbidiai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(b, d, b, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0de .and. b <= n1de) then 
!aibjbiblbi 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbiblbj 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(a, i, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbialbj 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(b, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
!aibjbialbi 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, j)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(b, j, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbiblaj 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
!aibjbiblai 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(b, i, l, j)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(b, j, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbiajdi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbiaidj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(b, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
!aibjbiaidi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(b, d, b, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjbibidj 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0lm .and. i <= n1lm) then 
if (b >= n0d .and. b <= n1d) then 
!aibjbibidi 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, j)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbibjdi 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbiajbl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
!aibjbiaibl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(b, i, l, j)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(b, j, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0de .and. b <= n1de) then 
!aibjbibibl 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(a, i, l, j)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
!aibjbibial 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, j)

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(b, j, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbibjbl 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(a, i, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjbibjal 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z9(p)= eom_cc3_32_mem_v06_z9(p) - vdav(p, idx) * tvooo(b, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_v06_z9        
        subroutine sub_eom_cc3_32_mem_v06_z10(eom_cc3_32_mem_v06_z10, a, i, b, j,vdav, ntrial, npair, nocc, nocc0, nvirt0, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m)
        real(F64), dimension(:), intent(out) :: eom_cc3_32_mem_v06_z10
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0, nocc, npair
        integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m
      
            integer, intent(in) :: ntrial, a, i, b, j
            integer :: p, idx
            integer :: n0de, n1de, n0lm, n1lm, d, l, e, m
            integer :: dl, em
            n0de = max(n0d, n0e)
            n1de = min(n1d, n1e)
            n0lm = max(n0l, n0m)
            n1lm = min(n1l, n1m)
            eom_cc3_32_mem_v06_z10 = ZERO
                
    if (i >= n0m .and. i <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibibjdjbi 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibibjdibj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0lm .and. i <= n1lm) then 
if (b >= n0e .and. b <= n1e) then 
!aibibjdibi 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, j)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibibjdjai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(b, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibibjdiaj 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0lm .and. i <= n1lm) then 
!aibibjdiai 
 do d = n0d, n1d 
 dl = (d - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(b, d, b, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0de .and. b <= n1de) then 
!aibibjblbi 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(a, i, l, j)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, i)
end do 
end if 
end do 

end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0m .and. j <= n1m) then 
!aibibjblbj 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(a, i, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
!aibibjalbi 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, j)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(b, j, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibibjalbj 
 do l = n0l, n1l 
 dl = (a - nvirt0) * nocc + (l - nocc0) + 1
            em = (b - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(b, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
!aibibjblai 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(b, i, l, j)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(b, j, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibibjblaj 
 do l = n0l, n1l 
 dl = (b - nvirt0) * nocc + (l - nocc0) + 1
            em = (a - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0lm .and. i <= n1lm) then 
!aibibjaidi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(b, d, b, j)
end do 
end if 
end do 

end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibibjaidj 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(b, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibibjajdi 
 do d = n0e, n1e 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0lm .and. i <= n1lm) then 
if (b >= n0d .and. b <= n1d) then 
!aibibjbidi 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, j)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(a, d, b, j)
end do 
end if 
end do 

end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibibjbidj 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (d - nvirt0) * nocc + (j - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibibjbjdi 
 do d = n0e, n1e 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (d - nvirt0) * nocc + (i - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(b, d, a, i)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + vdav(p, idx) * tvvvo(a, d, b, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
!aibibjaibl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(b, j, l, i)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(b, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibibjajbl 
 do l = n0m, n1m 
 dl = (a - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0de .and. b <= n1de) then 
!aibibjbibl 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, i)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(a, i, l, j)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
!aibibjbial 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (i - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(b, j, l, i)

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, j)
end do 
end if 
end do 

end if 
end if 
end if 

if (b >= n0de .and. b <= n1de) then 
if (j >= n0l .and. j <= n1l) then 
!aibibjbjbl 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (b - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(a, i, l, i)
end do 
end if 
end do 

end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibibjbjal 
 do l = n0m, n1m 
 dl = (b - nvirt0) * nocc + (j - nocc0) + 1
            em = (a - nvirt0) * nocc + (l - nocc0) + 1
            if (dl .ge. em) then
            idx = npair + ((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1 
do p = 1, ntrial 

eom_cc3_32_mem_v06_z10(p)= eom_cc3_32_mem_v06_z10(p) - vdav(p, idx) * tvooo(b, i, l, i)
end do 
end if 
end do 

end if 
end if 
end if 



    end subroutine sub_eom_cc3_32_mem_v06_z10        

        subroutine sub_eom_cc3_32_mem_vp_z0(eom_cc3_32_mem_vp_z0, a, i, b, j, c, k , vdav, ntrial,  npair, nocc, nocc0, nvirt0, n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m) 
        real(F64), dimension(:), intent(out) :: eom_cc3_32_mem_vp_z0
        real(F64), dimension(:, :), intent(in) :: vdav
    integer, intent(in) :: n0d,n1d,n0l,n1l,n0e,n1e,n0m,n1m  
            integer, intent(in) :: ntrial, a, i, b, j, c, k
            integer :: p, idx 
integer, intent(in) :: npair, nocc, nocc0, nvirt0
            integer :: n0de, n1de, n0lm, n1lm, d, l, e, m
            n0de = max(n0d, n0e)
            n1de = min(n1d, n1e)
            n0lm = max(n0l, n0m)
            n1lm = min(n1l, n1m)
            eom_cc3_32_mem_vp_z0 = ZERO
                
    if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
!aibjckdkci 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, k)
!if(idx==33) !print*, 'idx-32', 1, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(b, d, a, j)
 !!print*, vdav(p, idx) * tvvvo(b, d, a, j)


idx = aibj_mem(c, i, d, k)
!if(idx==33) !print*, 'idx-32', 2, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
!aibjckdick 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, i)
!if(idx==33) !print*, 'idx-32', 3, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, j)


idx = aibj_mem(c, k, d, i)
!if(idx==33) !print*, 'idx-32', 4, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(a, d, b, j)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(a, d, b, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
!aibjckdibk 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(b, k, d, i)
!if(idx==33) !print*, 'idx-32', 5, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(c, d, a, j)
 !!print*, vdav(p, idx) * tvvvo(c, d, a, j)


idx = aibj_mem(b, k, d, i)
!if(idx==33) !print*, 'idx-32', 6, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
!aibjckdkbi 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(b, i, d, k)
!if(idx==33) !print*, 'idx-32', 7, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, j)


idx = aibj_mem(b, i, d, k)
!if(idx==33) !print*, 'idx-32', 8, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(a, d, c, j)
 !!print*, vdav(p, idx) * tvvvo(a, d, c, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckdicj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(c, j, d, i)
!if(idx==33) !print*, 'idx-32', 9, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(b, d, a, k)
 !!print*, vdav(p, idx) * tvvvo(b, d, a, k)


idx = aibj_mem(c, j, d, i)
!if(idx==33) !print*, 'idx-32', 10, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckdjci 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, j)
!if(idx==33) !print*, 'idx-32', 11, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, k)


idx = aibj_mem(c, i, d, j)
!if(idx==33) !print*, 'idx-32', 12, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(a, d, b, k)
 !!print*, vdav(p, idx) * tvvvo(a, d, b, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0l .and. k <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckdkcj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(c, j, d, k)
!if(idx==33) !print*, 'idx-32', 13, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, i)


idx = aibj_mem(c, j, d, k)
!if(idx==33) !print*, 'idx-32', 14, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(a, d, b, i)
 !!print*, vdav(p, idx) * tvvvo(a, d, b, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0m .and. k <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckdjck 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, j)
!if(idx==33) !print*, 'idx-32', 15, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(b, d, a, i)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(b, d, a, i)


idx = aibj_mem(c, k, d, j)
!if(idx==33) !print*, 'idx-32', 16, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckdjbi 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(b, i, d, j)
!if(idx==33) !print*, 'idx-32', 17, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(c, d, a, k)
 !!print*, vdav(p, idx) * tvvvo(c, d, a, k)


idx = aibj_mem(b, i, d, j)
!if(idx==33) !print*, 'idx-32', 18, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0m .and. k <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckdjbk 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(b, k, d, j)
!if(idx==33) !print*, 'idx-32', 19, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, i)


idx = aibj_mem(b, k, d, j)
!if(idx==33) !print*, 'idx-32', 20, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(a, d, c, i)
 !!print*, vdav(p, idx) * tvvvo(a, d, c, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckdibj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(b, j, d, i)
!if(idx==33) !print*, 'idx-32', 21, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, k)


idx = aibj_mem(b, j, d, i)
!if(idx==33) !print*, 'idx-32', 22, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(a, d, c, k)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(a, d, c, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0l .and. k <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckdkbj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(b, j, d, k)
!if(idx==33) !print*, 'idx-32', 23, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(c, d, a, i)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(c, d, a, i)


idx = aibj_mem(b, j, d, k)
!if(idx==33) !print*, 'idx-32', 24, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckdkaj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(a, j, d, k)
!if(idx==33) !print*, 'idx-32', 25, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(b, d, c, i)
 !!print*, vdav(p, idx) * tvvvo(b, d, c, i)


idx = aibj_mem(a, j, d, k)
!if(idx==33) !print*, 'idx-32', 26, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckdjak 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, j)
!if(idx==33) !print*, 'idx-32', 27, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, i)


idx = aibj_mem(a, k, d, j)
!if(idx==33) !print*, 'idx-32', 28, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(c, d, b, i)
 !!print*, vdav(p, idx) * tvvvo(c, d, b, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckdiaj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(a, j, d, i)
!if(idx==33) !print*, 'idx-32', 29, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(c, d, b, k)
 !!print*, vdav(p, idx) * tvvvo(c, d, b, k)


idx = aibj_mem(a, j, d, i)
!if(idx==33) !print*, 'idx-32', 30, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
!aibjckdiak 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, i)
!if(idx==33) !print*, 'idx-32', 31, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, j)


idx = aibj_mem(a, k, d, i)
!if(idx==33) !print*, 'idx-32', 32, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(b, d, c, j)
 !!print*, vdav(p, idx) * tvvvo(b, d, c, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckdjai 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(a, i, d, j)
!if(idx==33) !print*, 'idx-32', 33, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, k)


idx = aibj_mem(a, i, d, j)
!if(idx==33) !print*, 'idx-32', 34, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(b, d, c, k)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(b, d, c, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
!aibjckdkai 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0d, n1d 
do p = 1, ntrial 

idx = aibj_mem(a, i, d, k)
!if(idx==33) !print*, 'idx-32', 35, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(c, d, b, j)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(c, d, b, j)


idx = aibj_mem(a, i, d, k)
!if(idx==33) !print*, 'idx-32', 36, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
!aibjckblci 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(b, l, c, i)
!if(idx==33) !print*, 'idx-32', 37, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(a, j, l, k)
 !!print*,-vdav(p, idx) * tvooo(a, j, l, k)


idx = aibj_mem(b, l, c, i)
!if(idx==33) !print*, 'idx-32', 38, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, k, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, k, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
!aibjckblck 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(b, l, c, k)
!if(idx==33) !print*, 'idx-32', 39, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, i)


idx = aibj_mem(b, l, c, k)
!if(idx==33) !print*, 'idx-32', 40, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(a, i, l, j)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(a, i, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
!aibjckclbk 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(b, k, c, l)
!if(idx==33) !print*, 'idx-32', 41, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(a, j, l, i)
 !!print*,-vdav(p, idx) * tvooo(a, j, l, i)


idx = aibj_mem(b, k, c, l)
!if(idx==33) !print*, 'idx-32', 42, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
!aibjckclbi 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(b, i, c, l)
!if(idx==33) !print*, 'idx-32', 43, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, k)


idx = aibj_mem(b, i, c, l)
!if(idx==33) !print*, 'idx-32', 44, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(a, k, l, j)
 !!print*,-vdav(p, idx) * tvooo(a, k, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckblcj 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(b, l, c, j)
!if(idx==33) !print*, 'idx-32', 45, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(a, k, l, i)
 !!print*,-vdav(p, idx) * tvooo(a, k, l, i)


idx = aibj_mem(b, l, c, j)
!if(idx==33) !print*, 'idx-32', 46, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckclbj 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(b, j, c, l)
!if(idx==33) !print*, 'idx-32', 47, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, k, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, k, l, i)


idx = aibj_mem(b, j, c, l)
!if(idx==33) !print*, 'idx-32', 48, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(a, i, l, k)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(a, i, l, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckalcj 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, j)
!if(idx==33) !print*, 'idx-32', 49, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(b, i, l, k)
 !!print*,-vdav(p, idx) * tvooo(b, i, l, k)


idx = aibj_mem(a, l, c, j)
!if(idx==33) !print*, 'idx-32', 50, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, k, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, k, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
!aibjckalck 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, k)
!if(idx==33) !print*, 'idx-32', 51, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, j)


idx = aibj_mem(a, l, c, k)
!if(idx==33) !print*, 'idx-32', 52, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(b, j, l, i)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(b, j, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
!aibjckalbk 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, l, b, k)
!if(idx==33) !print*, 'idx-32', 53, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(c, i, l, j)
 !!print*,-vdav(p, idx) * tvooo(c, i, l, j)


idx = aibj_mem(a, l, b, k)
!if(idx==33) !print*, 'idx-32', 54, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, j, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, j, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckalbj 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, l, b, j)
!if(idx==33) !print*, 'idx-32', 55, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, i, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, i, l, k)


idx = aibj_mem(a, l, b, j)
!if(idx==33) !print*, 'idx-32', 56, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(c, k, l, i)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(c, k, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckblaj 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, j, b, l)
!if(idx==33) !print*, 'idx-32', 57, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(c, i, l, k)
 !!print*,-vdav(p, idx) * tvooo(c, i, l, k)


idx = aibj_mem(a, j, b, l)
!if(idx==33) !print*, 'idx-32', 58, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, k, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, k, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0m .and. k <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
!aibjckblak 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, k, b, l)
!if(idx==33) !print*, 'idx-32', 59, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, i, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, i, l, j)


idx = aibj_mem(a, k, b, l)
!if(idx==33) !print*, 'idx-32', 60, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(c, j, l, i)
 !!print*,-vdav(p, idx) * tvooo(c, j, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
!aibjckclak 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, l)
!if(idx==33) !print*, 'idx-32', 61, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(b, i, l, j)
 !!print*,-vdav(p, idx) * tvooo(b, i, l, j)


idx = aibj_mem(a, k, c, l)
!if(idx==33) !print*, 'idx-32', 62, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, j, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, j, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckclaj 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, j, c, l)
!if(idx==33) !print*, 'idx-32', 63, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, k)


idx = aibj_mem(a, j, c, l)
!if(idx==33) !print*, 'idx-32', 64, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(b, k, l, i)
 !!print*,-vdav(p, idx) * tvooo(b, k, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0e .and. c <= n1e) then 
!aibjckalci 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, i)
!if(idx==33) !print*, 'idx-32', 65, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(b, k, l, j)
 !!print*,-vdav(p, idx) * tvooo(b, k, l, j)


idx = aibj_mem(a, l, c, i)
!if(idx==33) !print*, 'idx-32', 66, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, j, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, j, l, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0e .and. b <= n1e) then 
!aibjckalbi 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, l, b, i)
!if(idx==33) !print*, 'idx-32', 67, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(c, j, l, k)
 !!print*,-vdav(p, idx) * tvooo(c, j, l, k)


idx = aibj_mem(a, l, b, i)
!if(idx==33) !print*, 'idx-32', 68, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, k, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, k, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
!aibjckclai 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, i, c, l)
!if(idx==33) !print*, 'idx-32', 69, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, k, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, k, l, j)


idx = aibj_mem(a, i, c, l)
!if(idx==33) !print*, 'idx-32', 70, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(b, j, l, k)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(b, j, l, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
!aibjckblai 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0l, n1l 
do p = 1, ntrial 

idx = aibj_mem(a, i, b, l)
!if(idx==33) !print*, 'idx-32', 71, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, j, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, j, l, k)


idx = aibj_mem(a, i, b, l)
!if(idx==33) !print*, 'idx-32', 72, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(c, k, l, j)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(c, k, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckajdi 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(a, j, d, i)
!if(idx==33) !print*, 'idx-32', 73, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(c, d, b, k)
 !!print*, vdav(p, idx) * tvvvo(c, d, b, k)


idx = aibj_mem(a, j, d, i)
!if(idx==33) !print*, 'idx-32', 74, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0m .and. k <= n1m) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckajdk 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(a, j, d, k)
!if(idx==33) !print*, 'idx-32', 75, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, i)


idx = aibj_mem(a, j, d, k)
!if(idx==33) !print*, 'idx-32', 76, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(b, d, c, i)
 !!print*, vdav(p, idx) * tvvvo(b, d, c, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckakdj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, j)
!if(idx==33) !print*, 'idx-32', 77, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(c, d, b, i)
 !!print*, vdav(p, idx) * tvvvo(c, d, b, i)


idx = aibj_mem(a, k, d, j)
!if(idx==33) !print*, 'idx-32', 78, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
!aibjckakdi 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, i)
!if(idx==33) !print*, 'idx-32', 79, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, j)


idx = aibj_mem(a, k, d, i)
!if(idx==33) !print*, 'idx-32', 80, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(b, d, c, j)
 !!print*, vdav(p, idx) * tvvvo(b, d, c, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckaidj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(a, i, d, j)
!if(idx==33) !print*, 'idx-32', 81, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, b, k)


idx = aibj_mem(a, i, d, j)
!if(idx==33) !print*, 'idx-32', 82, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(b, d, c, k)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(b, d, c, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
!aibjckaidk 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(a, i, d, k)
!if(idx==33) !print*, 'idx-32', 83, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(c, d, b, j)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(c, d, b, j)


idx = aibj_mem(a, i, d, k)
!if(idx==33) !print*, 'idx-32', 84, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, c, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckbidj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(b, i, d, j)
!if(idx==33) !print*, 'idx-32', 85, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(c, d, a, k)
 !!print*, vdav(p, idx) * tvvvo(c, d, a, k)


idx = aibj_mem(b, i, d, j)
!if(idx==33) !print*, 'idx-32', 86, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
!aibjckbidk 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(b, i, d, k)
!if(idx==33) !print*, 'idx-32', 87, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, j)


idx = aibj_mem(b, i, d, k)
!if(idx==33) !print*, 'idx-32', 88, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(a, d, c, j)
 !!print*, vdav(p, idx) * tvvvo(a, d, c, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (k >= n0m .and. k <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
!aibjckcidk 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, k)
!if(idx==33) !print*, 'idx-32', 89, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(b, d, a, j)
 !!print*, vdav(p, idx) * tvvvo(b, d, a, j)


idx = aibj_mem(c, i, d, k)
!if(idx==33) !print*, 'idx-32', 90, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckcidj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, j)
!if(idx==33) !print*, 'idx-32', 91, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, k)


idx = aibj_mem(c, i, d, j)
!if(idx==33) !print*, 'idx-32', 92, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(a, d, b, k)
 !!print*, vdav(p, idx) * tvvvo(a, d, b, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
!aibjckbkdi 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(b, k, d, i)
!if(idx==33) !print*, 'idx-32', 93, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(c, d, a, j)
 !!print*, vdav(p, idx) * tvvvo(c, d, a, j)


idx = aibj_mem(b, k, d, i)
!if(idx==33) !print*, 'idx-32', 94, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0l .and. k <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckbkdj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(b, k, d, j)
!if(idx==33) !print*, 'idx-32', 95, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, i)


idx = aibj_mem(b, k, d, j)
!if(idx==33) !print*, 'idx-32', 96, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(a, d, c, i)
 !!print*, vdav(p, idx) * tvvvo(a, d, c, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckbjdi 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(b, j, d, i)
!if(idx==33) !print*, 'idx-32', 97, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(c, d, a, k)


idx = aibj_mem(b, j, d, i)
!if(idx==33) !print*, 'idx-32', 98, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(a, d, c, k)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(a, d, c, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0m .and. k <= n1m) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckbjdk 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(b, j, d, k)
!if(idx==33) !print*, 'idx-32', 99, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(c, d, a, i)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(c, d, a, i)


idx = aibj_mem(b, j, d, k)
!if(idx==33) !print*, 'idx-32', 100, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, c, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckcjdi 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(c, j, d, i)
!if(idx==33) !print*, 'idx-32', 101, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(b, d, a, k)
 !!print*, vdav(p, idx) * tvvvo(b, d, a, k)


idx = aibj_mem(c, j, d, i)
!if(idx==33) !print*, 'idx-32', 102, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, k)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0m .and. k <= n1m) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckcjdk 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(c, j, d, k)
!if(idx==33) !print*, 'idx-32', 103, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, i)


idx = aibj_mem(c, j, d, k)
!if(idx==33) !print*, 'idx-32', 104, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + vdav(p, idx) * tvvvo(a, d, b, i)
 !!print*, vdav(p, idx) * tvvvo(a, d, b, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0m .and. i <= n1m) then 
if (k >= n0l .and. k <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
!aibjckckdi 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, i)
!if(idx==33) !print*, 'idx-32', 105, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, j)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(b, d, a, j)


idx = aibj_mem(c, k, d, i)
!if(idx==33) !print*, 'idx-32', 106, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(a, d, b, j)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(a, d, b, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0l .and. k <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0m .and. j <= n1m) then 
!aibjckckdj 
!!$omp parallel private(d, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do d = n0e, n1e 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, j)
!if(idx==33) !print*, 'idx-32', 107, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * tvvvo(b, d, a, i)
 !!print*, 4.0_F64 * vdav(p, idx) * tvvvo(b, d, a, i)


idx = aibj_mem(c, k, d, j)
!if(idx==33) !print*, 'idx-32', 108, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, i)
 !!print*, -2.0_F64 * vdav(p, idx) * tvvvo(a, d, b, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckajcl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, j, c, l)
!if(idx==33) !print*, 'idx-32', 109, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(b, k, l, i)
 !!print*,-vdav(p, idx) * tvooo(b, k, l, i)


idx = aibj_mem(a, j, c, l)
!if(idx==33) !print*, 'idx-32', 110, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckajbl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, j, b, l)
!if(idx==33) !print*, 'idx-32', 111, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(c, i, l, k)
 !!print*,-vdav(p, idx) * tvooo(c, i, l, k)


idx = aibj_mem(a, j, b, l)
!if(idx==33) !print*, 'idx-32', 112, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, k, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, k, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (c >= n0e .and. c <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
!aibjckakcl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, l)
!if(idx==33) !print*, 'idx-32', 113, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(b, i, l, j)
 !!print*,-vdav(p, idx) * tvooo(b, i, l, j)


idx = aibj_mem(a, k, c, l)
!if(idx==33) !print*, 'idx-32', 114, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, j, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, j, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
!aibjckaicl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, i, c, l)
!if(idx==33) !print*, 'idx-32', 115, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, k, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, k, l, j)


idx = aibj_mem(a, i, c, l)
!if(idx==33) !print*, 'idx-32', 116, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(b, j, l, k)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(b, j, l, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
!aibjckakbl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, k, b, l)
!if(idx==33) !print*, 'idx-32', 117, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(c, j, l, i)
 !!print*,-vdav(p, idx) * tvooo(c, j, l, i)


idx = aibj_mem(a, k, b, l)
!if(idx==33) !print*, 'idx-32', 118, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, i, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, i, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0d .and. a <= n1d) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0e .and. b <= n1e) then 
!aibjckaibl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, i, b, l)
!if(idx==33) !print*, 'idx-32', 119, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, j, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, j, l, k)


idx = aibj_mem(a, i, b, l)
!if(idx==33) !print*, 'idx-32', 120, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(c, k, l, j)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(c, k, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
!aibjckbicl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(b, i, c, l)
!if(idx==33) !print*, 'idx-32', 121, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(a, k, l, j)
 !!print*,-vdav(p, idx) * tvooo(a, k, l, j)


idx = aibj_mem(b, i, c, l)
!if(idx==33) !print*, 'idx-32', 122, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
!aibjckcibl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(b, l, c, i)
!if(idx==33) !print*, 'idx-32', 123, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(a, j, l, k)
 !!print*,-vdav(p, idx) * tvooo(a, j, l, k)


idx = aibj_mem(b, l, c, i)
!if(idx==33) !print*, 'idx-32', 124, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, k, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, k, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (c >= n0d .and. c <= n1d) then 
!aibjckcial 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, i)
!if(idx==33) !print*, 'idx-32', 125, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(b, k, l, j)
 !!print*,-vdav(p, idx) * tvooo(b, k, l, j)


idx = aibj_mem(a, l, c, i)
!if(idx==33) !print*, 'idx-32', 126, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, j, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, j, l, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (i >= n0l .and. i <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
!aibjckbial 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, l, b, i)
!if(idx==33) !print*, 'idx-32', 127, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(c, j, l, k)
 !!print*,-vdav(p, idx) * tvooo(c, j, l, k)


idx = aibj_mem(a, l, b, i)
!if(idx==33) !print*, 'idx-32', 128, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, k, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, k, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
!aibjckbkcl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(b, k, c, l)
!if(idx==33) !print*, 'idx-32', 129, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(a, j, l, i)
 !!print*,-vdav(p, idx) * tvooo(a, j, l, i)


idx = aibj_mem(b, k, c, l)
!if(idx==33) !print*, 'idx-32', 130, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0e .and. c <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckbjcl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(b, j, c, l)
!if(idx==33) !print*, 'idx-32', 131, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, k, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, k, l, i)


idx = aibj_mem(b, j, c, l)
!if(idx==33) !print*, 'idx-32', 132, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(a, i, l, k)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(a, i, l, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckcjbl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(b, l, c, j)
!if(idx==33) !print*, 'idx-32', 133, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(a, k, l, i)
 !!print*,-vdav(p, idx) * tvooo(a, k, l, i)


idx = aibj_mem(b, l, c, j)
!if(idx==33) !print*, 'idx-32', 134, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, i, l, k)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0d .and. c <= n1d) then 
if (b >= n0e .and. b <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
!aibjckckbl 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(b, l, c, k)
!if(idx==33) !print*, 'idx-32', 135, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(a, j, l, i)


idx = aibj_mem(b, l, c, k)
!if(idx==33) !print*, 'idx-32', 136, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(a, i, l, j)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(a, i, l, j)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (k >= n0l .and. k <= n1l) then 
if (b >= n0d .and. b <= n1d) then 
!aibjckbkal 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, l, b, k)
!if(idx==33) !print*, 'idx-32', 137, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(c, i, l, j)
 !!print*,-vdav(p, idx) * tvooo(c, i, l, j)


idx = aibj_mem(a, l, b, k)
!if(idx==33) !print*, 'idx-32', 138, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, j, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, j, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (b >= n0d .and. b <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckbjal 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, l, b, j)
!if(idx==33) !print*, 'idx-32', 139, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(c, i, l, k)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(c, i, l, k)


idx = aibj_mem(a, l, b, j)
!if(idx==33) !print*, 'idx-32', 140, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(c, k, l, i)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(c, k, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (j >= n0l .and. j <= n1l) then 
!aibjckcjal 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, j)
!if(idx==33) !print*, 'idx-32', 141, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) - vdav(p, idx) * tvooo(b, i, l, k)
 !!print*,-vdav(p, idx) * tvooo(b, i, l, k)


idx = aibj_mem(a, l, c, j)
!if(idx==33) !print*, 'idx-32', 142, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, k, l, i)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, k, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (a >= n0e .and. a <= n1e) then 
if (c >= n0d .and. c <= n1d) then 
if (k >= n0l .and. k <= n1l) then 
!aibjckckal 
!!$omp parallel private(l, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_32_mem_vp_z0)
 do l = n0m, n1m 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, k)
!if(idx==33) !print*, 'idx-32', 143, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, j)
 !!print*, 2.0_F64 * vdav(p, idx) * tvooo(b, i, l, j)


idx = aibj_mem(a, l, c, k)
!if(idx==33) !print*, 'idx-32', 144, idx 
eom_cc3_32_mem_vp_z0(p)= eom_cc3_32_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * tvooo(b, j, l, i)
 !!print*, -4.0_F64 * vdav(p, idx) * tvooo(b, j, l, i)

end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 


    end subroutine sub_eom_cc3_32_mem_vp_z0
    end module eom_cc3_32_mem
    
