module eom_cc3_31_mem
    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    use arithmetic
    use cc3_intermediates
    use cc_gparams                                                                                                                                                    
    implicit none

    contains
            
        subroutine sub_eom_cc3_31_mem_v0_z1(eom_cc3_31_mem_v0_z1, t2, nocc, nactive, a, i, b, j, c, vdav, ntrial,n0d,n1d,n0l,n1l) 
        real(F64), dimension(:), intent(out) :: eom_cc3_31_mem_v0_z1
        real(F64), dimension(:, :), intent(in) :: vdav

        integer, intent(in) :: n0d,n1d,n0l,n1l
    
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                               
            integer, intent(in) :: ntrial, a, i, b, j, c
            integer :: p, idx ,k,e,l,d
            integer :: hh
            hh = 1
            eom_cc3_31_mem_v0_z1 = ZERO
    if (b >= n0d .and. b <= n1d) then 
!aibjcibk 
 do k = n0l, n1l 
idx = ai_mem(b, k)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(c,d,j,i) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(c,d,i,j) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,d,i,j) * tvoov(c, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,d,i,i) * tvoov(c, j, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(c,d,j,i) * tvvoo(a, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,d,i,j) * tvvoo(c, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(c,d,i,i) * tvvoo(a, d, k, j)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,d,i,i) * tvvoo(c, d, k, j)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aibjcick 
 do k = n0l, n1l 
idx = ai_mem(c, k)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,d,i,j) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,d,j,i) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,d,i,j) * tvoov(b, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,d,i,i) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,d,j,i) * tvvoo(a, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,d,i,j) * tvvoo(b, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,d,i,i) * tvvoo(a, d, k, j)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,d,i,i) * tvvoo(b, d, k, j)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjciak 
 do k = n0l, n1l 
idx = ai_mem(a, k)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(c,d,j,i) * tvoov(b, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,d,j,i) * tvoov(c, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(c,d,i,i) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,d,i,i) * tvoov(c, j, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(c,d,j,i) * tvvoo(b, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(c,d,i,j) * tvvoo(b, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,d,i,j) * tvvoo(c, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,d,j,i) * tvvoo(c, d, k, i)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjciei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(c,e,j,i) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(c,e,i,j) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,e,i,j) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,e,j,i) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjciei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(c,e,j,i) * read_ftvvvv(b, d, a, e)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,e,j,i) * read_ftvvvv(c, d, a, e)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjciej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(c,d,i,i) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,d,i,i) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,d,i,i) * read_ftvvvv(c, e, b, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjciej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,d,i,i) * read_ftvvvv(c, d, b, e)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjcidi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,c,k,j) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,c,k,j) * tvoov(b, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,b,k,j) * tvoov(c, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,c,k,i) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,b,k,i) * tvoov(c, j, k, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjcidj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,c,k,i) * tvoov(a, i, k, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjcidj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,c,i,k) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,b,i,k) * tvoov(c, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,c,i,k) * tvoov(b, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,c,k,i) * tvvoo(b, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,b,k,i) * tvvoo(c, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,c,i,k) * tvvoo(b, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,b,i,k) * tvvoo(c, d, k, i)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjcidi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,c,j,k) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,c,i,k) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,b,i,k) * tvoov(c, j, k, d)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,c,k,j) * tvvoo(a, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,c,k,i) * tvvoo(a, d, k, j)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,c,i,k) * tvvoo(a, d, k, j)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,c,j,k) * tvvoo(a, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,c,k,j) * tvvoo(b, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,b,k,j) * tvvoo(c, d, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,b,i,k) * tvvoo(c, d, k, j)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,c,i,k) * tvvoo(b, d, k, j)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjcial 
 do k = n0l, n1l 
idx = ai_mem(a, k)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,c,l,j) * toooo(l, i, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,c,l,i) * toooo(l, j, k, i)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjcial 
 do k = n0l, n1l 
idx = ai_mem(a, k)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(b,c,i,l) * toooo(l, j, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(b,c,j,l) * toooo(l, i, k, i)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibjcibl 
 do l = n0l, n1l 
idx = ai_mem(b, l)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,c,k,j) * toooo(l, i, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,c,k,i) * toooo(l, j, k, i)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aibjcicl 
 do l = n0l, n1l 
idx = ai_mem(c, l)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,b,k,j) * toooo(l, i, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,b,i,k) * toooo(l, i, k, j)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,b,k,i) * toooo(l, j, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,b,i,k) * toooo(l, j, k, i)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibjcibl 
 do k = n0l, n1l 
idx = ai_mem(b, k)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) - vdav(p, idx) * t2(a,c,i,l) * toooo(l, j, k, i)

eom_cc3_31_mem_v0_z1(p)= eom_cc3_31_mem_v0_z1(p) + vdav(p, idx) * t2(a,c,i,l) * toooo(l, i, k, j)
end do 
end do 
end do 

end if 


    !print*, 'hh  z 31_mem z1', hh
    end subroutine sub_eom_cc3_31_mem_v0_z1        
        subroutine sub_eom_cc3_31_mem_v0_z2(eom_cc3_31_mem_v0_z2, t2, nocc, nactive, a, i, b, j, c, vdav, ntrial,n0d,n1d,n0l,n1l) 
        real(F64), dimension(:), intent(out) :: eom_cc3_31_mem_v0_z2
        real(F64), dimension(:, :), intent(in) :: vdav

        integer, intent(in) :: n0d,n1d,n0l,n1l
    
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                               
            integer, intent(in) :: ntrial, a, i, b, j, c
            integer :: p, idx ,k,e,l,d
            integer :: hh
            hh = 1

            eom_cc3_31_mem_v0_z2 = ZERO
    if (b >= n0d .and. b <= n1d) then 
!aibjcjbk 
 do k = n0l, n1l 
idx = ai_mem(b, k)
do d = nocc + 1, nactive 
do p = 1, ntrial

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(c,d,i,j) * tvoov(a, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(c,d,j,j) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,d,j,j) * tvoov(c, i, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,d,i,j) * tvoov(c, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(c,d,i,j) * tvvoo(a, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(c,d,j,i) * tvvoo(a, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,d,j,i) * tvvoo(c, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,d,i,j) * tvvoo(c, d, k, j)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aibjcjck 
 do k = n0l, n1l 
idx = ai_mem(c, k)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,d,j,i) * tvoov(a, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,d,j,j) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,d,j,i) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,d,i,j) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,d,j,j) * tvvoo(a, d, k, i)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,d,j,j) * tvvoo(b, d, k, i)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,d,j,i) * tvvoo(a, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,d,i,j) * tvvoo(b, d, k, j)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjcjak 
 do k = n0l, n1l 
idx = ai_mem(a, k)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,d,j,j) * tvoov(c, i, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(c,d,i,j) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(c,d,j,i) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,d,j,i) * tvoov(c, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(c,d,i,j) * tvvoo(b, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,d,j,i) * tvvoo(c, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(c,d,j,j) * tvvoo(b, d, k, i)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,d,j,j) * tvvoo(c, d, k, i)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjcjej 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do e = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(c,e,i,j) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,e,i,j) * read_ftvvvv(c, e, b, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjcjej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,d,j,i) * read_ftvvvv(c, d, a, e)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,d,j,i) * read_ftvvvv(c, d, b, e)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjcjei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(c,e,j,j) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,e,j,j) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjcjei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,e,j,j) * read_ftvvvv(c, d, a, e)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,e,j,j) * read_ftvvvv(c, d, b, e)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjcjej 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do e = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(c,e,i,j) * read_ftvvvv(b, d, a, e)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,e,i,j) * read_ftvvvv(c, d, b, e)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjcjej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(c,d,j,i) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,d,j,i) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjcjdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,c,k,i) * tvoov(a, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,c,k,j) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,b,k,j) * tvoov(c, i, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,c,k,i) * tvoov(b, j, k, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjcjdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,c,j,k) * tvoov(a, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,c,j,k) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,c,k,j) * tvvoo(a, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,c,j,k) * tvvoo(a, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,b,k,j) * tvvoo(c, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,b,j,k) * tvvoo(c, d, k, j)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjcjdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,c,j,k) * tvoov(a, i, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,b,j,k) * tvoov(c, i, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,c,i,k) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,b,i,k) * tvoov(c, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,c,k,i) * tvvoo(a, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,c,j,k) * tvvoo(a, d, k, i)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,c,k,i) * tvvoo(b, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,c,k,j) * tvvoo(b, d, k, i)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,b,k,j) * tvvoo(c, d, k, i)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,c,j,k) * tvvoo(b, d, k, i)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,c,i,k) * tvvoo(b, d, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,b,i,k) * tvvoo(c, d, k, j)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjcjdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,c,k,j) * tvoov(b, j, k, d)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,b,k,j) * tvoov(c, j, k, d)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjcjal 
 do k = n0l, n1l 
idx = ai_mem(a, k)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,c,l,i) * toooo(l, j, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,c,l,j) * toooo(l, j, k, i)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjcjal 
 do k = n0l, n1l 
idx = ai_mem(a, k)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(b,c,j,l) * toooo(l, i, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(b,c,j,l) * toooo(l, j, k, i)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aibjcjcl 
 do l = n0l, n1l 
idx = ai_mem(c, l)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,b,k,j) * toooo(l, i, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,b,j,k) * toooo(l, i, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,b,k,j) * toooo(l, j, k, i)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,b,i,k) * toooo(l, j, k, j)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibjcjbl 
 do l = n0l, n1l 
idx = ai_mem(b, l)
do k = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,c,k,i) * toooo(l, j, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,c,k,j) * toooo(l, j, k, i)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibjcjbl 
 do k = n0l, n1l 
idx = ai_mem(b, k)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) - vdav(p, idx) * t2(a,c,j,l) * toooo(l, i, k, j)

eom_cc3_31_mem_v0_z2(p)= eom_cc3_31_mem_v0_z2(p) + vdav(p, idx) * t2(a,c,i,l) * toooo(l, j, k, j)
end do 
end do 
end do 

end if 


        !print*, 'hh  z 31_mem z2', hh
    end subroutine sub_eom_cc3_31_mem_v0_z2        
        subroutine sub_eom_cc3_31_mem_v6_z34(eom_cc3_31_mem_v6_z34, t2, nocc, nactive, a, i, j, c, k, vdav, ntrial,n0d,n1d,n0l,n1l) 
        real(F64), dimension(:), intent(out) :: eom_cc3_31_mem_v6_z34
        real(F64), dimension(:, :), intent(in) :: vdav

        integer, intent(in) :: n0d,n1d,n0l,n1l
    
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                               
            integer, intent(in) :: ntrial, a, i, j, c, k
            integer :: p, idx ,d,l,e,m
            integer :: hh
            hh = 1

            eom_cc3_31_mem_v6_z34 = ZERO
    if (a >= n0d .and. a <= n1d) then 
!aiajckal 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(c,d,k,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(c,d,j,i) * tvoov(a, k, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(c,d,j,k) * tvoov(a, i, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(c,d,k,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,d,i,k) * tvoov(c, j, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,d,k,i) * tvoov(c, j, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,d,i,j) * tvoov(c, k, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,d,j,i) * tvoov(c, k, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(c,d,k,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,d,i,k) * tvvoo(c, d, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(c,d,j,i) * tvvoo(a, d, l, k)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(c,d,j,k) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(c,d,k,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,d,i,j) * tvvoo(c, d, l, k)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,d,k,j) * tvvoo(c, d, l, i)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,d,j,k) * tvvoo(c, d, l, i)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aiajckcl 
 do l = n0l, n1l 
idx = ai_mem(c, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,d,i,k) * tvoov(a, j, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,d,i,j) * tvoov(a, k, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,d,k,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,d,j,k) * tvoov(a, i, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,d,i,k) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,d,k,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,d,i,j) * tvvoo(a, d, l, k)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,d,j,i) * tvvoo(a, d, l, k)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajckej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(c,d,k,i) * read_ftvvvv(a, e, a, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,d,k,i) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajckej 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,e,i,k) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
!aiajckek 
 do e = n0d, n1d 
idx = ai_mem(e, k)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(c,d,j,i) * read_ftvvvv(a, e, a, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,d,i,j) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,d,j,i) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajckei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(c,e,j,k) * read_ftvvvv(a, e, a, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(c,e,k,j) * read_ftvvvv(a, e, a, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,e,k,j) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,e,j,k) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
!aiajckek 
 do e = n0d, n1d 
idx = ai_mem(e, k)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,d,i,j) * read_ftvvvv(c, d, a, e)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajckej 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,e,i,k) * read_ftvvvv(c, d, a, e)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajckdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,l,k) * tvoov(a, j, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,l,j) * tvoov(a, k, l, d)
end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
!aiajckdk 
 do d = n0d, n1d 
idx = ai_mem(d, k)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,i,l) * tvoov(a, j, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,j,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,a,i,l) * tvoov(c, j, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,l,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,i,l) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,a,i,l) * tvvoo(c, d, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,a,j,l) * tvvoo(c, d, l, i)
end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
!aiajckdk 
 do d = n0d, n1d 
idx = ai_mem(d, k)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,l,j) * tvoov(a, i, l, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajckdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,l,k) * tvoov(a, i, l, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajckdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,i,l) * tvoov(a, k, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,k,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,a,i,l) * tvoov(c, k, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,l,k) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,i,l) * tvvoo(a, d, l, k)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,a,i,l) * tvvoo(c, d, l, k)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,a,k,l) * tvvoo(c, d, l, i)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajckdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,a,k,l) * tvoov(c, j, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,a,j,l) * tvoov(c, k, l, d)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,l,j) * tvvoo(a, d, l, k)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,l,k) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,k,l) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,j,l) * tvvoo(a, d, l, k)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aiajckam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,m,k) * toooo(m, i, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,m,j) * toooo(m, i, l, k)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,m,j) * toooo(m, k, l, i)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,m,k) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aiajckam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,i,m) * toooo(m, k, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,i,m) * toooo(m, j, l, k)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,c,k,m) * toooo(m, j, l, i)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,c,j,m) * toooo(m, k, l, i)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aiajckcm 
 do m = n0l, n1l 
idx = ai_mem(c, m)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,a,i,l) * toooo(m, j, l, k)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) - vdav(p, idx) * t2(a,a,k,l) * toooo(m, j, l, i)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,a,i,l) * toooo(m, k, l, j)

eom_cc3_31_mem_v6_z34(p)= eom_cc3_31_mem_v6_z34(p) + vdav(p, idx) * t2(a,a,j,l) * toooo(m, k, l, i)
end do 
end do 
end do 

end if 


        !print*, 'hh  z 31_mem z34', hh
    end subroutine sub_eom_cc3_31_mem_v6_z34        
        subroutine sub_eom_cc3_31_mem_v6_z56(eom_cc3_31_mem_v6_z56, t2, nocc, nactive, a, i, b, j, k, vdav, ntrial,n0d,n1d,n0l,n1l) 
        real(F64), dimension(:), intent(out) :: eom_cc3_31_mem_v6_z56
        real(F64), dimension(:, :), intent(in) :: vdav

        integer, intent(in) :: n0d,n1d,n0l,n1l
    
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                               
            integer, intent(in) :: ntrial, a, i, b, j, k
            integer :: p, idx ,d,l,e,m
            integer :: hh
            hh = 1

            eom_cc3_31_mem_v6_z56 = ZERO
    if (b >= n0d .and. b <= n1d) then 
!aibjbkbl 
 do l = n0l, n1l 
idx = ai_mem(b, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,d,i,k) * tvoov(a, j, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,d,k,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,d,j,k) * tvoov(a, i, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,d,k,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,d,j,k) * tvoov(b, i, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,d,j,i) * tvoov(b, k, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,d,i,j) * tvoov(b, k, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,d,i,k) * tvoov(b, j, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,d,k,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,d,j,k) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,d,i,j) * tvvoo(a, d, l, k)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,d,j,i) * tvvoo(a, d, l, k)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,d,k,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,d,j,i) * tvvoo(b, d, l, k)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,d,i,j) * tvvoo(b, d, l, k)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,d,i,k) * tvvoo(b, d, l, j)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjbkal 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,d,k,j) * tvoov(b, i, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,d,i,j) * tvoov(b, k, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,d,j,i) * tvoov(b, k, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,d,k,i) * tvoov(b, j, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,d,i,k) * tvvoo(b, d, l, j)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,d,k,i) * tvvoo(b, d, l, j)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,d,j,k) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,d,k,j) * tvvoo(b, d, l, i)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjbkej 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,e,i,k) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,e,i,k) * read_ftvvvv(b, e, b, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjbkej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,d,k,i) * read_ftvvvv(b, d, a, e)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjbkei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,e,j,k) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,e,k,j) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,e,j,k) * read_ftvvvv(b, e, b, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjbkei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,e,k,j) * read_ftvvvv(b, d, a, e)
end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
!aibjbkek 
 do e = n0d, n1d 
idx = ai_mem(e, k)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,d,i,j) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,d,j,i) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,d,j,i) * read_ftvvvv(b, e, b, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,d,i,j) * read_ftvvvv(b, e, b, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjbkej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,d,k,i) * read_ftvvvv(b, e, a, d)
end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
!aibjbkdk 
 do d = n0d, n1d 
idx = ai_mem(d, k)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,b,i,l) * tvoov(a, j, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,b,j,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,j,l) * tvoov(b, i, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,i,l) * tvoov(b, j, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,l,i) * tvvoo(b, d, l, j)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,l,j) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,j,l) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,i,l) * tvvoo(b, d, l, j)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjbkdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,b,k,l) * tvoov(a, j, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,j,l) * tvoov(b, k, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,b,j,l) * tvvoo(a, d, l, k)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,b,k,l) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,l,k) * tvvoo(b, d, l, j)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,j,l) * tvvoo(b, d, l, k)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjbkdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,b,k,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,i,l) * tvoov(b, k, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,b,i,l) * tvvoo(a, d, l, k)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,b,k,l) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,l,k) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,i,l) * tvvoo(b, d, l, k)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjbkdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,l,k) * tvoov(b, i, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,l,i) * tvoov(b, k, l, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjbkdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,l,j) * tvoov(b, k, l, d)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,l,k) * tvoov(b, j, l, d)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjbkam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,b,i,m) * toooo(m, k, l, j)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(b,b,k,m) * toooo(m, i, l, j)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,b,j,m) * toooo(m, k, l, i)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(b,b,k,m) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibjbkbm 
 do m = n0l, n1l 
idx = ai_mem(b, m)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,l,k) * toooo(m, i, l, j)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,l,i) * toooo(m, k, l, j)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,l,j) * toooo(m, k, l, i)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,l,k) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibjbkbm 
 do l = n0l, n1l 
idx = ai_mem(b, l)
do m = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,j,m) * toooo(m, k, l, i)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) - vdav(p, idx) * t2(a,b,j,m) * toooo(m, i, l, k)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,i,m) * toooo(m, j, l, k)

eom_cc3_31_mem_v6_z56(p)= eom_cc3_31_mem_v6_z56(p) + vdav(p, idx) * t2(a,b,i,m) * toooo(m, k, l, j)
end do 
end do 
end do 

end if 

    !print*, 'hh  z 31_mem z56', hh
    
    end subroutine sub_eom_cc3_31_mem_v6_z56        
        subroutine sub_eom_cc3_31_mem_v06_z7(eom_cc3_31_mem_v06_z7, t2, nocc, nactive, a, i, j, c, vdav, ntrial,n0d,n1d,n0l,n1l) 
        real(F64), dimension(:), intent(out) :: eom_cc3_31_mem_v06_z7
        real(F64), dimension(:, :), intent(in) :: vdav

        integer, intent(in) :: n0d,n1d,n0l,n1l
    
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                               
            integer, intent(in) :: ntrial, a, i, j, c
            integer :: p, idx ,d,l,e,m
            integer :: hh
            hh = 1

            eom_cc3_31_mem_v06_z7 = ZERO
    if (a >= n0d .and. a <= n1d) then 
!aiajcial 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(c,d,i,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(c,d,j,i) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(c,d,i,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,d,i,j) * tvoov(c, i, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,d,j,i) * tvoov(c, i, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,i,i) * tvoov(c, j, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(c,d,i,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,d,i,i) * tvvoo(c, d, l, j)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(c,d,j,i) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(c,d,i,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,i,j) * tvvoo(c, d, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,d,j,i) * tvvoo(c, d, l, i)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aiajcicl 
 do l = n0l, n1l 
idx = ai_mem(c, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,d,i,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,i,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,d,j,i) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,d,i,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,d,j,i) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,i,i) * tvvoo(a, d, l, j)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajciej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(c,d,i,i) * read_ftvvvv(a, e, a, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * t2(a,d,i,i) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajciej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(a,d,i,i) * read_ftvvvv(c, d, a, e)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajciei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * t2(c,e,j,i) * read_ftvvvv(a, e, a, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(c,e,i,j) * read_ftvvvv(a, e, a, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * t2(a,e,i,j) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(a,e,j,i) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajciei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(a,e,i,j) * read_ftvvvv(c, d, a, e)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(a,e,j,i) * read_ftvvvv(c, d, a, e)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajcidi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,c,l,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,l,j) * tvoov(a, i, l, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajcidi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,c,i,l) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,c,j,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,a,j,l) * tvoov(c, i, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(a,a,i,l) * tvoov(c, j, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,l,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,c,l,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,i,l) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,c,j,l) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,a,i,l) * tvvoo(c, d, l, j)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,a,j,l) * tvvoo(c, d, l, i)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajcidj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,c,l,i) * tvoov(a, i, l, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajcidj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,i,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,a,i,l) * tvoov(c, i, l, d)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,c,l,i) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) - vdav(p, idx) * t2(a,c,i,l) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + 2.0_F64 * vdav(p, idx) * t2(a,a,i,l) * tvvoo(c, d, l, i)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aiajciam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(a,c,m,i) * toooo(m, i, l, j)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * t2(a,c,m,j) * toooo(m, i, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(a,c,m,i) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aiajciam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(a,c,i,m) * toooo(m, i, l, j)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * t2(a,c,i,m) * toooo(m, j, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(a,c,j,m) * toooo(m, i, l, i)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aiajcicm 
 do m = n0l, n1l 
idx = ai_mem(c, m)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(a,a,i,l) * toooo(m, i, l, j)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) + vdav(p, idx) * t2(a,a,j,l) * toooo(m, i, l, i)

eom_cc3_31_mem_v06_z7(p)= eom_cc3_31_mem_v06_z7(p) -2.0_F64 * vdav(p, idx) * t2(a,a,i,l) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 


        !print*, 'hh  z 31_mem z7', hh
    end subroutine sub_eom_cc3_31_mem_v06_z7        
        subroutine sub_eom_cc3_31_mem_v06_z8(eom_cc3_31_mem_v06_z8, t2, nocc, nactive, a, i, j, c, vdav, ntrial,n0d,n1d,n0l,n1l) 
        real(F64), dimension(:), intent(out) :: eom_cc3_31_mem_v06_z8
        real(F64), dimension(:, :), intent(in) :: vdav

        integer, intent(in) :: n0d,n1d,n0l,n1l
    
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                               
            integer, intent(in) :: ntrial, a, i, j, c
            integer :: p, idx ,d,l,e,m
            integer :: hh
            hh = 1

            eom_cc3_31_mem_v06_z8 = ZERO
    if (a >= n0d .and. a <= n1d) then 
!aiajcjal 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(c,d,i,j) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(c,d,j,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(c,d,j,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,j,j) * tvoov(c, i, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,d,i,j) * tvoov(c, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,d,j,i) * tvoov(c, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(c,d,i,j) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(c,d,j,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,j,i) * tvvoo(c, d, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,d,i,j) * tvvoo(c, d, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(c,d,j,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,d,j,j) * tvvoo(c, d, l, i)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aiajcjcl 
 do l = n0l, n1l 
idx = ai_mem(c, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,j,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,d,i,j) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,d,j,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,j,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,d,i,j) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,d,j,i) * tvvoo(a, d, l, j)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajcjej 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * t2(c,e,i,j) * read_ftvvvv(a, e, a, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(a,e,i,j) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajcjej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(c,d,j,i) * read_ftvvvv(a, e, a, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(a,d,j,i) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajcjej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * t2(a,d,j,i) * read_ftvvvv(c, d, a, e)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajcjei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(c,e,j,j) * read_ftvvvv(a, e, a, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(a,e,j,j) * read_ftvvvv(c, e, a, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajcjei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * t2(a,e,j,j) * read_ftvvvv(c, d, a, e)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajcjej 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(a,e,i,j) * read_ftvvvv(c, d, a, e)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajcjdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,l,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,c,l,j) * tvoov(a, i, l, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajcjdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,c,l,j) * tvoov(a, j, l, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aiajcjdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,j,l) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,a,j,l) * tvoov(c, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,c,l,j) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,c,j,l) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(a,a,j,l) * tvvoo(c, d, l, j)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aiajcjdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,c,i,l) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,c,j,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(a,a,j,l) * tvoov(c, i, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,a,i,l) * tvoov(c, j, l, d)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,l,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,c,l,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,j,l) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,c,i,l) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,a,i,l) * tvvoo(c, d, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) - vdav(p, idx) * t2(a,a,j,l) * tvvoo(c, d, l, i)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aiajcjam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * t2(a,c,m,i) * toooo(m, j, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(a,c,m,j) * toooo(m, i, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(a,c,m,j) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aiajcjam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * t2(a,c,j,m) * toooo(m, i, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(a,c,i,m) * toooo(m, j, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(a,c,j,m) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aiajcjcm 
 do m = n0l, n1l 
idx = ai_mem(c, m)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) -2.0_F64 * vdav(p, idx) * t2(a,a,j,l) * toooo(m, i, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(a,a,i,l) * toooo(m, j, l, j)

eom_cc3_31_mem_v06_z8(p)= eom_cc3_31_mem_v06_z8(p) + vdav(p, idx) * t2(a,a,j,l) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 


        !print*, 'hh  z 31_mem z8', hh
    end subroutine sub_eom_cc3_31_mem_v06_z8        
        subroutine sub_eom_cc3_31_mem_v06_z9(eom_cc3_31_mem_v06_z9, t2, nocc, nactive, a, i, b, j, vdav, ntrial,n0d,n1d,n0l,n1l) 
        real(F64), dimension(:), intent(out) :: eom_cc3_31_mem_v06_z9
        real(F64), dimension(:, :), intent(in) :: vdav

        integer, intent(in) :: n0d,n1d,n0l,n1l
    
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                               
            integer, intent(in) :: ntrial, a, i, b, j
            integer :: p, idx ,d,l,e,m
            integer :: hh
            hh = 1

            eom_cc3_31_mem_v06_z9 = ZERO
    if (b >= n0d .and. b <= n1d) then 
!aibjbibl 
 do l = n0l, n1l 
idx = ai_mem(b, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(b,d,i,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,d,j,i) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,d,i,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,j,i) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,d,i,j) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,d,i,i) * tvoov(b, j, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,d,j,i) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(b,d,i,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,j,i) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,d,i,j) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,d,i,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,d,i,i) * tvvoo(b, d, l, j)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjbial 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,d,j,i) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(b,d,i,j) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,d,i,i) * tvoov(b, j, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(b,d,i,i) * tvvoo(b, d, l, j)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,d,j,i) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,d,i,j) * tvvoo(b, d, l, i)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjbiej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * t2(b,d,i,i) * read_ftvvvv(b, d, a, e)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjbiei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(b,e,j,i) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(b,e,i,j) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * t2(a,e,j,i) * read_ftvvvv(b, e, b, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(a,e,i,j) * read_ftvvvv(b, e, b, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjbiei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(b,e,j,i) * read_ftvvvv(b, d, a, e)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * t2(b,e,i,j) * read_ftvvvv(b, d, a, e)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjbiej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(b,d,i,i) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(a,d,i,i) * read_ftvvvv(b, e, b, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjbidi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(b,b,i,l) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,b,j,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,j,l) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,b,i,l) * tvoov(b, j, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,b,j,l) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,b,i,l) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,b,l,j) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,l,i) * tvvoo(b, d, l, j)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,j,l) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,b,i,l) * tvvoo(b, d, l, j)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjbidj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(b,b,i,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,b,i,l) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(b,b,i,l) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,b,l,i) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,b,i,l) * tvvoo(b, d, l, i)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjbidi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,b,l,j) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) - vdav(p, idx) * t2(a,b,l,i) * tvoov(b, j, l, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjbidj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,l,i) * tvoov(b, i, l, d)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjbiam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * t2(b,b,i,m) * toooo(m, i, l, j)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(b,b,j,m) * toooo(m, i, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(b,b,i,m) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibjbibm 
 do m = n0l, n1l 
idx = ai_mem(b, m)
do l = 1, nocc 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(a,b,l,j) * toooo(m, i, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * t2(a,b,l,i) * toooo(m, i, l, j)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(a,b,l,i) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibjbibm 
 do l = n0l, n1l 
idx = ai_mem(b, l)
do m = 1, nocc 
do p = 1, ntrial
cc_hh(9) = cc_hh(9) + 1 

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) -2.0_F64 * vdav(p, idx) * t2(a,b,j,m) * toooo(m, i, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(a,b,i,m) * toooo(m, j, l, i)

eom_cc3_31_mem_v06_z9(p)= eom_cc3_31_mem_v06_z9(p) + vdav(p, idx) * t2(a,b,i,m) * toooo(m, i, l, j)
end do 
end do 
end do 

end if 


        !print*, 'hh  z 31_mem z9', hh
    end subroutine sub_eom_cc3_31_mem_v06_z9        
        subroutine sub_eom_cc3_31_mem_v06_z10(eom_cc3_31_mem_v06_z10, t2, nocc, nactive, a, i, b, j, vdav, ntrial,n0d,n1d,n0l,n1l) 
        real(F64), dimension(:), intent(out) :: eom_cc3_31_mem_v06_z10
        real(F64), dimension(:, :), intent(in) :: vdav

        integer, intent(in) :: n0d,n1d,n0l,n1l
    
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                               
            integer, intent(in) :: ntrial, a, i, b, j
            integer :: p, idx ,d,l,e,m
            integer :: hh
            hh = 1

            eom_cc3_31_mem_v06_z10 = ZERO
    if (b >= n0d .and. b <= n1d) then 
!aibibjbl 
 do l = n0l, n1l 
idx = ai_mem(b, l)
do d = nocc + 1, nactive 
do p = 1, ntrial

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,d,i,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,d,j,i) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(b,d,i,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,d,i,j) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,j,i) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,d,i,i) * tvoov(b, j, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(b,d,i,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,d,j,i) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,d,i,j) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,j,i) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,d,i,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,d,i,i) * tvvoo(b, d, l, j)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibibjal 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do d = nocc + 1, nactive 
do p = 1, ntrial


eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(b,d,i,j) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,d,j,i) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,d,i,i) * tvoov(b, j, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,d,i,j) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,d,j,i) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(b,d,i,i) * tvvoo(b, d, l, j)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibibjei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(b,e,i,j) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(b,e,j,i) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(a,e,i,j) * read_ftvvvv(b, e, b, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * t2(a,e,j,i) * read_ftvvvv(b, e, b, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibibjej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * t2(b,d,i,i) * read_ftvvvv(b, d, a, e)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibibjei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * t2(b,e,i,j) * read_ftvvvv(b, d, a, e)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(b,e,j,i) * read_ftvvvv(b, d, a, e)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibibjej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(b,d,i,i) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(a,d,i,i) * read_ftvvvv(b, e, b, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibibjdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,b,i,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,b,i,l) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(b,b,i,l) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,b,l,i) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,b,i,l) * tvvoo(b, d, l, i)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibibjdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,b,j,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(b,b,i,l) * tvoov(a, j, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,j,l) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,b,i,l) * tvoov(b, j, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,b,i,l) * tvvoo(a, d, l, j)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(b,b,j,l) * tvvoo(a, d, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,l,i) * tvvoo(b, d, l, j)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,b,l,j) * tvvoo(b, d, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,b,i,l) * tvvoo(b, d, l, j)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,j,l) * tvvoo(b, d, l, i)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibibjdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,l,i) * tvoov(b, i, l, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibibjdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,b,l,j) * tvoov(b, i, l, d)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) - vdav(p, idx) * t2(a,b,l,i) * tvoov(b, j, l, d)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibibjam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(b,b,i,m) * toooo(m, j, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(b,b,j,m) * toooo(m, i, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * t2(b,b,i,m) * toooo(m, i, l, j)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibibjbm 
 do m = n0l, n1l 
idx = ai_mem(b, m)
do l = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * t2(a,b,l,i) * toooo(m, i, l, j)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(a,b,l,j) * toooo(m, i, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(a,b,l,i) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibibjbm 
 do l = n0l, n1l 
idx = ai_mem(b, l)
do m = 1, nocc 
do p = 1, ntrial
hh = hh + 1 

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(a,b,i,m) * toooo(m, j, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) -2.0_F64 * vdav(p, idx) * t2(a,b,j,m) * toooo(m, i, l, i)

eom_cc3_31_mem_v06_z10(p)= eom_cc3_31_mem_v06_z10(p) + vdav(p, idx) * t2(a,b,i,m) * toooo(m, i, l, j)
end do 
end do 
end do 

end if 


        !print*, 'hh  z 31_mem z10', hh
    end subroutine sub_eom_cc3_31_mem_v06_z10        
        subroutine sub_eom_cc3_31_mem_vp_z0(eom_cc3_31_mem_vp_z0, t2, nocc, nactive, a, i, b, j, c, k, vdav, ntrial,n0d,n1d,n0l,n1l) 
        real(F64), dimension(:), intent(out) :: eom_cc3_31_mem_vp_z0
        real(F64), dimension(:, :), intent(in) :: vdav

        integer, intent(in) :: n0d,n1d,n0l,n1l
    
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2                                                                                                                                               
            integer, intent(in) :: ntrial, a, i, b, j, c, k
            integer :: p, idx ,d,l,e,m
            integer :: hh
            hh = 1

            eom_cc3_31_mem_vp_z0 = ZERO
    if (b >= n0d .and. b <= n1d) then 
!aibjckbl 
 do l = n0l, n1l 
idx = ai_mem(b, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(c,d,i,k) * tvoov(a, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,k,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(c,d,j,i) * tvoov(a, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,i,j) * tvoov(a, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,j,k) * tvoov(a, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(c,d,k,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,d,j,k) * tvoov(c, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,k,j) * tvoov(c, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,d,k,i) * tvoov(c, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,j,i) * tvoov(c, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,i,k) * tvoov(c, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,d,i,j) * tvoov(c, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(c,d,j,k) * tvvoo(a, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,k,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,d,k,j) * tvvoo(c, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,j,k) * tvvoo(c, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(c,d,i,j) * tvvoo(a, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,j,i) * tvvoo(a, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,i,k) * tvvoo(a, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(c,d,k,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,d,j,i) * tvvoo(c, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,k,i) * tvvoo(c, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,i,j) * tvvoo(c, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,d,i,k) * tvvoo(c, d, l, j)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aibjckcl 
 do l = n0l, n1l 
idx = ai_mem(c, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,d,k,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,i,k) * tvoov(a, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,d,i,j) * tvoov(a, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,k,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,j,i) * tvoov(a, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(b,d,j,k) * tvoov(a, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,d,k,j) * tvoov(b, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,j,k) * tvoov(b, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,d,j,i) * tvoov(b, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,k,i) * tvoov(b, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,i,j) * tvoov(b, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,d,i,k) * tvoov(b, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,d,k,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,j,k) * tvvoo(a, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,d,j,k) * tvvoo(b, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,k,j) * tvvoo(b, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,d,i,k) * tvvoo(a, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,k,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,i,j) * tvvoo(a, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(b,d,j,i) * tvvoo(a, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,d,k,i) * tvvoo(b, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,j,i) * tvvoo(b, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,d,i,k) * tvvoo(b, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,d,i,j) * tvvoo(b, d, l, k)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjckal 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(c,d,j,k) * tvoov(b, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,k,j) * tvoov(b, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,d,k,j) * tvoov(c, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,j,k) * tvoov(c, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(c,d,i,j) * tvoov(b, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,j,i) * tvoov(b, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,i,k) * tvoov(b, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(c,d,k,i) * tvoov(b, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,d,i,k) * tvoov(c, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,k,i) * tvoov(c, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,i,j) * tvoov(c, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(b,d,j,i) * tvoov(c, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(c,d,i,k) * tvvoo(b, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,k,i) * tvvoo(b, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,d,k,i) * tvvoo(c, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,i,k) * tvvoo(c, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(c,d,j,i) * tvvoo(b, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,i,j) * tvvoo(b, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(c,d,j,k) * tvvoo(b, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(c,d,k,j) * tvvoo(b, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,d,i,j) * tvvoo(c, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,k,j) * tvvoo(c, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,d,j,i) * tvvoo(c, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(b,d,j,k) * tvvoo(c, d, l, i)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjckej 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do e = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(c,e,i,k) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,e,i,k) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(a,e,i,k) * read_ftvvvv(c, e, b, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjckej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(c,d,k,i) * read_ftvvvv(b, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(b,d,k,i) * read_ftvvvv(c, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,d,k,i) * read_ftvvvv(c, d, b, e)
end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
!aibjckek 
 do e = n0d, n1d 
idx = ai_mem(e, k)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(c,d,j,i) * read_ftvvvv(b, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(c,d,i,j) * read_ftvvvv(b, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(b,d,i,j) * read_ftvvvv(c, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,d,j,i) * read_ftvvvv(c, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,j,i) * read_ftvvvv(c, d, b, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,d,i,j) * read_ftvvvv(c, d, b, e)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjckei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(c,e,j,k) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(c,e,k,j) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,e,k,j) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(b,e,j,k) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,e,k,j) * read_ftvvvv(c, e, b, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,e,j,k) * read_ftvvvv(c, e, b, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjckei 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do e = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(c,e,j,k) * read_ftvvvv(b, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(c,e,k,j) * read_ftvvvv(b, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(b,e,k,j) * read_ftvvvv(c, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,e,j,k) * read_ftvvvv(c, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,e,j,k) * read_ftvvvv(c, d, b, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,e,k,j) * read_ftvvvv(c, d, b, e)
end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
!aibjckek 
 do e = n0d, n1d 
idx = ai_mem(e, k)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(c,d,i,j) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(c,d,j,i) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,d,i,j) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(b,d,j,i) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,d,j,i) * read_ftvvvv(c, e, b, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(a,d,i,j) * read_ftvvvv(c, e, b, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjckej 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do e = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(c,e,i,k) * read_ftvvvv(b, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(b,e,i,k) * read_ftvvvv(c, d, a, e)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,e,i,k) * read_ftvvvv(c, d, b, e)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjckej 
 do e = n0d, n1d 
idx = ai_mem(e, j)
do d = nocc + 1, nactive 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(c,d,k,i) * read_ftvvvv(b, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,d,k,i) * read_ftvvvv(c, e, a, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,d,k,i) * read_ftvvvv(c, e, b, d)
end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
!aibjckdk 
 do d = n0d, n1d 
idx = ai_mem(d, k)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,c,l,i) * tvoov(a, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,l,j) * tvoov(a, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,c,l,j) * tvoov(b, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,l,j) * tvoov(c, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,l,i) * tvoov(b, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,b,l,i) * tvoov(c, j, l, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjckdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,l,k) * tvoov(a, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,c,l,j) * tvoov(a, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,l,j) * tvoov(b, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,c,l,k) * tvoov(b, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,l,k) * tvoov(c, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,b,l,j) * tvoov(c, k, l, d)
end do 
end do 
end do 

end if 

if (i >= n0l .and. i <= n1l) then 
!aibjckdi 
 do d = n0d, n1d 
idx = ai_mem(d, i)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,c,k,l) * tvoov(a, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,j,l) * tvoov(a, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,c,j,l) * tvoov(b, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,k,l) * tvoov(b, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,b,k,l) * tvoov(c, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,j,l) * tvoov(c, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,l,j) * tvvoo(a, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(b,c,l,k) * tvvoo(a, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,k,l) * tvvoo(a, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(b,c,j,l) * tvvoo(a, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,c,l,j) * tvvoo(b, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,l,k) * tvvoo(b, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,b,l,k) * tvvoo(c, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,l,j) * tvvoo(c, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,b,j,l) * tvvoo(c, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,k,l) * tvvoo(c, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,c,k,l) * tvvoo(b, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,j,l) * tvvoo(b, d, l, k)
end do 
end do 
end do 

end if 

if (k >= n0l .and. k <= n1l) then 
!aibjckdk 
 do d = n0d, n1d 
idx = ai_mem(d, k)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,i,l) * tvoov(a, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(b,c,j,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,b,j,l) * tvoov(c, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,j,l) * tvoov(b, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,c,i,l) * tvoov(b, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,i,l) * tvoov(c, j, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,c,l,j) * tvvoo(a, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,l,i) * tvvoo(a, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,c,i,l) * tvvoo(a, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,j,l) * tvvoo(a, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,c,l,i) * tvvoo(b, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,l,j) * tvvoo(b, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,l,i) * tvvoo(c, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,b,l,j) * tvvoo(c, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,c,j,l) * tvvoo(b, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,j,l) * tvvoo(c, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,i,l) * tvvoo(b, d, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,b,i,l) * tvvoo(c, d, l, j)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjckdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,l,i) * tvoov(a, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(b,c,l,k) * tvoov(a, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,l,k) * tvoov(b, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,b,l,k) * tvoov(c, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,c,l,i) * tvoov(b, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,l,i) * tvoov(c, k, l, d)
end do 
end do 
end do 

end if 

if (j >= n0l .and. j <= n1l) then 
!aibjckdj 
 do d = n0d, n1d 
idx = ai_mem(d, j)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,c,i,l) * tvoov(a, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,k,l) * tvoov(a, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,k,l) * tvoov(c, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,c,k,l) * tvoov(b, i, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,i,l) * tvoov(b, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,b,i,l) * tvoov(c, k, l, d)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,c,l,i) * tvvoo(a, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,l,k) * tvvoo(a, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(b,c,k,l) * tvvoo(a, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(b,c,i,l) * tvvoo(a, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,l,i) * tvvoo(b, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,c,l,k) * tvvoo(b, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,b,l,i) * tvvoo(c, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,l,k) * tvvoo(c, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,c,k,l) * tvvoo(b, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -2.0_F64 * vdav(p, idx) * t2(a,b,k,l) * tvvoo(c, d, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -8.0_F64 * vdav(p, idx) * t2(a,c,i,l) * tvvoo(b, d, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 4.0_F64 * vdav(p, idx) * t2(a,b,i,l) * tvvoo(c, d, l, k)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjckam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(b,c,m,i) * toooo(m, k, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,c,m,k) * toooo(m, i, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(b,c,m,j) * toooo(m, i, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,c,m,i) * toooo(m, j, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,c,m,j) * toooo(m, k, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(b,c,m,k) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (a >= n0d .and. a <= n1d) then 
!aibjckam 
 do l = n0l, n1l 
idx = ai_mem(a, l)
do m = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(b,c,k,m) * toooo(m, i, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,c,i,m) * toooo(m, k, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(b,c,i,m) * toooo(m, j, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,c,k,m) * toooo(m, j, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(b,c,j,m) * toooo(m, i, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(b,c,j,m) * toooo(m, k, l, i)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibjckbm 
 do m = n0l, n1l 
idx = ai_mem(b, m)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,l,j) * toooo(m, i, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,c,l,k) * toooo(m, i, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,l,i) * toooo(m, k, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,c,l,j) * toooo(m, k, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,c,l,i) * toooo(m, j, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(a,c,l,k) * toooo(m, j, l, i)
end do 
end do 
end do 

end if 

if (c >= n0d .and. c <= n1d) then 
!aibjckcm 
 do m = n0l, n1l 
idx = ai_mem(c, m)
do l = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,l,k) * toooo(m, i, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,b,l,j) * toooo(m, i, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,j,l) * toooo(m, i, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,b,k,l) * toooo(m, i, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,l,i) * toooo(m, j, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,b,l,k) * toooo(m, j, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,b,l,i) * toooo(m, k, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(a,b,l,j) * toooo(m, k, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,b,k,l) * toooo(m, j, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,b,j,l) * toooo(m, k, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,b,i,l) * toooo(m, j, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(a,b,i,l) * toooo(m, k, l, j)
end do 
end do 
end do 

end if 

if (b >= n0d .and. b <= n1d) then 
!aibjckbm 
 do l = n0l, n1l 
idx = ai_mem(b, l)
do m = 1, nocc 
do p = 1, ntrial
 

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,k,m) * toooo(m, j, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,c,j,m) * toooo(m, k, l, i)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 2.0_F64 * vdav(p, idx) * t2(a,c,j,m) * toooo(m, i, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,c,k,m) * toooo(m, i, l, j)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) -4.0_F64 * vdav(p, idx) * t2(a,c,i,m) * toooo(m, j, l, k)

eom_cc3_31_mem_vp_z0(p)= eom_cc3_31_mem_vp_z0(p) + 8.0_F64 * vdav(p, idx) * t2(a,c,i,m) * toooo(m, k, l, j)
end do 
end do 
end do 

end if 


        !print*, 'hh  z 31_mem z0', hh
    end subroutine sub_eom_cc3_31_mem_vp_z0
    end module eom_cc3_31_mem
    
