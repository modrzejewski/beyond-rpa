module eom_cc3_23_mem_noR

    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    use arithmetic
    use cc3_intermediates
    use cc_gparams
    
    implicit none
    !               
    ! File generated automatically on 2017-04-20 14:07:24  
    !  
    contains
                                                                                                         
        subroutine sub_eom_cc3_23_mem_noR_z0(c, k, d, l, e, m, sigrows, d_small, sigma, vdav, &
              ntrial, npair, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, npair, c, k, d, l, e, m
            integer :: p1, p2, ai, bj, ibra ,n
            integer :: n0ab, n1ab, n0ij, n1ij, a, b, i, j
        integer :: hh
        hh = 1

            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            
    if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(l, c, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(m, c, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 2
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(m, c, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 3
 tmp = (one/(eorb-wr)) * sigma(p1) * 3.9999999999999996_F64 * tovoo(k, c, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(l, c, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * 3.9999999999999996_F64 * tovoo(k, c, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, d, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, d, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, e, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 9
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, e, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(n, c, k, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(n, c, k, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, d, k, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, d, k, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, e, k, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, e, k, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, l, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, m, n)
!if (ibra==25)!print*, 17, n, c, m, n, tovoo(n, c, m, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, m, n)
!if (ibra ==25)!print*, 18, n, c, m, n, tovoo(n, c, m, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, l, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(m, d, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * 3.9999999999999996_F64 * tovoo(l, d, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(m, d, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * 3.9999999999999996_F64 * tovoo(l, d, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(l, e, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(l, e, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(n, d, l, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(n, d, l, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 28
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, e, l, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, e, l, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, d, m, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, d, m, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * 3.9999999999999996_F64 * tovoo(m, e, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * 3.9999999999999996_F64 * tovoo(m, e, n, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 34
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(n, e, m, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 35
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(n, e, m, n)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 37
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 39
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, c, l, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, m, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, c, m, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, m, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 43
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 44
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, c, m, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 46
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, c, l, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 48
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 49
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 50
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 51
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 52
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, m, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(a, c, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 54
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, m, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 55
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(b, c, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 56
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 57
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 58
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(a, c, m, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 59
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(b, c, m, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 60
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 61
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, m, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 62
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, m, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 63
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(a, d, k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 64
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 65
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(b, d, k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 66
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 67
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, m, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 68
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, m, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 69
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(b, e, k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 70
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 71
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(a, e, k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 72
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 73
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, m, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 74
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, m, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 75
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 76
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, l, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 77
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, m, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 78
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, m, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 79
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, l, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 80
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 81
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(l, d, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 82
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 83
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(l, d, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 84
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 85
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 86
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(m, e, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 87
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(m, e, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 88
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 89
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 90
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 91
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 92
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, l, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 93
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(l, d, m, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 94
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, l, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 95
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(l, d, m, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 96
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 97
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 98
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(b, d, m, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 99
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(a, d, m, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 100
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, m, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 101
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(b, e, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 102
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, m, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 103
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(a, e, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 104
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, m, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 105
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, m, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 106
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(m, e, l, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 107
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(m, e, l, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 108
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 109
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(m, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 110
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(m, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 111
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 112
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 113
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 114
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 115
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 116
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 117
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 118
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(m, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 119
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 120
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(m, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 121
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 122
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 123
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 124
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(m, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 125
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(m, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do
        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

!print*, 'hh z z0 23_mem_noR', hh

    end subroutine sub_eom_cc3_23_mem_noR_z0                                                                                                     
        subroutine sub_eom_cc3_23_mem_noR_z1(c, k, d, l, e, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j, eorb, wr)
        real(F64), dimension(:,:), intent(inout) :: d_small, sigrows
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
       integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, npair, c, k, d, l, e
            integer :: p1, p2, ai, bj, ibra ,m
            integer :: n0ab, n1ab, n0ij, n1ij, a, b, i, j
        integer :: hh
        hh = 1

            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            
    if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 2
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 9
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 11
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(a, c, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(b, c, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 16
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 17
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 18
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 22
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, e, k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, e, k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 26
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 28
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, e, l, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, e, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 36
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, e, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, e, l, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(l, d, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 39
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, d, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(l, d, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, d, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 43
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 44
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, k, j)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 46
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 47
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 48
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 49
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 50
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 51
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 52
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 53
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 54
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(b, e, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 55
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(a, e, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 56
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, c, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 57
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, c, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 58
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(k, c, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 59
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(k, c, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 60
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, d, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 61
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, d, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 62
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, d, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 63
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, d, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 64
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(k, e, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 65
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(k, e, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 66
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, k, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 67
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, k, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 68
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 69
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 70
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 71
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 72
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, e, k, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 73
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, e, k, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 74
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 75
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 76
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tovoo(l, d, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 77
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tovoo(l, d, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 78
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, e, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 79
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, e, m, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 80
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(m, d, l, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 81
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(m, d, l, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 82
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, e, l, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 83
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, e, l, m)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 84
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(e, c, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 85
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(e, c, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 86
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(d, c, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 87
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(d, c, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 88
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(e, d, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 89
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(e, d, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 90
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(d, e, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 91
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(d, e, l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 92
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(c, d, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 93
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(c, d, l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 94
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(c, e, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 95
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(c, e, l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 96
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 97
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 98
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 99
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, c)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 100
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 101
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 102
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 103
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 104
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 105
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 106
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 107
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(l, d)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 108
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 109
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, e)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        
!$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

!print*, 'hh z z1 23_mem_noR', hh
    end subroutine sub_eom_cc3_23_mem_noR_z1                                                                                                     
        subroutine sub_eom_cc3_23_mem_noR_z2(c, k, d, l, e, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, npair, c, k, d, l, e
            integer :: p1, p2, ai, bj, ibra ,m
            integer :: n0ab, n1ab, n0ij, n1ij, a, b, i, j
        integer :: hh
        hh = 1

            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            
    if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 0
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, i)
do p2 = 1, ntrial

               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, i)
do p2 = 1, ntrial
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 2
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 3
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 4
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 5
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 6
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 7
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 8
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 9
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 10
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 11
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 12
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 13
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 14
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 15
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 17
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 18
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 19
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 20
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 21
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 22
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 23
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(a, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 24
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 25
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(b, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(b, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 31
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(a, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 32
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 33
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 34
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 35
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 36
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 37
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 38
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 39
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 40
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 41
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 42
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, e, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, e, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 45
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 46
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 47
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 48
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 49
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 50
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 51
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 52
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, e, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 53
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, e, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 54
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, e, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 55
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, e, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 56
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 57
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 58
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 59
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tovoo(k, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 60
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 61
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tovoo(k, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 62
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 63
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 64
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 65
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 66
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(m, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 67
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(m, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 68
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 69
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 70
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, e, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 71
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, e, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 72
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 73
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 74
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 75
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 76
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(l, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 77
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(l, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 78
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(l, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 79
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(l, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 80
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, d, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 81
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, d, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 82
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, e, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 83
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, e, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 84
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(e, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 85
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(e, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 86
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(d, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 87
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(d, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 88
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(e, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 89
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(e, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 90
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(d, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 91
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(d, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 92
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(c, e, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 93
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(c, e, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 94
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(c, d, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 95
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tvvov(c, d, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 96
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 97
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 98
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 99
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 100
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 101
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 102
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 103
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 104
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 105
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 106
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 107
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 108
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z2',  ibra, 109
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

!print*, 'hh z z2 23_mem_noR', hh
    end subroutine sub_eom_cc3_23_mem_noR_z2                                                                                                     
        subroutine sub_eom_cc3_23_mem_noR_z34(c, k, l, e, m, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, npair, c, k, l, e, m
            integer :: p1, p2, ai, bj, ibra ,n
            integer :: n0ab, n1ab, n0ij, n1ij, a, b, i, j
        integer :: hh
        hh = 1

            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            
    if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 0
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 2
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 5
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 6
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 7
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 9
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 11
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(m, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(m, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 15
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 18
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 20
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, m, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 21
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, m, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, m, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, m, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 29
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 33
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 36
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 40
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 42
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 43
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 45
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 46
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(a, c, m, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(a, c, m, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 48
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tvvov(b, c, m, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 49
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(b, c, m, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 50
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 51
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 52
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 53
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 54
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 55
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 56
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 57
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 58
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, m, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 59
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 60
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 61
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, e, m, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 62
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 63
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 64
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(m, e, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 65
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(m, e, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 66
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, m, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 67
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, e, m, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 68
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(m, e, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 69
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(m, e, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 70
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5_F64) * tovoo(k, e, m, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0ij .and. m <= n1ij) then 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 71
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tovoo(k, e, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0ij .and. m <= n1ij) then 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 72
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5_F64) * tovoo(k, e, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 73
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tovoo(k, e, m, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0ij .and. m <= n1ij) then 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 74
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5_F64) * tovoo(l, e, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0ij .and. m <= n1ij) then 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 75
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tovoo(l, e, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 76
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, e, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 77
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, e, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 78
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5_F64) * tovoo(l, e, m, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 79
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tovoo(l, e, m, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 80
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, e, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 81
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, e, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 82
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(m, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 83
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(l, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 84
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(m, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 85
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(m, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 86
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.000000000000001_F64 * tovoo(k, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 87
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(m, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 88
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(l, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 89
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.000000000000001_F64 * tovoo(k, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 90
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, e, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 91
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(k, e, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 92
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(n, c, k, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 93
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(n, c, k, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 94
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, e, k, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 95
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, e, k, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 96
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, m, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 97
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(n, c, l, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 98
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, m, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 99
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, m, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 100
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, m, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (e - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 101
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(n, c, l, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 102
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, e, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 103
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(l, e, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 104
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, e, l, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 105
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, e, l, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 106
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tovoo(m, e, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 107
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.000000000000001_F64 * tovoo(m, e, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 108
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(n, e, m, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 109
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(n, e, m, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

!print*, 'hh z z34 23_mem_noR', hh
    end subroutine sub_eom_cc3_23_mem_noR_z34                                                                                                     
        subroutine sub_eom_cc3_23_mem_noR_z56(c, k, d, l, m, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, npair, c, k, d, l, m
            integer :: p1, p2, ai, bj, ibra ,n
            integer :: n0ab, n1ab, n0ij, n1ij, a, b, i, j
        integer :: hh
        hh = 1

            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            
    if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 0
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 1
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 2
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 9
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 10
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(m, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(m, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, m, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, c, m, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 19
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(k, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, m, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 22
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 23
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 24
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 25
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(k, c, m, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 29
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 30
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, m, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 31
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, m, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 36
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 37
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 38
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(a, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tvvov(a, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 42
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, m, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 43
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tvvov(b, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tvvov(b, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 46
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, m, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 48
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 49
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, m, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 50
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 51
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, m, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 52
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, m, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 54
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, d, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 55
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 56
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, d, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 57
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 58
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 59
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 60
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 61
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 62
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, d, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 63
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, m, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 64
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, d, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 65
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, m, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 66
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 67
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 68
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, m, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 69
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, m, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 70
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5_F64) * tovoo(m, c, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0ij .and. m <= n1ij) then 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 71
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tovoo(l, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 72
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5_F64) * tovoo(l, c, m, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 73
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tovoo(m, c, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 74
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, c, m, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0ij .and. m <= n1ij) then 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 75
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 76
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5_F64) * tovoo(m, c, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 77
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tovoo(l, c, m, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0ij .and. m <= n1ij) then 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 78
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5_F64) * tovoo(l, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 79
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tovoo(m, c, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0ij .and. m <= n1ij) then 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 80
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 81
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, c, m, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 82
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(m, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 83
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(l, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 84
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(l, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 85
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(m, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 86
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.0_F64 * tovoo(k, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 87
 tmp = (one/(eorb-wr)) * sigma(p1) * 4.000000000000001_F64 * tovoo(k, c, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 88
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, d, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 89
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(k, d, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 90
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(k, d, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 91
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(k, d, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 92
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tovoo(n, c, k, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 93
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0000000000000004_F64) * tovoo(n, c, k, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 94
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, d, k, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 95
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, d, k, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 96
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, d, k, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 97
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, d, k, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 98
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, m, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 99
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, l, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 100
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, l, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 101
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(n, c, m, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 102
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(m, d, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 103
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.000000000000001_F64 * tovoo(l, d, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 104
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(m, d, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 105
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.000000000000001_F64 * tovoo(l, d, n, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (m - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 106
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(n, d, l, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (m - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 107
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(n, d, l, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do n = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 108
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(n, d, m, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do n = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 109
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(n, d, m, n)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

!print*, 'hh z z56 23_mem_noR', hh
    end subroutine sub_eom_cc3_23_mem_noR_z56                                                                                                     
        subroutine sub_eom_cc3_23_mem_noR_z7(c, k, l, e, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, npair, c, k, l, e
            integer :: p1, p2, ai, bj, ibra ,m
            integer :: n0ab, n1ab, n0ij, n1ij, a, b, i, j
        integer :: hh
        hh = 1

            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            
    if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 0
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 2
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 4
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 5
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 8
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 9
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 10
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 11
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 12
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 13
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 14
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 15
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 17
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999996_F64) * tvvov(a, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 18
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999996_F64) * tvvov(b, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 20
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 22
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, e, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 24
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, e, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, e, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, e, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 27
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999993_F64 * tovoo(l, e, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 28
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tovoo(l, e, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(l, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, e, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 31
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 32
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 33
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 35
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 36
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 37
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tov(l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 38
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5_F64) * tvvov(e, c, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 39
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tvvov(e, c, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 40
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(c, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 41
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(c, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 42
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.4999999999999999_F64) * tvvov(c, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 43
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5_F64) * tvvov(c, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 44
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.49999999999999994_F64 * tovoo(k, e, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 45
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.4999999999999999_F64 * tovoo(k, e, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 46
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tovoo(k, e, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 47
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5000000000000001_F64 * tovoo(k, e, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 48
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, e, k, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 49
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, e, k, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 50
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 51
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 52
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 53
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 54
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tovoo(k, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 55
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(k, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 56
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 57
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 58
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, e, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 59
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, e, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 60
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 61
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 62
 tmp = (one/(eorb-wr)) * sigma(p1) * (-3.999999999999999_F64) * tovoo(l, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

! 63
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(m, e, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

!print*, 'hh z z7 23_mem_noR', hh
    end subroutine sub_eom_cc3_23_mem_noR_z7                                                                                                     
        subroutine sub_eom_cc3_23_mem_noR_z8(c, k, l, e, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, npair, c, k, l, e
            integer :: p1, p2, ai, bj, ibra ,m
            integer :: n0ab, n1ab, n0ij, n1ij, a, b, i, j
        integer :: hh
        hh = 1

            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            
    if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 0
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 1
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 2
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (e - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 4
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 5
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 6
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(l, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 9
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 10
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 11
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999996_F64) * tvvov(b, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 12
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999996_F64) * tvvov(a, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 13
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 14
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 15
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 17
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 18
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 19
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 20
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, e, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 21
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 22
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 23
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(k, e, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 24
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(k, e, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, e, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, e, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 27
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, e, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 28
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, e, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(k, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, e, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 31
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 32
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 33
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 34
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 35
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tov(k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 36
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 37
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 38
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.4999999999999999_F64) * tvvov(e, c, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 39
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.4999999999999999_F64) * tvvov(e, c, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 40
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(c, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 41
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(c, c, k, e)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 42
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000002_F64) * tvvov(c, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 43
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000002_F64) * tvvov(c, e, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 44
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, e, l, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 45
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, e, l, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 46
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tovoo(l, e, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 47
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.4999999999999999_F64 * tovoo(l, e, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 48
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5000000000000001_F64 * tovoo(l, e, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 49
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tovoo(l, e, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 50
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999993_F64) * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 51
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999993_F64) * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 52
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.000000000000001_F64 * tovoo(k, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 53
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.000000000000001_F64 * tovoo(k, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 54
 tmp = (one/(eorb-wr)) * sigma(p1) * (-3.999999999999999_F64) * tovoo(k, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 55
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 56
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 57
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(m, e, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (e - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 58
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (e - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 59
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 60
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0000000000000004_F64 * tovoo(l, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 61
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(l, e, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 62
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, e, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z8', ibra, 63
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, e, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

!print*, 'hh z z8 23_mem_noR', hh
    end subroutine sub_eom_cc3_23_mem_noR_z8                                                                                                     
        subroutine sub_eom_cc3_23_mem_noR_z9(c, k, d, l, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, npair, c, k, d, l
            integer :: p1, p2, ai, bj, ibra ,m
            integer :: n0ab, n1ab, n0ij, n1ij, a, b, i, j
        integer :: hh
        hh = 1

            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            
    if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 0
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 1
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 2
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tovoo(l, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 4
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 5
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999993_F64 * tovoo(l, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 9, tvvov(a, c, l, d), a, d, c, d, d, k, k, k, l, k
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, l, d)
 ! !if(ibra ==25) !print*, 9, a, c, l, d, tvvov(a, c, l, d), sigma(p1), tmp, eorb, wr
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 10
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 11
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 12, (-1.9999999999999996_F64) * tvvov(a, d, l, c)
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999996_F64) * tvvov(a, d, l, c)
!if (ibra ==25) !print*, 12, a, d, l, c, (-1.9999999999999996_F64) * tvvov(a, d, l, c), sigma(p1), tmp, eorb, wr
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 13
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999993_F64) * tvvov(b, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 14
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 15
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 17, tovoo(k, d, l, i)
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, i)
!if (ibra ==25) !print*, 17, k, d, l, i, tovoo(k, d, l, i), sigma(p1), tmp, eorb, wr
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 18
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 19
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 20
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 21, -tovoo(l, d, k, j)
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, j)
!!if(ibra==25)!print*, 21, l, d, k, j, -tovoo(l, d, k, j), sigma(p1), tmp, eorb, wr
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 22, -tovoo(l, d, k, i)
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, i)
!if (ibra ==25) !print*, 22, l, d, k, i, -tovoo(l, d, k, i), sigma(p1), tmp, eorb, wr
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 24
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 27,  tvvov(b, d, l, d)
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, l, d)
!if (ibra ==25) !print*, 27, b, d, l, d, tvvov(b, d, l, d), sigma(p1), tmp, eorb, wr
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 28
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 31
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 32
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 33
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 36, tov(l, d)
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, d)
!!if(ibra==25)!print*, 36, l, d, tov(l, d), sigma(p1), tmp, eorb, wr
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 37
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 38
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tovoo(k, c, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 39
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, k, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 40
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.4999999999999999_F64 * tovoo(k, c, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 41
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tovoo(k, c, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 42
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, k, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 43
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.4999999999999999_F64 * tovoo(k, c, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 44
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000002_F64) * tvvov(d, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 45
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000002_F64) * tvvov(d, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 46
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(d, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 47
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(d, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 48
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.4999999999999999_F64) * tvvov(c, d, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 49
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.4999999999999999_F64) * tvvov(c, d, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 50
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tovoo(k, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 51
 tmp = (one/(eorb-wr)) * sigma(p1) * (-3.999999999999999_F64) * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 52
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(k, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 53
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 54
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999998_F64) * tovoo(k, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 55
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 56
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 57
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 58
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 59
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 60, 2.000000000000001_F64 * tovoo(l, d, m, m)
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.000000000000001_F64 * tovoo(l, d, m, m)
!!if(ibra==25)!print*, 60, l, d, m, m, 2.000000000000001_F64 * tovoo(l, d, m, m), sigma(p1), tmp, eorb, wr
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 61
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.000000000000001_F64 * tovoo(l, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

      !if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 62, -tovoo(m, d, l, m)
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, d, l, m)
!!if(ibra==25)!print*, 62, m, d, l, m, -tovoo(m, d, l, m), sigma(p1), tmp, eorb, wr
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z9', ibra, 63
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, d, l, m)
!if (ibra ==25) !print*, 63, m, d, l, m, -tovoo(m, d, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

!print*, 'hh z z9 23_mem_noR', hh
    end subroutine sub_eom_cc3_23_mem_noR_z9                                                                                                     
        subroutine sub_eom_cc3_23_mem_noR_z10(c, k, d, l, sigrows, d_small, sigma, vdav, ntrial, npair, nocc0, nvirt0, nocc, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j, eorb, wr)
        real(F64), dimension(:, :), intent(inout) :: sigrows
        real(F64), dimension(:,:), intent(inout) :: d_small
        real(F64), dimension(:), intent(in) :: sigma
        real(F64), intent(in) :: eorb, wr
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: nocc0, nvirt0
        real(F64) :: tmp
    integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, npair, c, k, d, l
            integer :: p1, p2, ai, bj, ibra ,m
            integer :: n0ab, n1ab, n0ij, n1ij, a, b, i, j
        integer :: hh
        hh = 1

            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            
    if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 0
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 1
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999993_F64 * tovoo(l, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 2
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 3
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 4
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999998_F64 * tovoo(l, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 5
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 6
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(k, c, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 7
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 8
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, c, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 9
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 10
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 11
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999993_F64) * tvvov(a, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 12
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 13
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 14
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999996_F64) * tvvov(b, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 15
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 16
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 17
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 18
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 19
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 20
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(k, d, l, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 21
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (c - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 22
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
 do i = n0i, n1i 
 ai = (d - nvirt0) * nocc + (i - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 23
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, i)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
 do j = n0j, n1j 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (j - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 24
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, d, k, j)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 25
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(b, d, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 26
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tvvov(a, d, k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 do b = n0b, n1b 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (b - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 27
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(b, d, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 do a = n0a, n1a 
 ai = (a - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 28
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(a, d, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 29
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999993_F64 * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 30
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 31
 tmp = (one/(eorb-wr)) * sigma(p1) * (-2.0_F64) * tov(l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 32
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 33
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(k, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 34
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 35
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tov(k, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 36
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 37
 tmp = (one/(eorb-wr)) * sigma(p1) * tov(l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 38
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, k, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 39
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.4999999999999999_F64 * tovoo(k, c, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 40
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tovoo(k, c, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 41
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(l, c, k, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 42
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.4999999999999999_F64 * tovoo(k, c, l, k)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 43
 tmp = (one/(eorb-wr)) * sigma(p1) * 0.5_F64 * tovoo(k, c, k, l)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 44
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.4999999999999999_F64) * tvvov(d, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 45
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.4999999999999999_F64) * tvvov(d, c, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 46
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(d, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 47
 tmp = (one/(eorb-wr)) * sigma(p1) * tvvov(d, d, l, c)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 48
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tvvov(c, d, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 49
 tmp = (one/(eorb-wr)) * sigma(p1) * (-0.5000000000000001_F64) * tvvov(c, d, l, d)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 50
 tmp = (one/(eorb-wr)) * sigma(p1) * (-3.9999999999999987_F64) * tovoo(l, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 51
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999996_F64 * tovoo(k, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 52
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tovoo(k, c, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 53
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999996_F64) * tovoo(k, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 54
 tmp = (one/(eorb-wr)) * sigma(p1) * (-1.9999999999999996_F64) * tovoo(k, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 55
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 56
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, c, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (l - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 57
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (l - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 58
 tmp = (one/(eorb-wr)) * sigma(p1) * tovoo(m, d, k, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 59
 tmp = (one/(eorb-wr)) * sigma(p1) * 1.9999999999999993_F64 * tovoo(m, c, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 60
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tovoo(l, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 61
 tmp = (one/(eorb-wr)) * sigma(p1) * 2.0_F64 * tovoo(l, d, m, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (c - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (d - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 62
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, d, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
do m = 1, nocc 
 ai = (d - nvirt0) * nocc + (k - nocc0) + 1 
             bj = (c - nvirt0) * nocc + (k - nocc0) + 1 
             if (ai .ge. bj) then 
             ibra = npair + ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
do p1 = 1, ntrial 

!if(ibra ==25) !print*, 'laciaton', 'z10', ibra, 63
 tmp =  (one/(eorb-wr)) * (- sigma(p1)) * tovoo(m, d, l, m)
do p2 = 1, ntrial
hh = hh + 1
               !if (abs(vdav(p2, ibra) * tmp).gt.1.d-8.and.p1==2.and.p2==2)!print*, 'kaktus', vdav(p2, ibra), tmp, vdav(p2, ibra) * tmp
d_small(p2, p1) = d_small(p2, p1) + vdav(p2, ibra) * tmp
!if(abs(d_small(p2, p1)).gt.1.d-8)!print*, 'd_small', p2, p1, d_small(p2, p1)
        end do

        !$omp atomic
sigrows(p1, ibra) = sigrows(p1, ibra) + tmp        
         end do 
end if 
end do 

end if 
end if 
end if 

!print*, 'hh z z10 23_mem_noR', hh
    end subroutine sub_eom_cc3_23_mem_noR_z10
    end module eom_cc3_23_mem_noR
    
