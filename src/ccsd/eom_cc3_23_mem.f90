module eom_cc3_23_mem

    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    use arithmetic
    use cc3_intermediates
    use cc_gparams
    
    implicit none
    !               
    ! File generated automatically on 2017-05-04 17:13:51  
    !  
    contains
            
        subroutine sub_eom_cc3_23_mem_z0(eom_cc3_23_mem_z0, nocc, c, k, d, l, e, m, vdav, ntrial, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j)
        real(F64), dimension(:), intent(out) :: eom_cc3_23_mem_z0
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, c, k, d, l, e, m
            integer :: p, idx ,n,i,j,a,b
            integer :: n0ab, n1ab, n0ij, n1ij
            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            eom_cc3_23_mem_z0 = ZERO
                
    if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
!dkem 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(l, c, n, n) * vdav(p, idx)

idx = aibj_mem(d, k, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, c, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ekdl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, k, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(m, c, n, n) * vdav(p, idx)

idx = aibj_mem(e, k, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, c, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlek 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, e, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(m, c, n, n) * vdav(p, idx)

idx = aibj_mem(d, l, e, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, c, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlem 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 1.9999999999999998_F64 * tovoo(k, c, n, n) * vdav(p, idx)

idx = aibj_mem(d, l, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(n, c, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
!emdk 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, m, d, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(l, c, n, n) * vdav(p, idx)

idx = aibj_mem(e, m, d, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, c, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!emdl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, m, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 1.9999999999999998_F64 * tovoo(k, c, n, n) * vdav(p, idx)

idx = aibj_mem(e, m, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(n, c, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clem 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(k, d, n, n) * vdav(p, idx)

idx = aibj_mem(c, l, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, d, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!emcl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, m, c, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(k, d, n, n) * vdav(p, idx)

idx = aibj_mem(e, m, c, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, d, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlcm 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(k, e, n, n) * vdav(p, idx)

idx = aibj_mem(d, l, c, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, e, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!cmdl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, m, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(k, e, n, n) * vdav(p, idx)

idx = aibj_mem(c, m, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, e, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckel 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(m, d, n, n) * vdav(p, idx)

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, d, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (k >= n0i .and. k <= n1i) then 
!ckem 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 1.9999999999999998_F64 * tovoo(l, d, n, n) * vdav(p, idx)

idx = aibj_mem(c, k, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(n, d, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!elck 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(m, d, n, n) * vdav(p, idx)

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, d, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (k >= n0j .and. k <= n1j) then 
!emck 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, m, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 1.9999999999999998_F64 * tovoo(l, d, n, n) * vdav(p, idx)

idx = aibj_mem(e, m, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(n, d, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdm 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(l, e, n, n) * vdav(p, idx)

idx = aibj_mem(c, k, d, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, e, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dmck 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, m, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(l, e, n, n) * vdav(p, idx)

idx = aibj_mem(d, m, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(n, e, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 1.9999999999999998_F64 * tovoo(m, e, n, n) * vdav(p, idx)

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(n, e, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dlck 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 1.9999999999999998_F64 * tovoo(m, e, n, n) * vdav(p, idx)

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(n, e, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
!diem 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(l, c, k, i) * vdav(p, idx)

idx = aibj_mem(d, i, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!eidl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(m, c, k, i) * vdav(p, idx)

idx = aibj_mem(e, i, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(k, c, m, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!diek 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, e, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(m, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!eidk 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, d, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(l, c, m, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, k, e, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(l, c, m, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ekdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, k, d, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(m, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, l, e, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(m, c, k, j) * vdav(p, idx)

idx = aibj_mem(d, l, e, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(k, c, m, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
!emdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, m, d, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(l, c, k, j) * vdav(p, idx)

idx = aibj_mem(e, m, d, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(k, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alem 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, c, k, d) * vdav(p, idx)

idx = aibj_mem(a, l, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(a, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!embl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, m, b, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, c, k, d) * vdav(p, idx)

idx = aibj_mem(e, m, b, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(b, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlbm 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, l, b, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, c, k, e) * vdav(p, idx)

idx = aibj_mem(d, l, b, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(b, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!amdl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, m, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, c, k, e) * vdav(p, idx)

idx = aibj_mem(a, m, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(a, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akel 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, e, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, c, m, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
!akem 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(a, c, l, d) * vdav(p, idx)

idx = aibj_mem(a, k, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, l, b, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, c, m, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
!embk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, m, b, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(b, c, l, d) * vdav(p, idx)

idx = aibj_mem(e, m, b, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
!akdm 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, c, l, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dmbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, m, b, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, c, l, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akdl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(a, c, m, e) * vdav(p, idx)

idx = aibj_mem(a, k, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, e, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, l, b, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(b, c, m, e) * vdav(p, idx)

idx = aibj_mem(d, l, b, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, e, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!ekbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, k, b, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, d, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!alek 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, e, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, d, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
!dkbm 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, k, b, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
!amdk 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, m, d, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
!ciem 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(k, d, l, i) * vdav(p, idx)

idx = aibj_mem(c, i, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(l, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!eicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, c, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(k, d, m, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!clej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(k, d, m, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
!emcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, m, c, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(k, d, l, j) * vdav(p, idx)

idx = aibj_mem(e, m, c, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(l, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
!dicm 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(k, e, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!cidl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(k, e, m, i) * vdav(p, idx)

idx = aibj_mem(c, i, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(m, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(k, e, m, j) * vdav(p, idx)

idx = aibj_mem(d, l, c, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(m, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
!cmdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, m, d, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(k, e, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ciel 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(m, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(m, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
!cidm 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(l, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dmcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, m, c, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(l, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clbm 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, e, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!amcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, m, c, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, e, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alcm 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, d, k, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!cmbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, m, b, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, d, k, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
!ckej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(m, d, l, j) * vdav(p, idx)

idx = aibj_mem(c, k, e, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(l, d, m, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
!eick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(m, d, l, i) * vdav(p, idx)

idx = aibj_mem(e, i, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(l, d, m, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (k >= n0i .and. k <= n1i) then 
!ckbm 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, d, l, e) * vdav(p, idx)

idx = aibj_mem(c, k, b, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(b, e, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (k >= n0j .and. k <= n1j) then 
!amck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, m, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, d, l, e) * vdav(p, idx)

idx = aibj_mem(a, m, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(a, e, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(b, d, m, e) * vdav(p, idx)

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(b, e, m, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tvvov(a, d, m, e) * vdav(p, idx)

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.49999999999999994_F64 * tvvov(a, e, m, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(l, e, m, j) * vdav(p, idx)

idx = aibj_mem(c, k, d, j)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(m, e, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z0)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + 0.49999999999999994_F64 * tovoo(l, e, m, i) * vdav(p, idx)

idx = aibj_mem(d, i, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) - tovoo(m, e, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
!dkem 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(d, k, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ekdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(e, k, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(m, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(d, l, e, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(m, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlem 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(d, l, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
!emdk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(e, m, d, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!emdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(e, m, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clem 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(c, l, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!emcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(e, m, c, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlcm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(d, l, c, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!cmdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(c, m, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(m, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (k >= n0i .and. k <= n1i) then 
!ckem 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(c, k, e, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!elck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(m, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (k >= n0j .and. k <= n1j) then 
!emck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(e, m, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(c, k, d, m)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(l, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dmck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(d, m, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) -0.5_F64 * tov(l, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tov(m, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dlck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z0)
do p = 1, ntrial 

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z0(p)= eom_cc3_23_mem_z0(p) + tov(m, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 


    end subroutine sub_eom_cc3_23_mem_z0        
        subroutine sub_eom_cc3_23_mem_z1(eom_cc3_23_mem_z1, nocc, c, k, d, l, e, vdav, ntrial, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j )
        real(F64), dimension(:), intent(out) :: eom_cc3_23_mem_z1
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, c, k, d, l, e
            integer :: p, idx ,i,j,a,b,m
            integer :: n0ab, n1ab, n0ij, n1ij
            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            eom_cc3_23_mem_z1 = ZERO
                
    if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!diek 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(l, c, k, i) * vdav(p, idx)

idx = aibj_mem(d, i, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!eidk 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tovoo(l, c, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!eidl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, d, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tovoo(k, c, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, k, e, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(l, c, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ekdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, k, d, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tovoo(l, c, k, j) * vdav(p, idx)

idx = aibj_mem(e, k, d, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tovoo(k, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, l, e, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tovoo(k, c, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akel 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, e, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(a, c, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!alek 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tvvov(a, c, k, d) * vdav(p, idx)

idx = aibj_mem(a, l, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tvvov(a, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, l, b, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(b, c, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!ekbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, k, b, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tvvov(b, c, k, d) * vdav(p, idx)

idx = aibj_mem(e, k, b, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tvvov(b, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, l, b, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tvvov(b, c, k, e) * vdav(p, idx)

idx = aibj_mem(d, l, b, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tvvov(b, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akdl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tvvov(a, c, k, e) * vdav(p, idx)

idx = aibj_mem(a, k, d, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tvvov(a, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (e >= n0b .and. e <= n1b) then 
!akek 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + tvvov(a, c, l, d) * vdav(p, idx)

idx = aibj_mem(a, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(a, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (e >= n0a .and. e <= n1a) then 
!ekbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, k, b, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + tvvov(b, c, l, d) * vdav(p, idx)

idx = aibj_mem(e, k, b, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(b, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0b .and. d <= n1b) then 
!akdk 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(a, c, l, e) * vdav(p, idx)

idx = aibj_mem(a, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(a, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0a .and. d <= n1a) then 
!dkbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, k, b, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(b, c, l, e) * vdav(p, idx)

idx = aibj_mem(d, k, b, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(b, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ciel 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(k, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
!ciek 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(k, d, l, i) * vdav(p, idx)

idx = aibj_mem(c, i, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(l, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
!eick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tovoo(k, d, l, i) * vdav(p, idx)

idx = aibj_mem(e, i, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(l, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!eicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, c, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tovoo(k, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
!ckej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tovoo(k, d, l, j) * vdav(p, idx)

idx = aibj_mem(c, k, e, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(l, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!clej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tovoo(k, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(k, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
!ekcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, k, c, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(k, d, l, j) * vdav(p, idx)

idx = aibj_mem(e, k, c, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(l, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tovoo(k, e, l, i) * vdav(p, idx)

idx = aibj_mem(d, i, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tovoo(l, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!cidl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000002_F64 * tovoo(k, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000002_F64 * tovoo(k, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tovoo(k, e, l, j) * vdav(p, idx)

idx = aibj_mem(c, k, d, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tovoo(l, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!cidk 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(l, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, k, c, j)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(l, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.49999999999999994_F64 * tvvov(b, e, k, d) * vdav(p, idx)

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tvvov(b, d, k, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(b, e, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.49999999999999994_F64 * tvvov(a, e, k, d) * vdav(p, idx)

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tvvov(a, d, k, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!akcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(a, e, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(b, d, l, e) * vdav(p, idx)

idx = aibj_mem(c, k, b, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + tvvov(b, e, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
!akck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(a, d, l, e) * vdav(p, idx)

idx = aibj_mem(a, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + tvvov(a, e, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkek 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ekdk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(e, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ekdl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, k, d, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(e, k, d, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlek 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, l, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckel 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(k, d, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(m, d, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clek 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(k, d, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tovoo(m, d, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!elck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(k, d, m, m) * vdav(p, idx)

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(m, d, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ekcl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, k, c, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(k, d, m, m) * vdav(p, idx)

idx = aibj_mem(e, k, c, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5000000000000001_F64 * tovoo(m, d, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dlck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + tovoo(k, e, m, m) * vdav(p, idx)

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tovoo(m, e, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + tovoo(k, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5000000000000001_F64 * tovoo(m, e, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckek 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 2.0_F64 * tovoo(l, d, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(m, d, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
!ekck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 2.0_F64 * tovoo(l, d, m, m) * vdav(p, idx)

idx = aibj_mem(e, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(m, d, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(l, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(m, e, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z1)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) - tovoo(l, e, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.5_F64 * tovoo(m, e, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (e >= n0ab .and. e <= n1ab) then 
!ekek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(e, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(e, c, l, d) * vdav(p, idx)

idx = aibj_mem(e, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(e, c, l, d) * vdav(p, idx)

idx = aibj_mem(e, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.25_F64 * tvvov(e, d, l, c) * vdav(p, idx)

idx = aibj_mem(e, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.25_F64 * tvvov(e, d, l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.25_F64 * tvvov(d, c, l, e) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.25_F64 * tvvov(d, c, l, e) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.25_F64 * tvvov(d, e, l, c) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.25_F64 * tvvov(d, e, l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.25_F64 * tvvov(c, d, l, e) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.25_F64 * tvvov(c, d, l, e) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(c, e, l, d) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tvvov(c, e, l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(d, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ekdk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(e, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ekdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(e, k, d, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(d, l, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(c, l, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!elck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ekcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(e, k, c, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dlck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tov(k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + 0.49999999999999994_F64 * tov(k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(c, k, e, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
!ekck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(e, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) + tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(c, k, d, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tov(l, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z1)
do p = 1, ntrial 

idx = aibj_mem(d, k, c, k)
eom_cc3_23_mem_z1(p)= eom_cc3_23_mem_z1(p) -0.5_F64 * tov(l, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 


    end subroutine sub_eom_cc3_23_mem_z1        
        subroutine sub_eom_cc3_23_mem_z2(eom_cc3_23_mem_z2, nocc, c, k, d, l, e, vdav, ntrial, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j )
        real(F64), dimension(:), intent(out) :: eom_cc3_23_mem_z2
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, c, k, d, l, e
            integer :: p, idx ,i,j,a,b,m
            integer :: n0ab, n1ab, n0ij, n1ij
            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            eom_cc3_23_mem_z2 = ZERO
                
    if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!diel 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(l, c, k, i) * vdav(p, idx)

idx = aibj_mem(d, i, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!eidl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(l, c, k, i) * vdav(p, idx)

idx = aibj_mem(e, i, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!diek 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, e, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(l, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!eidk 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, d, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(l, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, k, e, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(l, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ekdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, k, d, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(l, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, l, e, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(l, c, k, j) * vdav(p, idx)

idx = aibj_mem(d, l, e, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(k, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!eldj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, l, d, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(l, c, k, j) * vdav(p, idx)

idx = aibj_mem(e, l, d, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(k, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!alel 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(a, c, k, d) * vdav(p, idx)

idx = aibj_mem(a, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + tvvov(a, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!elbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, l, b, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(b, c, k, d) * vdav(p, idx)

idx = aibj_mem(e, l, b, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + tvvov(b, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!dlbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, l, b, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(b, c, k, e) * vdav(p, idx)

idx = aibj_mem(d, l, b, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + tvvov(b, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!aldl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(a, c, k, e) * vdav(p, idx)

idx = aibj_mem(a, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + tvvov(a, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akel 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5000000000000001_F64 * tvvov(a, c, l, d) * vdav(p, idx)

idx = aibj_mem(a, k, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(a, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, l, b, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5000000000000001_F64 * tvvov(b, c, l, d) * vdav(p, idx)

idx = aibj_mem(e, l, b, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(b, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akdl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5000000000000001_F64 * tvvov(a, c, l, e) * vdav(p, idx)

idx = aibj_mem(a, k, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(a, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, l, b, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5000000000000001_F64 * tvvov(b, c, l, e) * vdav(p, idx)

idx = aibj_mem(d, l, b, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(b, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!ekbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, k, b, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(b, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!alek 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, e, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(a, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dkbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, k, b, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(b, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!aldk 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, d, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(a, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ciel 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(k, d, l, i) * vdav(p, idx)

idx = aibj_mem(c, i, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000001_F64 * tovoo(l, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!eicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(k, d, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!clej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(k, d, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(k, d, l, j) * vdav(p, idx)

idx = aibj_mem(e, l, c, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000001_F64 * tovoo(l, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(k, e, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!cidl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(k, e, l, i) * vdav(p, idx)

idx = aibj_mem(c, i, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000001_F64 * tovoo(l, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(k, e, l, j) * vdav(p, idx)

idx = aibj_mem(d, l, c, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000001_F64 * tovoo(l, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!cldj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, d, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(k, e, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!clbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(b, e, k, d) * vdav(p, idx)

idx = aibj_mem(c, l, b, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(b, d, k, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!alcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(a, e, k, d) * vdav(p, idx)

idx = aibj_mem(a, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(a, d, k, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
!ckej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000001_F64 * tovoo(l, d, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
!eick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, c, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000001_F64 * tovoo(l, d, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5000000000000001_F64 * tvvov(b, d, l, e) * vdav(p, idx)

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5000000000000001_F64 * tvvov(b, e, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5000000000000001_F64 * tvvov(a, d, l, e) * vdav(p, idx)

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5000000000000001_F64 * tvvov(a, e, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, j)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000002_F64 * tovoo(l, e, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000002_F64 * tovoo(l, e, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dkel 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ekdl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, k, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(e, k, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlek 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, e, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, l, e, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!dlel 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 2.0_F64 * tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!eldk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, l, d, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(e, l, d, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!eldl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 2.0_F64 * tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(e, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!clel 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(k, d, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(m, d, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!elcl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(k, d, m, m) * vdav(p, idx)

idx = aibj_mem(e, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(m, d, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!dlcl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(k, e, m, m) * vdav(p, idx)

idx = aibj_mem(d, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(m, e, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!cldl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) - tovoo(k, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.5_F64 * tovoo(m, e, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckel 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + tovoo(l, d, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000001_F64 * tovoo(m, d, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!elck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + tovoo(l, d, m, m) * vdav(p, idx)

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000001_F64 * tovoo(m, d, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + tovoo(l, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000001_F64 * tovoo(m, e, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dlck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z2)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + tovoo(l, e, m, m) * vdav(p, idx)

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5000000000000001_F64 * tovoo(m, e, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!elel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(e, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.25_F64 * tvvov(e, c, k, d) * vdav(p, idx)

idx = aibj_mem(e, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.25_F64 * tvvov(e, c, k, d) * vdav(p, idx)

idx = aibj_mem(e, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(e, d, k, c) * vdav(p, idx)

idx = aibj_mem(e, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(e, d, k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!dldl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.25_F64 * tvvov(d, c, k, e) * vdav(p, idx)

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.25_F64 * tvvov(d, c, k, e) * vdav(p, idx)

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(d, e, k, c) * vdav(p, idx)

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tvvov(d, e, k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!clcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.25_F64 * tvvov(c, e, k, d) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.25_F64 * tvvov(c, e, k, d) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.25_F64 * tvvov(c, d, k, e) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.25_F64 * tvvov(c, d, k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dkel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(d, k, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ekdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(e, k, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(d, l, e, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!dlel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(d, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!eldk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(e, l, d, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!eldl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(e, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!clel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(c, l, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!elcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(e, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!dlcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(d, l, c, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tov(k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!cldl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(c, l, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) -0.5_F64 * tov(k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.49999999999999994_F64 * tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!elck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.49999999999999994_F64 * tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.49999999999999994_F64 * tov(l, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dlck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z2)
do p = 1, ntrial 

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z2(p)= eom_cc3_23_mem_z2(p) + 0.49999999999999994_F64 * tov(l, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 


    end subroutine sub_eom_cc3_23_mem_z2        
        subroutine sub_eom_cc3_23_mem_z34(eom_cc3_23_mem_z34, nocc, c, k, l, e, m, vdav, ntrial, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j )
        real(F64), dimension(:), intent(out) :: eom_cc3_23_mem_z34
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, c, k, l, e, m
            integer :: p, idx ,i,j,a,b,n
            integer :: n0ab, n1ab, n0ij, n1ij
            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            eom_cc3_23_mem_z34 = ZERO
                
    if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tov(m, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (k >= n0i .and. k <= n1i) then 
!ckem 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, k, e, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.49999999999999994_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ekcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(e, k, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tov(m, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, l, e, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tov(m, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clem 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, l, e, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!elck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tov(m, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (k >= n0j .and. k <= n1j) then 
!emck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(e, m, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.49999999999999994_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!emcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(e, m, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!cmcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, m, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tov(k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clcm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, l, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tov(k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
if (k >= n0j .and. k <= n1j) then 
!cmck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, m, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tov(l, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
if (k >= n0i .and. k <= n1i) then 
!ckcm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, k, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tov(l, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, l, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + tov(m, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, k, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + tov(m, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ciel 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.49999999999999994_F64 * tovoo(m, c, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
!ciem 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000001_F64 * tovoo(l, c, k, i) * vdav(p, idx)

idx = aibj_mem(c, i, e, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000002_F64 * tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!eicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(m, c, k, i) * vdav(p, idx)

idx = aibj_mem(e, i, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000002_F64 * tovoo(k, c, m, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
!ciek 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(m, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
!eick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.49999999999999994_F64 * tovoo(m, c, l, i) * vdav(p, idx)

idx = aibj_mem(e, i, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000001_F64 * tovoo(l, c, m, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
!ckej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.49999999999999994_F64 * tovoo(m, c, l, j) * vdav(p, idx)

idx = aibj_mem(c, k, e, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000001_F64 * tovoo(l, c, m, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
!ekcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, k, c, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(m, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!clej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(m, c, k, j) * vdav(p, idx)

idx = aibj_mem(c, l, e, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000002_F64 * tovoo(k, c, m, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.49999999999999994_F64 * tovoo(m, c, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
!emcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, m, c, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000001_F64 * tovoo(l, c, k, j) * vdav(p, idx)

idx = aibj_mem(e, m, c, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000002_F64 * tovoo(k, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alem 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, e, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tvvov(a, c, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!embl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, m, b, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tvvov(b, c, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!cmbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, m, b, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tvvov(b, c, k, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clbm 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000001_F64 * tvvov(b, c, k, e) * vdav(p, idx)

idx = aibj_mem(c, l, b, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000002_F64 * tvvov(b, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alcm 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tvvov(a, c, k, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!amcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, m, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000001_F64 * tvvov(a, c, k, e) * vdav(p, idx)

idx = aibj_mem(a, m, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000002_F64 * tvvov(a, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!alek 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, e, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tvvov(a, c, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akel 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, e, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.49999999999999994_F64 * tvvov(a, c, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
!akem 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, e, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tvvov(a, c, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!ekbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, k, b, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tvvov(b, c, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, l, b, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.49999999999999994_F64 * tvvov(b, c, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
!embk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, m, b, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tvvov(b, c, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (k >= n0j .and. k <= n1j) then 
!amck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, m, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.49999999999999994_F64 * tvvov(a, c, l, e) * vdav(p, idx)

idx = aibj_mem(a, m, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tvvov(a, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (k >= n0i .and. k <= n1i) then 
!akcm 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tvvov(a, c, l, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (k >= n0i .and. k <= n1i) then 
!ckbm 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.49999999999999994_F64 * tvvov(b, c, l, e) * vdav(p, idx)

idx = aibj_mem(c, k, b, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tvvov(b, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (k >= n0j .and. k <= n1j) then 
!cmbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, m, b, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tvvov(b, c, l, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + tvvov(a, c, m, e) * vdav(p, idx)

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.49999999999999994_F64 * tvvov(a, e, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!akcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + tvvov(a, c, m, e) * vdav(p, idx)

idx = aibj_mem(a, k, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tvvov(a, e, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + tvvov(b, c, m, e) * vdav(p, idx)

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.49999999999999994_F64 * tvvov(b, e, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + tvvov(b, c, m, e) * vdav(p, idx)

idx = aibj_mem(c, l, b, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5_F64 * tvvov(b, e, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!cicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(k, e, m, i) * vdav(p, idx)

idx = aibj_mem(c, i, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(m, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
!cicm 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tovoo(k, e, l, i) * vdav(p, idx)

idx = aibj_mem(c, i, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tovoo(l, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
!cmcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, m, c, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(k, e, l, j) * vdav(p, idx)

idx = aibj_mem(c, m, c, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(l, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!clcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, c, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tovoo(k, e, m, j) * vdav(p, idx)

idx = aibj_mem(c, l, c, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(m, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
!cick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(l, e, m, i) * vdav(p, idx)

idx = aibj_mem(c, i, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(m, e, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
!ckcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, c, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tovoo(l, e, m, j) * vdav(p, idx)

idx = aibj_mem(c, k, c, j)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(m, e, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!clcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.25_F64 * tovoo(k, e, m, l) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.25000000000000006_F64 * tovoo(k, e, m, l) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(m, e, k, l) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tovoo(m, e, k, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0ij .and. m <= n1ij) then 
!cmcm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, m, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.25000000000000006_F64 * tovoo(k, e, l, m) * vdav(p, idx)

idx = aibj_mem(c, m, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.25_F64 * tovoo(k, e, l, m) * vdav(p, idx)

idx = aibj_mem(c, m, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.25_F64 * tovoo(l, e, k, m) * vdav(p, idx)

idx = aibj_mem(c, m, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.25000000000000006_F64 * tovoo(l, e, k, m) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z34)
do p = 1, ntrial 

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.25_F64 * tovoo(l, e, m, k) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.25000000000000006_F64 * tovoo(l, e, m, k) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(m, e, l, k) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tovoo(m, e, l, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckel 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(m, c, n, n) * vdav(p, idx)

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(n, c, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (k >= n0i .and. k <= n1i) then 
!ckem 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + tovoo(l, c, n, n) * vdav(p, idx)

idx = aibj_mem(c, k, e, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000001_F64 * tovoo(n, c, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ekcl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, k, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(m, c, n, n) * vdav(p, idx)

idx = aibj_mem(e, k, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tovoo(n, c, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clek 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(m, c, n, n) * vdav(p, idx)

idx = aibj_mem(c, l, e, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tovoo(n, c, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clem 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + tovoo(k, c, n, n) * vdav(p, idx)

idx = aibj_mem(c, l, e, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000002_F64 * tovoo(n, c, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!elck 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(m, c, n, n) * vdav(p, idx)

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(n, c, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (k >= n0j .and. k <= n1j) then 
!emck 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, m, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + tovoo(l, c, n, n) * vdav(p, idx)

idx = aibj_mem(e, m, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000001_F64 * tovoo(n, c, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!emcl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, m, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + tovoo(k, c, n, n) * vdav(p, idx)

idx = aibj_mem(e, m, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) -0.5000000000000002_F64 * tovoo(n, c, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!cmcl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, m, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(k, e, n, n) * vdav(p, idx)

idx = aibj_mem(c, m, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(n, e, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clcm 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(k, e, n, n) * vdav(p, idx)

idx = aibj_mem(c, l, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tovoo(n, e, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0i .and. m <= n1i) then 
if (k >= n0j .and. k <= n1j) then 
!cmck 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, m, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(l, e, n, n) * vdav(p, idx)

idx = aibj_mem(c, m, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5_F64 * tovoo(n, e, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (m >= n0j .and. m <= n1j) then 
if (k >= n0i .and. k <= n1i) then 
!ckcm 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(l, e, n, n) * vdav(p, idx)

idx = aibj_mem(c, k, c, m)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 0.5000000000000001_F64 * tovoo(n, e, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clck 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 2.0_F64 * tovoo(m, e, n, n) * vdav(p, idx)

idx = aibj_mem(c, l, c, k)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(n, e, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckcl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z34)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) + 2.0000000000000004_F64 * tovoo(m, e, n, n) * vdav(p, idx)

idx = aibj_mem(c, k, c, l)
eom_cc3_23_mem_z34(p)= eom_cc3_23_mem_z34(p) - tovoo(n, e, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 


    end subroutine sub_eom_cc3_23_mem_z34        
        subroutine sub_eom_cc3_23_mem_z56(eom_cc3_23_mem_z56, nocc, c, k, d, l, m, vdav, ntrial, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j )
        real(F64), dimension(:), intent(out) :: eom_cc3_23_mem_z56
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, c, k, d, l, m
            integer :: p, idx ,i,j,a,b,n
            integer :: n0ab, n1ab, n0ij, n1ij
            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            eom_cc3_23_mem_z56 = ZERO
                
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!dkdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, k, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tov(m, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, k, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
!dmdk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, m, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, l, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tov(m, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (m >= n0i .and. m <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!dmdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, m, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (m >= n0j .and. m <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, l, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!cmdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(c, m, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!cldm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(c, l, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlcm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, l, c, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dmcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, m, c, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.49999999999999994_F64 * tov(m, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(c, k, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dlck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.49999999999999994_F64 * tov(m, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dmck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, m, c, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!didl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(m, c, k, i) * vdav(p, idx)

idx = aibj_mem(d, i, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(k, c, m, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (m >= n0j .and. m <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
!didm 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(l, c, k, i) * vdav(p, idx)

idx = aibj_mem(d, i, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
!didk 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(l, c, m, i) * vdav(p, idx)

idx = aibj_mem(d, i, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(m, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, k, d, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(m, c, l, j) * vdav(p, idx)

idx = aibj_mem(d, k, d, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(l, c, m, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (m >= n0i .and. m <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
!dmdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, m, d, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(l, c, k, j) * vdav(p, idx)

idx = aibj_mem(d, m, d, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(k, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, l, d, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(m, c, k, j) * vdav(p, idx)

idx = aibj_mem(d, l, d, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(k, c, m, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!amdl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, m, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tvvov(a, c, k, d) * vdav(p, idx)

idx = aibj_mem(a, m, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + tvvov(a, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (m >= n0j .and. m <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!aldm 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tvvov(a, c, k, d) * vdav(p, idx)

idx = aibj_mem(a, l, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + tvvov(a, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlbm 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, l, b, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tvvov(b, c, k, d) * vdav(p, idx)

idx = aibj_mem(d, l, b, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + tvvov(b, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (m >= n0i .and. m <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dmbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, m, b, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tvvov(b, c, k, d) * vdav(p, idx)

idx = aibj_mem(d, m, b, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + tvvov(b, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akdl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tvvov(a, c, m, d) * vdav(p, idx)

idx = aibj_mem(a, k, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tvvov(a, d, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
!akdm 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000002_F64 * tvvov(a, c, l, d) * vdav(p, idx)

idx = aibj_mem(a, k, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tvvov(a, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, l, b, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tvvov(b, c, m, d) * vdav(p, idx)

idx = aibj_mem(d, l, b, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tvvov(b, d, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dmbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, m, b, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000002_F64 * tvvov(b, c, l, d) * vdav(p, idx)

idx = aibj_mem(d, m, b, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tvvov(b, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
!dkbm 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, k, b, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tvvov(b, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dkbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, k, b, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tvvov(b, d, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
!amdk 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, m, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tvvov(a, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!aldk 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tvvov(a, d, m, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!cidl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(k, d, m, i) * vdav(p, idx)

idx = aibj_mem(c, i, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tovoo(m, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
!cidm 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(k, d, l, i) * vdav(p, idx)

idx = aibj_mem(c, i, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000002_F64 * tovoo(l, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
!dicm 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(k, d, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(k, d, m, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
!cmdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, m, d, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(k, d, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!cldj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, d, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(k, d, m, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(k, d, m, j) * vdav(p, idx)

idx = aibj_mem(d, l, c, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tovoo(m, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dmcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, m, c, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(k, d, l, j) * vdav(p, idx)

idx = aibj_mem(d, m, c, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000002_F64 * tovoo(l, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!cmbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, m, b, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tvvov(b, d, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clbm 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.49999999999999994_F64 * tvvov(b, d, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alcm 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5_F64 * tvvov(a, d, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!amcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, m, c, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.49999999999999994_F64 * tvvov(a, d, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tovoo(m, d, l, j) * vdav(p, idx)

idx = aibj_mem(c, k, d, j)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000002_F64 * tovoo(l, d, m, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tovoo(m, d, l, i) * vdav(p, idx)

idx = aibj_mem(d, i, c, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000002_F64 * tovoo(l, d, m, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (k >= n0i .and. k <= n1i) then 
!ckbm 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tvvov(b, d, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (k >= n0j .and. k <= n1j) then 
!amck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, m, c, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tvvov(a, d, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tvvov(b, d, m, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tvvov(a, d, m, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!dldl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.25_F64 * tovoo(m, c, k, l) * vdav(p, idx)

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(k, c, m, l) * vdav(p, idx)

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.25000000000000006_F64 * tovoo(m, c, k, l) * vdav(p, idx)

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(k, c, m, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (m >= n0ij .and. m <= n1ij) then 
if (d >= n0ab .and. d <= n1ab) then 
!dmdm 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, m, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.25000000000000006_F64 * tovoo(l, c, k, m) * vdav(p, idx)

idx = aibj_mem(d, m, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(k, c, l, m) * vdav(p, idx)

idx = aibj_mem(d, m, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.25_F64 * tovoo(l, c, k, m) * vdav(p, idx)

idx = aibj_mem(d, m, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(k, c, l, m) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z56)
do p = 1, ntrial 

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.25_F64 * tovoo(l, c, m, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.25000000000000006_F64 * tovoo(m, c, l, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.25_F64 * tovoo(m, c, l, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.25000000000000006_F64 * tovoo(l, c, m, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!dkdl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(m, c, n, n) * vdav(p, idx)

idx = aibj_mem(d, k, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(n, c, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdm 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(l, c, n, n) * vdav(p, idx)

idx = aibj_mem(d, k, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(n, c, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
!dmdk 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, m, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(l, c, n, n) * vdav(p, idx)

idx = aibj_mem(d, m, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(n, c, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldk 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(m, c, n, n) * vdav(p, idx)

idx = aibj_mem(d, l, d, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(n, c, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (m >= n0i .and. m <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!dmdl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, m, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 2.0_F64 * tovoo(k, c, n, n) * vdav(p, idx)

idx = aibj_mem(d, m, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(n, c, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (m >= n0j .and. m <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldm 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 2.0000000000000004_F64 * tovoo(k, c, n, n) * vdav(p, idx)

idx = aibj_mem(d, l, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(n, c, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!cmdl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, m, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(k, d, n, n) * vdav(p, idx)

idx = aibj_mem(c, m, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(n, d, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!cldm 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(k, d, n, n) * vdav(p, idx)

idx = aibj_mem(c, l, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(n, d, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0j .and. m <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlcm 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(k, d, n, n) * vdav(p, idx)

idx = aibj_mem(d, l, c, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5_F64 * tovoo(n, d, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (m >= n0i .and. m <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dmcl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, m, c, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) - tovoo(k, d, n, n) * vdav(p, idx)

idx = aibj_mem(d, m, c, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + 0.5000000000000001_F64 * tovoo(n, d, k, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdl 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + tovoo(m, d, n, n) * vdav(p, idx)

idx = aibj_mem(c, k, d, l)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tovoo(n, d, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (m >= n0j .and. m <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdm 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + tovoo(l, d, n, n) * vdav(p, idx)

idx = aibj_mem(c, k, d, m)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000002_F64 * tovoo(n, d, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dlck 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + tovoo(m, d, n, n) * vdav(p, idx)

idx = aibj_mem(d, l, c, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000001_F64 * tovoo(n, d, m, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (m >= n0i .and. m <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dmck 
!!$omp parallel private(n, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z56)
do n = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, m, c, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) + tovoo(l, d, n, n) * vdav(p, idx)

idx = aibj_mem(d, m, c, k)
eom_cc3_23_mem_z56(p)= eom_cc3_23_mem_z56(p) -0.5000000000000002_F64 * tovoo(n, d, l, n) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 


    end subroutine sub_eom_cc3_23_mem_z56        
        subroutine sub_eom_cc3_23_mem_z7(eom_cc3_23_mem_z7, nocc, c, k, l, e, vdav, ntrial, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j )
        real(F64), dimension(:), intent(out) :: eom_cc3_23_mem_z7
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, c, k, l, e
            integer :: p, idx ,i,j,a,b,m
            integer :: n0ab, n1ab, n0ij, n1ij
            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            eom_cc3_23_mem_z7 = ZERO
                
    if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z7)
do p = 1, ntrial 

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.5_F64 * tov(l, e) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) - tov(l, e) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.4999999999999999_F64 * tvvov(c, c, l, e) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.4999999999999999_F64 * tvvov(c, c, l, e) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.24999999999999994_F64 * tvvov(c, e, l, c) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.25_F64 * tvvov(c, e, l, c) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.24999999999999994_F64 * tovoo(k, e, l, k) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.25_F64 * tovoo(k, e, l, k) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.49999999999999983_F64 * tovoo(l, e, k, k) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.49999999999999994_F64 * tovoo(l, e, k, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ciel 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.4999999999999999_F64 * tovoo(k, c, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
!ciek 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.4999999999999999_F64 * tovoo(l, c, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
!eick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.5_F64 * tovoo(l, c, k, i) * vdav(p, idx)

idx = aibj_mem(e, i, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.49999999999999994_F64 * tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
!ckej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, j)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.49999999999999994_F64 * tovoo(k, c, l, j) * vdav(p, idx)

idx = aibj_mem(c, k, e, j)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.5_F64 * tovoo(l, c, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
!ekcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, k, c, j)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.4999999999999999_F64 * tovoo(l, c, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, j)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.4999999999999999_F64 * tovoo(k, c, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akel 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, e, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.49999999999999994_F64 * tvvov(a, c, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, l, b, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.49999999999999994_F64 * tvvov(b, c, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.5000000000000001_F64 * tvvov(b, c, k, e) * vdav(p, idx)

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.49999999999999994_F64 * tvvov(b, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.49999999999999983_F64 * tvvov(b, c, k, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.5000000000000001_F64 * tvvov(a, c, k, e) * vdav(p, idx)

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.49999999999999994_F64 * tvvov(a, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!akcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.49999999999999983_F64 * tvvov(a, c, k, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (e >= n0b .and. e <= n1b) then 
!akek 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, e, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.5_F64 * tvvov(a, c, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (e >= n0a .and. e <= n1a) then 
!ekbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, k, b, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.5000000000000001_F64 * tvvov(b, c, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
!akck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) - tvvov(a, c, l, e) * vdav(p, idx)

idx = aibj_mem(a, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.5_F64 * tvvov(a, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) - tvvov(b, c, l, e) * vdav(p, idx)

idx = aibj_mem(c, k, b, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.4999999999999999_F64 * tvvov(b, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!cicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, c, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.49999999999999994_F64 * tovoo(k, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
!cick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.4999999999999999_F64 * tovoo(k, e, l, i) * vdav(p, idx)

idx = aibj_mem(c, i, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + tovoo(l, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
!ckcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, c, j)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.5_F64 * tovoo(k, e, l, j) * vdav(p, idx)

idx = aibj_mem(c, k, c, j)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + tovoo(l, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!clcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, c, j)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.5000000000000001_F64 * tovoo(k, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + tovoo(l, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.4999999999999999_F64 * tovoo(m, e, l, m) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -1.9999999999999996_F64 * tovoo(l, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + tovoo(m, e, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z7)
do p = 1, ntrial 

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.5000000000000001_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z7)
do p = 1, ntrial 

idx = aibj_mem(c, k, e, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.4999999999999999_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
!ekck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z7)
do p = 1, ntrial 

idx = aibj_mem(e, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.4999999999999999_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!elck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z7)
do p = 1, ntrial 

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.5000000000000001_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z7)
do p = 1, ntrial 

idx = aibj_mem(c, k, c, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.5_F64 * tov(k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z7)
do p = 1, ntrial 

idx = aibj_mem(c, l, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.49999999999999994_F64 * tov(k, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (e >= n0ab .and. e <= n1ab) then 
!ekek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z7)
do p = 1, ntrial 

idx = aibj_mem(e, k, e, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.25_F64 * tvvov(e, c, l, c) * vdav(p, idx)

idx = aibj_mem(e, k, e, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.25000000000000006_F64 * tvvov(e, c, l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!clcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z7)
do p = 1, ntrial 

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.24999999999999997_F64 * tovoo(k, e, k, l) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.25000000000000006_F64 * tovoo(k, e, k, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckel 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) - tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, e, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.5_F64 * tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckek 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, e, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, e, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.5_F64 * tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
!ekck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(e, k, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.4999999999999999_F64 * tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!elck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) - tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(e, l, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + 0.5_F64 * tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckcl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, c, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + tovoo(k, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, c, l)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.5000000000000001_F64 * tovoo(m, e, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z7)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) + tovoo(k, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, c, k)
eom_cc3_23_mem_z7(p)= eom_cc3_23_mem_z7(p) -0.4999999999999999_F64 * tovoo(m, e, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 


    end subroutine sub_eom_cc3_23_mem_z7        
        subroutine sub_eom_cc3_23_mem_z8(eom_cc3_23_mem_z8, nocc, c, k, l, e, vdav, ntrial, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j )
        real(F64), dimension(:), intent(out) :: eom_cc3_23_mem_z8
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, c, k, l, e
            integer :: p, idx ,i,j,a,b,m
            integer :: n0ab, n1ab, n0ij, n1ij
            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            eom_cc3_23_mem_z8 = ZERO
                
    if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!clcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z8)
do p = 1, ntrial 

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.5_F64 * tov(k, e) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) - tov(k, e) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.4999999999999999_F64 * tvvov(c, c, k, e) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.4999999999999999_F64 * tvvov(c, c, k, e) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.2500000000000001_F64 * tvvov(c, e, k, c) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.2500000000000001_F64 * tvvov(c, e, k, c) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.4999999999999999_F64 * tovoo(k, e, l, l) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.4999999999999999_F64 * tovoo(k, e, l, l) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.25_F64 * tovoo(l, e, k, l) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.24999999999999994_F64 * tovoo(l, e, k, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!eicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(e, i, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.49999999999999983_F64 * tovoo(l, c, k, i) * vdav(p, idx)

idx = aibj_mem(e, i, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.5000000000000002_F64 * tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
!ciek 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.49999999999999983_F64 * tovoo(l, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!ciel 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, e, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.5000000000000002_F64 * tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
!ekcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, k, c, j)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.49999999999999983_F64 * tovoo(l, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!clej 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, j)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.49999999999999983_F64 * tovoo(l, c, k, j) * vdav(p, idx)

idx = aibj_mem(c, l, e, j)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.5000000000000002_F64 * tovoo(k, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!elcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, j)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.5000000000000002_F64 * tovoo(k, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!alel 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, e, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.4999999999999999_F64 * tvvov(a, c, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!elbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, l, b, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.4999999999999999_F64 * tvvov(b, c, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!clbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) - tvvov(b, c, k, e) * vdav(p, idx)

idx = aibj_mem(c, l, b, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.5000000000000002_F64 * tvvov(b, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!alcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) - tvvov(a, c, k, e) * vdav(p, idx)

idx = aibj_mem(a, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.5000000000000002_F64 * tvvov(a, e, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!alek 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, e, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.49999999999999994_F64 * tvvov(a, c, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!ekbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(e, k, b, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.49999999999999994_F64 * tvvov(b, c, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!alck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, c, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.5000000000000002_F64 * tvvov(a, c, l, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!akcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.5_F64 * tvvov(a, c, l, e) * vdav(p, idx)

idx = aibj_mem(a, k, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.49999999999999983_F64 * tvvov(a, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.5000000000000002_F64 * tvvov(b, c, l, e) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.5_F64 * tvvov(b, c, l, e) * vdav(p, idx)

idx = aibj_mem(c, l, b, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.49999999999999983_F64 * tvvov(b, e, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!cicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + tovoo(k, e, l, i) * vdav(p, idx)

idx = aibj_mem(c, i, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.4999999999999999_F64 * tovoo(l, e, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!clcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, c, j)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + tovoo(k, e, l, j) * vdav(p, idx)

idx = aibj_mem(c, l, c, j)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.5_F64 * tovoo(l, e, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
!cick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, c, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.5000000000000001_F64 * tovoo(l, e, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
!ckcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, c, j)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.5_F64 * tovoo(l, e, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!clcl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + tovoo(k, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.4999999999999999_F64 * tovoo(m, e, k, m) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -1.9999999999999996_F64 * tovoo(k, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + tovoo(m, e, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ekcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z8)
do p = 1, ntrial 

idx = aibj_mem(e, k, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.4999999999999999_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clek 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z8)
do p = 1, ntrial 

idx = aibj_mem(c, l, e, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.4999999999999999_F64 * tov(l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!clel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z8)
do p = 1, ntrial 

idx = aibj_mem(c, l, e, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.5_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!elcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z8)
do p = 1, ntrial 

idx = aibj_mem(e, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.5_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z8)
do p = 1, ntrial 

idx = aibj_mem(c, l, c, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.49999999999999994_F64 * tov(l, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z8)
do p = 1, ntrial 

idx = aibj_mem(c, k, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.5_F64 * tov(l, e) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (e >= n0ab .and. e <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!elel 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z8)
do p = 1, ntrial 

idx = aibj_mem(e, l, e, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.24999999999999994_F64 * tvvov(e, c, k, c) * vdav(p, idx)

idx = aibj_mem(e, l, e, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.24999999999999994_F64 * tvvov(e, c, k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z8)
do p = 1, ntrial 

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.25000000000000006_F64 * tovoo(l, e, l, k) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.25_F64 * tovoo(l, e, l, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ekcl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, k, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) - tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(e, k, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.49999999999999994_F64 * tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clek 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) - tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, e, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + 0.49999999999999994_F64 * tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (e >= n0b .and. e <= n1b) then 
if (l >= n0ij .and. l <= n1ij) then 
!clel 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, e, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, e, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.5000000000000002_F64 * tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (e >= n0a .and. e <= n1a) then 
if (l >= n0ij .and. l <= n1ij) then 
!elcl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(e, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(e, l, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.5000000000000002_F64 * tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, c, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + tovoo(l, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, c, k)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.5_F64 * tovoo(m, e, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!ckcl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z8)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) + tovoo(l, e, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, c, l)
eom_cc3_23_mem_z8(p)= eom_cc3_23_mem_z8(p) -0.4999999999999999_F64 * tovoo(m, e, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 


    end subroutine sub_eom_cc3_23_mem_z8        
        subroutine sub_eom_cc3_23_mem_z9(eom_cc3_23_mem_z9, nocc, c, k, d, l, vdav, ntrial, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j )
        real(F64), dimension(:), intent(out) :: eom_cc3_23_mem_z9
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, c, k, d, l
            integer :: p, idx ,i,j,a,b,m
            integer :: n0ab, n1ab, n0ij, n1ij
            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            eom_cc3_23_mem_z9 = ZERO
                
    if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z9)
do p = 1, ntrial 

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.5_F64 * tov(l, c) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) - tov(l, c) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.49999999999999994_F64 * tovoo(l, c, k, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.24999999999999994_F64 * tovoo(k, c, l, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.25_F64 * tovoo(k, c, l, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.49999999999999983_F64 * tovoo(l, c, k, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.2500000000000001_F64 * tvvov(d, c, l, d) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.2500000000000001_F64 * tvvov(d, c, l, d) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.4999999999999999_F64 * tvvov(d, d, l, c) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.49999999999999983_F64 * tvvov(d, d, l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!didl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, d, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.5_F64 * tovoo(k, c, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
!didk 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + tovoo(l, c, k, i) * vdav(p, idx)

idx = aibj_mem(d, i, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.4999999999999999_F64 * tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, k, d, j)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.5_F64 * tovoo(k, c, l, j) * vdav(p, idx)

idx = aibj_mem(d, k, d, j)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + tovoo(l, c, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, l, d, j)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.4999999999999999_F64 * tovoo(k, c, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!aldk 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.49999999999999994_F64 * tvvov(a, c, k, d) * vdav(p, idx)

idx = aibj_mem(a, l, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.4999999999999999_F64 * tvvov(a, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dkbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, k, b, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.49999999999999994_F64 * tvvov(b, c, k, d) * vdav(p, idx)

idx = aibj_mem(d, k, b, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.4999999999999999_F64 * tvvov(b, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0b .and. d <= n1b) then 
!akdk 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.5000000000000002_F64 * tvvov(a, c, l, d) * vdav(p, idx)

idx = aibj_mem(a, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) - tvvov(a, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0a .and. d <= n1a) then 
!dkbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, k, b, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.5000000000000002_F64 * tvvov(b, c, l, d) * vdav(p, idx)

idx = aibj_mem(d, k, b, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) - tvvov(b, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akdl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.5_F64 * tvvov(a, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, l, b, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.5_F64 * tvvov(b, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!cidk 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.49999999999999994_F64 * tovoo(k, d, l, i) * vdav(p, idx)

idx = aibj_mem(c, i, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.5000000000000003_F64 * tovoo(l, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.4999999999999999_F64 * tovoo(k, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!cldj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, d, j)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.4999999999999999_F64 * tovoo(k, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, k, c, j)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.49999999999999994_F64 * tovoo(k, d, l, j) * vdav(p, idx)

idx = aibj_mem(d, k, c, j)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.5000000000000003_F64 * tovoo(l, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, j)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.5000000000000001_F64 * tovoo(l, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.5000000000000001_F64 * tovoo(l, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.49999999999999994_F64 * tvvov(b, d, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!akcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.49999999999999994_F64 * tvvov(a, d, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.4999999999999999_F64 * tvvov(b, d, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
!akck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.4999999999999999_F64 * tvvov(a, d, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.4999999999999999_F64 * tovoo(m, c, l, m) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -1.9999999999999996_F64 * tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!dkdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z9)
do p = 1, ntrial 

idx = aibj_mem(d, k, d, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.49999999999999983_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z9)
do p = 1, ntrial 

idx = aibj_mem(d, l, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.5_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!cldk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z9)
do p = 1, ntrial 

idx = aibj_mem(c, l, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.4999999999999999_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z9)
do p = 1, ntrial 

idx = aibj_mem(d, k, c, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.4999999999999999_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z9)
do p = 1, ntrial 

idx = aibj_mem(c, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.5_F64 * tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z9)
do p = 1, ntrial 

idx = aibj_mem(d, k, c, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.5_F64 * tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!dldl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z9)
do p = 1, ntrial 

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.25_F64 * tovoo(k, c, k, l) * vdav(p, idx)

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.24999999999999994_F64 * tovoo(k, c, k, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z9)
do p = 1, ntrial 

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.24999999999999994_F64 * tvvov(c, d, l, d) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.24999999999999994_F64 * tvvov(c, d, l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!dkdl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, d, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, d, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.5000000000000002_F64 * tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, l, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.5000000000000001_F64 * tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!cldk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) - tovoo(k, d, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.49999999999999994_F64 * tovoo(m, d, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkcl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, c, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) - tovoo(k, d, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, c, l)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + 0.49999999999999994_F64 * tovoo(m, d, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + tovoo(l, d, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, d, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.5000000000000002_F64 * tovoo(m, d, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z9)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, c, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) + tovoo(l, d, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, c, k)
eom_cc3_23_mem_z9(p)= eom_cc3_23_mem_z9(p) -0.5000000000000002_F64 * tovoo(m, d, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 


    end subroutine sub_eom_cc3_23_mem_z9        
        subroutine sub_eom_cc3_23_mem_z10(eom_cc3_23_mem_z10, nocc, c, k, d, l, vdav, ntrial, n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j )
        real(F64), dimension(:), intent(out) :: eom_cc3_23_mem_z10
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i,n0b,n1b,n0j,n1j
    
    integer, intent(in) :: nocc 
            integer, intent(in) :: ntrial, c, k, d, l
            integer :: p, idx ,i,j,a,b,m
            integer :: n0ab, n1ab, n0ij, n1ij
            n0ab = max(n0a, n0b)
            n1ab = min(n1a, n1b)
            n0ij = max(n0i, n0j)
            n1ij = min(n1i, n1j)
            eom_cc3_23_mem_z10 = ZERO
                
    if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z10)
do p = 1, ntrial 

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.5_F64 * tov(l, c) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) - tov(l, c) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.49999999999999983_F64 * tovoo(l, c, k, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.25_F64 * tovoo(k, c, l, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.49999999999999994_F64 * tovoo(l, c, k, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.24999999999999994_F64 * tovoo(k, c, l, k) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.24999999999999994_F64 * tvvov(d, c, l, d) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.24999999999999994_F64 * tvvov(d, c, l, d) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.49999999999999983_F64 * tvvov(d, d, l, c) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tvvov(d, d, l, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
!didk 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + tovoo(l, c, k, i) * vdav(p, idx)

idx = aibj_mem(d, i, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.5_F64 * tovoo(k, c, l, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!didl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, d, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.4999999999999999_F64 * tovoo(k, c, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, k, d, j)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + tovoo(l, c, k, j) * vdav(p, idx)

idx = aibj_mem(d, k, d, j)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.4999999999999999_F64 * tovoo(k, c, l, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, l, d, j)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.5_F64 * tovoo(k, c, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!aldk 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, l, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.4999999999999999_F64 * tvvov(a, c, k, d) * vdav(p, idx)

idx = aibj_mem(a, l, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.5_F64 * tvvov(a, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dkbl 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, k, b, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.4999999999999999_F64 * tvvov(b, c, k, d) * vdav(p, idx)

idx = aibj_mem(d, k, b, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.5_F64 * tvvov(b, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0b .and. d <= n1b) then 
!akdk 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tvvov(a, c, l, d) * vdav(p, idx)

idx = aibj_mem(a, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) - tvvov(a, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0a .and. d <= n1a) then 
!dkbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, k, b, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tvvov(b, c, l, d) * vdav(p, idx)

idx = aibj_mem(d, k, b, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) - tvvov(b, d, l, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0j .and. l <= n1j) then 
!akdl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, d, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tvvov(a, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dlbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(d, l, b, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tvvov(b, d, k, c) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!cidk 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(c, i, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tovoo(k, d, l, i) * vdav(p, idx)

idx = aibj_mem(c, i, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.4999999999999999_F64 * tovoo(l, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
if (l >= n0j .and. l <= n1j) then 
!dicl 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.5_F64 * tovoo(k, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
if (l >= n0i .and. l <= n1i) then 
!cldj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, l, d, j)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.5_F64 * tovoo(k, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkcj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(d, k, c, j)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tovoo(k, d, l, j) * vdav(p, idx)

idx = aibj_mem(d, k, c, j)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.4999999999999999_F64 * tovoo(l, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdj 
!!$omp parallel private(j, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do j = n0j, n1j 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, j)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.5_F64 * tovoo(l, d, k, j) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dick 
!!$omp parallel private(i, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do i = n0i, n1i 
do p = 1, ntrial 

idx = aibj_mem(d, i, c, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.5_F64 * tovoo(l, d, k, i) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
!clbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, l, b, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.49999999999999983_F64 * tvvov(b, d, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
!akcl 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.49999999999999983_F64 * tvvov(a, d, k, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckbk 
!!$omp parallel private(b, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do b = n0b, n1b 
do p = 1, ntrial 

idx = aibj_mem(c, k, b, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.5000000000000001_F64 * tvvov(b, d, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0b .and. c <= n1b) then 
if (k >= n0ij .and. k <= n1ij) then 
!akck 
!!$omp parallel private(a, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
 do a = n0a, n1a 
do p = 1, ntrial 

idx = aibj_mem(a, k, c, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.5000000000000001_F64 * tvvov(a, d, l, d) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (d >= n0ab .and. d <= n1ab) then 
!dkdk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.49999999999999983_F64 * tovoo(m, c, l, m) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -1.9999999999999993_F64 * tovoo(l, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + tovoo(m, c, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!dkdl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z10)
do p = 1, ntrial 

idx = aibj_mem(d, k, d, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.5_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z10)
do p = 1, ntrial 

idx = aibj_mem(d, l, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.49999999999999994_F64 * tov(k, c) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!cldk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z10)
do p = 1, ntrial 

idx = aibj_mem(c, l, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkcl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z10)
do p = 1, ntrial 

idx = aibj_mem(d, k, c, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.5_F64 * tov(k, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z10)
do p = 1, ntrial 

idx = aibj_mem(c, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z10)
do p = 1, ntrial 

idx = aibj_mem(d, k, c, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tov(l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0ij .and. l <= n1ij) then 
!dldl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z10)
do p = 1, ntrial 

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.24999999999999994_F64 * tovoo(k, c, k, l) * vdav(p, idx)

idx = aibj_mem(d, l, d, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.25_F64 * tovoo(k, c, k, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0ab .and. c <= n1ab) then 
if (k >= n0ij .and. k <= n1ij) then 
!ckck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_23_mem_z10)
do p = 1, ntrial 

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.25000000000000006_F64 * tvvov(c, d, l, d) * vdav(p, idx)

idx = aibj_mem(c, k, c, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.25000000000000006_F64 * tvvov(c, d, l, d) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0j .and. l <= n1j) then 
!dkdl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, d, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, d, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.5000000000000001_F64 * tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (d >= n0ab .and. d <= n1ab) then 
if (l >= n0i .and. l <= n1i) then 
!dldk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, l, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + tovoo(k, c, m, m) * vdav(p, idx)

idx = aibj_mem(d, l, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.5000000000000001_F64 * tovoo(m, c, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0j .and. k <= n1j) then 
if (l >= n0i .and. l <= n1i) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!cldk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, l, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) - tovoo(k, d, m, m) * vdav(p, idx)

idx = aibj_mem(c, l, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tovoo(m, d, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (l >= n0j .and. l <= n1j) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkcl 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, c, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) - tovoo(k, d, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, c, l)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + 0.4999999999999999_F64 * tovoo(m, d, k, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0a .and. c <= n1a) then 
if (d >= n0b .and. d <= n1b) then 
!ckdk 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(c, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + tovoo(l, d, m, m) * vdav(p, idx)

idx = aibj_mem(c, k, d, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.4999999999999999_F64 * tovoo(m, d, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 

if (k >= n0ij .and. k <= n1ij) then 
if (c >= n0b .and. c <= n1b) then 
if (d >= n0a .and. d <= n1a) then 
!dkck 
!!$omp parallel private(m, p, idx)& 
!!$omp default(shared)
!!$omp do collapse(2)&
!!$omp reduction (+:eom_cc3_23_mem_z10)
do m = 1, nocc 
do p = 1, ntrial 

idx = aibj_mem(d, k, c, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) + tovoo(l, d, m, m) * vdav(p, idx)

idx = aibj_mem(d, k, c, k)
eom_cc3_23_mem_z10(p)= eom_cc3_23_mem_z10(p) -0.4999999999999999_F64 * tovoo(m, d, l, m) * vdav(p, idx)
end do 
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 
end if 


    end subroutine sub_eom_cc3_23_mem_z10
    end module eom_cc3_23_mem
    
