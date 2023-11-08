module eom_cc3_13_mem

    use ccsd_transformed_integrals
    use t1_transformed_int
    use basis
    use arithmetic
    use cc3_intermediates
    use cc_gparams
    
    implicit none
    !               
    ! File generated automatically on 2017-05-02 17:25:00  
    !  
    contains
            
        subroutine sub_eom_cc3_13_mem_z0(eom_cc3_13_mem_z0, b, j, c, k, d, l, vdav, ntrial, n0a,n1a,n0i,n1i) 
        real(F64), dimension(:), intent(out) :: eom_cc3_13_mem_z0
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i
      
            integer, intent(in) :: ntrial, b, j, c, k, d, l
            integer :: p, idx 
            eom_cc3_13_mem_z0 = ZERO
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z0)
do p = 1, ntrial 

idx = ai_mem(d, k)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) + vovo(b, l, c, j) * vdav(p, idx)

idx = ai_mem(d, k)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) -2.0_F64 * vovo(b, j, c, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z0)
do p = 1, ntrial 

idx = ai_mem(d, l)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) -2.0_F64 * vovo(b, k, c, j) * vdav(p, idx)

idx = ai_mem(d, l)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) + 4.0_F64 * vovo(b, j, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!cl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z0)
do p = 1, ntrial 

idx = ai_mem(c, l)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) + vovo(b, k, d, j) * vdav(p, idx)

idx = ai_mem(c, l)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) -2.0_F64 * vovo(b, j, d, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
!ck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z0)
do p = 1, ntrial 

idx = ai_mem(c, k)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) -2.0_F64 * vovo(b, l, d, j) * vdav(p, idx)

idx = ai_mem(c, k)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) + 4.0_F64 * vovo(b, j, d, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z0)
do p = 1, ntrial 

idx = ai_mem(d, j)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) + vovo(b, k, c, l) * vdav(p, idx)

idx = ai_mem(d, j)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) -2.0_F64 * vovo(b, l, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!cj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z0)
do p = 1, ntrial 

idx = ai_mem(c, j)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) + vovo(b, l, d, k) * vdav(p, idx)

idx = ai_mem(c, j)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) -2.0_F64 * vovo(b, k, d, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!bl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z0)
do p = 1, ntrial 

idx = ai_mem(b, l)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) + vovo(c, j, d, k) * vdav(p, idx)

idx = ai_mem(b, l)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) -2.0_F64 * vovo(c, k, d, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
!bk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z0)
do p = 1, ntrial 

idx = ai_mem(b, k)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) -2.0_F64 * vovo(c, j, d, l) * vdav(p, idx)

idx = ai_mem(b, k)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) + vovo(c, l, d, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!bj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z0)
do p = 1, ntrial 

idx = ai_mem(b, j)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) -2.0_F64 * vovo(c, l, d, k) * vdav(p, idx)

idx = ai_mem(b, j)
eom_cc3_13_mem_z0(p)= eom_cc3_13_mem_z0(p) + 4.0_F64 * vovo(c, k, d, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 


    end subroutine sub_eom_cc3_13_mem_z0        
        subroutine sub_eom_cc3_13_mem_z1(eom_cc3_13_mem_z1, b, j, c, k, d, vdav, ntrial, n0a,n1a,n0i,n1i) 
        real(F64), dimension(:), intent(out) :: eom_cc3_13_mem_z1
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i
      
            integer, intent(in) :: ntrial, b, j, c, k, d
            integer :: p, idx 
            eom_cc3_13_mem_z1 = ZERO
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z1)
do p = 1, ntrial 

idx = ai_mem(d, k)
eom_cc3_13_mem_z1(p)= eom_cc3_13_mem_z1(p) - vovo(b, j, c, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z1)
do p = 1, ntrial 

idx = ai_mem(d, j)
!if(idx == 12) print*, 'indexxxx z1', idx
eom_cc3_13_mem_z1(p)= eom_cc3_13_mem_z1(p) - vovo(b, k, c, j) * vdav(p, idx)

idx = ai_mem(d, j)
!if(idx == 12) print*, 'indexxxx z1', idx
eom_cc3_13_mem_z1(p)= eom_cc3_13_mem_z1(p) + 2.0_F64 * vovo(b, j, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!cj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z1)
do p = 1, ntrial 

idx = ai_mem(c, j)
!if(idx == 12) print*, 'indexxxx z1', idx
eom_cc3_13_mem_z1(p)= eom_cc3_13_mem_z1(p) - vovo(b, k, d, j) * vdav(p, idx)

idx = ai_mem(c, j)
!if(idx == 12) print*, 'indexxxx z1', idx
eom_cc3_13_mem_z1(p)= eom_cc3_13_mem_z1(p) - vovo(b, j, d, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
!ck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z1)
do p = 1, ntrial 

idx = ai_mem(c, k)
!if(idx == 12) print*, 'indexxxx z1', idx
eom_cc3_13_mem_z1(p)= eom_cc3_13_mem_z1(p) + 2.0_F64 * vovo(b, j, d, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!bj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z1)
do p = 1, ntrial 

idx = ai_mem(b, j)
!if(idx == 12) print*, 'indexxxx z1', idx
eom_cc3_13_mem_z1(p)= eom_cc3_13_mem_z1(p) - vovo(c, j, d, k) * vdav(p, idx)

idx = ai_mem(b, j)
!if(idx == 12) print*, 'indexxxx z1', idx
eom_cc3_13_mem_z1(p)= eom_cc3_13_mem_z1(p) + 2.0_F64 * vovo(c, k, d, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
!bk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z1)
do p = 1, ntrial 

idx = ai_mem(b, k)
!if(idx == 12) print*, 'indexxxx z1', idx
eom_cc3_13_mem_z1(p)= eom_cc3_13_mem_z1(p) - vovo(c, j, d, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 


    end subroutine sub_eom_cc3_13_mem_z1        
        subroutine sub_eom_cc3_13_mem_z2(eom_cc3_13_mem_z2, b, j, c, k, d, vdav, ntrial, n0a,n1a,n0i,n1i) 
        real(F64), dimension(:), intent(out) :: eom_cc3_13_mem_z2
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i
      
            integer, intent(in) :: ntrial, b, j, c, k, d
            integer :: p, idx 
            eom_cc3_13_mem_z2 = ZERO
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z2)
do p = 1, ntrial 

idx = ai_mem(d, k)
!if(idx==12)print*, 'idnedxxxz2', idx
eom_cc3_13_mem_z2(p) = eom_cc3_13_mem_z2(p) - vovo(b, k, c, j) * vdav(p, idx)

idx = ai_mem(d, k)
!if(idx==12)print*, 'idnedxxxz2', idx
eom_cc3_13_mem_z2(p) = eom_cc3_13_mem_z2(p) + 2.0_F64 * vovo(b, j, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
!ck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z2)
do p = 1, ntrial 

idx = ai_mem(c, k)
!if(idx==12)print*, 'idnedxxxz2', idx
eom_cc3_13_mem_z2(p) = eom_cc3_13_mem_z2(p) - vovo(b, k, d, j) * vdav(p, idx)

idx = ai_mem(c, k)
!if(idx==12)print*, 'idnedxxxz2', idx
eom_cc3_13_mem_z2(p) = eom_cc3_13_mem_z2(p) + 2.0_F64 * vovo(b, j, d, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z2)
do p = 1, ntrial 

idx = ai_mem(d, j)
!if(idx==12)print*, 'idnedxxxz2', idx
eom_cc3_13_mem_z2(p) = eom_cc3_13_mem_z2(p) - vovo(b, k, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!cj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z2)
do p = 1, ntrial 

idx = ai_mem(c, j)
!if(idx==12)print*, 'idnedxxxz2', idx
eom_cc3_13_mem_z2(p) = eom_cc3_13_mem_z2(p) - vovo(b, k, d, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
!bk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z2)
do p = 1, ntrial 

idx = ai_mem(b, k)
!if(idx==12)print*, 'idnedxxxz2', idx
eom_cc3_13_mem_z2(p) = eom_cc3_13_mem_z2(p) - vovo(c, j, d, k) * vdav(p, idx)

idx = ai_mem(b, k)
!if(idx==12)print*, 'idnedxxxz2', idx
eom_cc3_13_mem_z2(p) = eom_cc3_13_mem_z2(p) - vovo(c, k, d, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!bj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z2)
do p = 1, ntrial 

idx = ai_mem(b, j)
!if(idx==12)print*, 'idnedxxxz2', idx
eom_cc3_13_mem_z2(p) = eom_cc3_13_mem_z2(p) + 2.0_F64 * vovo(c, k, d, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 


    end subroutine sub_eom_cc3_13_mem_z2        
        subroutine sub_eom_cc3_13_mem_z34(eom_cc3_13_mem_z34, b, j, k, d, l, vdav, ntrial, n0a,n1a,n0i,n1i) 
        real(F64), dimension(:), intent(out) :: eom_cc3_13_mem_z34
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i
      
            integer, intent(in) :: ntrial, b, j, k, d, l
            integer :: p, idx 
            eom_cc3_13_mem_z34 = ZERO
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z34)
do p = 1, ntrial 

idx = ai_mem(d, k)
eom_cc3_13_mem_z34(p)= eom_cc3_13_mem_z34(p) - vovo(b, j, b, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (d >= n0a .and. d <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!dl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z34)
do p = 1, ntrial 

idx = ai_mem(d, l)
eom_cc3_13_mem_z34(p)= eom_cc3_13_mem_z34(p) + 2.0_F64 * vovo(b, j, b, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!bl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z34)
do p = 1, ntrial 

idx = ai_mem(b, l)
eom_cc3_13_mem_z34(p)= eom_cc3_13_mem_z34(p) - vovo(b, k, d, j) * vdav(p, idx)

idx = ai_mem(b, l)
eom_cc3_13_mem_z34(p)= eom_cc3_13_mem_z34(p) - vovo(b, j, d, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
!bk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z34)
do p = 1, ntrial 

idx = ai_mem(b, k)
eom_cc3_13_mem_z34(p)= eom_cc3_13_mem_z34(p) - vovo(b, l, d, j) * vdav(p, idx)

idx = ai_mem(b, k)
eom_cc3_13_mem_z34(p)= eom_cc3_13_mem_z34(p) + 2.0_F64 * vovo(b, j, d, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z34)
do p = 1, ntrial 

idx = ai_mem(d, j)
eom_cc3_13_mem_z34(p)= eom_cc3_13_mem_z34(p) - vovo(b, k, b, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!bj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z34)
do p = 1, ntrial 

idx = ai_mem(b, j)
eom_cc3_13_mem_z34(p)= eom_cc3_13_mem_z34(p) - vovo(b, l, d, k) * vdav(p, idx)

idx = ai_mem(b, j)
eom_cc3_13_mem_z34(p)= eom_cc3_13_mem_z34(p) + 2.0_F64 * vovo(b, k, d, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 


    end subroutine sub_eom_cc3_13_mem_z34        
        subroutine sub_eom_cc3_13_mem_z56(eom_cc3_13_mem_z56, b, j, c, k, l, vdav, ntrial, n0a,n1a,n0i,n1i) 
        real(F64), dimension(:), intent(out) :: eom_cc3_13_mem_z56
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i
      
            integer, intent(in) :: ntrial, b, j, c, k, l
            integer :: p, idx 
            eom_cc3_13_mem_z56 = ZERO
    if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
!ck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z56)
do p = 1, ntrial 

idx = ai_mem(c, k)
eom_cc3_13_mem_z56(p)= eom_cc3_13_mem_z56(p) - vovo(b, l, c, j) * vdav(p, idx)

idx = ai_mem(c, k)
eom_cc3_13_mem_z56(p)= eom_cc3_13_mem_z56(p) + 2.0_F64 * vovo(b, j, c, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!cl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z56)
do p = 1, ntrial 

idx = ai_mem(c, l)
eom_cc3_13_mem_z56(p)= eom_cc3_13_mem_z56(p) - vovo(b, k, c, j) * vdav(p, idx)

idx = ai_mem(c, l)
eom_cc3_13_mem_z56(p)= eom_cc3_13_mem_z56(p) + 2.0_F64 * vovo(b, j, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!cj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z56)
do p = 1, ntrial 

idx = ai_mem(c, j)
eom_cc3_13_mem_z56(p)= eom_cc3_13_mem_z56(p) - vovo(b, k, c, l) * vdav(p, idx)

idx = ai_mem(c, j)
eom_cc3_13_mem_z56(p)= eom_cc3_13_mem_z56(p) - vovo(b, l, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (l >= n0i .and. l <= n1i) then 
!bl 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z56)
do p = 1, ntrial 

idx = ai_mem(b, l)
eom_cc3_13_mem_z56(p)= eom_cc3_13_mem_z56(p) - vovo(c, j, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
!bk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z56)
do p = 1, ntrial 

idx = ai_mem(b, k)
eom_cc3_13_mem_z56(p)= eom_cc3_13_mem_z56(p) - vovo(c, j, c, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!bj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z56)
do p = 1, ntrial 

idx = ai_mem(b, j)
eom_cc3_13_mem_z56(p)= eom_cc3_13_mem_z56(p) + 2.0_F64 * vovo(c, k, c, l) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 


    end subroutine sub_eom_cc3_13_mem_z56        
        subroutine sub_eom_cc3_13_mem_z7(eom_cc3_13_mem_z7, b, j, k, d, vdav, ntrial, n0a,n1a,n0i,n1i) 
        real(F64), dimension(:), intent(out) :: eom_cc3_13_mem_z7
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i
      
            integer, intent(in) :: ntrial, b, j, k, d
            integer :: p, idx 
            eom_cc3_13_mem_z7 = ZERO
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z7)
do p = 1, ntrial 

idx = ai_mem(d, k)
!if (idx==12)print*, 'indexxxz7', idx
eom_cc3_13_mem_z7(p)= eom_cc3_13_mem_z7(p) - vovo(b, j, b, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z7)
do p = 1, ntrial 

idx = ai_mem(d, j)
!if (idx==12)print*, 'indexxxz7', idx
eom_cc3_13_mem_z7(p)= eom_cc3_13_mem_z7(p) + vovo(b, j, b, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!bj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z7)
do p = 1, ntrial 

idx = ai_mem(b, j)
!if (idx==12)print*, 'indexxxz7', idx
eom_cc3_13_mem_z7(p)= eom_cc3_13_mem_z7(p) + vovo(b, k, d, j) * vdav(p, idx)

idx = ai_mem(b, j)
!if (idx==12)print*, 'indexxxz7', idx
eom_cc3_13_mem_z7(p)= eom_cc3_13_mem_z7(p) -2.0_F64 * vovo(b, j, d, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
!bk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z7)
do p = 1, ntrial 

idx = ai_mem(b, k)
!if (idx==12)print*, 'indexxxz7', idx
eom_cc3_13_mem_z7(p)= eom_cc3_13_mem_z7(p) + vovo(b, j, d, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 


    end subroutine sub_eom_cc3_13_mem_z7        
        subroutine sub_eom_cc3_13_mem_z8(eom_cc3_13_mem_z8, b, j, k, d, vdav, ntrial, n0a,n1a,n0i,n1i) 
        real(F64), dimension(:), intent(out) :: eom_cc3_13_mem_z8
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i
      
            integer, intent(in) :: ntrial, b, j, k, d
            integer :: p, idx 
            eom_cc3_13_mem_z8 = ZERO
    if (k >= n0i .and. k <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z8)
do p = 1, ntrial 

idx = ai_mem(d, k)
!if (idx==12)print*, 'indexxxxz8', idx
eom_cc3_13_mem_z8(p)= eom_cc3_13_mem_z8(p) + vovo(b, j, b, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
!bk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z8)
do p = 1, ntrial 

idx = ai_mem(b, k)
!if (idx==12)print*, 'indexxxxz8', idx
eom_cc3_13_mem_z8(p)= eom_cc3_13_mem_z8(p) -2.0_F64 * vovo(b, k, d, j) * vdav(p, idx)

idx = ai_mem(b, k)
!if (idx==12)print*, 'indexxxxz8', idx
eom_cc3_13_mem_z8(p)= eom_cc3_13_mem_z8(p) + vovo(b, j, d, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (j >= n0i .and. j <= n1i) then 
if (d >= n0a .and. d <= n1a) then 
!dj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z8)
do p = 1, ntrial 

idx = ai_mem(d, j)
!if (idx==12)print*, 'indexxxxz8', idx
eom_cc3_13_mem_z8(p)= eom_cc3_13_mem_z8(p) - vovo(b, k, b, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!bj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z8)
do p = 1, ntrial 

idx = ai_mem(b, j)
!if (idx==12)print*, 'indexxxxz8', idx
eom_cc3_13_mem_z8(p)= eom_cc3_13_mem_z8(p) + vovo(b, k, d, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 


    end subroutine sub_eom_cc3_13_mem_z8        
        subroutine sub_eom_cc3_13_mem_z9(eom_cc3_13_mem_z9, b, j, c, k, vdav, ntrial, n0a,n1a,n0i,n1i) 
        real(F64), dimension(:), intent(out) :: eom_cc3_13_mem_z9
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i
      
            integer, intent(in) :: ntrial, b, j, c, k
            integer :: p, idx 
            eom_cc3_13_mem_z9 = ZERO
    if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
!ck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z9)
do p = 1, ntrial 

idx = ai_mem(c, k)
!if(idx==12)print*, '1indexxxxz9', idx
eom_cc3_13_mem_z9(p)= eom_cc3_13_mem_z9(p) + vovo(b, j, c, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!cj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z9)
do p = 1, ntrial 

idx = ai_mem(c, j)
!if(idx==12)print*, '2indexxxxz9', idx
eom_cc3_13_mem_z9(p)= eom_cc3_13_mem_z9(p) -2.0_F64 * vovo(b, k, c, j) * vdav(p, idx)

idx = ai_mem(c, j)
!if(idx==12)print*, '3indexxxxz9', idx
eom_cc3_13_mem_z9(p)= eom_cc3_13_mem_z9(p) + vovo(b, j, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!bj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z9)
do p = 1, ntrial 

idx = ai_mem(b, j)
!if(idx==12)print*, '4indexxxxz9', idx
eom_cc3_13_mem_z9(p)= eom_cc3_13_mem_z9(p) + vovo(c, j, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
!bk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z9)
do p = 1, ntrial 

idx = ai_mem(b, k)
!if(idx==12)print*, '5indexxxxz9', idx, - vovo(c, j, c, j) , vdav(p, idx), - vovo(c, j, c, j) * vdav(p, idx)
eom_cc3_13_mem_z9(p)= eom_cc3_13_mem_z9(p) - vovo(c, j, c, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 


    end subroutine sub_eom_cc3_13_mem_z9        
        subroutine sub_eom_cc3_13_mem_z10(eom_cc3_13_mem_z10, b, j, c, k, vdav, ntrial, n0a,n1a,n0i,n1i) 
        real(F64), dimension(:), intent(out) :: eom_cc3_13_mem_z10
        real(F64), dimension(:, :), intent(in) :: vdav
        integer, intent(in) :: n0a,n1a,n0i,n1i
      
            integer, intent(in) :: ntrial, b, j, c, k
            integer :: p, idx 
            eom_cc3_13_mem_z10 = ZERO
    if (c >= n0a .and. c <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!cj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z10)
do p = 1, ntrial 

idx = ai_mem(c, j)
!if(idx==12)print*, 'indexxxxxz10', idx
eom_cc3_13_mem_z10(p)= eom_cc3_13_mem_z10(p) -2.0_F64 * vovo(b, k, c, j) * vdav(p, idx)

idx = ai_mem(c, j)
!if(idx==12)print*, 'indexxxxxz10', idx
eom_cc3_13_mem_z10(p)= eom_cc3_13_mem_z10(p) + vovo(b, j, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (c >= n0a .and. c <= n1a) then 
if (k >= n0i .and. k <= n1i) then 
!ck 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z10)
do p = 1, ntrial 

idx = ai_mem(c, k)
!if(idx==12)print*, 'indexxxxxz10', idx
eom_cc3_13_mem_z10(p)= eom_cc3_13_mem_z10(p) + vovo(b, j, c, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (k >= n0i .and. k <= n1i) then 
if (b >= n0a .and. b <= n1a) then 
!bk 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z10)
do p = 1, ntrial 

idx = ai_mem(b, k)
!if(idx==12)print*, 'indexxxxxz10', idx
eom_cc3_13_mem_z10(p)= eom_cc3_13_mem_z10(p) - vovo(c, j, c, j) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 

if (b >= n0a .and. b <= n1a) then 
if (j >= n0i .and. j <= n1i) then 
!bj 
!!$omp parallel private(p, idx)& 
!!$omp default(shared)
!!$omp do &
!!$omp reduction (+:eom_cc3_13_mem_z10)
do p = 1, ntrial 

idx = ai_mem(b, j)
!if(idx==12)print*, 'indexxxxxz10', idx
eom_cc3_13_mem_z10(p)= eom_cc3_13_mem_z10(p) + vovo(c, j, c, k) * vdav(p, idx)
end do 

!!$omp end do
!!$omp end parallel 
end if 
end if 


    end subroutine sub_eom_cc3_13_mem_z10
    end module eom_cc3_13_mem
    
