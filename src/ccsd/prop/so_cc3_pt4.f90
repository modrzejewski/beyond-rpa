module so_cc3_pt4
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    !
    ! File generated automatically on 2018-04-18 11:46:20
    !

        real(F64), dimension(:, :), allocatable :: wm_interm_0_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_1_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_3_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_6_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_12_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_14_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_21_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_22_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_25_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_29_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_30_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_34_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_36_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_37_so_pt4 

    contains
    
    subroutine wm_so_intermediates_cc3_init_pt4(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_1_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_5_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_6_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_7_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_9_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_12_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_13_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_14_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_15_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_16_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_19_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_20_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_23_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_24_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_25_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_26_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_29_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_30_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_31_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_33_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_34_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_35_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_36_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_37_so_pt4(nocc+1: nactive, 1: nocc))
wm_interm_0_so_pt4 = zero 
wm_interm_1_so_pt4 = zero 
wm_interm_2_so_pt4 = zero 
wm_interm_3_so_pt4 = zero 
wm_interm_4_so_pt4 = zero 
wm_interm_5_so_pt4 = zero 
wm_interm_6_so_pt4 = zero 
wm_interm_7_so_pt4 = zero 
wm_interm_8_so_pt4 = zero 
wm_interm_9_so_pt4 = zero 
wm_interm_10_so_pt4 = zero 
wm_interm_11_so_pt4 = zero 
wm_interm_12_so_pt4 = zero 
wm_interm_13_so_pt4 = zero 
wm_interm_14_so_pt4 = zero 
wm_interm_15_so_pt4 = zero 
wm_interm_16_so_pt4 = zero 
wm_interm_17_so_pt4 = zero 
wm_interm_18_so_pt4 = zero 
wm_interm_19_so_pt4 = zero 
wm_interm_20_so_pt4 = zero 
wm_interm_21_so_pt4 = zero 
wm_interm_22_so_pt4 = zero 
wm_interm_23_so_pt4 = zero 
wm_interm_24_so_pt4 = zero 
wm_interm_25_so_pt4 = zero 
wm_interm_26_so_pt4 = zero 
wm_interm_27_so_pt4 = zero 
wm_interm_28_so_pt4 = zero 
wm_interm_29_so_pt4 = zero 
wm_interm_30_so_pt4 = zero 
wm_interm_31_so_pt4 = zero 
wm_interm_32_so_pt4 = zero 
wm_interm_33_so_pt4 = zero 
wm_interm_34_so_pt4 = zero 
wm_interm_35_so_pt4 = zero 
wm_interm_36_so_pt4 = zero 
wm_interm_37_so_pt4 = zero 

    end subroutine wm_so_intermediates_cc3_init_pt4
    
    subroutine wm_so_intermediates_cc3_free_pt4
    deallocate(wm_interm_0_so_pt4)
deallocate(wm_interm_1_so_pt4)
deallocate(wm_interm_2_so_pt4)
deallocate(wm_interm_3_so_pt4)
deallocate(wm_interm_4_so_pt4)
deallocate(wm_interm_5_so_pt4)
deallocate(wm_interm_6_so_pt4)
deallocate(wm_interm_7_so_pt4)
deallocate(wm_interm_8_so_pt4)
deallocate(wm_interm_9_so_pt4)
deallocate(wm_interm_10_so_pt4)
deallocate(wm_interm_11_so_pt4)
deallocate(wm_interm_12_so_pt4)
deallocate(wm_interm_13_so_pt4)
deallocate(wm_interm_14_so_pt4)
deallocate(wm_interm_15_so_pt4)
deallocate(wm_interm_16_so_pt4)
deallocate(wm_interm_17_so_pt4)
deallocate(wm_interm_18_so_pt4)
deallocate(wm_interm_19_so_pt4)
deallocate(wm_interm_20_so_pt4)
deallocate(wm_interm_21_so_pt4)
deallocate(wm_interm_22_so_pt4)
deallocate(wm_interm_23_so_pt4)
deallocate(wm_interm_24_so_pt4)
deallocate(wm_interm_25_so_pt4)
deallocate(wm_interm_26_so_pt4)
deallocate(wm_interm_27_so_pt4)
deallocate(wm_interm_28_so_pt4)
deallocate(wm_interm_29_so_pt4)
deallocate(wm_interm_30_so_pt4)
deallocate(wm_interm_31_so_pt4)
deallocate(wm_interm_32_so_pt4)
deallocate(wm_interm_33_so_pt4)
deallocate(wm_interm_34_so_pt4)
deallocate(wm_interm_35_so_pt4)
deallocate(wm_interm_36_so_pt4)
deallocate(wm_interm_37_so_pt4)

    end subroutine wm_so_intermediates_cc3_free_pt4
    
    subroutine wm_so_intermediates_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: i, a, b, j, c, k, l 

    !$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * t1(b,i)
end do 
wm_interm_0_so_pt4(a, b) = wm_interm_0_so_pt4(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
end do 
end do 
end do 
end do 
wm_interm_1_so_pt4(c, k) = wm_interm_1_so_pt4(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_2_so_pt4(c, k) = wm_interm_2_so_pt4(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,k,i)
end do 
end do 
end do 
end do 
wm_interm_3_so_pt4(c, k) = wm_interm_3_so_pt4(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,k)
end do 
wm_interm_4_so_pt4(b, i, j, k) = wm_interm_4_so_pt4(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,k)
end do 
wm_interm_5_so_pt4(b, i, j, k) = wm_interm_5_so_pt4(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t1(a,j)
end do 
wm_interm_6_so_pt4(i, j) = wm_interm_6_so_pt4(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_7_so_pt4(c, j, k, l) = wm_interm_7_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_8_so_pt4(c, j, k, l) = wm_interm_8_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_9_so_pt4(c, j, k, l) = wm_interm_9_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_10_so_pt4(c, j, k, l) = wm_interm_10_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_11_so_pt4(c, j, k, l) = wm_interm_11_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
end do 
end do 
end do 
end do 
wm_interm_12_so_pt4(c, k) = wm_interm_12_so_pt4(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_13_so_pt4(c, k) = wm_interm_13_so_pt4(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,k,i)
end do 
end do 
end do 
end do 
wm_interm_14_so_pt4(c, k) = wm_interm_14_so_pt4(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,k)
end do 
wm_interm_15_so_pt4(b, i, j, k) = wm_interm_15_so_pt4(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_16_so_pt4(c, j, k, l) = wm_interm_16_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_17_so_pt4(c, j, k, l) = wm_interm_17_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_18_so_pt4(c, j, k, l) = wm_interm_18_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_19_so_pt4(c, j, k, l) = wm_interm_19_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_20_so_pt4(c, j, k, l) = wm_interm_20_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_21_so_pt4(c, k) = wm_interm_21_so_pt4(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rr, a,i) * s1(b,i)
end do 
wm_interm_22_so_pt4(a, b) = wm_interm_22_so_pt4(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, l, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,l,k,i)
end do 
end do 
end do 
wm_interm_23_so_pt4(c, j, l, k) = wm_interm_23_so_pt4(c, j, l, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
sum = sum + r1(vrdav_Rr, a,i) * s1(b,j)
wm_interm_24_so_pt4(a, b, i, j) = wm_interm_24_so_pt4(a, b, i, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_25_so_pt4(c, k) = wm_interm_25_so_pt4(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_26_so_pt4(c, j, k, l) = wm_interm_26_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_27_so_pt4(c, j, k, l) = wm_interm_27_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_28_so_pt4(c, j, k, l) = wm_interm_28_so_pt4(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 
wm_interm_29_so_pt4(c, k) = wm_interm_29_so_pt4(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 
wm_interm_30_so_pt4(c, k) = wm_interm_30_so_pt4(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
end do 
wm_interm_31_so_pt4(b, j, i, k) = wm_interm_31_so_pt4(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, k, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
wm_interm_32_so_pt4(b, c, k, j) = wm_interm_32_so_pt4(b, c, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
wm_interm_33_so_pt4(b, c, j, k) = wm_interm_33_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_34_so_pt4(b, i, j, k) = wm_interm_34_so_pt4(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
wm_interm_35_so_pt4(b, c, j, k) = wm_interm_35_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_36_so_pt4(i, j) = wm_interm_36_so_pt4(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_37_so_pt4(b, j) = wm_interm_37_so_pt4(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_so_intermediates_cc3_pt4
    
    function calc_D_oo_wm_so_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_cc3_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_oo_wm_so_cc3_pt4 = zero
    do s = 0, 0
    calc_D_oo_wm_so_cc3_pt4 = calc_D_oo_wm_so_cc3_pt4 + term(s)
    end do

    end function calc_D_oo_wm_so_cc3_pt4
    
    function calc_D_ov_wm_so_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_cc3_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, j, b, k 
    real(F64), dimension(0:33) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_so_pt4(q,a) * wm_interm_1_so_pt4(a,p)
term(1) = term(1) + wm_interm_0_so_pt4(q,a) * wm_interm_2_so_pt4(a,p)
term(2) = term(2) + wm_interm_0_so_pt4(q,a) * wm_interm_3_so_pt4(a,p)
term(3) = term(3) + wm_interm_0_so_pt4(q,a) * wm_interm_12_so_pt4(a,p)
term(4) = term(4) + wm_interm_0_so_pt4(q,a) * wm_interm_13_so_pt4(a,p)
term(5) = term(5) + wm_interm_0_so_pt4(q,a) * wm_interm_14_so_pt4(a,p)
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(6) = term(6) + t3(nocc, nactive, a,b,q,j,p,i) * t1(b,k) * wm_interm_4_so_pt4(a,k,j,i)
term(7) = term(7) + t3(nocc, nactive, a,b,q,j,p,i) * t1(b,k) * wm_interm_4_so_pt4(a,k,i,j)
term(8) = term(8) + t3(nocc, nactive, a,b,q,j,p,i) * t1(b,k) * wm_interm_5_so_pt4(a,k,i,j)
term(9) = term(9) + t3(nocc, nactive, a,b,q,j,p,i) * t1(b,k) * wm_interm_15_so_pt4(a,k,j,i)
term(10) = term(10) + t3(nocc, nactive, a,b,q,j,p,i) * t1(b,k) * wm_interm_15_so_pt4(a,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (-3.9999999999999996d+0) 
term(10) = term(10) * (4.0d+0) 

do i = 1, nocc 
term(11) = term(11) + wm_interm_1_so_pt4(q,i) * wm_interm_6_so_pt4(p,i)
term(12) = term(12) + wm_interm_2_so_pt4(q,i) * wm_interm_6_so_pt4(p,i)
term(13) = term(13) + wm_interm_3_so_pt4(q,i) * wm_interm_6_so_pt4(p,i)
term(14) = term(14) + wm_interm_12_so_pt4(q,i) * wm_interm_6_so_pt4(p,i)
term(15) = term(15) + wm_interm_13_so_pt4(q,i) * wm_interm_6_so_pt4(p,i)
term(16) = term(16) + wm_interm_14_so_pt4(q,i) * wm_interm_6_so_pt4(p,i)
end do 

term(11) = term(11) * (-1.0d+0) 
term(12) = term(12) * (2.0d+0) 
term(13) = term(13) * (-1.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(16) = term(16) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(17) = term(17) + wm_interm_6_so_pt4(i,j) * wm_interm_7_so_pt4(q,i,p,j)
term(18) = term(18) + wm_interm_6_so_pt4(i,j) * wm_interm_8_so_pt4(q,i,p,j)
term(19) = term(19) + wm_interm_6_so_pt4(i,j) * wm_interm_9_so_pt4(q,i,p,j)
term(20) = term(20) + wm_interm_10_so_pt4(q,i,j,p) * wm_interm_6_so_pt4(i,j)
term(21) = term(21) + wm_interm_11_so_pt4(q,i,p,j) * wm_interm_6_so_pt4(i,j)
term(22) = term(22) + wm_interm_10_so_pt4(q,i,p,j) * wm_interm_6_so_pt4(i,j)
term(23) = term(23) + wm_interm_16_so_pt4(q,i,p,j) * wm_interm_6_so_pt4(i,j)
term(24) = term(24) + wm_interm_17_so_pt4(q,i,p,j) * wm_interm_6_so_pt4(i,j)
term(25) = term(25) + wm_interm_18_so_pt4(q,i,p,j) * wm_interm_6_so_pt4(i,j)
term(26) = term(26) + wm_interm_19_so_pt4(q,i,j,p) * wm_interm_6_so_pt4(i,j)
term(27) = term(27) + wm_interm_20_so_pt4(q,i,p,j) * wm_interm_6_so_pt4(i,j)
term(28) = term(28) + wm_interm_19_so_pt4(q,i,p,j) * wm_interm_6_so_pt4(i,j)
end do 
end do 

term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (-1.0d+0) 
term(20) = term(20) * (-1.0d+0) 
term(21) = term(21) * (-1.0d+0) 
term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (4.0d+0) 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(29) = term(29) + t3(nocc, nactive, a,b,q,p,j,i) * t1(b,k) * wm_interm_4_so_pt4(a,k,i,j)
term(30) = term(30) + t3(nocc, nactive, a,b,q,p,j,i) * t1(b,k) * wm_interm_5_so_pt4(a,k,j,i)
term(31) = term(31) + t3(nocc, nactive, a,b,q,p,j,i) * t1(b,k) * wm_interm_5_so_pt4(a,k,i,j)
term(32) = term(32) + t3(nocc, nactive, a,b,q,p,j,i) * t1(b,k) * wm_interm_15_so_pt4(a,k,i,j)
term(33) = term(33) + t3(nocc, nactive, a,b,q,p,j,i) * t1(b,k) * wm_interm_15_so_pt4(a,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(29) = term(29) * (-1.0d+0) 
term(30) = term(30) * (-1.0d+0) 
term(31) = term(31) * (2.0d+0) 
term(32) = term(32) * (-3.9999999999999996d+0) 
term(33) = term(33) * (4.0d+0) 


    calc_D_ov_wm_so_cc3_pt4 = zero
    do s = 0, 33
    calc_D_ov_wm_so_cc3_pt4 = calc_D_ov_wm_so_cc3_pt4 + term(s)
    end do

    end function calc_D_ov_wm_so_cc3_pt4
    
    function calc_D_vo_wm_so_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_cc3_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b, k 
    real(F64), dimension(0:40) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_23_so_pt4(a,i,j,q) * wm_interm_24_so_pt4(p,a,i,j)
term(1) = term(1) + wm_interm_23_so_pt4(a,i,q,j) * wm_interm_24_so_pt4(p,a,i,j)
term(2) = term(2) + wm_interm_24_so_pt4(p,a,i,j) * wm_interm_26_so_pt4(a,i,j,q)
term(3) = term(3) + wm_interm_24_so_pt4(p,a,i,j) * wm_interm_27_so_pt4(a,i,q,j)
term(4) = term(4) + wm_interm_24_so_pt4(p,a,i,j) * wm_interm_28_so_pt4(a,i,q,j)
term(5) = term(5) + wm_interm_24_so_pt4(p,a,i,j) * wm_interm_26_so_pt4(a,i,q,j)
term(6) = term(6) + wm_interm_24_so_pt4(p,a,i,j) * wm_interm_27_so_pt4(a,i,j,q)
term(7) = term(7) + wm_interm_24_so_pt4(p,a,i,j) * wm_interm_28_so_pt4(a,i,j,q)
term(8) = term(8) + wm_interm_31_so_pt4(a,i,j,q) * wm_interm_33_so_pt4(a,p,i,j)
term(9) = term(9) + wm_interm_33_so_pt4(a,p,i,j) * wm_interm_34_so_pt4(a,i,j,q)
term(10) = term(10) + wm_interm_32_so_pt4(a,p,i,j) * wm_interm_34_so_pt4(a,j,i,q)
term(11) = term(11) + wm_interm_31_so_pt4(a,i,j,q) * wm_interm_32_so_pt4(a,p,i,j)
term(12) = term(12) + wm_interm_31_so_pt4(a,i,j,q) * wm_interm_35_so_pt4(a,p,i,j)
term(13) = term(13) + wm_interm_32_so_pt4(a,p,i,j) * wm_interm_34_so_pt4(a,i,j,q)
term(14) = term(14) + wm_interm_34_so_pt4(a,i,j,q) * wm_interm_35_so_pt4(a,p,i,j)
term(15) = term(15) + wm_interm_33_so_pt4(a,p,i,j) * wm_interm_34_so_pt4(a,j,i,q)
end do 
end do 
end do 

term(0) = term(0) * (1.9999999999999998d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 
term(3) = term(3) * (3.9999999999999996d+0) 
term(4) = term(4) * (-7.999999999999999d+0) 
term(5) = term(5) * (1.9999999999999998d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (4.0d+0) 
term(8) = term(8) * (1.9999999999999998d+0) 
term(9) = term(9) * (-0.9999999999999999d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-3.9999999999999996d+0) 
term(13) = term(13) * (-1.0d+0) 
term(14) = term(14) * (1.9999999999999998d+0) 
term(15) = term(15) * (1.9999999999999998d+0) 

do i = 1, nocc 
term(16) = term(16) + wm_interm_21_so_pt4(p,i) * wm_interm_36_so_pt4(q,i)
term(17) = term(17) + wm_interm_25_so_pt4(p,i) * wm_interm_36_so_pt4(q,i)
term(18) = term(18) + wm_interm_29_so_pt4(p,i) * wm_interm_36_so_pt4(q,i)
term(19) = term(19) + wm_interm_30_so_pt4(p,i) * wm_interm_36_so_pt4(q,i)
end do 

term(16) = term(16) * (-1.9999999999999998d+0) 
term(17) = term(17) * (3.9999999999999996d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_31_so_pt4(a,k,i,j)
term(21) = term(21) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_34_so_pt4(a,i,k,j)
term(22) = term(22) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_31_so_pt4(b,i,k,j)
term(23) = term(23) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_31_so_pt4(b,k,i,j)
term(24) = term(24) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_34_so_pt4(b,k,i,j)
term(25) = term(25) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_34_so_pt4(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(20) = term(20) * (-0.9999999999999999d+0) 
term(21) = term(21) * (-0.9999999999999999d+0) 
term(22) = term(22) * (-0.9999999999999999d+0) 
term(23) = term(23) * (1.9999999999999998d+0) 
term(24) = term(24) * (-0.9999999999999999d+0) 
term(25) = term(25) * (1.9999999999999998d+0) 

do a = nocc + 1, nactive 
term(26) = term(26) + wm_interm_21_so_pt4(a,q) * wm_interm_22_so_pt4(p,a)
term(27) = term(27) + wm_interm_22_so_pt4(p,a) * wm_interm_25_so_pt4(a,q)
term(28) = term(28) + wm_interm_22_so_pt4(p,a) * wm_interm_29_so_pt4(a,q)
term(29) = term(29) + wm_interm_22_so_pt4(p,a) * wm_interm_30_so_pt4(a,q)
end do 

term(26) = term(26) * (-1.9999999999999998d+0) 
term(27) = term(27) * (3.9999999999999996d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(30) = term(30) + wm_interm_31_so_pt4(a,i,j,q) * wm_interm_32_so_pt4(a,p,j,i)
term(31) = term(31) + wm_interm_31_so_pt4(a,i,j,q) * wm_interm_33_so_pt4(a,p,j,i)
term(32) = term(32) + wm_interm_31_so_pt4(a,i,j,q) * wm_interm_35_so_pt4(a,p,j,i)
term(33) = term(33) + wm_interm_34_so_pt4(a,i,j,q) * wm_interm_35_so_pt4(a,p,j,i)
end do 
end do 
end do 

term(30) = term(30) * (-1.0d+0) 
term(31) = term(31) * (-0.9999999999999999d+0) 
term(32) = term(32) * (1.9999999999999998d+0) 
term(33) = term(33) * (-3.9999999999999996d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(34) = term(34) + wm_interm_32_so_pt4(a,p,q,i) * wm_interm_37_so_pt4(a,i)
term(35) = term(35) + wm_interm_35_so_pt4(a,p,q,i) * wm_interm_37_so_pt4(a,i)
term(36) = term(36) + wm_interm_33_so_pt4(a,p,q,i) * wm_interm_37_so_pt4(a,i)
end do 
end do 

term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (3.9999999999999996d+0) 
term(36) = term(36) * (-1.9999999999999998d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(37) = term(37) + wm_interm_26_so_pt4(p,i,q,j) * wm_interm_36_so_pt4(i,j)
term(38) = term(38) + wm_interm_23_so_pt4(p,i,j,q) * wm_interm_36_so_pt4(i,j)
term(39) = term(39) + wm_interm_27_so_pt4(p,i,q,j) * wm_interm_36_so_pt4(i,j)
term(40) = term(40) + wm_interm_28_so_pt4(p,i,q,j) * wm_interm_36_so_pt4(i,j)
end do 
end do 

term(37) = term(37) * (-0.9999999999999999d+0) 
term(38) = term(38) * (-0.9999999999999999d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (4.0d+0) 


    calc_D_vo_wm_so_cc3_pt4 = zero
    do s = 0, 40
    calc_D_vo_wm_so_cc3_pt4 = calc_D_vo_wm_so_cc3_pt4 + term(s)
    end do

    end function calc_D_vo_wm_so_cc3_pt4
    
    function calc_D_vv_wm_so_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_cc3_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_vv_wm_so_cc3_pt4 = zero
    do s = 0, 0
    calc_D_vv_wm_so_cc3_pt4 = calc_D_vv_wm_so_cc3_pt4 + term(s)
    end do

    end function calc_D_vv_wm_so_cc3_pt4
    

  end module so_cc3_pt4
