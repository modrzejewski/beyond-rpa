module density_exc_exc_functions_cc3_triplet_pt0123
       use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
    
    implicit none
    !
    ! File generated automatically on 2016-09-15 15:34:11
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_cc3_0_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_1_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_2_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_3_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_4_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_5_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_6_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_7_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_8_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_9_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_10_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_11_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_12_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_13_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_14_triplet_pt1

 real(F64), dimension(:, :), allocatable :: wm_interm_cc3_0_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_1_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_2_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_3_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_4_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_5_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_6_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_7_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_8_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_9_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_10_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_11_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_12_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_13_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_14_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_15_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_16_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_17_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_18_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_19_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_20_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_21_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_22_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_23_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_24_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_25_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_26_triplet_pt2

  real(F64), dimension(:, :), allocatable :: wm_interm_cc3_0_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_1_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_2_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_3_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_4_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_5_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_6_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_7_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_8_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_9_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_10_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_11_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_12_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_13_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_14_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_15_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_16_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_17_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_18_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_19_triplet_pt3 
real(F64) :: wm_interm_cc3_20_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_21_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_22_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_23_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_24_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_25_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_26_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_27_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_28_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_29_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_30_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_31_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_32_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_33_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_34_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_35_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_36_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_37_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_38_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_39_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_40_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_41_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_42_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_43_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_44_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_45_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_46_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_47_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_48_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_49_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_50_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_51_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_52_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_53_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_54_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_55_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_56_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_57_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_58_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_59_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_60_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_61_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_62_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_63_triplet_pt3 

  contains

    subroutine wm_triplet_intermediates_cc3_init_pt0(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive

    end subroutine wm_triplet_intermediates_cc3_init_pt0

    subroutine wm_triplet_intermediates_cc3_free_pt0

    end subroutine wm_triplet_intermediates_cc3_free_pt0

    subroutine wm_triplet_intermediates_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum

    end subroutine wm_triplet_intermediates_cc3_pt0

    

    subroutine wm_triplet_intermediates_cc3_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_cc3_0_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_1_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_2_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_3_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_4_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_5_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_6_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_7_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_8_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_9_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_10_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_11_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_12_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_13_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_14_triplet_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wm_interm_cc3_0_triplet_pt1 = zero 
wm_interm_cc3_1_triplet_pt1 = zero 
wm_interm_cc3_2_triplet_pt1 = zero 
wm_interm_cc3_3_triplet_pt1 = zero 
wm_interm_cc3_4_triplet_pt1 = zero 
wm_interm_cc3_5_triplet_pt1 = zero 
wm_interm_cc3_6_triplet_pt1 = zero 
wm_interm_cc3_7_triplet_pt1 = zero 
wm_interm_cc3_8_triplet_pt1 = zero 
wm_interm_cc3_9_triplet_pt1 = zero 
wm_interm_cc3_10_triplet_pt1 = zero 
wm_interm_cc3_11_triplet_pt1 = zero 
wm_interm_cc3_12_triplet_pt1 = zero 
wm_interm_cc3_13_triplet_pt1 = zero 
wm_interm_cc3_14_triplet_pt1 = zero 

end subroutine wm_triplet_intermediates_cc3_init_pt1
    

 subroutine wm_triplet_intermediates_cc3_free_pt1

    deallocate(wm_interm_cc3_0_triplet_pt1)
deallocate(wm_interm_cc3_1_triplet_pt1)
deallocate(wm_interm_cc3_2_triplet_pt1)
deallocate(wm_interm_cc3_3_triplet_pt1)
deallocate(wm_interm_cc3_4_triplet_pt1)
deallocate(wm_interm_cc3_5_triplet_pt1)
deallocate(wm_interm_cc3_6_triplet_pt1)
deallocate(wm_interm_cc3_7_triplet_pt1)
deallocate(wm_interm_cc3_8_triplet_pt1)
deallocate(wm_interm_cc3_9_triplet_pt1)
deallocate(wm_interm_cc3_10_triplet_pt1)
deallocate(wm_interm_cc3_11_triplet_pt1)
deallocate(wm_interm_cc3_12_triplet_pt1)
deallocate(wm_interm_cc3_13_triplet_pt1)
deallocate(wm_interm_cc3_14_triplet_pt1)

end subroutine wm_triplet_intermediates_cc3_free_pt1
    
    subroutine wm_triplet_intermediates_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, j, c, k 

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
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_0_triplet_pt1(c, k) = wm_interm_cc3_0_triplet_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_1_triplet_pt1(c, k) = wm_interm_cc3_1_triplet_pt1(c, k) + sum 
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
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_2_triplet_pt1(c, k) = wm_interm_cc3_2_triplet_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_3_triplet_pt1(c, k) = wm_interm_cc3_3_triplet_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_4_triplet_pt1(c, k) = wm_interm_cc3_4_triplet_pt1(c, k) + sum 
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
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_5_triplet_pt1(c, k) = wm_interm_cc3_5_triplet_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_6_triplet_pt1(c, k) = wm_interm_cc3_6_triplet_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_7_triplet_pt1(c, k) = wm_interm_cc3_7_triplet_pt1(c, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
wm_interm_cc3_8_triplet_pt1(b, c, j, k) = wm_interm_cc3_8_triplet_pt1(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
wm_interm_cc3_9_triplet_pt1(b, c, j, k) = wm_interm_cc3_9_triplet_pt1(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_10_triplet_pt1(b, c, k, j) = wm_interm_cc3_10_triplet_pt1(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_11_triplet_pt1(b, c, k, j) = wm_interm_cc3_11_triplet_pt1(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_12_triplet_pt1(b, c, j, k) = wm_interm_cc3_12_triplet_pt1(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_13_triplet_pt1(b, c, j, k) = wm_interm_cc3_13_triplet_pt1(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_14_triplet_pt1(b, c, j, k) = wm_interm_cc3_14_triplet_pt1(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_cc3_pt1

 subroutine wm_triplet_intermediates_cc3_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_cc3_0_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_1_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_2_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_3_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_4_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_5_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_6_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_7_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_8_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_9_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_10_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_11_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_12_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_13_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_14_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_15_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_16_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_17_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_18_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_19_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_20_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_21_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_22_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_23_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_24_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_25_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_26_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wm_interm_cc3_0_triplet_pt2 = zero 
wm_interm_cc3_1_triplet_pt2 = zero 
wm_interm_cc3_2_triplet_pt2 = zero 
wm_interm_cc3_3_triplet_pt2 = zero 
wm_interm_cc3_4_triplet_pt2 = zero 
wm_interm_cc3_5_triplet_pt2 = zero 
wm_interm_cc3_6_triplet_pt2 = zero 
wm_interm_cc3_7_triplet_pt2 = zero 
wm_interm_cc3_8_triplet_pt2 = zero 
wm_interm_cc3_9_triplet_pt2 = zero 
wm_interm_cc3_10_triplet_pt2 = zero 
wm_interm_cc3_11_triplet_pt2 = zero 
wm_interm_cc3_12_triplet_pt2 = zero 
wm_interm_cc3_13_triplet_pt2 = zero 
wm_interm_cc3_14_triplet_pt2 = zero 
wm_interm_cc3_15_triplet_pt2 = zero 
wm_interm_cc3_16_triplet_pt2 = zero 
wm_interm_cc3_17_triplet_pt2 = zero 
wm_interm_cc3_18_triplet_pt2 = zero 
wm_interm_cc3_19_triplet_pt2 = zero 
wm_interm_cc3_20_triplet_pt2 = zero 
wm_interm_cc3_21_triplet_pt2 = zero 
wm_interm_cc3_22_triplet_pt2 = zero 
wm_interm_cc3_23_triplet_pt2 = zero 
wm_interm_cc3_24_triplet_pt2 = zero 
wm_interm_cc3_25_triplet_pt2 = zero 
wm_interm_cc3_26_triplet_pt2 = zero 

    end subroutine wm_triplet_intermediates_cc3_init_pt2
    
    subroutine wm_triplet_intermediates_cc3_free_pt2
    deallocate(wm_interm_cc3_0_triplet_pt2)
deallocate(wm_interm_cc3_1_triplet_pt2)
deallocate(wm_interm_cc3_2_triplet_pt2)
deallocate(wm_interm_cc3_3_triplet_pt2)
deallocate(wm_interm_cc3_4_triplet_pt2)
deallocate(wm_interm_cc3_5_triplet_pt2)
deallocate(wm_interm_cc3_6_triplet_pt2)
deallocate(wm_interm_cc3_7_triplet_pt2)
deallocate(wm_interm_cc3_8_triplet_pt2)
deallocate(wm_interm_cc3_9_triplet_pt2)
deallocate(wm_interm_cc3_10_triplet_pt2)
deallocate(wm_interm_cc3_11_triplet_pt2)
deallocate(wm_interm_cc3_12_triplet_pt2)
deallocate(wm_interm_cc3_13_triplet_pt2)
deallocate(wm_interm_cc3_14_triplet_pt2)
deallocate(wm_interm_cc3_15_triplet_pt2)
deallocate(wm_interm_cc3_16_triplet_pt2)
deallocate(wm_interm_cc3_17_triplet_pt2)
deallocate(wm_interm_cc3_18_triplet_pt2)
deallocate(wm_interm_cc3_19_triplet_pt2)
deallocate(wm_interm_cc3_20_triplet_pt2)
deallocate(wm_interm_cc3_21_triplet_pt2)
deallocate(wm_interm_cc3_22_triplet_pt2)
deallocate(wm_interm_cc3_23_triplet_pt2)
deallocate(wm_interm_cc3_24_triplet_pt2)
deallocate(wm_interm_cc3_25_triplet_pt2)
deallocate(wm_interm_cc3_26_triplet_pt2)

    end subroutine wm_triplet_intermediates_cc3_free_pt2
    
    subroutine wm_triplet_intermediates_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, j, c, k 

    !$omp parallel private(a, b, i, j, c, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do c = nocc + 1, nactive 
do k = 1, nocc 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_0_triplet_pt2(c, k) = wm_interm_cc3_0_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_1_triplet_pt2(c, k) = wm_interm_cc3_1_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_2_triplet_pt2(c, k) = wm_interm_cc3_2_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_3_triplet_pt2(c, k) = wm_interm_cc3_3_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_4_triplet_pt2(c, k) = wm_interm_cc3_4_triplet_pt2(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_5_triplet_pt2(c, k) = wm_interm_cc3_5_triplet_pt2(c, k) + sum 
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
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_6_triplet_pt2(c, k) = wm_interm_cc3_6_triplet_pt2(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_7_triplet_pt2(c, k) = wm_interm_cc3_7_triplet_pt2(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_8_triplet_pt2(c, k) = wm_interm_cc3_8_triplet_pt2(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_9_triplet_pt2(c, k) = wm_interm_cc3_9_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_10_triplet_pt2(c, k) = wm_interm_cc3_10_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_11_triplet_pt2(c, k) = wm_interm_cc3_11_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_12_triplet_pt2(c, k) = wm_interm_cc3_12_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_13_triplet_pt2(c, k) = wm_interm_cc3_13_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_14_triplet_pt2(c, k) = wm_interm_cc3_14_triplet_pt2(c, k) + sum 
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
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_15_triplet_pt2(c, k) = wm_interm_cc3_15_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_16_triplet_pt2(c, k) = wm_interm_cc3_16_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_17_triplet_pt2(c, k) = wm_interm_cc3_17_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_18_triplet_pt2(c, k) = wm_interm_cc3_18_triplet_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s1(a,i)
end do 
end do 
wm_interm_cc3_19_triplet_pt2(b, c, j, k) = wm_interm_cc3_19_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s1(a,i)
end do 
end do 
wm_interm_cc3_20_triplet_pt2(b, c, k, j) = wm_interm_cc3_20_triplet_pt2(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s1(a,i)
end do 
end do 
wm_interm_cc3_21_triplet_pt2(b, c, k, j) = wm_interm_cc3_21_triplet_pt2(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t1(a,i)
end do 
end do 
wm_interm_cc3_22_triplet_pt2(b, c, j, k) = wm_interm_cc3_22_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t1(a,i)
end do 
end do 
wm_interm_cc3_23_triplet_pt2(b, c, k, j) = wm_interm_cc3_23_triplet_pt2(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t1(a,i)
end do 
end do 
wm_interm_cc3_24_triplet_pt2(b, c, j, k) = wm_interm_cc3_24_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t1(a,i)
end do 
end do 
wm_interm_cc3_25_triplet_pt2(b, c, k, j) = wm_interm_cc3_25_triplet_pt2(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t1(a,i)
end do 
end do 
wm_interm_cc3_26_triplet_pt2(b, c, j, k) = wm_interm_cc3_26_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_cc3_pt2

    subroutine wm_triplet_intermediates_cc3_init_pt3(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_cc3_0_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_cc3_1_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_2_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_3_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_4_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_5_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_6_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_7_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_cc3_8_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_9_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_10_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_11_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_12_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_13_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_14_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_15_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_16_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_17_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_18_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_cc3_19_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_21_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_22_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_cc3_23_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_24_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_25_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_26_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_27_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_28_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_29_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_30_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_31_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_32_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_33_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_34_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_35_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_36_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_37_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_cc3_38_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_39_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_40_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_41_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_42_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_43_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_44_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_cc3_45_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_46_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_47_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_48_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_49_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_50_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_51_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_52_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_53_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_54_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_55_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_56_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_57_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_58_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_59_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_60_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_61_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_62_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_63_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wm_interm_cc3_0_triplet_pt3 = zero 
wm_interm_cc3_1_triplet_pt3 = zero 
wm_interm_cc3_2_triplet_pt3 = zero 
wm_interm_cc3_3_triplet_pt3 = zero 
wm_interm_cc3_4_triplet_pt3 = zero 
wm_interm_cc3_5_triplet_pt3 = zero 
wm_interm_cc3_6_triplet_pt3 = zero 
wm_interm_cc3_7_triplet_pt3 = zero 
wm_interm_cc3_8_triplet_pt3 = zero 
wm_interm_cc3_9_triplet_pt3 = zero 
wm_interm_cc3_10_triplet_pt3 = zero 
wm_interm_cc3_11_triplet_pt3 = zero 
wm_interm_cc3_12_triplet_pt3 = zero 
wm_interm_cc3_13_triplet_pt3 = zero 
wm_interm_cc3_14_triplet_pt3 = zero 
wm_interm_cc3_15_triplet_pt3 = zero 
wm_interm_cc3_16_triplet_pt3 = zero 
wm_interm_cc3_17_triplet_pt3 = zero 
wm_interm_cc3_18_triplet_pt3 = zero 
wm_interm_cc3_19_triplet_pt3 = zero 
wm_interm_cc3_20_triplet_pt3 = zero 
wm_interm_cc3_21_triplet_pt3 = zero 
wm_interm_cc3_22_triplet_pt3 = zero 
wm_interm_cc3_23_triplet_pt3 = zero 
wm_interm_cc3_24_triplet_pt3 = zero 
wm_interm_cc3_25_triplet_pt3 = zero 
wm_interm_cc3_26_triplet_pt3 = zero 
wm_interm_cc3_27_triplet_pt3 = zero 
wm_interm_cc3_28_triplet_pt3 = zero 
wm_interm_cc3_29_triplet_pt3 = zero 
wm_interm_cc3_30_triplet_pt3 = zero 
wm_interm_cc3_31_triplet_pt3 = zero 
wm_interm_cc3_32_triplet_pt3 = zero 
wm_interm_cc3_33_triplet_pt3 = zero 
wm_interm_cc3_34_triplet_pt3 = zero 
wm_interm_cc3_35_triplet_pt3 = zero 
wm_interm_cc3_36_triplet_pt3 = zero 
wm_interm_cc3_37_triplet_pt3 = zero 
wm_interm_cc3_38_triplet_pt3 = zero 
wm_interm_cc3_39_triplet_pt3 = zero 
wm_interm_cc3_40_triplet_pt3 = zero 
wm_interm_cc3_41_triplet_pt3 = zero 
wm_interm_cc3_42_triplet_pt3 = zero 
wm_interm_cc3_43_triplet_pt3 = zero 
wm_interm_cc3_44_triplet_pt3 = zero 
wm_interm_cc3_45_triplet_pt3 = zero 
wm_interm_cc3_46_triplet_pt3 = zero 
wm_interm_cc3_47_triplet_pt3 = zero 
wm_interm_cc3_48_triplet_pt3 = zero 
wm_interm_cc3_49_triplet_pt3 = zero 
wm_interm_cc3_50_triplet_pt3 = zero 
wm_interm_cc3_51_triplet_pt3 = zero 
wm_interm_cc3_52_triplet_pt3 = zero 
wm_interm_cc3_53_triplet_pt3 = zero 
wm_interm_cc3_54_triplet_pt3 = zero 
wm_interm_cc3_55_triplet_pt3 = zero 
wm_interm_cc3_56_triplet_pt3 = zero 
wm_interm_cc3_57_triplet_pt3 = zero 
wm_interm_cc3_58_triplet_pt3 = zero 
wm_interm_cc3_59_triplet_pt3 = zero 
wm_interm_cc3_60_triplet_pt3 = zero 
wm_interm_cc3_61_triplet_pt3 = zero 
wm_interm_cc3_62_triplet_pt3 = zero 
wm_interm_cc3_63_triplet_pt3 = zero 

    end subroutine wm_triplet_intermediates_cc3_init_pt3
    
    subroutine wm_triplet_intermediates_cc3_free_pt3
    deallocate(wm_interm_cc3_0_triplet_pt3)
deallocate(wm_interm_cc3_1_triplet_pt3)
deallocate(wm_interm_cc3_2_triplet_pt3)
deallocate(wm_interm_cc3_3_triplet_pt3)
deallocate(wm_interm_cc3_4_triplet_pt3)
deallocate(wm_interm_cc3_5_triplet_pt3)
deallocate(wm_interm_cc3_6_triplet_pt3)
deallocate(wm_interm_cc3_7_triplet_pt3)
deallocate(wm_interm_cc3_8_triplet_pt3)
deallocate(wm_interm_cc3_9_triplet_pt3)
deallocate(wm_interm_cc3_10_triplet_pt3)
deallocate(wm_interm_cc3_11_triplet_pt3)
deallocate(wm_interm_cc3_12_triplet_pt3)
deallocate(wm_interm_cc3_13_triplet_pt3)
deallocate(wm_interm_cc3_14_triplet_pt3)
deallocate(wm_interm_cc3_15_triplet_pt3)
deallocate(wm_interm_cc3_16_triplet_pt3)
deallocate(wm_interm_cc3_17_triplet_pt3)
deallocate(wm_interm_cc3_18_triplet_pt3)
deallocate(wm_interm_cc3_19_triplet_pt3)
deallocate(wm_interm_cc3_21_triplet_pt3)
deallocate(wm_interm_cc3_22_triplet_pt3)
deallocate(wm_interm_cc3_23_triplet_pt3)
deallocate(wm_interm_cc3_24_triplet_pt3)
deallocate(wm_interm_cc3_25_triplet_pt3)
deallocate(wm_interm_cc3_26_triplet_pt3)
deallocate(wm_interm_cc3_27_triplet_pt3)
deallocate(wm_interm_cc3_28_triplet_pt3)
deallocate(wm_interm_cc3_29_triplet_pt3)
deallocate(wm_interm_cc3_30_triplet_pt3)
deallocate(wm_interm_cc3_31_triplet_pt3)
deallocate(wm_interm_cc3_32_triplet_pt3)
deallocate(wm_interm_cc3_33_triplet_pt3)
deallocate(wm_interm_cc3_34_triplet_pt3)
deallocate(wm_interm_cc3_35_triplet_pt3)
deallocate(wm_interm_cc3_36_triplet_pt3)
deallocate(wm_interm_cc3_37_triplet_pt3)
deallocate(wm_interm_cc3_38_triplet_pt3)
deallocate(wm_interm_cc3_39_triplet_pt3)
deallocate(wm_interm_cc3_40_triplet_pt3)
deallocate(wm_interm_cc3_41_triplet_pt3)
deallocate(wm_interm_cc3_42_triplet_pt3)
deallocate(wm_interm_cc3_43_triplet_pt3)
deallocate(wm_interm_cc3_44_triplet_pt3)
deallocate(wm_interm_cc3_45_triplet_pt3)
deallocate(wm_interm_cc3_46_triplet_pt3)
deallocate(wm_interm_cc3_47_triplet_pt3)
deallocate(wm_interm_cc3_48_triplet_pt3)
deallocate(wm_interm_cc3_49_triplet_pt3)
deallocate(wm_interm_cc3_50_triplet_pt3)
deallocate(wm_interm_cc3_51_triplet_pt3)
deallocate(wm_interm_cc3_52_triplet_pt3)
deallocate(wm_interm_cc3_53_triplet_pt3)
deallocate(wm_interm_cc3_54_triplet_pt3)
deallocate(wm_interm_cc3_55_triplet_pt3)
deallocate(wm_interm_cc3_56_triplet_pt3)
deallocate(wm_interm_cc3_57_triplet_pt3)
deallocate(wm_interm_cc3_58_triplet_pt3)
deallocate(wm_interm_cc3_59_triplet_pt3)
deallocate(wm_interm_cc3_60_triplet_pt3)
deallocate(wm_interm_cc3_61_triplet_pt3)
deallocate(wm_interm_cc3_62_triplet_pt3)
deallocate(wm_interm_cc3_63_triplet_pt3)

    end subroutine wm_triplet_intermediates_cc3_free_pt3
    
    subroutine wm_triplet_intermediates_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, j, b, c, k, l 

    !$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wm_interm_cc3_0_triplet_pt3(i, j) = wm_interm_cc3_0_triplet_pt3(i, j) + sum 
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
sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_cc3_1_triplet_pt3(c, j, k, l) = wm_interm_cc3_1_triplet_pt3(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_cc3_2_triplet_pt3(c, k, j, l) = wm_interm_cc3_2_triplet_pt3(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_cc3_3_triplet_pt3(c, k, j, l) = wm_interm_cc3_3_triplet_pt3(c, k, j, l) + sum 
end do 
end do 
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
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,i)
end do 
end do 
wm_interm_cc3_4_triplet_pt3(b, j) = wm_interm_cc3_4_triplet_pt3(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,k)
end do 
wm_interm_cc3_5_triplet_pt3(b, i, j, k) = wm_interm_cc3_5_triplet_pt3(b, i, j, k) + sum 
end do 
end do 
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
sum = sum + s2(a,b,j,i) * t1(a,k)
end do 
wm_interm_cc3_6_triplet_pt3(b, j, i, k) = wm_interm_cc3_6_triplet_pt3(b, j, i, k) + sum 
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
wm_interm_cc3_7_triplet_pt3(i, j) = wm_interm_cc3_7_triplet_pt3(i, j) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_cc3_8_triplet_pt3(c, j, k, l) = wm_interm_cc3_8_triplet_pt3(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_cc3_9_triplet_pt3(c, j, k, l) = wm_interm_cc3_9_triplet_pt3(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_cc3_10_triplet_pt3(c, k, j, l) = wm_interm_cc3_10_triplet_pt3(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,i,c,k) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_cc3_11_triplet_pt3(c, j, k, l) = wm_interm_cc3_11_triplet_pt3(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_cc3_12_triplet_pt3(c, k, j, l) = wm_interm_cc3_12_triplet_pt3(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,i,c,k) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_cc3_13_triplet_pt3(c, j, k, l) = wm_interm_cc3_13_triplet_pt3(c, j, k, l) + sum 
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
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_14_triplet_pt3(c, k) = wm_interm_cc3_14_triplet_pt3(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_15_triplet_pt3(c, k) = wm_interm_cc3_15_triplet_pt3(c, k) + sum 
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
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_16_triplet_pt3(c, k) = wm_interm_cc3_16_triplet_pt3(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_17_triplet_pt3(c, k) = wm_interm_cc3_17_triplet_pt3(c, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * t1(b,i)
end do 
wm_interm_cc3_18_triplet_pt3(a, b) = wm_interm_cc3_18_triplet_pt3(a, b) + sum 
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
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_cc3_19_triplet_pt3(c, j, k, l) = wm_interm_cc3_19_triplet_pt3(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_20_triplet_pt3 = wm_interm_cc3_20_triplet_pt3 + sum 
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
sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_21_triplet_pt3(c, k) = wm_interm_cc3_21_triplet_pt3(c, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,i)
end do 
wm_interm_cc3_22_triplet_pt3(a, b) = wm_interm_cc3_22_triplet_pt3(a, b) + sum 
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
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_cc3_23_triplet_pt3(c, j, k, l) = wm_interm_cc3_23_triplet_pt3(c, j, k, l) + sum 
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
sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_cc3_24_triplet_pt3(c, j, k, l) = wm_interm_cc3_24_triplet_pt3(c, j, k, l) + sum 
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
sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_cc3_25_triplet_pt3(c, j, k, l) = wm_interm_cc3_25_triplet_pt3(c, j, k, l) + sum 
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
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_26_triplet_pt3(c, k) = wm_interm_cc3_26_triplet_pt3(c, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,i)
end do 
end do 
wm_interm_cc3_27_triplet_pt3(b, j) = wm_interm_cc3_27_triplet_pt3(b, j) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,k)
end do 
wm_interm_cc3_28_triplet_pt3(b, i, j, k) = wm_interm_cc3_28_triplet_pt3(b, i, j, k) + sum 
end do 
end do 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,l,i,k)
end do 
end do 
end do 
wm_interm_cc3_29_triplet_pt3(c, j, l, k) = wm_interm_cc3_29_triplet_pt3(c, j, l, k) + sum 
end do 
end do 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,l,i,k)
end do 
end do 
end do 
wm_interm_cc3_30_triplet_pt3(c, j, l, k) = wm_interm_cc3_30_triplet_pt3(c, j, l, k) + sum 
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
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_31_triplet_pt3(c, k) = wm_interm_cc3_31_triplet_pt3(c, k) + sum 
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
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_32_triplet_pt3(c, k) = wm_interm_cc3_32_triplet_pt3(c, k) + sum 
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
sum = sum + s1(a,i) * t2(a,b,j,i)
end do 
end do 
wm_interm_cc3_33_triplet_pt3(b, j) = wm_interm_cc3_33_triplet_pt3(b, j) + sum 
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
sum = sum + s1(a,i) * t2(a,b,i,j)
end do 
end do 
wm_interm_cc3_34_triplet_pt3(b, j) = wm_interm_cc3_34_triplet_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, k, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t2(a,b,k,j)
end do 
wm_interm_cc3_35_triplet_pt3(b, i, k, j) = wm_interm_cc3_35_triplet_pt3(b, i, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_cc3_36_triplet_pt3(c, k, j, l) = wm_interm_cc3_36_triplet_pt3(c, k, j, l) + sum 
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
wm_interm_cc3_37_triplet_pt3(i, j) = wm_interm_cc3_37_triplet_pt3(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, c, k, j, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_cc3_38_triplet_pt3(c, k, j, l) = wm_interm_cc3_38_triplet_pt3(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_cc3_39_triplet_pt3(c, j, k, l) = wm_interm_cc3_39_triplet_pt3(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_cc3_40_triplet_pt3(c, j, k, l) = wm_interm_cc3_40_triplet_pt3(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_cc3_41_triplet_pt3(c, j, k, l) = wm_interm_cc3_41_triplet_pt3(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_cc3_42_triplet_pt3(c, j, k, l) = wm_interm_cc3_42_triplet_pt3(c, j, k, l) + sum 
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
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_43_triplet_pt3(c, k) = wm_interm_cc3_43_triplet_pt3(c, k) + sum 
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
wm_interm_cc3_44_triplet_pt3(a, b) = wm_interm_cc3_44_triplet_pt3(a, b) + sum 
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
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_45_triplet_pt3(c, k) = wm_interm_cc3_45_triplet_pt3(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_46_triplet_pt3(c, k) = wm_interm_cc3_46_triplet_pt3(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_47_triplet_pt3(c, k) = wm_interm_cc3_47_triplet_pt3(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s1(a,i)
end do 
end do 
wm_interm_cc3_48_triplet_pt3(b, c, j, k) = wm_interm_cc3_48_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s1(a,i)
end do 
end do 
wm_interm_cc3_49_triplet_pt3(b, c, k, j) = wm_interm_cc3_49_triplet_pt3(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,i,c,k) * s1(a,i)
end do 
end do 
wm_interm_cc3_50_triplet_pt3(b, c, j, k) = wm_interm_cc3_50_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s1(a,i)
end do 
end do 
wm_interm_cc3_51_triplet_pt3(b, c, k, j) = wm_interm_cc3_51_triplet_pt3(b, c, k, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
wm_interm_cc3_52_triplet_pt3(b, c, j, k) = wm_interm_cc3_52_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,i,b,j,c,k)
end do 
end do 
wm_interm_cc3_53_triplet_pt3(b, c, j, k) = wm_interm_cc3_53_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t1(a,i)
end do 
end do 
wm_interm_cc3_54_triplet_pt3(b, c, j, k) = wm_interm_cc3_54_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t1(a,i)
end do 
end do 
wm_interm_cc3_55_triplet_pt3(b, c, k, j) = wm_interm_cc3_55_triplet_pt3(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t1(a,i)
end do 
end do 
wm_interm_cc3_56_triplet_pt3(b, c, j, k) = wm_interm_cc3_56_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t1(a,i)
end do 
end do 
wm_interm_cc3_57_triplet_pt3(b, c, k, j) = wm_interm_cc3_57_triplet_pt3(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t1(a,i)
end do 
end do 
wm_interm_cc3_58_triplet_pt3(b, c, j, k) = wm_interm_cc3_58_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_59_triplet_pt3(b, c, k, j) = wm_interm_cc3_59_triplet_pt3(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_60_triplet_pt3(b, c, k, j) = wm_interm_cc3_60_triplet_pt3(b, c, k, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_61_triplet_pt3(b, c, j, k) = wm_interm_cc3_61_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_62_triplet_pt3(b, c, j, k) = wm_interm_cc3_62_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_63_triplet_pt3(b, c, j, k) = wm_interm_cc3_63_triplet_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_cc3_pt3

    
    function calc_D_oo_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_cc3_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, j 
    real(F64), dimension(0:39) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,b,q,i) * wm_interm_cc3_8_triplet_pt1(a, b, i, p)
term(1) = term(1) + s2(a,b,i,q) * wm_interm_cc3_9_triplet_pt1(a, b, p, i)
term(2) = term(2) + s2(a,b,i,q) * wm_interm_cc3_8_triplet_pt1(a, b, p, i)
term(3) = term(3) + s2(a,b,q,i) * wm_interm_cc3_8_triplet_pt1(a, b, p, i)
term(4) = term(4) + s2(a,b,q,i) * wm_interm_cc3_9_triplet_pt1(a, b, i, p)
term(5) = term(5) + s2(a,b,q,i) * wm_interm_cc3_9_triplet_pt1(a, b, p, i)
term(6) = term(6) + t2(a,b,q,i) * wm_interm_cc3_10_triplet_pt1(a, b, p, i)
term(7) = term(7) + t2(a,b,q,i) * wm_interm_cc3_11_triplet_pt1(a, b, i, p)
term(8) = term(8) + t2(a,b,q,i) * wm_interm_cc3_12_triplet_pt1(a, b, p, i)
term(9) = term(9) + t2(a,b,q,i) * wm_interm_cc3_11_triplet_pt1(a, b, p, i)
term(10) = term(10) + t2(a,b,q,i) * wm_interm_cc3_12_triplet_pt1(a, b, i, p)
term(11) = term(11) + t2(a,b,q,i) * wm_interm_cc3_13_triplet_pt1(a, b, i, p)
term(12) = term(12) + t2(a,b,q,i) * wm_interm_cc3_13_triplet_pt1(a, b, p, i)
term(13) = term(13) + t2(a,b,q,i) * wm_interm_cc3_11_triplet_pt1(b, a, p, i)
term(14) = term(14) + t2(a,b,q,i) * wm_interm_cc3_13_triplet_pt1(b, a, p, i)
term(15) = term(15) + t2(a,b,q,i) * wm_interm_cc3_12_triplet_pt1(b, a, p, i)
term(16) = term(16) + t2(a,b,q,i) * wm_interm_cc3_14_triplet_pt1(a, b, p, i)
term(17) = term(17) + t2(a,b,q,i) * wm_interm_cc3_10_triplet_pt1(b, a, i, p)
term(18) = term(18) + t2(a,b,q,i) * wm_interm_cc3_11_triplet_pt1(b, a, i, p)
term(19) = term(19) + t2(a,b,q,i) * wm_interm_cc3_12_triplet_pt1(b, a, i, p)
term(20) = term(20) + t2(a,b,q,i) * wm_interm_cc3_13_triplet_pt1(b, a, i, p)
term(21) = term(21) + t2(a,b,q,i) * wm_interm_cc3_14_triplet_pt1(b, a, i, p)
end do 
end do 
end do 

term(0) = term(0) * 6.0d+0 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * 6.0d+0 
term(3) = term(3) * (-8.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * 4.0d+0 
term(7) = term(7) * 2.0d+0 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * 2.0d+0 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * 4.0d+0 
term(13) = term(13) * 2.0d+0 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * 2.0d+0 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * 4.0d+0 

do a = nocc + 1, nactive 
term(22) = term(22) + r1(vrdav_Rl, a,q) * wm_interm_cc3_0_triplet_pt1(a, p)
term(23) = term(23) + r1(vrdav_Rl, a,q) * wm_interm_cc3_1_triplet_pt1(a, p)
term(24) = term(24) + r1(vrdav_Rl, a,q) * wm_interm_cc3_2_triplet_pt1(a, p)
term(25) = term(25) + r1(vrdav_Rl, a,q) * wm_interm_cc3_3_triplet_pt1(a, p)
term(26) = term(26) + r1(vrdav_Rr, a,p) * wm_interm_cc3_4_triplet_pt1(a, q)
term(27) = term(27) + r1(vrdav_Rr, a,p) * wm_interm_cc3_5_triplet_pt1(a, q)
term(28) = term(28) + r1(vrdav_Rr, a,p) * wm_interm_cc3_6_triplet_pt1(a, q)
term(29) = term(29) + r1(vrdav_Rr, a,p) * wm_interm_cc3_7_triplet_pt1(a, q)
end do 

term(22) = term(22) * (-6.0d+0) 
term(23) = term(23) * 8.0d+0 
term(24) = term(24) * 2.0d+0 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * 6.0d+0 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (-8.0d+0) 
term(29) = term(29) * 4.0d+0 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(30) = term(30) + s2(a,b,i,q) * wm_interm_cc3_8_triplet_pt1(a, b, i, p)
term(31) = term(31) + s2(a,b,i,q) * wm_interm_cc3_9_triplet_pt1(a, b, i, p)
end do 
end do 
end do 

term(30) = term(30) * (-8.0d+0) 
term(31) = term(31) * 4.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(32) = term(32) + r1(vrdav_Rl, a,i) * wm_interm_cc3_0_triplet_pt1(a, i)
term(33) = term(33) + r1(vrdav_Rl, a,i) * wm_interm_cc3_1_triplet_pt1(a, i)
term(34) = term(34) + r1(vrdav_Rl, a,i) * wm_interm_cc3_2_triplet_pt1(a, i)
term(35) = term(35) + r1(vrdav_Rl, a,i) * wm_interm_cc3_3_triplet_pt1(a, i)
term(36) = term(36) + r1(vrdav_Rr, a,i) * wm_interm_cc3_4_triplet_pt1(a, i)
term(37) = term(37) + r1(vrdav_Rr, a,i) * wm_interm_cc3_5_triplet_pt1(a, i)
term(38) = term(38) + r1(vrdav_Rr, a,i) * wm_interm_cc3_6_triplet_pt1(a, i)
term(39) = term(39) + r1(vrdav_Rr, a,i) * wm_interm_cc3_7_triplet_pt1(a, i)
end do 
end do 

term(32) = term(32) * 12.0d+0 
term(33) = term(33) * (-16.0d+0) 
term(34) = term(34) * (-4.0d+0) 
term(35) = term(35) * 8.0d+0 
term(36) = term(36) * (-12.0d+0) 
term(37) = term(37) * 4.0d+0 
term(38) = term(38) * 16.0d+0 
term(39) = term(39) * (-8.0d+0) 


    calc_D_oo_wm_triplet_cc3_pt1 = zero
    do s = 0, 39
    calc_D_oo_wm_triplet_cc3_pt1 = calc_D_oo_wm_triplet_cc3_pt1 + term(s)
    end do

    end function calc_D_oo_wm_triplet_cc3_pt1
    
    function calc_D_ov_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_cc3_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_ov_wm_triplet_cc3_pt1 = zero
    do s = 0, 1
    calc_D_ov_wm_triplet_cc3_pt1 = calc_D_ov_wm_triplet_cc3_pt1 + term(s)
    end do

    end function calc_D_ov_wm_triplet_cc3_pt1
    
    function calc_D_vo_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_cc3_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_vo_wm_triplet_cc3_pt1 = zero
    do s = 0, 1
    calc_D_vo_wm_triplet_cc3_pt1 = calc_D_vo_wm_triplet_cc3_pt1 + term(s)
    end do

    end function calc_D_vo_wm_triplet_cc3_pt1
    
    function calc_D_vv_wm_triplet_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_cc3_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, b 
    real(F64), dimension(0:31) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_cc3_8_triplet_pt1(p, a, j, i)
term(1) = term(1) + s2(a,q,j,i) * wm_interm_cc3_8_triplet_pt1(a, p, j, i)
term(2) = term(2) + s2(a,q,j,i) * wm_interm_cc3_9_triplet_pt1(a, p, j, i)
term(3) = term(3) + s2(a,q,j,i) * wm_interm_cc3_9_triplet_pt1(p, a, j, i)
term(4) = term(4) + t2(a,q,j,i) * wm_interm_cc3_11_triplet_pt1(p, a, j, i)
term(5) = term(5) + t2(a,q,j,i) * wm_interm_cc3_12_triplet_pt1(p, a, j, i)
term(6) = term(6) + t2(a,q,j,i) * wm_interm_cc3_13_triplet_pt1(p, a, j, i)
term(7) = term(7) + t2(a,q,j,i) * wm_interm_cc3_10_triplet_pt1(a, p, j, i)
term(8) = term(8) + t2(a,q,j,i) * wm_interm_cc3_11_triplet_pt1(a, p, j, i)
term(9) = term(9) + t2(a,q,j,i) * wm_interm_cc3_12_triplet_pt1(a, p, j, i)
term(10) = term(10) + t2(a,q,j,i) * wm_interm_cc3_13_triplet_pt1(a, p, j, i)
term(11) = term(11) + t2(a,q,j,i) * wm_interm_cc3_14_triplet_pt1(a, p, j, i)
end do 
end do 
end do 

term(0) = term(0) * (-6.0d+0) 
term(1) = term(1) * 8.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = term(8) * 4.0d+0 
term(9) = term(9) * 4.0d+0 
term(10) = term(10) * (-4.0d+0) 
term(11) = -term(11) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(12) = term(12) + s2(a,q,j,i) * wm_interm_cc3_9_triplet_pt1(a, p, i, j)
term(13) = term(13) + s2(a,q,j,i) * wm_interm_cc3_8_triplet_pt1(a, p, i, j)
term(14) = term(14) + s2(a,q,j,i) * wm_interm_cc3_8_triplet_pt1(p, a, i, j)
term(15) = term(15) + s2(a,q,j,i) * wm_interm_cc3_9_triplet_pt1(p, a, i, j)
term(16) = term(16) + t2(a,q,j,i) * wm_interm_cc3_10_triplet_pt1(p, a, i, j)
term(17) = term(17) + t2(a,q,j,i) * wm_interm_cc3_12_triplet_pt1(p, a, i, j)
term(18) = term(18) + t2(a,q,j,i) * wm_interm_cc3_11_triplet_pt1(p, a, i, j)
term(19) = term(19) + t2(a,q,j,i) * wm_interm_cc3_13_triplet_pt1(p, a, i, j)
term(20) = term(20) + t2(a,q,j,i) * wm_interm_cc3_11_triplet_pt1(a, p, i, j)
term(21) = term(21) + t2(a,q,j,i) * wm_interm_cc3_13_triplet_pt1(a, p, i, j)
term(22) = term(22) + t2(a,q,j,i) * wm_interm_cc3_12_triplet_pt1(a, p, i, j)
term(23) = term(23) + t2(a,q,j,i) * wm_interm_cc3_14_triplet_pt1(p, a, i, j)
end do 
end do 
end do 

term(12) = term(12) * 2.0d+0 
term(13) = term(13) * (-6.0d+0) 
term(14) = term(14) * 8.0d+0 
term(15) = term(15) * (-4.0d+0) 
term(16) = -term(16) 
term(17) = term(17) * 4.0d+0 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * 2.0d+0 
term(22) = term(22) * (-2.0d+0) 
term(23) = -term(23) 

do i = 1, nocc 
term(24) = term(24) + r1(vrdav_Rl, q,i) * wm_interm_cc3_0_triplet_pt1(p, i)
term(25) = term(25) + r1(vrdav_Rl, q,i) * wm_interm_cc3_1_triplet_pt1(p, i)
term(26) = term(26) + r1(vrdav_Rl, q,i) * wm_interm_cc3_2_triplet_pt1(p, i)
term(27) = term(27) + r1(vrdav_Rl, q,i) * wm_interm_cc3_3_triplet_pt1(p, i)
term(28) = term(28) + r1(vrdav_Rr, p,i) * wm_interm_cc3_4_triplet_pt1(q, i)
term(29) = term(29) + r1(vrdav_Rr, p,i) * wm_interm_cc3_5_triplet_pt1(q, i)
term(30) = term(30) + r1(vrdav_Rr, p,i) * wm_interm_cc3_6_triplet_pt1(q, i)
term(31) = term(31) + r1(vrdav_Rr, p,i) * wm_interm_cc3_7_triplet_pt1(q, i)
end do 

term(24) = term(24) * 6.0d+0 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * 4.0d+0 
term(28) = term(28) * (-6.0d+0) 
term(29) = term(29) * 2.0d+0 
term(30) = term(30) * 8.0d+0 
term(31) = term(31) * (-4.0d+0) 


    calc_D_vv_wm_triplet_cc3_pt1 = zero
    do s = 0, 31
    calc_D_vv_wm_triplet_cc3_pt1 = calc_D_vv_wm_triplet_cc3_pt1 + term(s)
    end do

    end function calc_D_vv_wm_triplet_cc3_pt1
    

    

    
    function calc_D_oo_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_cc3_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, j 
    real(F64), dimension(0:85) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_cc3_19_triplet_pt2(a, b, i, p)
term(1) = term(1) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_cc3_19_triplet_pt2(a, b, p, i)
term(2) = term(2) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_cc3_20_triplet_pt2(a, b, i, p)
term(3) = term(3) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_cc3_21_triplet_pt2(a, b, p, i)
term(4) = term(4) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_cc3_21_triplet_pt2(a, b, i, p)
term(5) = term(5) + r2p(vrdav_Rl, a,q,b,i) * wm_interm_cc3_20_triplet_pt2(a, b, p, i)
term(6) = term(6) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_cc3_19_triplet_pt2(a, b, i, p)
term(7) = term(7) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_cc3_19_triplet_pt2(a, b, p, i)
term(8) = term(8) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_cc3_19_triplet_pt2(a, b, p, i)
term(9) = term(9) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_cc3_20_triplet_pt2(a, b, i, p)
term(10) = term(10) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_cc3_21_triplet_pt2(a, b, p, i)
term(11) = term(11) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_cc3_21_triplet_pt2(a, b, i, p)
term(12) = term(12) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_cc3_20_triplet_pt2(a, b, p, i)
term(13) = term(13) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_cc3_21_triplet_pt2(a, b, p, i)
term(14) = term(14) + r2m(vrdav_Rl, a,q,b,i) * wm_interm_cc3_20_triplet_pt2(a, b, p, i)
term(15) = term(15) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_cc3_22_triplet_pt2(a, b, q, i)
term(16) = term(16) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_cc3_22_triplet_pt2(a, b, i, q)
term(17) = term(17) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_cc3_23_triplet_pt2(a, b, q, i)
term(18) = term(18) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_cc3_23_triplet_pt2(a, b, i, q)
term(19) = term(19) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_cc3_24_triplet_pt2(a, b, q, i)
term(20) = term(20) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_cc3_25_triplet_pt2(a, b, q, i)
term(21) = term(21) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_cc3_24_triplet_pt2(a, b, i, q)
term(22) = term(22) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_cc3_25_triplet_pt2(a, b, i, q)
term(23) = term(23) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_cc3_26_triplet_pt2(a, b, i, q)
term(24) = term(24) + r2p(vrdav_Rr, a,p,b,i) * wm_interm_cc3_26_triplet_pt2(a, b, q, i)
term(25) = term(25) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_cc3_22_triplet_pt2(a, b, q, i)
term(26) = term(26) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_cc3_22_triplet_pt2(a, b, q, i)
term(27) = term(27) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_cc3_22_triplet_pt2(a, b, i, q)
term(28) = term(28) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_cc3_23_triplet_pt2(a, b, q, i)
term(29) = term(29) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_cc3_23_triplet_pt2(a, b, q, i)
term(30) = term(30) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_cc3_23_triplet_pt2(a, b, i, q)
term(31) = term(31) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_cc3_24_triplet_pt2(a, b, q, i)
term(32) = term(32) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_cc3_24_triplet_pt2(a, b, q, i)
term(33) = term(33) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_cc3_25_triplet_pt2(a, b, q, i)
term(34) = term(34) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_cc3_24_triplet_pt2(a, b, i, q)
term(35) = term(35) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_cc3_25_triplet_pt2(a, b, q, i)
term(36) = term(36) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_cc3_25_triplet_pt2(a, b, i, q)
term(37) = term(37) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_cc3_26_triplet_pt2(a, b, i, q)
term(38) = term(38) + r2m(vrdav_Rr, a,p,b,i) * wm_interm_cc3_26_triplet_pt2(a, b, q, i)
term(39) = term(39) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_cc3_26_triplet_pt2(a, b, q, i)
end do 
end do 
end do 

term(0) = term(0) * 8.0d+0 
term(1) = term(1) * (-8.0d+0) 
term(2) = term(2) * (-5.0d+0) 
term(3) = term(3) * (-7.0d+0) 
term(4) = term(4) * 7.0d+0 
term(5) = term(5) * 5.0d+0 
term(6) = term(6) * 8.0d+0 
term(7) = term(7) * (-8.0d+0) 
term(8) = term(8) * 8.0d+0 
term(9) = term(9) * (-6.0d+0) 
term(10) = term(10) * (-6.0d+0) 
term(11) = term(11) * 8.0d+0 
term(12) = term(12) * (-6.0d+0) 
term(13) = term(13) * 8.0d+0 
term(14) = term(14) * 4.0d+0 
term(15) = term(15) * 8.0d+0 
term(16) = term(16) * (-8.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * 2.0d+0 
term(21) = term(21) * 4.0d+0 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * 2.0d+0 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * 8.0d+0 
term(27) = term(27) * (-8.0d+0) 
term(28) = term(28) * 4.0d+0 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * 4.0d+0 
term(31) = term(31) * 4.0d+0 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * 2.0d+0 
term(34) = term(34) * 4.0d+0 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * 2.0d+0 
term(39) = term(39) * (-2.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(40) = term(40) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_cc3_19_triplet_pt2(a, b, i, p)
term(41) = term(41) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_cc3_21_triplet_pt2(a, b, i, p)
term(42) = term(42) + r2m(vrdav_Rl, a,i,b,q) * wm_interm_cc3_20_triplet_pt2(a, b, i, p)
term(43) = term(43) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_cc3_22_triplet_pt2(a, b, i, q)
term(44) = term(44) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_cc3_23_triplet_pt2(a, b, i, q)
term(45) = term(45) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_cc3_24_triplet_pt2(a, b, i, q)
term(46) = term(46) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_cc3_25_triplet_pt2(a, b, i, q)
term(47) = term(47) + r2m(vrdav_Rr, a,i,b,p) * wm_interm_cc3_26_triplet_pt2(a, b, i, q)
end do 
end do 
end do 

term(40) = term(40) * (-8.0d+0) 
term(41) = term(41) * (-6.0d+0) 
term(42) = term(42) * 4.0d+0 
term(43) = term(43) * 8.0d+0 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * 2.0d+0 
term(47) = term(47) * 2.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(48) = term(48) + s1(a,i) * wm_interm_cc3_0_triplet_pt2(a, i)
term(49) = term(49) + s1(a,i) * wm_interm_cc3_1_triplet_pt2(a, i)
term(50) = term(50) + s1(a,i) * wm_interm_cc3_2_triplet_pt2(a, i)
term(51) = term(51) + s1(a,i) * wm_interm_cc3_3_triplet_pt2(a, i)
term(52) = term(52) + s1(a,i) * wm_interm_cc3_4_triplet_pt2(a, i)
term(53) = term(53) + s1(a,i) * wm_interm_cc3_5_triplet_pt2(a, i)
term(54) = term(54) + s1(a,i) * wm_interm_cc3_6_triplet_pt2(a, i)
term(55) = term(55) + s1(a,i) * wm_interm_cc3_7_triplet_pt2(a, i)
term(56) = term(56) + s1(a,i) * wm_interm_cc3_8_triplet_pt2(a, i)
term(57) = term(57) + s1(a,i) * wm_interm_cc3_9_triplet_pt2(a, i)
term(58) = term(58) + t1(a,i) * wm_interm_cc3_10_triplet_pt2(a, i)
term(59) = term(59) + t1(a,i) * wm_interm_cc3_11_triplet_pt2(a, i)
term(60) = term(60) + t1(a,i) * wm_interm_cc3_12_triplet_pt2(a, i)
term(61) = term(61) + t1(a,i) * wm_interm_cc3_13_triplet_pt2(a, i)
term(62) = term(62) + t1(a,i) * wm_interm_cc3_14_triplet_pt2(a, i)
term(63) = term(63) + t1(a,i) * wm_interm_cc3_15_triplet_pt2(a, i)
term(64) = term(64) + t1(a,i) * wm_interm_cc3_16_triplet_pt2(a, i)
term(65) = term(65) + t1(a,i) * wm_interm_cc3_17_triplet_pt2(a, i)
term(66) = term(66) + t1(a,i) * wm_interm_cc3_18_triplet_pt2(a, i)
end do 
end do 

term(48) = term(48) * (-8.0d+0) 
term(49) = term(49) * 8.0d+0 
term(50) = term(50) * 6.0d+0 
term(51) = term(51) * 6.0d+0 
term(52) = term(52) * (-8.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (-16.0d+0) 
term(55) = term(55) * 16.0d+0 
term(56) = term(56) * 24.0d+0 
term(57) = term(57) * (-24.0d+0) 
term(58) = term(58) * (-16.0d+0) 
term(59) = term(59) * 8.0d+0 
term(60) = term(60) * 8.0d+0 
term(61) = term(61) * (-4.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * 16.0d+0 
term(64) = term(64) * (-16.0d+0) 
term(65) = term(65) * (-24.0d+0) 
term(66) = term(66) * 24.0d+0 

do a = nocc + 1, nactive 
term(67) = term(67) + s1(a,q) * wm_interm_cc3_2_triplet_pt2(a, p)
term(68) = term(68) + s1(a,q) * wm_interm_cc3_4_triplet_pt2(a, p)
term(69) = term(69) + s1(a,q) * wm_interm_cc3_3_triplet_pt2(a, p)
term(70) = term(70) + s1(a,q) * wm_interm_cc3_5_triplet_pt2(a, p)
term(71) = term(71) + s1(a,q) * wm_interm_cc3_0_triplet_pt2(a, p)
term(72) = term(72) + s1(a,q) * wm_interm_cc3_1_triplet_pt2(a, p)
term(73) = term(73) + s1(a,q) * wm_interm_cc3_8_triplet_pt2(a, p)
term(74) = term(74) + s1(a,q) * wm_interm_cc3_9_triplet_pt2(a, p)
term(75) = term(75) + s1(a,q) * wm_interm_cc3_6_triplet_pt2(a, p)
term(76) = term(76) + s1(a,q) * wm_interm_cc3_7_triplet_pt2(a, p)
term(77) = term(77) + t1(a,q) * wm_interm_cc3_12_triplet_pt2(a, p)
term(78) = term(78) + t1(a,q) * wm_interm_cc3_14_triplet_pt2(a, p)
term(79) = term(79) + t1(a,q) * wm_interm_cc3_10_triplet_pt2(a, p)
term(80) = term(80) + t1(a,q) * wm_interm_cc3_13_triplet_pt2(a, p)
term(81) = term(81) + t1(a,q) * wm_interm_cc3_11_triplet_pt2(a, p)
term(82) = term(82) + t1(a,q) * wm_interm_cc3_17_triplet_pt2(a, p)
term(83) = term(83) + t1(a,q) * wm_interm_cc3_18_triplet_pt2(a, p)
term(84) = term(84) + t1(a,q) * wm_interm_cc3_15_triplet_pt2(a, p)
term(85) = term(85) + t1(a,q) * wm_interm_cc3_16_triplet_pt2(a, p)
end do 

term(67) = term(67) * 3.0d+0 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * 3.0d+0 
term(70) = term(70) * (-2.0d+0) 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * 4.0d+0 
term(73) = term(73) * 12.0d+0 
term(74) = term(74) * (-12.0d+0) 
term(75) = term(75) * (-8.0d+0) 
term(76) = term(76) * 8.0d+0 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * 2.0d+0 
term(79) = term(79) * 8.0d+0 
term(80) = term(80) * 2.0d+0 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * 12.0d+0 
term(83) = term(83) * (-12.0d+0) 
term(84) = term(84) * (-8.0d+0) 
term(85) = term(85) * 8.0d+0 


    calc_D_oo_wm_triplet_cc3_pt2 = zero
    do s = 0, 85
    calc_D_oo_wm_triplet_cc3_pt2 = calc_D_oo_wm_triplet_cc3_pt2 + term(s)
    end do

    end function calc_D_oo_wm_triplet_cc3_pt2
    
    function calc_D_ov_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_cc3_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_ov_wm_triplet_cc3_pt2 = zero
    do s = 0, 1
    calc_D_ov_wm_triplet_cc3_pt2 = calc_D_ov_wm_triplet_cc3_pt2 + term(s)
    end do

    end function calc_D_ov_wm_triplet_cc3_pt2
    
    function calc_D_vo_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_cc3_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, b, a 
    real(F64), dimension(0:13) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,j)
term(1) = term(1) + r1(vrdav_Rl, b,i) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,j)
term(2) = term(2) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,i)
term(3) = term(3) + r1(vrdav_Rl, b,j) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,i)
term(4) = term(4) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,i) * t1(b,j)
term(5) = term(5) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,i) * t1(a,j)
term(6) = term(6) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,j) * t1(b,i)
term(7) = term(7) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,j) * t1(a,i)
end do 
end do 
end do 
end do 

term(0) = term(0) * (-8.0d+0) 
term(1) = term(1) * 6.0d+0 
term(2) = term(2) * 6.0d+0 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * 8.0d+0 
term(5) = term(5) * (-6.0d+0) 
term(6) = term(6) * (-6.0d+0) 
term(7) = term(7) * 4.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(8) = term(8) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,j)
term(9) = term(9) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,i) * t1(b,j)
term(10) = term(10) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,j) * t1(b,i)
term(11) = term(11) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,i) * t1(a,j)
term(12) = term(12) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,j) * t1(a,i)
end do 
end do 
end do 
end do 

term(8) = term(8) * 8.0d+0 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * 4.0d+0 
term(11) = term(11) * 4.0d+0 
term(12) = term(12) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(13) = term(13) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,i,p,q) * s1(b,j)
end do 
end do 
end do 
end do 

term(13) = term(13) * (-8.0d+0) 


    calc_D_vo_wm_triplet_cc3_pt2 = zero
    do s = 0, 13
    calc_D_vo_wm_triplet_cc3_pt2 = calc_D_vo_wm_triplet_cc3_pt2 + term(s)
    end do

    end function calc_D_vo_wm_triplet_cc3_pt2
    
    function calc_D_vv_wm_triplet_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_cc3_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, a, i, b 
    real(F64), dimension(0:66) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(0) = term(0) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_cc3_19_triplet_pt2(a, p, i, j)
term(1) = term(1) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_cc3_21_triplet_pt2(a, p, i, j)
term(2) = term(2) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_cc3_20_triplet_pt2(a, p, i, j)
term(3) = term(3) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_cc3_22_triplet_pt2(q, a, i, j)
term(4) = term(4) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_cc3_22_triplet_pt2(a, q, i, j)
term(5) = term(5) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_cc3_23_triplet_pt2(q, a, i, j)
term(6) = term(6) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_cc3_23_triplet_pt2(a, q, i, j)
term(7) = term(7) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_cc3_24_triplet_pt2(q, a, i, j)
term(8) = term(8) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_cc3_25_triplet_pt2(q, a, i, j)
term(9) = term(9) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_cc3_25_triplet_pt2(a, q, i, j)
term(10) = term(10) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_cc3_24_triplet_pt2(a, q, i, j)
term(11) = term(11) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_cc3_26_triplet_pt2(q, a, i, j)
term(12) = term(12) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_cc3_26_triplet_pt2(a, q, i, j)
end do 
end do 
end do 

term(0) = term(0) * (-8.0d+0) 
term(1) = term(1) * (-5.0d+0) 
term(2) = term(2) * 5.0d+0 
term(3) = term(3) * (-8.0d+0) 
term(4) = term(4) * 8.0d+0 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * 4.0d+0 
term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * 2.0d+0 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(13) = term(13) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_cc3_19_triplet_pt2(a, p, j, i)
term(14) = term(14) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_cc3_21_triplet_pt2(a, p, j, i)
term(15) = term(15) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_cc3_20_triplet_pt2(a, p, j, i)
term(16) = term(16) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_cc3_19_triplet_pt2(a, p, i, j)
term(17) = term(17) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_cc3_19_triplet_pt2(a, p, j, i)
term(18) = term(18) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_cc3_19_triplet_pt2(a, p, i, j)
term(19) = term(19) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_cc3_21_triplet_pt2(a, p, i, j)
term(20) = term(20) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_cc3_21_triplet_pt2(a, p, j, i)
term(21) = term(21) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_cc3_20_triplet_pt2(a, p, j, i)
term(22) = term(22) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_cc3_20_triplet_pt2(a, p, i, j)
term(23) = term(23) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_cc3_21_triplet_pt2(a, p, i, j)
term(24) = term(24) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_cc3_20_triplet_pt2(a, p, i, j)
term(25) = term(25) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_22_triplet_pt2(q, a, i, j)
term(26) = term(26) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_22_triplet_pt2(a, q, j, i)
term(27) = term(27) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_22_triplet_pt2(a, q, i, j)
term(28) = term(28) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_22_triplet_pt2(q, a, j, i)
term(29) = term(29) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_23_triplet_pt2(q, a, i, j)
term(30) = term(30) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_23_triplet_pt2(a, q, j, i)
term(31) = term(31) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_23_triplet_pt2(a, q, i, j)
term(32) = term(32) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_23_triplet_pt2(q, a, j, i)
term(33) = term(33) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_24_triplet_pt2(q, a, i, j)
term(34) = term(34) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_24_triplet_pt2(q, a, j, i)
term(35) = term(35) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_25_triplet_pt2(q, a, j, i)
term(36) = term(36) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_25_triplet_pt2(a, q, i, j)
term(37) = term(37) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_25_triplet_pt2(a, q, j, i)
term(38) = term(38) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_24_triplet_pt2(a, q, j, i)
term(39) = term(39) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_24_triplet_pt2(a, q, i, j)
term(40) = term(40) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_25_triplet_pt2(q, a, i, j)
term(41) = term(41) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_26_triplet_pt2(q, a, j, i)
term(42) = term(42) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_26_triplet_pt2(q, a, i, j)
term(43) = term(43) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_26_triplet_pt2(a, q, i, j)
term(44) = term(44) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_cc3_26_triplet_pt2(a, q, j, i)
end do 
end do 
end do 

term(13) = term(13) * 8.0d+0 
term(14) = term(14) * 7.0d+0 
term(15) = term(15) * (-7.0d+0) 
term(16) = term(16) * (-8.0d+0) 
term(17) = term(17) * (-8.0d+0) 
term(18) = term(18) * 8.0d+0 
term(19) = term(19) * 6.0d+0 
term(20) = term(20) * (-8.0d+0) 
term(21) = term(21) * 6.0d+0 
term(22) = term(22) * 6.0d+0 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * 8.0d+0 
term(26) = term(26) * 8.0d+0 
term(27) = term(27) * (-8.0d+0) 
term(28) = term(28) * (-8.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * 4.0d+0 
term(32) = term(32) * 4.0d+0 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * 4.0d+0 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * 2.0d+0 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * 4.0d+0 
term(40) = term(40) * 2.0d+0 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * 2.0d+0 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * 2.0d+0 

do i = 1, nocc 
term(45) = term(45) + s1(q,i) * wm_interm_cc3_2_triplet_pt2(p, i)
term(46) = term(46) + s1(q,i) * wm_interm_cc3_4_triplet_pt2(p, i)
term(47) = term(47) + s1(q,i) * wm_interm_cc3_3_triplet_pt2(p, i)
term(48) = term(48) + s1(q,i) * wm_interm_cc3_5_triplet_pt2(p, i)
term(49) = term(49) + s1(q,i) * wm_interm_cc3_0_triplet_pt2(p, i)
term(50) = term(50) + s1(q,i) * wm_interm_cc3_1_triplet_pt2(p, i)
term(51) = term(51) + s1(q,i) * wm_interm_cc3_8_triplet_pt2(p, i)
term(52) = term(52) + s1(q,i) * wm_interm_cc3_9_triplet_pt2(p, i)
term(53) = term(53) + s1(q,i) * wm_interm_cc3_6_triplet_pt2(p, i)
term(54) = term(54) + s1(q,i) * wm_interm_cc3_7_triplet_pt2(p, i)
term(55) = term(55) + t1(q,i) * wm_interm_cc3_12_triplet_pt2(p, i)
term(56) = term(56) + t1(q,i) * wm_interm_cc3_14_triplet_pt2(p, i)
term(57) = term(57) + t1(q,i) * wm_interm_cc3_10_triplet_pt2(p, i)
term(58) = term(58) + t1(q,i) * wm_interm_cc3_13_triplet_pt2(p, i)
term(59) = term(59) + t1(q,i) * wm_interm_cc3_11_triplet_pt2(p, i)
term(60) = term(60) + t1(q,i) * wm_interm_cc3_17_triplet_pt2(p, i)
term(61) = term(61) + t1(q,i) * wm_interm_cc3_18_triplet_pt2(p, i)
term(62) = term(62) + t1(q,i) * wm_interm_cc3_15_triplet_pt2(p, i)
term(63) = term(63) + t1(q,i) * wm_interm_cc3_16_triplet_pt2(p, i)
end do 

term(45) = term(45) * (-3.0d+0) 
term(46) = term(46) * 4.0d+0 
term(47) = term(47) * (-3.0d+0) 
term(48) = term(48) * 2.0d+0 
term(49) = term(49) * 4.0d+0 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * (-12.0d+0) 
term(52) = term(52) * 12.0d+0 
term(53) = term(53) * 8.0d+0 
term(54) = term(54) * (-8.0d+0) 
term(55) = term(55) * 4.0d+0 
term(56) = term(56) * (-2.0d+0) 
term(57) = term(57) * (-8.0d+0) 
term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * 4.0d+0 
term(60) = term(60) * (-12.0d+0) 
term(61) = term(61) * 12.0d+0 
term(62) = term(62) * 8.0d+0 
term(63) = term(63) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(64) = term(64) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_cc3_19_triplet_pt2(a, p, j, i)
term(65) = term(65) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_cc3_21_triplet_pt2(a, p, j, i)
term(66) = term(66) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_cc3_20_triplet_pt2(a, p, j, i)
end do 
end do 
end do 

term(64) = term(64) * 8.0d+0 
term(65) = term(65) * 6.0d+0 
term(66) = term(66) * (-8.0d+0) 


    calc_D_vv_wm_triplet_cc3_pt2 = zero
    do s = 0, 66
    calc_D_vv_wm_triplet_cc3_pt2 = calc_D_vv_wm_triplet_cc3_pt2 + term(s)
    end do

    end function calc_D_vv_wm_triplet_cc3_pt2
    

    

    function calc_D_oo_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_oo_wm_triplet_cc3_pt0 = zero
    do s = 0, 1
    calc_D_oo_wm_triplet_cc3_pt0 = calc_D_oo_wm_triplet_cc3_pt0 + term(s)
    end do

    end function calc_D_oo_wm_triplet_cc3_pt0
    
    function calc_D_ov_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_ov_wm_triplet_cc3_pt0 = zero
    do s = 0, 1
    calc_D_ov_wm_triplet_cc3_pt0 = calc_D_ov_wm_triplet_cc3_pt0 + term(s)
    end do

    end function calc_D_ov_wm_triplet_cc3_pt0
    
    function calc_D_vo_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, b, a 
    real(F64), dimension(0:18) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2p(vrdav_Rl, b,i,a,j) * r3(vrdav_Rr, a,q,b,j,p,i)
term(1) = term(1) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,b,j,p,i)
term(2) = term(2) + r2p(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,b,j,p,i)
term(3) = term(3) + r2p(vrdav_Rl, b,j,a,i) * r3(vrdav_Rr, a,q,b,j,p,i)
term(4) = term(4) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,b,j,p,i)
term(5) = term(5) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,b,j,p,i)
term(6) = term(6) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, a,i,b,j)
term(7) = term(7) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, b,i,a,j)
term(8) = term(8) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, a,j,b,i)
term(9) = term(9) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, b,j,a,i)
term(10) = term(10) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, a,j,b,i)
term(11) = term(11) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 

term(0) = term(0) * 3.0d+0 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * 3.0d+0 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 12.0d+0 
term(5) = term(5) * (-12.0d+0) 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * 2.0d+0 
term(8) = term(8) * 2.0d+0 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * 12.0d+0 
term(11) = term(11) * (-12.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(12) = term(12) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,p,q)
term(13) = term(13) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,p,q)
term(14) = term(14) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 

term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * (-8.0d+0) 
term(14) = term(14) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(15) = term(15) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,p,q)
term(16) = term(16) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,p,q)
term(17) = term(17) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, a,i,b,j)
term(18) = term(18) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 

term(15) = term(15) * 4.0d+0 
term(16) = term(16) * 8.0d+0 
term(17) = term(17) * 8.0d+0 
term(18) = term(18) * 8.0d+0 


    calc_D_vo_wm_triplet_cc3_pt0 = zero
    do s = 0, 18
    calc_D_vo_wm_triplet_cc3_pt0 = calc_D_vo_wm_triplet_cc3_pt0 + term(s)
    end do

    end function calc_D_vo_wm_triplet_cc3_pt0
    
    function calc_D_vv_wm_triplet_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_vv_wm_triplet_cc3_pt0 = zero
    do s = 0, 1
    calc_D_vv_wm_triplet_cc3_pt0 = calc_D_vv_wm_triplet_cc3_pt0 + term(s)
    end do

    end function calc_D_vv_wm_triplet_cc3_pt0
    

    function calc_D_oo_wm_triplet_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_oo_wm_triplet_cc3_pt3 = zero
    do s = 0, 1
    calc_D_oo_wm_triplet_cc3_pt3 = calc_D_oo_wm_triplet_cc3_pt3 + term(s)
    end do

    end function calc_D_oo_wm_triplet_cc3_pt3
    
    function calc_D_ov_wm_triplet_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, b, a, k 
    real(F64), dimension(0:67) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_cc3_48_triplet_pt3(a, b, j, i)
term(1) = term(1) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_cc3_49_triplet_pt3(a, b, j, i)
term(2) = term(2) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_cc3_51_triplet_pt3(a, b, j, i)
term(3) = term(3) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_cc3_50_triplet_pt3(a, b, j, i)
term(4) = term(4) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_54_triplet_pt3(a, b, j, i)
term(5) = term(5) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_55_triplet_pt3(a, b, j, i)
term(6) = term(6) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_56_triplet_pt3(a, b, j, i)
term(7) = term(7) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_58_triplet_pt3(a, b, j, i)
end do 
end do 
end do 
end do 

term(0) = term(0) * (-8.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * 6.0d+0 
term(3) = term(3) * 3.0d+0 
term(4) = term(4) * 4.0d+0 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(8) = term(8) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_cc3_48_triplet_pt3(a, b, i, j)
term(9) = term(9) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_cc3_49_triplet_pt3(a, b, i, j)
term(10) = term(10) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_cc3_50_triplet_pt3(a, b, i, j)
term(11) = term(11) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_cc3_51_triplet_pt3(a, b, i, j)
term(12) = term(12) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_57_triplet_pt3(a, b, i, j)
term(13) = term(13) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_56_triplet_pt3(a, b, i, j)
term(14) = term(14) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_54_triplet_pt3(a, b, i, j)
term(15) = term(15) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_58_triplet_pt3(a, b, i, j)
end do 
end do 
end do 
end do 

term(8) = term(8) * 8.0d+0 
term(9) = term(9) * 3.0d+0 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = -term(12) 
term(13) = term(13) * 4.0d+0 
term(14) = term(14) * (-4.0d+0) 
term(15) = -term(15) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_cc3_14_triplet_pt3(a, i)
term(17) = term(17) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_cc3_15_triplet_pt3(a, i)
term(18) = term(18) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_cc3_16_triplet_pt3(a, i)
term(19) = term(19) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_cc3_17_triplet_pt3(a, i)
term(20) = term(20) + r1(vrdav_Rr, a,p) * t1(q,i) * wm_interm_cc3_43_triplet_pt3(a, i)
term(21) = term(21) + r1(vrdav_Rr, a,p) * t1(q,i) * wm_interm_cc3_45_triplet_pt3(a, i)
term(22) = term(22) + r1(vrdav_Rr, a,p) * t1(q,i) * wm_interm_cc3_46_triplet_pt3(a, i)
term(23) = term(23) + r1(vrdav_Rr, a,p) * t1(q,i) * wm_interm_cc3_47_triplet_pt3(a, i)
end do 
end do 

term(16) = term(16) * 6.0d+0 
term(17) = term(17) * (-8.0d+0) 
term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * 4.0d+0 
term(20) = term(20) * (-6.0d+0) 
term(21) = term(21) * 2.0d+0 
term(22) = term(22) * 8.0d+0 
term(23) = term(23) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(24) = term(24) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_54_triplet_pt3(b, a, i, j)
term(25) = term(25) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_55_triplet_pt3(b, a, i, j)
term(26) = term(26) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_56_triplet_pt3(b, a, i, j)
term(27) = term(27) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_57_triplet_pt3(b, a, i, j)
end do 
end do 
end do 
end do 

term(24) = term(24) * 4.0d+0 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * 2.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(28) = term(28) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_54_triplet_pt3(b, a, j, i)
term(29) = term(29) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_55_triplet_pt3(b, a, j, i)
term(30) = term(30) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_57_triplet_pt3(b, a, j, i)
term(31) = term(31) + r1(vrdav_Rr, a,p) * t2(b,q,j,i) * wm_interm_cc3_58_triplet_pt3(b, a, j, i)
end do 
end do 
end do 
end do 

term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * 4.0d+0 
term(30) = -term(30) 
term(31) = -term(31) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(32) = term(32) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_28_triplet_pt3(b, p, k, i)
term(33) = term(33) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_28_triplet_pt3(b, p, k, j)
term(34) = term(34) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_28_triplet_pt3(b, p, j, k)
term(35) = term(35) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, a,i) * wm_interm_cc3_35_triplet_pt3(b, p, j, k)
term(36) = term(36) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, b,j) * wm_interm_cc3_35_triplet_pt3(a, p, k, i)
term(37) = term(37) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, a,k) * wm_interm_cc3_35_triplet_pt3(b, p, j, i)
term(38) = term(38) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, a,j) * wm_interm_cc3_35_triplet_pt3(b, p, k, i)
term(39) = term(39) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, b,j) * wm_interm_cc3_35_triplet_pt3(a, p, i, k)
term(40) = term(40) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, a,j) * wm_interm_cc3_35_triplet_pt3(b, p, i, k)
term(41) = term(41) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, b,i) * wm_interm_cc3_35_triplet_pt3(a, p, j, k)
term(42) = term(42) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, b,k) * wm_interm_cc3_35_triplet_pt3(a, p, j, i)
end do 
end do 
end do 
end do 
end do 

term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * 4.0d+0 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (-3.0d+0) 
term(36) = term(36) * 4.0d+0 
term(37) = term(37) * 4.0d+0 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * (-3.0d+0) 
term(40) = term(40) * 2.0d+0 
term(41) = term(41) * 4.0d+0 
term(42) = term(42) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(43) = term(43) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, b,i) * wm_interm_cc3_35_triplet_pt3(a, p, k, j)
term(44) = term(44) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, a,i) * wm_interm_cc3_35_triplet_pt3(b, p, k, j)
term(45) = term(45) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, b,k) * wm_interm_cc3_35_triplet_pt3(a, p, i, j)
term(46) = term(46) + r3(vrdav_Rl, a,j,b,k,q,i) * r1(vrdav_Rr, a,k) * wm_interm_cc3_35_triplet_pt3(b, p, i, j)
end do 
end do 
end do 
end do 
end do 

term(43) = term(43) * (-3.0d+0) 
term(44) = term(44) * 4.0d+0 
term(45) = term(45) * 2.0d+0 
term(46) = term(46) * (-3.0d+0) 

do a = nocc + 1, nactive 
term(47) = term(47) + wm_interm_cc3_14_triplet_pt3(a, p) * wm_interm_cc3_18_triplet_pt3(a, q)
term(48) = term(48) + wm_interm_cc3_15_triplet_pt3(a, p) * wm_interm_cc3_18_triplet_pt3(a, q)
term(49) = term(49) + wm_interm_cc3_16_triplet_pt3(a, p) * wm_interm_cc3_18_triplet_pt3(a, q)
term(50) = term(50) + wm_interm_cc3_17_triplet_pt3(a, p) * wm_interm_cc3_18_triplet_pt3(a, q)
end do 

term(47) = term(47) * 6.0d+0 
term(48) = term(48) * (-8.0d+0) 
term(49) = term(49) * (-2.0d+0) 
term(50) = term(50) * 4.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(51) = term(51) + r1(vrdav_Rl, q,i) * r1(vrdav_Rr, a,j) * wm_interm_cc3_1_triplet_pt3(a, j, p, i)
term(52) = term(52) + r1(vrdav_Rl, q,i) * r1(vrdav_Rr, a,j) * wm_interm_cc3_2_triplet_pt3(a, p, j, i)
term(53) = term(53) + r1(vrdav_Rl, q,i) * r1(vrdav_Rr, a,j) * wm_interm_cc3_3_triplet_pt3(a, p, j, i)
term(54) = term(54) + r1(vrdav_Rl, a,j) * t1(q,i) * wm_interm_cc3_9_triplet_pt3(a, j, p, i)
term(55) = term(55) + r1(vrdav_Rl, a,j) * t1(q,i) * wm_interm_cc3_10_triplet_pt3(a, j, p, i)
term(56) = term(56) + r1(vrdav_Rl, a,j) * t1(q,i) * wm_interm_cc3_10_triplet_pt3(a, p, j, i)
term(57) = term(57) + r1(vrdav_Rl, a,j) * t1(q,i) * wm_interm_cc3_12_triplet_pt3(a, p, j, i)
term(58) = term(58) + r1(vrdav_Rl, a,j) * t1(q,i) * wm_interm_cc3_8_triplet_pt3(a, j, p, i)
term(59) = term(59) + r1(vrdav_Rl, a,j) * t1(q,i) * wm_interm_cc3_12_triplet_pt3(a, j, p, i)
end do 
end do 
end do 

term(51) = term(51) * (-2.0d+0) 
term(52) = term(52) * (-2.0d+0) 
term(53) = term(53) * 3.9999999999999996d+0 
term(54) = term(54) * (-6.0d+0) 
term(55) = term(55) * 4.0d+0 
term(56) = term(56) * (-6.0d+0) 
term(57) = term(57) * 8.0d+0 
term(58) = term(58) * 8.0d+0 
term(59) = term(59) * (-8.0d+0) 

do i = 1, nocc 
term(60) = term(60) + wm_interm_cc3_0_triplet_pt3(i, p) * wm_interm_cc3_21_triplet_pt3(q, i)
term(61) = term(61) + wm_interm_cc3_0_triplet_pt3(i, p) * wm_interm_cc3_26_triplet_pt3(q, i)
term(62) = term(62) + wm_interm_cc3_0_triplet_pt3(i, p) * wm_interm_cc3_31_triplet_pt3(q, i)
term(63) = term(63) + wm_interm_cc3_0_triplet_pt3(i, p) * wm_interm_cc3_32_triplet_pt3(q, i)
term(64) = term(64) + wm_interm_cc3_37_triplet_pt3(i, p) * wm_interm_cc3_43_triplet_pt3(q, i)
term(65) = term(65) + wm_interm_cc3_37_triplet_pt3(i, p) * wm_interm_cc3_45_triplet_pt3(q, i)
term(66) = term(66) + wm_interm_cc3_37_triplet_pt3(i, p) * wm_interm_cc3_46_triplet_pt3(q, i)
term(67) = term(67) + wm_interm_cc3_37_triplet_pt3(i, p) * wm_interm_cc3_47_triplet_pt3(q, i)
end do 

term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * 4.0d+0 
term(62) = term(62) * (-3.9999999999999996d+0) 
term(63) = term(63) * 1.9999999999999998d+0 
term(64) = term(64) * (-6.0d+0) 
term(65) = term(65) * 2.0d+0 
term(66) = term(66) * 8.0d+0 
term(67) = term(67) * (-4.0d+0) 


    calc_D_ov_wm_triplet_cc3_pt3 = zero
    do s = 0, 67
    calc_D_ov_wm_triplet_cc3_pt3 = calc_D_ov_wm_triplet_cc3_pt3 + term(s)
    end do

    end function calc_D_ov_wm_triplet_cc3_pt3
    
    function calc_D_vo_wm_triplet_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, b, i, a, k 
    real(F64), dimension(0:245) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,i,j) * wm_interm_cc3_4_triplet_pt3(b, j)
term(1) = term(1) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_interm_cc3_4_triplet_pt3(b, j)
term(2) = term(2) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_interm_cc3_27_triplet_pt3(b, j)
term(3) = term(3) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,i) * wm_interm_cc3_33_triplet_pt3(b, j)
term(4) = term(4) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,i) * wm_interm_cc3_34_triplet_pt3(b, j)
term(5) = term(5) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,j) * wm_interm_cc3_33_triplet_pt3(b, i)
term(6) = term(6) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,j) * wm_interm_cc3_34_triplet_pt3(b, i)
term(7) = term(7) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,i) * wm_interm_cc3_33_triplet_pt3(a, j)
term(8) = term(8) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,i) * wm_interm_cc3_34_triplet_pt3(a, j)
term(9) = term(9) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,j) * wm_interm_cc3_33_triplet_pt3(a, i)
term(10) = term(10) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,j) * wm_interm_cc3_34_triplet_pt3(a, i)
end do 
end do 
end do 
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * 3.9999999999999996d+0 
term(2) = term(2) * 4.0d+0 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * 8.0d+0 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-8.0d+0) 
term(7) = term(7) * 4.0d+0 
term(8) = term(8) * (-8.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * 8.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(11) = term(11) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,j,q,i) * wm_interm_cc3_4_triplet_pt3(b, j)
term(12) = term(12) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,i) * wm_interm_cc3_33_triplet_pt3(b, j)
term(13) = term(13) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,i) * wm_interm_cc3_34_triplet_pt3(b, j)
term(14) = term(14) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,i) * wm_interm_cc3_33_triplet_pt3(a, j)
term(15) = term(15) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,j) * wm_interm_cc3_33_triplet_pt3(b, i)
term(16) = term(16) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,i) * wm_interm_cc3_34_triplet_pt3(a, j)
term(17) = term(17) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,j) * wm_interm_cc3_34_triplet_pt3(b, i)
term(18) = term(18) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,j) * wm_interm_cc3_33_triplet_pt3(a, i)
term(19) = term(19) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,j) * wm_interm_cc3_34_triplet_pt3(a, i)
term(20) = term(20) + r1(vrdav_Rl, a,i) * s2(b,p,j,q) * wm_interm_cc3_48_triplet_pt3(a, b, j, i)
term(21) = term(21) + r1(vrdav_Rl, a,i) * s2(b,p,j,q) * wm_interm_cc3_51_triplet_pt3(a, b, j, i)
term(22) = term(22) + s1(a,q) * s2(b,p,j,i) * wm_interm_cc3_52_triplet_pt3(a, b, j, i)
term(23) = term(23) + s1(a,q) * s2(b,p,j,i) * wm_interm_cc3_53_triplet_pt3(a, b, j, i)
term(24) = term(24) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_54_triplet_pt3(a, b, j, i)
term(25) = term(25) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_55_triplet_pt3(a, b, j, i)
term(26) = term(26) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_56_triplet_pt3(a, b, j, i)
term(27) = term(27) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_57_triplet_pt3(a, b, j, i)
term(28) = term(28) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_54_triplet_pt3(a, b, j, i)
term(29) = term(29) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_55_triplet_pt3(a, b, j, i)
term(30) = term(30) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_56_triplet_pt3(a, b, j, i)
term(31) = term(31) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_58_triplet_pt3(a, b, j, i)
term(32) = term(32) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_60_triplet_pt3(a, b, j, i)
term(33) = term(33) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_61_triplet_pt3(a, b, j, i)
term(34) = term(34) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_62_triplet_pt3(a, b, j, i)
end do 
end do 
end do 
end do 

term(11) = term(11) * (-1.9999999999999998d+0) 
term(12) = term(12) * 8.0d+0 
term(13) = term(13) * (-16.0d+0) 
term(14) = term(14) * (-6.0d+0) 
term(15) = term(15) * (-6.0d+0) 
term(16) = term(16) * 12.0d+0 
term(17) = term(17) * 12.0d+0 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * 16.0d+0 
term(21) = term(21) * (-12.0d+0) 
term(22) = term(22) * 12.0d+0 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * 4.0d+0 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * 2.0d+0 
term(28) = term(28) * (-8.0d+0) 
term(29) = term(29) * 4.0d+0 
term(30) = term(30) * 4.0d+0 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (-2.0d+0) 
term(34) = term(34) * 2.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(35) = term(35) + r1(vrdav_Rl, b,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_27_triplet_pt3(a, j)
term(36) = term(36) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_27_triplet_pt3(b, i)
end do 
end do 
end do 
end do 

term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(37) = term(37) + wm_interm_cc3_21_triplet_pt3(a, q) * wm_interm_cc3_22_triplet_pt3(a, p)
term(38) = term(38) + wm_interm_cc3_22_triplet_pt3(a, p) * wm_interm_cc3_26_triplet_pt3(a, q)
term(39) = term(39) + wm_interm_cc3_22_triplet_pt3(a, p) * wm_interm_cc3_31_triplet_pt3(a, q)
term(40) = term(40) + wm_interm_cc3_22_triplet_pt3(a, p) * wm_interm_cc3_32_triplet_pt3(a, q)
term(41) = term(41) + wm_interm_cc3_43_triplet_pt3(a, q) * wm_interm_cc3_44_triplet_pt3(a, p)
term(42) = term(42) + wm_interm_cc3_44_triplet_pt3(a, p) * wm_interm_cc3_45_triplet_pt3(a, q)
term(43) = term(43) + wm_interm_cc3_44_triplet_pt3(a, p) * wm_interm_cc3_46_triplet_pt3(a, q)
term(44) = term(44) + wm_interm_cc3_44_triplet_pt3(a, p) * wm_interm_cc3_47_triplet_pt3(a, q)
end do 

term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * 3.9999999999999996d+0 
term(39) = term(39) * (-3.9999999999999996d+0) 
term(40) = term(40) * 1.9999999999999998d+0 
term(41) = term(41) * (-6.0d+0) 
term(42) = term(42) * 2.0d+0 
term(43) = term(43) * 8.0d+0 
term(44) = term(44) * (-4.0d+0) 

do i = 1, nocc 
term(45) = term(45) + wm_interm_cc3_14_triplet_pt3(p, i) * wm_interm_cc3_7_triplet_pt3(i, q)
term(46) = term(46) + wm_interm_cc3_15_triplet_pt3(p, i) * wm_interm_cc3_7_triplet_pt3(i, q)
term(47) = term(47) + wm_interm_cc3_16_triplet_pt3(p, i) * wm_interm_cc3_7_triplet_pt3(i, q)
term(48) = term(48) + wm_interm_cc3_17_triplet_pt3(p, i) * wm_interm_cc3_7_triplet_pt3(i, q)
end do 

term(45) = term(45) * 6.0d+0 
term(46) = term(46) * (-8.0d+0) 
term(47) = term(47) * (-2.0d+0) 
term(48) = term(48) * 4.0d+0 

do i = 1, nocc 
do j = 1, nocc 
term(49) = term(49) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_1_triplet_pt3(p, q, j, i)
term(50) = term(50) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_1_triplet_pt3(p, j, q, i)
term(51) = term(51) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_2_triplet_pt3(p, j, q, i)
term(52) = term(52) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_2_triplet_pt3(p, q, j, i)
term(53) = term(53) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_3_triplet_pt3(p, j, q, i)
term(54) = term(54) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_3_triplet_pt3(p, q, j, i)
term(55) = term(55) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_19_triplet_pt3(p, j, q, i)
term(56) = term(56) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_23_triplet_pt3(p, j, i, q)
term(57) = term(57) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_24_triplet_pt3(p, j, i, q)
term(58) = term(58) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_25_triplet_pt3(p, j, q, i)
term(59) = term(59) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_19_triplet_pt3(p, j, i, q)
term(60) = term(60) + wm_interm_cc3_0_triplet_pt3(i, j) * wm_interm_cc3_25_triplet_pt3(p, j, i, q)
end do 
end do 

term(49) = term(49) * (-2.0d+0) 
term(50) = term(50) * 4.0d+0 
term(51) = term(51) * (-2.0d+0) 
term(52) = term(52) * 4.0d+0 
term(53) = term(53) * 3.9999999999999996d+0 
term(54) = term(54) * (-7.999999999999999d+0) 
term(55) = term(55) * (-1.9999999999999998d+0) 
term(56) = term(56) * (-2.0d+0) 
term(57) = term(57) * 4.0d+0 
term(58) = term(58) * 3.9999999999999996d+0 
term(59) = term(59) * 4.0d+0 
term(60) = term(60) * (-7.999999999999999d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(61) = term(61) + wm_interm_cc3_7_triplet_pt3(i, j) * wm_interm_cc3_8_triplet_pt3(p, i, q, j)
term(62) = term(62) + wm_interm_cc3_7_triplet_pt3(i, j) * wm_interm_cc3_9_triplet_pt3(p, i, q, j)
term(63) = term(63) + wm_interm_cc3_10_triplet_pt3(p, i, q, j) * wm_interm_cc3_7_triplet_pt3(i, j)
term(64) = term(64) + wm_interm_cc3_11_triplet_pt3(p, i, q, j) * wm_interm_cc3_7_triplet_pt3(i, j)
term(65) = term(65) + wm_interm_cc3_12_triplet_pt3(p, i, q, j) * wm_interm_cc3_7_triplet_pt3(i, j)
term(66) = term(66) + wm_interm_cc3_13_triplet_pt3(p, i, q, j) * wm_interm_cc3_7_triplet_pt3(i, j)
term(67) = term(67) + wm_interm_cc3_36_triplet_pt3(p, i, q, j) * wm_interm_cc3_37_triplet_pt3(i, j)
term(68) = term(68) + wm_interm_cc3_36_triplet_pt3(p, q, i, j) * wm_interm_cc3_37_triplet_pt3(i, j)
term(69) = term(69) + wm_interm_cc3_37_triplet_pt3(i, j) * wm_interm_cc3_38_triplet_pt3(p, i, q, j)
term(70) = term(70) + wm_interm_cc3_37_triplet_pt3(i, j) * wm_interm_cc3_38_triplet_pt3(p, q, i, j)
term(71) = term(71) + wm_interm_cc3_37_triplet_pt3(i, j) * wm_interm_cc3_41_triplet_pt3(p, i, q, j)
term(72) = term(72) + wm_interm_cc3_37_triplet_pt3(i, j) * wm_interm_cc3_40_triplet_pt3(p, q, i, j)
term(73) = term(73) + wm_interm_cc3_37_triplet_pt3(i, j) * wm_interm_cc3_40_triplet_pt3(p, i, q, j)
term(74) = term(74) + wm_interm_cc3_37_triplet_pt3(i, j) * wm_interm_cc3_39_triplet_pt3(p, q, i, j)
term(75) = term(75) + wm_interm_cc3_37_triplet_pt3(i, j) * wm_interm_cc3_39_triplet_pt3(p, i, q, j)
term(76) = term(76) + wm_interm_cc3_37_triplet_pt3(i, j) * wm_interm_cc3_42_triplet_pt3(p, q, i, j)
term(77) = term(77) + wm_interm_cc3_37_triplet_pt3(i, j) * wm_interm_cc3_41_triplet_pt3(p, q, i, j)
term(78) = term(78) + wm_interm_cc3_37_triplet_pt3(i, j) * wm_interm_cc3_42_triplet_pt3(p, i, q, j)
end do 
end do 

term(61) = term(61) * (-6.0d+0) 
term(62) = term(62) * 4.0d+0 
term(63) = term(63) * (-6.0d+0) 
term(64) = term(64) * 8.0d+0 
term(65) = term(65) * 8.0d+0 
term(66) = term(66) * (-8.0d+0) 
term(67) = term(67) * (-3.0d+0) 
term(68) = term(68) * 4.0d+0 
term(69) = term(69) * 4.0d+0 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * (-3.0d+0) 
term(72) = term(72) * (-3.0d+0) 
term(73) = term(73) * 2.0d+0 
term(74) = term(74) * (-3.0d+0) 
term(75) = term(75) * 4.0d+0 
term(76) = term(76) * 4.0d+0 
term(77) = term(77) * 2.0d+0 
term(78) = term(78) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(79) = term(79) + r1(vrdav_Rl, a,i) * s2(b,p,j,q) * wm_interm_cc3_48_triplet_pt3(a, b, i, j)
term(80) = term(80) + r1(vrdav_Rl, a,i) * s2(b,p,j,q) * wm_interm_cc3_49_triplet_pt3(a, b, i, j)
term(81) = term(81) + r1(vrdav_Rl, a,i) * s2(b,p,j,q) * wm_interm_cc3_50_triplet_pt3(a, b, i, j)
term(82) = term(82) + r1(vrdav_Rl, a,i) * s2(b,p,j,q) * wm_interm_cc3_51_triplet_pt3(a, b, i, j)
term(83) = term(83) + r1(vrdav_Rl, a,i) * s2(b,p,q,j) * wm_interm_cc3_48_triplet_pt3(a, b, j, i)
term(84) = term(84) + r1(vrdav_Rl, a,i) * s2(b,p,q,j) * wm_interm_cc3_48_triplet_pt3(a, b, i, j)
term(85) = term(85) + r1(vrdav_Rl, a,i) * s2(b,p,q,j) * wm_interm_cc3_49_triplet_pt3(a, b, i, j)
term(86) = term(86) + r1(vrdav_Rl, a,i) * s2(b,p,q,j) * wm_interm_cc3_50_triplet_pt3(a, b, i, j)
term(87) = term(87) + r1(vrdav_Rl, a,i) * s2(b,p,q,j) * wm_interm_cc3_51_triplet_pt3(a, b, j, i)
term(88) = term(88) + r1(vrdav_Rl, a,i) * s2(b,p,q,j) * wm_interm_cc3_51_triplet_pt3(a, b, i, j)
term(89) = term(89) + s1(a,q) * s2(b,p,j,i) * wm_interm_cc3_52_triplet_pt3(a, b, i, j)
term(90) = term(90) + s1(a,q) * s2(b,p,j,i) * wm_interm_cc3_53_triplet_pt3(a, b, i, j)
term(91) = term(91) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_54_triplet_pt3(a, b, i, j)
term(92) = term(92) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_55_triplet_pt3(a, b, i, j)
term(93) = term(93) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_57_triplet_pt3(a, b, i, j)
term(94) = term(94) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_58_triplet_pt3(a, b, i, j)
term(95) = term(95) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_57_triplet_pt3(a, b, i, j)
term(96) = term(96) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_56_triplet_pt3(a, b, i, j)
term(97) = term(97) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_58_triplet_pt3(a, b, i, j)
term(98) = term(98) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_54_triplet_pt3(a, b, i, j)
term(99) = term(99) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_59_triplet_pt3(a, b, i, j)
term(100) = term(100) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_61_triplet_pt3(a, b, i, j)
term(101) = term(101) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_60_triplet_pt3(a, b, i, j)
term(102) = term(102) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_62_triplet_pt3(a, b, i, j)
term(103) = term(103) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_63_triplet_pt3(a, b, i, j)
end do 
end do 
end do 
end do 

term(79) = term(79) * (-16.0d+0) 
term(80) = term(80) * (-12.0d+0) 
term(81) = term(81) * 16.0d+0 
term(82) = term(82) * 8.0d+0 
term(83) = term(83) * (-8.0d+0) 
term(84) = term(84) * 8.0d+0 
term(85) = term(85) * 6.0d+0 
term(86) = term(86) * (-8.0d+0) 
term(87) = term(87) * 6.0d+0 
term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (-16.0d+0) 
term(90) = term(90) * 8.0d+0 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * 4.0d+0 
term(93) = -term(93) 
term(94) = -term(94) 
term(95) = term(95) * 2.0d+0 
term(96) = term(96) * (-8.0d+0) 
term(97) = term(97) * 2.0d+0 
term(98) = term(98) * 8.0d+0 
term(99) = -term(99) 
term(100) = term(100) * 4.0d+0 
term(101) = term(101) * 4.0d+0 
term(102) = term(102) * (-4.0d+0) 
term(103) = -term(103) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(104) = term(104) + s1(a,q) * s2(b,p,j,i) * wm_interm_cc3_53_triplet_pt3(b, a, i, j)
term(105) = term(105) + s1(a,q) * s2(b,p,j,i) * wm_interm_cc3_52_triplet_pt3(b, a, i, j)
term(106) = term(106) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_54_triplet_pt3(b, a, i, j)
term(107) = term(107) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_55_triplet_pt3(b, a, i, j)
term(108) = term(108) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_56_triplet_pt3(b, a, i, j)
term(109) = term(109) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_58_triplet_pt3(b, a, i, j)
term(110) = term(110) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_54_triplet_pt3(b, a, i, j)
term(111) = term(111) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_55_triplet_pt3(b, a, i, j)
term(112) = term(112) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_56_triplet_pt3(b, a, i, j)
term(113) = term(113) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_57_triplet_pt3(b, a, i, j)
term(114) = term(114) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_60_triplet_pt3(b, a, i, j)
term(115) = term(115) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_62_triplet_pt3(b, a, i, j)
term(116) = term(116) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_61_triplet_pt3(b, a, i, j)
end do 
end do 
end do 
end do 

term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * 12.0d+0 
term(106) = term(106) * 4.0d+0 
term(107) = term(107) * (-2.0d+0) 
term(108) = term(108) * (-2.0d+0) 
term(109) = term(109) * 2.0d+0 
term(110) = term(110) * (-8.0d+0) 
term(111) = term(111) * 4.0d+0 
term(112) = term(112) * 4.0d+0 
term(113) = term(113) * (-4.0d+0) 
term(114) = term(114) * (-2.0d+0) 
term(115) = term(115) * 2.0d+0 
term(116) = term(116) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(117) = term(117) + s1(a,q) * s2(b,p,j,i) * wm_interm_cc3_52_triplet_pt3(b, a, j, i)
term(118) = term(118) + s1(a,q) * s2(b,p,j,i) * wm_interm_cc3_53_triplet_pt3(b, a, j, i)
term(119) = term(119) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_57_triplet_pt3(b, a, j, i)
term(120) = term(120) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_56_triplet_pt3(b, a, j, i)
term(121) = term(121) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_58_triplet_pt3(b, a, j, i)
term(122) = term(122) + r1(vrdav_Rr, b,j) * t2(a,p,q,i) * wm_interm_cc3_54_triplet_pt3(b, a, j, i)
term(123) = term(123) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_54_triplet_pt3(b, a, j, i)
term(124) = term(124) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_55_triplet_pt3(b, a, j, i)
term(125) = term(125) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_57_triplet_pt3(b, a, j, i)
term(126) = term(126) + r1(vrdav_Rr, a,i) * t2(b,p,j,q) * wm_interm_cc3_58_triplet_pt3(b, a, j, i)
term(127) = term(127) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_59_triplet_pt3(b, a, j, i)
term(128) = term(128) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_60_triplet_pt3(b, a, j, i)
term(129) = term(129) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_61_triplet_pt3(b, a, j, i)
term(130) = term(130) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_62_triplet_pt3(b, a, j, i)
term(131) = term(131) + t1(a,q) * t2(b,p,j,i) * wm_interm_cc3_63_triplet_pt3(b, a, j, i)
end do 
end do 
end do 
end do 

term(117) = term(117) * (-16.0d+0) 
term(118) = term(118) * 8.0d+0 
term(119) = -term(119) 
term(120) = term(120) * 4.0d+0 
term(121) = -term(121) 
term(122) = term(122) * (-4.0d+0) 
term(123) = term(123) * 8.0d+0 
term(124) = term(124) * (-8.0d+0) 
term(125) = term(125) * 2.0d+0 
term(126) = term(126) * 2.0d+0 
term(127) = -term(127) 
term(128) = term(128) * 4.0d+0 
term(129) = term(129) * 4.0d+0 
term(130) = term(130) * (-4.0d+0) 
term(131) = -term(131) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(132) = term(132) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_cc3_14_triplet_pt3(a, i)
term(133) = term(133) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_cc3_15_triplet_pt3(a, i)
term(134) = term(134) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_cc3_16_triplet_pt3(a, i)
term(135) = term(135) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_cc3_17_triplet_pt3(a, i)
term(136) = term(136) + r1(vrdav_Rl, p,i) * s1(a,q) * wm_interm_cc3_14_triplet_pt3(a, i)
term(137) = term(137) + r1(vrdav_Rl, p,i) * s1(a,q) * wm_interm_cc3_15_triplet_pt3(a, i)
term(138) = term(138) + r1(vrdav_Rl, p,i) * s1(a,q) * wm_interm_cc3_16_triplet_pt3(a, i)
term(139) = term(139) + r1(vrdav_Rl, p,i) * s1(a,q) * wm_interm_cc3_17_triplet_pt3(a, i)
term(140) = term(140) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_cc3_14_triplet_pt3(a, i)
term(141) = term(141) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_cc3_15_triplet_pt3(a, i)
term(142) = term(142) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_cc3_16_triplet_pt3(a, i)
term(143) = term(143) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_cc3_17_triplet_pt3(a, i)
term(144) = term(144) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_cc3_43_triplet_pt3(a, i)
term(145) = term(145) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_cc3_45_triplet_pt3(a, i)
term(146) = term(146) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_cc3_46_triplet_pt3(a, i)
term(147) = term(147) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_cc3_47_triplet_pt3(a, i)
term(148) = term(148) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_cc3_43_triplet_pt3(a, i)
term(149) = term(149) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_cc3_45_triplet_pt3(a, i)
term(150) = term(150) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_cc3_46_triplet_pt3(a, i)
term(151) = term(151) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_cc3_47_triplet_pt3(a, i)
term(152) = term(152) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_cc3_43_triplet_pt3(a, i)
term(153) = term(153) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_cc3_45_triplet_pt3(a, i)
term(154) = term(154) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_cc3_46_triplet_pt3(a, i)
term(155) = term(155) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_cc3_47_triplet_pt3(a, i)
end do 
end do 

term(132) = term(132) * (-12.0d+0) 
term(133) = term(133) * 16.0d+0 
term(134) = term(134) * 4.0d+0 
term(135) = term(135) * (-8.0d+0) 
term(136) = term(136) * 6.0d+0 
term(137) = term(137) * (-8.0d+0) 
term(138) = term(138) * (-2.0d+0) 
term(139) = term(139) * 4.0d+0 
term(140) = term(140) * (-12.0d+0) 
term(141) = term(141) * 16.0d+0 
term(142) = term(142) * 4.0d+0 
term(143) = term(143) * (-8.0d+0) 
term(144) = term(144) * 12.0d+0 
term(145) = term(145) * (-4.0d+0) 
term(146) = term(146) * (-16.0d+0) 
term(147) = term(147) * 8.0d+0 
term(148) = term(148) * (-6.0d+0) 
term(149) = term(149) * 2.0d+0 
term(150) = term(150) * 8.0d+0 
term(151) = term(151) * (-4.0d+0) 
term(152) = term(152) * 12.0d+0 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (-16.0d+0) 
term(155) = term(155) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(156) = term(156) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_cc3_11_triplet_pt3(a, i, j, q)
term(157) = term(157) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_cc3_13_triplet_pt3(a, i, j, q)
term(158) = term(158) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_cc3_8_triplet_pt3(a, i, j, q)
term(159) = term(159) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_cc3_12_triplet_pt3(a, i, j, q)
term(160) = term(160) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_cc3_10_triplet_pt3(a, i, j, q)
term(161) = term(161) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_cc3_9_triplet_pt3(a, i, j, q)
term(162) = term(162) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_cc3_10_triplet_pt3(a, i, j, q)
term(163) = term(163) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_cc3_12_triplet_pt3(a, i, j, q)
term(164) = term(164) + r1(vrdav_Rl, a,j) * r1(vrdav_Rr, p,i) * wm_interm_cc3_24_triplet_pt3(a, i, j, q)
term(165) = term(165) + r1(vrdav_Rl, a,j) * r1(vrdav_Rr, p,i) * wm_interm_cc3_29_triplet_pt3(a, i, q, j)
term(166) = term(166) + r1(vrdav_Rl, a,j) * r1(vrdav_Rr, p,i) * wm_interm_cc3_30_triplet_pt3(a, i, q, j)
term(167) = term(167) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_39_triplet_pt3(a, i, j, q)
term(168) = term(168) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_42_triplet_pt3(a, i, j, q)
term(169) = term(169) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_41_triplet_pt3(a, i, j, q)
term(170) = term(170) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_38_triplet_pt3(a, i, j, q)
term(171) = term(171) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_36_triplet_pt3(a, i, j, q)
term(172) = term(172) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_40_triplet_pt3(a, i, j, q)
term(173) = term(173) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_36_triplet_pt3(a, i, j, q)
term(174) = term(174) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_40_triplet_pt3(a, i, j, q)
term(175) = term(175) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_42_triplet_pt3(a, i, j, q)
term(176) = term(176) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_41_triplet_pt3(a, i, j, q)
term(177) = term(177) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_39_triplet_pt3(a, i, j, q)
term(178) = term(178) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_38_triplet_pt3(a, i, j, q)
end do 
end do 
end do 

term(156) = term(156) * (-8.0d+0) 
term(157) = term(157) * 8.0d+0 
term(158) = term(158) * 6.0d+0 
term(159) = term(159) * (-8.0d+0) 
term(160) = term(160) * 6.0d+0 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * 12.0d+0 
term(163) = term(163) * (-16.0d+0) 
term(164) = term(164) * (-2.0d+0) 
term(165) = term(165) * (-2.0d+0) 
term(166) = term(166) * 3.9999999999999996d+0 
term(167) = term(167) * 4.0d+0 
term(168) = term(168) * (-4.0d+0) 
term(169) = term(169) * (-3.0d+0) 
term(170) = term(170) * 4.0d+0 
term(171) = term(171) * (-3.0d+0) 
term(172) = term(172) * 2.0d+0 
term(173) = term(173) * (-3.0d+0) 
term(174) = term(174) * 4.0d+0 
term(175) = term(175) * (-4.0d+0) 
term(176) = term(176) * (-3.0d+0) 
term(177) = term(177) * 2.0d+0 
term(178) = term(178) * 4.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(179) = term(179) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_cc3_9_triplet_pt3(a, j, i, q)
term(180) = term(180) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_cc3_10_triplet_pt3(a, j, i, q)
term(181) = term(181) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_cc3_8_triplet_pt3(a, j, i, q)
term(182) = term(182) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_cc3_12_triplet_pt3(a, j, i, q)
term(183) = term(183) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_36_triplet_pt3(a, q, j, i)
term(184) = term(184) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_38_triplet_pt3(a, q, j, i)
term(185) = term(185) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_39_triplet_pt3(a, j, q, i)
term(186) = term(186) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_36_triplet_pt3(a, j, q, i)
term(187) = term(187) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_40_triplet_pt3(a, j, q, i)
term(188) = term(188) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_41_triplet_pt3(a, q, j, i)
term(189) = term(189) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_41_triplet_pt3(a, j, q, i)
term(190) = term(190) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_40_triplet_pt3(a, q, j, i)
term(191) = term(191) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_42_triplet_pt3(a, j, q, i)
term(192) = term(192) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_42_triplet_pt3(a, q, j, i)
term(193) = term(193) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_39_triplet_pt3(a, q, j, i)
term(194) = term(194) + r1(vrdav_Rr, a,j) * s1(p,i) * wm_interm_cc3_38_triplet_pt3(a, j, q, i)
term(195) = term(195) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_42_triplet_pt3(a, j, i, q)
term(196) = term(196) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_39_triplet_pt3(a, j, i, q)
term(197) = term(197) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_41_triplet_pt3(a, j, i, q)
term(198) = term(198) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_40_triplet_pt3(a, j, i, q)
term(199) = term(199) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_36_triplet_pt3(a, j, i, q)
term(200) = term(200) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_cc3_38_triplet_pt3(a, j, i, q)
term(201) = term(201) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_40_triplet_pt3(a, j, i, q)
term(202) = term(202) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_42_triplet_pt3(a, j, i, q)
term(203) = term(203) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_39_triplet_pt3(a, j, i, q)
term(204) = term(204) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_36_triplet_pt3(a, j, i, q)
term(205) = term(205) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_41_triplet_pt3(a, j, i, q)
term(206) = term(206) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_cc3_38_triplet_pt3(a, j, i, q)
end do 
end do 
end do 

term(179) = term(179) * 12.0d+0 
term(180) = term(180) * (-8.0d+0) 
term(181) = term(181) * (-16.0d+0) 
term(182) = term(182) * 16.0d+0 
term(183) = term(183) * (-3.0d+0) 
term(184) = term(184) * 4.0d+0 
term(185) = term(185) * (-3.0d+0) 
term(186) = term(186) * 2.0d+0 
term(187) = term(187) * (-3.0d+0) 
term(188) = term(188) * (-3.0d+0) 
term(189) = term(189) * 4.0d+0 
term(190) = term(190) * 4.0d+0 
term(191) = term(191) * 4.0d+0 
term(192) = term(192) * (-4.0d+0) 
term(193) = term(193) * 2.0d+0 
term(194) = term(194) * (-4.0d+0) 
term(195) = term(195) * 4.0d+0 
term(196) = term(196) * (-3.0d+0) 
term(197) = term(197) * 2.0d+0 
term(198) = term(198) * (-3.0d+0) 
term(199) = term(199) * 4.0d+0 
term(200) = term(200) * (-4.0d+0) 
term(201) = term(201) * (-3.0d+0) 
term(202) = term(202) * 4.0d+0 
term(203) = term(203) * (-3.0d+0) 
term(204) = term(204) * 2.0d+0 
term(205) = term(205) * 4.0d+0 
term(206) = term(206) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(207) = term(207) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,i,j) * wm_interm_cc3_5_triplet_pt3(b, q, k, j)
term(208) = term(208) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,q,j) * wm_interm_cc3_5_triplet_pt3(b, i, k, j)
term(209) = term(209) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,q,j) * wm_interm_cc3_5_triplet_pt3(b, i, j, k)
end do 
end do 
end do 
end do 
end do 

term(207) = term(207) * (-2.0d+0) 
term(208) = term(208) * 4.0d+0 
term(209) = term(209) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(210) = term(210) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,i) * wm_interm_cc3_5_triplet_pt3(b, q, k, j)
term(211) = term(211) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,i) * wm_interm_cc3_5_triplet_pt3(b, q, j, k)
term(212) = term(212) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,k,p,i) * wm_interm_cc3_6_triplet_pt3(b, j, k, q)
term(213) = term(213) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,j,b,k,p,i) * wm_interm_cc3_6_triplet_pt3(b, i, k, q)
term(214) = term(214) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,k,p,i) * wm_interm_cc3_6_triplet_pt3(b, k, j, q)
term(215) = term(215) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,j,b,k,p,i) * wm_interm_cc3_6_triplet_pt3(b, k, i, q)
end do 
end do 
end do 
end do 
end do 

term(210) = term(210) * 3.9999999999999996d+0 
term(211) = term(211) * (-2.0d+0) 
term(212) = term(212) * (-6.0d+0) 
term(213) = term(213) * 4.0d+0 
term(214) = term(214) * 8.0d+0 
term(215) = term(215) * (-8.0d+0) 

do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(216) = term(216) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_cc3_5_triplet_pt3(b, i, k, j)
term(217) = term(217) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_cc3_5_triplet_pt3(b, i, j, k)
term(218) = term(218) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_cc3_28_triplet_pt3(b, i, k, j)
term(219) = term(219) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_cc3_28_triplet_pt3(b, i, j, k)
end do 
end do 
end do 
end do 
end do 

term(216) = term(216) * (-7.999999999999999d+0) 
term(217) = term(217) * 4.0d+0 
term(218) = term(218) * (-8.0d+0) 
term(219) = term(219) * 4.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(220) = term(220) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,k,j) * wm_interm_cc3_5_triplet_pt3(b, i, j, k)
term(221) = term(221) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,k,j) * wm_interm_cc3_5_triplet_pt3(b, i, k, j)
end do 
end do 
end do 
end do 
end do 

term(220) = term(220) * 3.9999999999999996d+0 
term(221) = term(221) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(222) = term(222) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_cc3_6_triplet_pt3(b, i, k, j)
term(223) = term(223) + r1(vrdav_Rl, b,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_cc3_6_triplet_pt3(a, i, k, j)
term(224) = term(224) + r1(vrdav_Rl, b,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_cc3_6_triplet_pt3(a, k, i, j)
term(225) = term(225) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_cc3_6_triplet_pt3(b, k, i, j)
end do 
end do 
end do 
end do 
end do 

term(222) = term(222) * (-6.0d+0) 
term(223) = term(223) * 4.0d+0 
term(224) = term(224) * (-6.0d+0) 
term(225) = term(225) * 8.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(226) = term(226) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,k,p,q) * wm_interm_cc3_6_triplet_pt3(b, j, k, i)
term(227) = term(227) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,k,p,q) * wm_interm_cc3_6_triplet_pt3(b, k, j, i)
end do 
end do 
end do 
end do 
end do 

term(226) = term(226) * 8.0d+0 
term(227) = term(227) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(228) = term(228) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,k,b,j,p,i) * wm_interm_cc3_6_triplet_pt3(b, i, k, q)
term(229) = term(229) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,k,b,j,p,i) * wm_interm_cc3_6_triplet_pt3(b, k, i, q)
term(230) = term(230) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,k) * wm_interm_cc3_35_triplet_pt3(b, k, i, j)
term(231) = term(231) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,k) * wm_interm_cc3_35_triplet_pt3(b, k, j, i)
term(232) = term(232) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,k) * wm_interm_cc3_35_triplet_pt3(a, k, j, i)
term(233) = term(233) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,k) * wm_interm_cc3_35_triplet_pt3(a, k, i, j)
end do 
end do 
end do 
end do 
end do 

term(228) = term(228) * (-6.0d+0) 
term(229) = term(229) * 8.0d+0 
term(230) = term(230) * (-6.0d+0) 
term(231) = term(231) * 8.0d+0 
term(232) = term(232) * (-6.0d+0) 
term(233) = term(233) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(234) = term(234) + r1(vrdav_Rl, b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_cc3_28_triplet_pt3(a, j, k, i)
term(235) = term(235) + r1(vrdav_Rl, b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_cc3_28_triplet_pt3(a, j, i, k)
term(236) = term(236) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_cc3_28_triplet_pt3(b, j, i, k)
term(237) = term(237) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_cc3_28_triplet_pt3(b, j, k, i)
end do 
end do 
end do 
end do 
end do 

term(234) = term(234) * 4.0d+0 
term(235) = term(235) * (-2.0d+0) 
term(236) = term(236) * 4.0d+0 
term(237) = term(237) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(238) = term(238) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,k) * wm_interm_cc3_35_triplet_pt3(a, k, j, i)
term(239) = term(239) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,k) * wm_interm_cc3_35_triplet_pt3(b, k, j, i)
end do 
end do 
end do 
end do 
end do 

term(238) = term(238) * 4.0d+0 
term(239) = term(239) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(240) = term(240) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,k) * wm_interm_cc3_35_triplet_pt3(b, k, i, j)
term(241) = term(241) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,k) * wm_interm_cc3_35_triplet_pt3(a, k, i, j)
end do 
end do 
end do 
end do 
end do 

term(240) = term(240) * 4.0d+0 
term(241) = term(241) * (-4.0d+0) 

term(242) = term(242) + wm_interm_cc3_20_triplet_pt3 * wm_interm_cc3_21_triplet_pt3(p, q)
term(243) = term(243) + wm_interm_cc3_20_triplet_pt3 * wm_interm_cc3_26_triplet_pt3(p, q)
term(244) = term(244) + wm_interm_cc3_20_triplet_pt3 * wm_interm_cc3_31_triplet_pt3(p, q)
term(245) = term(245) + wm_interm_cc3_20_triplet_pt3 * wm_interm_cc3_32_triplet_pt3(p, q)

term(242) = term(242) * 3.9999999999999996d+0 
term(243) = term(243) * (-7.999999999999999d+0) 
term(244) = term(244) * 7.999999999999999d+0 
term(245) = term(245) * (-3.9999999999999996d+0) 


    calc_D_vo_wm_triplet_cc3_pt3 = zero
    do s = 0, 245
    calc_D_vo_wm_triplet_cc3_pt3 = calc_D_vo_wm_triplet_cc3_pt3 + term(s)
    end do

    end function calc_D_vo_wm_triplet_cc3_pt3
    
    function calc_D_vv_wm_triplet_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_vv_wm_triplet_cc3_pt3 = zero
    do s = 0, 1
    calc_D_vv_wm_triplet_cc3_pt3 = calc_D_vv_wm_triplet_cc3_pt3 + term(s)
    end do

    end function calc_D_vv_wm_triplet_cc3_pt3
    

    

    
end module density_exc_exc_functions_cc3_triplet_pt0123
