module ss_ccsd_pt012
      use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

        implicit none
    !
    ! File generated automatically on 2018-04-18 11:07:38
        !

            real(F64), dimension(:, :), allocatable :: wm_interm_0_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_1_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_2_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_3_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_4_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_5_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_6_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_8_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_9_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_pt1 


    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_1_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_2_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_3_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_4_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_7_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_10_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_11_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_12_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_15_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_16_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_22_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_23_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_24_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_27_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_30_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_31_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_34_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_35_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_37_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_38_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_39_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_40_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_41_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_42_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_43_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_44_pt2 
real(F64) :: wm_interm_45_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_46_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_47_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_49_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_51_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_52_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_57_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_58_pt2 
real(F64) :: wm_interm_59_pt2 
real(F64) :: wm_interm_60_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_62_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63_pt2 


    contains
    
    subroutine wm_intermediates_ccsd_init_pt0(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    
    end subroutine wm_intermediates_ccsd_init_pt0
    
    subroutine wm_intermediates_ccsd_free_pt0
    
    end subroutine wm_intermediates_ccsd_free_pt0
    
    subroutine wm_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum

    

  end subroutine wm_intermediates_ccsd_pt0

      subroutine wm_intermediates_ccsd_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_3_pt1(1: nocc, 1: nocc))
allocate(wm_interm_4_pt1(1: nocc, 1: nocc))
allocate(wm_interm_5_pt1(1: nocc, 1: nocc))
allocate(wm_interm_6_pt1(1: nocc, 1: nocc))
allocate(wm_interm_7_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_pt1 = zero 
wm_interm_1_pt1 = zero 
wm_interm_2_pt1 = zero 
wm_interm_3_pt1 = zero 
wm_interm_4_pt1 = zero 
wm_interm_5_pt1 = zero 
wm_interm_6_pt1 = zero 
wm_interm_7_pt1 = zero 
wm_interm_8_pt1 = zero 
wm_interm_9_pt1 = zero 
wm_interm_10_pt1 = zero 

    end subroutine wm_intermediates_ccsd_init_pt1
    
    subroutine wm_intermediates_ccsd_free_pt1
    deallocate(wm_interm_0_pt1)
deallocate(wm_interm_1_pt1)
deallocate(wm_interm_2_pt1)
deallocate(wm_interm_3_pt1)
deallocate(wm_interm_4_pt1)
deallocate(wm_interm_5_pt1)
deallocate(wm_interm_6_pt1)
deallocate(wm_interm_7_pt1)
deallocate(wm_interm_8_pt1)
deallocate(wm_interm_9_pt1)
deallocate(wm_interm_10_pt1)

    end subroutine wm_intermediates_ccsd_free_pt1
    
    subroutine wm_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, b, j, k 

    !$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_0_pt1(b, j) = wm_interm_0_pt1(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_1_pt1(b, j) = wm_interm_1_pt1(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k)
end do 
wm_interm_2_pt1(b, i, j, k) = wm_interm_2_pt1(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_3_pt1(j, k) = wm_interm_3_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_4_pt1(j, k) = wm_interm_4_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,i,k)
end do 
end do 
end do 
wm_interm_5_pt1(j, k) = wm_interm_5_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_6_pt1(j, k) = wm_interm_6_pt1(j, k) + sum 
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
wm_interm_7_pt1(b, j, i, k) = wm_interm_7_pt1(b, j, i, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_8_pt1(b, j) = wm_interm_8_pt1(b, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_9_pt1(b, j) = wm_interm_9_pt1(b, j) + sum 
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
wm_interm_10_pt1(b, i, j, k) = wm_interm_10_pt1(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



end subroutine wm_intermediates_ccsd_pt1

    subroutine wm_intermediates_ccsd_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_pt2(1: nocc, 1: nocc))
allocate(wm_interm_2_pt2(1: nocc, 1: nocc))
allocate(wm_interm_3_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_4_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_5_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_6_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_7_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_8_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_9_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_10_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_11_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_12_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_13_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_14_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_16_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_17_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_18_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_19_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_20_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_21_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_pt2(1: nocc, 1: nocc))
allocate(wm_interm_23_pt2(1: nocc, 1: nocc))
allocate(wm_interm_24_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_25_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_26_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_28_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_29_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_31_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_32_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_33_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_34_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_35_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_36_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_37_pt2(1: nocc, 1: nocc))
allocate(wm_interm_38_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_39_pt2(1: nocc, 1: nocc))
allocate(wm_interm_40_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_41_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_42_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_43_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_44_pt2(1: nocc, 1: nocc))
allocate(wm_interm_46_pt2(1: nocc, 1: nocc))
allocate(wm_interm_47_pt2(1: nocc, 1: nocc))
allocate(wm_interm_48_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_49_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_50_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_51_pt2(1: nocc, 1: nocc))
allocate(wm_interm_52_pt2(1: nocc, 1: nocc))
allocate(wm_interm_53_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_54_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_55_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_56_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_57_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_58_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_61_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_62_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_63_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_pt2 = zero 
wm_interm_1_pt2 = zero 
wm_interm_2_pt2 = zero 
wm_interm_3_pt2 = zero 
wm_interm_4_pt2 = zero 
wm_interm_5_pt2 = zero 
wm_interm_6_pt2 = zero 
wm_interm_7_pt2 = zero 
wm_interm_8_pt2 = zero 
wm_interm_9_pt2 = zero 
wm_interm_10_pt2 = zero 
wm_interm_11_pt2 = zero 
wm_interm_12_pt2 = zero 
wm_interm_13_pt2 = zero 
wm_interm_14_pt2 = zero 
wm_interm_15_pt2 = zero 
wm_interm_16_pt2 = zero 
wm_interm_17_pt2 = zero 
wm_interm_18_pt2 = zero 
wm_interm_19_pt2 = zero 
wm_interm_20_pt2 = zero 
wm_interm_21_pt2 = zero 
wm_interm_22_pt2 = zero 
wm_interm_23_pt2 = zero 
wm_interm_24_pt2 = zero 
wm_interm_25_pt2 = zero 
wm_interm_26_pt2 = zero 
wm_interm_27_pt2 = zero 
wm_interm_28_pt2 = zero 
wm_interm_29_pt2 = zero 
wm_interm_30_pt2 = zero 
wm_interm_31_pt2 = zero 
wm_interm_32_pt2 = zero 
wm_interm_33_pt2 = zero 
wm_interm_34_pt2 = zero 
wm_interm_35_pt2 = zero 
wm_interm_36_pt2 = zero 
wm_interm_37_pt2 = zero 
wm_interm_38_pt2 = zero 
wm_interm_39_pt2 = zero 
wm_interm_40_pt2 = zero 
wm_interm_41_pt2 = zero 
wm_interm_42_pt2 = zero 
wm_interm_43_pt2 = zero 
wm_interm_44_pt2 = zero 
wm_interm_45_pt2 = zero 
wm_interm_46_pt2 = zero 
wm_interm_47_pt2 = zero 
wm_interm_48_pt2 = zero 
wm_interm_49_pt2 = zero 
wm_interm_50_pt2 = zero 
wm_interm_51_pt2 = zero 
wm_interm_52_pt2 = zero 
wm_interm_53_pt2 = zero 
wm_interm_54_pt2 = zero 
wm_interm_55_pt2 = zero 
wm_interm_56_pt2 = zero 
wm_interm_57_pt2 = zero 
wm_interm_58_pt2 = zero 
wm_interm_59_pt2 = zero 
wm_interm_60_pt2 = zero 
wm_interm_61_pt2 = zero 
wm_interm_62_pt2 = zero 
wm_interm_63_pt2 = zero 

    end subroutine wm_intermediates_ccsd_init_pt2
    
    subroutine wm_intermediates_ccsd_free_pt2
    deallocate(wm_interm_0_pt2)
deallocate(wm_interm_1_pt2)
deallocate(wm_interm_2_pt2)
deallocate(wm_interm_3_pt2)
deallocate(wm_interm_4_pt2)
deallocate(wm_interm_5_pt2)
deallocate(wm_interm_6_pt2)
deallocate(wm_interm_7_pt2)
deallocate(wm_interm_8_pt2)
deallocate(wm_interm_9_pt2)
deallocate(wm_interm_10_pt2)
deallocate(wm_interm_11_pt2)
deallocate(wm_interm_12_pt2)
deallocate(wm_interm_13_pt2)
deallocate(wm_interm_14_pt2)
deallocate(wm_interm_15_pt2)
deallocate(wm_interm_16_pt2)
deallocate(wm_interm_17_pt2)
deallocate(wm_interm_18_pt2)
deallocate(wm_interm_19_pt2)
deallocate(wm_interm_20_pt2)
deallocate(wm_interm_21_pt2)
deallocate(wm_interm_22_pt2)
deallocate(wm_interm_23_pt2)
deallocate(wm_interm_24_pt2)
deallocate(wm_interm_25_pt2)
deallocate(wm_interm_26_pt2)
deallocate(wm_interm_27_pt2)
deallocate(wm_interm_28_pt2)
deallocate(wm_interm_29_pt2)
deallocate(wm_interm_30_pt2)
deallocate(wm_interm_31_pt2)
deallocate(wm_interm_32_pt2)
deallocate(wm_interm_33_pt2)
deallocate(wm_interm_34_pt2)
deallocate(wm_interm_35_pt2)
deallocate(wm_interm_36_pt2)
deallocate(wm_interm_37_pt2)
deallocate(wm_interm_38_pt2)
deallocate(wm_interm_39_pt2)
deallocate(wm_interm_40_pt2)
deallocate(wm_interm_41_pt2)
deallocate(wm_interm_42_pt2)
deallocate(wm_interm_43_pt2)
deallocate(wm_interm_44_pt2)
deallocate(wm_interm_46_pt2)
deallocate(wm_interm_47_pt2)
deallocate(wm_interm_48_pt2)
deallocate(wm_interm_49_pt2)
deallocate(wm_interm_50_pt2)
deallocate(wm_interm_51_pt2)
deallocate(wm_interm_52_pt2)
deallocate(wm_interm_53_pt2)
deallocate(wm_interm_54_pt2)
deallocate(wm_interm_55_pt2)
deallocate(wm_interm_56_pt2)
deallocate(wm_interm_57_pt2)
deallocate(wm_interm_58_pt2)
deallocate(wm_interm_61_pt2)
deallocate(wm_interm_62_pt2)
deallocate(wm_interm_63_pt2)

    end subroutine wm_intermediates_ccsd_free_pt2
    
    subroutine wm_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, j, k, c, l 

    !$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k)
end do 
wm_interm_0_pt2(b, i, j, k) = wm_interm_0_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_1_pt2(j, k) = wm_interm_1_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_2_pt2(j, k) = wm_interm_2_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_3_pt2(b, c) = wm_interm_3_pt2(b, c) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,i,j)
end do 
end do 
wm_interm_4_pt2(b, j) = wm_interm_4_pt2(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,k,i)
end do 
end do 
wm_interm_5_pt2(b, c, j, k) = wm_interm_5_pt2(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,c,k,i)
end do 
end do 
wm_interm_6_pt2(b, c, j, k) = wm_interm_6_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_7_pt2(b, c) = wm_interm_7_pt2(b, c) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_8_pt2(b, c, j, k) = wm_interm_8_pt2(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,c,i,k)
end do 
end do 
wm_interm_9_pt2(b, c, j, k) = wm_interm_9_pt2(b, c, j, k) + sum 
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
wm_interm_10_pt2(b, j) = wm_interm_10_pt2(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_11_pt2(b, j) = wm_interm_11_pt2(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_12_pt2(b, j) = wm_interm_12_pt2(b, j) + sum 
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
wm_interm_13_pt2(b, i, j, k) = wm_interm_13_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_14_pt2(i, j, k, l) = wm_interm_14_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_15_pt2(b, c) = wm_interm_15_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_16_pt2(b, c) = wm_interm_16_pt2(b, c) + sum 
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
sum = sum + s2(a,b,j,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_17_pt2(b, c, j, k) = wm_interm_17_pt2(b, c, j, k) + sum 
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
sum = sum + s2(a,b,j,i) * t2(a,c,k,i)
end do 
end do 
wm_interm_18_pt2(b, c, j, k) = wm_interm_18_pt2(b, c, j, k) + sum 
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
sum = sum + s2(a,b,i,j) * t2(a,c,k,i)
end do 
end do 
wm_interm_19_pt2(b, c, j, k) = wm_interm_19_pt2(b, c, j, k) + sum 
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
sum = sum + s2(a,b,i,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_20_pt2(b, c, j, k) = wm_interm_20_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_21_pt2(i, j, k, l) = wm_interm_21_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_22_pt2(j, k) = wm_interm_22_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_23_pt2(j, k) = wm_interm_23_pt2(j, k) + sum 
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
wm_interm_24_pt2(b, j) = wm_interm_24_pt2(b, j) + sum 
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
wm_interm_25_pt2(b, j, i, k) = wm_interm_25_pt2(b, j, i, k) + sum 
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
wm_interm_26_pt2(b, i, j, k) = wm_interm_26_pt2(b, i, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_27_pt2(b, j) = wm_interm_27_pt2(b, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_28_pt2(b, c, j, k) = wm_interm_28_pt2(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,k)
end do 
wm_interm_29_pt2(b, i, j, k) = wm_interm_29_pt2(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,i)
end do 
end do 
wm_interm_30_pt2(b, j) = wm_interm_30_pt2(b, j) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,i,j)
end do 
end do 
wm_interm_31_pt2(b, j) = wm_interm_31_pt2(b, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_32_pt2(b, c, j, k) = wm_interm_32_pt2(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,k,i)
end do 
end do 
wm_interm_33_pt2(b, c, j, k) = wm_interm_33_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_34_pt2(b, c) = wm_interm_34_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,j)
end do 
end do 
end do 
wm_interm_35_pt2(b, c) = wm_interm_35_pt2(b, c) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,k,i)
end do 
end do 
wm_interm_36_pt2(b, c, j, k) = wm_interm_36_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,i,k)
end do 
end do 
end do 
wm_interm_37_pt2(j, k) = wm_interm_37_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_38_pt2(i, j, k, l) = wm_interm_38_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_39_pt2(j, k) = wm_interm_39_pt2(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s1(a,i)
end do 
end do 
wm_interm_40_pt2(b, j) = wm_interm_40_pt2(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_41_pt2(b, j) = wm_interm_41_pt2(b, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,i)
end do 
end do 
wm_interm_42_pt2(b, j) = wm_interm_42_pt2(b, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_43_pt2(b, j) = wm_interm_43_pt2(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wm_interm_44_pt2(i, j) = wm_interm_44_pt2(i, j) + sum 
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
wm_interm_45_pt2 = wm_interm_45_pt2 + sum 
!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_46_pt2(i, j) = wm_interm_46_pt2(i, j) + sum 
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
wm_interm_47_pt2(i, j) = wm_interm_47_pt2(i, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,j)
wm_interm_48_pt2(a, b, i, j) = wm_interm_48_pt2(a, b, i, j) + sum 
end do 
end do 
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
wm_interm_49_pt2(a, b) = wm_interm_49_pt2(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_50_pt2(i, j, k, l) = wm_interm_50_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_51_pt2(j, k) = wm_interm_51_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_52_pt2(j, k) = wm_interm_52_pt2(j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_53_pt2(b, c, j, k) = wm_interm_53_pt2(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_54_pt2(b, c, j, k) = wm_interm_54_pt2(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_55_pt2(b, c, j, k) = wm_interm_55_pt2(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_56_pt2(b, c, j, k) = wm_interm_56_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_57_pt2(b, c) = wm_interm_57_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_58_pt2(b, c) = wm_interm_58_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_59_pt2 = wm_interm_59_pt2 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_60_pt2 = wm_interm_60_pt2 + sum 
!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_61_pt2(b, j, i, k) = wm_interm_61_pt2(b, j, i, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_62_pt2(b, i, j, k) = wm_interm_62_pt2(b, i, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t1(a,k)
end do 
wm_interm_63_pt2(b, j, i, k) = wm_interm_63_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_intermediates_ccsd_pt2

        function calc_D_oo_wm_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, j 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,p,b,i) * r2(vrdav_Rr, a,i,b,q)
term(1) = term(1) + r2(vrdav_Rl, a,p,b,i) * r2(vrdav_Rr, a,q,b,i)
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(2) = term(2) + r1(vrdav_Rl, a,p) * r1(vrdav_Rr, a,q)
end do 

term(2) = term(2) * (-2.0d+0) 


    calc_D_oo_wm_pt0 = zero
    do s = 0, 2
    calc_D_oo_wm_pt0 = calc_D_oo_wm_pt0 + term(s)
    end do

    end function calc_D_oo_wm_pt0
    
    function calc_D_ov_wm_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_pt0
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
    
    calc_D_ov_wm_pt0 = zero
    do s = 0, 0
    calc_D_ov_wm_pt0 = calc_D_ov_wm_pt0 + term(s)
    end do

    end function calc_D_ov_wm_pt0
    
    function calc_D_vo_wm_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, a, b, j 
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,q,p,i)
term(1) = term(1) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,p,q)
term(2) = term(2) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
term(3) = term(3) + r2(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, a,i)
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 


    calc_D_vo_wm_pt0 = zero
    do s = 0, 3
    calc_D_vo_wm_pt0 = calc_D_vo_wm_pt0 + term(s)
    end do

    end function calc_D_vo_wm_pt0
    
    function calc_D_vv_wm_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b 
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,j,p,i) * r2(vrdav_Rr, a,i,q,j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1) = term(1) + r2(vrdav_Rl, a,j,p,i) * r2(vrdav_Rr, a,j,q,i)
end do 
end do 
end do 

term(1) = term(1) * (4.0d+0) 

do i = 1, nocc 
term(2) = term(2) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, q,i)
end do 

term(2) = term(2) * (2.0d+0) 


    calc_D_vv_wm_pt0 = zero
    do s = 0, 2
    calc_D_vv_wm_pt0 = calc_D_vv_wm_pt0 + term(s)
    end do

    end function calc_D_vv_wm_pt0

        
    function calc_D_oo_wm_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_pt1
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
    
    calc_D_oo_wm_pt1 = zero
    do s = 0, 0
    calc_D_oo_wm_pt1 = calc_D_oo_wm_pt1 + term(s)
    end do

    end function calc_D_oo_wm_pt1
    
    function calc_D_ov_wm_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, b 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_2_pt1(a,p,j,i)
term(1) = term(1) + t2(a,q,j,i) * wm_interm_7_pt1(a,i,j,p)
term(2) = term(2) + t2(a,q,j,i) * wm_interm_7_pt1(a,j,i,p)
term(3) = term(3) + t2(a,q,j,i) * wm_interm_10_pt1(a,j,i,p)
term(4) = term(4) + t2(a,q,j,i) * wm_interm_10_pt1(a,i,j,p)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(5) = term(5) + s2(a,q,j,i) * wm_interm_2_pt1(a,p,i,j)
end do 
end do 
end do 

term(5) = term(5) * (4.0d+0) 


    calc_D_ov_wm_pt1 = zero
    do s = 0, 5
    calc_D_ov_wm_pt1 = calc_D_ov_wm_pt1 + term(s)
    end do

    end function calc_D_ov_wm_pt1
    
    function calc_D_vo_wm_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, a, b, j 
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + r1(vrdav_Rl, p,i) * wm_interm_3_pt1(i,q)
term(1) = term(1) + r1(vrdav_Rl, p,i) * wm_interm_4_pt1(i,q)
term(2) = term(2) + r1(vrdav_Rr, p,i) * wm_interm_5_pt1(i,q)
term(3) = term(3) + r1(vrdav_Rr, p,i) * wm_interm_6_pt1(i,q)
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + s2(a,p,q,i) * wm_interm_0_pt1(a,i)
term(5) = term(5) + s2(a,p,q,i) * wm_interm_1_pt1(a,i)
term(6) = term(6) + s2(a,p,i,q) * wm_interm_0_pt1(a,i)
term(7) = term(7) + s2(a,p,i,q) * wm_interm_1_pt1(a,i)
term(8) = term(8) + t2(a,p,i,q) * wm_interm_8_pt1(a,i)
term(9) = term(9) + t2(a,p,i,q) * wm_interm_9_pt1(a,i)
term(10) = term(10) + t2(a,p,q,i) * wm_interm_8_pt1(a,i)
term(11) = term(11) + t2(a,p,q,i) * wm_interm_9_pt1(a,i)
end do 
end do 

term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (4.0d+0) 
term(7) = term(7) * (-8.0d+0) 
term(8) = term(8) * (4.0d+0) 
term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (4.0d+0) 


    calc_D_vo_wm_pt1 = zero
    do s = 0, 11
    calc_D_vo_wm_pt1 = calc_D_vo_wm_pt1 + term(s)
    end do

    end function calc_D_vo_wm_pt1
    
    function calc_D_vv_wm_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_pt1
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
    
    calc_D_vv_wm_pt1 = zero
    do s = 0, 0
    calc_D_vv_wm_pt1 = calc_D_vv_wm_pt1 + term(s)
    end do

    end function calc_D_vv_wm_pt1
    
    
    function calc_D_oo_wm_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, k, j, l 
    real(F64), dimension(0:237) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_28_pt2(a,b,q,i) * wm_interm_5_pt2(a,b,p,i)
term(1) = term(1) + wm_interm_28_pt2(a,b,q,i) * wm_interm_6_pt2(a,b,p,i)
term(2) = term(2) + wm_interm_28_pt2(a,b,q,i) * wm_interm_8_pt2(a,b,p,i)
term(3) = term(3) + wm_interm_28_pt2(a,b,q,i) * wm_interm_9_pt2(a,b,p,i)
term(4) = term(4) + wm_interm_17_pt2(a,b,q,i) * wm_interm_55_pt2(b,a,i,p)
term(5) = term(5) + wm_interm_17_pt2(a,b,q,i) * wm_interm_56_pt2(b,a,i,p)
term(6) = term(6) + wm_interm_20_pt2(a,b,q,i) * wm_interm_55_pt2(b,a,i,p)
term(7) = term(7) + wm_interm_20_pt2(a,b,q,i) * wm_interm_56_pt2(b,a,i,p)
term(8) = term(8) + wm_interm_32_pt2(a,b,q,i) * wm_interm_5_pt2(a,b,p,i)
term(9) = term(9) + wm_interm_32_pt2(a,b,q,i) * wm_interm_6_pt2(a,b,p,i)
term(10) = term(10) + wm_interm_32_pt2(a,b,q,i) * wm_interm_8_pt2(a,b,p,i)
term(11) = term(11) + wm_interm_32_pt2(a,b,q,i) * wm_interm_9_pt2(a,b,p,i)
term(12) = term(12) + wm_interm_17_pt2(a,b,q,i) * wm_interm_54_pt2(b,a,i,p)
term(13) = term(13) + wm_interm_17_pt2(a,b,q,i) * wm_interm_53_pt2(b,a,i,p)
term(14) = term(14) + wm_interm_20_pt2(a,b,q,i) * wm_interm_54_pt2(b,a,i,p)
term(15) = term(15) + wm_interm_20_pt2(a,b,q,i) * wm_interm_53_pt2(b,a,i,p)
term(16) = term(16) + wm_interm_33_pt2(a,b,q,i) * wm_interm_8_pt2(a,b,p,i)
term(17) = term(17) + wm_interm_33_pt2(a,b,q,i) * wm_interm_9_pt2(a,b,p,i)
term(18) = term(18) + wm_interm_19_pt2(a,b,q,i) * wm_interm_54_pt2(b,a,i,p)
term(19) = term(19) + wm_interm_19_pt2(a,b,q,i) * wm_interm_53_pt2(b,a,i,p)
term(20) = term(20) + wm_interm_18_pt2(a,b,q,i) * wm_interm_53_pt2(b,a,i,p)
term(21) = term(21) + wm_interm_18_pt2(a,b,q,i) * wm_interm_54_pt2(b,a,i,p)
term(22) = term(22) + wm_interm_33_pt2(a,b,q,i) * wm_interm_6_pt2(a,b,p,i)
term(23) = term(23) + wm_interm_33_pt2(a,b,q,i) * wm_interm_5_pt2(a,b,p,i)
term(24) = term(24) + wm_interm_36_pt2(a,b,q,i) * wm_interm_5_pt2(a,b,p,i)
term(25) = term(25) + wm_interm_36_pt2(a,b,q,i) * wm_interm_6_pt2(a,b,p,i)
term(26) = term(26) + wm_interm_36_pt2(a,b,q,i) * wm_interm_8_pt2(a,b,p,i)
term(27) = term(27) + wm_interm_36_pt2(a,b,q,i) * wm_interm_9_pt2(a,b,p,i)
term(28) = term(28) + wm_interm_18_pt2(a,b,q,i) * wm_interm_55_pt2(b,a,i,p)
term(29) = term(29) + wm_interm_18_pt2(a,b,q,i) * wm_interm_56_pt2(b,a,i,p)
term(30) = term(30) + wm_interm_19_pt2(a,b,q,i) * wm_interm_55_pt2(b,a,i,p)
term(31) = term(31) + wm_interm_19_pt2(a,b,q,i) * wm_interm_56_pt2(b,a,i,p)
term(32) = term(32) + wm_interm_17_pt2(a,b,i,q) * wm_interm_54_pt2(b,a,p,i)
term(33) = term(33) + wm_interm_17_pt2(a,b,i,q) * wm_interm_53_pt2(b,a,p,i)
term(34) = term(34) + wm_interm_17_pt2(a,b,i,q) * wm_interm_55_pt2(b,a,p,i)
term(35) = term(35) + wm_interm_17_pt2(a,b,i,q) * wm_interm_56_pt2(b,a,p,i)
term(36) = term(36) + wm_interm_36_pt2(a,b,i,q) * wm_interm_5_pt2(a,b,i,p)
term(37) = term(37) + wm_interm_36_pt2(a,b,i,q) * wm_interm_6_pt2(a,b,i,p)
term(38) = term(38) + wm_interm_33_pt2(a,b,i,q) * wm_interm_6_pt2(a,b,i,p)
term(39) = term(39) + wm_interm_33_pt2(a,b,i,q) * wm_interm_5_pt2(a,b,i,p)
term(40) = term(40) + wm_interm_36_pt2(a,b,i,q) * wm_interm_8_pt2(a,b,i,p)
term(41) = term(41) + wm_interm_36_pt2(a,b,i,q) * wm_interm_9_pt2(a,b,i,p)
term(42) = term(42) + wm_interm_33_pt2(a,b,i,q) * wm_interm_8_pt2(a,b,i,p)
term(43) = term(43) + wm_interm_33_pt2(a,b,i,q) * wm_interm_9_pt2(a,b,i,p)
term(44) = term(44) + wm_interm_20_pt2(a,b,i,q) * wm_interm_54_pt2(b,a,p,i)
term(45) = term(45) + wm_interm_20_pt2(a,b,i,q) * wm_interm_53_pt2(b,a,p,i)
term(46) = term(46) + wm_interm_20_pt2(a,b,i,q) * wm_interm_55_pt2(b,a,p,i)
term(47) = term(47) + wm_interm_20_pt2(a,b,i,q) * wm_interm_56_pt2(b,a,p,i)
term(48) = term(48) + wm_interm_19_pt2(a,b,i,q) * wm_interm_54_pt2(b,a,p,i)
term(49) = term(49) + wm_interm_19_pt2(a,b,i,q) * wm_interm_53_pt2(b,a,p,i)
term(50) = term(50) + wm_interm_19_pt2(a,b,i,q) * wm_interm_55_pt2(b,a,p,i)
term(51) = term(51) + wm_interm_19_pt2(a,b,i,q) * wm_interm_56_pt2(b,a,p,i)
term(52) = term(52) + wm_interm_18_pt2(a,b,i,q) * wm_interm_55_pt2(b,a,p,i)
term(53) = term(53) + wm_interm_18_pt2(a,b,i,q) * wm_interm_56_pt2(b,a,p,i)
term(54) = term(54) + wm_interm_18_pt2(a,b,i,q) * wm_interm_53_pt2(b,a,p,i)
term(55) = term(55) + wm_interm_18_pt2(a,b,i,q) * wm_interm_54_pt2(b,a,p,i)
term(56) = term(56) + wm_interm_32_pt2(a,b,i,q) * wm_interm_5_pt2(a,b,i,p)
term(57) = term(57) + wm_interm_32_pt2(a,b,i,q) * wm_interm_6_pt2(a,b,i,p)
term(58) = term(58) + wm_interm_28_pt2(a,b,i,q) * wm_interm_5_pt2(a,b,i,p)
term(59) = term(59) + wm_interm_28_pt2(a,b,i,q) * wm_interm_6_pt2(a,b,i,p)
term(60) = term(60) + wm_interm_32_pt2(a,b,i,q) * wm_interm_8_pt2(a,b,i,p)
term(61) = term(61) + wm_interm_32_pt2(a,b,i,q) * wm_interm_9_pt2(a,b,i,p)
term(62) = term(62) + wm_interm_28_pt2(a,b,i,q) * wm_interm_8_pt2(a,b,i,p)
term(63) = term(63) + wm_interm_28_pt2(a,b,i,q) * wm_interm_9_pt2(a,b,i,p)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * (8.0d+0) 
term(2) = term(2) * (8.0d+0) 
term(3) = term(3) * (-16.0d+0) 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * (8.0d+0) 
term(6) = term(6) * (8.0d+0) 
term(7) = term(7) * (-16.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (8.0d+0) 
term(12) = term(12) * (2.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * (8.0d+0) 
term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * (2.0d+0) 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (8.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * (8.0d+0) 
term(32) = term(32) * (2.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (-4.0d+0) 
term(35) = term(35) * (8.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-4.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (8.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (8.0d+0) 
term(46) = term(46) * (8.0d+0) 
term(47) = term(47) * (-16.0d+0) 
term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * (8.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-4.0d+0) 
term(56) = term(56) * (2.0d+0) 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (8.0d+0) 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * (8.0d+0) 
term(62) = term(62) * (8.0d+0) 
term(63) = term(63) * (-16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(64) = term(64) + wm_interm_1_pt2(i,j) * wm_interm_50_pt2(q,j,i,p)
term(65) = term(65) + wm_interm_1_pt2(i,j) * wm_interm_50_pt2(q,j,p,i)
term(66) = term(66) + wm_interm_2_pt2(i,j) * wm_interm_50_pt2(q,j,i,p)
term(67) = term(67) + wm_interm_2_pt2(i,j) * wm_interm_50_pt2(q,j,p,i)
term(68) = term(68) + wm_interm_1_pt2(i,j) * wm_interm_50_pt2(j,q,p,i)
term(69) = term(69) + wm_interm_2_pt2(i,j) * wm_interm_50_pt2(j,q,p,i)
term(70) = term(70) + wm_interm_1_pt2(i,j) * wm_interm_50_pt2(j,q,i,p)
term(71) = term(71) + wm_interm_2_pt2(i,j) * wm_interm_50_pt2(j,q,i,p)
end do 
end do 

term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(69) = term(69) * (-2.0d+0) 
term(70) = term(70) * (-2.0d+0) 
term(71) = term(71) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(72) = term(72) + wm_interm_21_pt2(i,j,k,l) * wm_interm_38_pt2(i,j,l,k)
term(73) = term(73) + wm_interm_21_pt2(i,j,k,l) * wm_interm_38_pt2(i,j,k,l)
end do 
end do 
end do 
end do 

term(72) = term(72) * (-2.0d+0) 
term(73) = term(73) * (4.0d+0) 

do a = nocc + 1, nactive 
term(74) = term(74) + r1(vrdav_Rl, a,q) * wm_interm_40_pt2(a,p)
term(75) = term(75) + r1(vrdav_Rl, a,q) * wm_interm_41_pt2(a,p)
term(76) = term(76) + s1(a,q) * wm_interm_11_pt2(a,p)
term(77) = term(77) + s1(a,q) * wm_interm_12_pt2(a,p)
term(78) = term(78) + r1(vrdav_Rr, a,p) * wm_interm_42_pt2(a,q)
term(79) = term(79) + r1(vrdav_Rr, a,p) * wm_interm_43_pt2(a,q)
term(80) = term(80) + t1(a,q) * wm_interm_24_pt2(a,p)
term(81) = term(81) + t1(a,q) * wm_interm_27_pt2(a,p)
term(82) = term(82) + wm_interm_10_pt2(a,q) * wm_interm_30_pt2(a,p)
term(83) = term(83) + wm_interm_10_pt2(a,q) * wm_interm_31_pt2(a,p)
term(84) = term(84) + wm_interm_30_pt2(a,p) * wm_interm_4_pt2(a,q)
term(85) = term(85) + wm_interm_31_pt2(a,p) * wm_interm_4_pt2(a,q)
end do 

term(74) = term(74) * (-4.0d+0) 
term(75) = term(75) * (2.0d+0) 
term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * (4.0d+0) 
term(78) = term(78) * (4.0d+0) 
term(79) = term(79) * (-2.0d+0) 
term(80) = term(80) * (-2.0d+0) 
term(81) = term(81) * (4.0d+0) 
term(82) = term(82) * (-2.0d+0) 
term(83) = term(83) * (4.0d+0) 
term(84) = term(84) * (4.0d+0) 
term(85) = term(85) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(86) = term(86) + wm_interm_1_pt2(i,j) * wm_interm_44_pt2(j,i)
term(87) = term(87) + wm_interm_2_pt2(i,j) * wm_interm_44_pt2(j,i)
term(88) = term(88) + wm_interm_14_pt2(p,i,j,q) * wm_interm_44_pt2(j,i)
term(89) = term(89) + wm_interm_14_pt2(i,p,j,q) * wm_interm_44_pt2(j,i)
term(90) = term(90) + wm_interm_1_pt2(i,j) * wm_interm_51_pt2(j,i)
term(91) = term(91) + wm_interm_1_pt2(i,j) * wm_interm_52_pt2(j,i)
term(92) = term(92) + wm_interm_2_pt2(i,j) * wm_interm_51_pt2(j,i)
term(93) = term(93) + wm_interm_2_pt2(i,j) * wm_interm_52_pt2(j,i)
term(94) = term(94) + wm_interm_21_pt2(i,p,q,j) * wm_interm_37_pt2(i,j)
term(95) = term(95) + wm_interm_21_pt2(i,p,j,q) * wm_interm_37_pt2(i,j)
term(96) = term(96) + wm_interm_22_pt2(i,j) * wm_interm_37_pt2(i,j)
term(97) = term(97) + wm_interm_23_pt2(i,j) * wm_interm_37_pt2(i,j)
term(98) = term(98) + wm_interm_21_pt2(i,p,q,j) * wm_interm_39_pt2(i,j)
term(99) = term(99) + wm_interm_21_pt2(i,p,j,q) * wm_interm_39_pt2(i,j)
term(100) = term(100) + wm_interm_22_pt2(i,j) * wm_interm_39_pt2(i,j)
term(101) = term(101) + wm_interm_23_pt2(i,j) * wm_interm_39_pt2(i,j)
term(102) = term(102) + wm_interm_14_pt2(p,i,j,q) * wm_interm_51_pt2(j,i)
term(103) = term(103) + wm_interm_14_pt2(p,i,j,q) * wm_interm_52_pt2(j,i)
term(104) = term(104) + wm_interm_14_pt2(i,p,j,q) * wm_interm_51_pt2(j,i)
term(105) = term(105) + wm_interm_14_pt2(i,p,j,q) * wm_interm_52_pt2(j,i)
term(106) = term(106) + wm_interm_22_pt2(i,j) * wm_interm_38_pt2(i,p,q,j)
term(107) = term(107) + wm_interm_22_pt2(i,j) * wm_interm_38_pt2(p,i,q,j)
term(108) = term(108) + wm_interm_22_pt2(i,j) * wm_interm_38_pt2(p,i,j,q)
term(109) = term(109) + wm_interm_22_pt2(i,j) * wm_interm_38_pt2(i,p,j,q)
term(110) = term(110) + wm_interm_23_pt2(i,j) * wm_interm_38_pt2(i,p,q,j)
term(111) = term(111) + wm_interm_23_pt2(i,j) * wm_interm_38_pt2(p,i,q,j)
term(112) = term(112) + wm_interm_23_pt2(i,j) * wm_interm_38_pt2(p,i,j,q)
term(113) = term(113) + wm_interm_23_pt2(i,j) * wm_interm_38_pt2(i,p,j,q)
end do 
end do 

term(86) = term(86) * (4.0d+0) 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(89) = term(89) * (4.0d+0) 
term(90) = term(90) * (-4.0d+0) 
term(91) = term(91) * (8.0d+0) 
term(92) = term(92) * (8.0d+0) 
term(93) = term(93) * (-16.0d+0) 
term(94) = term(94) * (2.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (-4.0d+0) 
term(97) = term(97) * (8.0d+0) 
term(98) = term(98) * (-4.0d+0) 
term(99) = term(99) * (8.0d+0) 
term(100) = term(100) * (8.0d+0) 
term(101) = term(101) * (-16.0d+0) 
term(102) = term(102) * (2.0d+0) 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * (8.0d+0) 
term(107) = term(107) * (-2.0d+0) 
term(109) = term(109) * (-2.0d+0) 
term(110) = term(110) * (-2.0d+0) 
term(111) = term(111) * (4.0d+0) 
term(112) = term(112) * (-2.0d+0) 
term(113) = term(113) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(114) = term(114) + wm_interm_14_pt2(i,j,k,q) * wm_interm_50_pt2(k,p,j,i)
term(115) = term(115) + wm_interm_14_pt2(i,j,k,q) * wm_interm_50_pt2(p,k,j,i)
end do 
end do 
end do 

term(115) = term(115) * (-2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(116) = term(116) + wm_interm_3_pt2(a,b) * wm_interm_48_pt2(b,a,q,p)
term(117) = term(117) + wm_interm_3_pt2(a,b) * wm_interm_49_pt2(b,a)
term(118) = term(118) + wm_interm_48_pt2(a,b,q,p) * wm_interm_7_pt2(b,a)
term(119) = term(119) + wm_interm_49_pt2(a,b) * wm_interm_7_pt2(b,a)
term(120) = term(120) + wm_interm_17_pt2(a,b,p,q) * wm_interm_49_pt2(b,a)
term(121) = term(121) + wm_interm_20_pt2(a,b,p,q) * wm_interm_49_pt2(b,a)
term(122) = term(122) + wm_interm_19_pt2(a,b,p,q) * wm_interm_49_pt2(b,a)
term(123) = term(123) + wm_interm_18_pt2(a,b,p,q) * wm_interm_49_pt2(b,a)
term(124) = term(124) + wm_interm_3_pt2(a,b) * wm_interm_53_pt2(b,a,q,p)
term(125) = term(125) + wm_interm_3_pt2(a,b) * wm_interm_54_pt2(b,a,q,p)
term(126) = term(126) + wm_interm_53_pt2(a,b,q,p) * wm_interm_7_pt2(b,a)
term(127) = term(127) + wm_interm_54_pt2(a,b,q,p) * wm_interm_7_pt2(b,a)
term(128) = term(128) + wm_interm_3_pt2(a,b) * wm_interm_55_pt2(b,a,q,p)
term(129) = term(129) + wm_interm_3_pt2(a,b) * wm_interm_56_pt2(b,a,q,p)
term(130) = term(130) + wm_interm_3_pt2(a,b) * wm_interm_57_pt2(b,a)
term(131) = term(131) + wm_interm_3_pt2(a,b) * wm_interm_58_pt2(b,a)
term(132) = term(132) + wm_interm_55_pt2(a,b,q,p) * wm_interm_7_pt2(b,a)
term(133) = term(133) + wm_interm_56_pt2(a,b,q,p) * wm_interm_7_pt2(b,a)
term(134) = term(134) + wm_interm_57_pt2(a,b) * wm_interm_7_pt2(b,a)
term(135) = term(135) + wm_interm_58_pt2(a,b) * wm_interm_7_pt2(b,a)
term(136) = term(136) + wm_interm_34_pt2(a,b) * wm_interm_8_pt2(a,b,p,q)
term(137) = term(137) + wm_interm_34_pt2(a,b) * wm_interm_9_pt2(a,b,p,q)
term(138) = term(138) + wm_interm_34_pt2(a,b) * wm_interm_6_pt2(a,b,p,q)
term(139) = term(139) + wm_interm_34_pt2(a,b) * wm_interm_5_pt2(a,b,p,q)
term(140) = term(140) + wm_interm_15_pt2(a,b) * wm_interm_34_pt2(a,b)
term(141) = term(141) + wm_interm_16_pt2(a,b) * wm_interm_34_pt2(a,b)
term(142) = term(142) + wm_interm_35_pt2(a,b) * wm_interm_8_pt2(a,b,p,q)
term(143) = term(143) + wm_interm_35_pt2(a,b) * wm_interm_9_pt2(a,b,p,q)
term(144) = term(144) + wm_interm_35_pt2(a,b) * wm_interm_6_pt2(a,b,p,q)
term(145) = term(145) + wm_interm_35_pt2(a,b) * wm_interm_5_pt2(a,b,p,q)
term(146) = term(146) + wm_interm_15_pt2(a,b) * wm_interm_35_pt2(a,b)
term(147) = term(147) + wm_interm_16_pt2(a,b) * wm_interm_35_pt2(a,b)
term(148) = term(148) + wm_interm_17_pt2(a,b,p,q) * wm_interm_57_pt2(b,a)
term(149) = term(149) + wm_interm_17_pt2(a,b,p,q) * wm_interm_58_pt2(b,a)
term(150) = term(150) + wm_interm_20_pt2(a,b,p,q) * wm_interm_57_pt2(b,a)
term(151) = term(151) + wm_interm_20_pt2(a,b,p,q) * wm_interm_58_pt2(b,a)
term(152) = term(152) + wm_interm_15_pt2(a,b) * wm_interm_36_pt2(a,b,p,q)
term(153) = term(153) + wm_interm_16_pt2(a,b) * wm_interm_36_pt2(a,b,p,q)
term(154) = term(154) + wm_interm_15_pt2(a,b) * wm_interm_33_pt2(a,b,p,q)
term(155) = term(155) + wm_interm_16_pt2(a,b) * wm_interm_33_pt2(a,b,p,q)
term(156) = term(156) + wm_interm_19_pt2(a,b,p,q) * wm_interm_57_pt2(b,a)
term(157) = term(157) + wm_interm_19_pt2(a,b,p,q) * wm_interm_58_pt2(b,a)
term(158) = term(158) + wm_interm_18_pt2(a,b,p,q) * wm_interm_57_pt2(b,a)
term(159) = term(159) + wm_interm_18_pt2(a,b,p,q) * wm_interm_58_pt2(b,a)
term(160) = term(160) + wm_interm_15_pt2(a,b) * wm_interm_32_pt2(a,b,p,q)
term(161) = term(161) + wm_interm_16_pt2(a,b) * wm_interm_32_pt2(a,b,p,q)
term(162) = term(162) + wm_interm_15_pt2(a,b) * wm_interm_28_pt2(a,b,p,q)
term(163) = term(163) + wm_interm_16_pt2(a,b) * wm_interm_28_pt2(a,b,p,q)
end do 
end do 

term(116) = term(116) * (-2.0d+0) 
term(117) = term(117) * (4.0d+0) 
term(118) = term(118) * (4.0d+0) 
term(119) = term(119) * (-8.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * (4.0d+0) 
term(122) = term(122) * (-2.0d+0) 
term(123) = term(123) * (4.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * (8.0d+0) 
term(128) = term(128) * (2.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (-4.0d+0) 
term(131) = term(131) * (8.0d+0) 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (8.0d+0) 
term(135) = term(135) * (-16.0d+0) 
term(136) = term(136) * (-4.0d+0) 
term(137) = term(137) * (8.0d+0) 
term(138) = term(138) * (-4.0d+0) 
term(139) = term(139) * (8.0d+0) 
term(140) = term(140) * (8.0d+0) 
term(141) = term(141) * (-16.0d+0) 
term(142) = term(142) * (2.0d+0) 
term(143) = term(143) * (-4.0d+0) 
term(144) = term(144) * (2.0d+0) 
term(145) = term(145) * (-4.0d+0) 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (8.0d+0) 
term(148) = term(148) * (2.0d+0) 
term(149) = term(149) * (-4.0d+0) 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * (8.0d+0) 
term(152) = term(152) * (2.0d+0) 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * (8.0d+0) 
term(156) = term(156) * (2.0d+0) 
term(157) = term(157) * (-4.0d+0) 
term(158) = term(158) * (-4.0d+0) 
term(159) = term(159) * (8.0d+0) 
term(160) = term(160) * (2.0d+0) 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * (-4.0d+0) 
term(163) = term(163) * (8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(164) = term(164) + s1(a,i) * wm_interm_12_pt2(a,i)
term(165) = term(165) + s1(a,i) * wm_interm_11_pt2(a,i)
term(166) = term(166) + t1(a,i) * wm_interm_27_pt2(a,i)
term(167) = term(167) + t1(a,i) * wm_interm_24_pt2(a,i)
term(168) = term(168) + wm_interm_29_pt2(a,p,i,q) * wm_interm_4_pt2(a,i)
term(169) = term(169) + wm_interm_30_pt2(a,i) * wm_interm_4_pt2(a,i)
term(170) = term(170) + wm_interm_29_pt2(a,p,q,i) * wm_interm_4_pt2(a,i)
term(171) = term(171) + wm_interm_31_pt2(a,i) * wm_interm_4_pt2(a,i)
term(172) = term(172) + wm_interm_10_pt2(a,i) * wm_interm_29_pt2(a,p,i,q)
term(173) = term(173) + wm_interm_10_pt2(a,i) * wm_interm_30_pt2(a,i)
term(174) = term(174) + wm_interm_10_pt2(a,i) * wm_interm_29_pt2(a,p,q,i)
term(175) = term(175) + wm_interm_10_pt2(a,i) * wm_interm_31_pt2(a,i)
term(176) = term(176) + wm_interm_13_pt2(a,p,q,i) * wm_interm_30_pt2(a,i)
term(177) = term(177) + wm_interm_13_pt2(a,p,q,i) * wm_interm_31_pt2(a,i)
term(178) = term(178) + wm_interm_13_pt2(a,p,i,q) * wm_interm_31_pt2(a,i)
term(179) = term(179) + wm_interm_13_pt2(a,p,i,q) * wm_interm_30_pt2(a,i)
end do 
end do 

term(164) = term(164) * (8.0d+0) 
term(165) = term(165) * (-4.0d+0) 
term(166) = term(166) * (-8.0d+0) 
term(167) = term(167) * (4.0d+0) 
term(168) = term(168) * (4.0d+0) 
term(169) = term(169) * (-8.0d+0) 
term(170) = term(170) * (-8.0d+0) 
term(171) = term(171) * (16.0d+0) 
term(172) = term(172) * (-2.0d+0) 
term(173) = term(173) * (4.0d+0) 
term(174) = term(174) * (4.0d+0) 
term(175) = term(175) * (-8.0d+0) 
term(176) = term(176) * (4.0d+0) 
term(177) = term(177) * (-8.0d+0) 
term(178) = term(178) * (4.0d+0) 
term(179) = term(179) * (-2.0d+0) 

do i = 1, nocc 
term(180) = term(180) + wm_interm_1_pt2(q,i) * wm_interm_44_pt2(i,p)
term(181) = term(181) + wm_interm_2_pt2(q,i) * wm_interm_44_pt2(i,p)
term(182) = term(182) + wm_interm_1_pt2(i,q) * wm_interm_44_pt2(p,i)
term(183) = term(183) + wm_interm_2_pt2(i,q) * wm_interm_44_pt2(p,i)
term(184) = term(184) + wm_interm_1_pt2(q,i) * wm_interm_51_pt2(i,p)
term(185) = term(185) + wm_interm_1_pt2(q,i) * wm_interm_52_pt2(i,p)
term(186) = term(186) + wm_interm_2_pt2(q,i) * wm_interm_51_pt2(i,p)
term(187) = term(187) + wm_interm_2_pt2(q,i) * wm_interm_52_pt2(i,p)
term(188) = term(188) + wm_interm_22_pt2(p,i) * wm_interm_37_pt2(q,i)
term(189) = term(189) + wm_interm_23_pt2(p,i) * wm_interm_37_pt2(q,i)
term(190) = term(190) + wm_interm_22_pt2(p,i) * wm_interm_39_pt2(q,i)
term(191) = term(191) + wm_interm_23_pt2(p,i) * wm_interm_39_pt2(q,i)
term(192) = term(192) + wm_interm_1_pt2(i,q) * wm_interm_51_pt2(p,i)
term(193) = term(193) + wm_interm_1_pt2(i,q) * wm_interm_52_pt2(p,i)
term(194) = term(194) + wm_interm_2_pt2(i,q) * wm_interm_51_pt2(p,i)
term(195) = term(195) + wm_interm_2_pt2(i,q) * wm_interm_52_pt2(p,i)
term(196) = term(196) + wm_interm_22_pt2(i,p) * wm_interm_37_pt2(i,q)
term(197) = term(197) + wm_interm_22_pt2(i,p) * wm_interm_39_pt2(i,q)
term(198) = term(198) + wm_interm_23_pt2(i,p) * wm_interm_37_pt2(i,q)
term(199) = term(199) + wm_interm_23_pt2(i,p) * wm_interm_39_pt2(i,q)
end do 

term(180) = term(180) * (-2.0d+0) 
term(181) = term(181) * (4.0d+0) 
term(182) = term(182) * (-2.0d+0) 
term(183) = term(183) * (4.0d+0) 
term(184) = term(184) * (2.0d+0) 
term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * (-4.0d+0) 
term(187) = term(187) * (8.0d+0) 
term(188) = term(188) * (2.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * (-4.0d+0) 
term(191) = term(191) * (8.0d+0) 
term(192) = term(192) * (2.0d+0) 
term(193) = term(193) * (-4.0d+0) 
term(194) = term(194) * (-4.0d+0) 
term(195) = term(195) * (8.0d+0) 
term(196) = term(196) * (2.0d+0) 
term(197) = term(197) * (-4.0d+0) 
term(198) = term(198) * (-4.0d+0) 
term(199) = term(199) * (8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(200) = term(200) + wm_interm_14_pt2(i,j,k,q) * wm_interm_50_pt2(k,p,i,j)
term(201) = term(201) + wm_interm_14_pt2(i,j,k,q) * wm_interm_50_pt2(p,k,i,j)
end do 
end do 
end do 

term(200) = term(200) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(202) = term(202) + wm_interm_17_pt2(a,b,i,j) * wm_interm_55_pt2(b,a,j,i)
term(203) = term(203) + wm_interm_17_pt2(a,b,i,j) * wm_interm_56_pt2(b,a,j,i)
term(204) = term(204) + wm_interm_20_pt2(a,b,i,j) * wm_interm_55_pt2(b,a,j,i)
term(205) = term(205) + wm_interm_20_pt2(a,b,i,j) * wm_interm_56_pt2(b,a,j,i)
term(206) = term(206) + wm_interm_17_pt2(a,b,i,j) * wm_interm_54_pt2(b,a,j,i)
term(207) = term(207) + wm_interm_17_pt2(a,b,i,j) * wm_interm_53_pt2(b,a,j,i)
term(208) = term(208) + wm_interm_20_pt2(a,b,i,j) * wm_interm_54_pt2(b,a,j,i)
term(209) = term(209) + wm_interm_20_pt2(a,b,i,j) * wm_interm_53_pt2(b,a,j,i)
term(210) = term(210) + wm_interm_19_pt2(a,b,i,j) * wm_interm_54_pt2(b,a,j,i)
term(211) = term(211) + wm_interm_19_pt2(a,b,i,j) * wm_interm_53_pt2(b,a,j,i)
term(212) = term(212) + wm_interm_18_pt2(a,b,i,j) * wm_interm_53_pt2(b,a,j,i)
term(213) = term(213) + wm_interm_18_pt2(a,b,i,j) * wm_interm_54_pt2(b,a,j,i)
term(214) = term(214) + wm_interm_18_pt2(a,b,i,j) * wm_interm_55_pt2(b,a,j,i)
term(215) = term(215) + wm_interm_18_pt2(a,b,i,j) * wm_interm_56_pt2(b,a,j,i)
term(216) = term(216) + wm_interm_19_pt2(a,b,i,j) * wm_interm_55_pt2(b,a,j,i)
term(217) = term(217) + wm_interm_19_pt2(a,b,i,j) * wm_interm_56_pt2(b,a,j,i)
end do 
end do 
end do 
end do 

term(202) = term(202) * (8.0d+0) 
term(203) = term(203) * (-16.0d+0) 
term(204) = term(204) * (-16.0d+0) 
term(205) = term(205) * (32.0d+0) 
term(206) = term(206) * (-4.0d+0) 
term(207) = term(207) * (8.0d+0) 
term(208) = term(208) * (8.0d+0) 
term(209) = term(209) * (-16.0d+0) 
term(210) = term(210) * (-4.0d+0) 
term(211) = term(211) * (8.0d+0) 
term(212) = term(212) * (-4.0d+0) 
term(213) = term(213) * (8.0d+0) 
term(214) = term(214) * (-4.0d+0) 
term(215) = term(215) * (8.0d+0) 
term(216) = term(216) * (8.0d+0) 
term(217) = term(217) * (-16.0d+0) 

term(218) = term(218) + wm_interm_1_pt2(p,q) * wm_interm_45_pt2
term(219) = term(219) + wm_interm_2_pt2(p,q) * wm_interm_45_pt2
term(220) = term(220) + wm_interm_1_pt2(p,q) * wm_interm_59_pt2
term(221) = term(221) + wm_interm_1_pt2(p,q) * wm_interm_60_pt2
term(222) = term(222) + wm_interm_2_pt2(p,q) * wm_interm_59_pt2
term(223) = term(223) + wm_interm_2_pt2(p,q) * wm_interm_60_pt2

term(218) = term(218) * (4.0d+0) 
term(219) = term(219) * (-8.0d+0) 
term(220) = term(220) * (-2.0d+0) 
term(221) = term(221) * (4.0d+0) 
term(222) = term(222) * (4.0d+0) 
term(223) = term(223) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(224) = term(224) + wm_interm_14_pt2(i,q,j,k) * wm_interm_50_pt2(j,k,p,i)
term(225) = term(225) + wm_interm_14_pt2(q,i,j,k) * wm_interm_50_pt2(j,k,p,i)
end do 
end do 
end do 

term(225) = term(225) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(226) = term(226) + wm_interm_14_pt2(i,q,j,k) * wm_interm_50_pt2(j,k,i,p)
term(227) = term(227) + wm_interm_14_pt2(q,i,j,k) * wm_interm_50_pt2(j,k,i,p)
term(228) = term(228) + wm_interm_21_pt2(i,p,j,k) * wm_interm_38_pt2(i,q,k,j)
term(229) = term(229) + wm_interm_21_pt2(i,p,j,k) * wm_interm_38_pt2(i,q,j,k)
term(230) = term(230) + wm_interm_21_pt2(i,p,j,k) * wm_interm_38_pt2(q,i,j,k)
term(231) = term(231) + wm_interm_21_pt2(i,p,j,k) * wm_interm_38_pt2(q,i,k,j)
term(232) = term(232) + wm_interm_21_pt2(i,j,p,k) * wm_interm_38_pt2(i,j,k,q)
term(233) = term(233) + wm_interm_21_pt2(i,j,k,p) * wm_interm_38_pt2(i,j,k,q)
term(234) = term(234) + wm_interm_21_pt2(i,j,k,p) * wm_interm_38_pt2(i,j,q,k)
term(235) = term(235) + wm_interm_21_pt2(i,j,p,k) * wm_interm_38_pt2(i,j,q,k)
end do 
end do 
end do 

term(226) = term(226) * (-2.0d+0) 
term(229) = term(229) * (-2.0d+0) 
term(231) = term(231) * (-2.0d+0) 
term(233) = term(233) * (-2.0d+0) 
term(235) = term(235) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(236) = term(236) + wm_interm_14_pt2(i,j,k,l) * wm_interm_50_pt2(k,l,j,i)
end do 
end do 
end do 
end do 

term(236) = term(236) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(237) = term(237) + wm_interm_14_pt2(i,j,k,l) * wm_interm_50_pt2(k,l,i,j)
end do 
end do 
end do 
end do 

term(237) = term(237) * (4.0d+0) 


    calc_D_oo_wm_pt2 = zero
    do s = 0, 237
    calc_D_oo_wm_pt2 = calc_D_oo_wm_pt2 + term(s)
    end do

    end function calc_D_oo_wm_pt2
    
    function calc_D_ov_wm_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, k, b 
    real(F64), dimension(0:43) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_pt2(a,i,j,p) * wm_interm_17_pt2(a,q,j,i)
term(1) = term(1) + wm_interm_0_pt2(a,i,j,p) * wm_interm_18_pt2(a,q,j,i)
term(2) = term(2) + wm_interm_0_pt2(a,i,j,p) * wm_interm_19_pt2(a,q,j,i)
term(3) = term(3) + wm_interm_0_pt2(a,i,j,p) * wm_interm_20_pt2(a,q,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + wm_interm_0_pt2(a,i,p,j) * wm_interm_17_pt2(a,q,j,i)
term(5) = term(5) + wm_interm_0_pt2(a,i,p,j) * wm_interm_18_pt2(a,q,j,i)
term(6) = term(6) + wm_interm_0_pt2(a,i,p,j) * wm_interm_19_pt2(a,q,j,i)
term(7) = term(7) + wm_interm_0_pt2(a,i,p,j) * wm_interm_20_pt2(a,q,j,i)
term(8) = term(8) + wm_interm_28_pt2(q,a,i,j) * wm_interm_29_pt2(a,i,j,p)
term(9) = term(9) + wm_interm_28_pt2(q,a,i,j) * wm_interm_29_pt2(a,i,p,j)
term(10) = term(10) + wm_interm_29_pt2(a,i,j,p) * wm_interm_32_pt2(q,a,i,j)
term(11) = term(11) + wm_interm_29_pt2(a,i,p,j) * wm_interm_32_pt2(q,a,i,j)
term(12) = term(12) + wm_interm_29_pt2(a,i,p,j) * wm_interm_33_pt2(q,a,i,j)
term(13) = term(13) + wm_interm_29_pt2(a,i,j,p) * wm_interm_33_pt2(q,a,i,j)
term(14) = term(14) + wm_interm_29_pt2(a,i,j,p) * wm_interm_36_pt2(q,a,i,j)
term(15) = term(15) + wm_interm_29_pt2(a,i,p,j) * wm_interm_36_pt2(q,a,i,j)
end do 
end do 
end do 

term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (4.0d+0) 
term(7) = term(7) * (-8.0d+0) 
term(8) = term(8) * (4.0d+0) 
term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 

do a = nocc + 1, nactive 
term(16) = term(16) + wm_interm_11_pt2(a,p) * wm_interm_3_pt2(a,q)
term(17) = term(17) + wm_interm_12_pt2(a,p) * wm_interm_3_pt2(a,q)
term(18) = term(18) + wm_interm_11_pt2(a,p) * wm_interm_7_pt2(a,q)
term(19) = term(19) + wm_interm_12_pt2(a,p) * wm_interm_7_pt2(a,q)
term(20) = term(20) + wm_interm_30_pt2(a,p) * wm_interm_34_pt2(q,a)
term(21) = term(21) + wm_interm_31_pt2(a,p) * wm_interm_34_pt2(q,a)
term(22) = term(22) + wm_interm_30_pt2(a,p) * wm_interm_35_pt2(q,a)
term(23) = term(23) + wm_interm_31_pt2(a,p) * wm_interm_35_pt2(q,a)
end do 

term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * (4.0d+0) 
term(21) = term(21) * (-8.0d+0) 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(24) = term(24) + wm_interm_13_pt2(q,i,j,k) * wm_interm_21_pt2(i,p,k,j)
term(25) = term(25) + wm_interm_13_pt2(q,i,j,k) * wm_interm_21_pt2(i,p,j,k)
end do 
end do 
end do 

term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 

do i = 1, nocc 
term(26) = term(26) + wm_interm_10_pt2(q,i) * wm_interm_22_pt2(p,i)
term(27) = term(27) + wm_interm_10_pt2(q,i) * wm_interm_23_pt2(p,i)
term(28) = term(28) + wm_interm_22_pt2(p,i) * wm_interm_4_pt2(q,i)
term(29) = term(29) + wm_interm_23_pt2(p,i) * wm_interm_4_pt2(q,i)
term(30) = term(30) + wm_interm_1_pt2(p,i) * wm_interm_24_pt2(q,i)
term(31) = term(31) + wm_interm_24_pt2(q,i) * wm_interm_2_pt2(p,i)
term(32) = term(32) + wm_interm_1_pt2(p,i) * wm_interm_27_pt2(q,i)
term(33) = term(33) + wm_interm_27_pt2(q,i) * wm_interm_2_pt2(p,i)
term(34) = term(34) + s1(q,i) * wm_interm_44_pt2(p,i)
term(35) = term(35) + t1(q,i) * wm_interm_44_pt2(i,p)
term(36) = term(36) + s1(q,i) * wm_interm_51_pt2(p,i)
term(37) = term(37) + s1(q,i) * wm_interm_52_pt2(p,i)
term(38) = term(38) + t1(q,i) * wm_interm_51_pt2(i,p)
term(39) = term(39) + t1(q,i) * wm_interm_52_pt2(i,p)
end do 

term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-8.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (2.0d+0) 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(40) = term(40) + wm_interm_14_pt2(i,p,j,k) * wm_interm_25_pt2(q,j,k,i)
term(41) = term(41) + wm_interm_14_pt2(p,i,j,k) * wm_interm_25_pt2(q,j,k,i)
term(42) = term(42) + wm_interm_14_pt2(p,i,j,k) * wm_interm_26_pt2(q,j,k,i)
term(43) = term(43) + wm_interm_14_pt2(i,p,j,k) * wm_interm_26_pt2(q,j,k,i)
end do 
end do 
end do 

term(40) = term(40) * (-1.0d+0) 
term(41) = term(41) * (2.0d+0) 
term(42) = term(42) * (-1.0d+0) 
term(43) = term(43) * (2.0d+0) 


    calc_D_ov_wm_pt2 = zero
    do s = 0, 43
    calc_D_ov_wm_pt2 = calc_D_ov_wm_pt2 + term(s)
    end do

    end function calc_D_ov_wm_pt2
    
    function calc_D_vo_wm_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, b 
    real(F64), dimension(0:135) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_13_pt2(a,i,q,j) * wm_interm_5_pt2(p,a,i,j)
term(1) = term(1) + wm_interm_13_pt2(a,i,q,j) * wm_interm_6_pt2(p,a,i,j)
term(2) = term(2) + wm_interm_13_pt2(a,i,q,j) * wm_interm_8_pt2(p,a,i,j)
term(3) = term(3) + wm_interm_13_pt2(a,i,q,j) * wm_interm_9_pt2(p,a,i,j)
term(4) = term(4) + wm_interm_13_pt2(a,i,j,q) * wm_interm_8_pt2(p,a,i,j)
term(5) = term(5) + wm_interm_13_pt2(a,i,j,q) * wm_interm_9_pt2(p,a,i,j)
term(6) = term(6) + wm_interm_13_pt2(a,i,j,q) * wm_interm_6_pt2(p,a,i,j)
term(7) = term(7) + wm_interm_13_pt2(a,i,j,q) * wm_interm_5_pt2(p,a,i,j)
term(8) = term(8) + wm_interm_17_pt2(p,a,i,j) * wm_interm_26_pt2(a,q,j,i)
term(9) = term(9) + wm_interm_20_pt2(p,a,i,j) * wm_interm_26_pt2(a,q,j,i)
term(10) = term(10) + wm_interm_17_pt2(p,a,i,j) * wm_interm_26_pt2(a,j,q,i)
term(11) = term(11) + wm_interm_20_pt2(p,a,i,j) * wm_interm_26_pt2(a,j,q,i)
term(12) = term(12) + wm_interm_17_pt2(p,a,i,j) * wm_interm_25_pt2(a,q,j,i)
term(13) = term(13) + wm_interm_20_pt2(p,a,i,j) * wm_interm_25_pt2(a,q,j,i)
term(14) = term(14) + wm_interm_17_pt2(p,a,i,j) * wm_interm_25_pt2(a,j,q,i)
term(15) = term(15) + wm_interm_20_pt2(p,a,i,j) * wm_interm_25_pt2(a,j,q,i)
term(16) = term(16) + wm_interm_19_pt2(p,a,i,j) * wm_interm_25_pt2(a,q,j,i)
term(17) = term(17) + wm_interm_18_pt2(p,a,i,j) * wm_interm_25_pt2(a,q,j,i)
term(18) = term(18) + wm_interm_18_pt2(p,a,i,j) * wm_interm_26_pt2(a,q,j,i)
term(19) = term(19) + wm_interm_19_pt2(p,a,i,j) * wm_interm_26_pt2(a,q,j,i)
term(20) = term(20) + wm_interm_18_pt2(p,a,i,j) * wm_interm_26_pt2(a,j,q,i)
term(21) = term(21) + wm_interm_19_pt2(p,a,i,j) * wm_interm_26_pt2(a,j,q,i)
term(22) = term(22) + wm_interm_18_pt2(p,a,i,j) * wm_interm_25_pt2(a,j,q,i)
term(23) = term(23) + wm_interm_19_pt2(p,a,i,j) * wm_interm_25_pt2(a,j,q,i)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (4.0d+0) 
term(3) = term(3) * (-8.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (4.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-1.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(24) = term(24) + wm_interm_0_pt2(p,i,j,k) * wm_interm_14_pt2(k,j,i,q)
term(25) = term(25) + wm_interm_0_pt2(p,i,j,k) * wm_interm_14_pt2(j,k,i,q)
term(26) = term(26) + wm_interm_29_pt2(p,i,j,k) * wm_interm_38_pt2(q,i,j,k)
term(27) = term(27) + wm_interm_29_pt2(p,i,j,k) * wm_interm_38_pt2(i,q,j,k)
term(28) = term(28) + wm_interm_29_pt2(p,i,j,k) * wm_interm_38_pt2(i,q,k,j)
term(29) = term(29) + wm_interm_29_pt2(p,i,j,k) * wm_interm_38_pt2(q,i,k,j)
end do 
end do 
end do 

term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (2.0d+0) 
term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (2.0d+0) 

do i = 1, nocc 
term(30) = term(30) + wm_interm_11_pt2(p,i) * wm_interm_1_pt2(i,q)
term(31) = term(31) + wm_interm_12_pt2(p,i) * wm_interm_1_pt2(i,q)
term(32) = term(32) + wm_interm_11_pt2(p,i) * wm_interm_2_pt2(i,q)
term(33) = term(33) + wm_interm_12_pt2(p,i) * wm_interm_2_pt2(i,q)
term(34) = term(34) + wm_interm_31_pt2(p,i) * wm_interm_37_pt2(q,i)
term(35) = term(35) + wm_interm_30_pt2(p,i) * wm_interm_37_pt2(q,i)
term(36) = term(36) + wm_interm_31_pt2(p,i) * wm_interm_39_pt2(q,i)
term(37) = term(37) + wm_interm_30_pt2(p,i) * wm_interm_39_pt2(q,i)
term(38) = term(38) + r1(vrdav_Rl, p,i) * wm_interm_46_pt2(i,q)
term(39) = term(39) + r1(vrdav_Rr, p,i) * wm_interm_47_pt2(i,q)
end do 

term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * (4.0d+0) 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (-8.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (2.0d+0) 

do a = nocc + 1, nactive 
term(40) = term(40) + wm_interm_10_pt2(a,q) * wm_interm_15_pt2(p,a)
term(41) = term(41) + wm_interm_10_pt2(a,q) * wm_interm_16_pt2(p,a)
term(42) = term(42) + wm_interm_15_pt2(p,a) * wm_interm_4_pt2(a,q)
term(43) = term(43) + wm_interm_16_pt2(p,a) * wm_interm_4_pt2(a,q)
term(44) = term(44) + wm_interm_24_pt2(a,q) * wm_interm_3_pt2(p,a)
term(45) = term(45) + wm_interm_24_pt2(a,q) * wm_interm_7_pt2(p,a)
term(46) = term(46) + wm_interm_27_pt2(a,q) * wm_interm_3_pt2(p,a)
term(47) = term(47) + wm_interm_27_pt2(a,q) * wm_interm_7_pt2(p,a)
end do 

term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (4.0d+0) 
term(43) = term(43) * (-8.0d+0) 
term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (4.0d+0) 
term(47) = term(47) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(48) = term(48) + wm_interm_1_pt2(i,j) * wm_interm_25_pt2(p,q,j,i)
term(49) = term(49) + wm_interm_1_pt2(i,j) * wm_interm_26_pt2(p,q,j,i)
term(50) = term(50) + wm_interm_1_pt2(i,j) * wm_interm_25_pt2(p,j,q,i)
term(51) = term(51) + wm_interm_1_pt2(i,j) * wm_interm_26_pt2(p,j,q,i)
end do 
end do 

term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (-1.0d+0) 
term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (2.0d+0) 

term(52) = term(52) + s1(p,q) * wm_interm_45_pt2
term(53) = term(53) + t1(p,q) * wm_interm_45_pt2
term(54) = term(54) + s1(p,q) * wm_interm_59_pt2
term(55) = term(55) + s1(p,q) * wm_interm_60_pt2
term(56) = term(56) + t1(p,q) * wm_interm_59_pt2
term(57) = term(57) + t1(p,q) * wm_interm_60_pt2

term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-4.0d+0) 
term(56) = term(56) * (2.0d+0) 
term(57) = term(57) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(58) = term(58) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_15_pt2(b,a)
term(59) = term(59) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_16_pt2(b,a)
term(60) = term(60) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_3_pt2(b,a)
term(61) = term(61) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_7_pt2(b,a)
end do 
end do 
end do 

term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * (4.0d+0) 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(62) = term(62) + wm_interm_0_pt2(p,i,q,j) * wm_interm_1_pt2(j,i)
term(63) = term(63) + wm_interm_0_pt2(p,i,j,q) * wm_interm_1_pt2(j,i)
term(64) = term(64) + wm_interm_0_pt2(p,i,q,j) * wm_interm_2_pt2(j,i)
term(65) = term(65) + wm_interm_0_pt2(p,i,j,q) * wm_interm_2_pt2(j,i)
term(66) = term(66) + wm_interm_13_pt2(p,i,j,q) * wm_interm_22_pt2(i,j)
term(67) = term(67) + wm_interm_13_pt2(p,i,j,q) * wm_interm_23_pt2(i,j)
term(68) = term(68) + wm_interm_13_pt2(p,i,q,j) * wm_interm_22_pt2(i,j)
term(69) = term(69) + wm_interm_13_pt2(p,i,q,j) * wm_interm_23_pt2(i,j)
term(70) = term(70) + wm_interm_25_pt2(p,q,i,j) * wm_interm_2_pt2(j,i)
term(71) = term(71) + wm_interm_26_pt2(p,q,i,j) * wm_interm_2_pt2(j,i)
term(72) = term(72) + wm_interm_25_pt2(p,i,q,j) * wm_interm_2_pt2(j,i)
term(73) = term(73) + wm_interm_26_pt2(p,i,q,j) * wm_interm_2_pt2(j,i)
term(74) = term(74) + wm_interm_29_pt2(p,i,q,j) * wm_interm_37_pt2(i,j)
term(75) = term(75) + wm_interm_29_pt2(p,i,j,q) * wm_interm_37_pt2(i,j)
term(76) = term(76) + wm_interm_29_pt2(p,i,q,j) * wm_interm_39_pt2(i,j)
term(77) = term(77) + wm_interm_29_pt2(p,i,j,q) * wm_interm_39_pt2(i,j)
end do 
end do 

term(62) = term(62) * (-2.0d+0) 
term(63) = term(63) * (4.0d+0) 
term(64) = term(64) * (4.0d+0) 
term(65) = term(65) * (-8.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (-8.0d+0) 
term(68) = term(68) * (-2.0d+0) 
term(69) = term(69) * (4.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * (2.0d+0) 
term(72) = term(72) * (2.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (-8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(78) = term(78) + r1(vrdav_Rl, b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_3_pt2(a,b)
term(79) = term(79) + r1(vrdav_Rl, b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_7_pt2(a,b)
term(80) = term(80) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_34_pt2(a,b)
term(81) = term(81) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_35_pt2(a,b)
end do 
end do 
end do 

term(78) = term(78) * (-2.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (4.0d+0) 
term(81) = term(81) * (-2.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(82) = term(82) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,i,p,q) * wm_interm_3_pt2(b,a)
term(83) = term(83) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,i,p,q) * wm_interm_7_pt2(b,a)
term(84) = term(84) + r2(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_3_pt2(b,a)
term(85) = term(85) + r2(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_7_pt2(b,a)
end do 
end do 
end do 

term(82) = term(82) * (4.0d+0) 
term(83) = term(83) * (-8.0d+0) 
term(84) = term(84) * (4.0d+0) 
term(85) = term(85) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(86) = term(86) + wm_interm_4_pt2(a,i) * wm_interm_5_pt2(p,a,q,i)
term(87) = term(87) + wm_interm_4_pt2(a,i) * wm_interm_6_pt2(p,a,q,i)
term(88) = term(88) + wm_interm_4_pt2(a,i) * wm_interm_8_pt2(p,a,q,i)
term(89) = term(89) + wm_interm_4_pt2(a,i) * wm_interm_9_pt2(p,a,q,i)
term(90) = term(90) + wm_interm_10_pt2(a,i) * wm_interm_5_pt2(p,a,q,i)
term(91) = term(91) + wm_interm_10_pt2(a,i) * wm_interm_6_pt2(p,a,q,i)
term(92) = term(92) + wm_interm_10_pt2(a,i) * wm_interm_8_pt2(p,a,q,i)
term(93) = term(93) + wm_interm_10_pt2(a,i) * wm_interm_9_pt2(p,a,q,i)
term(94) = term(94) + wm_interm_11_pt2(a,i) * wm_interm_17_pt2(a,p,i,q)
term(95) = term(95) + wm_interm_12_pt2(a,i) * wm_interm_17_pt2(a,p,i,q)
term(96) = term(96) + wm_interm_11_pt2(a,i) * wm_interm_20_pt2(a,p,i,q)
term(97) = term(97) + wm_interm_12_pt2(a,i) * wm_interm_20_pt2(a,p,i,q)
term(98) = term(98) + wm_interm_11_pt2(a,i) * wm_interm_18_pt2(a,p,i,q)
term(99) = term(99) + wm_interm_12_pt2(a,i) * wm_interm_18_pt2(a,p,i,q)
term(100) = term(100) + wm_interm_11_pt2(a,i) * wm_interm_19_pt2(a,p,i,q)
term(101) = term(101) + wm_interm_12_pt2(a,i) * wm_interm_19_pt2(a,p,i,q)
term(102) = term(102) + wm_interm_28_pt2(p,a,q,i) * wm_interm_30_pt2(a,i)
term(103) = term(103) + wm_interm_28_pt2(p,a,q,i) * wm_interm_31_pt2(a,i)
term(104) = term(104) + wm_interm_17_pt2(p,a,q,i) * wm_interm_27_pt2(a,i)
term(105) = term(105) + wm_interm_20_pt2(p,a,q,i) * wm_interm_27_pt2(a,i)
term(106) = term(106) + wm_interm_30_pt2(a,i) * wm_interm_32_pt2(p,a,q,i)
term(107) = term(107) + wm_interm_31_pt2(a,i) * wm_interm_32_pt2(p,a,q,i)
term(108) = term(108) + wm_interm_17_pt2(p,a,q,i) * wm_interm_24_pt2(a,i)
term(109) = term(109) + wm_interm_20_pt2(p,a,q,i) * wm_interm_24_pt2(a,i)
term(110) = term(110) + wm_interm_31_pt2(a,i) * wm_interm_33_pt2(p,a,q,i)
term(111) = term(111) + wm_interm_19_pt2(p,a,q,i) * wm_interm_24_pt2(a,i)
term(112) = term(112) + wm_interm_30_pt2(a,i) * wm_interm_33_pt2(p,a,q,i)
term(113) = term(113) + wm_interm_18_pt2(p,a,q,i) * wm_interm_24_pt2(a,i)
term(114) = term(114) + wm_interm_30_pt2(a,i) * wm_interm_36_pt2(p,a,q,i)
term(115) = term(115) + wm_interm_31_pt2(a,i) * wm_interm_36_pt2(p,a,q,i)
term(116) = term(116) + wm_interm_18_pt2(p,a,q,i) * wm_interm_27_pt2(a,i)
term(117) = term(117) + wm_interm_19_pt2(p,a,q,i) * wm_interm_27_pt2(a,i)
term(118) = term(118) + r2(vrdav_Rl, a,q,p,i) * wm_interm_40_pt2(a,i)
term(119) = term(119) + r2(vrdav_Rl, a,i,p,q) * wm_interm_40_pt2(a,i)
term(120) = term(120) + r2(vrdav_Rl, a,q,p,i) * wm_interm_41_pt2(a,i)
term(121) = term(121) + r2(vrdav_Rl, a,i,p,q) * wm_interm_41_pt2(a,i)
term(122) = term(122) + r2(vrdav_Rr, a,q,p,i) * wm_interm_42_pt2(a,i)
term(123) = term(123) + r2(vrdav_Rr, a,i,p,q) * wm_interm_42_pt2(a,i)
term(124) = term(124) + r2(vrdav_Rr, a,q,p,i) * wm_interm_43_pt2(a,i)
term(125) = term(125) + r2(vrdav_Rr, a,i,p,q) * wm_interm_43_pt2(a,i)
end do 
end do 

term(86) = term(86) * (4.0d+0) 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * (-8.0d+0) 
term(89) = term(89) * (16.0d+0) 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * (4.0d+0) 
term(92) = term(92) * (4.0d+0) 
term(93) = term(93) * (-8.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (-8.0d+0) 
term(96) = term(96) * (-8.0d+0) 
term(97) = term(97) * (16.0d+0) 
term(98) = term(98) * (-2.0d+0) 
term(99) = term(99) * (4.0d+0) 
term(100) = term(100) * (4.0d+0) 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * (16.0d+0) 
term(104) = term(104) * (-8.0d+0) 
term(105) = term(105) * (16.0d+0) 
term(106) = term(106) * (4.0d+0) 
term(107) = term(107) * (-8.0d+0) 
term(108) = term(108) * (4.0d+0) 
term(109) = term(109) * (-8.0d+0) 
term(110) = term(110) * (4.0d+0) 
term(111) = term(111) * (4.0d+0) 
term(112) = term(112) * (-2.0d+0) 
term(113) = term(113) * (-2.0d+0) 
term(114) = term(114) * (4.0d+0) 
term(115) = term(115) * (-8.0d+0) 
term(116) = term(116) * (4.0d+0) 
term(117) = term(117) * (-8.0d+0) 
term(118) = term(118) * (-4.0d+0) 
term(119) = term(119) * (8.0d+0) 
term(120) = term(120) * (2.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (4.0d+0) 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (-2.0d+0) 
term(125) = term(125) * (4.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(126) = term(126) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_15_pt2(a,b)
term(127) = term(127) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_16_pt2(a,b)
term(128) = term(128) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_34_pt2(a,b)
term(129) = term(129) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_35_pt2(a,b)
end do 
end do 
end do 

term(126) = term(126) * (4.0d+0) 
term(127) = term(127) * (-8.0d+0) 
term(128) = term(128) * (-8.0d+0) 
term(129) = term(129) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(130) = term(130) + r2(vrdav_Rl, a,j,p,i) * wm_interm_61_pt2(a,i,j,q)
term(131) = term(131) + r2(vrdav_Rl, a,j,p,i) * wm_interm_61_pt2(a,j,i,q)
term(132) = term(132) + r2(vrdav_Rr, a,j,p,i) * wm_interm_62_pt2(a,j,i,q)
term(133) = term(133) + r2(vrdav_Rr, a,j,p,i) * wm_interm_62_pt2(a,i,j,q)
term(134) = term(134) + r2(vrdav_Rr, a,j,p,i) * wm_interm_63_pt2(a,i,j,q)
term(135) = term(135) + r2(vrdav_Rr, a,j,p,i) * wm_interm_63_pt2(a,j,i,q)
end do 
end do 
end do 

term(130) = term(130) * (-2.0d+0) 
term(131) = term(131) * (4.0d+0) 
term(132) = term(132) * (-1.0d+0) 
term(133) = term(133) * (2.0d+0) 
term(134) = term(134) * (-1.0d+0) 
term(135) = term(135) * (2.0d+0) 


    calc_D_vo_wm_pt2 = zero
    do s = 0, 135
    calc_D_vo_wm_pt2 = calc_D_vo_wm_pt2 + term(s)
    end do

    end function calc_D_vo_wm_pt2
    
    function calc_D_vv_wm_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, b, l 
    real(F64), dimension(0:181) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_17_pt2(q,a,i,j) * wm_interm_48_pt2(a,p,j,i)
term(1) = term(1) + wm_interm_20_pt2(q,a,i,j) * wm_interm_48_pt2(a,p,j,i)
term(2) = term(2) + wm_interm_18_pt2(q,a,i,j) * wm_interm_48_pt2(a,p,j,i)
term(3) = term(3) + wm_interm_19_pt2(q,a,i,j) * wm_interm_48_pt2(a,p,j,i)
term(4) = term(4) + wm_interm_17_pt2(a,q,i,j) * wm_interm_48_pt2(p,a,j,i)
term(5) = term(5) + wm_interm_18_pt2(a,q,i,j) * wm_interm_48_pt2(p,a,j,i)
term(6) = term(6) + wm_interm_19_pt2(a,q,i,j) * wm_interm_48_pt2(p,a,j,i)
term(7) = term(7) + wm_interm_20_pt2(a,q,i,j) * wm_interm_48_pt2(p,a,j,i)
term(8) = term(8) + wm_interm_28_pt2(q,a,i,j) * wm_interm_5_pt2(p,a,i,j)
term(9) = term(9) + wm_interm_28_pt2(q,a,i,j) * wm_interm_6_pt2(p,a,i,j)
term(10) = term(10) + wm_interm_28_pt2(q,a,i,j) * wm_interm_8_pt2(p,a,i,j)
term(11) = term(11) + wm_interm_28_pt2(q,a,i,j) * wm_interm_9_pt2(p,a,i,j)
term(12) = term(12) + wm_interm_17_pt2(q,a,i,j) * wm_interm_55_pt2(a,p,j,i)
term(13) = term(13) + wm_interm_20_pt2(q,a,i,j) * wm_interm_55_pt2(a,p,j,i)
term(14) = term(14) + wm_interm_17_pt2(q,a,i,j) * wm_interm_56_pt2(a,p,j,i)
term(15) = term(15) + wm_interm_20_pt2(q,a,i,j) * wm_interm_56_pt2(a,p,j,i)
term(16) = term(16) + wm_interm_32_pt2(q,a,i,j) * wm_interm_5_pt2(p,a,i,j)
term(17) = term(17) + wm_interm_32_pt2(q,a,i,j) * wm_interm_6_pt2(p,a,i,j)
term(18) = term(18) + wm_interm_32_pt2(q,a,i,j) * wm_interm_8_pt2(p,a,i,j)
term(19) = term(19) + wm_interm_32_pt2(q,a,i,j) * wm_interm_9_pt2(p,a,i,j)
term(20) = term(20) + wm_interm_17_pt2(q,a,i,j) * wm_interm_54_pt2(a,p,j,i)
term(21) = term(21) + wm_interm_20_pt2(q,a,i,j) * wm_interm_54_pt2(a,p,j,i)
term(22) = term(22) + wm_interm_17_pt2(q,a,i,j) * wm_interm_53_pt2(a,p,j,i)
term(23) = term(23) + wm_interm_20_pt2(q,a,i,j) * wm_interm_53_pt2(a,p,j,i)
term(24) = term(24) + wm_interm_33_pt2(q,a,i,j) * wm_interm_8_pt2(p,a,i,j)
term(25) = term(25) + wm_interm_33_pt2(q,a,i,j) * wm_interm_9_pt2(p,a,i,j)
term(26) = term(26) + wm_interm_19_pt2(q,a,i,j) * wm_interm_54_pt2(a,p,j,i)
term(27) = term(27) + wm_interm_19_pt2(q,a,i,j) * wm_interm_53_pt2(a,p,j,i)
term(28) = term(28) + wm_interm_33_pt2(q,a,i,j) * wm_interm_6_pt2(p,a,i,j)
term(29) = term(29) + wm_interm_33_pt2(q,a,i,j) * wm_interm_5_pt2(p,a,i,j)
term(30) = term(30) + wm_interm_18_pt2(q,a,i,j) * wm_interm_53_pt2(a,p,j,i)
term(31) = term(31) + wm_interm_18_pt2(q,a,i,j) * wm_interm_54_pt2(a,p,j,i)
term(32) = term(32) + wm_interm_36_pt2(q,a,i,j) * wm_interm_5_pt2(p,a,i,j)
term(33) = term(33) + wm_interm_36_pt2(q,a,i,j) * wm_interm_6_pt2(p,a,i,j)
term(34) = term(34) + wm_interm_36_pt2(q,a,i,j) * wm_interm_8_pt2(p,a,i,j)
term(35) = term(35) + wm_interm_36_pt2(q,a,i,j) * wm_interm_9_pt2(p,a,i,j)
term(36) = term(36) + wm_interm_18_pt2(q,a,i,j) * wm_interm_55_pt2(a,p,j,i)
term(37) = term(37) + wm_interm_19_pt2(q,a,i,j) * wm_interm_55_pt2(a,p,j,i)
term(38) = term(38) + wm_interm_18_pt2(q,a,i,j) * wm_interm_56_pt2(a,p,j,i)
term(39) = term(39) + wm_interm_19_pt2(q,a,i,j) * wm_interm_56_pt2(a,p,j,i)
term(40) = term(40) + wm_interm_17_pt2(a,q,i,j) * wm_interm_54_pt2(p,a,j,i)
term(41) = term(41) + wm_interm_17_pt2(a,q,i,j) * wm_interm_53_pt2(p,a,j,i)
term(42) = term(42) + wm_interm_17_pt2(a,q,i,j) * wm_interm_55_pt2(p,a,j,i)
term(43) = term(43) + wm_interm_17_pt2(a,q,i,j) * wm_interm_56_pt2(p,a,j,i)
term(44) = term(44) + wm_interm_18_pt2(a,q,i,j) * wm_interm_55_pt2(p,a,j,i)
term(45) = term(45) + wm_interm_18_pt2(a,q,i,j) * wm_interm_56_pt2(p,a,j,i)
term(46) = term(46) + wm_interm_18_pt2(a,q,i,j) * wm_interm_53_pt2(p,a,j,i)
term(47) = term(47) + wm_interm_18_pt2(a,q,i,j) * wm_interm_54_pt2(p,a,j,i)
term(48) = term(48) + wm_interm_19_pt2(a,q,i,j) * wm_interm_55_pt2(p,a,j,i)
term(49) = term(49) + wm_interm_19_pt2(a,q,i,j) * wm_interm_56_pt2(p,a,j,i)
term(50) = term(50) + wm_interm_19_pt2(a,q,i,j) * wm_interm_54_pt2(p,a,j,i)
term(51) = term(51) + wm_interm_19_pt2(a,q,i,j) * wm_interm_53_pt2(p,a,j,i)
term(52) = term(52) + wm_interm_20_pt2(a,q,i,j) * wm_interm_54_pt2(p,a,j,i)
term(53) = term(53) + wm_interm_20_pt2(a,q,i,j) * wm_interm_53_pt2(p,a,j,i)
term(54) = term(54) + wm_interm_20_pt2(a,q,i,j) * wm_interm_55_pt2(p,a,j,i)
term(55) = term(55) + wm_interm_20_pt2(a,q,i,j) * wm_interm_56_pt2(p,a,j,i)
term(56) = term(56) + wm_interm_33_pt2(a,q,i,j) * wm_interm_8_pt2(a,p,i,j)
term(57) = term(57) + wm_interm_33_pt2(a,q,i,j) * wm_interm_9_pt2(a,p,i,j)
term(58) = term(58) + wm_interm_36_pt2(a,q,i,j) * wm_interm_8_pt2(a,p,i,j)
term(59) = term(59) + wm_interm_36_pt2(a,q,i,j) * wm_interm_9_pt2(a,p,i,j)
term(60) = term(60) + wm_interm_36_pt2(a,q,i,j) * wm_interm_5_pt2(a,p,i,j)
term(61) = term(61) + wm_interm_36_pt2(a,q,i,j) * wm_interm_6_pt2(a,p,i,j)
term(62) = term(62) + wm_interm_33_pt2(a,q,i,j) * wm_interm_6_pt2(a,p,i,j)
term(63) = term(63) + wm_interm_33_pt2(a,q,i,j) * wm_interm_5_pt2(a,p,i,j)
term(64) = term(64) + wm_interm_32_pt2(a,q,i,j) * wm_interm_5_pt2(a,p,i,j)
term(65) = term(65) + wm_interm_32_pt2(a,q,i,j) * wm_interm_6_pt2(a,p,i,j)
term(66) = term(66) + wm_interm_28_pt2(a,q,i,j) * wm_interm_5_pt2(a,p,i,j)
term(67) = term(67) + wm_interm_28_pt2(a,q,i,j) * wm_interm_6_pt2(a,p,i,j)
term(68) = term(68) + wm_interm_32_pt2(a,q,i,j) * wm_interm_8_pt2(a,p,i,j)
term(69) = term(69) + wm_interm_32_pt2(a,q,i,j) * wm_interm_9_pt2(a,p,i,j)
term(70) = term(70) + wm_interm_28_pt2(a,q,i,j) * wm_interm_8_pt2(a,p,i,j)
term(71) = term(71) + wm_interm_28_pt2(a,q,i,j) * wm_interm_9_pt2(a,p,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * (8.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * (8.0d+0) 
term(8) = term(8) * (4.0d+0) 
term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * (-8.0d+0) 
term(11) = term(11) * (16.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (-8.0d+0) 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * (16.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (-2.0d+0) 
term(29) = term(29) * (4.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (4.0d+0) 
term(35) = term(35) * (-8.0d+0) 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (-8.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (4.0d+0) 
term(43) = term(43) * (-8.0d+0) 
term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (-2.0d+0) 
term(47) = term(47) * (4.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (-8.0d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (4.0d+0) 
term(53) = term(53) * (-8.0d+0) 
term(54) = term(54) * (-8.0d+0) 
term(55) = term(55) * (16.0d+0) 
term(56) = term(56) * (-2.0d+0) 
term(57) = term(57) * (4.0d+0) 
term(58) = term(58) * (4.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (4.0d+0) 
term(62) = term(62) * (-2.0d+0) 
term(63) = term(63) * (4.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (4.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (-8.0d+0) 
term(68) = term(68) * (4.0d+0) 
term(69) = term(69) * (-8.0d+0) 
term(70) = term(70) * (-8.0d+0) 
term(71) = term(71) * (16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(72) = term(72) + wm_interm_1_pt2(i,j) * wm_interm_48_pt2(q,p,j,i)
term(73) = term(73) + wm_interm_2_pt2(i,j) * wm_interm_48_pt2(q,p,j,i)
term(74) = term(74) + wm_interm_1_pt2(i,j) * wm_interm_53_pt2(q,p,j,i)
term(75) = term(75) + wm_interm_1_pt2(i,j) * wm_interm_54_pt2(q,p,j,i)
term(76) = term(76) + wm_interm_2_pt2(i,j) * wm_interm_53_pt2(q,p,j,i)
term(77) = term(77) + wm_interm_2_pt2(i,j) * wm_interm_54_pt2(q,p,j,i)
term(78) = term(78) + wm_interm_1_pt2(i,j) * wm_interm_55_pt2(q,p,j,i)
term(79) = term(79) + wm_interm_2_pt2(i,j) * wm_interm_55_pt2(q,p,j,i)
term(80) = term(80) + wm_interm_1_pt2(i,j) * wm_interm_56_pt2(q,p,j,i)
term(81) = term(81) + wm_interm_2_pt2(i,j) * wm_interm_56_pt2(q,p,j,i)
end do 
end do 

term(72) = term(72) * (2.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (-8.0d+0) 
term(78) = term(78) * (-2.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (4.0d+0) 
term(81) = term(81) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(82) = term(82) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_14_pt2(l,i,j,k)
term(83) = term(83) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_14_pt2(i,l,j,k)
term(84) = term(84) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_50_pt2(l,i,j,k)
term(85) = term(85) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_50_pt2(i,l,j,k)
end do 
end do 
end do 
end do 
end do 

term(82) = term(82) * (-1.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-1.0d+0) 
term(85) = term(85) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(86) = term(86) + wm_interm_13_pt2(q,i,j,k) * wm_interm_29_pt2(p,i,k,j)
term(87) = term(87) + wm_interm_13_pt2(q,i,j,k) * wm_interm_29_pt2(p,i,j,k)
end do 
end do 
end do 

term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (-4.0d+0) 

term(88) = term(88) + wm_interm_3_pt2(p,q) * wm_interm_45_pt2
term(89) = term(89) + wm_interm_45_pt2 * wm_interm_7_pt2(p,q)
term(90) = term(90) + wm_interm_3_pt2(p,q) * wm_interm_59_pt2
term(91) = term(91) + wm_interm_3_pt2(p,q) * wm_interm_60_pt2
term(92) = term(92) + wm_interm_59_pt2 * wm_interm_7_pt2(p,q)
term(93) = term(93) + wm_interm_60_pt2 * wm_interm_7_pt2(p,q)

term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (8.0d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * (-4.0d+0) 
term(93) = term(93) * (8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(94) = term(94) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_38_pt2(j,i,k,l)
term(95) = term(95) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_38_pt2(i,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(94) = term(94) * (-1.0d+0) 
term(95) = term(95) * (2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(96) = term(96) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_21_pt2(k,i,j,l)
term(97) = term(97) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_21_pt2(i,k,j,l)
end do 
end do 
end do 
end do 
end do 

term(96) = term(96) * (-1.0d+0) 
term(97) = term(97) * (2.0d+0) 

do i = 1, nocc 
term(98) = term(98) + r1(vrdav_Rl, q,i) * wm_interm_40_pt2(p,i)
term(99) = term(99) + r1(vrdav_Rl, q,i) * wm_interm_41_pt2(p,i)
term(100) = term(100) + s1(q,i) * wm_interm_11_pt2(p,i)
term(101) = term(101) + s1(q,i) * wm_interm_12_pt2(p,i)
term(102) = term(102) + r1(vrdav_Rr, p,i) * wm_interm_42_pt2(q,i)
term(103) = term(103) + r1(vrdav_Rr, p,i) * wm_interm_43_pt2(q,i)
term(104) = term(104) + t1(q,i) * wm_interm_24_pt2(p,i)
term(105) = term(105) + t1(q,i) * wm_interm_27_pt2(p,i)
term(106) = term(106) + wm_interm_10_pt2(q,i) * wm_interm_31_pt2(p,i)
term(107) = term(107) + wm_interm_10_pt2(q,i) * wm_interm_30_pt2(p,i)
term(108) = term(108) + wm_interm_30_pt2(p,i) * wm_interm_4_pt2(q,i)
term(109) = term(109) + wm_interm_31_pt2(p,i) * wm_interm_4_pt2(q,i)
end do 

term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (-2.0d+0) 
term(100) = term(100) * (2.0d+0) 
term(101) = term(101) * (-4.0d+0) 
term(102) = term(102) * (-4.0d+0) 
term(103) = term(103) * (2.0d+0) 
term(104) = term(104) * (2.0d+0) 
term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * (-4.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (-4.0d+0) 
term(109) = term(109) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(110) = term(110) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_14_pt2(i,l,k,j)
term(111) = term(111) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_14_pt2(l,i,k,j)
term(112) = term(112) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_50_pt2(l,i,k,j)
term(113) = term(113) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_50_pt2(i,l,k,j)
end do 
end do 
end do 
end do 
end do 

term(110) = term(110) * (-1.0d+0) 
term(111) = term(111) * (2.0d+0) 
term(112) = term(112) * (2.0d+0) 
term(113) = term(113) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(114) = term(114) + wm_interm_17_pt2(p,q,i,j) * wm_interm_44_pt2(j,i)
term(115) = term(115) + wm_interm_19_pt2(p,q,i,j) * wm_interm_44_pt2(j,i)
term(116) = term(116) + wm_interm_18_pt2(p,q,i,j) * wm_interm_44_pt2(j,i)
term(117) = term(117) + wm_interm_20_pt2(p,q,i,j) * wm_interm_44_pt2(j,i)
term(118) = term(118) + wm_interm_37_pt2(i,j) * wm_interm_8_pt2(p,q,i,j)
term(119) = term(119) + wm_interm_37_pt2(i,j) * wm_interm_9_pt2(p,q,i,j)
term(120) = term(120) + wm_interm_37_pt2(i,j) * wm_interm_6_pt2(p,q,i,j)
term(121) = term(121) + wm_interm_37_pt2(i,j) * wm_interm_5_pt2(p,q,i,j)
term(122) = term(122) + wm_interm_39_pt2(i,j) * wm_interm_8_pt2(p,q,i,j)
term(123) = term(123) + wm_interm_39_pt2(i,j) * wm_interm_9_pt2(p,q,i,j)
term(124) = term(124) + wm_interm_39_pt2(i,j) * wm_interm_6_pt2(p,q,i,j)
term(125) = term(125) + wm_interm_39_pt2(i,j) * wm_interm_5_pt2(p,q,i,j)
term(126) = term(126) + wm_interm_17_pt2(p,q,i,j) * wm_interm_51_pt2(j,i)
term(127) = term(127) + wm_interm_17_pt2(p,q,i,j) * wm_interm_52_pt2(j,i)
term(128) = term(128) + wm_interm_19_pt2(p,q,i,j) * wm_interm_51_pt2(j,i)
term(129) = term(129) + wm_interm_19_pt2(p,q,i,j) * wm_interm_52_pt2(j,i)
term(130) = term(130) + wm_interm_18_pt2(p,q,i,j) * wm_interm_51_pt2(j,i)
term(131) = term(131) + wm_interm_18_pt2(p,q,i,j) * wm_interm_52_pt2(j,i)
term(132) = term(132) + wm_interm_20_pt2(p,q,i,j) * wm_interm_51_pt2(j,i)
term(133) = term(133) + wm_interm_20_pt2(p,q,i,j) * wm_interm_52_pt2(j,i)
term(134) = term(134) + wm_interm_22_pt2(i,j) * wm_interm_36_pt2(p,q,i,j)
term(135) = term(135) + wm_interm_22_pt2(i,j) * wm_interm_33_pt2(p,q,i,j)
term(136) = term(136) + wm_interm_23_pt2(i,j) * wm_interm_36_pt2(p,q,i,j)
term(137) = term(137) + wm_interm_23_pt2(i,j) * wm_interm_33_pt2(p,q,i,j)
term(138) = term(138) + wm_interm_22_pt2(i,j) * wm_interm_32_pt2(p,q,i,j)
term(139) = term(139) + wm_interm_22_pt2(i,j) * wm_interm_28_pt2(p,q,i,j)
term(140) = term(140) + wm_interm_23_pt2(i,j) * wm_interm_32_pt2(p,q,i,j)
term(141) = term(141) + wm_interm_23_pt2(i,j) * wm_interm_28_pt2(p,q,i,j)
end do 
end do 

term(114) = term(114) * (2.0d+0) 
term(115) = term(115) * (2.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (-4.0d+0) 
term(118) = term(118) * (-2.0d+0) 
term(119) = term(119) * (4.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * (4.0d+0) 
term(122) = term(122) * (4.0d+0) 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (4.0d+0) 
term(125) = term(125) * (-8.0d+0) 
term(126) = term(126) * (-2.0d+0) 
term(127) = term(127) * (4.0d+0) 
term(128) = term(128) * (-2.0d+0) 
term(129) = term(129) * (4.0d+0) 
term(130) = term(130) * (4.0d+0) 
term(131) = term(131) * (-8.0d+0) 
term(132) = term(132) * (4.0d+0) 
term(133) = term(133) * (-8.0d+0) 
term(134) = term(134) * (-2.0d+0) 
term(135) = term(135) * (4.0d+0) 
term(136) = term(136) * (4.0d+0) 
term(137) = term(137) * (-8.0d+0) 
term(138) = term(138) * (-2.0d+0) 
term(139) = term(139) * (4.0d+0) 
term(140) = term(140) * (4.0d+0) 
term(141) = term(141) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(142) = term(142) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_21_pt2(k,i,l,j)
term(143) = term(143) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_21_pt2(i,k,l,j)
end do 
end do 
end do 
end do 
end do 

term(142) = term(142) * (2.0d+0) 
term(143) = term(143) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(144) = term(144) + r2(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_3_pt2(b,a)
term(145) = term(145) + r2(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_7_pt2(b,a)
term(146) = term(146) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_57_pt2(b,a)
term(147) = term(147) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_58_pt2(b,a)
end do 
end do 
end do 
end do 

term(144) = term(144) * (-2.0d+0) 
term(145) = term(145) * (4.0d+0) 
term(146) = term(146) * (-2.0d+0) 
term(147) = term(147) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(148) = term(148) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, b,j,p,i) * wm_interm_3_pt2(b,a)
term(149) = term(149) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, b,j,p,i) * wm_interm_7_pt2(b,a)
term(150) = term(150) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_57_pt2(b,a)
term(151) = term(151) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_58_pt2(b,a)
end do 
end do 
end do 
end do 

term(148) = term(148) * (4.0d+0) 
term(149) = term(149) * (-8.0d+0) 
term(150) = term(150) * (4.0d+0) 
term(151) = term(151) * (-8.0d+0) 

do a = nocc + 1, nactive 
term(152) = term(152) + wm_interm_3_pt2(q,a) * wm_interm_49_pt2(a,p)
term(153) = term(153) + wm_interm_49_pt2(a,p) * wm_interm_7_pt2(q,a)
term(154) = term(154) + wm_interm_3_pt2(a,q) * wm_interm_49_pt2(p,a)
term(155) = term(155) + wm_interm_49_pt2(p,a) * wm_interm_7_pt2(a,q)
term(156) = term(156) + wm_interm_3_pt2(q,a) * wm_interm_57_pt2(a,p)
term(157) = term(157) + wm_interm_57_pt2(a,p) * wm_interm_7_pt2(q,a)
term(158) = term(158) + wm_interm_3_pt2(q,a) * wm_interm_58_pt2(a,p)
term(159) = term(159) + wm_interm_58_pt2(a,p) * wm_interm_7_pt2(q,a)
term(160) = term(160) + wm_interm_15_pt2(p,a) * wm_interm_34_pt2(q,a)
term(161) = term(161) + wm_interm_16_pt2(p,a) * wm_interm_34_pt2(q,a)
term(162) = term(162) + wm_interm_15_pt2(p,a) * wm_interm_35_pt2(q,a)
term(163) = term(163) + wm_interm_16_pt2(p,a) * wm_interm_35_pt2(q,a)
term(164) = term(164) + wm_interm_3_pt2(a,q) * wm_interm_57_pt2(p,a)
term(165) = term(165) + wm_interm_3_pt2(a,q) * wm_interm_58_pt2(p,a)
term(166) = term(166) + wm_interm_57_pt2(p,a) * wm_interm_7_pt2(a,q)
term(167) = term(167) + wm_interm_58_pt2(p,a) * wm_interm_7_pt2(a,q)
term(168) = term(168) + wm_interm_15_pt2(a,p) * wm_interm_35_pt2(a,q)
term(169) = term(169) + wm_interm_16_pt2(a,p) * wm_interm_35_pt2(a,q)
term(170) = term(170) + wm_interm_15_pt2(a,p) * wm_interm_34_pt2(a,q)
term(171) = term(171) + wm_interm_16_pt2(a,p) * wm_interm_34_pt2(a,q)
end do 

term(152) = term(152) * (2.0d+0) 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (2.0d+0) 
term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * (-2.0d+0) 
term(157) = term(157) * (4.0d+0) 
term(158) = term(158) * (4.0d+0) 
term(159) = term(159) * (-8.0d+0) 
term(160) = term(160) * (4.0d+0) 
term(161) = term(161) * (-8.0d+0) 
term(162) = term(162) * (-2.0d+0) 
term(163) = term(163) * (4.0d+0) 
term(164) = term(164) * (-2.0d+0) 
term(165) = term(165) * (4.0d+0) 
term(166) = term(166) * (4.0d+0) 
term(167) = term(167) * (-8.0d+0) 
term(168) = term(168) * (-2.0d+0) 
term(169) = term(169) * (4.0d+0) 
term(170) = term(170) * (4.0d+0) 
term(171) = term(171) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(172) = term(172) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,i,j) * wm_interm_34_pt2(a,b)
term(173) = term(173) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,i,j) * wm_interm_35_pt2(a,b)
term(174) = term(174) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,i,j) * wm_interm_15_pt2(a,b)
term(175) = term(175) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,i,j) * wm_interm_16_pt2(a,b)
end do 
end do 
end do 
end do 

term(172) = term(172) * (4.0d+0) 
term(173) = term(173) * (-2.0d+0) 
term(174) = term(174) * (-2.0d+0) 
term(175) = term(175) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(176) = term(176) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,j,i) * wm_interm_34_pt2(a,b)
term(177) = term(177) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,j,i) * wm_interm_35_pt2(a,b)
term(178) = term(178) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_15_pt2(a,b)
term(179) = term(179) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_16_pt2(a,b)
end do 
end do 
end do 
end do 

term(176) = term(176) * (-8.0d+0) 
term(177) = term(177) * (4.0d+0) 
term(178) = term(178) * (4.0d+0) 
term(179) = term(179) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(180) = term(180) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_38_pt2(i,j,l,k)
term(181) = term(181) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_38_pt2(j,i,l,k)
end do 
end do 
end do 
end do 
end do 

term(180) = term(180) * (-1.0d+0) 
term(181) = term(181) * (2.0d+0) 


    calc_D_vv_wm_pt2 = zero
    do s = 0, 181
    calc_D_vv_wm_pt2 = calc_D_vv_wm_pt2 + term(s)
    end do

    end function calc_D_vv_wm_pt2
    

  end module ss_ccsd_pt012
