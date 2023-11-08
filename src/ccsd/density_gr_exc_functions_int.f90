module density_gr_exc_functions_int


      use cc3_intermediates
      use arithmetic
      use s_gen
      use basis
      use eom_vectors

      implicit none
      !
      ! File generated automatically on 2015-02-10 13:21:16
      !
      real(F64), dimension(:, :), allocatable :: xi_interm_1_ccsd 
      real(F64), dimension(:, :), allocatable :: xi_interm_2_ccsd 
      real(F64), dimension(:, :), allocatable :: xi_interm_3_ccsd 
      real(F64), dimension(:, :), allocatable :: xi_interm_4_ccsd 

      real(F64), dimension(:, :), allocatable :: xi_interm_1_cc3 
      real(F64), dimension(:, :), allocatable :: xi_interm_2_cc3 
      real(F64), dimension(:, :), allocatable :: xi_interm_3_cc3 
      real(F64), dimension(:, :), allocatable :: xi_interm_4_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: xi_interm_5_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: xi_interm_6_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: xi_interm_7_cc3 

      real(F64), dimension(:, :), allocatable :: gamma_interm_1_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_2_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_3_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_4_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_5_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_6_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_7_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_8_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_9_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_10_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_11_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_12_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_13_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_14_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_15_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_16_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_17_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_18_ccsd 
      real(F64), dimension(:, :), allocatable :: gamma_interm_19_ccsd 

      real(F64), dimension(:, :), allocatable :: gamma_interm_1_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_2_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_3_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_4_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_5_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_6_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_7_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_8_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_9_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_10_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_11_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_12_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_13_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_14_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_15_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_16_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_17_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_18_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_19_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_20_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_21_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_22_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_23_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_24_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_25_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_26_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_27_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_28_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_29_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_30_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_31_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_32_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_33_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_34_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_35_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_36_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_37_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_38_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_39_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_40_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_41_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_42_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_43_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_44_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_45_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_46_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_47_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_48_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_49_cc3 
      real(F64), dimension(:, :), allocatable :: gamma_interm_50_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_51_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_52_cc3 
      real(F64), dimension(:, :, :, :), allocatable :: gamma_interm_53_cc3 





contains

      subroutine xi_intermediates_ccsd_init(nocc, nactive)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive
            allocate(xi_interm_1_ccsd(1: nocc, 1: nocc))
            allocate(xi_interm_2_ccsd(nocc+1: nactive, nocc+1: nactive))
            allocate(xi_interm_3_ccsd(1: nocc, 1: nocc))
            allocate(xi_interm_4_ccsd(nocc+1: nactive, nocc+1: nactive))
            xi_interm_1_ccsd = zero 
            xi_interm_2_ccsd = zero 
            xi_interm_3_ccsd = zero 
            xi_interm_4_ccsd = zero 


      end subroutine xi_intermediates_ccsd_init



      subroutine xi_intermediates_ccsd_free()

            deallocate(xi_interm_1_ccsd)
            deallocate(xi_interm_2_ccsd)
            deallocate(xi_interm_3_ccsd)
            deallocate(xi_interm_4_ccsd)

      end subroutine xi_intermediates_ccsd_free


      subroutine xi_intermediates_cc3_init(nocc, nactive)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive


            allocate(xi_interm_1_cc3(1: nocc, 1: nocc))
            allocate(xi_interm_2_cc3(nocc+1: nactive, nocc+1: nactive))
            allocate(xi_interm_3_cc3(1: nocc, 1: nocc))
            allocate(xi_interm_4_cc3(nocc+1: nactive, nocc+1: nactive))
            allocate(xi_interm_5_cc3(nocc+1: nactive, 1: nocc, nocc+1: nactive, nocc+1: nactive))
            allocate(xi_interm_6_cc3(1: nocc, nocc+1: nactive, nocc+1: nactive, nocc+1: nactive))
            allocate(xi_interm_7_cc3(nocc+1: nactive, nocc+1: nactive, 1: nocc, nocc+1: nactive))
            xi_interm_1_cc3 = zero 
            xi_interm_2_cc3 = zero 
            xi_interm_3_cc3 = zero 
            xi_interm_4_cc3 = zero 
            xi_interm_5_cc3 = zero 
            xi_interm_6_cc3 = zero 
            xi_interm_7_cc3 = zero

      end subroutine xi_intermediates_cc3_init

      subroutine xi_intermediates_cc3_free()

            deallocate(xi_interm_1_cc3)
            deallocate(xi_interm_2_cc3)
            deallocate(xi_interm_3_cc3)
            deallocate(xi_interm_4_cc3)
            deallocate(xi_interm_5_cc3)
            deallocate(xi_interm_6_cc3)
            deallocate(xi_interm_7_cc3)
      end subroutine xi_intermediates_cc3_free


      subroutine gamma_intermediates_ccsd_init(nocc, nactive)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive


            allocate(gamma_interm_1_ccsd(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_2_ccsd(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_3_ccsd(nocc+1: nactive, nocc+1: nactive))
            allocate(gamma_interm_4_ccsd(1: nocc, 1: nocc))
            allocate(gamma_interm_5_ccsd(1: nocc, 1: nocc))
            allocate(gamma_interm_6_ccsd(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_7_ccsd(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_8_ccsd(1: nocc, 1: nocc))
            allocate(gamma_interm_9_ccsd(1: nocc, 1: nocc))
            allocate(gamma_interm_10_ccsd(nocc+1: nactive, nocc+1: nactive))
            allocate(gamma_interm_11_ccsd(1: nocc, 1: nocc))
            allocate(gamma_interm_12_ccsd(1: nocc, 1: nocc))
            allocate(gamma_interm_13_ccsd(1: nocc, 1: nocc))
            allocate(gamma_interm_14_ccsd(1: nocc, 1: nocc))
            allocate(gamma_interm_15_ccsd(nocc+1: nactive, nocc+1: nactive))
            allocate(gamma_interm_16_ccsd(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_17_ccsd(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_18_ccsd(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_19_ccsd(nocc+1: nactive, 1: nocc))
            gamma_interm_1_ccsd = zero 
            gamma_interm_2_ccsd = zero 
            gamma_interm_3_ccsd = zero 
            gamma_interm_4_ccsd = zero 
            gamma_interm_5_ccsd = zero 
            gamma_interm_6_ccsd = zero 
            gamma_interm_7_ccsd = zero 
            gamma_interm_8_ccsd = zero 
            gamma_interm_9_ccsd = zero 
            gamma_interm_10_ccsd = zero 
            gamma_interm_11_ccsd = zero 
            gamma_interm_12_ccsd = zero 
            gamma_interm_13_ccsd = zero 
            gamma_interm_14_ccsd = zero 
            gamma_interm_15_ccsd = zero 
            gamma_interm_16_ccsd = zero 
            gamma_interm_17_ccsd = zero 
            gamma_interm_18_ccsd = zero 
            gamma_interm_19_ccsd = zero 


      end subroutine gamma_intermediates_ccsd_init

      subroutine gamma_intermediates_ccsd_free()
            deallocate(gamma_interm_1_ccsd)
            deallocate(gamma_interm_2_ccsd)
            deallocate(gamma_interm_3_ccsd)
            deallocate(gamma_interm_4_ccsd)
            deallocate(gamma_interm_5_ccsd)
            deallocate(gamma_interm_6_ccsd)
            deallocate(gamma_interm_7_ccsd)
            deallocate(gamma_interm_8_ccsd)
            deallocate(gamma_interm_9_ccsd)
            deallocate(gamma_interm_10_ccsd)
            deallocate(gamma_interm_11_ccsd)
            deallocate(gamma_interm_12_ccsd)
            deallocate(gamma_interm_13_ccsd)
            deallocate(gamma_interm_14_ccsd)
            deallocate(gamma_interm_15_ccsd)
            deallocate(gamma_interm_16_ccsd)
            deallocate(gamma_interm_17_ccsd)
            deallocate(gamma_interm_18_ccsd)
            deallocate(gamma_interm_19_ccsd)
      end subroutine gamma_intermediates_ccsd_free


      subroutine gamma_intermediates_cc3_init(nocc, nactive)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive
            allocate(gamma_interm_1_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_2_cc3(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_3_cc3(1: nocc, 1: nocc, nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_4_cc3(1: nocc, 1: nocc, nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_5_cc3(1: nocc, 1: nocc, 1: nocc, nocc+1: nactive))
            allocate(gamma_interm_6_cc3(1: nocc, 1: nocc, 1: nocc, nocc+1: nactive))
            allocate(gamma_interm_7_cc3(1: nocc, 1: nocc, nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_8_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_9_cc3(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_10_cc3(1: nocc, nocc+1: nactive, 1: nocc, 1: nocc))
            allocate(gamma_interm_11_cc3(1: nocc, nocc+1: nactive, 1: nocc, 1: nocc))
            allocate(gamma_interm_12_cc3(1: nocc, 1: nocc, nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_13_cc3(1: nocc, 1: nocc, nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_14_cc3(1: nocc, nocc+1: nactive, 1: nocc, 1: nocc))
            allocate(gamma_interm_15_cc3(1: nocc, nocc+1: nactive, 1: nocc, 1: nocc))
            allocate(gamma_interm_16_cc3(1: nocc, 1: nocc, nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_17_cc3(1: nocc, 1: nocc, nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_18_cc3(1: nocc, 1: nocc, 1: nocc, nocc+1: nactive))
            allocate(gamma_interm_19_cc3(nocc+1: nactive, nocc+1: nactive))
            allocate(gamma_interm_20_cc3(1: nocc, 1: nocc))
            allocate(gamma_interm_21_cc3(1: nocc, 1: nocc))
            allocate(gamma_interm_22_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_23_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_24_cc3(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_25_cc3(nocc+1: nactive, nocc+1: nactive, 1: nocc, nocc+1: nactive))
            allocate(gamma_interm_26_cc3(nocc+1: nactive, 1: nocc, nocc+1: nactive, nocc+1: nactive))
            allocate(gamma_interm_27_cc3(nocc+1: nactive, nocc+1: nactive, nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_28_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_29_cc3(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_30_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_31_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_32_cc3(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_33_cc3(1: nocc, 1: nocc))
            allocate(gamma_interm_34_cc3(1: nocc, 1: nocc))
            allocate(gamma_interm_35_cc3(nocc+1: nactive, nocc+1: nactive))
            allocate(gamma_interm_36_cc3(1: nocc, 1: nocc))
            allocate(gamma_interm_37_cc3(1: nocc, 1: nocc))
            allocate(gamma_interm_38_cc3(1: nocc, 1: nocc))
            allocate(gamma_interm_39_cc3(1: nocc, 1: nocc))
            allocate(gamma_interm_40_cc3(nocc+1: nactive, nocc+1: nactive))
            allocate(gamma_interm_41_cc3(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_42_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_43_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_44_cc3(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_45_cc3(nocc+1: nactive, 1: nocc, nocc+1: nactive, nocc+1: nactive))
            allocate(gamma_interm_46_cc3(nocc+1: nactive, nocc+1: nactive, nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_47_cc3(nocc+1: nactive, nocc+1: nactive, 1: nocc, nocc+1: nactive))
            allocate(gamma_interm_48_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_49_cc3(nocc+1: nactive, 1: nocc))
            allocate(gamma_interm_50_cc3(1: nocc, nocc+1: nactive))
            allocate(gamma_interm_51_cc3(nocc+1: nactive, nocc+1: nactive, 1: nocc, nocc+1: nactive))
            allocate(gamma_interm_52_cc3(1: nocc, nocc+1: nactive, nocc+1: nactive, nocc+1: nactive))
            allocate(gamma_interm_53_cc3(nocc+1: nactive, 1: nocc, nocc+1: nactive, nocc+1: nactive))
            gamma_interm_1_cc3 = zero 
            gamma_interm_2_cc3 = zero 
            gamma_interm_3_cc3 = zero 
            gamma_interm_4_cc3 = zero 
            gamma_interm_5_cc3 = zero 
            gamma_interm_6_cc3 = zero 
            gamma_interm_7_cc3 = zero 
            gamma_interm_8_cc3 = zero 
            gamma_interm_9_cc3 = zero 
            gamma_interm_10_cc3 = zero 
            gamma_interm_11_cc3 = zero 
            gamma_interm_12_cc3 = zero 
            gamma_interm_13_cc3 = zero 
            gamma_interm_14_cc3 = zero 
            gamma_interm_15_cc3 = zero 
            gamma_interm_16_cc3 = zero 
            gamma_interm_17_cc3 = zero 
            gamma_interm_18_cc3 = zero 
            gamma_interm_19_cc3 = zero 
            gamma_interm_20_cc3 = zero 
            gamma_interm_21_cc3 = zero 
            gamma_interm_22_cc3 = zero 
            gamma_interm_23_cc3 = zero 
            gamma_interm_24_cc3 = zero 
            gamma_interm_25_cc3 = zero 
            gamma_interm_26_cc3 = zero 
            gamma_interm_27_cc3 = zero 
            gamma_interm_28_cc3 = zero 
            gamma_interm_29_cc3 = zero 
            gamma_interm_30_cc3 = zero 
            gamma_interm_31_cc3 = zero 
            gamma_interm_32_cc3 = zero 
            gamma_interm_33_cc3 = zero 
            gamma_interm_34_cc3 = zero 
            gamma_interm_35_cc3 = zero 
            gamma_interm_36_cc3 = zero 
            gamma_interm_37_cc3 = zero 
            gamma_interm_38_cc3 = zero 
            gamma_interm_39_cc3 = zero 
            gamma_interm_40_cc3 = zero 
            gamma_interm_41_cc3 = zero 
            gamma_interm_42_cc3 = zero 
            gamma_interm_43_cc3 = zero 
            gamma_interm_44_cc3 = zero 
            gamma_interm_45_cc3 = zero 
            gamma_interm_46_cc3 = zero 
            gamma_interm_47_cc3 = zero 
            gamma_interm_48_cc3 = zero 
            gamma_interm_49_cc3 = zero 
            gamma_interm_50_cc3 = zero 
            gamma_interm_51_cc3 = zero 
            gamma_interm_52_cc3 = zero 
            gamma_interm_53_cc3 = zero 


      end subroutine gamma_intermediates_cc3_init

      subroutine gamma_intermediates_cc3_free()

            deallocate(gamma_interm_1_cc3)
            deallocate(gamma_interm_2_cc3)
            deallocate(gamma_interm_3_cc3)
            deallocate(gamma_interm_4_cc3)
            deallocate(gamma_interm_5_cc3)
            deallocate(gamma_interm_6_cc3)
            deallocate(gamma_interm_7_cc3)
            deallocate(gamma_interm_8_cc3)
            deallocate(gamma_interm_9_cc3)
            deallocate(gamma_interm_10_cc3)
            deallocate(gamma_interm_11_cc3)
            deallocate(gamma_interm_12_cc3)
            deallocate(gamma_interm_13_cc3)
            deallocate(gamma_interm_14_cc3)
            deallocate(gamma_interm_15_cc3)
            deallocate(gamma_interm_16_cc3)
            deallocate(gamma_interm_17_cc3)
            deallocate(gamma_interm_18_cc3)
            deallocate(gamma_interm_19_cc3)
            deallocate(gamma_interm_20_cc3)
            deallocate(gamma_interm_21_cc3)
            deallocate(gamma_interm_22_cc3)
            deallocate(gamma_interm_23_cc3)
            deallocate(gamma_interm_24_cc3)
            deallocate(gamma_interm_25_cc3)
            deallocate(gamma_interm_26_cc3)
            deallocate(gamma_interm_27_cc3)
            deallocate(gamma_interm_28_cc3)
            deallocate(gamma_interm_29_cc3)
            deallocate(gamma_interm_30_cc3)
            deallocate(gamma_interm_31_cc3)
            deallocate(gamma_interm_32_cc3)
            deallocate(gamma_interm_33_cc3)
            deallocate(gamma_interm_34_cc3)
            deallocate(gamma_interm_35_cc3)
            deallocate(gamma_interm_36_cc3)
            deallocate(gamma_interm_37_cc3)
            deallocate(gamma_interm_38_cc3)
            deallocate(gamma_interm_39_cc3)
            deallocate(gamma_interm_40_cc3)
            deallocate(gamma_interm_41_cc3)
            deallocate(gamma_interm_42_cc3)
            deallocate(gamma_interm_43_cc3)
            deallocate(gamma_interm_44_cc3)
            deallocate(gamma_interm_45_cc3)
            deallocate(gamma_interm_46_cc3)
            deallocate(gamma_interm_47_cc3)
            deallocate(gamma_interm_48_cc3)
            deallocate(gamma_interm_49_cc3)
            deallocate(gamma_interm_50_cc3)
            deallocate(gamma_interm_51_cc3)
            deallocate(gamma_interm_52_cc3)
            deallocate(gamma_interm_53_cc3)
      end subroutine gamma_intermediates_cc3_free


      subroutine xi_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, vdav) 
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            double precision, dimension(:), intent(in) :: vdav
            integer :: a, i, b, j, c, k 
            real(F64) :: sum

            !$omp parallel private(b, c, k, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + l2(vdav, b,j,c,k) * t2(b,c,i,k)
                                    end do
                              end do
                        end do
                        xi_interm_1_ccsd(j, i) = xi_interm_1_ccsd(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, b, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do b = nocc + 1, nactive 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              do k = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          sum = sum + l2(vdav, c,k,b,j) * t2(c,a,k,j)
                                    end do
                              end do
                        end do
                        xi_interm_2_ccsd(b, a) = xi_interm_2_ccsd(b, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do b = nocc + 1, nactive 
                              sum = sum + l2(vdav, b,j,b,j) * t2(b,b,i,j)
                        end do
                        xi_interm_3_ccsd(j, i) = xi_interm_3_ccsd(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, b, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do b = nocc + 1, nactive 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              sum = sum + l2(vdav, b,j,b,j) * t2(a,b,j,j)
                        end do
                        xi_interm_4_ccsd(b, a) = xi_interm_4_ccsd(b, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 


      end subroutine xi_intermediates_ccsd

      subroutine xi_intermediates_cc3(t2, t1, s2, s1, nocc, nactive, vdav) 
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            double precision, dimension(:), intent(in) :: vdav
            integer :: a, i, b, j, c, k, d, l 
            real(F64) :: sum

            !$omp parallel private(b, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do b = nocc + 1, nactive 
                              sum = sum + l2(vdav, b,j,b,j) * t2(b,b,i,j)
                        end do
                        xi_interm_1_cc3(j, i) = xi_interm_1_cc3(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, b, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do b = nocc + 1, nactive 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              sum = sum + l2(vdav, b,j,b,j) * t2(a,b,j,j)
                        end do
                        xi_interm_2_cc3(b, a) = xi_interm_2_cc3(b, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + l2(vdav, b,j,c,k) * t2(b,c,i,k)
                                    end do
                              end do
                        end do
                        xi_interm_3_cc3(j, i) = xi_interm_3_cc3(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, b, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do b = nocc + 1, nactive 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              do k = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          sum = sum + l2(vdav, c,k,b,j) * t2(c,a,k,j)
                                    end do
                              end do
                        end do
                        xi_interm_4_cc3(b, a) = xi_interm_4_cc3(b, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(c, j, l, b, k, d, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do b = nocc + 1, nactive 
                  do k = 1, nocc 
                        do d = nocc + 1, nactive 
                              do a = nocc + 1, nactive 
                                    sum = zero 
                                    do l = 1, nocc 
                                          do j = 1, nocc 
                                                do c = nocc + 1, nactive 
                                                      sum = sum + l3(vdav, c,j,b,k,d,l) * t2(c,a,l,j)
                                                end do
                                          end do
                                    end do
                                    xi_interm_5_cc3(b, k, d, a) = xi_interm_5_cc3(b, k, d, a) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, l, k, b, d, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        do d = nocc + 1, nactive 
                              do a = nocc + 1, nactive 
                                    sum = zero 
                                    do l = 1, nocc 
                                          do j = 1, nocc 
                                                do c = nocc + 1, nactive 
                                                      sum = sum + l3(vdav, c,k,b,j,d,l) * t2(c,a,l,j)
                                                end do
                                          end do
                                    end do
                                    xi_interm_6_cc3(k, b, d, a) = xi_interm_6_cc3(k, b, d, a) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(k, c, j, b, d, l, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do b = nocc + 1, nactive 
                  do d = nocc + 1, nactive 
                        do l = 1, nocc 
                              do a = nocc + 1, nactive 
                                    sum = zero 
                                    do k = 1, nocc 
                                          do j = 1, nocc 
                                                do c = nocc + 1, nactive 
                                                      sum = sum + l3(vdav, c,j,b,k,d,l) * t2(c,a,k,j)
                                                end do
                                          end do
                                    end do
                                    xi_interm_7_cc3(b, d, l, a) = xi_interm_7_cc3(b, d, l, a) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 




      end subroutine xi_intermediates_cc3

      subroutine gamma_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, vdav) 
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            double precision, dimension(:), intent(in) :: vdav
            integer :: a, i, j, b, k, c 
            real(F64) :: sum

            !$omp parallel private(b, k, j, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do k = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = sum + r1(vdav, b,k) * s2(b,a,j,k)
                              end do
                        end do
                        gamma_interm_1_ccsd(j, a) = gamma_interm_1_ccsd(j, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, a, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do a = nocc + 1, nactive 
                  do j = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = sum + r1(vdav, b,k) * s2(b,a,k,j)
                              end do
                        end do
                        gamma_interm_2_ccsd(a, j) = gamma_interm_2_ccsd(a, j) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(k, c, j, b, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do b = nocc + 1, nactive 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do k = 1, nocc 
                              do j = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          sum = sum + s2(c,b,j,k) * t2(c,a,k,j)
                                    end do
                              end do
                        end do
                        gamma_interm_3_ccsd(b, a) = gamma_interm_3_ccsd(b, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, i, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do i = 1, nocc 
                  do j = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,i,c,k) * s2(b,c,k,j)
                                    end do
                              end do
                        end do
                        gamma_interm_4_ccsd(i, j) = gamma_interm_4_ccsd(i, j) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, i, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do i = 1, nocc 
                  do j = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,i,c,k) * s2(b,c,j,k)
                                    end do
                              end do
                        end do
                        gamma_interm_5_ccsd(i, j) = gamma_interm_5_ccsd(i, j) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, k, c, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = sum + r1(vdav, b,j) * s2(b,c,k,j)
                              end do
                        end do
                        gamma_interm_6_ccsd(k, c) = gamma_interm_6_ccsd(k, c) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, c, k, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do c = nocc + 1, nactive 
                  do k = 1, nocc 
                        sum = zero 
                        do j = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = sum + r1(vdav, b,j) * s2(b,c,j,k)
                              end do
                        end do
                        gamma_interm_7_ccsd(c, k) = gamma_interm_7_ccsd(c, k) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, c, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + s2(b,c,k,j) * t2(b,c,i,k)
                                    end do
                              end do
                        end do
                        gamma_interm_8_ccsd(j, i) = gamma_interm_8_ccsd(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + s2(b,c,j,k) * t2(b,c,i,k)
                                    end do
                              end do
                        end do
                        gamma_interm_9_ccsd(j, i) = gamma_interm_9_ccsd(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, b, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do b = nocc + 1, nactive 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do k = 1, nocc 
                              do j = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          sum = sum + r2(vdav, c,k,b,j) * s2(c,a,j,k)
                                    end do
                              end do
                        end do
                        gamma_interm_10_ccsd(b, a) = gamma_interm_10_ccsd(b, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, c, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,k,c,j) * s2(b,c,i,k)
                                    end do
                              end do
                        end do
                        gamma_interm_11_ccsd(j, i) = gamma_interm_11_ccsd(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,j,c,k) * s2(b,c,i,k)
                                    end do
                              end do
                        end do
                        gamma_interm_12_ccsd(j, i) = gamma_interm_12_ccsd(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,j,c,k) * s2(b,c,k,i)
                                    end do
                              end do
                        end do
                        gamma_interm_13_ccsd(j, i) = gamma_interm_13_ccsd(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, c, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do c = nocc + 1, nactive 
                              do k = 1, nocc 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,k,c,j) * s2(b,c,k,i)
                                    end do
                              end do
                        end do
                        gamma_interm_14_ccsd(j, i) = gamma_interm_14_ccsd(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, a, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do a = nocc + 1, nactive 
                  do b = nocc + 1, nactive 
                        sum = zero 
                        do k = 1, nocc 
                              do j = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          sum = sum + r2(vdav, c,k,a,j) * s2(c,b,j,k)
                                    end do
                              end do
                        end do
                        gamma_interm_15_ccsd(a, b) = gamma_interm_15_ccsd(a, b) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, c, k, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do c = nocc + 1, nactive 
                  do k = 1, nocc 
                        sum = zero 
                        do j = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = sum + r2(vdav, b,j,c,k) * s1(b,j)
                              end do
                        end do
                        gamma_interm_16_ccsd(c, k) = gamma_interm_16_ccsd(c, k) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              do c = nocc + 1, nactive 
                                    sum = sum + r2(vdav, c,k,b,j) * s1(c,j)
                              end do
                        end do
                        gamma_interm_17_ccsd(k, b) = gamma_interm_17_ccsd(k, b) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(c, j, i, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do i = 1, nocc 
                  do b = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              do c = nocc + 1, nactive 
                                    sum = sum + r1(vdav, c,j) * s2(c,b,i,j)
                              end do
                        end do
                        gamma_interm_18_ccsd(i, b) = gamma_interm_18_ccsd(i, b) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(c, j, b, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do b = nocc + 1, nactive 
                  do i = 1, nocc 
                        sum = zero 
                        do j = 1, nocc 
                              do c = nocc + 1, nactive 
                                    sum = sum + r1(vdav, c,j) * s2(c,b,j,i)
                              end do
                        end do
                        gamma_interm_19_ccsd(b, i) = gamma_interm_19_ccsd(b, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 



      end subroutine gamma_intermediates_ccsd

      subroutine gamma_intermediates_cc3(t2, t1, s2, s1, nocc, nactive, vdav) 
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            double precision, dimension(:), intent(in) :: vdav
            integer :: b, k, j, a, c, l, i, d 
            real(F64) :: sum

            !$omp parallel private(b, k, j, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do k = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = sum + r1(vdav, b,k) * s2(b,a,j,k)
                              end do
                        end do
                        gamma_interm_1_cc3(j, a) = gamma_interm_1_cc3(j, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, a, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do a = nocc + 1, nactive 
                  do j = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = sum + r1(vdav, b,k) * s2(b,a,k,j)
                              end do
                        end do
                        gamma_interm_2_cc3(a, j) = gamma_interm_2_cc3(a, j) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, l, j, k, a, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do j = 1, nocc 
                  do k = 1, nocc 
                        do a = nocc + 1, nactive 
                              do i = 1, nocc 
                                    sum = zero 
                                    do l = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                do b = nocc + 1, nactive 
                                                      sum = sum + t2(b,c,j,l) * t3(nocc, nactive, b,c,a,l,k,i)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_3_cc3(j, k, a, i) = gamma_interm_3_cc3(j, k, a, i) + sum  
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(a, l, b, j, i, c, k, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do j = 1, nocc 
                  do i = 1, nocc 
                        do c = nocc + 1, nactive 
                              do k = 1, nocc 
                                    sum = zero 
                                    do b = nocc + 1, nactive 
                                          do l = 1, nocc 
                                                do a = nocc + 1, nactive 
                                                      sum = sum + t2(a,b,l,j) * t3(nocc, nactive, a,b,c,i,l,k)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_4_cc3(j, i, c, k) = gamma_interm_4_cc3(j, i, c, k) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(a, l, b, j, i, k, c, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do j = 1, nocc 
                  do i = 1, nocc 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    sum = zero 
                                    do l = 1, nocc 
                                          do b = nocc + 1, nactive 
                                                do a = nocc + 1, nactive 
                                                      sum = sum + t2(a,b,l,j) * t3(nocc, nactive, a,b,c,i,k,l)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_5_cc3(j, i, k, c) = gamma_interm_5_cc3(j, i, k, c) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(a, c, l, j, i, k, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do j = 1, nocc 
                  do i = 1, nocc 
                        do k = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = zero 
                                    do l = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                do a = nocc + 1, nactive 
                                                      sum = sum + t2(a,c,j,l) * t3(nocc, nactive, a,c,b,i,k,l)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_6_cc3(j, i, k, b) = gamma_interm_6_cc3(j, i, k, b) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, l, j, k, a, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do j = 1, nocc 
                  do k = 1, nocc 
                        do a = nocc + 1, nactive 
                              do i = 1, nocc 
                                    sum = zero 
                                    do c = nocc + 1, nactive 
                                          do l = 1, nocc 
                                                do b = nocc + 1, nactive 
                                                      sum = sum + t2(b,c,j,l) * t3(nocc, nactive, b,c,a,k,l,i)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_7_cc3(j, k, a, i) = gamma_interm_7_cc3(j, k, a, i) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(a, k, c, l, i, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do i = 1, nocc 
                  do b = nocc + 1, nactive 
                        sum = zero 
                        do k = 1, nocc 
                              do l = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          do a = nocc + 1, nactive 
                                                sum = sum + r3(vdav, a,i,c,l,b,k) * s2(a,c,k,l)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_8_cc3(i, b) = gamma_interm_8_cc3(i, b) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, c, l, a, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do a = nocc + 1, nactive 
                  do i = 1, nocc 
                        sum = zero 
                        do l = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do k = 1, nocc 
                                          do b = nocc + 1, nactive 
                                                sum = sum + r3(vdav, b,k,c,l,a,i) * s2(b,c,k,l)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_9_cc3(a, i) = gamma_interm_9_cc3(a, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(a, c, l, i, b, k, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do i = 1, nocc 
                  do b = nocc + 1, nactive 
                        do k = 1, nocc 
                              do j = 1, nocc 
                                    sum = zero 
                                    do l = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                do a = nocc + 1, nactive 
                                                      sum = sum + r3(vdav, a,i,c,l,b,k) * s2(a,c,l,j)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_10_cc3(i, b, k, j) = gamma_interm_10_cc3(i, b, k, j) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(a, c, l, i, b, k, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do i = 1, nocc 
                  do b = nocc + 1, nactive 
                        do k = 1, nocc 
                              do j = 1, nocc 
                                    sum = zero 
                                    do l = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                do a = nocc + 1, nactive 
                                                      sum = sum + r3(vdav, a,i,c,l,b,k) * s2(a,c,j,l)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_11_cc3(i, b, k, j) = gamma_interm_11_cc3(i, b, k, j) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(a, b, l, i, k, c, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do i = 1, nocc 
                  do k = 1, nocc 
                        do c = nocc + 1, nactive 
                              do j = 1, nocc 
                                    sum = zero 
                                    do l = 1, nocc 
                                          do b = nocc + 1, nactive 
                                                do a = nocc + 1, nactive 
                                                      sum = sum + r3(vdav, a,i,b,k,c,l) * s2(a,b,j,l)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_12_cc3(i, k, c, j) = gamma_interm_12_cc3(i, k, c, j) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(a, b, l, i, k, c, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do i = 1, nocc 
                  do k = 1, nocc 
                        do c = nocc + 1, nactive 
                              do j = 1, nocc 
                                    sum = zero 
                                    do l = 1, nocc 
                                          do b = nocc + 1, nactive 
                                                do a = nocc + 1, nactive 
                                                      sum = sum + r3(vdav, a,i,b,k,c,l) * s2(a,b,l,j)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_13_cc3(i, k, c, j) = gamma_interm_13_cc3(i, k, c, j) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, c, l, a, i, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do l = 1, nocc 
                  do a = nocc + 1, nactive 
                        do i = 1, nocc 
                              do j = 1, nocc 
                                    sum = zero 
                                    do c = nocc + 1, nactive 
                                          do k = 1, nocc 
                                                do b = nocc + 1, nactive 
                                                      sum = sum + r3(vdav, b,k,c,l,a,i) * s2(b,c,k,j)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_14_cc3(l, a, i, j) = gamma_interm_14_cc3(l, a, i, j) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, c, l, a, i, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do l = 1, nocc 
                  do a = nocc + 1, nactive 
                        do i = 1, nocc 
                              do j = 1, nocc 
                                    sum = zero 
                                    do k = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                do b = nocc + 1, nactive 
                                                      sum = sum + r3(vdav, b,k,c,l,a,i) * s2(b,c,j,k)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_15_cc3(l, a, i, j) = gamma_interm_15_cc3(l, a, i, j) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, i, l, d, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do i = 1, nocc 
                  do l = 1, nocc 
                        do d = nocc + 1, nactive 
                              do j = 1, nocc 
                                    sum = zero 
                                    do k = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                do b = nocc + 1, nactive 
                                                      sum = sum + r2(vdav, b,i,c,k) * t3(nocc, nactive, b,c,d,k,l,j)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_16_cc3(i, l, d, j) = gamma_interm_16_cc3(i, l, d, j) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, i, l, d, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do i = 1, nocc 
                  do l = 1, nocc 
                        do d = nocc + 1, nactive 
                              do j = 1, nocc 
                                    sum = zero 
                                    do c = nocc + 1, nactive 
                                          do k = 1, nocc 
                                                do b = nocc + 1, nactive 
                                                      sum = sum + r2(vdav, b,i,c,k) * t3(nocc, nactive, b,c,d,l,k,j)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_17_cc3(i, l, d, j) = gamma_interm_17_cc3(i, l, d, j) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, i, l, j, d, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do i = 1, nocc 
                  do l = 1, nocc 
                        do j = 1, nocc 
                              do d = nocc + 1, nactive 
                                    sum = zero 
                                    do k = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                do b = nocc + 1, nactive 
                                                      sum = sum + r2(vdav, b,i,c,k) * t3(nocc, nactive, b,c,d,l,j,k)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_18_cc3(i, l, j, d) = gamma_interm_18_cc3(i, l, j, d) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(k, c, j, b, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do b = nocc + 1, nactive 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do k = 1, nocc 
                              do j = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          sum = sum + s2(c,b,j,k) * t2(c,a,k,j)
                                    end do
                              end do
                        end do
                        gamma_interm_19_cc3(b, a) = gamma_interm_19_cc3(b, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, i, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do i = 1, nocc 
                  do j = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,i,c,k) * s2(b,c,k,j)
                                    end do
                              end do
                        end do
                        gamma_interm_20_cc3(i, j) = gamma_interm_20_cc3(i, j) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, i, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do i = 1, nocc 
                  do j = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,i,c,k) * s2(b,c,j,k)
                                    end do
                              end do
                        end do
                        gamma_interm_21_cc3(i, j) = gamma_interm_21_cc3(i, j) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, c, k, l, d, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do j = 1, nocc 
                                          do b = nocc + 1, nactive 
                                                sum = sum + r2(vdav, b,j,c,k) * t3(nocc, nactive, b,c,d,l,j,k)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_22_cc3(l, d) = gamma_interm_22_cc3(l, d) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, c, k, l, d, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              do k = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          do b = nocc + 1, nactive 
                                                sum = sum + r2(vdav, b,j,c,k) * t3(nocc, nactive, b,c,d,k,l,j)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_23_cc3(l, d) = gamma_interm_23_cc3(l, d) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, c, k, d, l, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do d = nocc + 1, nactive 
                  do l = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do j = 1, nocc 
                                          do b = nocc + 1, nactive 
                                                sum = sum + r2(vdav, b,j,c,k) * t3(nocc, nactive, b,c,d,k,j,l)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_24_cc3(d, l) = gamma_interm_24_cc3(d, l) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, a, b, l, d, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do a = nocc + 1, nactive 
                  do b = nocc + 1, nactive 
                        do l = 1, nocc 
                              do d = nocc + 1, nactive 
                                    sum = zero 
                                    do k = 1, nocc 
                                          do j = 1, nocc 
                                                do c = nocc + 1, nactive 
                                                      sum = sum + r2(vdav, c,k,a,j) * t3(nocc, nactive, c,b,d,j,l,k)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_25_cc3(a, b, l, d) = gamma_interm_25_cc3(a, b, l, d) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, a, l, b, d, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do a = nocc + 1, nactive 
                  do l = 1, nocc 
                        do b = nocc + 1, nactive 
                              do d = nocc + 1, nactive 
                                    sum = zero 
                                    do j = 1, nocc 
                                          do k = 1, nocc 
                                                do c = nocc + 1, nactive 
                                                      sum = sum + r2(vdav, c,k,a,j) * t3(nocc, nactive, c,b,d,l,k,j)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_26_cc3(a, l, b, d) = gamma_interm_26_cc3(a, l, b, d) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, a, b, d, l, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do a = nocc + 1, nactive 
                  do b = nocc + 1, nactive 
                        do d = nocc + 1, nactive 
                              do l = 1, nocc 
                                    sum = zero 
                                    do j = 1, nocc 
                                          do k = 1, nocc 
                                                do c = nocc + 1, nactive 
                                                      sum = sum + r2(vdav, c,k,a,j) * t3(nocc, nactive, c,b,d,j,k,l)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_27_cc3(a, b, d, l) = gamma_interm_27_cc3(a, b, d, l) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, c, l, k, d, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do k = 1, nocc 
                  do d = nocc + 1, nactive 
                        sum = zero 
                        do l = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do j = 1, nocc 
                                          do b = nocc + 1, nactive 
                                                sum = sum + r3(vdav, b,j,c,k,d,l) * s2(b,c,j,l)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_28_cc3(k, d) = gamma_interm_28_cc3(k, d) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, c, k, d, l, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do d = nocc + 1, nactive 
                  do l = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do j = 1, nocc 
                                          do b = nocc + 1, nactive 
                                                sum = sum + r3(vdav, b,j,c,k,d,l) * s2(b,c,j,k)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_29_cc3(d, l) = gamma_interm_29_cc3(d, l) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, d, l, k, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              do l = 1, nocc 
                                    do d = nocc + 1, nactive 
                                          do c = nocc + 1, nactive 
                                                sum = sum + r3(vdav, c,k,d,l,b,j) * s2(c,d,j,l)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_30_cc3(k, b) = gamma_interm_30_cc3(k, b) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, k, c, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = sum + r1(vdav, b,j) * s2(b,c,k,j)
                              end do
                        end do
                        gamma_interm_31_cc3(k, c) = gamma_interm_31_cc3(k, c) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, c, k, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do c = nocc + 1, nactive 
                  do k = 1, nocc 
                        sum = zero 
                        do j = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = sum + r1(vdav, b,j) * s2(b,c,j,k)
                              end do
                        end do
                        gamma_interm_32_cc3(c, k) = gamma_interm_32_cc3(c, k) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, c, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + s2(b,c,k,j) * t2(b,c,i,k)
                                    end do
                              end do
                        end do
                        gamma_interm_33_cc3(j, i) = gamma_interm_33_cc3(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + s2(b,c,j,k) * t2(b,c,i,k)
                                    end do
                              end do
                        end do
                        gamma_interm_34_cc3(j, i) = gamma_interm_34_cc3(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, b, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do b = nocc + 1, nactive 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do k = 1, nocc 
                              do j = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          sum = sum + r2(vdav, c,k,b,j) * s2(c,a,j,k)
                                    end do
                              end do
                        end do
                        gamma_interm_35_cc3(b, a) = gamma_interm_35_cc3(b, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, c, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,k,c,j) * s2(b,c,i,k)
                                    end do
                              end do
                        end do
                        gamma_interm_36_cc3(j, i) = gamma_interm_36_cc3(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,j,c,k) * s2(b,c,i,k)
                                    end do
                              end do
                        end do
                        gamma_interm_37_cc3(j, i) = gamma_interm_37_cc3(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, c, k, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,j,c,k) * s2(b,c,k,i)
                                    end do
                              end do
                        end do
                        gamma_interm_38_cc3(j, i) = gamma_interm_38_cc3(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, k, c, j, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do i = 1, nocc 
                        sum = zero 
                        do c = nocc + 1, nactive 
                              do k = 1, nocc 
                                    do b = nocc + 1, nactive 
                                          sum = sum + r2(vdav, b,k,c,j) * s2(b,c,k,i)
                                    end do
                              end do
                        end do
                        gamma_interm_39_cc3(j, i) = gamma_interm_39_cc3(j, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, a, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do a = nocc + 1, nactive 
                  do b = nocc + 1, nactive 
                        sum = zero 
                        do k = 1, nocc 
                              do j = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          sum = sum + r2(vdav, c,k,a,j) * s2(c,b,j,k)
                                    end do
                              end do
                        end do
                        gamma_interm_40_cc3(a, b) = gamma_interm_40_cc3(a, b) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(b, j, c, k, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do c = nocc + 1, nactive 
                  do k = 1, nocc 
                        sum = zero 
                        do j = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = sum + r2(vdav, b,j,c,k) * s1(b,j)
                              end do
                        end do
                        gamma_interm_41_cc3(c, k) = gamma_interm_41_cc3(c, k) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, c, k, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              do c = nocc + 1, nactive 
                                    sum = sum + r2(vdav, c,k,b,j) * s1(c,j)
                              end do
                        end do
                        gamma_interm_42_cc3(k, b) = gamma_interm_42_cc3(k, b) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(c, j, i, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do i = 1, nocc 
                  do b = nocc + 1, nactive 
                        sum = zero 
                        do j = 1, nocc 
                              do c = nocc + 1, nactive 
                                    sum = sum + r1(vdav, c,j) * s2(c,b,i,j)
                              end do
                        end do
                        gamma_interm_43_cc3(i, b) = gamma_interm_43_cc3(i, b) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(c, j, b, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do b = nocc + 1, nactive 
                  do i = 1, nocc 
                        sum = zero 
                        do j = 1, nocc 
                              do c = nocc + 1, nactive 
                                    sum = sum + r1(vdav, c,j) * s2(c,b,j,i)
                              end do
                        end do
                        gamma_interm_44_cc3(b, i) = gamma_interm_44_cc3(b, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(i, d, j, a, k, b, c, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do a = nocc + 1, nactive 
                  do k = 1, nocc 
                        do b = nocc + 1, nactive 
                              do c = nocc + 1, nactive 
                                    sum = zero 
                                    do i = 1, nocc 
                                          do j = 1, nocc 
                                                do d = nocc + 1, nactive 
                                                      sum = sum + t2(d,a,j,i) * t3(nocc, nactive, d,b,c,k,j,i)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_45_cc3(a, k, b, c) = gamma_interm_45_cc3(a, k, b, c) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(i, d, j, a, b, c, k, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do a = nocc + 1, nactive 
                  do b = nocc + 1, nactive 
                        do c = nocc + 1, nactive 
                              do k = 1, nocc 
                                    sum = zero 
                                    do i = 1, nocc 
                                          do j = 1, nocc 
                                                do d = nocc + 1, nactive 
                                                      sum = sum + t2(d,a,j,i) * t3(nocc, nactive, d,b,c,i,j,k)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_46_cc3(a, b, c, k) = gamma_interm_46_cc3(a, b, c, k) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(i, d, k, a, b, j, c, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do a = nocc + 1, nactive 
                  do b = nocc + 1, nactive 
                        do j = 1, nocc 
                              do c = nocc + 1, nactive 
                                    sum = zero 
                                    do k = 1, nocc 
                                          do i = 1, nocc 
                                                do d = nocc + 1, nactive 
                                                      sum = sum + t2(d,a,k,i) * t3(nocc, nactive, d,b,c,i,j,k)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_47_cc3(a, b, j, c) = gamma_interm_47_cc3(a, b, j, c) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(i, c, k, d, j, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do i = 1, nocc 
                              do d = nocc + 1, nactive 
                                    do k = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                sum = sum + r3(vdav, c,k,d,j,a,i) * s2(c,d,k,i)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_48_cc3(j, a) = gamma_interm_48_cc3(j, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(c, j, d, k, a, i, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do a = nocc + 1, nactive 
                  do i = 1, nocc 
                        sum = zero 
                        do k = 1, nocc 
                              do d = nocc + 1, nactive 
                                    do j = 1, nocc 
                                          do c = nocc + 1, nactive 
                                                sum = sum + r3(vdav, c,j,d,k,a,i) * s2(c,d,j,k)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_49_cc3(a, i) = gamma_interm_49_cc3(a, i) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(i, c, d, k, j, a, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do j = 1, nocc 
                  do a = nocc + 1, nactive 
                        sum = zero 
                        do i = 1, nocc 
                              do k = 1, nocc 
                                    do d = nocc + 1, nactive 
                                          do c = nocc + 1, nactive 
                                                sum = sum + r3(vdav, c,j,d,k,a,i) * s2(c,d,i,k)
                                          end do
                                    end do
                              end do
                        end do
                        gamma_interm_50_cc3(j, a) = gamma_interm_50_cc3(j, a) + sum 
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(i, d, k, a, c, j, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do a = nocc + 1, nactive 
                  do c = nocc + 1, nactive 
                        do j = 1, nocc 
                              do b = nocc + 1, nactive 
                                    sum = zero 
                                    do k = 1, nocc 
                                          do i = 1, nocc 
                                                do d = nocc + 1, nactive 
                                                      sum = sum + r3(vdav, d,k,a,i,c,j) * s2(d,b,i,k)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_51_cc3(a, c, j, b) = gamma_interm_51_cc3(a, c, j, b) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(i, c, k, j, a, d, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do j = 1, nocc 
                  do a = nocc + 1, nactive 
                        do d = nocc + 1, nactive 
                              do b = nocc + 1, nactive 
                                    sum = zero 
                                    do k = 1, nocc 
                                          do i = 1, nocc 
                                                do c = nocc + 1, nactive 
                                                      sum = sum + r3(vdav, c,j,a,i,d,k) * s2(c,b,i,k)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_52_cc3(j, a, d, b) = gamma_interm_52_cc3(j, a, d, b) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 

            !$omp parallel private(j, d, k, a, i, c, b, sum)& 
            !$omp default(shared)
            !$omp do collapse(4)
            do a = nocc + 1, nactive 
                  do i = 1, nocc 
                        do c = nocc + 1, nactive 
                              do b = nocc + 1, nactive 
                                    sum = zero 
                                    do j = 1, nocc 
                                          do k = 1, nocc 
                                                do d = nocc + 1, nactive 
                                                      sum = sum + r3(vdav, d,k,a,i,c,j) * s2(d,b,j,k)
                                                end do
                                          end do
                                    end do
                                    gamma_interm_53_cc3(a, i, c, b) = gamma_interm_53_cc3(a, i, c, b) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 



      end subroutine gamma_intermediates_cc3

      function calc_D_oo_xi_with_int(t2, t1, s2, s1, vdav, nocc, nactive, i,j) 
            double precision :: calc_D_oo_xi_with_int
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: i,j 
            integer :: s ,a,k,b 
            double precision, dimension(0:2) :: term 
            term = 0.d+0 
            do a = nocc + 1, nactive 
                  term(0) = term(0) + l1(vdav, a,j) * t1(a,i)
                  term(1) = term(1) + l2(vdav, a,j,a,j) * t2(a,a,i,j)
            end do

            term(0) = -term(0) 
            term(1) = term(1) * 0.49999999999999994d+0 

            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        do a = nocc + 1, nactive 
                              term(2) = term(2) + l2(vdav, a,i,b,k) * t2(a,b,j,k)
                        end do
                  end do
            end do

            term(2) = -term(2) 


            calc_D_oo_xi_with_int = 0.d+0 
            do s = 0, 2
                  calc_D_oo_xi_with_int = calc_D_oo_xi_with_int + term(s)
            end do

      end function calc_D_oo_xi_with_int

      function calc_D_vo_xi_with_int(t2, t1, s2, s1, vdav, nocc, nactive, a,i) 
            double precision :: calc_D_vo_xi_with_int
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: a,i 
            integer :: s ,j,b 
            double precision, dimension(0:6) :: term 
            term = 0.d+0 
            term(0) = term(0) + l1(vdav, a,i)


            do j = 1, nocc 
                  term(1) = term(1) + t1(a,j) * xi_interm_1_ccsd(j, i)
                  term(2) = term(2) + t1(a,j) * xi_interm_3_ccsd(j, i)
            end do

            term(1) = -term(1) 
            term(2) = term(2) * 0.49999999999999994d+0 

            do b = nocc + 1, nactive 
                  term(3) = term(3) + t1(b,i) * xi_interm_2_ccsd(b, a)
                  term(4) = term(4) + t1(b,i) * xi_interm_4_ccsd(b, a)
            end do

            term(3) = -term(3) 
            term(4) = term(4) * 0.49999999999999994d+0 

            do j = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(5) = term(5) + l1(vdav, b,j) * t2(a,b,i,j)
                  end do
            end do

            term(5) = term(5) * 2.0d+0 

            do b = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(6) = term(6) + l1(vdav, b,j) * t2(a,b,j,i)
                  end do
            end do

            term(6) = -term(6) 


            calc_D_vo_xi_with_int = 0.d+0 
            do s = 0, 6
                  calc_D_vo_xi_with_int = calc_D_vo_xi_with_int + term(s)
            end do

      end function calc_D_vo_xi_with_int

      function calc_D_vv_xi_with_int(t2, t1, s2, s1, vdav, nocc, nactive, a,b) 
            double precision :: calc_D_vv_xi_with_int
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: a,b 
            integer :: s ,i,j,c 
            double precision, dimension(0:2) :: term 
            term = 0.d+0 
            do i = 1, nocc 
                  term(0) = term(0) + l1(vdav, b,i) * t1(a,i)
                  term(1) = term(1) + l2(vdav, b,i,b,i) * t2(a,b,i,i)
            end do

            term(1) = term(1) * (-0.49999999999999994d+0) 

            do j = 1, nocc 
                  do c = nocc + 1, nactive 
                        do i = 1, nocc 
                              term(2) = term(2) + l2(vdav, a,i,c,j) * t2(b,c,i,j)
                        end do
                  end do
            end do



            calc_D_vv_xi_with_int = 0.d+0 
            do s = 0, 2
                  calc_D_vv_xi_with_int = calc_D_vv_xi_with_int + term(s)
            end do

      end function calc_D_vv_xi_with_int

      function calc_D_oo_xi_with_int_cc3(t2, t1, s2, s1, vdav, nocc, nactive, i,j) 
            double precision :: calc_D_oo_xi_with_int_cc3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: i,j 
            integer :: s ,a,k,b,l,c 
            double precision, dimension(0:6) :: term 
            term = 0.d+0 
            do a = nocc + 1, nactive 
                  term(0) = term(0) + l1(vdav, a,j) * t1(a,i)
                  term(1) = term(1) + l2(vdav, a,j,a,j) * t2(a,a,i,j)
            end do

            term(0) = -term(0) 
            term(1) = term(1) * 0.49999999999999994d+0 

            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        do a = nocc + 1, nactive 
                              term(2) = term(2) + l2(vdav, a,i,b,k) * t2(a,b,j,k)
                        end do
                  end do
            end do

            term(2) = -term(2) 

            do l = 1, nocc 
                  do k = 1, nocc 
                        do c = nocc + 1, nactive 
                              do b = nocc + 1, nactive 
                                    do a = nocc + 1, nactive 
                                          term(3) = term(3) + l3(vdav, a,i,b,k,c,l) * t3(nocc, nactive, a,b,c,l,j,k)
                                          term(4) = term(4) + l3(vdav, a,i,b,k,c,l) * t3(nocc, nactive, a,b,c,j,l,k)
                                    end do
                              end do
                        end do
                  end do
            end do

            term(3) = term(3) * (-1.9999999999999996d+0) 
            term(4) = term(4) * 1.9999999999999996d+0 

            do l = 1, nocc 
                  do c = nocc + 1, nactive 
                        do k = 1, nocc 
                              do b = nocc + 1, nactive 
                                    do a = nocc + 1, nactive 
                                          term(5) = term(5) + l3(vdav, a,i,b,k,c,l) * t3(nocc, nactive, a,b,c,k,j,l)
                                    end do
                              end do
                        end do
                  end do
            end do

            term(5) = term(5) * 3.999999999999999d+0 

            do l = 1, nocc 
                  do c = nocc + 1, nactive 
                        do b = nocc + 1, nactive 
                              do k = 1, nocc 
                                    do a = nocc + 1, nactive 
                                          term(6) = term(6) + l3(vdav, a,i,b,k,c,l) * t3(nocc, nactive, a,b,c,j,k,l)
                                    end do
                              end do
                        end do
                  end do
            end do

            term(6) = term(6) * (-3.999999999999999d+0) 


            calc_D_oo_xi_with_int_cc3 = 0.d+0 
            do s = 0, 6
                  calc_D_oo_xi_with_int_cc3 = calc_D_oo_xi_with_int_cc3 + term(s)
            end do

      end function calc_D_oo_xi_with_int_cc3

      function calc_D_vo_xi_with_int_cc3(t2, t1, s2, s1, vdav, nocc, nactive, a,i) 
            double precision :: calc_D_vo_xi_with_int_cc3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: a,i 
            integer :: s ,j,b,k,c,d,l 
            double precision, dimension(0:22) :: term 
            term = 0.d+0 
            do j = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(0) = term(0) + l1(vdav, b,j) * t2(a,b,i,j)
                        term(1) = term(1) + l2(vdav, b,j,b,j) * t3(nocc, nactive, a,b,b,i,j,j)
                        term(2) = term(2) + l2(vdav, b,j,b,j) * t3(nocc, nactive, a,b,b,j,i,j)
                  end do
            end do

            term(0) = term(0) * 2.0d+0 
            term(1) = term(1) * (-0.5d+0) 
            term(2) = term(2) * 0.5d+0 

            do b = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(3) = term(3) + l1(vdav, b,j) * t2(a,b,j,i)
                  end do
            end do

            term(3) = -term(3) 

            do l = 1, nocc 
                  do c = nocc + 1, nactive 
                        do b = nocc + 1, nactive 
                              term(4) = term(4) + t2(b,c,i,l) * xi_interm_6_cc3(l, b, c, a)
                              term(5) = term(5) + t2(b,c,i,l) * xi_interm_6_cc3(l, b, c, a)
                              term(6) = term(6) + t2(b,c,i,l) * xi_interm_7_cc3(b, c, l, a)
                        end do
                  end do
            end do

            term(4) = term(4) * 2.0d+0 
            term(5) = -term(5) 
            term(6) = term(6) * 2.0d+0 

            do j = 1, nocc 
                  term(7) = term(7) + t1(a,j) * xi_interm_1_cc3(j, i)
                  term(8) = term(8) + t1(a,j) * xi_interm_3_cc3(j, i)
            end do

            term(7) = term(7) * 0.49999999999999994d+0 
            term(8) = -term(8) 

            do b = nocc + 1, nactive 
                  term(9) = term(9) + t1(b,i) * xi_interm_2_cc3(b, a)
                  term(10) = term(10) + t1(b,i) * xi_interm_4_cc3(b, a)
            end do

            term(9) = term(9) * 0.49999999999999994d+0 
            term(10) = -term(10) 

            term(11) = term(11) + l1(vdav, a,i)


            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        do c = nocc + 1, nactive 
                              do j = 1, nocc 
                                    term(12) = term(12) + l2(vdav, b,j,c,k) * t3(nocc, nactive, a,b,c,i,j,k)
                              end do
                        end do
                  end do
            end do


            do k = 1, nocc 
                  do j = 1, nocc 
                        do b = nocc + 1, nactive 
                              do c = nocc + 1, nactive 
                                    term(13) = term(13) + l2(vdav, b,j,c,k) * t3(nocc, nactive, a,b,c,j,i,k)
                              end do
                        end do
                  end do
            end do

            term(13) = -term(13) 

            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        do b = nocc + 1, nactive 
                              term(14) = term(14) + t2(b,c,i,k) * xi_interm_5_cc3(b, k, c, a)
                              term(15) = term(15) + t2(b,c,i,k) * xi_interm_5_cc3(b, k, c, a)
                              term(16) = term(16) + t2(b,c,i,k) * xi_interm_7_cc3(b, c, k, a)
                        end do
                  end do
            end do

            term(14) = -term(14) 
            term(15) = term(15) * 2.0d+0 
            term(16) = term(16) * (-4.0d+0) 

            do k = 1, nocc 
                  do d = nocc + 1, nactive 
                        do b = nocc + 1, nactive 
                              term(17) = term(17) + t2(b,d,i,k) * xi_interm_5_cc3(b, k, d, a)
                              term(18) = term(18) + t2(b,d,i,k) * xi_interm_6_cc3(k, b, d, a)
                              term(19) = term(19) + t2(b,d,i,k) * xi_interm_5_cc3(b, k, d, a)
                        end do
                  end do
            end do

            term(17) = -term(17) 
            term(18) = term(18) * 2.0d+0 
            term(19) = term(19) * 2.0d+0 

            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        do b = nocc + 1, nactive 
                              term(20) = term(20) + t2(b,d,i,l) * xi_interm_6_cc3(l, b, d, a)
                              term(21) = term(21) + t2(b,d,i,l) * xi_interm_7_cc3(b, d, l, a)
                              term(22) = term(22) + t2(b,d,i,l) * xi_interm_7_cc3(b, d, l, a)
                        end do
                  end do
            end do

            term(20) = -term(20) 
            term(21) = term(21) * 2.0d+0 
            term(22) = term(22) * (-4.0d+0) 


            calc_D_vo_xi_with_int_cc3 = 0.d+0 
            do s = 0, 22
                  calc_D_vo_xi_with_int_cc3 = calc_D_vo_xi_with_int_cc3 + term(s)
            end do

      end function calc_D_vo_xi_with_int_cc3

      function calc_D_vv_xi_with_int_cc3(t2, t1, s2, s1, vdav, nocc, nactive, a,b) 
            double precision :: calc_D_vv_xi_with_int_cc3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: a,b 
            integer :: s ,i,j,c,k,d 
            double precision, dimension(0:8) :: term 
            term = 0.d+0 
            do i = 1, nocc 
                  term(0) = term(0) + l1(vdav, b,i) * t1(a,i)
                  term(1) = term(1) + l2(vdav, b,i,b,i) * t2(a,b,i,i)
            end do

            term(1) = term(1) * (-0.49999999999999994d+0) 

            do j = 1, nocc 
                  do c = nocc + 1, nactive 
                        do i = 1, nocc 
                              term(2) = term(2) + l2(vdav, a,i,c,j) * t2(b,c,i,j)
                        end do
                  end do
            end do


            do k = 1, nocc 
                  do i = 1, nocc 
                        do d = nocc + 1, nactive 
                              do j = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          term(3) = term(3) + l3(vdav, a,i,c,j,d,k) * t3(nocc, nactive, b,c,d,j,k,i)
                                    end do
                              end do
                        end do
                  end do
            end do


            do k = 1, nocc 
                  do j = 1, nocc 
                        do d = nocc + 1, nactive 
                              do i = 1, nocc 
                                    do c = nocc + 1, nactive 
                                          term(4) = term(4) + l3(vdav, a,i,c,j,d,k) * t3(nocc, nactive, b,c,d,i,k,j)
                                    end do
                              end do
                        end do
                  end do
            end do

            term(4) = term(4) * (-1.9999999999999996d+0) 

            do k = 1, nocc 
                  do j = 1, nocc 
                        do d = nocc + 1, nactive 
                              do c = nocc + 1, nactive 
                                    do i = 1, nocc 
                                          term(5) = term(5) + l3(vdav, a,i,c,k,d,j) * t3(nocc, nactive, b,c,d,j,i,k)
                                    end do
                              end do
                        end do
                  end do
            end do


            do k = 1, nocc 
                  do d = nocc + 1, nactive 
                        do j = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do i = 1, nocc 
                                          term(6) = term(6) + l3(vdav, a,i,c,j,d,k) * t3(nocc, nactive, b,c,d,j,i,k)
                                    end do
                              end do
                        end do
                  end do
            end do

            term(6) = term(6) * (-1.9999999999999996d+0) 

            do j = 1, nocc 
                  do i = 1, nocc 
                        do d = nocc + 1, nactive 
                              do c = nocc + 1, nactive 
                                    do k = 1, nocc 
                                          term(7) = term(7) + l3(vdav, a,i,c,k,d,j) * t3(nocc, nactive, b,c,d,j,k,i)
                                    end do
                              end do
                        end do
                  end do
            end do

            term(7) = term(7) * (-1.9999999999999996d+0) 

            do k = 1, nocc 
                  do d = nocc + 1, nactive 
                        do i = 1, nocc 
                              do c = nocc + 1, nactive 
                                    do j = 1, nocc 
                                          term(8) = term(8) + l3(vdav, a,i,c,j,d,k) * t3(nocc, nactive, b,c,d,i,j,k)
                                    end do
                              end do
                        end do
                  end do
            end do

            term(8) = term(8) * 3.999999999999999d+0 


            calc_D_vv_xi_with_int_cc3 = 0.d+0 
            do s = 0, 8
                  calc_D_vv_xi_with_int_cc3 = calc_D_vv_xi_with_int_cc3 + term(s)
            end do

      end function calc_D_vv_xi_with_int_cc3

     function calc_D_oo_gamma_with_int(t2, t1, s2, s1, vdav, nocc, nactive, i,j) 
            double precision :: calc_D_oo_gamma_with_int
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: i,j 
            integer :: s ,a,k,b 
            double precision, dimension(0:5) :: term 
            term = 0.d+0 
            do a = nocc + 1, nactive 
                  term(0) = term(0) + r1(vdav, a,i) * s1(a,j)
                  term(1) = term(1) + t1(a,i) * gamma_interm_1_ccsd(j, a)
                  term(2) = term(2) + t1(a,i) * gamma_interm_2_ccsd(a, j)
                  term(3) = term(3) + t1(a,j) * gamma_interm_2_ccsd(a, i)
            end do

            term(0) = term(0) * (-2.0d+0) 
            term(1) = term(1) * 2.0d+0 
            term(2) = term(2) * (-2.0d+0) 
            term(3) = term(3) * (-2.0d+0) 

            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        do a = nocc + 1, nactive 
                              term(4) = term(4) + r2(vdav, a,i,b,k) * s2(a,b,k,j)
                              term(5) = term(5) + r2(vdav, a,i,b,k) * s2(a,b,j,k)
                        end do
                  end do
            end do

            term(4) = term(4) * 2.0d+0 
            term(5) = term(5) * (-4.0d+0) 


            calc_D_oo_gamma_with_int = 0.d+0 
            do s = 0, 5
                  calc_D_oo_gamma_with_int = calc_D_oo_gamma_with_int + term(s)
            end do

      end function calc_D_oo_gamma_with_int

      function calc_D_ov_gamma_with_int(t2, t1, s2, s1, vdav, nocc, nactive, i,a) 
            double precision :: calc_D_ov_gamma_with_int
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: i,a 
            integer :: s ,b,j 
            double precision, dimension(0:3) :: term 
            term = 0.d+0 
            do b = nocc + 1, nactive 
                  term(0) = term(0) + r1(vdav, b,i) * gamma_interm_3_ccsd(b, a)
                  term(1) = term(1) + r1(vdav, b,i) * gamma_interm_3_ccsd(b, a)
            end do

            term(0) = term(0) * 2.0d+0 
            term(1) = term(1) * (-4.0d+0) 

            do j = 1, nocc 
                  term(2) = term(2) + t1(a,j) * gamma_interm_4_ccsd(i, j)
                  term(3) = term(3) + t1(a,j) * gamma_interm_5_ccsd(i, j)
            end do

            term(2) = term(2) * 2.0d+0 
            term(3) = term(3) * (-4.0d+0) 


            calc_D_ov_gamma_with_int = 0.d+0 
            do s = 0, 3
                  calc_D_ov_gamma_with_int = calc_D_ov_gamma_with_int + term(s)
            end do

      end function calc_D_ov_gamma_with_int

      function calc_D_vo_gamma_with_int(t2, t1, s2, s1, vdav, nocc, nactive, a,i) 
            double precision :: calc_D_vo_gamma_with_int
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: a,i 
            integer :: s ,k,c,j,b 
            double precision, dimension(0:26) :: term 
            term = 0.d+0 
            term(0) = term(0) + r1(vdav, a,i)

            term(0) = term(0) * 2.0d+0 

            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(1) = term(1) + t2(a,c,i,k) * gamma_interm_6_ccsd(k, c)
                        term(2) = term(2) + t2(a,c,i,k) * gamma_interm_7_ccsd(c, k)
                        term(3) = term(3) + s2(a,c,i,k) * gamma_interm_16_ccsd(c, k)
                  end do
            end do

            term(1) = term(1) * (-4.0d+0) 
            term(2) = term(2) * 8.0d+0 
            term(3) = term(3) * 4.0d+0 

            do j = 1, nocc 
                  term(4) = term(4) + r1(vdav, a,j) * gamma_interm_8_ccsd(j, i)
                  term(5) = term(5) + r1(vdav, a,j) * gamma_interm_9_ccsd(j, i)
                  term(6) = term(6) + s1(a,j) * gamma_interm_11_ccsd(j, i)
                  term(7) = term(7) + s1(a,j) * gamma_interm_12_ccsd(j, i)
                  term(8) = term(8) + s1(a,j) * gamma_interm_13_ccsd(j, i)
                  term(9) = term(9) + s1(a,j) * gamma_interm_14_ccsd(j, i)
            end do

            term(4) = term(4) * 2.0d+0 
            term(5) = term(5) * (-4.0d+0) 
            term(6) = -term(6) 
            term(7) = term(7) * 2.0d+0 
            term(8) = -term(8) 
            term(9) = term(9) * 2.0d+0 

            do b = nocc + 1, nactive 
                  do k = 1, nocc 
                        term(10) = term(10) + s2(a,b,k,i) * gamma_interm_17_ccsd(k, b)
                  end do
            end do


            do b = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(11) = term(11) + t2(a,b,j,i) * gamma_interm_7_ccsd(b, j)
                        term(12) = term(12) + t2(a,b,j,i) * gamma_interm_6_ccsd(j, b)
                        term(13) = term(13) + s2(a,b,j,i) * gamma_interm_16_ccsd(b, j)
                        term(14) = term(14) + r1(vdav, b,j) * s2(a,b,j,i)
                        term(15) = term(15) + r2(vdav, a,j,b,i) * s1(b,j)
                  end do
            end do

            term(11) = term(11) * (-4.0d+0) 
            term(12) = term(12) * 2.0d+0 
            term(13) = term(13) * (-2.0d+0) 
            term(14) = term(14) * (-2.0d+0) 
            term(15) = term(15) * (-2.0d+0) 

            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(16) = term(16) + s2(a,b,i,k) * gamma_interm_17_ccsd(k, b)
                  end do
            end do

            term(16) = term(16) * (-2.0d+0) 

            do b = nocc + 1, nactive 
                  term(17) = term(17) + s1(b,i) * gamma_interm_10_ccsd(b, a)
                  term(18) = term(18) + s1(b,i) * gamma_interm_10_ccsd(b, a)
                  term(19) = term(19) + t1(b,i) * gamma_interm_15_ccsd(a, b)
                  term(20) = term(20) + t1(b,i) * gamma_interm_15_ccsd(a, b)
            end do

            term(17) = term(17) * (-2.0d+0) 
            term(18) = term(18) * 4.0d+0 
            term(19) = term(19) * 2.0d+0 
            term(20) = term(20) * (-4.0d+0) 

            do j = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(21) = term(21) + s2(a,b,i,j) * gamma_interm_16_ccsd(b, j)
                        term(22) = term(22) + r1(vdav, b,j) * s2(a,b,i,j)
                        term(23) = term(23) + r2(vdav, a,i,b,j) * s1(b,j)
                  end do
            end do

            term(21) = term(21) * 4.0d+0 
            term(22) = term(22) * 4.0d+0 
            term(23) = term(23) * 4.0d+0 

            do j = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(24) = term(24) + s2(a,c,i,j) * gamma_interm_17_ccsd(j, c)
                  end do
            end do

            term(24) = term(24) * (-2.0d+0) 

            do c = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(25) = term(25) + s2(a,c,j,i) * gamma_interm_17_ccsd(j, c)
                  end do
            end do


            do c = nocc + 1, nactive 
                  do k = 1, nocc 
                        term(26) = term(26) + s2(a,c,k,i) * gamma_interm_16_ccsd(c, k)
                  end do
            end do

            term(26) = term(26) * (-2.0d+0) 


            calc_D_vo_gamma_with_int = 0.d+0 
            do s = 0, 26
                  calc_D_vo_gamma_with_int = calc_D_vo_gamma_with_int + term(s)
            end do

      end function calc_D_vo_gamma_with_int

      function calc_D_vv_gamma_with_int(t2, t1, s2, s1, vdav, nocc, nactive, a,b) 
            double precision :: calc_D_vv_gamma_with_int
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: a,b 
            integer :: s ,i,j,c 
            double precision, dimension(0:5) :: term 
            term = 0.d+0 
            do i = 1, nocc 
                  term(0) = term(0) + r1(vdav, a,i) * s1(b,i)
                  term(1) = term(1) + t1(a,i) * gamma_interm_18_ccsd(i, b)
                  term(2) = term(2) + t1(a,i) * gamma_interm_19_ccsd(b, i)
                  term(3) = term(3) + t1(b,i) * gamma_interm_19_ccsd(a, i)
            end do

            term(0) = term(0) * 2.0d+0 
            term(1) = term(1) * (-2.0d+0) 
            term(2) = term(2) * 2.0d+0 
            term(3) = term(3) * 2.0d+0 

            do j = 1, nocc 
                  do i = 1, nocc 
                        do c = nocc + 1, nactive 
                              term(4) = term(4) + r2(vdav, a,i,c,j) * s2(b,c,j,i)
                        end do
                  end do
            end do

            term(4) = term(4) * (-2.0d+0) 

            do j = 1, nocc 
                  do c = nocc + 1, nactive 
                        do i = 1, nocc 
                              term(5) = term(5) + r2(vdav, a,i,c,j) * s2(b,c,i,j)
                        end do
                  end do
            end do

            term(5) = term(5) * 4.0d+0 


            calc_D_vv_gamma_with_int = 0.d+0 
            do s = 0, 5
                  calc_D_vv_gamma_with_int = calc_D_vv_gamma_with_int + term(s)
            end do

      end function calc_D_vv_gamma_with_int

      function calc_D_oo_gamma_with_int_cc3(t2, t1, s2, s1, vdav, nocc, nactive, i,j) 
            double precision :: calc_D_oo_gamma_with_int_cc3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: i,j 
            integer :: s ,a,k,b,c,l 
            double precision, dimension(0:27) :: term 
            term = 0.d+0 
            do a = nocc + 1, nactive 
                  term(0) = term(0) + r1(vdav, a,i) * s1(a,j)
                  term(1) = term(1) + t1(a,i) * gamma_interm_1_cc3(j, a)
                  term(2) = term(2) + t1(a,i) * gamma_interm_2_cc3(a, j)
                  term(3) = term(3) + t1(a,j) * gamma_interm_2_cc3(a, i)
                  term(4) = term(4) + s1(a,j) * gamma_interm_9_cc3(a, i)
                  term(5) = term(5) + s1(a,j) * gamma_interm_9_cc3(a, i)
            end do

            term(0) = term(0) * (-2.0d+0) 
            term(1) = term(1) * 2.0d+0 
            term(2) = term(2) * (-2.0d+0) 
            term(3) = term(3) * (-2.0d+0) 
            term(4) = term(4) * (-4.0d+0) 
            term(5) = term(5) * 2.0d+0 

            do b = nocc + 1, nactive 
                  do l = 1, nocc 
                        term(6) = term(6) + s1(b,l) * gamma_interm_12_cc3(i, l, b, j)
                        term(7) = term(7) + s1(b,l) * gamma_interm_13_cc3(i, l, b, j)
                  end do
            end do

            term(6) = term(6) * 2.0d+0 
            term(7) = -term(7) 

            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        do a = nocc + 1, nactive 
                              term(8) = term(8) + r2(vdav, a,i,b,k) * s2(a,b,k,j)
                              term(9) = term(9) + r2(vdav, a,i,b,k) * s2(a,b,j,k)
                        end do
                  end do
            end do

            term(8) = term(8) * 2.0d+0 
            term(9) = term(9) * (-4.0d+0) 

            do a = nocc + 1, nactive 
                  do k = 1, nocc 
                        term(10) = term(10) + r1(vdav, a,k) * gamma_interm_3_cc3(j, k, a, i)
                        term(11) = term(11) + r1(vdav, a,k) * gamma_interm_7_cc3(j, k, a, i)
                        term(12) = term(12) + s1(a,k) * gamma_interm_10_cc3(k, a, i, j)
                        term(13) = term(13) + s1(a,k) * gamma_interm_11_cc3(k, a, i, j)
                  end do
            end do

            term(10) = term(10) * (-1.9999999999999998d+0) 
            term(11) = term(11) * 4.0d+0 
            term(12) = -term(12) 
            term(13) = term(13) * 2.0d+0 

            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(14) = term(14) + r1(vdav, c,k) * gamma_interm_4_cc3(j, i, c, k)
                  end do
            end do

            term(14) = term(14) * 3.9999999999999996d+0 

            do c = nocc + 1, nactive 
                  do k = 1, nocc 
                        term(15) = term(15) + r1(vdav, c,k) * gamma_interm_5_cc3(j, i, k, c)
                        term(16) = term(16) + s1(c,k) * gamma_interm_12_cc3(i, k, c, j)
                        term(17) = term(17) + s1(c,k) * gamma_interm_13_cc3(i, k, c, j)
                  end do
            end do

            term(15) = term(15) * (-1.9999999999999998d+0) 
            term(16) = term(16) * 2.0d+0 
            term(17) = -term(17) 

            do b = nocc + 1, nactive 
                  do k = 1, nocc 
                        term(18) = term(18) + r1(vdav, b,k) * gamma_interm_6_cc3(j, i, k, b)
                  end do
            end do

            term(18) = term(18) * 4.0d+0 

            do a = nocc + 1, nactive 
                  do l = 1, nocc 
                        term(19) = term(19) + s1(a,l) * gamma_interm_14_cc3(l, a, i, j)
                        term(20) = term(20) + s1(a,l) * gamma_interm_15_cc3(l, a, i, j)
                  end do
            end do

            term(19) = term(19) * 2.0d+0 
            term(20) = -term(20) 

            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(21) = term(21) + r1(vdav, b,k) * gamma_interm_7_cc3(j, i, b, k)
                        term(22) = term(22) + s1(b,k) * gamma_interm_10_cc3(i, b, k, j)
                        term(23) = term(23) + s1(b,k) * gamma_interm_11_cc3(i, b, k, j)
                  end do
            end do

            term(21) = term(21) * (-7.999999999999999d+0) 
            term(22) = term(22) * 2.0d+0 
            term(23) = term(23) * (-4.0d+0) 

            do b = nocc + 1, nactive 
                  term(24) = term(24) + s1(b,j) * gamma_interm_8_cc3(i, b)
                  term(25) = term(25) + s1(b,j) * gamma_interm_8_cc3(i, b)
            end do

            term(24) = term(24) * 3.9999999999999996d+0 
            term(25) = term(25) * (-1.9999999999999998d+0) 

            do l = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(26) = term(26) + s1(c,l) * gamma_interm_11_cc3(i, c, l, j)
                        term(27) = term(27) + s1(c,l) * gamma_interm_10_cc3(i, c, l, j)
                  end do
            end do

            term(26) = term(26) * (-4.0d+0) 
            term(27) = term(27) * 2.0d+0 


            calc_D_oo_gamma_with_int_cc3 = 0.d+0 
            do s = 0, 27
                  calc_D_oo_gamma_with_int_cc3 = calc_D_oo_gamma_with_int_cc3 + term(s)
            end do

      end function calc_D_oo_gamma_with_int_cc3

      function calc_D_ov_gamma_with_int_cc3(t2, t1, s2, s1, vdav, nocc, nactive, i,a) 
            double precision :: calc_D_ov_gamma_with_int_cc3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: i,a 
            integer :: s ,l,j,d,b 
            double precision, dimension(0:9) :: term 
            term = 0.d+0 
            do l = 1, nocc 
                  do j = 1, nocc 
                        do d = nocc + 1, nactive 
                              term(0) = term(0) + t2(a,d,j,l) * gamma_interm_16_cc3(i, l, d, j)
                              term(1) = term(1) + t2(a,d,j,l) * gamma_interm_17_cc3(i, l, d, j)
                        end do
                  end do
            end do

            term(0) = term(0) * (-2.0d+0) 
            term(1) = term(1) * 4.0d+0 

            do j = 1, nocc 
                  term(2) = term(2) + t1(a,j) * gamma_interm_20_cc3(i, j)
                  term(3) = term(3) + t1(a,j) * gamma_interm_21_cc3(i, j)
            end do

            term(2) = term(2) * 2.0d+0 
            term(3) = term(3) * (-4.0d+0) 

            do d = nocc + 1, nactive 
                  do l = 1, nocc 
                        do j = 1, nocc 
                              term(4) = term(4) + t2(a,d,j,l) * gamma_interm_18_cc3(i, l, j, d)
                              term(5) = term(5) + t2(a,d,j,l) * gamma_interm_18_cc3(i, j, l, d)
                        end do
                  end do
            end do

            term(4) = term(4) * (-2.0d+0) 
            term(5) = term(5) * 4.0d+0 

            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        do j = 1, nocc 
                              term(6) = term(6) + t2(a,d,j,l) * gamma_interm_16_cc3(i, j, d, l)
                              term(7) = term(7) + t2(a,d,j,l) * gamma_interm_17_cc3(i, j, d, l)
                        end do
                  end do
            end do

            term(6) = term(6) * 4.0d+0 
            term(7) = term(7) * (-8.0d+0) 

            do b = nocc + 1, nactive 
                  term(8) = term(8) + r1(vdav, b,i) * gamma_interm_19_cc3(b, a)
                  term(9) = term(9) + r1(vdav, b,i) * gamma_interm_19_cc3(b, a)
            end do

            term(8) = term(8) * 2.0d+0 
            term(9) = term(9) * (-4.0d+0) 


            calc_D_ov_gamma_with_int_cc3 = 0.d+0 
            do s = 0, 9
                  calc_D_ov_gamma_with_int_cc3 = calc_D_ov_gamma_with_int_cc3 + term(s)
            end do

      end function calc_D_ov_gamma_with_int_cc3

      function calc_D_vo_gamma_with_int_cc3(t2, t1, s2, s1, vdav, nocc, nactive, a,i) 
            double precision :: calc_D_vo_gamma_with_int_cc3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: a,i 
            integer :: s ,l,d,b,j,k,c 
            double precision, dimension(0:88) :: term 
            term = 0.d+0 
            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        term(0) = term(0) + t2(a,d,i,l) * gamma_interm_22_cc3(l, d)
                        term(1) = term(1) + t2(a,d,i,l) * gamma_interm_22_cc3(l, d)
                        term(2) = term(2) + t2(a,d,i,l) * gamma_interm_23_cc3(l, d)
                        term(3) = term(3) + t2(a,d,i,l) * gamma_interm_23_cc3(l, d)
                        term(4) = term(4) + t2(a,d,i,l) * gamma_interm_24_cc3(d, l)
                        term(5) = term(5) + t2(a,d,i,l) * gamma_interm_24_cc3(d, l)
                        term(6) = term(6) + s2(a,d,i,l) * gamma_interm_29_cc3(d, l)
                        term(7) = term(7) + s2(a,d,i,l) * gamma_interm_29_cc3(d, l)
                  end do
            end do

            term(0) = term(0) * 2.0d+0 
            term(1) = term(1) * (-4.0d+0) 
            term(2) = term(2) * 2.0d+0 
            term(3) = term(3) * (-4.0d+0) 
            term(4) = term(4) * (-4.0d+0) 
            term(5) = term(5) * 8.0d+0 
            term(6) = term(6) * 2.6666666666666665d+0 
            term(7) = term(7) * (-1.3333333333333333d+0) 

            do j = 1, nocc 
                  term(8) = term(8) + r1(vdav, a,j) * gamma_interm_33_cc3(j, i)
                  term(9) = term(9) + r1(vdav, a,j) * gamma_interm_34_cc3(j, i)
                  term(10) = term(10) + s1(a,j) * gamma_interm_36_cc3(j, i)
                  term(11) = term(11) + s1(a,j) * gamma_interm_37_cc3(j, i)
                  term(12) = term(12) + s1(a,j) * gamma_interm_38_cc3(j, i)
                  term(13) = term(13) + s1(a,j) * gamma_interm_39_cc3(j, i)
            end do

            term(8) = term(8) * 2.0d+0 
            term(9) = term(9) * (-4.0d+0) 
            term(10) = -term(10) 
            term(11) = term(11) * 2.0d+0 
            term(12) = -term(12) 
            term(13) = term(13) * 2.0d+0 

            do b = nocc + 1, nactive 
                  term(14) = term(14) + s1(b,i) * gamma_interm_35_cc3(b, a)
                  term(15) = term(15) + s1(b,i) * gamma_interm_35_cc3(b, a)
                  term(16) = term(16) + t1(b,i) * gamma_interm_40_cc3(a, b)
                  term(17) = term(17) + t1(b,i) * gamma_interm_40_cc3(a, b)
            end do

            term(14) = term(14) * (-2.0d+0) 
            term(15) = term(15) * 4.0d+0 
            term(16) = term(16) * 2.0d+0 
            term(17) = term(17) * (-4.0d+0) 

            do d = nocc + 1, nactive 
                  do l = 1, nocc 
                        do b = nocc + 1, nactive 
                              term(18) = term(18) + t2(b,d,i,l) * gamma_interm_25_cc3(a, b, l, d)
                              term(19) = term(19) + t2(b,d,i,l) * gamma_interm_25_cc3(a, b, l, d)
                              term(20) = term(20) + t2(b,d,i,l) * gamma_interm_26_cc3(a, l, b, d)
                              term(21) = term(21) + t2(b,d,i,l) * gamma_interm_26_cc3(a, l, b, d)
                        end do
                  end do
            end do

            term(18) = term(18) * (-2.0d+0) 
            term(19) = term(19) * 4.0d+0 
            term(20) = term(20) * (-2.0d+0) 
            term(21) = term(21) * 4.0d+0 

            do l = 1, nocc 
                  do d = nocc + 1, nactive 
                        do b = nocc + 1, nactive 
                              term(22) = term(22) + t2(b,d,i,l) * gamma_interm_27_cc3(a, b, d, l)
                              term(23) = term(23) + t2(b,d,i,l) * gamma_interm_27_cc3(a, b, d, l)
                        end do
                  end do
            end do

            term(22) = term(22) * 4.0d+0 
            term(23) = term(23) * (-8.0d+0) 

            do b = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(24) = term(24) + t2(a,b,j,i) * gamma_interm_24_cc3(b, j)
                        term(25) = term(25) + t2(a,b,j,i) * gamma_interm_24_cc3(b, j)
                        term(26) = term(26) + t2(a,b,j,i) * gamma_interm_22_cc3(j, b)
                        term(27) = term(27) + t2(a,b,j,i) * gamma_interm_22_cc3(j, b)
                        term(28) = term(28) + t2(a,b,j,i) * gamma_interm_23_cc3(j, b)
                        term(29) = term(29) + t2(a,b,j,i) * gamma_interm_23_cc3(j, b)
                        term(30) = term(30) + s2(a,b,j,i) * gamma_interm_29_cc3(b, j)
                        term(31) = term(31) + s2(a,b,j,i) * gamma_interm_29_cc3(b, j)
                        term(32) = term(32) + t2(a,b,j,i) * gamma_interm_32_cc3(b, j)
                        term(33) = term(33) + t2(a,b,j,i) * gamma_interm_31_cc3(j, b)
                        term(34) = term(34) + s2(a,b,j,i) * gamma_interm_41_cc3(b, j)
                        term(35) = term(35) + r1(vdav, b,j) * s2(a,b,j,i)
                        term(36) = term(36) + r2(vdav, a,j,b,i) * s1(b,j)
                  end do
            end do

            term(24) = term(24) * 2.0d+0 
            term(25) = term(25) * (-4.0d+0) 
            term(26) = -term(26) 
            term(27) = term(27) * 2.0d+0 
            term(28) = -term(28) 
            term(29) = term(29) * 2.0d+0 
            term(30) = term(30) * (-1.3333333333333333d+0) 
            term(31) = term(31) * 0.6666666666666666d+0 
            term(32) = term(32) * (-4.0d+0) 
            term(33) = term(33) * 2.0d+0 
            term(34) = term(34) * (-2.0d+0) 
            term(35) = term(35) * (-2.0d+0) 
            term(36) = term(36) * (-2.0d+0) 

            do d = nocc + 1, nactive 
                  do k = 1, nocc 
                        term(37) = term(37) + s2(a,d,k,i) * gamma_interm_28_cc3(k, d)
                        term(38) = term(38) + s2(a,d,k,i) * gamma_interm_28_cc3(k, d)
                  end do
            end do

            term(37) = term(37) * 0.6666666666666666d+0 
            term(38) = term(38) * (-0.3333333333333333d+0) 

            do d = nocc + 1, nactive 
                  do l = 1, nocc 
                        term(39) = term(39) + s2(a,d,l,i) * gamma_interm_29_cc3(d, l)
                        term(40) = term(40) + s2(a,d,l,i) * gamma_interm_29_cc3(d, l)
                  end do
            end do

            term(39) = term(39) * (-1.3333333333333333d+0) 
            term(40) = term(40) * 0.6666666666666666d+0 

            do k = 1, nocc 
                  do d = nocc + 1, nactive 
                        term(41) = term(41) + s2(a,d,i,k) * gamma_interm_28_cc3(k, d)
                        term(42) = term(42) + s2(a,d,i,k) * gamma_interm_28_cc3(k, d)
                  end do
            end do

            term(41) = term(41) * (-1.3333333333333333d+0) 
            term(42) = term(42) * 0.6666666666666666d+0 

            do c = nocc + 1, nactive 
                  do l = 1, nocc 
                        term(43) = term(43) + s2(a,c,l,i) * gamma_interm_28_cc3(l, c)
                        term(44) = term(44) + s2(a,c,l,i) * gamma_interm_28_cc3(l, c)
                  end do
            end do

            term(43) = term(43) * 0.6666666666666666d+0 
            term(44) = term(44) * (-0.3333333333333333d+0) 

            do l = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(45) = term(45) + s2(a,c,i,l) * gamma_interm_28_cc3(l, c)
                        term(46) = term(46) + s2(a,c,i,l) * gamma_interm_28_cc3(l, c)
                  end do
            end do

            term(45) = term(45) * (-1.3333333333333333d+0) 
            term(46) = term(46) * 0.6666666666666666d+0 

            do c = nocc + 1, nactive 
                  do k = 1, nocc 
                        term(47) = term(47) + s2(a,c,k,i) * gamma_interm_29_cc3(c, k)
                        term(48) = term(48) + s2(a,c,k,i) * gamma_interm_29_cc3(c, k)
                        term(49) = term(49) + s2(a,c,k,i) * gamma_interm_41_cc3(c, k)
                  end do
            end do

            term(47) = term(47) * (-1.3333333333333333d+0) 
            term(48) = term(48) * 0.6666666666666666d+0 
            term(49) = term(49) * (-2.0d+0) 

            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(50) = term(50) + s2(a,c,i,k) * gamma_interm_29_cc3(c, k)
                        term(51) = term(51) + s2(a,c,i,k) * gamma_interm_29_cc3(c, k)
                        term(52) = term(52) + t2(a,c,i,k) * gamma_interm_31_cc3(k, c)
                        term(53) = term(53) + t2(a,c,i,k) * gamma_interm_32_cc3(c, k)
                        term(54) = term(54) + s2(a,c,i,k) * gamma_interm_41_cc3(c, k)
                  end do
            end do

            term(50) = term(50) * 2.6666666666666665d+0 
            term(51) = term(51) * (-1.3333333333333333d+0) 
            term(52) = term(52) * (-4.0d+0) 
            term(53) = term(53) * 8.0d+0 
            term(54) = term(54) * 4.0d+0 

            do b = nocc + 1, nactive 
                  do l = 1, nocc 
                        term(55) = term(55) + s2(a,b,l,i) * gamma_interm_28_cc3(l, b)
                        term(56) = term(56) + s2(a,b,l,i) * gamma_interm_28_cc3(l, b)
                  end do
            end do

            term(55) = term(55) * (-0.3333333333333333d+0) 
            term(56) = term(56) * 0.6666666666666666d+0 

            do b = nocc + 1, nactive 
                  do k = 1, nocc 
                        term(57) = term(57) + s2(a,b,k,i) * gamma_interm_30_cc3(k, b)
                        term(58) = term(58) + s2(a,b,k,i) * gamma_interm_30_cc3(k, b)
                        term(59) = term(59) + s2(a,b,k,i) * gamma_interm_42_cc3(k, b)
                  end do
            end do

            term(57) = term(57) * 0.6666666666666666d+0 
            term(58) = term(58) * (-0.3333333333333333d+0) 

            do l = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(60) = term(60) + s2(a,b,i,l) * gamma_interm_28_cc3(l, b)
                        term(61) = term(61) + s2(a,b,i,l) * gamma_interm_28_cc3(l, b)
                  end do
            end do

            term(60) = term(60) * 0.6666666666666666d+0 
            term(61) = term(61) * (-1.3333333333333333d+0) 

            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(62) = term(62) + s2(a,b,i,k) * gamma_interm_30_cc3(k, b)
                        term(63) = term(63) + s2(a,b,i,k) * gamma_interm_30_cc3(k, b)
                        term(64) = term(64) + s2(a,b,i,k) * gamma_interm_42_cc3(k, b)
                  end do
            end do

            term(62) = term(62) * (-1.3333333333333333d+0) 
            term(63) = term(63) * 0.6666666666666666d+0 
            term(64) = term(64) * (-2.0d+0) 

            do j = 1, nocc 
                  do b = nocc + 1, nactive 
                        term(65) = term(65) + s2(a,b,i,j) * gamma_interm_29_cc3(b, j)
                        term(66) = term(66) + s2(a,b,i,j) * gamma_interm_29_cc3(b, j)
                        term(67) = term(67) + s2(a,b,i,j) * gamma_interm_41_cc3(b, j)
                        term(68) = term(68) + r1(vdav, b,j) * s2(a,b,i,j)
                        term(69) = term(69) + r2(vdav, a,i,b,j) * s1(b,j)
                  end do
            end do

            term(65) = term(65) * 2.6666666666666665d+0 
            term(66) = term(66) * (-1.3333333333333333d+0) 
            term(67) = term(67) * 4.0d+0 
            term(68) = term(68) * 4.0d+0 
            term(69) = term(69) * 4.0d+0 

            do d = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(70) = term(70) + s2(a,d,j,i) * gamma_interm_30_cc3(j, d)
                        term(71) = term(71) + s2(a,d,j,i) * gamma_interm_30_cc3(j, d)
                  end do
            end do

            term(70) = term(70) * 0.6666666666666666d+0 
            term(71) = term(71) * (-0.3333333333333333d+0) 

            do j = 1, nocc 
                  do d = nocc + 1, nactive 
                        term(72) = term(72) + s2(a,d,i,j) * gamma_interm_30_cc3(j, d)
                        term(73) = term(73) + s2(a,d,i,j) * gamma_interm_30_cc3(j, d)
                  end do
            end do

            term(72) = term(72) * (-1.3333333333333333d+0) 
            term(73) = term(73) * 0.6666666666666666d+0 

            do c = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(74) = term(74) + s2(a,c,j,i) * gamma_interm_30_cc3(j, c)
                        term(75) = term(75) + s2(a,c,j,i) * gamma_interm_30_cc3(j, c)
                        term(76) = term(76) + s2(a,c,j,i) * gamma_interm_42_cc3(j, c)
                  end do
            end do

            term(74) = term(74) * 0.6666666666666666d+0 
            term(75) = term(75) * (-0.3333333333333333d+0) 

            do j = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(77) = term(77) + s2(a,c,i,j) * gamma_interm_30_cc3(j, c)
                        term(78) = term(78) + s2(a,c,i,j) * gamma_interm_30_cc3(j, c)
                        term(79) = term(79) + s2(a,c,i,j) * gamma_interm_42_cc3(j, c)
                  end do
            end do

            term(77) = term(77) * (-1.3333333333333333d+0) 
            term(78) = term(78) * 0.6666666666666666d+0 
            term(79) = term(79) * (-2.0d+0) 

            term(80) = term(80) + r1(vdav, a,i)

            term(80) = term(80) * 2.0d+0 

            do k = 1, nocc 
                  do j = 1, nocc 
                        do b = nocc + 1, nactive 
                              do c = nocc + 1, nactive 
                                    term(81) = term(81) + r2(vdav, b,k,c,j) * t3(nocc, nactive, a,b,c,j,i,k)
                                    term(82) = term(82) + r2(vdav, b,j,c,k) * t3(nocc, nactive, a,b,c,j,i,k)
                              end do
                        end do
                  end do
            end do

            term(81) = term(81) * 1.9999999999999998d+0 
            term(82) = term(82) * (-3.9999999999999996d+0) 

            do j = 1, nocc 
                  do b = nocc + 1, nactive 
                        do k = 1, nocc 
                              do c = nocc + 1, nactive 
                                    term(83) = term(83) + r2(vdav, b,j,c,k) * t3(nocc, nactive, a,b,c,i,k,j)
                              end do
                        end do
                  end do
            end do

            term(83) = term(83) * (-1.9999999999999998d+0) 

            do k = 1, nocc 
                  do b = nocc + 1, nactive 
                        do c = nocc + 1, nactive 
                              do j = 1, nocc 
                                    term(84) = term(84) + r2(vdav, b,j,c,k) * t3(nocc, nactive, a,b,c,i,j,k)
                              end do
                        end do
                  end do
            end do

            term(84) = term(84) * 3.9999999999999996d+0 

            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        do b = nocc + 1, nactive 
                              do j = 1, nocc 
                                    term(85) = term(85) + r3(vdav, a,j,b,i,c,k) * s2(b,c,j,k)
                              end do
                        end do
                  end do
            end do

            term(85) = term(85) * (-3.9999999999999996d+0) 

            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        do j = 1, nocc 
                              do b = nocc + 1, nactive 
                                    term(86) = term(86) + r3(vdav, a,i,b,j,c,k) * s2(b,c,j,k)
                                    term(87) = term(87) + r3(vdav, a,j,b,i,c,k) * s2(b,c,k,j)
                                    term(88) = term(88) + r3(vdav, a,i,b,j,c,k) * s2(b,c,k,j)
                              end do
                        end do
                  end do
            end do

            term(86) = term(86) * 4.0d+0 
            term(87) = term(87) * 1.9999999999999998d+0 
            term(88) = term(88) * (-2.0d+0) 


            calc_D_vo_gamma_with_int_cc3 = 0.d+0 
            do s = 0, 88
                  calc_D_vo_gamma_with_int_cc3 = calc_D_vo_gamma_with_int_cc3 + term(s)
            end do

      end function calc_D_vo_gamma_with_int_cc3

      function calc_D_vv_gamma_with_int_cc3(t2, t1, s2, s1, vdav, nocc, nactive, a,b) 
            double precision :: calc_D_vv_gamma_with_int_cc3
            integer, intent(in) :: nocc, nactive
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
            double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
            double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
            real(F64), dimension(:), intent(in) :: vdav
            integer, intent(in) :: a,b 
            integer :: s ,i,j,c,k,d 
            double precision, dimension(0:35) :: term 
            term = 0.d+0 
            do i = 1, nocc 
                  term(0) = term(0) + r1(vdav, a,i) * s1(b,i)
                  term(1) = term(1) + t1(a,i) * gamma_interm_43_cc3(i, b)
                  term(2) = term(2) + t1(a,i) * gamma_interm_44_cc3(b, i)
                  term(3) = term(3) + t1(b,i) * gamma_interm_44_cc3(a, i)
                  term(4) = term(4) + s1(b,i) * gamma_interm_49_cc3(a, i)
                  term(5) = term(5) + s1(b,i) * gamma_interm_49_cc3(a, i)
            end do

            term(0) = term(0) * 2.0d+0 
            term(1) = term(1) * (-2.0d+0) 
            term(2) = term(2) * 2.0d+0 
            term(3) = term(3) * 2.0d+0 
            term(4) = term(4) * 4.0d+0 
            term(5) = term(5) * (-2.0d+0) 

            do d = nocc + 1, nactive 
                  do i = 1, nocc 
                        term(6) = term(6) + s1(d,i) * gamma_interm_53_cc3(a, i, d, b)
                        term(7) = term(7) + s1(d,i) * gamma_interm_53_cc3(a, i, d, b)
                  end do
            end do

            term(6) = term(6) * (-2.0d+0) 

            do j = 1, nocc 
                  do i = 1, nocc 
                        do c = nocc + 1, nactive 
                              term(8) = term(8) + r2(vdav, a,i,c,j) * s2(b,c,j,i)
                        end do
                  end do
            end do

            term(8) = term(8) * (-2.0d+0) 

            do j = 1, nocc 
                  do c = nocc + 1, nactive 
                        do i = 1, nocc 
                              term(9) = term(9) + r2(vdav, a,i,c,j) * s2(b,c,i,j)
                        end do
                  end do
            end do

            term(9) = term(9) * 4.0d+0 

            do c = nocc + 1, nactive 
                  do k = 1, nocc 
                        term(10) = term(10) + r1(vdav, c,k) * gamma_interm_45_cc3(a, k, b, c)
                        term(11) = term(11) + r1(vdav, c,k) * gamma_interm_45_cc3(b, k, a, c)
                        term(12) = term(12) + s1(c,k) * gamma_interm_52_cc3(k, a, c, b)
                        term(13) = term(13) + s1(c,k) * gamma_interm_52_cc3(k, a, c, b)
                  end do
            end do

            term(13) = term(13) * (-2.0d+0) 

            do k = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(14) = term(14) + r1(vdav, c,k) * gamma_interm_46_cc3(a, b, c, k)
                        term(15) = term(15) + r1(vdav, c,k) * gamma_interm_46_cc3(b, a, c, k)
                  end do
            end do

            term(14) = term(14) * (-1.9999999999999998d+0) 
            term(15) = term(15) * (-1.9999999999999998d+0) 

            do c = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(16) = term(16) + r1(vdav, c,j) * gamma_interm_47_cc3(a, b, j, c)
                        term(17) = term(17) + r1(vdav, c,j) * gamma_interm_45_cc3(a, j, b, c)
                        term(18) = term(18) + r1(vdav, c,j) * gamma_interm_47_cc3(a, b, j, c)
                        term(19) = term(19) + r1(vdav, c,j) * gamma_interm_45_cc3(b, j, a, c)
                  end do
            end do

            term(17) = term(17) * (-1.9999999999999998d+0) 
            term(18) = term(18) * (-1.9999999999999998d+0) 
            term(19) = term(19) * (-1.9999999999999998d+0) 

            do j = 1, nocc 
                  do c = nocc + 1, nactive 
                        term(20) = term(20) + r1(vdav, c,j) * gamma_interm_46_cc3(a, b, c, j)
                        term(21) = term(21) + r1(vdav, c,j) * gamma_interm_46_cc3(b, a, c, j)
                        term(22) = term(22) + s1(c,j) * gamma_interm_51_cc3(a, c, j, b)
                        term(23) = term(23) + s1(c,j) * gamma_interm_51_cc3(a, c, j, b)
                  end do
            end do

            term(20) = term(20) * 3.9999999999999996d+0 
            term(21) = term(21) * 3.9999999999999996d+0 
            term(22) = term(22) * (-2.0d+0) 
            term(23) = term(23) * 4.0d+0 

            do c = nocc + 1, nactive 
                  do i = 1, nocc 
                        term(24) = term(24) + r1(vdav, c,i) * gamma_interm_47_cc3(b, a, i, c)
                        term(25) = term(25) + r1(vdav, c,i) * gamma_interm_47_cc3(b, a, i, c)
                        term(26) = term(26) + s1(c,i) * gamma_interm_53_cc3(a, i, c, b)
                        term(27) = term(27) + s1(c,i) * gamma_interm_53_cc3(a, i, c, b)
                  end do
            end do

            term(25) = term(25) * (-1.9999999999999998d+0) 
            term(27) = term(27) * (-2.0d+0) 

            do j = 1, nocc 
                  term(28) = term(28) + s1(b,j) * gamma_interm_48_cc3(j, a)
                  term(29) = term(29) + s1(b,j) * gamma_interm_48_cc3(j, a)
                  term(30) = term(30) + s1(b,j) * gamma_interm_50_cc3(j, a)
                  term(31) = term(31) + s1(b,j) * gamma_interm_50_cc3(j, a)
            end do

            term(28) = term(28) * (-2.0d+0) 
            term(30) = term(30) * (-2.0d+0) 

            do d = nocc + 1, nactive 
                  do j = 1, nocc 
                        term(32) = term(32) + s1(d,j) * gamma_interm_52_cc3(j, a, d, b)
                        term(33) = term(33) + s1(d,j) * gamma_interm_52_cc3(j, a, d, b)
                  end do
            end do

            term(33) = term(33) * (-2.0d+0) 

            do k = 1, nocc 
                  do d = nocc + 1, nactive 
                        term(34) = term(34) + s1(d,k) * gamma_interm_51_cc3(a, d, k, b)
                        term(35) = term(35) + s1(d,k) * gamma_interm_51_cc3(a, d, k, b)
                  end do
            end do

            term(34) = term(34) * (-2.0d+0) 
            term(35) = term(35) * 4.0d+0 


            calc_D_vv_gamma_with_int_cc3 = 0.d+0 
            do s = 0, 35
                  calc_D_vv_gamma_with_int_cc3 = calc_D_vv_gamma_with_int_cc3 + term(s)
            end do

      end function calc_D_vv_gamma_with_int_cc3





end module density_gr_exc_functions_int
