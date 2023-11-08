module density_exc_exc_functions

      use cc3_intermediates
      use arithmetic
      use s_gen
      use basis
      use eom_vectors
      
      implicit none

      !
      ! File generated automatically on 2015-04-27 00:44:26
      !
      real(F64) :: wm_interm_0 
      real(F64) :: wm_interm_1 
      real(F64), dimension(:, :), allocatable :: wm_interm_2 
      real(F64), dimension(:, :), allocatable :: wm_interm_3 
      real(F64), dimension(:, :), allocatable :: wm_interm_4 
      real(F64), dimension(:, :), allocatable :: wm_interm_5 
      real(F64), dimension(:, :), allocatable :: wm_interm_6 
      real(F64), dimension(:, :), allocatable :: wm_interm_7 
      real(F64), dimension(:, :), allocatable :: wm_interm_8 
      real(F64), dimension(:, :), allocatable :: wm_interm_9 
      real(F64), dimension(:, :), allocatable :: wm_interm_10 
      real(F64) :: wm_interm_11 
      real(F64), dimension(:, :), allocatable :: wm_interm_12 
      real(F64), dimension(:, :), allocatable :: wm_interm_13 
      real(F64), dimension(:, :), allocatable :: wm_interm_14 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15 
      real(F64), dimension(:, :), allocatable :: wm_interm_16 
      real(F64), dimension(:, :), allocatable :: wm_interm_17 
      real(F64), dimension(:, :), allocatable :: wm_interm_18 
      real(F64), dimension(:, :), allocatable :: wm_interm_19 
      real(F64), dimension(:, :), allocatable :: wm_interm_20 
      real(F64), dimension(:, :), allocatable :: wm_interm_21 
      real(F64), dimension(:, :), allocatable :: wm_interm_22 
      real(F64), dimension(:, :), allocatable :: wm_interm_23 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24 
      real(F64), dimension(:, :), allocatable :: wm_interm_25 
      real(F64), dimension(:, :), allocatable :: wm_interm_26 
      real(F64), dimension(:, :), allocatable :: wm_interm_27 
      real(F64), dimension(:, :), allocatable :: wm_interm_28 
      real(F64), dimension(:, :), allocatable :: wm_interm_29 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30 
      real(F64), dimension(:, :), allocatable :: wm_interm_31 
      real(F64), dimension(:, :), allocatable :: wm_interm_32 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33 
      real(F64), dimension(:, :), allocatable :: wm_interm_34 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37 
      real(F64), dimension(:, :), allocatable :: wm_interm_38 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39 
      real(F64), dimension(:, :), allocatable :: wm_interm_40 
      real(F64), dimension(:, :), allocatable :: wm_interm_41 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_42 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_43 
      real(F64), dimension(:, :), allocatable :: wm_interm_44 
      real(F64), dimension(:, :), allocatable :: wm_interm_45 
      real(F64), dimension(:, :), allocatable :: wm_interm_46 
      real(F64), dimension(:, :), allocatable :: wm_interm_47 
      real(F64), dimension(:, :), allocatable :: wm_interm_48 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50 
      real(F64), dimension(:, :), allocatable :: wm_interm_51 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52 
      real(F64), dimension(:, :), allocatable :: wm_interm_53 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_57 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_62 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_64 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_65 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_67 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_68 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_70 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_71 
      real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72 

     real(F64), dimension(:, :), allocatable :: wmo_interm_0 
real(F64), dimension(:, :), allocatable :: wmo_interm_1 
real(F64), dimension(:, :), allocatable :: wmo_interm_2 
real(F64), dimension(:, :), allocatable :: wmo_interm_3 
real(F64), dimension(:, :), allocatable :: wmo_interm_4 
real(F64), dimension(:, :), allocatable :: wmo_interm_5 
real(F64), dimension(:, :), allocatable :: wmo_interm_6 
real(F64), dimension(:, :), allocatable :: wmo_interm_7 
real(F64), dimension(:, :), allocatable :: wmo_interm_8 
real(F64), dimension(:, :), allocatable :: wmo_interm_9 
real(F64), dimension(:, :), allocatable :: wmo_interm_10 
real(F64), dimension(:, :), allocatable :: wmo_interm_11 
real(F64), dimension(:, :), allocatable :: wmo_interm_12 
real(F64), dimension(:, :), allocatable :: wmo_interm_13 
real(F64), dimension(:, :), allocatable :: wmo_interm_14 
real(F64), dimension(:, :), allocatable :: wmo_interm_15 
real(F64), dimension(:, :), allocatable :: wmo_interm_16 
real(F64), dimension(:, :), allocatable :: wmo_interm_17 
real(F64), dimension(:, :), allocatable :: wmo_interm_18 
real(F64), dimension(:, :), allocatable :: wmo_interm_19 
real(F64), dimension(:, :), allocatable :: wmo_interm_20 
real(F64), dimension(:, :), allocatable :: wmo_interm_21 
real(F64), dimension(:, :), allocatable :: wmo_interm_22 
real(F64), dimension(:, :), allocatable :: wmo_interm_23 
real(F64), dimension(:, :), allocatable :: wmo_interm_24 
real(F64), dimension(:, :), allocatable :: wmo_interm_25 
real(F64), dimension(:, :), allocatable :: wmo_interm_26 
real(F64), dimension(:, :), allocatable :: wmo_interm_27 
real(F64), dimension(:, :), allocatable :: wmo_interm_28 
real(F64), dimension(:, :), allocatable :: wmo_interm_29 
real(F64), dimension(:, :), allocatable :: wmo_interm_30 
real(F64), dimension(:, :), allocatable :: wmo_interm_31 
real(F64), dimension(:, :), allocatable :: wmo_interm_32 
real(F64), dimension(:, :), allocatable :: wmo_interm_33 
real(F64), dimension(:, :), allocatable :: wmo_interm_34 
real(F64), dimension(:, :), allocatable :: wmo_interm_35 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_36 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_37 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_38 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_39 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_40 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_41 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_42 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_43 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_44 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_45 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_46 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_47 





contains

      subroutine wm_intermediates_ccsd_init(nocc, nactive)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive

        allocate(wm_interm_2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_5(nocc+1: nactive, 1: nocc))
allocate(wm_interm_6(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7(nocc+1: nactive, 1: nocc))
allocate(wm_interm_8(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10(nocc+1: nactive, nocc+1: nactive))

allocate(wm_interm_12(1: nocc, 1: nocc))
allocate(wm_interm_13(1: nocc, 1: nocc))
allocate(wm_interm_14(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_15(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_16(1: nocc, 1: nocc))
allocate(wm_interm_17(1: nocc, 1: nocc))
allocate(wm_interm_18(1: nocc, 1: nocc))
allocate(wm_interm_19(1: nocc, 1: nocc))
allocate(wm_interm_20(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_21(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_22(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_23(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_24(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_26(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_27(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_28(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_29(1: nocc, 1: nocc))
allocate(wm_interm_30(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_31(1: nocc, 1: nocc))
allocate(wm_interm_32(1: nocc, 1: nocc))
allocate(wm_interm_33(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_34(1: nocc, 1: nocc))
allocate(wm_interm_35(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_36(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_37(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_38(1: nocc, 1: nocc))
allocate(wm_interm_39(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_40(nocc+1: nactive, 1: nocc))
allocate(wm_interm_41(nocc+1: nactive, 1: nocc))
allocate(wm_interm_42(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_43(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_44(nocc+1: nactive, 1: nocc))
allocate(wm_interm_45(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_46(nocc+1: nactive, 1: nocc))
allocate(wm_interm_47(nocc+1: nactive, 1: nocc))
allocate(wm_interm_48(nocc+1: nactive, 1: nocc))
allocate(wm_interm_49(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_51(1: nocc, 1: nocc))
allocate(wm_interm_52(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_53(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_54(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_55(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_56(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_57(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_58(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_59(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_60(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_61(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_62(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_63(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_64(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_65(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_66(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_67(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_68(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_69(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_70(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_71(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_72(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wm_interm_0 = zero 
wm_interm_1 = zero 
wm_interm_2 = zero 
wm_interm_3 = zero 
wm_interm_4 = zero 
wm_interm_5 = zero 
wm_interm_6 = zero 
wm_interm_7 = zero 
wm_interm_8 = zero 
wm_interm_9 = zero 
wm_interm_10 = zero 
wm_interm_11 = zero 
wm_interm_12 = zero 
wm_interm_13 = zero 
wm_interm_14 = zero 
wm_interm_15 = zero 
wm_interm_16 = zero 
wm_interm_17 = zero 
wm_interm_18 = zero 
wm_interm_19 = zero 
wm_interm_20 = zero 
wm_interm_21 = zero 
wm_interm_22 = zero 
wm_interm_23 = zero 
wm_interm_24 = zero 
wm_interm_25 = zero 
wm_interm_26 = zero 
wm_interm_27 = zero 
wm_interm_28 = zero 
wm_interm_29 = zero 
wm_interm_30 = zero 
wm_interm_31 = zero 
wm_interm_32 = zero 
wm_interm_33 = zero 
wm_interm_34 = zero 
wm_interm_35 = zero 
wm_interm_36 = zero 
wm_interm_37 = zero 
wm_interm_38 = zero 
wm_interm_39 = zero 
wm_interm_40 = zero 
wm_interm_41 = zero 
wm_interm_42 = zero 
wm_interm_43 = zero 
wm_interm_44 = zero 
wm_interm_45 = zero 
wm_interm_46 = zero 
wm_interm_47 = zero 
wm_interm_48 = zero 
wm_interm_49 = zero 
wm_interm_50 = zero 
wm_interm_51 = zero 
wm_interm_52 = zero 
wm_interm_53 = zero 
wm_interm_54 = zero 
wm_interm_55 = zero 
wm_interm_56 = zero 
wm_interm_57 = zero 
wm_interm_58 = zero 
wm_interm_59 = zero 
wm_interm_60 = zero 
wm_interm_61 = zero 
wm_interm_62 = zero 
wm_interm_63 = zero 
wm_interm_64 = zero 
wm_interm_65 = zero 
wm_interm_66 = zero 
wm_interm_67 = zero 
wm_interm_68 = zero 
wm_interm_69 = zero 
wm_interm_70 = zero 
wm_interm_71 = zero 
wm_interm_72 = zero 

      end subroutine wm_intermediates_ccsd_init

      subroutine overlap_ccsd_init(nocc, nactive)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive


    allocate(wmo_interm_0(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_1(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_2(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_3(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_4(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_5(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_6(1: nocc, 1: nocc))
allocate(wmo_interm_7(1: nocc, 1: nocc))
allocate(wmo_interm_8(1: nocc, 1: nocc))
allocate(wmo_interm_9(1: nocc, 1: nocc))
allocate(wmo_interm_10(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_11(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_12(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_13(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_14(1: nocc, 1: nocc))
allocate(wmo_interm_15(1: nocc, 1: nocc))
allocate(wmo_interm_16(1: nocc, 1: nocc))
allocate(wmo_interm_17(1: nocc, 1: nocc))
allocate(wmo_interm_18(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_19(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_20(1: nocc, 1: nocc))
allocate(wmo_interm_21(1: nocc, 1: nocc))
allocate(wmo_interm_22(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_23(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_24(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_25(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_26(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_27(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_28(1: nocc, 1: nocc))
allocate(wmo_interm_29(1: nocc, 1: nocc))
allocate(wmo_interm_30(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_31(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_32(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_33(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_34(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_35(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_36(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_37(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_38(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_39(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_40(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_41(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_42(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_43(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_44(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_45(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_46(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_47(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wmo_interm_0 = zero 
wmo_interm_1 = zero 
wmo_interm_2 = zero 
wmo_interm_3 = zero 
wmo_interm_4 = zero 
wmo_interm_5 = zero 
wmo_interm_6 = zero 
wmo_interm_7 = zero 
wmo_interm_8 = zero 
wmo_interm_9 = zero 
wmo_interm_10 = zero 
wmo_interm_11 = zero 
wmo_interm_12 = zero 
wmo_interm_13 = zero 
wmo_interm_14 = zero 
wmo_interm_15 = zero 
wmo_interm_16 = zero 
wmo_interm_17 = zero 
wmo_interm_18 = zero 
wmo_interm_19 = zero 
wmo_interm_20 = zero 
wmo_interm_21 = zero 
wmo_interm_22 = zero 
wmo_interm_23 = zero 
wmo_interm_24 = zero 
wmo_interm_25 = zero 
wmo_interm_26 = zero 
wmo_interm_27 = zero 
wmo_interm_28 = zero 
wmo_interm_29 = zero 
wmo_interm_30 = zero 
wmo_interm_31 = zero 
wmo_interm_32 = zero 
wmo_interm_33 = zero 
wmo_interm_34 = zero 
wmo_interm_35 = zero 
wmo_interm_36 = zero 
wmo_interm_37 = zero 
wmo_interm_38 = zero 
wmo_interm_39 = zero 
wmo_interm_40 = zero 
wmo_interm_41 = zero 
wmo_interm_42 = zero 
wmo_interm_43 = zero 
wmo_interm_44 = zero 
wmo_interm_45 = zero 
wmo_interm_46 = zero 
wmo_interm_47 = zero  



      end subroutine overlap_ccsd_init

      subroutine wm_intermediates_ccsd_free

          deallocate(wm_interm_2)
deallocate(wm_interm_3)
deallocate(wm_interm_4)
deallocate(wm_interm_5)
deallocate(wm_interm_6)
deallocate(wm_interm_7)
deallocate(wm_interm_8)
deallocate(wm_interm_9)
deallocate(wm_interm_10)

deallocate(wm_interm_12)
deallocate(wm_interm_13)
deallocate(wm_interm_14)
deallocate(wm_interm_15)
deallocate(wm_interm_16)
deallocate(wm_interm_17)
deallocate(wm_interm_18)
deallocate(wm_interm_19)
deallocate(wm_interm_20)
deallocate(wm_interm_21)
deallocate(wm_interm_22)
deallocate(wm_interm_23)
deallocate(wm_interm_24)
deallocate(wm_interm_25)
deallocate(wm_interm_26)
deallocate(wm_interm_27)
deallocate(wm_interm_28)
deallocate(wm_interm_29)
deallocate(wm_interm_30)
deallocate(wm_interm_31)
deallocate(wm_interm_32)
deallocate(wm_interm_33)
deallocate(wm_interm_34)
deallocate(wm_interm_35)
deallocate(wm_interm_36)
deallocate(wm_interm_37)
deallocate(wm_interm_38)
deallocate(wm_interm_39)
deallocate(wm_interm_40)
deallocate(wm_interm_41)
deallocate(wm_interm_42)
deallocate(wm_interm_43)
deallocate(wm_interm_44)
deallocate(wm_interm_45)
deallocate(wm_interm_46)
deallocate(wm_interm_47)
deallocate(wm_interm_48)
deallocate(wm_interm_49)
deallocate(wm_interm_50)
deallocate(wm_interm_51)
deallocate(wm_interm_52)
deallocate(wm_interm_53)
deallocate(wm_interm_54)
deallocate(wm_interm_55)
deallocate(wm_interm_56)
deallocate(wm_interm_57)
deallocate(wm_interm_58)
deallocate(wm_interm_59)
deallocate(wm_interm_60)
deallocate(wm_interm_61)
deallocate(wm_interm_62)
deallocate(wm_interm_63)
deallocate(wm_interm_64)
deallocate(wm_interm_65)
deallocate(wm_interm_66)
deallocate(wm_interm_67)
deallocate(wm_interm_68)
deallocate(wm_interm_69)
deallocate(wm_interm_70)
deallocate(wm_interm_71)
deallocate(wm_interm_72)




      end subroutine wm_intermediates_ccsd_free

      subroutine overlap_ccsd_free

deallocate(wmo_interm_0)
deallocate(wmo_interm_1)
deallocate(wmo_interm_2)
deallocate(wmo_interm_3)
deallocate(wmo_interm_4)
deallocate(wmo_interm_5)
deallocate(wmo_interm_6)
deallocate(wmo_interm_7)
deallocate(wmo_interm_8)
deallocate(wmo_interm_9)
deallocate(wmo_interm_10)
deallocate(wmo_interm_11)
deallocate(wmo_interm_12)
deallocate(wmo_interm_13)
deallocate(wmo_interm_14)
deallocate(wmo_interm_15)
deallocate(wmo_interm_16)
deallocate(wmo_interm_17)
deallocate(wmo_interm_18)
deallocate(wmo_interm_19)
deallocate(wmo_interm_20)
deallocate(wmo_interm_21)
deallocate(wmo_interm_22)
deallocate(wmo_interm_23)
deallocate(wmo_interm_24)
deallocate(wmo_interm_25)
deallocate(wmo_interm_26)
deallocate(wmo_interm_27)
deallocate(wmo_interm_28)
deallocate(wmo_interm_29)
deallocate(wmo_interm_30)
deallocate(wmo_interm_31)
deallocate(wmo_interm_32)
deallocate(wmo_interm_33)
deallocate(wmo_interm_34)
deallocate(wmo_interm_35)
deallocate(wmo_interm_36)
deallocate(wmo_interm_37)
deallocate(wmo_interm_38)
deallocate(wmo_interm_39)
deallocate(wmo_interm_40)
deallocate(wmo_interm_41)
deallocate(wmo_interm_42)
deallocate(wmo_interm_43)
deallocate(wmo_interm_44)
deallocate(wmo_interm_45)
deallocate(wmo_interm_46)
deallocate(wmo_interm_47)
      end subroutine overlap_ccsd_free

      subroutine wm_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr) 

        integer, intent(in) :: nocc, nactive
        real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
        real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
        real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
        real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
        double precision, dimension(:), intent(in) :: vrdav_Rl
        double precision, dimension(:), intent(in) :: vrdav_Rr
        integer :: a, i, j, b, k, c, l 
        real(F64) :: sum

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
wm_interm_0 = wm_interm_0 + sum 



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
wm_interm_1 = wm_interm_1 + sum 

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
wm_interm_2(b, j) = wm_interm_2(b, j) + sum 
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
wm_interm_3(b, j) = wm_interm_3(b, j) + sum 
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
wm_interm_4(b, j) = wm_interm_4(b, j) + sum 
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
wm_interm_5(b, j) = wm_interm_5(b, j) + sum 
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
wm_interm_6(b, j) = wm_interm_6(b, j) + sum 
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
wm_interm_7(b, j) = wm_interm_7(b, j) + sum 
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
wm_interm_8(b, j) = wm_interm_8(b, j) + sum 
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
wm_interm_9(b, j) = wm_interm_9(b, j) + sum 
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
wm_interm_10(a, b) = wm_interm_10(a, b) + sum 
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
wm_interm_11 = wm_interm_11 + sum 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_12(i, j) = wm_interm_12(i, j) + sum 
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
wm_interm_13(i, j) = wm_interm_13(i, j) + sum 
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
wm_interm_14(a, b) = wm_interm_14(a, b) + sum 
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
wm_interm_15(i, j, k, l) = wm_interm_15(i, j, k, l) + sum 
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
wm_interm_16(j, k) = wm_interm_16(j, k) + sum 
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
wm_interm_17(j, k) = wm_interm_17(j, k) + sum 
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
wm_interm_18(j, k) = wm_interm_18(j, k) + sum 
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
wm_interm_19(j, k) = wm_interm_19(j, k) + sum 
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
wm_interm_20(b, c) = wm_interm_20(b, c) + sum 
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
wm_interm_21(b, c) = wm_interm_21(b, c) + sum 
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
wm_interm_22(b, c) = wm_interm_22(b, c) + sum 
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
wm_interm_23(b, c) = wm_interm_23(b, c) + sum 
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
wm_interm_24(i, j, k, l) = wm_interm_24(i, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,j)
end do 
end do 
end do 
wm_interm_25(b, c) = wm_interm_25(b, c) + sum 
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
wm_interm_26(b, c) = wm_interm_26(b, c) + sum 
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
wm_interm_27(b, c) = wm_interm_27(b, c) + sum 
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
wm_interm_28(b, c) = wm_interm_28(b, c) + sum 
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
wm_interm_29(j, k) = wm_interm_29(j, k) + sum 
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
wm_interm_30(i, j, k, l) = wm_interm_30(i, j, k, l) + sum 
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
wm_interm_31(j, k) = wm_interm_31(j, k) + sum 
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
wm_interm_32(j, k) = wm_interm_32(j, k) + sum 
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
wm_interm_33(i, j, k, l) = wm_interm_33(i, j, k, l) + sum 
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
wm_interm_34(j, k) = wm_interm_34(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_35(b, j, i, k) = wm_interm_35(b, j, i, k) + sum 
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
wm_interm_36(b, i, j, k) = wm_interm_36(b, i, j, k) + sum 
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
wm_interm_37(b, j, i, k) = wm_interm_37(b, j, i, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wm_interm_38(i, j) = wm_interm_38(i, j) + sum 
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
wm_interm_39(b, j, i, k) = wm_interm_39(b, j, i, k) + sum 
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
wm_interm_40(b, j) = wm_interm_40(b, j) + sum 
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
wm_interm_41(b, j) = wm_interm_41(b, j) + sum 
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
wm_interm_42(b, i, j, k) = wm_interm_42(b, i, j, k) + sum 
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
sum = sum + s1(a,i) * t2(a,b,j,k)
end do 
wm_interm_43(b, i, j, k) = wm_interm_43(b, i, j, k) + sum 
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
sum = sum + s1(a,i) * t2(a,b,j,i)
end do 
end do 
wm_interm_44(b, j) = wm_interm_44(b, j) + sum 
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
wm_interm_45(a, b) = wm_interm_45(a, b) + sum 
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
wm_interm_46(b, j) = wm_interm_46(b, j) + sum 
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
wm_interm_47(b, j) = wm_interm_47(b, j) + sum 
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
wm_interm_48(b, j) = wm_interm_48(b, j) + sum 
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
wm_interm_49(b, i, j, k) = wm_interm_49(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k)
end do 
wm_interm_50(b, i, j, k) = wm_interm_50(b, i, j, k) + sum 
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
sum = sum + s1(a,i) * t1(a,j)
end do 
wm_interm_51(i, j) = wm_interm_51(i, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s1(a,k)
end do 
wm_interm_52(b, i, j, k) = wm_interm_52(b, i, j, k) + sum 
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
sum = sum + s1(a,i) * t1(b,i)
end do 
wm_interm_53(a, b) = wm_interm_53(a, b) + sum 
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
wm_interm_54(b, j, i, k) = wm_interm_54(b, j, i, k) + sum 
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
wm_interm_55(b, i, j, k) = wm_interm_55(b, i, j, k) + sum 
end do 
end do 
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
wm_interm_56(b, i, k, j) = wm_interm_56(b, i, k, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,k,i)
end do 
end do 
wm_interm_57(b, c, j, k) = wm_interm_57(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,k,i)
end do 
end do 
wm_interm_58(b, c, j, k) = wm_interm_58(b, c, j, k) + sum 
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
wm_interm_59(b, c, j, k) = wm_interm_59(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_60(b, c, j, k) = wm_interm_60(b, c, j, k) + sum 
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
wm_interm_61(b, c, j, k) = wm_interm_61(b, c, j, k) + sum 
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
wm_interm_62(b, c, j, k) = wm_interm_62(b, c, j, k) + sum 
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
wm_interm_63(b, c, j, k) = wm_interm_63(b, c, j, k) + sum 
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
wm_interm_64(b, c, j, k) = wm_interm_64(b, c, j, k) + sum 
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
wm_interm_65(b, c, j, k) = wm_interm_65(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_66(b, c, j, k) = wm_interm_66(b, c, j, k) + sum 
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
wm_interm_67(b, c, j, k) = wm_interm_67(b, c, j, k) + sum 
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
sum = sum + s2(a,b,j,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_68(b, c, j, k) = wm_interm_68(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_69(b, c, j, k) = wm_interm_69(b, c, j, k) + sum 
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
wm_interm_70(b, c, j, k) = wm_interm_70(b, c, j, k) + sum 
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
wm_interm_71(b, c, j, k) = wm_interm_71(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_72(b, c, j, k) = wm_interm_72(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 


      end subroutine wm_intermediates_ccsd

      subroutine overlap_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr) 

        integer, intent(in) :: nocc, nactive
        real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
        real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
        real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
        real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
        double precision, dimension(:), intent(in) ::vrdav_Rl
        double precision, dimension(:), intent(in) ::vrdav_Rr
        integer :: a, i, j, b, k, c, l 
        real(F64) :: sum

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
wmo_interm_0(b, j) = wmo_interm_0(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

! do j = 1, nocc
! do b = nocc+1, nactive
!       if (abs(wmo_interm_0(b, j)) .gt.1.d-10)then
!             print*, b, j, wmo_interm_0(b, j)
!       end if
! end do
! end do

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
wmo_interm_1(b, j) = wmo_interm_1(b, j) + sum 
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
wmo_interm_2(b, j) = wmo_interm_2(b, j) + sum 
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
wmo_interm_3(b, j) = wmo_interm_3(b, j) + sum 
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
wmo_interm_4(b, c) = wmo_interm_4(b, c) + sum 
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
wmo_interm_5(b, c) = wmo_interm_5(b, c) + sum 
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
wmo_interm_6(j, k) = wmo_interm_6(j, k) + sum 
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
wmo_interm_7(j, k) = wmo_interm_7(j, k) + sum 
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
wmo_interm_8(j, k) = wmo_interm_8(j, k) + sum 
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
wmo_interm_9(j, k) = wmo_interm_9(j, k) + sum 
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
wmo_interm_10(b, j) = wmo_interm_10(b, j) + sum 
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
wmo_interm_11(b, c) = wmo_interm_11(b, c) + sum 
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
wmo_interm_12(b, c) = wmo_interm_12(b, c) + sum 
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
wmo_interm_13(b, j) = wmo_interm_13(b, j) + sum 
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
wmo_interm_14(i, j) = wmo_interm_14(i, j) + sum 
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
sum = sum + s1(a,i) * t1(a,j)
end do 
wmo_interm_15(i, j) = wmo_interm_15(i, j) + sum 
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
wmo_interm_16(i, j) = wmo_interm_16(i, j) + sum 
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
wmo_interm_17(i, j) = wmo_interm_17(i, j) + sum 
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
wmo_interm_18(b, j) = wmo_interm_18(b, j) + sum 
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
wmo_interm_19(b, j) = wmo_interm_19(b, j) + sum 
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
wmo_interm_20(j, k) = wmo_interm_20(j, k) + sum 
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
wmo_interm_21(j, k) = wmo_interm_21(j, k) + sum 
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
wmo_interm_22(b, c) = wmo_interm_22(b, c) + sum 
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
sum = sum + s1(a,i) * t1(b,i)
end do 
wmo_interm_23(a, b) = wmo_interm_23(a, b) + sum 
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
wmo_interm_24(b, c) = wmo_interm_24(b, c) + sum 
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
wmo_interm_25(a, b) = wmo_interm_25(a, b) + sum 
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
wmo_interm_26(b, c) = wmo_interm_26(b, c) + sum 
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
wmo_interm_27(b, c) = wmo_interm_27(b, c) + sum 
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
wmo_interm_28(j, k) = wmo_interm_28(j, k) + sum 
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
wmo_interm_29(j, k) = wmo_interm_29(j, k) + sum 
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
wmo_interm_30(b, j) = wmo_interm_30(b, j) + sum 
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
wmo_interm_31(a, b) = wmo_interm_31(a, b) + sum 
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
wmo_interm_32(b, j) = wmo_interm_32(b, j) + sum 
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
wmo_interm_33(b, j) = wmo_interm_33(b, j) + sum 
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
wmo_interm_34(b, j) = wmo_interm_34(b, j) + sum 
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
wmo_interm_35(a, b) = wmo_interm_35(a, b) + sum 
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
wmo_interm_36(i, j, k, l) = wmo_interm_36(i, j, k, l) + sum 
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
wmo_interm_37(i, j, k, l) = wmo_interm_37(i, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wmo_interm_38(i, j, k, l) = wmo_interm_38(i, j, k, l) + sum 
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
wmo_interm_39(i, j, k, l) = wmo_interm_39(i, j, k, l) + sum 
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
wmo_interm_40(b, c, j, k) = wmo_interm_40(b, c, j, k) + sum 
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
wmo_interm_41(b, c, j, k) = wmo_interm_41(b, c, j, k) + sum 
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
wmo_interm_42(b, c, j, k) = wmo_interm_42(b, c, j, k) + sum 
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
wmo_interm_43(b, c, j, k) = wmo_interm_43(b, c, j, k) + sum 
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
wmo_interm_44(b, c, j, k) = wmo_interm_44(b, c, j, k) + sum 
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
sum = sum + s2(a,b,j,i) * t2(a,c,i,k)
end do 
end do 
wmo_interm_45(b, c, j, k) = wmo_interm_45(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,c,k)
end do 
end do 
wmo_interm_46(b, c, j, k) = wmo_interm_46(b, c, j, k) + sum 
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
wmo_interm_47(b, c, j, k) = wmo_interm_47(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

      
      end subroutine overlap_intermediates_ccsd

      
    function calc_D_oo_wm2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, k, i, a, b, l, c 
    real(F64), dimension(0:360) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_15(i, j, k, p) * wm_interm_24(q, k, i, j)
term(1) = term(1) + wm_interm_15(i, j, k, p) * wm_interm_24(k, q, i, j)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(2) = term(2) + wm_interm_15(i, j, p, k) * wm_interm_24(q, k, i, j)
term(3) = term(3) + wm_interm_15(i, j, p, k) * wm_interm_24(k, q, i, j)
term(4) = term(4) + wm_interm_30(i, p, j, k) * wm_interm_33(q, i, j, k)
term(5) = term(5) + wm_interm_30(i, p, j, k) * wm_interm_33(q, i, k, j)
term(6) = term(6) + wm_interm_30(i, p, j, k) * wm_interm_33(i, q, k, j)
term(7) = term(7) + wm_interm_30(i, p, j, k) * wm_interm_33(i, q, j, k)
term(8) = term(8) + wm_interm_15(p, i, j, k) * wm_interm_24(j, k, i, q)
term(9) = term(9) + wm_interm_15(p, i, j, k) * wm_interm_24(k, j, i, q)
term(10) = term(10) + wm_interm_15(i, p, j, k) * wm_interm_24(k, j, i, q)
term(11) = term(11) + wm_interm_15(i, p, j, k) * wm_interm_24(j, k, i, q)
term(12) = term(12) + wm_interm_30(i, j, p, k) * wm_interm_33(i, j, k, q)
term(13) = term(13) + wm_interm_30(i, j, k, p) * wm_interm_33(i, j, k, q)
term(14) = term(14) + wm_interm_30(i, j, k, p) * wm_interm_33(i, j, q, k)
term(15) = term(15) + wm_interm_30(i, j, p, k) * wm_interm_33(i, j, q, k)
end do 
end do 
end do 

term(2) = term(2) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(15) = term(15) * (-2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(16) = term(16) + wm_interm_20(a, b) * wm_interm_22(b, a)
term(17) = term(17) + wm_interm_20(a, b) * wm_interm_23(b, a)
term(18) = term(18) + wm_interm_21(a, b) * wm_interm_22(b, a)
term(19) = term(19) + wm_interm_21(a, b) * wm_interm_23(b, a)
term(20) = term(20) + wm_interm_25(a, b) * wm_interm_27(a, b)
term(21) = term(21) + wm_interm_25(a, b) * wm_interm_28(a, b)
term(22) = term(22) + wm_interm_26(a, b) * wm_interm_27(a, b)
term(23) = term(23) + wm_interm_26(a, b) * wm_interm_28(a, b)
term(24) = term(24) + wm_interm_20(a, b) * wm_interm_45(b, a)
term(25) = term(25) + wm_interm_21(a, b) * wm_interm_45(b, a)
term(26) = term(26) + wm_interm_14(a, b) * wm_interm_27(a, b)
term(27) = term(27) + wm_interm_14(a, b) * wm_interm_28(a, b)
term(28) = term(28) + wm_interm_10(a, b) * wm_interm_25(a, b)
term(29) = term(29) + wm_interm_10(a, b) * wm_interm_26(a, b)
term(30) = term(30) + wm_interm_22(a, b) * wm_interm_53(b, a)
term(31) = term(31) + wm_interm_23(a, b) * wm_interm_53(b, a)
end do 
end do 

term(16) = term(16) * 8.0d+0 
term(17) = term(17) * (-16.0d+0) 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * 8.0d+0 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * 8.0d+0 
term(22) = term(22) * 8.0d+0 
term(23) = term(23) * (-16.0d+0) 
term(24) = term(24) * (-8.0d+0) 
term(25) = term(25) * 4.0d+0 
term(26) = term(26) * 4.0d+0 
term(27) = term(27) * (-8.0d+0) 
term(28) = term(28) * 4.0d+0 
term(29) = term(29) * (-8.0d+0) 
term(30) = term(30) * 4.0d+0 
term(31) = term(31) * (-8.0d+0) 

do a = nocc + 1, nactive 
term(32) = term(32) + r1(vrdav_Rl, a,q) * wm_interm_2(a, p)
term(33) = term(33) + r1(vrdav_Rl, a,q) * wm_interm_3(a, p)
term(34) = term(34) + s1(a,q) * wm_interm_4(a, p)
term(35) = term(35) + s1(a,q) * wm_interm_5(a, p)
term(36) = term(36) + r1(vrdav_Rr, a,p) * wm_interm_6(a, q)
term(37) = term(37) + r1(vrdav_Rr, a,p) * wm_interm_7(a, q)
term(38) = term(38) + t1(a,q) * wm_interm_8(a, p)
term(39) = term(39) + t1(a,q) * wm_interm_9(a, p)
term(40) = term(40) + wm_interm_41(a, p) * wm_interm_46(a, q)
term(41) = term(41) + wm_interm_40(a, p) * wm_interm_47(a, q)
term(42) = term(42) + wm_interm_41(a, p) * wm_interm_47(a, q)
term(43) = term(43) + wm_interm_40(a, p) * wm_interm_46(a, q)
term(44) = term(44) + wm_interm_44(a, q) * wm_interm_9(a, p)
term(45) = term(45) + wm_interm_44(a, q) * wm_interm_8(a, p)
term(46) = term(46) + wm_interm_48(a, q) * wm_interm_8(a, p)
term(47) = term(47) + wm_interm_48(a, q) * wm_interm_9(a, p)
term(48) = term(48) + wm_interm_2(a, p) * wm_interm_6(a, q)
term(49) = term(49) + wm_interm_3(a, p) * wm_interm_6(a, q)
term(50) = term(50) + wm_interm_2(a, p) * wm_interm_7(a, q)
term(51) = term(51) + wm_interm_3(a, p) * wm_interm_7(a, q)
term(52) = term(52) + r1(vrdav_Rl, a,p) * r1(vrdav_Rr, a,q)
end do 

term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * 2.0d+0 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * 4.0d+0 
term(36) = term(36) * 4.0d+0 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * 4.0d+0 
term(40) = term(40) * (-8.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * 4.0d+0 
term(43) = term(43) * 4.0d+0 
term(44) = term(44) * 4.0d+0 
term(45) = term(45) * (-2.0d+0) 
term(46) = term(46) * 4.0d+0 
term(47) = term(47) * (-8.0d+0) 
term(48) = term(48) * 8.0d+0 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * 2.0d+0 
term(52) = term(52) * (-2.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(53) = term(53) + r2(vrdav_Rl, b,q,c,i) * r2(vrdav_Rr, a,p,b,i) * wm_interm_20(a, c)
term(54) = term(54) + r2(vrdav_Rl, a,q,b,i) * r2(vrdav_Rr, a,p,c,i) * wm_interm_20(c, b)
term(55) = term(55) + r2(vrdav_Rl, b,q,c,i) * r2(vrdav_Rr, a,p,b,i) * wm_interm_21(a, c)
term(56) = term(56) + r2(vrdav_Rl, a,q,b,i) * r2(vrdav_Rr, a,p,c,i) * wm_interm_21(c, b)
term(57) = term(57) + r2(vrdav_Rl, a,i,b,q) * r2(vrdav_Rr, a,p,c,i) * wm_interm_20(c, b)
term(58) = term(58) + r2(vrdav_Rl, b,q,c,i) * r2(vrdav_Rr, a,p,c,i) * wm_interm_20(a, b)
term(59) = term(59) + r2(vrdav_Rl, a,i,b,q) * r2(vrdav_Rr, a,p,c,i) * wm_interm_21(c, b)
term(60) = term(60) + r2(vrdav_Rl, b,q,c,i) * r2(vrdav_Rr, a,p,c,i) * wm_interm_21(a, b)
term(61) = term(61) + r2(vrdav_Rr, a,p,b,i) * s2(b,c,q,i) * wm_interm_25(a, c)
term(62) = term(62) + r2(vrdav_Rr, a,p,b,i) * s2(a,c,q,i) * wm_interm_25(b, c)
term(63) = term(63) + r2(vrdav_Rr, a,p,b,i) * s2(b,c,q,i) * wm_interm_26(a, c)
term(64) = term(64) + r2(vrdav_Rr, a,p,b,i) * s2(a,c,q,i) * wm_interm_26(b, c)
term(65) = term(65) + r2(vrdav_Rr, a,p,b,i) * s2(a,c,i,q) * wm_interm_25(b, c)
term(66) = term(66) + r2(vrdav_Rr, a,p,b,i) * s2(b,c,i,q) * wm_interm_25(a, c)
term(67) = term(67) + r2(vrdav_Rr, a,p,b,i) * s2(a,c,i,q) * wm_interm_26(b, c)
term(68) = term(68) + r2(vrdav_Rr, a,p,b,i) * s2(b,c,i,q) * wm_interm_26(a, c)
term(69) = term(69) + s2(a,b,p,i) * t2(a,c,q,i) * wm_interm_22(c, b)
term(70) = term(70) + s2(a,b,p,i) * t2(a,c,q,i) * wm_interm_23(c, b)
term(71) = term(71) + r2(vrdav_Rl, a,p,c,i) * t2(a,b,i,q) * wm_interm_27(c, b)
term(72) = term(72) + r2(vrdav_Rl, a,p,c,i) * t2(a,b,i,q) * wm_interm_28(c, b)
term(73) = term(73) + r2(vrdav_Rl, a,p,c,i) * t2(b,c,q,i) * wm_interm_27(a, b)
term(74) = term(74) + r2(vrdav_Rl, a,p,c,i) * t2(b,c,q,i) * wm_interm_28(a, b)
term(75) = term(75) + s2(b,c,p,i) * t2(a,c,q,i) * wm_interm_22(a, b)
term(76) = term(76) + s2(b,c,p,i) * t2(a,c,q,i) * wm_interm_23(a, b)
term(77) = term(77) + s2(b,c,i,p) * t2(a,c,q,i) * wm_interm_22(a, b)
term(78) = term(78) + s2(b,c,i,p) * t2(a,c,q,i) * wm_interm_23(a, b)
term(79) = term(79) + s2(a,b,i,p) * t2(a,c,q,i) * wm_interm_22(c, b)
term(80) = term(80) + s2(a,b,i,p) * t2(a,c,q,i) * wm_interm_23(c, b)
term(81) = term(81) + r2(vrdav_Rl, a,p,b,i) * t2(b,c,q,i) * wm_interm_27(a, c)
term(82) = term(82) + r2(vrdav_Rl, a,p,b,i) * t2(b,c,q,i) * wm_interm_28(a, c)
term(83) = term(83) + r2(vrdav_Rl, a,p,b,i) * t2(a,c,q,i) * wm_interm_27(b, c)
term(84) = term(84) + r2(vrdav_Rl, a,p,b,i) * t2(a,c,q,i) * wm_interm_28(b, c)
end do 
end do 
end do 
end do 

term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * 8.0d+0 
term(55) = term(55) * 2.0d+0 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * 8.0d+0 
term(59) = term(59) * 2.0d+0 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * 2.0d+0 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * 8.0d+0 
term(65) = term(65) * 2.0d+0 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * (-4.0d+0) 
term(68) = term(68) * 8.0d+0 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * 8.0d+0 
term(71) = term(71) * 2.0d+0 
term(72) = term(72) * (-4.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * 8.0d+0 
term(75) = term(75) * (-4.0d+0) 
term(76) = term(76) * 8.0d+0 
term(77) = term(77) * 2.0d+0 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * 2.0d+0 
term(80) = term(80) * (-4.0d+0) 
term(81) = term(81) * 2.0d+0 
term(82) = term(82) * (-4.0d+0) 
term(83) = term(83) * (-4.0d+0) 
term(84) = term(84) * 8.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(85) = term(85) + r1(vrdav_Rl, a,i) * wm_interm_2(a, i)
term(86) = term(86) + r1(vrdav_Rl, a,i) * wm_interm_3(a, i)
term(87) = term(87) + r1(vrdav_Rr, a,i) * wm_interm_6(a, i)
term(88) = term(88) + r1(vrdav_Rr, a,i) * wm_interm_7(a, i)
term(89) = term(89) + wm_interm_42(a, p, i, q) * wm_interm_47(a, i)
term(90) = term(90) + wm_interm_40(a, i) * wm_interm_47(a, i)
term(91) = term(91) + wm_interm_42(a, p, q, i) * wm_interm_47(a, i)
term(92) = term(92) + wm_interm_41(a, i) * wm_interm_47(a, i)
term(93) = term(93) + wm_interm_42(a, p, q, i) * wm_interm_46(a, i)
term(94) = term(94) + wm_interm_41(a, i) * wm_interm_46(a, i)
term(95) = term(95) + wm_interm_42(a, p, i, q) * wm_interm_46(a, i)
term(96) = term(96) + wm_interm_40(a, i) * wm_interm_46(a, i)
term(97) = term(97) + wm_interm_40(a, i) * wm_interm_49(a, p, i, q)
term(98) = term(98) + wm_interm_41(a, i) * wm_interm_49(a, p, i, q)
term(99) = term(99) + wm_interm_41(a, i) * wm_interm_49(a, p, q, i)
term(100) = term(100) + wm_interm_40(a, i) * wm_interm_49(a, p, q, i)
term(101) = term(101) + wm_interm_39(a, i, p, q) * wm_interm_4(a, i)
term(102) = term(102) + wm_interm_39(a, i, p, q) * wm_interm_5(a, i)
term(103) = term(103) + wm_interm_39(a, p, i, q) * wm_interm_4(a, i)
term(104) = term(104) + wm_interm_39(a, p, i, q) * wm_interm_5(a, i)
term(105) = term(105) + wm_interm_44(a, i) * wm_interm_55(a, q, i, p)
term(106) = term(106) + wm_interm_44(a, i) * wm_interm_9(a, i)
term(107) = term(107) + wm_interm_48(a, i) * wm_interm_55(a, q, i, p)
term(108) = term(108) + wm_interm_48(a, i) * wm_interm_9(a, i)
term(109) = term(109) + wm_interm_44(a, i) * wm_interm_54(a, i, q, p)
term(110) = term(110) + wm_interm_44(a, i) * wm_interm_55(a, i, q, p)
term(111) = term(111) + wm_interm_44(a, i) * wm_interm_8(a, i)
term(112) = term(112) + wm_interm_48(a, i) * wm_interm_55(a, i, q, p)
term(113) = term(113) + wm_interm_48(a, i) * wm_interm_8(a, i)
term(114) = term(114) + wm_interm_44(a, i) * wm_interm_54(a, q, i, p)
term(115) = term(115) + wm_interm_48(a, i) * wm_interm_54(a, q, i, p)
term(116) = term(116) + wm_interm_48(a, i) * wm_interm_54(a, i, q, p)
term(117) = term(117) + wm_interm_56(a, p, i, q) * wm_interm_8(a, i)
term(118) = term(118) + wm_interm_56(a, p, i, q) * wm_interm_9(a, i)
term(119) = term(119) + wm_interm_56(a, p, q, i) * wm_interm_8(a, i)
term(120) = term(120) + wm_interm_56(a, p, q, i) * wm_interm_9(a, i)
term(121) = term(121) + wm_interm_2(a, i) * wm_interm_6(a, i)
term(122) = term(122) + wm_interm_3(a, i) * wm_interm_6(a, i)
term(123) = term(123) + wm_interm_2(a, i) * wm_interm_7(a, i)
term(124) = term(124) + wm_interm_3(a, i) * wm_interm_7(a, i)
term(125) = term(125) + wm_interm_35(a, p, i, q) * wm_interm_6(a, i)
term(126) = term(126) + wm_interm_35(a, i, p, q) * wm_interm_6(a, i)
term(127) = term(127) + wm_interm_35(a, p, i, q) * wm_interm_7(a, i)
term(128) = term(128) + wm_interm_35(a, i, p, q) * wm_interm_7(a, i)
term(129) = term(129) + wm_interm_2(a, i) * wm_interm_36(a, i, p, q)
term(130) = term(130) + wm_interm_2(a, i) * wm_interm_36(a, p, i, q)
term(131) = term(131) + wm_interm_2(a, i) * wm_interm_37(a, p, i, q)
term(132) = term(132) + wm_interm_2(a, i) * wm_interm_37(a, i, p, q)
term(133) = term(133) + wm_interm_3(a, i) * wm_interm_36(a, i, p, q)
term(134) = term(134) + wm_interm_3(a, i) * wm_interm_36(a, p, i, q)
term(135) = term(135) + wm_interm_3(a, i) * wm_interm_37(a, p, i, q)
term(136) = term(136) + wm_interm_3(a, i) * wm_interm_37(a, i, p, q)
end do 
end do 

term(85) = term(85) * 8.0d+0 
term(86) = term(86) * (-4.0d+0) 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * 4.0d+0 
term(89) = term(89) * (-2.0d+0) 
term(90) = term(90) * 4.0d+0 
term(91) = term(91) * 4.0d+0 
term(92) = term(92) * (-8.0d+0) 
term(93) = term(93) * (-8.0d+0) 
term(94) = term(94) * 16.0d+0 
term(95) = term(95) * 4.0d+0 
term(96) = term(96) * (-8.0d+0) 
term(97) = term(97) * (-2.0d+0) 
term(98) = term(98) * 4.0d+0 
term(99) = term(99) * (-8.0d+0) 
term(100) = term(100) * 4.0d+0 
term(101) = term(101) * 4.0d+0 
term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * (-2.0d+0) 
term(104) = term(104) * 4.0d+0 
term(105) = term(105) * 2.0d+0 
term(106) = term(106) * (-8.0d+0) 
term(107) = term(107) * (-4.0d+0) 
term(108) = term(108) * 16.0d+0 
term(109) = term(109) * 2.0d+0 
term(110) = -term(110) 
term(111) = term(111) * 4.0d+0 
term(112) = term(112) * 2.0d+0 
term(113) = term(113) * (-8.0d+0) 
term(114) = -term(114) 
term(115) = term(115) * 2.0d+0 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * 4.0d+0 
term(118) = term(118) * (-8.0d+0) 
term(119) = term(119) * (-2.0d+0) 
term(120) = term(120) * 4.0d+0 
term(121) = term(121) * (-16.0d+0) 
term(122) = term(122) * 8.0d+0 
term(123) = term(123) * 8.0d+0 
term(124) = term(124) * (-4.0d+0) 
term(125) = term(125) * 4.0d+0 
term(126) = term(126) * (-8.0d+0) 
term(127) = term(127) * (-2.0d+0) 
term(128) = term(128) * 4.0d+0 
term(129) = term(129) * (-2.0d+0) 
term(130) = term(130) * 4.0d+0 
term(131) = term(131) * (-2.0d+0) 
term(132) = term(132) * 4.0d+0 
term(134) = term(134) * (-2.0d+0) 
term(136) = term(136) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(137) = term(137) + wm_interm_42(a, p, i, j) * wm_interm_49(a, q, i, j)
term(138) = term(138) + wm_interm_42(a, p, i, j) * wm_interm_49(a, q, j, i)
term(139) = term(139) + wm_interm_42(a, i, j, p) * wm_interm_49(a, i, j, q)
term(140) = term(140) + wm_interm_42(a, i, p, j) * wm_interm_49(a, i, q, j)
term(141) = term(141) + wm_interm_42(a, i, j, p) * wm_interm_49(a, i, q, j)
term(142) = term(142) + wm_interm_42(a, i, p, j) * wm_interm_49(a, i, j, q)
term(143) = term(143) + wm_interm_39(a, q, i, j) * wm_interm_50(a, j, p, i)
term(144) = term(144) + wm_interm_39(a, q, i, j) * wm_interm_50(a, j, i, p)
term(145) = term(145) + wm_interm_39(a, i, q, j) * wm_interm_50(a, j, i, p)
term(146) = term(146) + wm_interm_39(a, i, q, j) * wm_interm_50(a, j, p, i)
term(147) = term(147) + wm_interm_39(a, i, j, q) * wm_interm_50(a, p, i, j)
term(148) = term(148) + wm_interm_55(a, i, j, p) * wm_interm_56(a, q, i, j)
term(149) = term(149) + wm_interm_54(a, i, j, p) * wm_interm_56(a, q, i, j)
term(150) = term(150) + wm_interm_54(a, p, i, j) * wm_interm_56(a, j, i, q)
term(151) = term(151) + wm_interm_55(a, p, i, j) * wm_interm_56(a, j, i, q)
term(152) = term(152) + wm_interm_54(a, i, p, j) * wm_interm_56(a, j, i, q)
term(153) = term(153) + wm_interm_55(a, i, p, j) * wm_interm_56(a, j, i, q)
term(154) = term(154) + wm_interm_54(a, i, p, j) * wm_interm_56(a, j, q, i)
term(155) = term(155) + wm_interm_54(a, p, i, j) * wm_interm_56(a, j, q, i)
term(156) = term(156) + wm_interm_55(a, p, i, j) * wm_interm_56(a, j, q, i)
term(157) = term(157) + wm_interm_55(a, i, p, j) * wm_interm_56(a, j, q, i)
term(158) = term(158) + wm_interm_35(a, i, p, j) * wm_interm_37(a, q, i, j)
term(159) = term(159) + wm_interm_35(a, p, i, j) * wm_interm_37(a, q, i, j)
term(160) = term(160) + wm_interm_35(a, p, i, j) * wm_interm_37(a, i, q, j)
term(161) = term(161) + wm_interm_35(a, i, p, j) * wm_interm_37(a, i, q, j)
term(162) = term(162) + wm_interm_35(a, p, i, j) * wm_interm_36(a, q, i, j)
term(163) = term(163) + wm_interm_35(a, i, p, j) * wm_interm_36(a, q, i, j)
term(164) = term(164) + wm_interm_35(a, i, p, j) * wm_interm_36(a, i, q, j)
term(165) = term(165) + wm_interm_35(a, p, i, j) * wm_interm_36(a, i, q, j)
term(166) = term(166) + wm_interm_35(a, i, j, p) * wm_interm_36(a, i, j, q)
term(167) = term(167) + wm_interm_35(a, i, j, p) * wm_interm_36(a, j, i, q)
term(168) = term(168) + wm_interm_35(a, i, j, p) * wm_interm_37(a, j, i, q)
term(169) = term(169) + wm_interm_35(a, i, j, p) * wm_interm_37(a, i, j, q)
end do 
end do 
end do 

term(137) = term(137) * 4.0d+0 
term(138) = term(138) * (-2.0d+0) 
term(139) = term(139) * 4.0d+0 
term(140) = term(140) * 4.0d+0 
term(141) = term(141) * (-2.0d+0) 
term(142) = term(142) * (-2.0d+0) 
term(143) = term(143) * (-2.0d+0) 
term(144) = term(144) * 4.0d+0 
term(145) = term(145) * (-2.0d+0) 
term(146) = term(146) * 4.0d+0 
term(147) = term(147) * (-2.0d+0) 
term(148) = -term(148) 
term(149) = term(149) * 2.0d+0 
term(150) = -term(150) 
term(151) = term(151) * 2.0d+0 
term(152) = term(152) * 2.0d+0 
term(153) = -term(153) 
term(154) = -term(154) 
term(155) = term(155) * 2.0d+0 
term(156) = -term(156) 
term(157) = term(157) * 2.0d+0 
term(158) = -term(158) 
term(159) = term(159) * 2.0d+0 
term(160) = -term(160) 
term(161) = term(161) * 2.0d+0 
term(162) = -term(162) 
term(163) = term(163) * 2.0d+0 
term(164) = -term(164) 
term(165) = term(165) * 2.0d+0 
term(166) = -term(166) 
term(167) = term(167) * 2.0d+0 
term(168) = -term(168) 
term(169) = term(169) * 2.0d+0 

do i = 1, nocc 
do j = 1, nocc 
term(170) = term(170) + wm_interm_18(i, j) * wm_interm_24(j, p, i, q)
term(171) = term(171) + wm_interm_19(i, j) * wm_interm_24(j, p, i, q)
term(172) = term(172) + wm_interm_18(i, j) * wm_interm_24(p, j, i, q)
term(173) = term(173) + wm_interm_19(i, j) * wm_interm_24(p, j, i, q)
end do 
end do 

term(170) = term(170) * (-4.0d+0) 
term(171) = term(171) * 8.0d+0 
term(172) = term(172) * 2.0d+0 
term(173) = term(173) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(174) = term(174) + wm_interm_15(q, i, j, p) * wm_interm_16(j, i)
term(175) = term(175) + wm_interm_15(q, i, p, j) * wm_interm_16(j, i)
term(176) = term(176) + wm_interm_15(q, i, j, p) * wm_interm_17(j, i)
term(177) = term(177) + wm_interm_15(q, i, p, j) * wm_interm_17(j, i)
term(178) = term(178) + wm_interm_16(i, j) * wm_interm_18(j, i)
term(179) = term(179) + wm_interm_16(i, j) * wm_interm_19(j, i)
term(180) = term(180) + wm_interm_17(i, j) * wm_interm_18(j, i)
term(181) = term(181) + wm_interm_17(i, j) * wm_interm_19(j, i)
term(182) = term(182) + wm_interm_15(i, q, p, j) * wm_interm_16(j, i)
term(183) = term(183) + wm_interm_15(i, q, j, p) * wm_interm_16(j, i)
term(184) = term(184) + wm_interm_15(i, q, p, j) * wm_interm_17(j, i)
term(185) = term(185) + wm_interm_15(i, q, j, p) * wm_interm_17(j, i)
term(186) = term(186) + wm_interm_29(i, j) * wm_interm_30(i, p, q, j)
term(187) = term(187) + wm_interm_29(i, j) * wm_interm_30(i, p, j, q)
term(188) = term(188) + wm_interm_29(i, j) * wm_interm_31(i, j)
term(189) = term(189) + wm_interm_29(i, j) * wm_interm_32(i, j)
term(190) = term(190) + wm_interm_30(i, p, q, j) * wm_interm_34(i, j)
term(191) = term(191) + wm_interm_30(i, p, j, q) * wm_interm_34(i, j)
term(192) = term(192) + wm_interm_31(i, j) * wm_interm_34(i, j)
term(193) = term(193) + wm_interm_32(i, j) * wm_interm_34(i, j)
term(194) = term(194) + wm_interm_31(i, j) * wm_interm_33(i, p, q, j)
term(195) = term(195) + wm_interm_32(i, j) * wm_interm_33(i, p, q, j)
term(196) = term(196) + wm_interm_31(i, j) * wm_interm_33(p, i, q, j)
term(197) = term(197) + wm_interm_32(i, j) * wm_interm_33(p, i, q, j)
term(198) = term(198) + wm_interm_31(i, j) * wm_interm_33(p, i, j, q)
term(199) = term(199) + wm_interm_32(i, j) * wm_interm_33(p, i, j, q)
term(200) = term(200) + wm_interm_31(i, j) * wm_interm_33(i, p, j, q)
term(201) = term(201) + wm_interm_32(i, j) * wm_interm_33(i, p, j, q)
term(202) = term(202) + wm_interm_38(i, j) * wm_interm_51(j, i)
term(203) = term(203) + wm_interm_12(i, j) * wm_interm_13(i, j)
term(204) = term(204) + wm_interm_16(i, j) * wm_interm_38(j, i)
term(205) = term(205) + wm_interm_17(i, j) * wm_interm_38(j, i)
term(206) = term(206) + wm_interm_24(i, p, j, q) * wm_interm_38(j, i)
term(207) = term(207) + wm_interm_24(p, i, j, q) * wm_interm_38(j, i)
term(208) = term(208) + wm_interm_13(i, j) * wm_interm_30(i, p, q, j)
term(209) = term(209) + wm_interm_13(i, j) * wm_interm_30(i, p, j, q)
term(210) = term(210) + wm_interm_13(i, j) * wm_interm_31(i, j)
term(211) = term(211) + wm_interm_13(i, j) * wm_interm_32(i, j)
term(212) = term(212) + wm_interm_12(i, j) * wm_interm_29(i, j)
term(213) = term(213) + wm_interm_12(i, j) * wm_interm_34(i, j)
term(214) = term(214) + wm_interm_12(i, j) * wm_interm_33(i, p, q, j)
term(215) = term(215) + wm_interm_12(i, j) * wm_interm_33(p, i, q, j)
term(216) = term(216) + wm_interm_12(i, j) * wm_interm_33(p, i, j, q)
term(217) = term(217) + wm_interm_12(i, j) * wm_interm_33(i, p, j, q)
term(218) = term(218) + wm_interm_15(q, i, j, p) * wm_interm_51(j, i)
term(219) = term(219) + wm_interm_15(q, i, p, j) * wm_interm_51(j, i)
term(220) = term(220) + wm_interm_18(i, j) * wm_interm_51(j, i)
term(221) = term(221) + wm_interm_19(i, j) * wm_interm_51(j, i)
term(222) = term(222) + wm_interm_15(i, q, p, j) * wm_interm_51(j, i)
term(223) = term(223) + wm_interm_15(i, q, j, p) * wm_interm_51(j, i)
end do 
end do 

term(175) = term(175) * (-2.0d+0) 
term(176) = term(176) * (-2.0d+0) 
term(177) = term(177) * 4.0d+0 
term(178) = term(178) * (-4.0d+0) 
term(179) = term(179) * 8.0d+0 
term(180) = term(180) * 8.0d+0 
term(181) = term(181) * (-16.0d+0) 
term(183) = term(183) * (-2.0d+0) 
term(184) = term(184) * (-2.0d+0) 
term(185) = term(185) * 4.0d+0 
term(186) = term(186) * 2.0d+0 
term(187) = term(187) * (-4.0d+0) 
term(188) = term(188) * (-4.0d+0) 
term(189) = term(189) * 8.0d+0 
term(190) = term(190) * (-4.0d+0) 
term(191) = term(191) * 8.0d+0 
term(192) = term(192) * 8.0d+0 
term(193) = term(193) * (-16.0d+0) 
term(195) = term(195) * (-2.0d+0) 
term(196) = term(196) * (-2.0d+0) 
term(197) = term(197) * 4.0d+0 
term(199) = term(199) * (-2.0d+0) 
term(200) = term(200) * (-2.0d+0) 
term(201) = term(201) * 4.0d+0 
term(202) = term(202) * (-4.0d+0) 
term(203) = term(203) * (-4.0d+0) 
term(204) = term(204) * 4.0d+0 
term(205) = term(205) * (-8.0d+0) 
term(206) = term(206) * 4.0d+0 
term(207) = term(207) * (-2.0d+0) 
term(208) = term(208) * (-2.0d+0) 
term(209) = term(209) * 4.0d+0 
term(210) = term(210) * 4.0d+0 
term(211) = term(211) * (-8.0d+0) 
term(212) = term(212) * 4.0d+0 
term(213) = term(213) * (-8.0d+0) 
term(214) = -term(214) 
term(215) = term(215) * 2.0d+0 
term(216) = -term(216) 
term(217) = term(217) * 2.0d+0 
term(218) = -term(218) 
term(219) = term(219) * 2.0d+0 
term(220) = term(220) * 4.0d+0 
term(221) = term(221) * (-8.0d+0) 
term(222) = -term(222) 
term(223) = term(223) * 2.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(224) = term(224) + wm_interm_57(a, b, q, i) * wm_interm_58(a, b, p, i)
term(225) = term(225) + wm_interm_57(a, b, q, i) * wm_interm_59(a, b, p, i)
term(226) = term(226) + wm_interm_57(a, b, q, i) * wm_interm_60(a, b, p, i)
term(227) = term(227) + wm_interm_57(a, b, q, i) * wm_interm_61(a, b, p, i)
term(228) = term(228) + wm_interm_62(a, b, i, p) * wm_interm_63(b, a, q, i)
term(229) = term(229) + wm_interm_63(a, b, q, i) * wm_interm_64(b, a, i, p)
term(230) = term(230) + wm_interm_62(a, b, i, p) * wm_interm_65(b, a, q, i)
term(231) = term(231) + wm_interm_64(a, b, i, p) * wm_interm_65(b, a, q, i)
term(232) = term(232) + wm_interm_60(a, b, p, i) * wm_interm_66(a, b, q, i)
term(233) = term(233) + wm_interm_61(a, b, p, i) * wm_interm_66(a, b, q, i)
term(234) = term(234) + wm_interm_62(a, b, i, p) * wm_interm_67(b, a, q, i)
term(235) = term(235) + wm_interm_64(a, b, i, p) * wm_interm_67(b, a, q, i)
term(236) = term(236) + wm_interm_58(a, b, p, i) * wm_interm_66(a, b, q, i)
term(237) = term(237) + wm_interm_59(a, b, p, i) * wm_interm_66(a, b, q, i)
term(238) = term(238) + wm_interm_62(a, b, i, p) * wm_interm_68(b, a, q, i)
term(239) = term(239) + wm_interm_64(a, b, i, p) * wm_interm_68(b, a, q, i)
term(240) = term(240) + wm_interm_63(a, b, q, i) * wm_interm_69(b, a, i, p)
term(241) = term(241) + wm_interm_63(a, b, q, i) * wm_interm_70(b, a, i, p)
term(242) = term(242) + wm_interm_59(a, b, p, i) * wm_interm_71(a, b, q, i)
term(243) = term(243) + wm_interm_58(a, b, p, i) * wm_interm_71(a, b, q, i)
term(244) = term(244) + wm_interm_65(a, b, q, i) * wm_interm_70(b, a, i, p)
term(245) = term(245) + wm_interm_65(a, b, q, i) * wm_interm_69(b, a, i, p)
term(246) = term(246) + wm_interm_60(a, b, p, i) * wm_interm_71(a, b, q, i)
term(247) = term(247) + wm_interm_61(a, b, p, i) * wm_interm_71(a, b, q, i)
term(248) = term(248) + wm_interm_67(a, b, q, i) * wm_interm_70(b, a, i, p)
term(249) = term(249) + wm_interm_67(a, b, q, i) * wm_interm_69(b, a, i, p)
term(250) = term(250) + wm_interm_60(a, b, p, i) * wm_interm_72(a, b, q, i)
term(251) = term(251) + wm_interm_61(a, b, p, i) * wm_interm_72(a, b, q, i)
term(252) = term(252) + wm_interm_68(a, b, q, i) * wm_interm_70(b, a, i, p)
term(253) = term(253) + wm_interm_68(a, b, q, i) * wm_interm_69(b, a, i, p)
term(254) = term(254) + wm_interm_58(a, b, p, i) * wm_interm_72(a, b, q, i)
term(255) = term(255) + wm_interm_59(a, b, p, i) * wm_interm_72(a, b, q, i)
term(256) = term(256) + wm_interm_62(a, b, p, i) * wm_interm_63(b, a, i, q)
term(257) = term(257) + wm_interm_63(a, b, i, q) * wm_interm_64(b, a, p, i)
term(258) = term(258) + wm_interm_63(a, b, i, q) * wm_interm_69(b, a, p, i)
term(259) = term(259) + wm_interm_63(a, b, i, q) * wm_interm_70(b, a, p, i)
term(260) = term(260) + wm_interm_62(a, b, p, i) * wm_interm_65(b, a, i, q)
term(261) = term(261) + wm_interm_64(a, b, p, i) * wm_interm_65(b, a, i, q)
term(262) = term(262) + wm_interm_65(a, b, i, q) * wm_interm_70(b, a, p, i)
term(263) = term(263) + wm_interm_65(a, b, i, q) * wm_interm_69(b, a, p, i)
term(264) = term(264) + wm_interm_60(a, b, i, p) * wm_interm_72(a, b, i, q)
term(265) = term(265) + wm_interm_61(a, b, i, p) * wm_interm_72(a, b, i, q)
term(266) = term(266) + wm_interm_60(a, b, i, p) * wm_interm_66(a, b, i, q)
term(267) = term(267) + wm_interm_61(a, b, i, p) * wm_interm_66(a, b, i, q)
term(268) = term(268) + wm_interm_57(a, b, i, q) * wm_interm_58(a, b, i, p)
term(269) = term(269) + wm_interm_57(a, b, i, q) * wm_interm_59(a, b, i, p)
term(270) = term(270) + wm_interm_59(a, b, i, p) * wm_interm_71(a, b, i, q)
term(271) = term(271) + wm_interm_58(a, b, i, p) * wm_interm_71(a, b, i, q)
term(272) = term(272) + wm_interm_60(a, b, i, p) * wm_interm_71(a, b, i, q)
term(273) = term(273) + wm_interm_61(a, b, i, p) * wm_interm_71(a, b, i, q)
term(274) = term(274) + wm_interm_57(a, b, i, q) * wm_interm_60(a, b, i, p)
term(275) = term(275) + wm_interm_57(a, b, i, q) * wm_interm_61(a, b, i, p)
term(276) = term(276) + wm_interm_58(a, b, i, p) * wm_interm_72(a, b, i, q)
term(277) = term(277) + wm_interm_59(a, b, i, p) * wm_interm_72(a, b, i, q)
term(278) = term(278) + wm_interm_58(a, b, i, p) * wm_interm_66(a, b, i, q)
term(279) = term(279) + wm_interm_59(a, b, i, p) * wm_interm_66(a, b, i, q)
term(280) = term(280) + wm_interm_67(a, b, i, q) * wm_interm_70(b, a, p, i)
term(281) = term(281) + wm_interm_67(a, b, i, q) * wm_interm_69(b, a, p, i)
term(282) = term(282) + wm_interm_62(a, b, p, i) * wm_interm_67(b, a, i, q)
term(283) = term(283) + wm_interm_64(a, b, p, i) * wm_interm_67(b, a, i, q)
term(284) = term(284) + wm_interm_68(a, b, i, q) * wm_interm_70(b, a, p, i)
term(285) = term(285) + wm_interm_68(a, b, i, q) * wm_interm_69(b, a, p, i)
term(286) = term(286) + wm_interm_62(a, b, p, i) * wm_interm_68(b, a, i, q)
term(287) = term(287) + wm_interm_64(a, b, p, i) * wm_interm_68(b, a, i, q)
term(288) = term(288) + r2(vrdav_Rl, a,p,b,i) * r2(vrdav_Rr, a,i,b,q)
term(289) = term(289) + r2(vrdav_Rl, a,p,b,i) * r2(vrdav_Rr, a,q,b,i)
end do 
end do 
end do 

term(224) = term(224) * 2.0d+0 
term(225) = term(225) * (-4.0d+0) 
term(226) = term(226) * (-4.0d+0) 
term(227) = term(227) * 8.0d+0 
term(228) = term(228) * 2.0d+0 
term(229) = term(229) * (-4.0d+0) 
term(230) = term(230) * (-4.0d+0) 
term(231) = term(231) * 8.0d+0 
term(232) = term(232) * 8.0d+0 
term(233) = term(233) * (-16.0d+0) 
term(234) = term(234) * 8.0d+0 
term(235) = term(235) * (-16.0d+0) 
term(236) = term(236) * (-4.0d+0) 
term(237) = term(237) * 8.0d+0 
term(238) = term(238) * (-4.0d+0) 
term(239) = term(239) * 8.0d+0 
term(240) = term(240) * 2.0d+0 
term(241) = term(241) * (-4.0d+0) 
term(242) = term(242) * 2.0d+0 
term(243) = term(243) * (-4.0d+0) 
term(244) = term(244) * 2.0d+0 
term(245) = term(245) * (-4.0d+0) 
term(246) = term(246) * 2.0d+0 
term(247) = term(247) * (-4.0d+0) 
term(248) = term(248) * (-4.0d+0) 
term(249) = term(249) * 8.0d+0 
term(250) = term(250) * (-4.0d+0) 
term(251) = term(251) * 8.0d+0 
term(252) = term(252) * 2.0d+0 
term(253) = term(253) * (-4.0d+0) 
term(254) = term(254) * 2.0d+0 
term(255) = term(255) * (-4.0d+0) 
term(256) = term(256) * 2.0d+0 
term(257) = term(257) * (-4.0d+0) 
term(258) = term(258) * 2.0d+0 
term(259) = term(259) * (-4.0d+0) 
term(260) = term(260) * (-4.0d+0) 
term(261) = term(261) * 8.0d+0 
term(262) = term(262) * 2.0d+0 
term(263) = term(263) * (-4.0d+0) 
term(264) = term(264) * (-4.0d+0) 
term(265) = term(265) * 8.0d+0 
term(266) = term(266) * 8.0d+0 
term(267) = term(267) * (-16.0d+0) 
term(268) = term(268) * 2.0d+0 
term(269) = term(269) * (-4.0d+0) 
term(270) = term(270) * 2.0d+0 
term(271) = term(271) * (-4.0d+0) 
term(272) = term(272) * 2.0d+0 
term(273) = term(273) * (-4.0d+0) 
term(274) = term(274) * (-4.0d+0) 
term(275) = term(275) * 8.0d+0 
term(276) = term(276) * 2.0d+0 
term(277) = term(277) * (-4.0d+0) 
term(278) = term(278) * (-4.0d+0) 
term(279) = term(279) * 8.0d+0 
term(280) = term(280) * (-4.0d+0) 
term(281) = term(281) * 8.0d+0 
term(282) = term(282) * 8.0d+0 
term(283) = term(283) * (-16.0d+0) 
term(284) = term(284) * 2.0d+0 
term(285) = term(285) * (-4.0d+0) 
term(286) = term(286) * (-4.0d+0) 
term(287) = term(287) * 8.0d+0 
term(288) = term(288) * 2.0d+0 
term(289) = term(289) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(290) = term(290) + wm_interm_15(i, j, k, l) * wm_interm_24(l, k, i, j)
term(291) = term(291) + wm_interm_15(i, j, k, l) * wm_interm_24(k, l, i, j)
end do 
end do 
end do 
end do 

term(290) = term(290) * (-2.0d+0) 
term(291) = term(291) * 4.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(292) = term(292) + wm_interm_30(i, j, k, l) * wm_interm_33(i, j, l, k)
term(293) = term(293) + wm_interm_30(i, j, k, l) * wm_interm_33(i, j, k, l)
end do 
end do 
end do 
end do 

term(292) = term(292) * (-2.0d+0) 
term(293) = term(293) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(294) = term(294) + wm_interm_62(a, b, i, j) * wm_interm_63(b, a, j, i)
term(295) = term(295) + wm_interm_63(a, b, i, j) * wm_interm_64(b, a, j, i)
term(296) = term(296) + wm_interm_62(a, b, i, j) * wm_interm_65(b, a, j, i)
term(297) = term(297) + wm_interm_64(a, b, i, j) * wm_interm_65(b, a, j, i)
term(298) = term(298) + wm_interm_62(a, b, i, j) * wm_interm_67(b, a, j, i)
term(299) = term(299) + wm_interm_64(a, b, i, j) * wm_interm_67(b, a, j, i)
term(300) = term(300) + wm_interm_62(a, b, i, j) * wm_interm_68(b, a, j, i)
term(301) = term(301) + wm_interm_64(a, b, i, j) * wm_interm_68(b, a, j, i)
term(302) = term(302) + wm_interm_63(a, b, i, j) * wm_interm_69(b, a, j, i)
term(303) = term(303) + wm_interm_63(a, b, i, j) * wm_interm_70(b, a, j, i)
term(304) = term(304) + wm_interm_65(a, b, i, j) * wm_interm_70(b, a, j, i)
term(305) = term(305) + wm_interm_65(a, b, i, j) * wm_interm_69(b, a, j, i)
term(306) = term(306) + wm_interm_67(a, b, i, j) * wm_interm_70(b, a, j, i)
term(307) = term(307) + wm_interm_67(a, b, i, j) * wm_interm_69(b, a, j, i)
term(308) = term(308) + wm_interm_68(a, b, i, j) * wm_interm_70(b, a, j, i)
term(309) = term(309) + wm_interm_68(a, b, i, j) * wm_interm_69(b, a, j, i)
end do 
end do 
end do 
end do 

term(294) = term(294) * (-4.0d+0) 
term(295) = term(295) * 8.0d+0 
term(296) = term(296) * 8.0d+0 
term(297) = term(297) * (-16.0d+0) 
term(298) = term(298) * (-16.0d+0) 
term(299) = term(299) * 32.0d+0 
term(300) = term(300) * 8.0d+0 
term(301) = term(301) * (-16.0d+0) 
term(302) = term(302) * (-4.0d+0) 
term(303) = term(303) * 8.0d+0 
term(304) = term(304) * (-4.0d+0) 
term(305) = term(305) * 8.0d+0 
term(306) = term(306) * 8.0d+0 
term(307) = term(307) * (-16.0d+0) 
term(308) = term(308) * (-4.0d+0) 
term(309) = term(309) * 8.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(310) = term(310) + wm_interm_39(a, i, j, q) * wm_interm_50(a, p, j, i)
term(311) = term(311) + wm_interm_54(a, i, j, p) * wm_interm_56(a, q, j, i)
term(312) = term(312) + wm_interm_55(a, i, j, p) * wm_interm_56(a, q, j, i)
end do 
end do 
end do 

term(310) = term(310) * 4.0d+0 
term(311) = -term(311) 
term(312) = term(312) * 2.0d+0 

term(313) = term(313) + wm_interm_0
term(314) = term(314) + wm_interm_1
term(315) = term(315) + wm_interm_11
term(316) = term(316) + wm_interm_0 * wm_interm_17(p, q)
term(317) = term(317) + wm_interm_1 * wm_interm_17(p, q)
term(318) = term(318) + wm_interm_0 * wm_interm_16(p, q)
term(319) = term(319) + wm_interm_1 * wm_interm_16(p, q)
term(320) = term(320) + wm_interm_11 * wm_interm_51(p, q)
term(321) = term(321) + wm_interm_11 * wm_interm_17(p, q)
term(322) = term(322) + wm_interm_11 * wm_interm_16(p, q)
term(323) = term(323) + wm_interm_0 * wm_interm_51(p, q)
term(324) = term(324) + wm_interm_1 * wm_interm_51(p, q)

term(313) = term(313) * (-2.0d+0) 
term(314) = term(314) * 4.0d+0 
term(315) = term(315) * 4.0d+0 
term(316) = term(316) * 4.0d+0 
term(317) = term(317) * (-8.0d+0) 
term(318) = term(318) * (-2.0d+0) 
term(319) = term(319) * 4.0d+0 
term(320) = term(320) * (-4.0d+0) 
term(321) = term(321) * (-8.0d+0) 
term(322) = term(322) * 4.0d+0 
term(323) = term(323) * 2.0d+0 
term(324) = term(324) * (-4.0d+0) 

do i = 1, nocc 
term(325) = term(325) + wm_interm_16(q, i) * wm_interm_18(i, p)
term(326) = term(326) + wm_interm_16(q, i) * wm_interm_19(i, p)
term(327) = term(327) + wm_interm_17(q, i) * wm_interm_18(i, p)
term(328) = term(328) + wm_interm_17(q, i) * wm_interm_19(i, p)
term(329) = term(329) + wm_interm_29(q, i) * wm_interm_31(p, i)
term(330) = term(330) + wm_interm_29(q, i) * wm_interm_32(p, i)
term(331) = term(331) + wm_interm_31(p, i) * wm_interm_34(q, i)
term(332) = term(332) + wm_interm_32(p, i) * wm_interm_34(q, i)
term(333) = term(333) + wm_interm_16(i, q) * wm_interm_18(p, i)
term(334) = term(334) + wm_interm_16(i, q) * wm_interm_19(p, i)
term(335) = term(335) + wm_interm_17(i, q) * wm_interm_18(p, i)
term(336) = term(336) + wm_interm_17(i, q) * wm_interm_19(p, i)
term(337) = term(337) + wm_interm_29(i, q) * wm_interm_31(i, p)
term(338) = term(338) + wm_interm_29(i, q) * wm_interm_32(i, p)
term(339) = term(339) + wm_interm_31(i, p) * wm_interm_34(i, q)
term(340) = term(340) + wm_interm_32(i, p) * wm_interm_34(i, q)
term(341) = term(341) + wm_interm_38(i, p) * wm_interm_51(q, i)
term(342) = term(342) + wm_interm_12(p, i) * wm_interm_13(q, i)
term(343) = term(343) + wm_interm_38(p, i) * wm_interm_51(i, q)
term(344) = term(344) + wm_interm_12(i, p) * wm_interm_13(i, q)
term(345) = term(345) + wm_interm_16(q, i) * wm_interm_38(i, p)
term(346) = term(346) + wm_interm_17(q, i) * wm_interm_38(i, p)
term(347) = term(347) + wm_interm_16(i, q) * wm_interm_38(p, i)
term(348) = term(348) + wm_interm_17(i, q) * wm_interm_38(p, i)
term(349) = term(349) + wm_interm_13(q, i) * wm_interm_31(p, i)
term(350) = term(350) + wm_interm_13(q, i) * wm_interm_32(p, i)
term(351) = term(351) + wm_interm_13(i, q) * wm_interm_31(i, p)
term(352) = term(352) + wm_interm_13(i, q) * wm_interm_32(i, p)
term(353) = term(353) + wm_interm_12(p, i) * wm_interm_29(q, i)
term(354) = term(354) + wm_interm_12(p, i) * wm_interm_34(q, i)
term(355) = term(355) + wm_interm_12(i, p) * wm_interm_29(i, q)
term(356) = term(356) + wm_interm_12(i, p) * wm_interm_34(i, q)
term(357) = term(357) + wm_interm_18(i, p) * wm_interm_51(q, i)
term(358) = term(358) + wm_interm_19(i, p) * wm_interm_51(q, i)
term(359) = term(359) + wm_interm_18(p, i) * wm_interm_51(i, q)
term(360) = term(360) + wm_interm_19(p, i) * wm_interm_51(i, q)
end do 

term(325) = term(325) * 2.0d+0 
term(326) = term(326) * (-4.0d+0) 
term(327) = term(327) * (-4.0d+0) 
term(328) = term(328) * 8.0d+0 
term(329) = term(329) * 2.0d+0 
term(330) = term(330) * (-4.0d+0) 
term(331) = term(331) * (-4.0d+0) 
term(332) = term(332) * 8.0d+0 
term(333) = term(333) * 2.0d+0 
term(334) = term(334) * (-4.0d+0) 
term(335) = term(335) * (-4.0d+0) 
term(336) = term(336) * 8.0d+0 
term(337) = term(337) * 2.0d+0 
term(338) = term(338) * (-4.0d+0) 
term(339) = term(339) * (-4.0d+0) 
term(340) = term(340) * 8.0d+0 
term(341) = term(341) * 2.0d+0 
term(342) = term(342) * 2.0d+0 
term(343) = term(343) * 2.0d+0 
term(344) = term(344) * 2.0d+0 
term(345) = term(345) * (-2.0d+0) 
term(346) = term(346) * 4.0d+0 
term(347) = term(347) * (-2.0d+0) 
term(348) = term(348) * 4.0d+0 
term(349) = term(349) * (-2.0d+0) 
term(350) = term(350) * 4.0d+0 
term(351) = term(351) * (-2.0d+0) 
term(352) = term(352) * 4.0d+0 
term(353) = term(353) * (-2.0d+0) 
term(354) = term(354) * 4.0d+0 
term(355) = term(355) * (-2.0d+0) 
term(356) = term(356) * 4.0d+0 
term(357) = term(357) * (-2.0d+0) 
term(358) = term(358) * 4.0d+0 
term(359) = term(359) * (-2.0d+0) 
term(360) = term(360) * 4.0d+0 


    calc_D_oo_wm2 = zero
    do s = 0, 360
    calc_D_oo_wm2 = calc_D_oo_wm2 + term(s)
    end do

    end function calc_D_oo_wm2
    
    function calc_D_ov_wm2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b 
    real(F64), dimension(0:109) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_30(i, p, j, k) * wm_interm_49(q, i, k, j)
term(1) = term(1) + wm_interm_30(i, p, j, k) * wm_interm_49(q, i, j, k)
term(2) = term(2) + wm_interm_30(i, j, k, p) * wm_interm_37(q, i, j, k)
term(3) = term(3) + wm_interm_30(i, j, p, k) * wm_interm_37(q, i, j, k)
term(4) = term(4) + wm_interm_30(i, j, p, k) * wm_interm_36(q, i, j, k)
term(5) = term(5) + wm_interm_30(i, j, k, p) * wm_interm_36(q, i, j, k)
term(6) = term(6) + wm_interm_15(i, j, p, k) * wm_interm_43(q, k, i, j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 4.0d+0 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(7) = term(7) + wm_interm_24(i, p, j, k) * wm_interm_54(q, j, k, i)
term(8) = term(8) + wm_interm_24(p, i, j, k) * wm_interm_54(q, j, k, i)
term(9) = term(9) + wm_interm_24(p, i, j, k) * wm_interm_55(q, j, k, i)
term(10) = term(10) + wm_interm_24(i, p, j, k) * wm_interm_55(q, j, k, i)
term(11) = term(11) + wm_interm_15(i, j, p, k) * wm_interm_43(q, k, j, i)
end do 
end do 
end do 

term(7) = -term(7) 
term(8) = term(8) * 2.0d+0 
term(9) = -term(9) 
term(10) = term(10) * 2.0d+0 
term(11) = term(11) * 2.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(12) = term(12) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_2(a, i)
term(13) = term(13) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_3(a, i)
term(14) = term(14) + r1(vrdav_Rr, a,p) * t1(q,i) * wm_interm_6(a, i)
term(15) = term(15) + r1(vrdav_Rr, a,p) * t1(q,i) * wm_interm_7(a, i)
end do 
end do 

term(12) = term(12) * 4.0d+0 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_50(a, k, p, j)
term(17) = term(17) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_50(a, k, j, p)
term(18) = term(18) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_50(a, k, p, j)
term(19) = term(19) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_42(b, i, k, p)
term(20) = term(20) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,i) * wm_interm_39(b, p, k, j)
term(21) = term(21) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,i) * wm_interm_39(b, k, p, j)
term(22) = term(22) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,j) * wm_interm_39(b, p, k, i)
term(23) = term(23) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,j) * wm_interm_39(b, k, p, i)
end do 
end do 
end do 
end do 
end do 

term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * 4.0d+0 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-2.0d+0) 
term(20) = term(20) * 4.0d+0 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(24) = term(24) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_50(a, k, j, p)
end do 
end do 
end do 
end do 
end do 

term(24) = term(24) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(25) = term(25) + wm_interm_15(i, j, k, p) * wm_interm_43(q, k, j, i)
end do 
end do 
end do 

term(25) = -term(25) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(26) = term(26) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_50(a, i, p, j)
term(27) = term(27) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_42(b, i, p, k)
term(28) = term(28) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,i,b,k) * wm_interm_39(b, p, k, j)
term(29) = term(29) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,i,b,k) * wm_interm_39(b, k, p, j)
term(30) = term(30) + r2(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_35(b, p, j, k)
term(31) = term(31) + r2(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_35(b, j, p, k)
term(32) = term(32) + r2(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_35(a, j, p, k)
term(33) = term(33) + r2(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_35(a, p, j, k)
end do 
end do 
end do 
end do 
end do 

term(26) = term(26) * (-8.0d+0) 
term(27) = term(27) * 4.0d+0 
term(28) = term(28) * (-2.0d+0) 
term(29) = term(29) * 4.0d+0 
term(30) = -term(30) 
term(31) = term(31) * 2.0d+0 
term(32) = -term(32) 
term(33) = term(33) * 2.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(34) = term(34) + wm_interm_15(i, j, k, p) * wm_interm_43(q, k, i, j)
end do 
end do 
end do 

term(34) = term(34) * 2.0d+0 

do a = nocc + 1, nactive 
term(35) = term(35) + r1(vrdav_Rl, a,p) * wm_interm_10(a, q)
term(36) = term(36) + r1(vrdav_Rr, a,p) * wm_interm_14(a, q)
term(37) = term(37) + r1(vrdav_Rl, a,p) * wm_interm_27(a, q)
term(38) = term(38) + r1(vrdav_Rl, a,p) * wm_interm_28(a, q)
term(39) = term(39) + r1(vrdav_Rr, a,p) * wm_interm_25(a, q)
term(40) = term(40) + r1(vrdav_Rr, a,p) * wm_interm_26(a, q)
term(41) = term(41) + wm_interm_14(q, a) * wm_interm_40(a, p)
term(42) = term(42) + wm_interm_14(q, a) * wm_interm_41(a, p)
term(43) = term(43) + wm_interm_14(a, q) * wm_interm_2(a, p)
term(44) = term(44) + wm_interm_14(a, q) * wm_interm_3(a, p)
term(45) = term(45) + wm_interm_4(a, p) * wm_interm_53(a, q)
term(46) = term(46) + wm_interm_5(a, p) * wm_interm_53(a, q)
term(47) = term(47) + wm_interm_20(a, q) * wm_interm_4(a, p)
term(48) = term(48) + wm_interm_20(a, q) * wm_interm_5(a, p)
term(49) = term(49) + wm_interm_21(a, q) * wm_interm_4(a, p)
term(50) = term(50) + wm_interm_21(a, q) * wm_interm_5(a, p)
term(51) = term(51) + wm_interm_25(q, a) * wm_interm_40(a, p)
term(52) = term(52) + wm_interm_26(q, a) * wm_interm_40(a, p)
term(53) = term(53) + wm_interm_25(q, a) * wm_interm_41(a, p)
term(54) = term(54) + wm_interm_26(q, a) * wm_interm_41(a, p)
term(55) = term(55) + wm_interm_2(a, p) * wm_interm_25(a, q)
term(56) = term(56) + wm_interm_2(a, p) * wm_interm_26(a, q)
term(57) = term(57) + wm_interm_25(a, q) * wm_interm_3(a, p)
term(58) = term(58) + wm_interm_26(a, q) * wm_interm_3(a, p)
end do 

term(35) = term(35) * 2.0d+0 
term(36) = term(36) * 2.0d+0 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * 4.0d+0 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * 4.0d+0 
term(41) = term(41) * 2.0d+0 
term(42) = term(42) * (-4.0d+0) 
term(43) = term(43) * 4.0d+0 
term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * 2.0d+0 
term(46) = term(46) * (-4.0d+0) 
term(47) = term(47) * 4.0d+0 
term(48) = term(48) * (-8.0d+0) 
term(49) = term(49) * (-2.0d+0) 
term(50) = term(50) * 4.0d+0 
term(51) = term(51) * (-2.0d+0) 
term(52) = term(52) * 4.0d+0 
term(53) = term(53) * 4.0d+0 
term(54) = term(54) * (-8.0d+0) 
term(55) = term(55) * (-4.0d+0) 
term(56) = term(56) * 8.0d+0 
term(57) = term(57) * 2.0d+0 
term(58) = term(58) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(59) = term(59) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,k,i) * wm_interm_42(b, j, k, p)
end do 
end do 
end do 
end do 
end do 

term(59) = term(59) * 4.0d+0 

do i = 1, nocc 
term(60) = term(60) + s1(q,i) * wm_interm_18(p, i)
term(61) = term(61) + s1(q,i) * wm_interm_19(p, i)
term(62) = term(62) + t1(q,i) * wm_interm_18(i, p)
term(63) = term(63) + t1(q,i) * wm_interm_19(i, p)
term(64) = term(64) + wm_interm_38(i, p) * wm_interm_44(q, i)
term(65) = term(65) + wm_interm_12(p, i) * wm_interm_46(q, i)
term(66) = term(66) + wm_interm_38(i, p) * wm_interm_48(q, i)
term(67) = term(67) + wm_interm_12(p, i) * wm_interm_47(q, i)
term(68) = term(68) + wm_interm_51(p, i) * wm_interm_8(q, i)
term(69) = term(69) + wm_interm_51(p, i) * wm_interm_9(q, i)
term(70) = term(70) + wm_interm_12(i, p) * wm_interm_6(q, i)
term(71) = term(71) + wm_interm_12(i, p) * wm_interm_7(q, i)
term(72) = term(72) + wm_interm_31(p, i) * wm_interm_46(q, i)
term(73) = term(73) + wm_interm_32(p, i) * wm_interm_46(q, i)
term(74) = term(74) + wm_interm_31(p, i) * wm_interm_47(q, i)
term(75) = term(75) + wm_interm_32(p, i) * wm_interm_47(q, i)
term(76) = term(76) + wm_interm_16(p, i) * wm_interm_8(q, i)
term(77) = term(77) + wm_interm_17(p, i) * wm_interm_8(q, i)
term(78) = term(78) + wm_interm_16(p, i) * wm_interm_9(q, i)
term(79) = term(79) + wm_interm_17(p, i) * wm_interm_9(q, i)
term(80) = term(80) + wm_interm_31(i, p) * wm_interm_6(q, i)
term(81) = term(81) + wm_interm_32(i, p) * wm_interm_6(q, i)
term(82) = term(82) + wm_interm_31(i, p) * wm_interm_7(q, i)
term(83) = term(83) + wm_interm_32(i, p) * wm_interm_7(q, i)
term(84) = term(84) + wm_interm_18(i, p) * wm_interm_44(q, i)
term(85) = term(85) + wm_interm_19(i, p) * wm_interm_44(q, i)
term(86) = term(86) + wm_interm_18(i, p) * wm_interm_48(q, i)
term(87) = term(87) + wm_interm_19(i, p) * wm_interm_48(q, i)
end do 

term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * 4.0d+0 
term(62) = term(62) * (-2.0d+0) 
term(63) = term(63) * 4.0d+0 
term(64) = term(64) * 2.0d+0 
term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * 2.0d+0 
term(68) = term(68) * 2.0d+0 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * 2.0d+0 
term(72) = term(72) * 4.0d+0 
term(73) = term(73) * (-8.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * 4.0d+0 
term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * 4.0d+0 
term(78) = term(78) * 4.0d+0 
term(79) = term(79) * (-8.0d+0) 
term(80) = term(80) * 4.0d+0 
term(81) = term(81) * (-8.0d+0) 
term(82) = term(82) * (-2.0d+0) 
term(83) = term(83) * 4.0d+0 
term(84) = term(84) * (-2.0d+0) 
term(85) = term(85) * 4.0d+0 
term(86) = term(86) * 4.0d+0 
term(87) = term(87) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(88) = term(88) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_50(a, i, j, p)
term(89) = term(89) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_50(a, i, k, p)
term(90) = term(90) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_50(a, i, p, k)
term(91) = term(91) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_42(b, i, p, k)
term(92) = term(92) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_42(b, i, k, p)
term(93) = term(93) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,k,i) * wm_interm_42(b, j, p, k)
term(94) = term(94) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_42(b, j, p, k)
term(95) = term(95) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_42(b, j, k, p)
term(96) = term(96) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_39(b, p, k, i)
term(97) = term(97) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_39(b, k, p, i)
term(98) = term(98) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_35(b, p, j, i)
term(99) = term(99) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_35(b, j, p, i)
term(100) = term(100) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_35(b, p, k, i)
term(101) = term(101) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_35(b, k, p, i)
term(102) = term(102) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_35(a, p, k, i)
term(103) = term(103) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_35(a, k, p, i)
term(104) = term(104) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_35(a, p, j, i)
term(105) = term(105) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_35(a, j, p, i)
term(106) = term(106) + r2(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_35(b, j, p, k)
term(107) = term(107) + r2(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_35(b, p, j, k)
term(108) = term(108) + r2(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_35(a, p, j, k)
term(109) = term(109) + r2(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_35(a, j, p, k)
end do 
end do 
end do 
end do 
end do 

term(88) = term(88) * 4.0d+0 
term(89) = term(89) * (-2.0d+0) 
term(90) = term(90) * 4.0d+0 
term(91) = term(91) * (-8.0d+0) 
term(92) = term(92) * 4.0d+0 
term(93) = term(93) * (-2.0d+0) 
term(94) = term(94) * 4.0d+0 
term(95) = term(95) * (-2.0d+0) 
term(96) = term(96) * 4.0d+0 
term(97) = term(97) * (-8.0d+0) 
term(98) = -term(98) 
term(99) = term(99) * 2.0d+0 
term(100) = term(100) * 2.0d+0 
term(101) = term(101) * (-4.0d+0) 
term(102) = -term(102) 
term(103) = term(103) * 2.0d+0 
term(104) = term(104) * 2.0d+0 
term(105) = term(105) * (-4.0d+0) 
term(106) = -term(106) 
term(107) = term(107) * 2.0d+0 
term(108) = -term(108) 
term(109) = term(109) * 2.0d+0 


    calc_D_ov_wm2 = zero
    do s = 0, 109
    calc_D_ov_wm2 = calc_D_ov_wm2 + term(s)
    end do

    end function calc_D_ov_wm2
    
    function calc_D_vo_wm2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, k, b 
    real(F64), dimension(0:343) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,j,p,i) * wm_interm_35(a, i, j, q)
term(1) = term(1) + r2(vrdav_Rl, a,j,p,i) * wm_interm_35(a, j, i, q)
term(2) = term(2) + r2(vrdav_Rr, a,j,p,i) * wm_interm_36(a, j, i, q)
term(3) = term(3) + r2(vrdav_Rr, a,j,p,i) * wm_interm_36(a, i, j, q)
term(4) = term(4) + r2(vrdav_Rr, a,j,p,i) * wm_interm_37(a, i, j, q)
term(5) = term(5) + r2(vrdav_Rr, a,j,p,i) * wm_interm_37(a, j, i, q)
term(6) = term(6) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_35(a, j, i, q)
term(7) = term(7) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_37(a, j, i, q)
term(8) = term(8) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_36(a, j, i, q)
term(9) = term(9) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_37(a, j, i, q)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 4.0d+0 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = term(5) * 2.0d+0 
term(6) = term(6) * 8.0d+0 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * 2.0d+0 
term(9) = term(9) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(10) = term(10) + wm_interm_24(i, j, k, q) * wm_interm_50(p, k, j, i)
end do 
end do 
end do 

term(10) = term(10) * (-2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(11) = term(11) + wm_interm_24(i, j, k, q) * wm_interm_50(p, k, i, j)
end do 
end do 
end do 

term(11) = term(11) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(12) = term(12) + wm_interm_33(q, i, j, k) * wm_interm_42(p, i, j, k)
term(13) = term(13) + wm_interm_33(i, q, j, k) * wm_interm_42(p, i, j, k)
term(14) = term(14) + wm_interm_33(i, q, j, k) * wm_interm_42(p, i, k, j)
term(15) = term(15) + wm_interm_33(q, i, j, k) * wm_interm_42(p, i, k, j)
term(16) = term(16) + wm_interm_33(i, j, k, q) * wm_interm_35(p, i, j, k)
term(17) = term(17) + wm_interm_33(i, j, k, q) * wm_interm_52(p, i, j, k)
term(18) = term(18) + wm_interm_33(i, j, q, k) * wm_interm_52(p, i, j, k)
term(19) = term(19) + wm_interm_33(i, j, q, k) * wm_interm_35(p, i, j, k)
end do 
end do 
end do 

term(12) = -term(12) 
term(13) = term(13) * 2.0d+0 
term(14) = -term(14) 
term(15) = term(15) * 2.0d+0 
term(16) = -term(16) 
term(17) = term(17) * 2.0d+0 
term(18) = -term(18) 
term(19) = term(19) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(20) = term(20) + wm_interm_15(q, i, j, k) * wm_interm_39(p, k, j, i)
term(21) = term(21) + wm_interm_15(q, i, j, k) * wm_interm_39(p, j, k, i)
term(22) = term(22) + wm_interm_15(i, q, j, k) * wm_interm_39(p, j, k, i)
term(23) = term(23) + wm_interm_15(i, q, j, k) * wm_interm_39(p, k, j, i)
end do 
end do 
end do 

term(20) = -term(20) 
term(21) = term(21) * 2.0d+0 
term(22) = -term(22) 
term(23) = term(23) * 2.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(24) = term(24) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,q) * wm_interm_56(b, k, i, j)
term(25) = term(25) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,q) * wm_interm_56(a, k, i, j)
end do 
end do 
end do 
end do 
end do 

term(24) = term(24) * 2.0d+0 
term(25) = term(25) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(26) = term(26) + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, b,k,p,i) * wm_interm_56(a, k, j, q)
term(27) = term(27) + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,p,i) * wm_interm_56(b, k, j, q)
end do 
end do 
end do 
end do 
end do 

term(26) = term(26) * 2.0d+0 
term(27) = -term(27) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(28) = term(28) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_47(b, j)
term(29) = term(29) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,j,i) * wm_interm_47(b, j)
term(30) = term(30) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_46(b, j)
term(31) = term(31) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,j,i) * wm_interm_46(b, j)
term(32) = term(32) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_4(b, j)
term(33) = term(33) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_5(b, j)
term(34) = term(34) + r2(vrdav_Rl, a,i,p,q) * t2(a,b,j,i) * wm_interm_40(b, j)
term(35) = term(35) + r2(vrdav_Rl, a,i,p,q) * t2(a,b,j,i) * wm_interm_41(b, j)
term(36) = term(36) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_9(b, j)
term(37) = term(37) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_9(b, j)
term(38) = term(38) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_8(b, j)
term(39) = term(39) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_40(b, j)
term(40) = term(40) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_8(b, j)
term(41) = term(41) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_41(b, j)
term(42) = term(42) + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_44(b, j)
term(43) = term(43) + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_48(b, j)
term(44) = term(44) + r2(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_2(b, j)
term(45) = term(45) + r2(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_3(b, j)
end do 
end do 
end do 
end do 

term(28) = term(28) * (-2.0d+0) 
term(29) = term(29) * 4.0d+0 
term(30) = term(30) * 4.0d+0 
term(31) = term(31) * (-8.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * 4.0d+0 
term(34) = term(34) * 4.0d+0 
term(35) = term(35) * (-8.0d+0) 
term(36) = term(36) * 4.0d+0 
term(37) = term(37) * (-8.0d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * 4.0d+0 
term(41) = term(41) * 4.0d+0 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * 4.0d+0 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(46) = term(46) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,i,j) * wm_interm_47(b, j)
term(47) = term(47) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,i,j) * wm_interm_46(b, j)
term(48) = term(48) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_4(b, j)
term(49) = term(49) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_5(b, j)
term(50) = term(50) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_9(b, j)
term(51) = term(51) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,i,j) * wm_interm_41(b, j)
term(52) = term(52) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_8(b, j)
term(53) = term(53) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,i,j) * wm_interm_40(b, j)
term(54) = term(54) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_44(b, j)
term(55) = term(55) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_48(b, j)
term(56) = term(56) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_6(a, j)
term(57) = term(57) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_7(a, j)
term(58) = term(58) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_2(b, j)
term(59) = term(59) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_3(b, j)
end do 
end do 
end do 
end do 

term(46) = term(46) * 4.0d+0 
term(47) = term(47) * (-8.0d+0) 
term(48) = term(48) * 4.0d+0 
term(49) = term(49) * (-8.0d+0) 
term(50) = term(50) * (-8.0d+0) 
term(51) = term(51) * (-8.0d+0) 
term(52) = term(52) * 4.0d+0 
term(53) = term(53) * 4.0d+0 
term(54) = term(54) * 4.0d+0 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * 4.0d+0 
term(57) = term(57) * (-2.0d+0) 
term(58) = term(58) * 8.0d+0 
term(59) = term(59) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(60) = term(60) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,i,j) * wm_interm_47(b, j)
term(61) = term(61) + r2(vrdav_Rr, a,i,p,q) * s2(a,b,i,j) * wm_interm_46(b, j)
term(62) = term(62) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_4(a, i)
term(63) = term(63) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_5(a, i)
term(64) = term(64) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_4(a, j)
term(65) = term(65) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_5(a, j)
term(66) = term(66) + r2(vrdav_Rl, a,i,p,q) * t2(a,b,i,j) * wm_interm_41(b, j)
term(67) = term(67) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_9(b, j)
term(68) = term(68) + r2(vrdav_Rl, a,i,p,q) * t2(a,b,i,j) * wm_interm_40(b, j)
term(69) = term(69) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_8(b, j)
term(70) = term(70) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,p,q) * wm_interm_44(b, j)
term(71) = term(71) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,p,q) * wm_interm_48(b, j)
term(72) = term(72) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,p,q) * wm_interm_44(a, i)
term(73) = term(73) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,p,q) * wm_interm_44(b, i)
term(74) = term(74) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,p,q) * wm_interm_48(b, i)
term(75) = term(75) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,p,q) * wm_interm_44(a, j)
term(76) = term(76) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,p,q) * wm_interm_48(a, j)
term(77) = term(77) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,p,q) * wm_interm_48(a, i)
term(78) = term(78) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_6(a, j)
term(79) = term(79) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_6(a, i)
term(80) = term(80) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_7(a, j)
term(81) = term(81) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_7(a, i)
term(82) = term(82) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_6(a, i)
term(83) = term(83) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_7(a, i)
term(84) = term(84) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_2(b, i)
term(85) = term(85) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,i,q) * wm_interm_2(b, j)
term(86) = term(86) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_2(a, j)
term(87) = term(87) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_2(a, i)
term(88) = term(88) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_3(b, i)
term(89) = term(89) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,i,q) * wm_interm_3(b, j)
term(90) = term(90) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_3(a, j)
term(91) = term(91) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_3(a, i)
end do 
end do 
end do 
end do 

term(60) = term(60) * (-8.0d+0) 
term(61) = term(61) * 16.0d+0 
term(62) = term(62) * (-8.0d+0) 
term(63) = term(63) * 16.0d+0 
term(64) = term(64) * 4.0d+0 
term(65) = term(65) * (-8.0d+0) 
term(66) = term(66) * 16.0d+0 
term(67) = term(67) * 16.0d+0 
term(68) = term(68) * (-8.0d+0) 
term(69) = term(69) * (-8.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * 8.0d+0 
term(72) = term(72) * (-4.0d+0) 
term(73) = term(73) * 2.0d+0 
term(74) = term(74) * (-4.0d+0) 
term(75) = term(75) * 2.0d+0 
term(76) = term(76) * (-4.0d+0) 
term(77) = term(77) * 8.0d+0 
term(78) = term(78) * (-8.0d+0) 
term(79) = term(79) * 16.0d+0 
term(80) = term(80) * 4.0d+0 
term(81) = term(81) * (-8.0d+0) 
term(82) = term(82) * (-8.0d+0) 
term(83) = term(83) * 4.0d+0 
term(84) = term(84) * 4.0d+0 
term(85) = term(85) * (-8.0d+0) 
term(86) = term(86) * 4.0d+0 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(89) = term(89) * 4.0d+0 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * 4.0d+0 

term(92) = term(92) + s1(p,q) * wm_interm_11
term(93) = term(93) + t1(p,q) * wm_interm_11
term(94) = term(94) + s1(p,q) * wm_interm_0
term(95) = term(95) + s1(p,q) * wm_interm_1
term(96) = term(96) + t1(p,q) * wm_interm_0
term(97) = term(97) + t1(p,q) * wm_interm_1
term(98) = term(98) + wm_interm_11 * wm_interm_44(p, q)
term(99) = term(99) + wm_interm_11 * wm_interm_48(p, q)
term(100) = term(100) + wm_interm_0 * wm_interm_44(p, q)
term(101) = term(101) + wm_interm_1 * wm_interm_44(p, q)
term(102) = term(102) + wm_interm_0 * wm_interm_48(p, q)
term(103) = term(103) + wm_interm_1 * wm_interm_48(p, q)

term(92) = term(92) * (-4.0d+0) 
term(93) = term(93) * (-4.0d+0) 
term(94) = term(94) * 2.0d+0 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * 2.0d+0 
term(97) = term(97) * (-4.0d+0) 
term(98) = term(98) * (-4.0d+0) 
term(99) = term(99) * 8.0d+0 
term(100) = term(100) * 2.0d+0 
term(101) = term(101) * (-4.0d+0) 
term(102) = term(102) * (-4.0d+0) 
term(103) = term(103) * 8.0d+0 

do i = 1, nocc 
term(104) = term(104) + r1(vrdav_Rl, p,i) * wm_interm_12(i, q)
term(105) = term(105) + r1(vrdav_Rr, p,i) * wm_interm_13(i, q)
term(106) = term(106) + r1(vrdav_Rl, p,i) * wm_interm_31(i, q)
term(107) = term(107) + r1(vrdav_Rl, p,i) * wm_interm_32(i, q)
term(108) = term(108) + r1(vrdav_Rr, p,i) * wm_interm_29(i, q)
term(109) = term(109) + r1(vrdav_Rr, p,i) * wm_interm_34(i, q)
term(110) = term(110) + wm_interm_13(q, i) * wm_interm_40(p, i)
term(111) = term(111) + wm_interm_13(q, i) * wm_interm_41(p, i)
term(112) = term(112) + wm_interm_13(i, q) * wm_interm_2(p, i)
term(113) = term(113) + wm_interm_13(i, q) * wm_interm_3(p, i)
term(114) = term(114) + wm_interm_4(p, i) * wm_interm_51(i, q)
term(115) = term(115) + wm_interm_5(p, i) * wm_interm_51(i, q)
term(116) = term(116) + wm_interm_16(i, q) * wm_interm_4(p, i)
term(117) = term(117) + wm_interm_16(i, q) * wm_interm_5(p, i)
term(118) = term(118) + wm_interm_17(i, q) * wm_interm_4(p, i)
term(119) = term(119) + wm_interm_17(i, q) * wm_interm_5(p, i)
term(120) = term(120) + wm_interm_29(q, i) * wm_interm_40(p, i)
term(121) = term(121) + wm_interm_29(q, i) * wm_interm_41(p, i)
term(122) = term(122) + wm_interm_34(q, i) * wm_interm_40(p, i)
term(123) = term(123) + wm_interm_34(q, i) * wm_interm_41(p, i)
term(124) = term(124) + wm_interm_2(p, i) * wm_interm_29(i, q)
term(125) = term(125) + wm_interm_2(p, i) * wm_interm_34(i, q)
term(126) = term(126) + wm_interm_29(i, q) * wm_interm_3(p, i)
term(127) = term(127) + wm_interm_3(p, i) * wm_interm_34(i, q)
end do 

term(104) = term(104) * 2.0d+0 
term(105) = term(105) * 2.0d+0 
term(106) = term(106) * (-2.0d+0) 
term(107) = term(107) * 4.0d+0 
term(108) = term(108) * (-2.0d+0) 
term(109) = term(109) * 4.0d+0 
term(110) = term(110) * 2.0d+0 
term(111) = term(111) * (-4.0d+0) 
term(112) = term(112) * 4.0d+0 
term(113) = term(113) * (-2.0d+0) 
term(114) = term(114) * 2.0d+0 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (-2.0d+0) 
term(117) = term(117) * 4.0d+0 
term(118) = term(118) * 4.0d+0 
term(119) = term(119) * (-8.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * 4.0d+0 
term(122) = term(122) * 4.0d+0 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (-4.0d+0) 
term(125) = term(125) * 8.0d+0 
term(126) = term(126) * 2.0d+0 
term(127) = term(127) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(128) = term(128) + wm_interm_44(a, q) * wm_interm_45(a, p)
term(129) = term(129) + wm_interm_10(p, a) * wm_interm_46(a, q)
term(130) = term(130) + wm_interm_10(p, a) * wm_interm_47(a, q)
term(131) = term(131) + wm_interm_45(a, p) * wm_interm_48(a, q)
term(132) = term(132) + wm_interm_53(p, a) * wm_interm_8(a, q)
term(133) = term(133) + wm_interm_53(p, a) * wm_interm_9(a, q)
term(134) = term(134) + wm_interm_10(a, p) * wm_interm_6(a, q)
term(135) = term(135) + wm_interm_10(a, p) * wm_interm_7(a, q)
term(136) = term(136) + wm_interm_27(p, a) * wm_interm_46(a, q)
term(137) = term(137) + wm_interm_28(p, a) * wm_interm_46(a, q)
term(138) = term(138) + wm_interm_27(p, a) * wm_interm_47(a, q)
term(139) = term(139) + wm_interm_28(p, a) * wm_interm_47(a, q)
term(140) = term(140) + wm_interm_20(p, a) * wm_interm_8(a, q)
term(141) = term(141) + wm_interm_21(p, a) * wm_interm_8(a, q)
term(142) = term(142) + wm_interm_20(p, a) * wm_interm_9(a, q)
term(143) = term(143) + wm_interm_21(p, a) * wm_interm_9(a, q)
term(144) = term(144) + wm_interm_27(a, p) * wm_interm_6(a, q)
term(145) = term(145) + wm_interm_28(a, p) * wm_interm_6(a, q)
term(146) = term(146) + wm_interm_27(a, p) * wm_interm_7(a, q)
term(147) = term(147) + wm_interm_28(a, p) * wm_interm_7(a, q)
term(148) = term(148) + wm_interm_22(a, p) * wm_interm_44(a, q)
term(149) = term(149) + wm_interm_23(a, p) * wm_interm_44(a, q)
term(150) = term(150) + wm_interm_22(a, p) * wm_interm_48(a, q)
term(151) = term(151) + wm_interm_23(a, p) * wm_interm_48(a, q)
end do 

term(128) = term(128) * 2.0d+0 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * 2.0d+0 
term(131) = term(131) * (-4.0d+0) 
term(132) = term(132) * 2.0d+0 
term(133) = term(133) * (-4.0d+0) 
term(134) = term(134) * (-4.0d+0) 
term(135) = term(135) * 2.0d+0 
term(136) = term(136) * 4.0d+0 
term(137) = term(137) * (-8.0d+0) 
term(138) = term(138) * (-2.0d+0) 
term(139) = term(139) * 4.0d+0 
term(140) = term(140) * 4.0d+0 
term(141) = term(141) * (-2.0d+0) 
term(142) = term(142) * (-8.0d+0) 
term(143) = term(143) * 4.0d+0 
term(144) = term(144) * 4.0d+0 
term(145) = term(145) * (-8.0d+0) 
term(146) = term(146) * (-2.0d+0) 
term(147) = term(147) * 4.0d+0 
term(148) = term(148) * (-2.0d+0) 
term(149) = term(149) * 4.0d+0 
term(150) = term(150) * 4.0d+0 
term(151) = term(151) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(152) = term(152) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_35(a, i, j, q)
term(153) = term(153) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_35(a, i, j, q)
term(154) = term(154) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_37(a, i, j, q)
term(155) = term(155) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_36(a, i, j, q)
term(156) = term(156) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_37(a, i, j, q)
end do 
end do 
end do 

term(152) = term(152) * (-2.0d+0) 
term(153) = term(153) * (-4.0d+0) 
term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * 2.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(157) = term(157) + r1(vrdav_Rl, b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_20(a, b)
term(158) = term(158) + r1(vrdav_Rl, b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_21(a, b)
term(159) = term(159) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_25(a, b)
term(160) = term(160) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_26(a, b)
end do 
end do 
end do 

term(157) = term(157) * 4.0d+0 
term(158) = term(158) * (-2.0d+0) 
term(159) = term(159) * (-2.0d+0) 
term(160) = term(160) * 4.0d+0 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(161) = term(161) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,i,p,q) * wm_interm_20(b, a)
term(162) = term(162) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,i,p,q) * wm_interm_21(b, a)
term(163) = term(163) + r2(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_20(b, a)
term(164) = term(164) + r2(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_21(b, a)
end do 
end do 
end do 

term(161) = term(161) * (-8.0d+0) 
term(162) = term(162) * 4.0d+0 
term(163) = term(163) * (-8.0d+0) 
term(164) = term(164) * 4.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(165) = term(165) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_27(b, a)
term(166) = term(166) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_28(b, a)
term(167) = term(167) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_20(b, a)
term(168) = term(168) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_21(b, a)
end do 
end do 
end do 

term(165) = term(165) * (-2.0d+0) 
term(166) = term(166) * 4.0d+0 
term(167) = term(167) * 4.0d+0 
term(168) = term(168) * (-2.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(169) = term(169) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_27(a, b)
term(170) = term(170) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_28(a, b)
term(171) = term(171) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_25(a, b)
term(172) = term(172) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_26(a, b)
end do 
end do 
end do 

term(169) = term(169) * 4.0d+0 
term(170) = term(170) * (-8.0d+0) 
term(171) = term(171) * 4.0d+0 
term(172) = term(172) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(173) = term(173) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_49(b, i, k, q)
term(174) = term(174) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_55(b, q, k, j)
term(175) = term(175) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_55(b, q, k, i)
term(176) = term(176) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_54(b, k, q, j)
term(177) = term(177) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_55(b, k, q, j)
term(178) = term(178) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_54(b, k, q, i)
term(179) = term(179) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_55(b, k, q, i)
term(180) = term(180) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_54(b, q, k, j)
term(181) = term(181) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_54(b, q, k, i)
term(182) = term(182) + r2(vrdav_Rl, a,k,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_56(b, i, k, j)
term(183) = term(183) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,q) * wm_interm_56(a, k, j, i)
term(184) = term(184) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,q) * wm_interm_56(b, k, j, i)
term(185) = term(185) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_56(a, k, j, q)
term(186) = term(186) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_56(b, k, j, q)
term(187) = term(187) + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, b,k,p,i) * wm_interm_56(a, k, q, j)
term(188) = term(188) + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,p,i) * wm_interm_56(b, k, q, j)
term(189) = term(189) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_56(a, k, q, j)
term(190) = term(190) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_56(b, k, q, j)
term(191) = term(191) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_35(b, k, j, i)
term(192) = term(192) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_35(b, j, k, i)
end do 
end do 
end do 
end do 
end do 

term(173) = term(173) * (-2.0d+0) 
term(174) = -term(174) 
term(175) = term(175) * 2.0d+0 
term(176) = -term(176) 
term(177) = term(177) * 2.0d+0 
term(178) = term(178) * 2.0d+0 
term(179) = -term(179) 
term(180) = term(180) * 2.0d+0 
term(181) = -term(181) 
term(182) = -term(182) 
term(183) = term(183) * 2.0d+0 
term(184) = term(184) * (-4.0d+0) 
term(185) = -term(185) 
term(186) = term(186) * 2.0d+0 
term(187) = -term(187) 
term(188) = term(188) * 2.0d+0 
term(189) = term(189) * 2.0d+0 
term(190) = -term(190) 
term(191) = -term(191) 
term(192) = term(192) * 2.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(193) = term(193) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,k,i) * wm_interm_49(b, j, k, q)
end do 
end do 
end do 
end do 
end do 

term(193) = term(193) * 4.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(194) = term(194) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_49(b, j, k, q)
term(195) = term(195) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_49(b, i, k, q)
term(196) = term(196) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_49(b, j, q, k)
term(197) = term(197) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_49(b, i, q, k)
term(198) = term(198) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,k,i) * wm_interm_49(b, j, q, k)
term(199) = term(199) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_55(b, q, k, i)
term(200) = term(200) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_55(b, k, q, i)
term(201) = term(201) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_54(b, q, k, i)
term(202) = term(202) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_54(b, k, q, i)
term(203) = term(203) + r2(vrdav_Rl, a,i,p,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_39(b, j, k, i)
term(204) = term(204) + r2(vrdav_Rl, a,i,p,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_39(b, k, j, i)
term(205) = term(205) + r2(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_39(b, j, k, i)
term(206) = term(206) + r2(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_39(b, k, j, i)
term(207) = term(207) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_56(b, i, j, k)
term(208) = term(208) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,j,p,i) * wm_interm_56(a, i, k, q)
term(209) = term(209) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,k,p,i) * wm_interm_56(a, i, j, q)
term(210) = term(210) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,k,p,i) * wm_interm_56(b, i, j, q)
term(211) = term(211) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,j,p,i) * wm_interm_56(b, i, k, q)
term(212) = term(212) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,j,p,i) * wm_interm_56(a, i, q, k)
term(213) = term(213) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,j,p,i) * wm_interm_56(b, i, q, k)
term(214) = term(214) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_35(b, j, k, i)
term(215) = term(215) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_35(b, k, j, i)
end do 
end do 
end do 
end do 
end do 

term(194) = term(194) * (-2.0d+0) 
term(195) = term(195) * 4.0d+0 
term(196) = term(196) * 4.0d+0 
term(197) = term(197) * (-8.0d+0) 
term(198) = term(198) * (-2.0d+0) 
term(199) = term(199) * (-4.0d+0) 
term(200) = term(200) * 2.0d+0 
term(201) = term(201) * 2.0d+0 
term(202) = term(202) * (-4.0d+0) 
term(203) = term(203) * 4.0d+0 
term(204) = term(204) * (-8.0d+0) 
term(205) = term(205) * (-2.0d+0) 
term(206) = term(206) * 4.0d+0 
term(207) = -term(207) 
term(208) = term(208) * 2.0d+0 
term(209) = term(209) * (-4.0d+0) 
term(210) = term(210) * 2.0d+0 
term(211) = term(211) * (-4.0d+0) 
term(212) = -term(212) 
term(213) = term(213) * 2.0d+0 
term(214) = -term(214) 
term(215) = term(215) * 2.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(216) = term(216) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_49(b, i, q, k)
term(217) = term(217) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_55(b, q, k, j)
term(218) = term(218) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_55(b, k, q, j)
term(219) = term(219) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_54(b, q, k, j)
term(220) = term(220) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_54(b, k, q, j)
term(221) = term(221) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_37(a, q, j, k)
term(222) = term(222) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_37(a, j, q, k)
term(223) = term(223) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_36(a, q, j, k)
term(224) = term(224) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_36(a, j, q, k)
term(225) = term(225) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_56(b, i, k, j)
term(226) = term(226) + r2(vrdav_Rl, a,k,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_56(b, i, j, k)
term(227) = term(227) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,k,p,i) * wm_interm_56(a, i, q, j)
term(228) = term(228) + r2(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,k,p,i) * wm_interm_56(b, i, q, j)
end do 
end do 
end do 
end do 
end do 

term(216) = term(216) * 4.0d+0 
term(217) = term(217) * 2.0d+0 
term(218) = -term(218) 
term(219) = -term(219) 
term(220) = term(220) * 2.0d+0 
term(221) = term(221) * 2.0d+0 
term(222) = -term(222) 
term(223) = -term(223) 
term(224) = term(224) * 2.0d+0 
term(225) = term(225) * 2.0d+0 
term(226) = term(226) * 2.0d+0 
term(227) = term(227) * 2.0d+0 
term(228) = -term(228) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(229) = term(229) + s2(a,p,i,q) * wm_interm_4(a, i)
term(230) = term(230) + s2(a,p,i,q) * wm_interm_5(a, i)
term(231) = term(231) + s2(a,p,q,i) * wm_interm_4(a, i)
term(232) = term(232) + s2(a,p,q,i) * wm_interm_5(a, i)
term(233) = term(233) + t2(a,p,q,i) * wm_interm_8(a, i)
term(234) = term(234) + t2(a,p,q,i) * wm_interm_9(a, i)
term(235) = term(235) + t2(a,p,i,q) * wm_interm_8(a, i)
term(236) = term(236) + t2(a,p,i,q) * wm_interm_9(a, i)
term(237) = term(237) + r2(vrdav_Rl, a,q,p,i) * wm_interm_2(a, i)
term(238) = term(238) + r2(vrdav_Rl, a,i,p,q) * wm_interm_2(a, i)
term(239) = term(239) + r2(vrdav_Rl, a,q,p,i) * wm_interm_3(a, i)
term(240) = term(240) + r2(vrdav_Rl, a,i,p,q) * wm_interm_3(a, i)
term(241) = term(241) + r2(vrdav_Rr, a,q,p,i) * wm_interm_6(a, i)
term(242) = term(242) + r2(vrdav_Rr, a,i,p,q) * wm_interm_6(a, i)
term(243) = term(243) + r2(vrdav_Rr, a,q,p,i) * wm_interm_7(a, i)
term(244) = term(244) + r2(vrdav_Rr, a,i,p,q) * wm_interm_7(a, i)
term(245) = term(245) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,q,p,i)
term(246) = term(246) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,p,q)
term(247) = term(247) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
term(248) = term(248) + r2(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, a,i)
term(249) = term(249) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_2(a, i)
term(250) = term(250) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_3(a, i)
term(251) = term(251) + r1(vrdav_Rl, p,i) * s1(a,q) * wm_interm_2(a, i)
term(252) = term(252) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_2(a, i)
term(253) = term(253) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_3(a, i)
term(254) = term(254) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_6(a, i)
term(255) = term(255) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_7(a, i)
term(256) = term(256) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_6(a, i)
term(257) = term(257) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_7(a, i)
term(258) = term(258) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_6(a, i)
term(259) = term(259) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_7(a, i)
end do 
end do 

term(229) = term(229) * 4.0d+0 
term(230) = term(230) * (-8.0d+0) 
term(231) = term(231) * (-2.0d+0) 
term(232) = term(232) * 4.0d+0 
term(233) = term(233) * (-2.0d+0) 
term(234) = term(234) * 4.0d+0 
term(235) = term(235) * 4.0d+0 
term(236) = term(236) * (-8.0d+0) 
term(237) = term(237) * (-4.0d+0) 
term(238) = term(238) * 8.0d+0 
term(239) = term(239) * 2.0d+0 
term(240) = term(240) * (-4.0d+0) 
term(241) = term(241) * 4.0d+0 
term(242) = term(242) * (-8.0d+0) 
term(243) = term(243) * (-2.0d+0) 
term(244) = term(244) * 4.0d+0 
term(245) = term(245) * (-2.0d+0) 
term(246) = term(246) * 4.0d+0 
term(247) = term(247) * (-2.0d+0) 
term(248) = term(248) * 4.0d+0 
term(249) = term(249) * (-8.0d+0) 
term(250) = term(250) * 4.0d+0 
term(251) = term(251) * 4.0d+0 
term(252) = term(252) * (-8.0d+0) 
term(253) = term(253) * 4.0d+0 
term(254) = term(254) * 8.0d+0 
term(255) = term(255) * (-4.0d+0) 
term(256) = term(256) * (-2.0d+0) 
term(258) = term(258) * 8.0d+0 
term(259) = term(259) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(260) = term(260) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_37(a, q, i, k)
term(261) = term(261) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_37(a, q, j, k)
term(262) = term(262) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_37(a, q, i, k)
term(263) = term(263) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_37(a, i, q, k)
term(264) = term(264) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_37(a, j, i, k)
term(265) = term(265) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_37(a, i, j, k)
term(266) = term(266) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_37(a, j, i, k)
term(267) = term(267) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_37(a, i, j, k)
term(268) = term(268) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_37(a, j, q, k)
term(269) = term(269) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_37(a, i, q, k)
term(270) = term(270) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_36(a, q, i, k)
term(271) = term(271) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_36(a, i, q, k)
term(272) = term(272) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_36(a, i, j, k)
term(273) = term(273) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_36(a, j, i, k)
term(274) = term(274) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_36(a, i, j, k)
term(275) = term(275) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_36(a, j, i, k)
term(276) = term(276) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_36(a, j, q, k)
term(277) = term(277) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_36(a, i, q, k)
term(278) = term(278) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_36(a, q, j, k)
term(279) = term(279) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_36(a, q, i, k)
term(280) = term(280) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_35(b, i, j, k)
term(281) = term(281) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_35(b, j, i, k)
term(282) = term(282) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_35(a, j, i, k)
term(283) = term(283) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_35(a, i, j, k)
end do 
end do 
end do 
end do 
end do 

term(260) = -term(260) 
term(261) = -term(261) 
term(262) = term(262) * 2.0d+0 
term(263) = term(263) * 2.0d+0 
term(264) = term(264) * 2.0d+0 
term(265) = term(265) * (-4.0d+0) 
term(266) = -term(266) 
term(267) = term(267) * 2.0d+0 
term(268) = term(268) * 2.0d+0 
term(269) = term(269) * (-4.0d+0) 
term(270) = term(270) * 2.0d+0 
term(271) = -term(271) 
term(272) = term(272) * 2.0d+0 
term(273) = term(273) * (-4.0d+0) 
term(274) = -term(274) 
term(275) = term(275) * 2.0d+0 
term(276) = -term(276) 
term(277) = term(277) * 2.0d+0 
term(278) = term(278) * 2.0d+0 
term(279) = term(279) * (-4.0d+0) 
term(280) = term(280) * 2.0d+0 
term(281) = term(281) * (-4.0d+0) 
term(282) = term(282) * 2.0d+0 
term(283) = term(283) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(284) = term(284) + wm_interm_38(i, j) * wm_interm_39(p, j, q, i)
term(285) = term(285) + wm_interm_38(i, j) * wm_interm_39(p, q, j, i)
term(286) = term(286) + wm_interm_38(i, j) * wm_interm_43(p, j, q, i)
term(287) = term(287) + wm_interm_38(i, j) * wm_interm_43(p, j, i, q)
term(288) = term(288) + wm_interm_51(i, j) * wm_interm_54(p, q, j, i)
term(289) = term(289) + wm_interm_51(i, j) * wm_interm_55(p, q, j, i)
term(290) = term(290) + wm_interm_51(i, j) * wm_interm_54(p, j, q, i)
term(291) = term(291) + wm_interm_51(i, j) * wm_interm_55(p, j, q, i)
term(292) = term(292) + wm_interm_16(i, j) * wm_interm_50(p, j, q, i)
term(293) = term(293) + wm_interm_16(i, j) * wm_interm_50(p, j, i, q)
term(294) = term(294) + wm_interm_17(i, j) * wm_interm_50(p, j, q, i)
term(295) = term(295) + wm_interm_17(i, j) * wm_interm_50(p, j, i, q)
term(296) = term(296) + wm_interm_16(i, j) * wm_interm_54(p, q, j, i)
term(297) = term(297) + wm_interm_17(i, j) * wm_interm_54(p, q, j, i)
term(298) = term(298) + wm_interm_16(i, j) * wm_interm_55(p, q, j, i)
term(299) = term(299) + wm_interm_17(i, j) * wm_interm_55(p, q, j, i)
term(300) = term(300) + wm_interm_16(i, j) * wm_interm_54(p, j, q, i)
term(301) = term(301) + wm_interm_16(i, j) * wm_interm_55(p, j, q, i)
term(302) = term(302) + wm_interm_17(i, j) * wm_interm_54(p, j, q, i)
term(303) = term(303) + wm_interm_17(i, j) * wm_interm_55(p, j, q, i)
term(304) = term(304) + wm_interm_18(i, j) * wm_interm_39(p, j, q, i)
term(305) = term(305) + wm_interm_19(i, j) * wm_interm_39(p, j, q, i)
term(306) = term(306) + wm_interm_18(i, j) * wm_interm_39(p, q, j, i)
term(307) = term(307) + wm_interm_19(i, j) * wm_interm_39(p, q, j, i)
term(308) = term(308) + wm_interm_18(i, j) * wm_interm_43(p, j, q, i)
term(309) = term(309) + wm_interm_19(i, j) * wm_interm_43(p, j, q, i)
term(310) = term(310) + wm_interm_18(i, j) * wm_interm_43(p, j, i, q)
term(311) = term(311) + wm_interm_19(i, j) * wm_interm_43(p, j, i, q)
end do 
end do 

term(284) = term(284) * 2.0d+0 
term(285) = term(285) * (-4.0d+0) 
term(286) = term(286) * 2.0d+0 
term(287) = term(287) * (-4.0d+0) 
term(288) = term(288) * (-2.0d+0) 
term(291) = term(291) * (-2.0d+0) 
term(292) = term(292) * (-2.0d+0) 
term(293) = term(293) * 4.0d+0 
term(294) = term(294) * 4.0d+0 
term(295) = term(295) * (-8.0d+0) 
term(296) = term(296) * 2.0d+0 
term(297) = term(297) * (-4.0d+0) 
term(298) = -term(298) 
term(299) = term(299) * 2.0d+0 
term(300) = -term(300) 
term(301) = term(301) * 2.0d+0 
term(302) = term(302) * 2.0d+0 
term(303) = term(303) * (-4.0d+0) 
term(304) = term(304) * (-2.0d+0) 
term(305) = term(305) * 4.0d+0 
term(306) = term(306) * 4.0d+0 
term(307) = term(307) * (-8.0d+0) 
term(308) = term(308) * (-2.0d+0) 
term(309) = term(309) * 4.0d+0 
term(310) = term(310) * 4.0d+0 
term(311) = term(311) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(312) = term(312) + wm_interm_13(i, j) * wm_interm_42(p, i, j, q)
term(313) = term(313) + wm_interm_13(i, j) * wm_interm_42(p, i, q, j)
term(314) = term(314) + wm_interm_12(i, j) * wm_interm_49(p, i, q, j)
term(315) = term(315) + wm_interm_12(i, j) * wm_interm_49(p, i, j, q)
term(316) = term(316) + wm_interm_50(p, i, q, j) * wm_interm_51(j, i)
term(317) = term(317) + wm_interm_50(p, i, j, q) * wm_interm_51(j, i)
term(318) = term(318) + wm_interm_13(i, j) * wm_interm_35(p, i, q, j)
term(319) = term(319) + wm_interm_13(i, j) * wm_interm_52(p, i, q, j)
term(320) = term(320) + wm_interm_12(i, j) * wm_interm_37(p, i, q, j)
term(321) = term(321) + wm_interm_12(i, j) * wm_interm_37(p, q, i, j)
term(322) = term(322) + wm_interm_12(i, j) * wm_interm_36(p, q, i, j)
term(323) = term(323) + wm_interm_12(i, j) * wm_interm_36(p, i, q, j)
term(324) = term(324) + wm_interm_31(i, j) * wm_interm_49(p, i, q, j)
term(325) = term(325) + wm_interm_32(i, j) * wm_interm_49(p, i, q, j)
term(326) = term(326) + wm_interm_31(i, j) * wm_interm_49(p, i, j, q)
term(327) = term(327) + wm_interm_32(i, j) * wm_interm_49(p, i, j, q)
term(328) = term(328) + wm_interm_29(i, j) * wm_interm_42(p, i, j, q)
term(329) = term(329) + wm_interm_29(i, j) * wm_interm_42(p, i, q, j)
term(330) = term(330) + wm_interm_34(i, j) * wm_interm_42(p, i, j, q)
term(331) = term(331) + wm_interm_34(i, j) * wm_interm_42(p, i, q, j)
term(332) = term(332) + wm_interm_31(i, j) * wm_interm_37(p, i, q, j)
term(333) = term(333) + wm_interm_32(i, j) * wm_interm_37(p, i, q, j)
term(334) = term(334) + wm_interm_31(i, j) * wm_interm_37(p, q, i, j)
term(335) = term(335) + wm_interm_32(i, j) * wm_interm_37(p, q, i, j)
term(336) = term(336) + wm_interm_31(i, j) * wm_interm_36(p, q, i, j)
term(337) = term(337) + wm_interm_32(i, j) * wm_interm_36(p, q, i, j)
term(338) = term(338) + wm_interm_31(i, j) * wm_interm_36(p, i, q, j)
term(339) = term(339) + wm_interm_32(i, j) * wm_interm_36(p, i, q, j)
term(340) = term(340) + wm_interm_29(i, j) * wm_interm_35(p, i, q, j)
term(341) = term(341) + wm_interm_29(i, j) * wm_interm_52(p, i, q, j)
term(342) = term(342) + wm_interm_34(i, j) * wm_interm_35(p, i, q, j)
term(343) = term(343) + wm_interm_34(i, j) * wm_interm_52(p, i, q, j)
end do 
end do 

term(312) = term(312) * (-4.0d+0) 
term(313) = term(313) * 2.0d+0 
term(314) = term(314) * 2.0d+0 
term(315) = term(315) * (-4.0d+0) 
term(316) = term(316) * 2.0d+0 
term(317) = term(317) * (-4.0d+0) 
term(318) = term(318) * 2.0d+0 
term(319) = term(319) * (-4.0d+0) 
term(321) = term(321) * (-2.0d+0) 
term(323) = term(323) * (-2.0d+0) 
term(324) = term(324) * (-2.0d+0) 
term(325) = term(325) * 4.0d+0 
term(326) = term(326) * 4.0d+0 
term(327) = term(327) * (-8.0d+0) 
term(328) = term(328) * 4.0d+0 
term(329) = term(329) * (-2.0d+0) 
term(330) = term(330) * (-8.0d+0) 
term(331) = term(331) * 4.0d+0 
term(332) = -term(332) 
term(333) = term(333) * 2.0d+0 
term(334) = term(334) * 2.0d+0 
term(335) = term(335) * (-4.0d+0) 
term(336) = -term(336) 
term(337) = term(337) * 2.0d+0 
term(338) = term(338) * 2.0d+0 
term(339) = term(339) * (-4.0d+0) 
term(340) = term(340) * (-2.0d+0) 
term(341) = term(341) * 4.0d+0 
term(342) = term(342) * 4.0d+0 
term(343) = term(343) * (-8.0d+0) 


    calc_D_vo_wm2 = zero
    do s = 0, 343
    calc_D_vo_wm2 = calc_D_vo_wm2 + term(s)
    end do

    end function calc_D_vo_wm2
    
    function calc_D_vv_wm2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b, l 
    real(F64), dimension(0:259) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_42(p, i, j, k) * wm_interm_49(q, i, j, k)
term(1) = term(1) + wm_interm_42(p, i, j, k) * wm_interm_49(q, i, k, j)
term(2) = term(2) + wm_interm_39(q, i, j, k) * wm_interm_50(p, k, i, j)
term(3) = term(3) + wm_interm_35(p, i, j, k) * wm_interm_37(q, j, i, k)
term(4) = term(4) + wm_interm_35(p, i, j, k) * wm_interm_37(q, i, j, k)
term(5) = term(5) + wm_interm_35(p, i, j, k) * wm_interm_36(q, i, j, k)
term(6) = term(6) + wm_interm_35(p, i, j, k) * wm_interm_36(q, j, i, k)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * 2.0d+0 
term(4) = term(4) * (-2.0d+0) 
term(6) = term(6) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(7) = term(7) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,i,k) * wm_interm_29(j, k)
term(8) = term(8) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,j,k) * wm_interm_29(i, k)
term(9) = term(9) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,i,k) * wm_interm_34(j, k)
term(10) = term(10) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,j,k) * wm_interm_34(i, k)
term(11) = term(11) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,i,k) * wm_interm_13(j, k)
term(12) = term(12) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,j,k) * wm_interm_13(i, k)
end do 
end do 
end do 
end do 

term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * 4.0d+0 
term(9) = term(9) * 4.0d+0 
term(10) = term(10) * (-8.0d+0) 
term(11) = term(11) * 2.0d+0 
term(12) = term(12) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(13) = term(13) + wm_interm_20(q, a) * wm_interm_22(a, p)
term(14) = term(14) + wm_interm_21(q, a) * wm_interm_22(a, p)
term(15) = term(15) + wm_interm_20(q, a) * wm_interm_23(a, p)
term(16) = term(16) + wm_interm_21(q, a) * wm_interm_23(a, p)
term(17) = term(17) + wm_interm_25(q, a) * wm_interm_27(p, a)
term(18) = term(18) + wm_interm_26(q, a) * wm_interm_27(p, a)
term(19) = term(19) + wm_interm_25(q, a) * wm_interm_28(p, a)
term(20) = term(20) + wm_interm_26(q, a) * wm_interm_28(p, a)
term(21) = term(21) + wm_interm_25(a, q) * wm_interm_27(a, p)
term(22) = term(22) + wm_interm_25(a, q) * wm_interm_28(a, p)
term(23) = term(23) + wm_interm_26(a, q) * wm_interm_27(a, p)
term(24) = term(24) + wm_interm_26(a, q) * wm_interm_28(a, p)
term(25) = term(25) + wm_interm_20(a, q) * wm_interm_22(p, a)
term(26) = term(26) + wm_interm_20(a, q) * wm_interm_23(p, a)
term(27) = term(27) + wm_interm_21(a, q) * wm_interm_22(p, a)
term(28) = term(28) + wm_interm_21(a, q) * wm_interm_23(p, a)
term(29) = term(29) + wm_interm_10(p, a) * wm_interm_14(q, a)
term(30) = term(30) + wm_interm_45(a, p) * wm_interm_53(q, a)
term(31) = term(31) + wm_interm_10(a, p) * wm_interm_14(a, q)
term(32) = term(32) + wm_interm_45(p, a) * wm_interm_53(a, q)
term(33) = term(33) + wm_interm_20(q, a) * wm_interm_45(a, p)
term(34) = term(34) + wm_interm_21(q, a) * wm_interm_45(a, p)
term(35) = term(35) + wm_interm_20(a, q) * wm_interm_45(p, a)
term(36) = term(36) + wm_interm_21(a, q) * wm_interm_45(p, a)
term(37) = term(37) + wm_interm_14(q, a) * wm_interm_27(p, a)
term(38) = term(38) + wm_interm_14(q, a) * wm_interm_28(p, a)
term(39) = term(39) + wm_interm_14(a, q) * wm_interm_27(a, p)
term(40) = term(40) + wm_interm_14(a, q) * wm_interm_28(a, p)
term(41) = term(41) + wm_interm_10(p, a) * wm_interm_25(q, a)
term(42) = term(42) + wm_interm_10(p, a) * wm_interm_26(q, a)
term(43) = term(43) + wm_interm_10(a, p) * wm_interm_25(a, q)
term(44) = term(44) + wm_interm_10(a, p) * wm_interm_26(a, q)
term(45) = term(45) + wm_interm_22(a, p) * wm_interm_53(q, a)
term(46) = term(46) + wm_interm_23(a, p) * wm_interm_53(q, a)
term(47) = term(47) + wm_interm_22(p, a) * wm_interm_53(a, q)
term(48) = term(48) + wm_interm_23(p, a) * wm_interm_53(a, q)
end do 

term(13) = term(13) * 4.0d+0 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (-8.0d+0) 
term(16) = term(16) * 4.0d+0 
term(17) = term(17) * (-2.0d+0) 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * 4.0d+0 
term(20) = term(20) * (-8.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * 4.0d+0 
term(23) = term(23) * 4.0d+0 
term(24) = term(24) * (-8.0d+0) 
term(25) = term(25) * 4.0d+0 
term(26) = term(26) * (-8.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * 4.0d+0 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (-2.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * 2.0d+0 
term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * 2.0d+0 
term(37) = term(37) * 2.0d+0 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * 2.0d+0 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * 2.0d+0 
term(42) = term(42) * (-4.0d+0) 
term(43) = term(43) * 2.0d+0 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * 2.0d+0 
term(46) = term(46) * (-4.0d+0) 
term(47) = term(47) * 2.0d+0 
term(48) = term(48) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(49) = term(49) + wm_interm_39(q, i, j, k) * wm_interm_50(p, k, j, i)
term(50) = term(50) + wm_interm_43(q, i, j, k) * wm_interm_54(p, j, k, i)
term(51) = term(51) + wm_interm_43(q, i, j, k) * wm_interm_54(p, k, j, i)
term(52) = term(52) + wm_interm_43(q, i, j, k) * wm_interm_55(p, k, j, i)
term(53) = term(53) + wm_interm_43(q, i, j, k) * wm_interm_55(p, j, k, i)
end do 
end do 
end do 

term(49) = term(49) * (-4.0d+0) 
term(51) = term(51) * (-2.0d+0) 
term(53) = term(53) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(54) = term(54) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_30(k, i, l, j)
term(55) = term(55) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_30(i, k, l, j)
end do 
end do 
end do 
end do 
end do 

term(54) = term(54) * 2.0d+0 
term(55) = -term(55) 

do i = 1, nocc 
term(56) = term(56) + r1(vrdav_Rl, q,i) * wm_interm_2(p, i)
term(57) = term(57) + r1(vrdav_Rl, q,i) * wm_interm_3(p, i)
term(58) = term(58) + s1(q,i) * wm_interm_4(p, i)
term(59) = term(59) + s1(q,i) * wm_interm_5(p, i)
term(60) = term(60) + r1(vrdav_Rr, p,i) * wm_interm_6(q, i)
term(61) = term(61) + r1(vrdav_Rr, p,i) * wm_interm_7(q, i)
term(62) = term(62) + t1(q,i) * wm_interm_8(p, i)
term(63) = term(63) + t1(q,i) * wm_interm_9(p, i)
term(64) = term(64) + wm_interm_40(p, i) * wm_interm_47(q, i)
term(65) = term(65) + wm_interm_41(p, i) * wm_interm_47(q, i)
term(66) = term(66) + wm_interm_41(p, i) * wm_interm_46(q, i)
term(67) = term(67) + wm_interm_40(p, i) * wm_interm_46(q, i)
term(68) = term(68) + wm_interm_44(q, i) * wm_interm_8(p, i)
term(69) = term(69) + wm_interm_44(q, i) * wm_interm_9(p, i)
term(70) = term(70) + wm_interm_48(q, i) * wm_interm_8(p, i)
term(71) = term(71) + wm_interm_48(q, i) * wm_interm_9(p, i)
term(72) = term(72) + wm_interm_2(p, i) * wm_interm_6(q, i)
term(73) = term(73) + wm_interm_3(p, i) * wm_interm_6(q, i)
term(74) = term(74) + wm_interm_2(p, i) * wm_interm_7(q, i)
term(75) = term(75) + wm_interm_3(p, i) * wm_interm_7(q, i)
term(76) = term(76) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, q,i)
end do 

term(56) = term(56) * 4.0d+0 
term(57) = term(57) * (-2.0d+0) 
term(58) = term(58) * 2.0d+0 
term(59) = term(59) * (-4.0d+0) 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * 2.0d+0 
term(62) = term(62) * 2.0d+0 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * 2.0d+0 
term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * 8.0d+0 
term(67) = term(67) * (-4.0d+0) 
term(68) = term(68) * 2.0d+0 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * 8.0d+0 
term(72) = term(72) * (-8.0d+0) 
term(73) = term(73) * 4.0d+0 
term(74) = term(74) * 4.0d+0 
term(75) = term(75) * (-2.0d+0) 
term(76) = term(76) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(77) = term(77) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_16(i, j)
term(78) = term(78) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_17(i, j)
term(79) = term(79) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_31(i, j)
term(80) = term(80) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_32(i, j)
term(81) = term(81) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_29(i, j)
term(82) = term(82) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_34(i, j)
term(83) = term(83) + s1(p,i) * t1(q,j) * wm_interm_18(j, i)
term(84) = term(84) + s1(p,i) * t1(q,j) * wm_interm_19(j, i)
end do 
end do 

term(77) = term(77) * 2.0d+0 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * 2.0d+0 
term(80) = term(80) * (-4.0d+0) 
term(81) = term(81) * 2.0d+0 
term(82) = term(82) * (-4.0d+0) 
term(83) = term(83) * 2.0d+0 
term(84) = term(84) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(85) = term(85) + wm_interm_57(q, a, i, j) * wm_interm_58(p, a, i, j)
term(86) = term(86) + wm_interm_57(q, a, i, j) * wm_interm_59(p, a, i, j)
term(87) = term(87) + wm_interm_57(q, a, i, j) * wm_interm_60(p, a, i, j)
term(88) = term(88) + wm_interm_57(q, a, i, j) * wm_interm_61(p, a, i, j)
term(89) = term(89) + wm_interm_62(a, p, i, j) * wm_interm_63(q, a, j, i)
term(90) = term(90) + wm_interm_62(a, p, i, j) * wm_interm_65(q, a, j, i)
term(91) = term(91) + wm_interm_63(q, a, i, j) * wm_interm_64(a, p, j, i)
term(92) = term(92) + wm_interm_64(a, p, i, j) * wm_interm_65(q, a, j, i)
term(93) = term(93) + wm_interm_60(p, a, i, j) * wm_interm_66(q, a, i, j)
term(94) = term(94) + wm_interm_61(p, a, i, j) * wm_interm_66(q, a, i, j)
term(95) = term(95) + wm_interm_62(a, p, i, j) * wm_interm_67(q, a, j, i)
term(96) = term(96) + wm_interm_64(a, p, i, j) * wm_interm_67(q, a, j, i)
term(97) = term(97) + wm_interm_58(p, a, i, j) * wm_interm_66(q, a, i, j)
term(98) = term(98) + wm_interm_59(p, a, i, j) * wm_interm_66(q, a, i, j)
term(99) = term(99) + wm_interm_62(a, p, i, j) * wm_interm_68(q, a, j, i)
term(100) = term(100) + wm_interm_64(a, p, i, j) * wm_interm_68(q, a, j, i)
term(101) = term(101) + wm_interm_63(q, a, i, j) * wm_interm_69(a, p, j, i)
term(102) = term(102) + wm_interm_63(q, a, i, j) * wm_interm_70(a, p, j, i)
term(103) = term(103) + wm_interm_59(p, a, i, j) * wm_interm_71(q, a, i, j)
term(104) = term(104) + wm_interm_58(p, a, i, j) * wm_interm_71(q, a, i, j)
term(105) = term(105) + wm_interm_65(q, a, i, j) * wm_interm_70(a, p, j, i)
term(106) = term(106) + wm_interm_65(q, a, i, j) * wm_interm_69(a, p, j, i)
term(107) = term(107) + wm_interm_60(p, a, i, j) * wm_interm_71(q, a, i, j)
term(108) = term(108) + wm_interm_61(p, a, i, j) * wm_interm_71(q, a, i, j)
term(109) = term(109) + wm_interm_67(q, a, i, j) * wm_interm_70(a, p, j, i)
term(110) = term(110) + wm_interm_67(q, a, i, j) * wm_interm_69(a, p, j, i)
term(111) = term(111) + wm_interm_60(p, a, i, j) * wm_interm_72(q, a, i, j)
term(112) = term(112) + wm_interm_61(p, a, i, j) * wm_interm_72(q, a, i, j)
term(113) = term(113) + wm_interm_68(q, a, i, j) * wm_interm_70(a, p, j, i)
term(114) = term(114) + wm_interm_68(q, a, i, j) * wm_interm_69(a, p, j, i)
term(115) = term(115) + wm_interm_58(p, a, i, j) * wm_interm_72(q, a, i, j)
term(116) = term(116) + wm_interm_59(p, a, i, j) * wm_interm_72(q, a, i, j)
term(117) = term(117) + wm_interm_57(a, q, i, j) * wm_interm_58(a, p, i, j)
term(118) = term(118) + wm_interm_57(a, q, i, j) * wm_interm_59(a, p, i, j)
term(119) = term(119) + wm_interm_59(a, p, i, j) * wm_interm_71(a, q, i, j)
term(120) = term(120) + wm_interm_58(a, p, i, j) * wm_interm_71(a, q, i, j)
term(121) = term(121) + wm_interm_57(a, q, i, j) * wm_interm_60(a, p, i, j)
term(122) = term(122) + wm_interm_57(a, q, i, j) * wm_interm_61(a, p, i, j)
term(123) = term(123) + wm_interm_60(a, p, i, j) * wm_interm_71(a, q, i, j)
term(124) = term(124) + wm_interm_61(a, p, i, j) * wm_interm_71(a, q, i, j)
term(125) = term(125) + wm_interm_60(a, p, i, j) * wm_interm_72(a, q, i, j)
term(126) = term(126) + wm_interm_61(a, p, i, j) * wm_interm_72(a, q, i, j)
term(127) = term(127) + wm_interm_60(a, p, i, j) * wm_interm_66(a, q, i, j)
term(128) = term(128) + wm_interm_61(a, p, i, j) * wm_interm_66(a, q, i, j)
term(129) = term(129) + wm_interm_62(p, a, i, j) * wm_interm_63(a, q, j, i)
term(130) = term(130) + wm_interm_63(a, q, i, j) * wm_interm_64(p, a, j, i)
term(131) = term(131) + wm_interm_63(a, q, i, j) * wm_interm_69(p, a, j, i)
term(132) = term(132) + wm_interm_63(a, q, i, j) * wm_interm_70(p, a, j, i)
term(133) = term(133) + wm_interm_65(a, q, i, j) * wm_interm_70(p, a, j, i)
term(134) = term(134) + wm_interm_65(a, q, i, j) * wm_interm_69(p, a, j, i)
term(135) = term(135) + wm_interm_62(p, a, i, j) * wm_interm_65(a, q, j, i)
term(136) = term(136) + wm_interm_64(p, a, i, j) * wm_interm_65(a, q, j, i)
term(137) = term(137) + wm_interm_58(a, p, i, j) * wm_interm_72(a, q, i, j)
term(138) = term(138) + wm_interm_59(a, p, i, j) * wm_interm_72(a, q, i, j)
term(139) = term(139) + wm_interm_58(a, p, i, j) * wm_interm_66(a, q, i, j)
term(140) = term(140) + wm_interm_59(a, p, i, j) * wm_interm_66(a, q, i, j)
term(141) = term(141) + wm_interm_67(a, q, i, j) * wm_interm_70(p, a, j, i)
term(142) = term(142) + wm_interm_67(a, q, i, j) * wm_interm_69(p, a, j, i)
term(143) = term(143) + wm_interm_62(p, a, i, j) * wm_interm_67(a, q, j, i)
term(144) = term(144) + wm_interm_64(p, a, i, j) * wm_interm_67(a, q, j, i)
term(145) = term(145) + wm_interm_68(a, q, i, j) * wm_interm_70(p, a, j, i)
term(146) = term(146) + wm_interm_68(a, q, i, j) * wm_interm_69(p, a, j, i)
term(147) = term(147) + wm_interm_62(p, a, i, j) * wm_interm_68(a, q, j, i)
term(148) = term(148) + wm_interm_64(p, a, i, j) * wm_interm_68(a, q, j, i)
term(149) = term(149) + r2(vrdav_Rl, a,j,p,i) * r2(vrdav_Rr, a,i,q,j)
term(150) = term(150) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_47(a, j)
term(151) = term(151) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_46(a, j)
term(152) = term(152) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_40(a, j)
term(153) = term(153) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_41(a, j)
term(154) = term(154) + s2(a,p,i,j) * t1(q,i) * wm_interm_4(a, j)
term(155) = term(155) + s2(a,p,i,j) * t1(q,i) * wm_interm_5(a, j)
term(156) = term(156) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_44(a, j)
term(157) = term(157) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_48(a, j)
term(158) = term(158) + s1(p,i) * t2(a,q,i,j) * wm_interm_9(a, j)
term(159) = term(159) + s1(p,i) * t2(a,q,i,j) * wm_interm_8(a, j)
end do 
end do 
end do 

term(85) = term(85) * (-2.0d+0) 
term(86) = term(86) * 4.0d+0 
term(87) = term(87) * 4.0d+0 
term(88) = term(88) * (-8.0d+0) 
term(89) = term(89) * (-2.0d+0) 
term(90) = term(90) * 4.0d+0 
term(91) = term(91) * 4.0d+0 
term(92) = term(92) * (-8.0d+0) 
term(93) = term(93) * (-8.0d+0) 
term(94) = term(94) * 16.0d+0 
term(95) = term(95) * (-8.0d+0) 
term(96) = term(96) * 16.0d+0 
term(97) = term(97) * 4.0d+0 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * 4.0d+0 
term(100) = term(100) * (-8.0d+0) 
term(101) = term(101) * (-2.0d+0) 
term(102) = term(102) * 4.0d+0 
term(103) = term(103) * (-2.0d+0) 
term(104) = term(104) * 4.0d+0 
term(105) = term(105) * (-2.0d+0) 
term(106) = term(106) * 4.0d+0 
term(107) = term(107) * (-2.0d+0) 
term(108) = term(108) * 4.0d+0 
term(109) = term(109) * 4.0d+0 
term(110) = term(110) * (-8.0d+0) 
term(111) = term(111) * 4.0d+0 
term(112) = term(112) * (-8.0d+0) 
term(113) = term(113) * (-2.0d+0) 
term(114) = term(114) * 4.0d+0 
term(115) = term(115) * (-2.0d+0) 
term(116) = term(116) * 4.0d+0 
term(117) = term(117) * (-2.0d+0) 
term(118) = term(118) * 4.0d+0 
term(119) = term(119) * (-2.0d+0) 
term(120) = term(120) * 4.0d+0 
term(121) = term(121) * 4.0d+0 
term(122) = term(122) * (-8.0d+0) 
term(123) = term(123) * (-2.0d+0) 
term(124) = term(124) * 4.0d+0 
term(125) = term(125) * 4.0d+0 
term(126) = term(126) * (-8.0d+0) 
term(127) = term(127) * (-8.0d+0) 
term(128) = term(128) * 16.0d+0 
term(129) = term(129) * (-2.0d+0) 
term(130) = term(130) * 4.0d+0 
term(131) = term(131) * (-2.0d+0) 
term(132) = term(132) * 4.0d+0 
term(133) = term(133) * (-2.0d+0) 
term(134) = term(134) * 4.0d+0 
term(135) = term(135) * 4.0d+0 
term(136) = term(136) * (-8.0d+0) 
term(137) = term(137) * (-2.0d+0) 
term(138) = term(138) * 4.0d+0 
term(139) = term(139) * 4.0d+0 
term(140) = term(140) * (-8.0d+0) 
term(141) = term(141) * 4.0d+0 
term(142) = term(142) * (-8.0d+0) 
term(143) = term(143) * (-8.0d+0) 
term(144) = term(144) * 16.0d+0 
term(145) = term(145) * (-2.0d+0) 
term(146) = term(146) * 4.0d+0 
term(147) = term(147) * 4.0d+0 
term(148) = term(148) * (-8.0d+0) 
term(149) = term(149) * (-2.0d+0) 
term(150) = term(150) * 2.0d+0 
term(151) = term(151) * (-4.0d+0) 
term(152) = term(152) * 2.0d+0 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * 2.0d+0 
term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * 2.0d+0 
term(157) = term(157) * (-4.0d+0) 
term(158) = term(158) * (-4.0d+0) 
term(159) = term(159) * 2.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(160) = term(160) + r2(vrdav_Rl, a,j,p,i) * r2(vrdav_Rr, a,j,q,i)
term(161) = term(161) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_47(a, j)
term(162) = term(162) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_46(a, j)
term(163) = term(163) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_41(a, j)
term(164) = term(164) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_40(a, j)
term(165) = term(165) + s2(a,p,j,i) * t1(q,i) * wm_interm_4(a, j)
term(166) = term(166) + s2(a,p,j,i) * t1(q,i) * wm_interm_5(a, j)
term(167) = term(167) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_44(a, j)
term(168) = term(168) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_48(a, j)
term(169) = term(169) + s1(p,i) * t2(a,q,j,i) * wm_interm_8(a, j)
term(170) = term(170) + s1(p,i) * t2(a,q,j,i) * wm_interm_9(a, j)
term(171) = term(171) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * wm_interm_6(a, i)
term(172) = term(172) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_6(a, j)
term(173) = term(173) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * wm_interm_7(a, i)
term(174) = term(174) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_7(a, j)
term(175) = term(175) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * wm_interm_2(a, i)
term(176) = term(176) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_2(a, j)
term(177) = term(177) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * wm_interm_3(a, i)
term(178) = term(178) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_3(a, j)
end do 
end do 
end do 

term(160) = term(160) * 4.0d+0 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * 8.0d+0 
term(163) = term(163) * 8.0d+0 
term(164) = term(164) * (-4.0d+0) 
term(165) = term(165) * (-4.0d+0) 
term(166) = term(166) * 8.0d+0 
term(167) = term(167) * (-4.0d+0) 
term(168) = term(168) * 8.0d+0 
term(169) = term(169) * (-4.0d+0) 
term(170) = term(170) * 8.0d+0 
term(171) = term(171) * (-4.0d+0) 
term(172) = term(172) * 8.0d+0 
term(173) = term(173) * 2.0d+0 
term(174) = term(174) * (-4.0d+0) 
term(175) = term(175) * 4.0d+0 
term(176) = term(176) * (-8.0d+0) 
term(177) = term(177) * (-2.0d+0) 
term(178) = term(178) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(179) = term(179) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,j,p,i) * wm_interm_16(i, k)
term(180) = term(180) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,j,p,i) * wm_interm_17(i, k)
term(181) = term(181) + r2(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_16(k, j)
term(182) = term(182) + r2(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_17(k, j)
term(183) = term(183) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_16(i, j)
term(184) = term(184) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_17(i, j)
term(185) = term(185) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,k,j) * wm_interm_29(i, k)
term(186) = term(186) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,k,j) * wm_interm_34(i, k)
term(187) = term(187) + s2(a,p,k,j) * t2(a,q,j,i) * wm_interm_18(i, k)
term(188) = term(188) + s2(a,p,k,j) * t2(a,q,j,i) * wm_interm_19(i, k)
term(189) = term(189) + s2(a,p,k,j) * t2(a,q,k,i) * wm_interm_18(i, j)
term(190) = term(190) + s2(a,p,k,j) * t2(a,q,k,i) * wm_interm_19(i, j)
term(191) = term(191) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,i,j) * wm_interm_31(k, j)
term(192) = term(192) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,i,j) * wm_interm_32(k, j)
term(193) = term(193) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,k,j) * wm_interm_31(i, j)
term(194) = term(194) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,k,j) * wm_interm_32(i, j)
term(195) = term(195) + s2(a,p,i,j) * t2(a,q,k,i) * wm_interm_18(k, j)
term(196) = term(196) + s2(a,p,i,j) * t2(a,q,k,i) * wm_interm_19(k, j)
term(197) = term(197) + r2(vrdav_Rl, a,j,p,i) * t2(a,q,k,j) * wm_interm_31(i, k)
term(198) = term(198) + r2(vrdav_Rl, a,j,p,i) * t2(a,q,k,j) * wm_interm_32(i, k)
term(199) = term(199) + s2(a,p,k,j) * t2(a,q,j,i) * wm_interm_38(i, k)
term(200) = term(200) + s2(a,p,k,j) * t2(a,q,k,i) * wm_interm_38(i, j)
term(201) = term(201) + s2(a,p,i,j) * t2(a,q,k,i) * wm_interm_38(k, j)
term(202) = term(202) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,k,j) * wm_interm_13(i, k)
term(203) = term(203) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,i,j) * wm_interm_12(k, j)
term(204) = term(204) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,k,j) * wm_interm_12(i, j)
term(205) = term(205) + r2(vrdav_Rl, a,j,p,i) * t2(a,q,k,j) * wm_interm_12(i, k)
term(206) = term(206) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,j,p,i) * wm_interm_51(i, k)
term(207) = term(207) + r2(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_51(k, j)
term(208) = term(208) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_51(i, j)
end do 
end do 
end do 
end do 

term(179) = term(179) * (-2.0d+0) 
term(180) = term(180) * 4.0d+0 
term(181) = term(181) * (-2.0d+0) 
term(182) = term(182) * 4.0d+0 
term(183) = term(183) * 4.0d+0 
term(184) = term(184) * (-8.0d+0) 
term(185) = term(185) * (-2.0d+0) 
term(186) = term(186) * 4.0d+0 
term(187) = term(187) * (-2.0d+0) 
term(188) = term(188) * 4.0d+0 
term(189) = term(189) * 4.0d+0 
term(190) = term(190) * (-8.0d+0) 
term(191) = term(191) * (-2.0d+0) 
term(192) = term(192) * 4.0d+0 
term(193) = term(193) * 4.0d+0 
term(194) = term(194) * (-8.0d+0) 
term(195) = term(195) * (-2.0d+0) 
term(196) = term(196) * 4.0d+0 
term(197) = term(197) * (-2.0d+0) 
term(198) = term(198) * 4.0d+0 
term(199) = term(199) * 2.0d+0 
term(200) = term(200) * (-4.0d+0) 
term(201) = term(201) * 2.0d+0 
term(202) = term(202) * 2.0d+0 
term(203) = term(203) * 2.0d+0 
term(204) = term(204) * (-4.0d+0) 
term(205) = term(205) * 2.0d+0 
term(206) = term(206) * 2.0d+0 
term(207) = term(207) * 2.0d+0 
term(208) = term(208) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(209) = term(209) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,p,i) * wm_interm_16(k, j)
term(210) = term(210) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,p,i) * wm_interm_17(k, j)
term(211) = term(211) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,k,i) * wm_interm_29(j, k)
term(212) = term(212) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,k,i) * wm_interm_34(j, k)
term(213) = term(213) + s2(a,p,j,i) * t2(a,q,k,i) * wm_interm_18(k, j)
term(214) = term(214) + s2(a,p,j,i) * t2(a,q,k,i) * wm_interm_19(k, j)
term(215) = term(215) + r2(vrdav_Rl, a,j,p,i) * t2(a,q,k,i) * wm_interm_31(j, k)
term(216) = term(216) + r2(vrdav_Rl, a,j,p,i) * t2(a,q,k,i) * wm_interm_32(j, k)
term(217) = term(217) + s2(a,p,j,i) * t2(a,q,k,i) * wm_interm_38(k, j)
term(218) = term(218) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,k,i) * wm_interm_13(j, k)
term(219) = term(219) + r2(vrdav_Rl, a,j,p,i) * t2(a,q,k,i) * wm_interm_12(j, k)
term(220) = term(220) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,p,i) * wm_interm_51(k, j)
end do 
end do 
end do 
end do 

term(209) = term(209) * 4.0d+0 
term(210) = term(210) * (-8.0d+0) 
term(211) = term(211) * 4.0d+0 
term(212) = term(212) * (-8.0d+0) 
term(213) = term(213) * 4.0d+0 
term(214) = term(214) * (-8.0d+0) 
term(215) = term(215) * 4.0d+0 
term(216) = term(216) * (-8.0d+0) 
term(217) = term(217) * (-4.0d+0) 
term(218) = term(218) * (-4.0d+0) 
term(219) = term(219) * (-4.0d+0) 
term(220) = term(220) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(221) = term(221) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_24(i, l, k, j)
term(222) = term(222) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_24(l, i, k, j)
term(223) = term(223) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_15(i, l, k, j)
term(224) = term(224) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_15(l, i, k, j)
end do 
end do 
end do 
end do 
end do 

term(221) = -term(221) 
term(222) = term(222) * 2.0d+0 
term(223) = -term(223) 
term(224) = term(224) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(225) = term(225) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_33(i, j, l, k)
term(226) = term(226) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_33(j, i, l, k)
end do 
end do 
end do 
end do 
end do 

term(225) = -term(225) 
term(226) = term(226) * 2.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(227) = term(227) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_33(j, i, k, l)
term(228) = term(228) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_33(i, j, k, l)
end do 
end do 
end do 
end do 
end do 

term(227) = -term(227) 
term(228) = term(228) * 2.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(229) = term(229) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_24(l, i, j, k)
term(230) = term(230) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_24(i, l, j, k)
term(231) = term(231) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_15(i, l, j, k)
term(232) = term(232) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_15(l, i, j, k)
end do 
end do 
end do 
end do 
end do 

term(229) = -term(229) 
term(230) = term(230) * 2.0d+0 
term(231) = term(231) * 2.0d+0 
term(232) = -term(232) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(233) = term(233) + r2(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_20(b, a)
term(234) = term(234) + r2(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_21(b, a)
term(235) = term(235) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_22(b, a)
term(236) = term(236) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_23(b, a)
end do 
end do 
end do 
end do 

term(233) = term(233) * 4.0d+0 
term(234) = term(234) * (-2.0d+0) 
term(235) = term(235) * (-2.0d+0) 
term(236) = term(236) * 4.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(237) = term(237) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, b,j,p,i) * wm_interm_20(b, a)
term(238) = term(238) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, b,j,p,i) * wm_interm_21(b, a)
term(239) = term(239) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_22(b, a)
term(240) = term(240) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_23(b, a)
end do 
end do 
end do 
end do 

term(237) = term(237) * (-8.0d+0) 
term(238) = term(238) * 4.0d+0 
term(239) = term(239) * 4.0d+0 
term(240) = term(240) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(241) = term(241) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,i,j) * wm_interm_25(a, b)
term(242) = term(242) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,i,j) * wm_interm_26(a, b)
term(243) = term(243) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,i,j) * wm_interm_27(a, b)
term(244) = term(244) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,i,j) * wm_interm_28(a, b)
end do 
end do 
end do 
end do 

term(241) = term(241) * (-2.0d+0) 
term(242) = term(242) * 4.0d+0 
term(243) = term(243) * (-2.0d+0) 
term(244) = term(244) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(245) = term(245) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,j,i) * wm_interm_25(a, b)
term(246) = term(246) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,j,i) * wm_interm_26(a, b)
term(247) = term(247) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_27(a, b)
term(248) = term(248) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_28(a, b)
end do 
end do 
end do 
end do 

term(245) = term(245) * 4.0d+0 
term(246) = term(246) * (-8.0d+0) 
term(247) = term(247) * 4.0d+0 
term(248) = term(248) * (-8.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(249) = term(249) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_30(k, i, j, l)
term(250) = term(250) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_30(i, k, j, l)
end do 
end do 
end do 
end do 
end do 

term(249) = -term(249) 
term(250) = term(250) * 2.0d+0 

term(251) = term(251) + wm_interm_0 * wm_interm_20(p, q)
term(252) = term(252) + wm_interm_1 * wm_interm_20(p, q)
term(253) = term(253) + wm_interm_0 * wm_interm_21(p, q)
term(254) = term(254) + wm_interm_1 * wm_interm_21(p, q)
term(255) = term(255) + wm_interm_11 * wm_interm_53(p, q)
term(256) = term(256) + wm_interm_11 * wm_interm_20(p, q)
term(257) = term(257) + wm_interm_11 * wm_interm_21(p, q)
term(258) = term(258) + wm_interm_0 * wm_interm_53(p, q)
term(259) = term(259) + wm_interm_1 * wm_interm_53(p, q)

term(251) = term(251) * (-4.0d+0) 
term(252) = term(252) * 8.0d+0 
term(253) = term(253) * 2.0d+0 
term(254) = term(254) * (-4.0d+0) 
term(255) = term(255) * 4.0d+0 
term(256) = term(256) * 8.0d+0 
term(257) = term(257) * (-4.0d+0) 
term(258) = term(258) * (-2.0d+0) 
term(259) = term(259) * 4.0d+0 


    calc_D_vv_wm2 = zero
    do s = 0, 259
    calc_D_vv_wm2 = calc_D_vv_wm2 + term(s)
    end do

    end function calc_D_vv_wm2
    


























    
    
    function overlap_f(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr) 
    double precision :: overlap_f
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer :: s ,c,a,k,i,j,b,l,d 
    double precision, dimension(0:225) :: term 
   term = 0.d+0 
    do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_0(a, i) * wmo_interm_1(b, j)
term(1) = term(1) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_2(a, i) * wmo_interm_1(b, j)
term(2) = term(2) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_0(a, i) * wmo_interm_3(b, j)
term(3) = term(3) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_2(a, i) * wmo_interm_3(b, j)
term(4) = term(4) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_0(b, j) * wmo_interm_1(a, i)
term(5) = term(5) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_0(a, j) * wmo_interm_1(b, i)
term(6) = term(6) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_2(b, j) * wmo_interm_1(a, i)
term(7) = term(7) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_2(a, j) * wmo_interm_1(b, i)
term(8) = term(8) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_0(a, j) * wmo_interm_3(b, i)
term(9) = term(9) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_2(a, j) * wmo_interm_3(b, i)
term(10) = term(10) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_0(b, i) * wmo_interm_1(a, j)
term(11) = term(11) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_2(b, i) * wmo_interm_1(a, j)
term(12) = term(12) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_0(b, i) * wmo_interm_3(a, j)
term(13) = term(13) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_0(b, j) * wmo_interm_3(a, i)
term(14) = term(14) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_2(b, i) * wmo_interm_3(a, j)
term(15) = term(15) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_2(b, j) * wmo_interm_3(a, i)
term(16) = term(16) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,b,j)
term(17) = term(17) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,b,j) * s1(b,j)
term(18) = term(18) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i) * t1(b,j)
term(19) = term(19) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,j) * t1(b,i)
term(20) = term(20) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,i) * t1(a,j)
term(21) = term(21) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,j) * t1(a,i)
end do 
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * 8.0d+0 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * 2.0d+0 
term(6) = term(6) * 2.0d+0 
term(7) = -term(7) 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * 2.0d+0 
term(10) = term(10) * 2.0d+0 
term(11) = -term(11) 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * 8.0d+0 
term(14) = term(14) * 2.0d+0 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * 2.0d+0 
term(17) = term(17) * 4.0d+0 
term(18) = term(18) * (-2.0d+0) 
term(21) = term(21) * (-2.0d+0) 

do d = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(22) = term(22) + s1(d,k) * wmo_interm_4(b, d) * wmo_interm_0(b, k)
term(23) = term(23) + s1(d,k) * wmo_interm_5(b, d) * wmo_interm_0(b, k)
end do 
end do 
end do 

term(22) = term(22) * 2.0d+0 
term(23) = term(23) * (-4.0d+0) 

do d = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(24) = term(24) + s1(d,k) * wmo_interm_5(a, d) * wmo_interm_0(a, k)
term(25) = term(25) + s1(d,k) * wmo_interm_4(a, d) * wmo_interm_0(a, k)
end do 
end do 
end do 

term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * 2.0d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(26) = term(26) + s1(d,l) * wmo_interm_4(b, d) * wmo_interm_2(b, l)
term(27) = term(27) + s1(d,l) * wmo_interm_5(b, d) * wmo_interm_2(b, l)
end do 
end do 
end do 

term(26) = -term(26) 
term(27) = term(27) * 2.0d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(28) = term(28) + s1(d,l) * wmo_interm_5(a, d) * wmo_interm_2(a, l)
term(29) = term(29) + s1(d,l) * wmo_interm_4(a, d) * wmo_interm_2(a, l)
end do 
end do 
end do 

term(28) = term(28) * 2.0d+0 
term(29) = -term(29) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(30) = term(30) + wmo_interm_36(i, j, k, l) * wmo_interm_37(l, k, i, j)
term(31) = term(31) + wmo_interm_36(i, j, k, l) * wmo_interm_37(k, l, i, j)
end do 
end do 
end do 
end do 

term(30) = term(30) * (-0.5d+0) 

do l = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(32) = term(32) + s1(c,l) * wmo_interm_6(j, l) * wmo_interm_0(c, j)
term(33) = term(33) + s1(c,l) * wmo_interm_6(j, l) * wmo_interm_2(c, j)
term(34) = term(34) + s1(c,l) * wmo_interm_7(j, l) * wmo_interm_0(c, j)
term(35) = term(35) + s1(c,l) * wmo_interm_7(j, l) * wmo_interm_2(c, j)
end do 
end do 
end do 

term(33) = term(33) * (-0.5d+0) 
term(34) = term(34) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
term(36) = term(36) + wmo_interm_6(j, l) * wmo_interm_28(j, l)
term(37) = term(37) + wmo_interm_6(j, l) * wmo_interm_29(j, l)
term(38) = term(38) + wmo_interm_6(j, l) * wmo_interm_28(j, l)
term(39) = term(39) + wmo_interm_6(j, l) * wmo_interm_29(j, l)
term(40) = term(40) + wmo_interm_7(j, l) * wmo_interm_28(j, l)
term(41) = term(41) + wmo_interm_7(j, l) * wmo_interm_29(j, l)
term(42) = term(42) + wmo_interm_7(j, l) * wmo_interm_28(j, l)
term(43) = term(43) + wmo_interm_7(j, l) * wmo_interm_29(j, l)
end do 
end do 

term(36) = term(36) * (-0.5d+0) 
term(38) = term(38) * (-0.5d+0) 
term(41) = term(41) * (-2.0d+0) 
term(43) = term(43) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do d = nocc + 1, nactive 
term(44) = term(44) + s1(d,l) * wmo_interm_6(j, l) * wmo_interm_2(d, j)
term(45) = term(45) + s1(d,l) * wmo_interm_6(j, l) * wmo_interm_0(d, j)
term(46) = term(46) + s1(d,l) * wmo_interm_7(j, l) * wmo_interm_2(d, j)
term(47) = term(47) + s1(d,l) * wmo_interm_7(j, l) * wmo_interm_0(d, j)
end do 
end do 
end do 

term(44) = term(44) * (-0.5d+0) 
term(47) = term(47) * (-2.0d+0) 

do d = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(48) = term(48) + wmo_interm_5(a, d) * wmo_interm_26(a, d)
term(49) = term(49) + wmo_interm_5(a, d) * wmo_interm_27(a, d)
term(50) = term(50) + wmo_interm_4(a, d) * wmo_interm_26(a, d)
term(51) = term(51) + wmo_interm_4(a, d) * wmo_interm_27(a, d)
end do 
end do 

term(48) = term(48) * 2.0d+0 
term(49) = term(49) * (-4.0d+0) 
term(50) = -term(50) 
term(51) = term(51) * 2.0d+0 

do l = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(52) = term(52) + s1(c,l) * wmo_interm_7(i, l) * wmo_interm_0(c, i)
term(53) = term(53) + s1(c,l) * wmo_interm_7(i, l) * wmo_interm_2(c, i)
term(54) = term(54) + s1(c,l) * wmo_interm_6(i, l) * wmo_interm_0(c, i)
term(55) = term(55) + s1(c,l) * wmo_interm_6(i, l) * wmo_interm_2(c, i)
end do 
end do 
end do 

term(52) = term(52) * (-2.0d+0) 
term(55) = term(55) * (-0.5d+0) 

do l = 1, nocc 
do i = 1, nocc 
do d = nocc + 1, nactive 
term(56) = term(56) + s1(d,l) * wmo_interm_7(i, l) * wmo_interm_2(d, i)
term(57) = term(57) + s1(d,l) * wmo_interm_7(i, l) * wmo_interm_0(d, i)
term(58) = term(58) + s1(d,l) * wmo_interm_6(i, l) * wmo_interm_2(d, i)
term(59) = term(59) + s1(d,l) * wmo_interm_6(i, l) * wmo_interm_0(d, i)
end do 
end do 
end do 

term(57) = term(57) * (-2.0d+0) 
term(58) = term(58) * (-0.5d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(60) = term(60) + wmo_interm_38(i, j, l, k) * wmo_interm_39(i, j, k, l)
term(61) = term(61) + wmo_interm_38(i, j, l, k) * wmo_interm_39(j, i, k, l)
term(62) = term(62) + wmo_interm_38(i, j, k, l) * wmo_interm_39(j, i, k, l)
term(63) = term(63) + wmo_interm_38(i, j, k, l) * wmo_interm_39(i, j, k, l)
end do 
end do 
end do 
end do 

term(60) = term(60) * (-0.5d+0) 
term(62) = term(62) * (-0.5d+0) 

do l = 1, nocc 
do i = 1, nocc 
term(64) = term(64) + wmo_interm_7(i, l) * wmo_interm_28(i, l)
term(65) = term(65) + wmo_interm_7(i, l) * wmo_interm_29(i, l)
term(66) = term(66) + wmo_interm_7(i, l) * wmo_interm_28(i, l)
term(67) = term(67) + wmo_interm_7(i, l) * wmo_interm_29(i, l)
term(68) = term(68) + wmo_interm_6(i, l) * wmo_interm_28(i, l)
term(69) = term(69) + wmo_interm_6(i, l) * wmo_interm_29(i, l)
term(70) = term(70) + wmo_interm_6(i, l) * wmo_interm_28(i, l)
term(71) = term(71) + wmo_interm_6(i, l) * wmo_interm_29(i, l)
end do 
end do 

term(65) = term(65) * (-2.0d+0) 
term(67) = term(67) * (-2.0d+0) 
term(68) = term(68) * (-0.5d+0) 
term(70) = term(70) * (-0.5d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(72) = term(72) + r1(vrdav_Rl, a,i) * wmo_interm_0(a, j) * wmo_interm_8(j, i)
term(73) = term(73) + r1(vrdav_Rl, a,i) * wmo_interm_0(a, j) * wmo_interm_9(j, i)
term(74) = term(74) + r1(vrdav_Rl, a,i) * wmo_interm_0(a, j) * wmo_interm_8(j, i)
term(75) = term(75) + r1(vrdav_Rl, a,i) * wmo_interm_0(a, j) * wmo_interm_9(j, i)
term(76) = term(76) + r1(vrdav_Rl, a,i) * wmo_interm_0(a, j) * wmo_interm_15(j, i)
end do 
end do 
end do 

term(72) = term(72) * 2.0d+0 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * 2.0d+0 
term(75) = term(75) * (-4.0d+0) 
term(76) = term(76) * (-4.0d+0) 

do j = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(77) = term(77) + s2(b,d,l,j) * wmo_interm_10(d, l) * wmo_interm_0(b, j)
term(78) = term(78) + s2(b,d,l,j) * wmo_interm_13(d, l) * wmo_interm_0(b, j)
end do 
end do 
end do 
end do 

term(77) = term(77) * 2.0d+0 
term(78) = term(78) * (-4.0d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(79) = term(79) + s2(b,d,j,l) * wmo_interm_10(d, l) * wmo_interm_0(b, j)
term(80) = term(80) + s2(b,d,j,l) * wmo_interm_13(d, l) * wmo_interm_0(b, j)
end do 
end do 
end do 
end do 

term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * 8.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(81) = term(81) + r1(vrdav_Rl, a,i) * wmo_interm_2(a, k) * wmo_interm_8(k, i)
term(82) = term(82) + r1(vrdav_Rl, a,i) * wmo_interm_2(a, k) * wmo_interm_9(k, i)
term(83) = term(83) + r1(vrdav_Rl, a,i) * wmo_interm_2(a, k) * wmo_interm_8(k, i)
term(84) = term(84) + r1(vrdav_Rl, a,i) * wmo_interm_2(a, k) * wmo_interm_9(k, i)
term(85) = term(85) + r1(vrdav_Rl, a,i) * wmo_interm_2(a, k) * wmo_interm_15(k, i)
end do 
end do 
end do 

term(81) = -term(81) 
term(82) = term(82) * 2.0d+0 
term(83) = -term(83) 
term(84) = term(84) * 2.0d+0 
term(85) = term(85) * 2.0d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(86) = term(86) + s2(c,d,j,l) * wmo_interm_10(d, l) * wmo_interm_2(c, j)
term(87) = term(87) + s2(c,d,j,l) * wmo_interm_13(d, l) * wmo_interm_2(c, j)
end do 
end do 
end do 
end do 

term(86) = term(86) * 2.0d+0 
term(87) = term(87) * (-4.0d+0) 

do k = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(88) = term(88) + s2(b,d,l,k) * wmo_interm_10(d, l) * wmo_interm_2(b, k)
term(89) = term(89) + s2(b,d,l,k) * wmo_interm_13(d, l) * wmo_interm_2(b, k)
end do 
end do 
end do 
end do 

term(88) = -term(88) 
term(89) = term(89) * 2.0d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(90) = term(90) + s2(b,d,k,l) * wmo_interm_10(d, l) * wmo_interm_2(b, k)
term(91) = term(91) + s2(b,d,k,l) * wmo_interm_13(d, l) * wmo_interm_2(b, k)
end do 
end do 
end do 
end do 

term(90) = term(90) * 2.0d+0 
term(91) = term(91) * (-4.0d+0) 

do j = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do c = nocc + 1, nactive 
term(92) = term(92) + s2(c,d,l,j) * wmo_interm_10(d, l) * wmo_interm_2(c, j)
term(93) = term(93) + s2(c,d,l,j) * wmo_interm_13(d, l) * wmo_interm_2(c, j)
end do 
end do 
end do 
end do 

term(92) = -term(92) 
term(93) = term(93) * 2.0d+0 

do k = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do c = nocc + 1, nactive 
term(94) = term(94) + s2(c,d,l,k) * wmo_interm_10(d, l) * wmo_interm_0(c, k)
term(95) = term(95) + s2(c,d,l,k) * wmo_interm_13(d, l) * wmo_interm_0(c, k)
end do 
end do 
end do 
end do 

term(94) = term(94) * 2.0d+0 
term(95) = term(95) * (-4.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(96) = term(96) + wmo_interm_36(i, j, k, l) * wmo_interm_37(k, l, j, i)
term(97) = term(97) + wmo_interm_36(i, j, k, l) * wmo_interm_37(l, k, j, i)
end do 
end do 
end do 
end do 

term(96) = term(96) * (-0.5d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
term(98) = term(98) + s2(c,d,k,l) * wmo_interm_10(d, l) * wmo_interm_0(c, k)
term(99) = term(99) + s2(c,d,k,l) * wmo_interm_13(d, l) * wmo_interm_0(c, k)
end do 
end do 
end do 
end do 

term(98) = term(98) * (-4.0d+0) 
term(99) = term(99) * 8.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(100) = term(100) + wmo_interm_40(b, c, j, k) * wmo_interm_41(c, b, k, j)
term(101) = term(101) + wmo_interm_42(b, c, j, k) * wmo_interm_41(c, b, k, j)
term(102) = term(102) + wmo_interm_40(b, c, j, k) * wmo_interm_43(c, b, k, j)
term(103) = term(103) + wmo_interm_42(b, c, j, k) * wmo_interm_43(c, b, k, j)
term(104) = term(104) + wmo_interm_40(b, c, j, k) * wmo_interm_44(c, b, k, j)
term(105) = term(105) + wmo_interm_42(b, c, j, k) * wmo_interm_44(c, b, k, j)
term(106) = term(106) + wmo_interm_40(b, c, j, k) * wmo_interm_45(c, b, k, j)
term(107) = term(107) + wmo_interm_42(b, c, j, k) * wmo_interm_45(c, b, k, j)
end do 
end do 
end do 
end do 

term(100) = -term(100) 
term(101) = term(101) * 2.0d+0 
term(102) = term(102) * 2.0d+0 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * 8.0d+0 
term(106) = term(106) * 2.0d+0 
term(107) = term(107) * (-4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(108) = term(108) + r1(vrdav_Rl, a,i) * wmo_interm_2(c, i) * wmo_interm_11(c, a)
term(109) = term(109) + r1(vrdav_Rl, a,i) * wmo_interm_0(c, i) * wmo_interm_11(c, a)
term(110) = term(110) + r1(vrdav_Rl, a,i) * wmo_interm_2(c, i) * wmo_interm_12(c, a)
term(111) = term(111) + r1(vrdav_Rl, a,i) * wmo_interm_0(c, i) * wmo_interm_12(c, a)
end do 
end do 
end do 

term(108) = term(108) * 2.0d+0 
term(109) = term(109) * (-4.0d+0) 
term(110) = -term(110) 
term(111) = term(111) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(112) = term(112) + wmo_interm_40(a, c, i, k) * wmo_interm_41(c, a, k, i)
term(113) = term(113) + wmo_interm_42(a, c, i, k) * wmo_interm_41(c, a, k, i)
term(114) = term(114) + wmo_interm_40(a, c, i, k) * wmo_interm_43(c, a, k, i)
term(115) = term(115) + wmo_interm_42(a, c, i, k) * wmo_interm_43(c, a, k, i)
term(116) = term(116) + wmo_interm_40(a, c, i, k) * wmo_interm_44(c, a, k, i)
term(117) = term(117) + wmo_interm_42(a, c, i, k) * wmo_interm_44(c, a, k, i)
term(118) = term(118) + wmo_interm_40(a, c, i, k) * wmo_interm_45(c, a, k, i)
term(119) = term(119) + wmo_interm_42(a, c, i, k) * wmo_interm_45(c, a, k, i)
end do 
end do 
end do 
end do 

term(112) = -term(112) 
term(113) = term(113) * 2.0d+0 
term(114) = term(114) * 2.0d+0 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * 8.0d+0 
term(118) = term(118) * 2.0d+0 
term(119) = term(119) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(120) = term(120) + wmo_interm_46(b, c, i, k) * wmo_interm_41(c, b, k, i)
term(121) = term(121) + wmo_interm_47(b, c, i, k) * wmo_interm_41(c, b, k, i)
term(122) = term(122) + wmo_interm_47(b, c, i, k) * wmo_interm_43(c, b, k, i)
term(123) = term(123) + wmo_interm_46(b, c, i, k) * wmo_interm_43(c, b, k, i)
term(124) = term(124) + wmo_interm_47(b, c, i, k) * wmo_interm_44(c, b, k, i)
term(125) = term(125) + wmo_interm_46(b, c, i, k) * wmo_interm_44(c, b, k, i)
term(126) = term(126) + wmo_interm_47(b, c, i, k) * wmo_interm_45(c, b, k, i)
term(127) = term(127) + wmo_interm_46(b, c, i, k) * wmo_interm_45(c, b, k, i)
end do 
end do 
end do 
end do 

term(120) = -term(120) 
term(121) = term(121) * 2.0d+0 
term(122) = -term(122) 
term(123) = term(123) * 2.0d+0 
term(124) = term(124) * 2.0d+0 
term(125) = term(125) * (-4.0d+0) 
term(126) = -term(126) 
term(127) = term(127) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(128) = term(128) + wmo_interm_14(i, j) * wmo_interm_15(j, i)
term(129) = term(129) + wmo_interm_16(i, j) * wmo_interm_17(i, j)
term(130) = term(130) + wmo_interm_14(i, j) * wmo_interm_8(j, i)
term(131) = term(131) + wmo_interm_14(i, j) * wmo_interm_9(j, i)
term(132) = term(132) + wmo_interm_14(i, j) * wmo_interm_8(j, i)
term(133) = term(133) + wmo_interm_14(i, j) * wmo_interm_9(j, i)
end do 
end do 

term(128) = term(128) * (-2.0d+0) 
term(129) = term(129) * (-2.0d+0) 
term(131) = term(131) * (-2.0d+0) 
term(133) = term(133) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(134) = term(134) + wmo_interm_46(a, c, j, k) * wmo_interm_41(c, a, k, j)
term(135) = term(135) + wmo_interm_47(a, c, j, k) * wmo_interm_41(c, a, k, j)
term(136) = term(136) + wmo_interm_47(a, c, j, k) * wmo_interm_43(c, a, k, j)
term(137) = term(137) + wmo_interm_46(a, c, j, k) * wmo_interm_43(c, a, k, j)
term(138) = term(138) + wmo_interm_47(a, c, j, k) * wmo_interm_44(c, a, k, j)
term(139) = term(139) + wmo_interm_46(a, c, j, k) * wmo_interm_44(c, a, k, j)
term(140) = term(140) + wmo_interm_47(a, c, j, k) * wmo_interm_45(c, a, k, j)
term(141) = term(141) + wmo_interm_46(a, c, j, k) * wmo_interm_45(c, a, k, j)
end do 
end do 
end do 
end do 

term(134) = -term(134) 
term(135) = term(135) * 2.0d+0 
term(136) = -term(136) 
term(137) = term(137) * 2.0d+0 
term(138) = term(138) * 2.0d+0 
term(139) = term(139) * (-4.0d+0) 
term(140) = -term(140) 
term(141) = term(141) * 2.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(142) = term(142) + wmo_interm_18(a, i) * wmo_interm_0(a, i)
term(143) = term(143) + wmo_interm_18(a, i) * wmo_interm_2(a, i)
term(144) = term(144) + wmo_interm_30(a, i) * wmo_interm_1(a, i)
term(145) = term(145) + wmo_interm_30(a, i) * wmo_interm_3(a, i)
term(146) = term(146) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,i)
end do 
end do 

term(142) = term(142) * (-4.0d+0) 
term(143) = term(143) * 2.0d+0 
term(144) = term(144) * (-2.0d+0) 
term(145) = term(145) * 4.0d+0 
term(146) = term(146) * 2.0d+0 

do j = 1, nocc 
do a = nocc + 1, nactive 
term(147) = term(147) + wmo_interm_19(a, j) * wmo_interm_0(a, j)
term(148) = term(148) + wmo_interm_19(a, j) * wmo_interm_2(a, j)
term(149) = term(149) + wmo_interm_32(a, j) * wmo_interm_1(a, j)
term(150) = term(150) + wmo_interm_32(a, j) * wmo_interm_3(a, j)
end do 
end do 

term(147) = term(147) * 2.0d+0 
term(148) = -term(148) 
term(150) = term(150) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
term(151) = term(151) + wmo_interm_19(b, i) * wmo_interm_0(b, i)
term(152) = term(152) + wmo_interm_19(b, i) * wmo_interm_2(b, i)
term(153) = term(153) + wmo_interm_32(b, i) * wmo_interm_1(b, i)
term(154) = term(154) + wmo_interm_32(b, i) * wmo_interm_3(b, i)
end do 
end do 

term(151) = term(151) * 2.0d+0 
term(152) = -term(152) 
term(154) = term(154) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
term(155) = term(155) + wmo_interm_18(b, j) * wmo_interm_0(b, j)
term(156) = term(156) + wmo_interm_18(b, j) * wmo_interm_2(b, j)
term(157) = term(157) + wmo_interm_30(b, j) * wmo_interm_1(b, j)
term(158) = term(158) + wmo_interm_30(b, j) * wmo_interm_3(b, j)
end do 
end do 

term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * 2.0d+0 
term(157) = term(157) * (-2.0d+0) 
term(158) = term(158) * 4.0d+0 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(159) = term(159) + r1(vrdav_Rl, a,i) * wmo_interm_0(b, i) * wmo_interm_11(b, a)
term(160) = term(160) + r1(vrdav_Rl, a,i) * wmo_interm_0(b, i) * wmo_interm_12(b, a)
term(161) = term(161) + r1(vrdav_Rl, a,i) * wmo_interm_2(b, i) * wmo_interm_11(b, a)
term(162) = term(162) + r1(vrdav_Rl, a,i) * wmo_interm_2(b, i) * wmo_interm_12(b, a)
end do 
end do 
end do 

term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * 2.0d+0 
term(161) = term(161) * 2.0d+0 
term(162) = -term(162) 

do k = 1, nocc 
do j = 1, nocc 
term(163) = term(163) + wmo_interm_20(j, k) * wmo_interm_15(k, j)
term(164) = term(164) + wmo_interm_21(j, k) * wmo_interm_15(k, j)
term(165) = term(165) + wmo_interm_6(j, k) * wmo_interm_17(j, k)
term(166) = term(166) + wmo_interm_7(j, k) * wmo_interm_17(j, k)
term(167) = term(167) + wmo_interm_20(j, k) * wmo_interm_8(k, j)
term(168) = term(168) + wmo_interm_21(j, k) * wmo_interm_8(k, j)
term(169) = term(169) + wmo_interm_20(j, k) * wmo_interm_9(k, j)
term(170) = term(170) + wmo_interm_21(j, k) * wmo_interm_9(k, j)
term(171) = term(171) + wmo_interm_20(j, k) * wmo_interm_8(k, j)
term(172) = term(172) + wmo_interm_21(j, k) * wmo_interm_8(k, j)
term(173) = term(173) + wmo_interm_20(j, k) * wmo_interm_9(k, j)
term(174) = term(174) + wmo_interm_21(j, k) * wmo_interm_9(k, j)
end do 
end do 

term(164) = term(164) * (-2.0d+0) 
term(166) = term(166) * (-2.0d+0) 
term(167) = term(167) * (-0.5d+0) 
term(170) = term(170) * (-2.0d+0) 
term(171) = term(171) * (-0.5d+0) 
term(174) = term(174) * (-2.0d+0) 

do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(175) = term(175) + wmo_interm_22(b, c) * wmo_interm_23(c, b)
term(176) = term(176) + wmo_interm_24(b, c) * wmo_interm_23(c, b)
term(177) = term(177) + wmo_interm_4(b, c) * wmo_interm_31(b, c)
term(178) = term(178) + wmo_interm_5(b, c) * wmo_interm_31(b, c)
term(179) = term(179) + wmo_interm_22(b, c) * wmo_interm_11(c, b)
term(180) = term(180) + wmo_interm_24(b, c) * wmo_interm_11(c, b)
term(181) = term(181) + wmo_interm_22(b, c) * wmo_interm_12(c, b)
term(182) = term(182) + wmo_interm_24(b, c) * wmo_interm_12(c, b)
end do 
end do 

term(176) = term(176) * (-2.0d+0) 
term(178) = term(178) * (-2.0d+0) 
term(179) = term(179) * 2.0d+0 
term(180) = term(180) * (-4.0d+0) 
term(181) = -term(181) 
term(182) = term(182) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
term(183) = term(183) + wmo_interm_20(i, k) * wmo_interm_15(k, i)
term(184) = term(184) + wmo_interm_21(i, k) * wmo_interm_15(k, i)
term(185) = term(185) + wmo_interm_16(i, k) * wmo_interm_28(i, k)
term(186) = term(186) + wmo_interm_16(i, k) * wmo_interm_29(i, k)
term(187) = term(187) + wmo_interm_16(i, k) * wmo_interm_28(i, k)
term(188) = term(188) + wmo_interm_16(i, k) * wmo_interm_29(i, k)
term(189) = term(189) + wmo_interm_7(i, k) * wmo_interm_17(i, k)
term(190) = term(190) + wmo_interm_6(i, k) * wmo_interm_17(i, k)
term(191) = term(191) + wmo_interm_20(i, k) * wmo_interm_8(k, i)
term(192) = term(192) + wmo_interm_21(i, k) * wmo_interm_8(k, i)
term(193) = term(193) + wmo_interm_20(i, k) * wmo_interm_9(k, i)
term(194) = term(194) + wmo_interm_21(i, k) * wmo_interm_9(k, i)
term(195) = term(195) + wmo_interm_20(i, k) * wmo_interm_9(k, i)
term(196) = term(196) + wmo_interm_21(i, k) * wmo_interm_9(k, i)
term(197) = term(197) + wmo_interm_20(i, k) * wmo_interm_8(k, i)
term(198) = term(198) + wmo_interm_21(i, k) * wmo_interm_8(k, i)
end do 
end do 

term(184) = term(184) * (-2.0d+0) 
term(186) = term(186) * (-2.0d+0) 
term(188) = term(188) * (-2.0d+0) 
term(189) = term(189) * (-2.0d+0) 
term(191) = term(191) * (-0.5d+0) 
term(194) = term(194) * (-2.0d+0) 
term(196) = term(196) * (-2.0d+0) 
term(197) = term(197) * (-0.5d+0) 

do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(199) = term(199) + wmo_interm_22(a, c) * wmo_interm_23(c, a)
term(200) = term(200) + wmo_interm_24(a, c) * wmo_interm_23(c, a)
term(201) = term(201) + wmo_interm_25(a, c) * wmo_interm_26(a, c)
term(202) = term(202) + wmo_interm_25(a, c) * wmo_interm_27(a, c)
term(203) = term(203) + wmo_interm_5(a, c) * wmo_interm_31(a, c)
term(204) = term(204) + wmo_interm_4(a, c) * wmo_interm_31(a, c)
term(205) = term(205) + wmo_interm_22(a, c) * wmo_interm_11(c, a)
term(206) = term(206) + wmo_interm_24(a, c) * wmo_interm_11(c, a)
term(207) = term(207) + wmo_interm_22(a, c) * wmo_interm_12(c, a)
term(208) = term(208) + wmo_interm_24(a, c) * wmo_interm_12(c, a)
end do 
end do 

term(200) = term(200) * (-2.0d+0) 
term(201) = term(201) * 2.0d+0 
term(202) = term(202) * (-4.0d+0) 
term(203) = term(203) * (-2.0d+0) 
term(205) = term(205) * 2.0d+0 
term(206) = term(206) * (-4.0d+0) 
term(207) = -term(207) 
term(208) = term(208) * 2.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(209) = term(209) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,i) * s1(b,j)
end do 
end do 
end do 
end do 

term(209) = term(209) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(210) = term(210) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 

term(210) = -term(210) 

do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(211) = term(211) + wmo_interm_4(b, d) * wmo_interm_26(b, d)
term(212) = term(212) + wmo_interm_4(b, d) * wmo_interm_27(b, d)
term(213) = term(213) + wmo_interm_5(b, d) * wmo_interm_26(b, d)
term(214) = term(214) + wmo_interm_5(b, d) * wmo_interm_27(b, d)
end do 
end do 

term(211) = -term(211) 
term(212) = term(212) * 2.0d+0 
term(213) = term(213) * 2.0d+0 
term(214) = term(214) * (-4.0d+0) 

do k = 1, nocc 
do c = nocc + 1, nactive 
term(215) = term(215) + wmo_interm_10(c, k) * wmo_interm_33(c, k)
term(216) = term(216) + wmo_interm_10(c, k) * wmo_interm_34(c, k)
term(217) = term(217) + wmo_interm_13(c, k) * wmo_interm_34(c, k)
term(218) = term(218) + wmo_interm_13(c, k) * wmo_interm_33(c, k)
end do 
end do 

term(215) = term(215) * 2.0d+0 
term(216) = term(216) * (-4.0d+0) 
term(217) = term(217) * 8.0d+0 
term(218) = term(218) * (-4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(219) = term(219) + wmo_interm_35(a, b) * wmo_interm_11(b, a)
term(220) = term(220) + wmo_interm_35(a, b) * wmo_interm_12(b, a)
end do 
end do 

term(219) = term(219) * (-4.0d+0) 
term(220) = term(220) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(221) = term(221) + s1(b,k) * wmo_interm_16(i, k) * wmo_interm_0(b, i)
term(222) = term(222) + s1(b,k) * wmo_interm_16(i, k) * wmo_interm_2(b, i)
end do 
end do 
end do 

term(221) = term(221) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(223) = term(223) + s1(c,k) * wmo_interm_16(i, k) * wmo_interm_2(c, i)
term(224) = term(224) + s1(c,k) * wmo_interm_16(i, k) * wmo_interm_0(c, i)
end do 
end do 
end do 

term(224) = term(224) * (-2.0d+0) 


    overlap_f = 0.d+0 
    do s = 0, 224
!          print*, 'overlap', s, term(s)
    overlap_f = overlap_f + term(s)
    end do
    ! print*, 'rl'
    ! do s = 1, size(vrdav_Rl, dim=1)
    !       print*, s, vrdav_Rl(s)
    ! end do
    ! print*, 'rr'
    ! do s = 1, size(vrdav_Rr, dim=1)
    !       print*, s, vrdav_Rr(s)
    ! end do


end function overlap_f
    
    

    



function calc_D_oo_wl(t2, t1, s2, s1, nocc, nactive, vldav, p,q) 
real(F64) :: calc_D_oo_wl
integer, intent(in) :: nocc, nactive
real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
real(F64), dimension(:), intent(in) :: vldav
integer, intent(in) :: p,q 
integer :: s ,b,i,a,j 
real(F64), dimension(0:3) :: term 
term = 0.d+0 
do b = nocc + 1, nactive 
      do i = 1, nocc 
            do a = nocc + 1, nactive 
                  term(0) = term(0) + l2(vldav, a,i,b,p) * t2(a,b,i,q)
            end do
      end do
end do

term(0) = term(0) * (-0.5d+0) 

do j = 1, nocc 
      do b = nocc + 1, nactive 
            do a = nocc + 1, nactive 
                  term(1) = term(1) + l2(vldav, a,p,b,j) * t2(a,b,q,j)
            end do
      end do
end do

term(1) = term(1) * (-0.5d+0) 

do a = nocc + 1, nactive 
      term(2) = term(2) + l1(vldav, a,p) * t1(a,q)
      term(3) = term(3) + l2(vldav, a,q,a,q) * t2(a,a,q,p)
end do

term(2) = -term(2) 
term(3) = term(3) * 0.5d+0 


calc_D_oo_wl = 0.d+0 
do s = 0, 3
      calc_D_oo_wl = calc_D_oo_wl + term(s)
end do

end function calc_D_oo_wl

function calc_D_vo_wl(t2, t1, s2, s1, nocc, nactive, vldav, p,q) 
real(F64) :: calc_D_vo_wl
integer, intent(in) :: nocc, nactive
real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
real(F64), dimension(:), intent(in) :: vldav
integer, intent(in) :: p,q 
integer :: s ,i,a,j,b 
real(F64), dimension(0:8) :: term 
term = 0.d+0 
do i = 1, nocc 
      do a = nocc + 1, nactive 
            term(0) = term(0) + l2(vldav, a,i,a,i) * t1(p,i) * t2(a,a,i,q)
            term(1) = term(1) + l2(vldav, a,i,a,i) * t1(a,q) * t2(a,p,i,i)
            term(2) = term(2) + l1(vldav, a,i) * t2(a,p,i,q)
            term(3) = term(3) + l1(vldav, a,i) * t2(a,p,q,i)
      end do
end do

term(0) = term(0) * 0.5d+0 
term(1) = term(1) * 0.5d+0 
term(2) = term(2) * 2.0d+0 
term(3) = -term(3) 

term(4) = term(4) + l1(vldav, p,q)


do j = 1, nocc 
      do b = nocc + 1, nactive 
            do i = 1, nocc 
                  do a = nocc + 1, nactive 
                        term(5) = term(5) + l2(vldav, a,i,b,j) * t1(p,j) * t2(a,b,i,q)
                        term(6) = term(6) + l2(vldav, a,i,b,j) * t1(p,i) * t2(a,b,q,j)
                        term(7) = term(7) + l2(vldav, a,i,b,j) * t1(b,q) * t2(a,p,i,j)
                  end do
            end do
      end do
end do

term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(7) = term(7) * (-0.5d+0) 

do j = 1, nocc 
      do i = 1, nocc 
            do b = nocc + 1, nactive 
                  do a = nocc + 1, nactive 
                        term(8) = term(8) + l2(vldav, a,i,b,j) * t1(a,q) * t2(b,p,j,i)
                  end do
            end do
      end do
end do

term(8) = term(8) * (-0.5d+0) 


calc_D_vo_wl = 0.d+0 
do s = 0, 8
      calc_D_vo_wl = calc_D_vo_wl + term(s)
end do

end function calc_D_vo_wl

function calc_D_vv_wl(t2, t1, s2, s1, nocc, nactive, vldav, p,q) 
real(F64) :: calc_D_vv_wl
integer, intent(in) :: nocc, nactive
real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
real(F64), dimension(:), intent(in) :: vldav
integer, intent(in) :: p,q 
integer :: s ,j,i,a,b 
real(F64), dimension(0:3) :: term 
term = 0.d+0 
do j = 1, nocc 
      do i = 1, nocc 
            do a = nocc + 1, nactive 
                  term(0) = term(0) + l2(vldav, a,i,p,j) * t2(a,q,i,j)
            end do
      end do
end do

term(0) = term(0) * 0.5d+0 

do j = 1, nocc 
      do i = 1, nocc 
            do b = nocc + 1, nactive 
                  term(1) = term(1) + l2(vldav, p,i,b,j) * t2(b,q,j,i)
            end do
      end do
end do

term(1) = term(1) * 0.5d+0 

do i = 1, nocc 
      term(2) = term(2) + l1(vldav, p,i) * t1(q,i)
      term(3) = term(3) + l2(vldav, q,i,q,i) * t2(q,p,i,i)
end do

term(3) = term(3) * (-0.5d+0) 


calc_D_vv_wl = 0.d+0 
do s = 0, 3
      calc_D_vv_wl = calc_D_vv_wl + term(s)
end do

end function calc_D_vv_wl




!*************************************************************************************************

    
!     function overlap_f(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr) 
!     double precision :: overlap_f
!     integer, intent(in) :: nocc, nactive
!     double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
!     double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
!     double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
!     double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
!     real(F64), dimension(:), intent(in) :: vrdav_Rl
!     real(F64), dimension(:), intent(in) :: vrdav_Rr 
!     integer :: s ,j,i,b,a,k,c,l,d 
!     double precision, dimension(0:202) :: term 

!     term = 0.d+0 
!     do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(0) = term(0) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,b,i)

! term(1) = term(1) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,a,i)

! end do 
! end do 
! end do 
! end do 

! term(0) = term(0) * (-0.5d+0) 


! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(2) = term(2) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,b,j)
! term(3) = term(3) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,b,j) * s1(b,j)
! term(4) = term(4) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i) * t1(b,j)
! term(5) = term(5) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,j) * t1(b,i)
! term(6) = term(6) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,i) * t1(a,j)
! term(7) = term(7) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,j) * t1(a,i)
! end do 
! end do 
! end do 
! end do 

! term(3) = term(3) * 2.0d+0 
! term(4) = term(4) * (-2.0d+0) 
! term(7) = term(7) * (-2.0d+0) 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! term(8) = term(8) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,a,j)
! end do 
! end do 
! end do 
! end do 

! term(8) = term(8) * (-0.5d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do l = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(9) = term(9) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,b,k) * s2(c,d,l,k) * t2(c,d,l,j)
! term(10) = term(10) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,b,k) * s2(c,d,l,k) * t2(a,d,l,j)
! term(11) = term(11) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,b,j) * s2(c,d,l,k) * t2(a,d,l,k)
! term(12) = term(12) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,d,k) * s2(c,d,l,k) * t2(a,b,l,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(9) = -term(9) 
! term(11) = term(11) * (-2.0d+0) 
! term(12) = term(12) * (-2.0d+0) 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(13) = term(13) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,i) * s1(b,j)
! end do 
! end do 
! end do 
! end do 

! term(13) = -term(13) 

! do j = 1, nocc 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(14) = term(14) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,i,a,j) * s1(b,j)
! end do 
! end do 
! end do 
! end do 

! term(14) = -term(14) 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! term(15) = term(15) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,a,i) * s1(b,j)
! end do 
! end do 
! end do 
! end do 

! term(15) = term(15) * 2.0d+0 

! do l = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(16) = term(16) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,c,i) * s2(c,d,l,k) * t2(b,d,k,l)
! term(17) = term(17) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,i) * s2(c,d,l,k) * t2(b,d,j,l)
! term(18) = term(18) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,l) * s2(c,d,l,k) * t2(a,d,j,i)
! term(19) = term(19) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,c,k) * s2(c,d,k,l) * t2(a,d,l,i)
! term(20) = term(20) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,c,k) * s2(c,d,k,l) * t2(b,d,l,i)
! term(21) = term(21) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,l) * s2(c,d,l,k) * t2(b,d,j,i)
! term(22) = term(22) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,c,i) * s2(c,d,l,k) * t2(a,d,k,l)
! term(23) = term(23) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,i) * s2(c,d,l,k) * t2(a,d,j,l)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(16) = term(16) * (-0.5d+0) 
! term(19) = term(19) * (-2.0d+0) 
! term(21) = term(21) * (-2.0d+0) 
! term(23) = term(23) * (-0.5d+0) 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(24) = term(24) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,i)
! end do 
! end do 

! term(24) = term(24) * 2.0d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(25) = term(25) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,a,k) * s2(b,c,k,j) * t1(c,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 


! do k = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(26) = term(26) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,a,k) * s2(b,c,j,k) * t1(c,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(26) = term(26) * (-2.0d+0) 

! do k = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(27) = term(27) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k) * s2(b,c,j,k) * t1(c,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 


! do k = 1, nocc 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(28) = term(28) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k) * s2(b,c,k,j) * t1(c,i)
! term(29) = term(29) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i) * s1(c,k) * t2(b,c,j,k)
! term(30) = term(30) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,j) * s1(c,k) * t2(b,c,i,k)
! term(31) = term(31) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,i) * s1(c,k) * t2(a,c,j,k)
! term(32) = term(32) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,j) * s1(c,k) * t2(a,c,i,k)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(28) = term(28) * (-2.0d+0) 
! term(29) = term(29) * 4.0d+0 
! term(30) = term(30) * (-2.0d+0) 
! term(31) = term(31) * (-2.0d+0) 
! term(32) = term(32) * 4.0d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(33) = term(33) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,i,c,j) * s2(b,c,j,k) * t1(a,k)
! term(34) = term(34) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j) * s2(b,c,k,j) * t2(b,c,i,k)
! term(35) = term(35) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,i) * s2(b,c,k,j) * t2(a,c,j,k)
! term(36) = term(36) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,j) * s2(b,c,k,j) * t2(a,c,i,k)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(35) = term(35) * 2.0d+0 
! term(36) = term(36) * (-4.0d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(37) = term(37) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,c,i) * s2(b,c,j,k) * t1(a,k)
! term(38) = term(38) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,j) * s2(b,c,j,k) * t2(a,c,k,i)
! term(39) = term(39) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j) * s2(b,c,j,k) * t2(b,c,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(37) = term(37) * (-2.0d+0) 
! term(38) = term(38) * (-4.0d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(40) = term(40) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,c,i) * s2(b,c,k,j) * t1(a,k)
! term(41) = term(41) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,j) * s2(b,c,k,j) * t2(a,c,k,i)
! term(42) = term(42) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j) * s2(b,c,k,j) * t2(b,c,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(41) = term(41) * 2.0d+0 
! term(42) = term(42) * (-2.0d+0) 

! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(43) = term(43) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,i,c,j) * s2(b,c,k,j) * t1(a,k)
! term(44) = term(44) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,i) * s2(b,c,k,j) * t2(a,c,k,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(43) = term(43) * (-2.0d+0) 
! term(44) = term(44) * (-4.0d+0) 

! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(45) = term(45) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i) * s1(c,k) * t2(b,c,k,j)
! term(46) = term(46) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k) * s1(c,k) * t2(b,c,i,j)
! term(47) = term(47) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,i) * s1(c,k) * t2(a,c,k,j)
! term(48) = term(48) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,k) * s1(c,k) * t2(a,c,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(45) = term(45) * (-2.0d+0) 
! term(48) = term(48) * (-2.0d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! term(49) = term(49) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,k) * s1(c,k) * t2(a,c,j,i)
! term(50) = term(50) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,j) * s1(c,k) * t2(a,c,k,i)
! term(51) = term(51) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,j) * s1(c,k) * t2(b,c,k,i)
! term(52) = term(52) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k) * s1(c,k) * t2(b,c,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(50) = term(50) * (-2.0d+0) 
! term(52) = term(52) * (-2.0d+0) 

! do l = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(53) = term(53) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,l) * s2(c,d,k,l) * t2(c,d,i,j)
! term(54) = term(54) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,j) * s2(c,d,k,l) * t2(c,d,i,l)
! term(55) = term(55) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,d,j) * s2(c,d,k,l) * t2(a,b,k,l)
! term(56) = term(56) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,l) * s2(c,d,k,l) * t2(a,d,i,j)
! term(57) = term(57) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,j) * s2(c,d,k,l) * t2(a,d,i,l)
! term(58) = term(58) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,d,j) * s2(c,d,k,l) * t2(a,b,i,l)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(53) = term(53) * 0.5d+0 
! term(54) = -term(54) 
! term(56) = term(56) * (-2.0d+0) 
! term(57) = term(57) * 4.0d+0 
! term(58) = term(58) * (-2.0d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(59) = term(59) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, c,j) * s1(c,k) * t2(a,b,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 


! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(60) = term(60) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, c,i) * s1(c,k) * t2(a,b,k,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(60) = term(60) * (-2.0d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(61) = term(61) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, c,i) * s1(c,k) * t2(a,b,j,k)
! term(62) = term(62) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, c,j) * s1(c,k) * t2(a,b,i,k)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(62) = term(62) * (-2.0d+0) 

! do k = 1, nocc 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(63) = term(63) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j) * s2(b,c,j,k) * t2(b,c,i,k)
! term(64) = term(64) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,j) * s2(b,c,j,k) * t2(a,c,i,k)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(63) = term(63) * (-2.0d+0) 
! term(64) = term(64) * 8.0d+0 

! do l = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! term(65) = term(65) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,l) * s2(c,d,k,l) * t2(c,d,i,j)
! term(66) = term(66) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,j) * s2(c,d,k,l) * t2(c,d,i,l)
! term(67) = term(67) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,l) * s2(c,d,k,l) * t2(b,d,i,j)
! term(68) = term(68) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,j) * s2(c,d,k,l) * t2(b,d,i,l)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(65) = term(65) * (-0.25d+0) 
! term(66) = term(66) * 0.5d+0 
! term(68) = term(68) * (-2.0d+0) 

! do l = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(69) = term(69) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,l) * s2(c,d,k,l) * t2(b,d,i,j)
! term(70) = term(70) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,j) * s2(c,d,k,l) * t2(b,d,i,l)
! term(71) = term(71) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,l) * s2(c,d,k,l) * t2(a,d,i,j)
! term(72) = term(72) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,j) * s2(c,d,k,l) * t2(a,d,i,l)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(69) = term(69) * (-0.5d+0) 
! term(72) = term(72) * (-2.0d+0) 

! do l = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(73) = term(73) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,i) * s2(c,d,l,k) * t2(c,d,j,l)
! term(74) = term(74) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,l) * s2(c,d,l,k) * t2(a,d,j,i)
! term(75) = term(75) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,b,k) * s2(c,d,k,l) * t2(a,d,l,i)
! term(76) = term(76) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,d,k) * s2(c,d,k,l) * t2(a,b,l,i)
! term(77) = term(77) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,b,i) * s2(c,d,l,k) * t2(a,d,k,l)
! term(78) = term(78) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,d,i) * s2(c,d,k,l) * t2(a,b,l,k)
! term(79) = term(79) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,d,i) * s2(c,d,l,k) * t2(a,b,j,l)
! term(80) = term(80) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,i) * s2(c,d,l,k) * t2(a,d,j,l)
! term(81) = term(81) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,l) * s2(c,d,l,k) * t2(c,d,j,i)
! term(82) = term(82) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,b,k) * s2(c,d,k,l) * t2(c,d,l,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(73) = term(73) * (-0.25d+0) 
! term(74) = term(74) * (-0.5d+0) 
! term(76) = term(76) * (-0.5d+0) 
! term(77) = term(77) * (-0.5d+0) 
! term(79) = term(79) * (-0.5d+0) 
! term(81) = term(81) * 0.5d+0 
! term(82) = term(82) * (-0.25d+0) 

! do l = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(83) = term(83) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,b,k) * s2(c,d,l,k) * t2(c,d,j,l)
! term(84) = term(84) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,b,k) * s2(c,d,k,l) * t2(c,d,j,l)
! term(85) = term(85) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,b,k) * s2(c,d,k,l) * t2(c,d,l,j)
! term(86) = term(86) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,b,k) * s2(c,d,l,k) * t2(c,d,i,l)
! term(87) = term(87) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,j) * s2(c,d,l,k) * t2(c,d,i,l)
! term(88) = term(88) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,l) * s2(c,d,l,k) * t2(c,d,i,j)
! term(89) = term(89) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,b,k) * s2(c,d,k,l) * t2(c,d,i,l)
! term(90) = term(90) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,b,j) * s2(c,d,l,k) * t2(a,d,k,l)
! term(91) = term(91) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,d,j) * s2(c,d,k,l) * t2(a,b,l,k)
! term(92) = term(92) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,b,k) * s2(c,d,k,l) * t2(a,d,l,j)
! term(93) = term(93) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,d,k) * s2(c,d,k,l) * t2(a,b,l,j)
! term(94) = term(94) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,b,k) * s2(c,d,k,l) * t2(a,d,j,l)
! term(95) = term(95) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,l) * s2(c,d,l,k) * t2(a,d,i,j)
! term(96) = term(96) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,b,k) * s2(c,d,k,l) * t2(a,d,i,l)
! term(97) = term(97) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,d,k) * s2(c,d,k,l) * t2(a,b,j,l)
! term(98) = term(98) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,d,k) * s2(c,d,k,l) * t2(a,b,i,l)
! term(99) = term(99) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,d,j) * s2(c,d,l,k) * t2(a,b,i,l)
! term(100) = term(100) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,d,k) * s2(c,d,l,k) * t2(a,b,j,l)
! term(101) = term(101) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,d,k) * s2(c,d,l,k) * t2(a,b,i,l)
! term(102) = term(102) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,b,k) * s2(c,d,l,k) * t2(a,d,j,l)
! term(103) = term(103) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,b,k) * s2(c,d,l,k) * t2(a,d,i,l)
! term(104) = term(104) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,j) * s2(c,d,l,k) * t2(a,d,i,l)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(83) = term(83) * 0.5d+0 
! term(84) = -term(84) 
! term(85) = term(85) * 0.5d+0 
! term(86) = term(86) * (-0.25d+0) 
! term(87) = term(87) * 0.5d+0 
! term(88) = term(88) * (-0.25d+0) 
! term(89) = term(89) * 0.5d+0 
! term(91) = term(91) * (-0.5d+0) 
! term(92) = term(92) * (-0.5d+0) 
! term(96) = term(96) * (-2.0d+0) 
! term(97) = term(97) * (-0.5d+0) 
! term(101) = term(101) * (-2.0d+0) 
! term(102) = term(102) * (-0.5d+0) 
! term(104) = term(104) * (-2.0d+0) 

! do l = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(105) = term(105) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,i) * s2(c,d,k,l) * t2(c,d,j,l)
! term(106) = term(106) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,i) * s2(c,d,k,l) * t2(c,d,l,j)
! term(107) = term(107) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,l) * s2(c,d,k,l) * t2(a,d,j,i)
! term(108) = term(108) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,j) * s2(c,d,k,l) * t2(a,d,l,i)
! term(109) = term(109) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,d,j) * s2(c,d,k,l) * t2(a,b,l,i)
! term(110) = term(110) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,d,i) * s2(c,d,k,l) * t2(a,b,k,l)
! term(111) = term(111) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,i) * s2(c,d,k,l) * t2(a,d,l,j)
! term(112) = term(112) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,d,i) * s2(c,d,k,l) * t2(a,b,l,j)
! term(113) = term(113) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,i) * s2(c,d,k,l) * t2(a,d,j,l)
! term(114) = term(114) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,d,i) * s2(c,d,k,l) * t2(a,b,j,l)
! term(115) = term(115) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,l) * s2(c,d,k,l) * t2(c,d,j,i)
! term(116) = term(116) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,j) * s2(c,d,k,l) * t2(c,d,l,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(105) = term(105) * 0.5d+0 
! term(106) = term(106) * (-0.25d+0) 
! term(108) = term(108) * (-2.0d+0) 
! term(110) = term(110) * (-0.5d+0) 
! term(112) = term(112) * (-2.0d+0) 
! term(113) = term(113) * (-2.0d+0) 
! term(115) = term(115) * (-0.25d+0) 
! term(116) = term(116) * 0.5d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do l = 1, nocc 
! do a = nocc + 1, nactive 
! term(117) = term(117) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,i) * s2(c,d,l,k) * t2(b,d,l,j)
! term(118) = term(118) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,c,i) * s2(c,d,l,k) * t2(b,d,l,k)
! term(119) = term(119) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,c,k) * s2(c,d,l,k) * t2(b,d,l,i)
! term(120) = term(120) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,j) * s2(c,d,l,k) * t2(b,d,l,i)
! term(121) = term(121) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,j) * s2(c,d,l,k) * t2(a,d,l,i)
! term(122) = term(122) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,c,k) * s2(c,d,l,k) * t2(a,d,l,i)
! term(123) = term(123) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,i) * s2(c,d,l,k) * t2(a,d,l,j)
! term(124) = term(124) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,c,i) * s2(c,d,l,k) * t2(a,d,l,k)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(117) = term(117) * (-0.5d+0) 
! term(119) = term(119) * (-0.5d+0) 
! term(121) = term(121) * (-0.5d+0) 
! term(124) = term(124) * (-2.0d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do l = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(125) = term(125) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,c,k) * s2(c,d,l,k) * t2(b,d,l,j)
! term(126) = term(126) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,c,j) * s2(c,d,l,k) * t2(b,d,l,k)
! term(127) = term(127) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,c,k) * s2(c,d,l,k) * t2(a,d,l,j)
! term(128) = term(128) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,c,j) * s2(c,d,l,k) * t2(a,d,l,k)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(126) = term(126) * (-2.0d+0) 
! term(127) = term(127) * (-0.5d+0) 

! do l = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! term(129) = term(129) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,i) * s2(c,d,k,l) * t2(b,d,l,j)
! term(130) = term(130) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,i) * s2(c,d,k,l) * t2(b,d,j,l)
! term(131) = term(131) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,l) * s2(c,d,k,l) * t2(a,d,j,i)
! term(132) = term(132) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,j) * s2(c,d,k,l) * t2(a,d,l,i)
! term(133) = term(133) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,j) * s2(c,d,k,l) * t2(b,d,l,i)
! term(134) = term(134) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,l) * s2(c,d,k,l) * t2(b,d,j,i)
! term(135) = term(135) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,i) * s2(c,d,k,l) * t2(a,d,l,j)
! term(136) = term(136) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,i) * s2(c,d,k,l) * t2(a,d,j,l)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(130) = term(130) * (-2.0d+0) 
! term(131) = term(131) * (-0.5d+0) 
! term(133) = term(133) * (-0.5d+0) 
! term(135) = term(135) * (-0.5d+0) 

! do l = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(137) = term(137) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,c,k) * s2(c,d,k,l) * t2(b,d,l,j)
! term(138) = term(138) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,c,j) * s2(c,d,l,k) * t2(b,d,k,l)
! term(139) = term(139) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,c,k) * s2(c,d,k,l) * t2(b,d,j,l)
! term(140) = term(140) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,c,k) * s2(c,d,l,k) * t2(b,d,j,l)
! term(141) = term(141) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,l) * s2(c,d,l,k) * t2(b,d,i,j)
! term(142) = term(142) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,c,k) * s2(c,d,k,l) * t2(b,d,i,l)
! term(143) = term(143) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,j) * s2(c,d,l,k) * t2(b,d,i,l)
! term(144) = term(144) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,c,k) * s2(c,d,l,k) * t2(b,d,i,l)
! term(145) = term(145) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,c,j) * s2(c,d,l,k) * t2(a,d,k,l)
! term(146) = term(146) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,c,k) * s2(c,d,k,l) * t2(a,d,l,j)
! term(147) = term(147) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,c,k) * s2(c,d,k,l) * t2(a,d,j,l)
! term(148) = term(148) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,l) * s2(c,d,l,k) * t2(a,d,i,j)
! term(149) = term(149) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,c,k) * s2(c,d,k,l) * t2(a,d,i,l)
! term(150) = term(150) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,c,j) * s2(c,d,l,k) * t2(a,d,i,l)
! term(151) = term(151) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,c,k) * s2(c,d,l,k) * t2(a,d,j,l)
! term(152) = term(152) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,c,k) * s2(c,d,l,k) * t2(a,d,i,l)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(137) = term(137) * (-2.0d+0) 
! term(139) = term(139) * 4.0d+0 
! term(140) = term(140) * (-2.0d+0) 
! term(142) = term(142) * (-2.0d+0) 
! term(143) = term(143) * (-0.5d+0) 
! term(145) = term(145) * (-0.5d+0) 
! term(147) = term(147) * (-2.0d+0) 
! term(148) = term(148) * (-2.0d+0) 
! term(149) = term(149) * 4.0d+0 
! term(152) = term(152) * (-2.0d+0) 

! do l = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! term(153) = term(153) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,a,k) * s2(c,d,l,k) * t2(c,d,j,l)
! term(154) = term(154) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,a,k) * s2(c,d,k,l) * t2(c,d,j,l)
! term(155) = term(155) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,a,k) * s2(c,d,k,l) * t2(b,d,l,j)
! term(156) = term(156) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,a,k) * s2(c,d,k,l) * t2(c,d,l,j)
! term(157) = term(157) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,a,j) * s2(c,d,l,k) * t2(b,d,k,l)
! term(158) = term(158) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,a,k) * s2(c,d,k,l) * t2(b,d,j,l)
! term(159) = term(159) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,a,k) * s2(c,d,l,k) * t2(b,d,j,l)
! term(160) = term(160) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,j) * s2(c,d,l,k) * t2(c,d,i,l)
! term(161) = term(161) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,a,k) * s2(c,d,l,k) * t2(c,d,i,l)
! term(162) = term(162) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,l) * s2(c,d,l,k) * t2(c,d,i,j)
! term(163) = term(163) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,a,k) * s2(c,d,k,l) * t2(c,d,i,l)
! term(164) = term(164) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,l) * s2(c,d,l,k) * t2(b,d,i,j)
! term(165) = term(165) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,a,k) * s2(c,d,k,l) * t2(b,d,i,l)
! term(166) = term(166) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,a,k) * s2(c,d,l,k) * t2(b,d,i,l)
! term(167) = term(167) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,j) * s2(c,d,l,k) * t2(b,d,i,l)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(153) = term(153) * (-0.25d+0) 
! term(154) = term(154) * 0.5d+0 
! term(156) = term(156) * (-0.25d+0) 
! term(157) = term(157) * (-0.5d+0) 
! term(158) = term(158) * (-2.0d+0) 
! term(160) = term(160) * (-0.25d+0) 
! term(161) = term(161) * 0.5d+0 
! term(162) = term(162) * 0.5d+0 
! term(163) = -term(163) 
! term(164) = term(164) * (-0.5d+0) 
! term(166) = term(166) * (-0.5d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do l = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! term(168) = term(168) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,a,k) * s2(c,d,l,k) * t2(b,d,l,j)
! term(169) = term(169) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,i,a,k) * s2(c,d,l,k) * t2(c,d,l,j)
! term(170) = term(170) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,i,a,j) * s2(c,d,l,k) * t2(b,d,l,k)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(168) = term(168) * (-0.5d+0) 
! term(169) = term(169) * 0.5d+0 

! do l = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! term(171) = term(171) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,i) * s2(c,d,l,k) * t2(c,d,j,l)
! term(172) = term(172) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,a,i) * s2(c,d,l,k) * t2(b,d,k,l)
! term(173) = term(173) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,i) * s2(c,d,l,k) * t2(b,d,j,l)
! term(174) = term(174) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,a,k) * s2(c,d,k,l) * t2(b,d,l,i)
! term(175) = term(175) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,l) * s2(c,d,l,k) * t2(b,d,j,i)
! term(176) = term(176) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,l) * s2(c,d,l,k) * t2(c,d,j,i)
! term(177) = term(177) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,a,k) * s2(c,d,k,l) * t2(c,d,l,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(171) = term(171) * 0.5d+0 
! term(173) = term(173) * (-2.0d+0) 
! term(174) = term(174) * (-0.5d+0) 
! term(176) = term(176) * (-0.25d+0) 
! term(177) = term(177) * 0.5d+0 

! do l = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do k = 1, nocc 
! do c = nocc + 1, nactive 
! term(178) = term(178) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,i) * s2(c,d,k,l) * t2(c,d,j,l)
! term(179) = term(179) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,i) * s2(c,d,k,l) * t2(b,d,l,j)
! term(180) = term(180) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,i) * s2(c,d,k,l) * t2(c,d,l,j)
! term(181) = term(181) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,i) * s2(c,d,k,l) * t2(b,d,j,l)
! term(182) = term(182) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,j) * s2(c,d,k,l) * t2(b,d,l,i)
! term(183) = term(183) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,l) * s2(c,d,k,l) * t2(b,d,j,i)
! term(184) = term(184) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,l) * s2(c,d,k,l) * t2(c,d,j,i)
! term(185) = term(185) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,j) * s2(c,d,k,l) * t2(c,d,l,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(178) = -term(178) 
! term(179) = term(179) * (-2.0d+0) 
! term(180) = term(180) * 0.5d+0 
! term(181) = term(181) * 4.0d+0 
! term(183) = term(183) * (-2.0d+0) 
! term(184) = term(184) * 0.5d+0 
! term(185) = term(185) * (-0.25d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do l = 1, nocc 
! do c = nocc + 1, nactive 
! term(186) = term(186) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,i) * s2(c,d,l,k) * t2(b,d,l,j)
! term(187) = term(187) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,i) * s2(c,d,l,k) * t2(c,d,l,j)
! term(188) = term(188) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,a,i) * s2(c,d,l,k) * t2(b,d,l,k)
! term(189) = term(189) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,a,j) * s2(c,d,l,k) * t2(b,d,l,i)
! term(190) = term(190) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,a,k) * s2(c,d,l,k) * t2(b,d,l,i)
! term(191) = term(191) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,a,j) * s2(c,d,l,k) * t2(c,d,l,i)
! term(192) = term(192) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,j,a,k) * s2(c,d,l,k) * t2(c,d,l,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(187) = -term(187) 
! term(188) = term(188) * (-2.0d+0) 
! term(189) = term(189) * (-0.5d+0) 
! term(191) = term(191) * 0.5d+0 
! term(192) = -term(192) 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do l = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(193) = term(193) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,i) * s2(c,d,l,k) * t2(c,d,l,j)
! term(194) = term(194) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,b,k) * s2(c,d,l,k) * t2(a,d,l,i)
! term(195) = term(195) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,j) * s2(c,d,l,k) * t2(a,d,l,i)
! term(196) = term(196) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,d,j) * s2(c,d,l,k) * t2(a,b,l,i)
! term(197) = term(197) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,d,k) * s2(c,d,l,k) * t2(a,b,l,i)
! term(198) = term(198) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,b,i) * s2(c,d,l,k) * t2(a,d,l,j)
! term(199) = term(199) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,j,b,i) * s2(c,d,l,k) * t2(a,d,l,k)
! term(200) = term(200) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, c,k,d,i) * s2(c,d,l,k) * t2(a,b,l,j)
! term(201) = term(201) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,b,k) * s2(c,d,l,k) * t2(c,d,l,i)
! term(202) = term(202) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,j) * s2(c,d,l,k) * t2(c,d,l,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 
! end do 

! term(193) = term(193) * 0.5d+0 
! term(194) = term(194) * (-0.5d+0) 
! term(196) = term(196) * (-0.5d+0) 
! term(198) = term(198) * (-0.5d+0) 
! term(201) = term(201) * 0.5d+0 
! term(202) = -term(202) 


!     overlap_f = 0.d+0 
!     do s = 0, 202
!           overlap_f = overlap_f + term(s)
!     end do

!     end function overlap_f
    



end module density_exc_exc_functions
