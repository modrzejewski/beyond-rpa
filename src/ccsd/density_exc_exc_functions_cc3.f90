module density_exc_exc_functions_cc3

      use cc3_intermediates
      use arithmetic
      use s_gen
      use basis
      use eom_vectors
      
      implicit none

      !
      ! File generated automatically on 2015-05-15 12:46:42
      !
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_0 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_1 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_2 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_3 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_4 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_5 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_6 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_7 
      real(F64) :: wm_cc3_interm_8 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_9 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_10 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_11 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_12 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_13 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_14 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_15 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_16 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_17 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_18 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_19 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_20 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_21 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_22 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_23 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_24 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_25 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_26 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_27 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_28 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_29 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_30 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_31 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_32 
      real(F64), dimension(:, :), allocatable :: wm_cc3_interm_33 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_34 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_35 
      real(F64), dimension(:, :, :, :), allocatable :: wm_cc3_interm_36 

contains

      subroutine wm_intermediates_cc3_init(nocc, nactive)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive

                        allocate(wm_cc3_interm_0(1: nocc, 1: nocc))
            allocate(wm_cc3_interm_1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_4(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_5(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_6(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_7(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_9(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_10(nocc+1: nactive, nocc+1: nactive))
            allocate(wm_cc3_interm_11(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_12(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_13(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_14(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_15(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_16(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_17(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_18(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_19(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_20(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_21(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_22(nocc+1: nactive, nocc+1: nactive))
            allocate(wm_cc3_interm_23(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_24(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_25(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_26(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_27(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_28(nocc+1: nactive, 1: nocc))
            allocate(wm_cc3_interm_29(1: nocc, 1: nocc))
            allocate(wm_cc3_interm_30(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_31(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_32(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_33(nocc+1: nactive, nocc+1: nactive))
            allocate(wm_cc3_interm_34(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_35(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
            allocate(wm_cc3_interm_36(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))


            wm_cc3_interm_0 = zero 
            wm_cc3_interm_1 = zero 
            wm_cc3_interm_2 = zero 
            wm_cc3_interm_3 = zero 
            wm_cc3_interm_4 = zero 
            wm_cc3_interm_5 = zero 
            wm_cc3_interm_6 = zero 
            wm_cc3_interm_7 = zero 
            wm_cc3_interm_8 = zero 
            wm_cc3_interm_9 = zero 
            wm_cc3_interm_10 = zero 
            wm_cc3_interm_11 = zero 
            wm_cc3_interm_12 = zero 
            wm_cc3_interm_13 = zero 
            wm_cc3_interm_14 = zero 
            wm_cc3_interm_15 = zero 
            wm_cc3_interm_16 = zero 
            wm_cc3_interm_17 = zero 
            wm_cc3_interm_18 = zero 
            wm_cc3_interm_19 = zero 
            wm_cc3_interm_20 = zero 
            wm_cc3_interm_21 = zero 
            wm_cc3_interm_22 = zero 
            wm_cc3_interm_23 = zero 
            wm_cc3_interm_24 = zero 
            wm_cc3_interm_25 = zero 
            wm_cc3_interm_26 = zero 
            wm_cc3_interm_27 = zero 
            wm_cc3_interm_28 = zero 
            wm_cc3_interm_29 = zero 
            wm_cc3_interm_30 = zero 
            wm_cc3_interm_31 = zero 
            wm_cc3_interm_32 = zero 
            wm_cc3_interm_33 = zero 
            wm_cc3_interm_34 = zero 
            wm_cc3_interm_35 = zero 
            wm_cc3_interm_36 = zero 


      end subroutine wm_intermediates_cc3_init


      subroutine wm_intermediates_cc3_free
            deallocate(wm_cc3_interm_0)
            deallocate(wm_cc3_interm_1)
            deallocate(wm_cc3_interm_2)
            deallocate(wm_cc3_interm_3)
            deallocate(wm_cc3_interm_4)
            deallocate(wm_cc3_interm_5)
            deallocate(wm_cc3_interm_6)
            deallocate(wm_cc3_interm_7)

            deallocate(wm_cc3_interm_9)
            deallocate(wm_cc3_interm_10)
            deallocate(wm_cc3_interm_11)
            deallocate(wm_cc3_interm_12)
            deallocate(wm_cc3_interm_13)
            deallocate(wm_cc3_interm_14)
            deallocate(wm_cc3_interm_15)
            deallocate(wm_cc3_interm_16)
            deallocate(wm_cc3_interm_17)
            deallocate(wm_cc3_interm_18)
            deallocate(wm_cc3_interm_19)
            deallocate(wm_cc3_interm_20)
            deallocate(wm_cc3_interm_21)
            deallocate(wm_cc3_interm_22)
            deallocate(wm_cc3_interm_23)
            deallocate(wm_cc3_interm_24)
            deallocate(wm_cc3_interm_25)
            deallocate(wm_cc3_interm_26)
            deallocate(wm_cc3_interm_27)
            deallocate(wm_cc3_interm_28)
            deallocate(wm_cc3_interm_29)
            deallocate(wm_cc3_interm_30)
            deallocate(wm_cc3_interm_31)
            deallocate(wm_cc3_interm_32)
            deallocate(wm_cc3_interm_33)
            deallocate(wm_cc3_interm_34)
            deallocate(wm_cc3_interm_35)
            deallocate(wm_cc3_interm_36)

      end subroutine wm_intermediates_cc3_free


      subroutine wm_intermediates_cc3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr) 

        integer, intent(in) :: nocc, nactive
        real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
        real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
        real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
        real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
        double precision, dimension(:), intent(in) :: vrdav_Rl
        double precision, dimension(:), intent(in) :: vrdav_Rr
        integer :: a, i, j, b, k, c, l 
        real(F64) :: sum


            !$omp parallel private(a, i, j, sum)& 
            !$omp default(shared)
            !$omp do collapse(2)
            do i = 1, nocc 
                  do j = 1, nocc 
                        sum = zero 
                        do a = nocc + 1, nactive 
                              sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
                        end do
                        wm_cc3_interm_0(i, j) = wm_cc3_interm_0(i, j) + sum 
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
                                    wm_cc3_interm_1(c, j, k, l) = wm_cc3_interm_1(c, j, k, l) + sum 
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
                                    wm_cc3_interm_2(c, k, j, l) = wm_cc3_interm_2(c, k, j, l) + sum 
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
                                    wm_cc3_interm_3(c, k, j, l) = wm_cc3_interm_3(c, k, j, l) + sum 
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
                        wm_cc3_interm_4(b, j) = wm_cc3_interm_4(b, j) + sum 
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
                                    wm_cc3_interm_5(b, i, j, k) = wm_cc3_interm_5(b, i, j, k) + sum 
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
                                    sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,i,j)
                              end do
                        end do
                        wm_cc3_interm_6(b, j) = wm_cc3_interm_6(b, j) + sum 
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
                                    wm_cc3_interm_7(c, j, k, l) = wm_cc3_interm_7(c, j, k, l) + sum 
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
            wm_cc3_interm_8 = wm_cc3_interm_8 + sum 

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
                        wm_cc3_interm_9(c, k) = wm_cc3_interm_9(c, k) + sum 
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
                        wm_cc3_interm_10(a, b) = wm_cc3_interm_10(a, b) + sum 
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
                                    wm_cc3_interm_11(c, j, l, k) = wm_cc3_interm_11(c, j, l, k) + sum 
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
                                                      sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,l,i)
                                                end do
                                          end do
                                    end do
                                    wm_cc3_interm_12(c, j, k, l) = wm_cc3_interm_12(c, j, k, l) + sum 
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
                                    wm_cc3_interm_13(c, j, k, l) = wm_cc3_interm_13(c, j, k, l) + sum 
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
                                    wm_cc3_interm_14(c, j, k, l) = wm_cc3_interm_14(c, j, k, l) + sum 
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
                        wm_cc3_interm_15(c, k) = wm_cc3_interm_15(c, k) + sum 
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
                                    wm_cc3_interm_16(c, j, l, k) = wm_cc3_interm_16(c, j, l, k) + sum 
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
                        wm_cc3_interm_17(b, j) = wm_cc3_interm_17(b, j) + sum 
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
                                    wm_cc3_interm_18(b, i, j, k) = wm_cc3_interm_18(b, i, j, k) + sum 
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
                                    sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,i,j)
                              end do
                        end do
                        wm_cc3_interm_19(b, j) = wm_cc3_interm_19(b, j) + sum 
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
                        wm_cc3_interm_20(c, k) = wm_cc3_interm_20(c, k) + sum 
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
                        wm_cc3_interm_21(c, k) = wm_cc3_interm_21(c, k) + sum 
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
                        wm_cc3_interm_22(a, b) = wm_cc3_interm_22(a, b) + sum 
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
                                                sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
                                          end do
                                    end do
                              end do
                        end do
                        wm_cc3_interm_23(c, k) = wm_cc3_interm_23(c, k) + sum 
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
                                                sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
                                          end do
                                    end do
                              end do
                        end do
                        wm_cc3_interm_24(c, k) = wm_cc3_interm_24(c, k) + sum 
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
                                                sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,k,i)
                                          end do
                                    end do
                              end do
                        end do
                        wm_cc3_interm_25(c, k) = wm_cc3_interm_25(c, k) + sum 
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
                                                sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,k,j)
                                          end do
                                    end do
                              end do
                        end do
                        wm_cc3_interm_26(c, k) = wm_cc3_interm_26(c, k) + sum 
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
                                                sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
                                          end do
                                    end do
                              end do
                        end do
                        wm_cc3_interm_27(c, k) = wm_cc3_interm_27(c, k) + sum 
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
                                                sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
                                          end do
                                    end do
                              end do
                        end do
                        wm_cc3_interm_28(c, k) = wm_cc3_interm_28(c, k) + sum 
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
                        wm_cc3_interm_29(i, j) = wm_cc3_interm_29(i, j) + sum 
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
                                                      sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
                                                end do
                                          end do
                                    end do
                                    wm_cc3_interm_30(c, j, k, l) = wm_cc3_interm_30(c, j, k, l) + sum 
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
                                                      sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
                                                end do
                                          end do
                                    end do
                                    wm_cc3_interm_31(c, j, k, l) = wm_cc3_interm_31(c, j, k, l) + sum 
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
                                                      sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
                                                end do
                                          end do
                                    end do
                                    wm_cc3_interm_32(c, j, k, l) = wm_cc3_interm_32(c, j, k, l) + sum 
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
                              sum = sum + r1(vrdav_Rr, a,i) * s1(b,i)
                        end do
                        wm_cc3_interm_33(a, b) = wm_cc3_interm_33(a, b) + sum 
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
                                                      sum = sum + t3(nocc, nactive, a,b,c,i,j,k) * t2(a,b,l,i)
                                                end do
                                          end do
                                    end do
                                    wm_cc3_interm_34(c, j, k, l) = wm_cc3_interm_34(c, j, k, l) + sum 
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
                                                      sum = sum + t3(nocc, nactive, a,b,c,j,i,k) * t2(a,b,l,i)
                                                end do
                                          end do
                                    end do
                                    wm_cc3_interm_35(c, j, k, l) = wm_cc3_interm_35(c, j, k, l) + sum 
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
                                                      sum = sum + t3(nocc, nactive, a,b,c,k,j,i) * t2(a,b,l,i)
                                                end do
                                          end do
                                    end do
                                    wm_cc3_interm_36(c, k, j, l) = wm_cc3_interm_36(c, k, j, l) + sum 
                              end do
                        end do
                  end do
            end do
            !$omp end do nowait 
            !$omp end parallel 
      end subroutine wm_intermediates_cc3


    
    function calc_D_oo_wm_cc3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_cc3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, b, c, a, k, d 
    real(F64), dimension(0:101) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,j,p,i) * t1(a,q) * wm_cc3_interm_4(c, j)
term(1) = term(1) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,j,p,i) * t1(a,q) * wm_cc3_interm_6(c, j)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * 4.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(2) = term(2) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,j,i,p) * t1(a,q) * wm_cc3_interm_4(c, j)
term(3) = term(3) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,p,i,j) * t1(a,q) * wm_cc3_interm_4(c, j)
term(4) = term(4) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,p,i,j) * t1(a,q) * wm_cc3_interm_6(c, j)
term(5) = term(5) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,j,i,p) * t1(a,q) * wm_cc3_interm_6(c, j)
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * 3.9999999999999996d+0 
term(3) = term(3) * (-7.999999999999999d+0) 
term(4) = term(4) * 15.999999999999998d+0 
term(5) = term(5) * (-8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(6) = term(6) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,i,j,p) * t1(a,q) * wm_cc3_interm_4(c, j)
term(7) = term(7) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,p,j,i) * t1(a,q) * wm_cc3_interm_4(c, j)
term(8) = term(8) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,i,j,p) * t1(a,q) * wm_cc3_interm_6(c, j)
term(9) = term(9) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,p,j,i) * t1(a,q) * wm_cc3_interm_6(c, j)
end do 
end do 
end do 
end do 
end do 

term(6) = term(6) * (-1.9999999999999998d+0) 
term(7) = term(7) * 3.9999999999999996d+0 
term(8) = term(8) * 4.0d+0 
term(9) = term(9) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(10) = term(10) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,i,p,j) * t1(a,q) * wm_cc3_interm_4(c, j)
term(11) = term(11) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,i,p,j) * t1(a,q) * wm_cc3_interm_6(c, j)
end do 
end do 
end do 
end do 
end do 

term(10) = term(10) * 3.9999999999999996d+0 
term(11) = term(11) * (-7.999999999999999d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(12) = term(12) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,p,i,j) * t1(c,k) * wm_cc3_interm_5(b, k, j, q)
term(13) = term(13) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,j,i,p) * t1(c,k) * wm_cc3_interm_5(b, k, j, q)
term(14) = term(14) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,p,i,j) * t1(c,k) * wm_cc3_interm_5(b, k, q, j)
term(15) = term(15) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,j,i,p) * t1(c,k) * wm_cc3_interm_5(b, k, q, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(12) = term(12) * 3.9999999999999996d+0 
term(13) = term(13) * (-1.9999999999999998d+0) 
term(14) = term(14) * (-1.9999999999999998d+0) 
term(15) = term(15) * 3.9999999999999996d+0 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,j,p,i) * t1(c,k) * wm_cc3_interm_5(b, k, j, q)
term(17) = term(17) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,j,p,i) * t1(c,k) * wm_cc3_interm_5(b, k, q, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(16) = term(16) * 3.9999999999999996d+0 
term(17) = term(17) * (-1.9999999999999998d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(18) = term(18) + t1(a,i) * wm_cc3_interm_0(i, j) * wm_cc3_interm_1(a, j, p, q)
term(19) = term(19) + t1(a,i) * wm_cc3_interm_0(i, j) * wm_cc3_interm_2(a, j, p, q)
term(20) = term(20) + t1(a,i) * wm_cc3_interm_0(i, j) * wm_cc3_interm_2(a, p, j, q)
term(21) = term(21) + t1(a,i) * wm_cc3_interm_0(i, j) * wm_cc3_interm_3(a, p, j, q)
term(22) = term(22) + t1(a,i) * wm_cc3_interm_0(i, j) * wm_cc3_interm_1(a, p, j, q)
term(23) = term(23) + t1(a,i) * wm_cc3_interm_0(i, j) * wm_cc3_interm_3(a, j, p, q)
term(24) = term(24) + r1(vrdav_Rr, a,i) * wm_cc3_interm_2(a, j, p, q) * wm_cc3_interm_29(i, j)
term(25) = term(25) + r1(vrdav_Rr, a,i) * wm_cc3_interm_2(a, p, j, q) * wm_cc3_interm_29(i, j)
term(26) = term(26) + r1(vrdav_Rr, a,j) * wm_cc3_interm_2(a, i, j, q) * wm_cc3_interm_29(p, i)
term(27) = term(27) + r1(vrdav_Rr, a,j) * wm_cc3_interm_29(p, i) * wm_cc3_interm_3(a, i, j, q)
term(28) = term(28) + r1(vrdav_Rr, a,i) * wm_cc3_interm_29(i, j) * wm_cc3_interm_3(a, j, p, q)
term(29) = term(29) + r1(vrdav_Rr, a,i) * wm_cc3_interm_1(a, p, j, q) * wm_cc3_interm_29(i, j)
term(30) = term(30) + r1(vrdav_Rr, a,i) * wm_cc3_interm_1(a, j, p, q) * wm_cc3_interm_29(i, j)
term(31) = term(31) + r1(vrdav_Rr, a,i) * wm_cc3_interm_29(i, j) * wm_cc3_interm_3(a, p, j, q)
term(32) = term(32) + r1(vrdav_Rr, a,j) * wm_cc3_interm_1(a, i, j, q) * wm_cc3_interm_29(p, i)
term(33) = term(33) + t1(a,i) * wm_cc3_interm_0(i, j) * wm_cc3_interm_36(a, p, j, q)
term(34) = term(34) + t1(a,i) * wm_cc3_interm_0(i, j) * wm_cc3_interm_36(a, j, p, q)
term(35) = term(35) + r1(vrdav_Rr, a,i) * wm_cc3_interm_29(i, j) * wm_cc3_interm_36(a, p, j, q)
term(36) = term(36) + r1(vrdav_Rr, a,j) * wm_cc3_interm_29(p, i) * wm_cc3_interm_36(a, i, j, q)
term(37) = term(37) + r1(vrdav_Rr, a,i) * wm_cc3_interm_29(i, j) * wm_cc3_interm_36(a, j, p, q)
term(38) = term(38) + t1(a,q) * wm_cc3_interm_29(i, j) * wm_cc3_interm_30(a, i, p, j)
term(39) = term(39) + t1(a,q) * wm_cc3_interm_29(i, j) * wm_cc3_interm_30(a, i, j, p)
term(40) = term(40) + t1(a,q) * wm_cc3_interm_29(i, j) * wm_cc3_interm_31(a, i, j, p)
term(41) = term(41) + t1(a,q) * wm_cc3_interm_29(i, j) * wm_cc3_interm_32(a, i, j, p)
term(42) = term(42) + t1(a,q) * wm_cc3_interm_29(i, j) * wm_cc3_interm_31(a, i, p, j)
term(43) = term(43) + t1(a,q) * wm_cc3_interm_29(i, j) * wm_cc3_interm_32(a, i, p, j)
end do 
end do 
end do 

term(18) = -term(18) 
term(19) = term(19) * 4.0d+0 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * 4.0d+0 
term(22) = term(22) * 2.0d+0 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * 4.0d+0 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * 4.0d+0 
term(28) = term(28) * (-8.0d+0) 
term(29) = term(29) * 2.0d+0 
term(30) = -term(30) 
term(31) = term(31) * 4.0d+0 
term(32) = term(32) * 2.0d+0 
term(33) = -term(33) 
term(34) = term(34) * 2.0d+0 
term(35) = -term(35) 
term(36) = -term(36) 
term(37) = term(37) * 2.0d+0 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * 4.0d+0 
term(40) = term(40) * 4.0d+0 
term(41) = term(41) * (-8.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(44) = term(44) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,i,p,j) * t1(c,k) * wm_cc3_interm_5(b, k, j, q)
term(45) = term(45) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,i,p,j) * t1(c,k) * wm_cc3_interm_5(b, k, q, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(44) = term(44) * (-7.999999999999999d+0) 
term(45) = term(45) * 3.9999999999999996d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(46) = term(46) + t1(a,i) * wm_cc3_interm_23(a, p) * wm_cc3_interm_29(i, q)
term(47) = term(47) + t1(a,i) * wm_cc3_interm_24(a, p) * wm_cc3_interm_29(i, q)
term(48) = term(48) + t1(a,i) * wm_cc3_interm_25(a, p) * wm_cc3_interm_29(i, q)
term(49) = term(49) + t1(a,i) * wm_cc3_interm_26(a, p) * wm_cc3_interm_29(i, q)
term(50) = term(50) + t1(a,i) * wm_cc3_interm_27(a, p) * wm_cc3_interm_29(i, q)
term(51) = term(51) + t1(a,i) * wm_cc3_interm_28(a, p) * wm_cc3_interm_29(i, q)
term(52) = term(52) + t1(a,q) * wm_cc3_interm_25(a, i) * wm_cc3_interm_29(p, i)
term(53) = term(53) + t1(a,q) * wm_cc3_interm_26(a, i) * wm_cc3_interm_29(p, i)
term(54) = term(54) + t1(a,q) * wm_cc3_interm_23(a, i) * wm_cc3_interm_29(p, i)
term(55) = term(55) + t1(a,q) * wm_cc3_interm_24(a, i) * wm_cc3_interm_29(p, i)
term(56) = term(56) + t1(a,q) * wm_cc3_interm_27(a, i) * wm_cc3_interm_29(p, i)
term(57) = term(57) + t1(a,q) * wm_cc3_interm_28(a, i) * wm_cc3_interm_29(p, i)
end do 
end do 

term(46) = -term(46) 
term(47) = term(47) * 2.0d+0 
term(48) = -term(48) 
term(49) = term(49) * 2.0d+0 
term(50) = term(50) * 2.0d+0 
term(51) = term(51) * (-4.0d+0) 
term(52) = -term(52) 
term(53) = term(53) * 2.0d+0 
term(54) = -term(54) 
term(55) = term(55) * 2.0d+0 
term(56) = term(56) * 2.0d+0 
term(57) = term(57) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(58) = term(58) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,p,j,i) * t1(c,k) * wm_cc3_interm_5(b, k, j, q)
term(59) = term(59) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,i,j,p) * t1(c,k) * wm_cc3_interm_5(b, k, j, q)
term(60) = term(60) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,p,j,i) * t1(c,k) * wm_cc3_interm_5(b, k, q, j)
term(61) = term(61) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,i,j,p) * t1(c,k) * wm_cc3_interm_5(b, k, q, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(58) = term(58) * (-1.9999999999999998d+0) 
term(59) = term(59) * 3.9999999999999996d+0 
term(60) = term(60) * 3.9999999999999996d+0 
term(61) = term(61) * (-7.999999999999999d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(62) = term(62) + t1(a,q) * wm_cc3_interm_0(i, j) * wm_cc3_interm_1(a, p, j, i)
term(63) = term(63) + t1(a,q) * wm_cc3_interm_0(i, j) * wm_cc3_interm_1(a, j, p, i)
term(64) = term(64) + r1(vrdav_Rr, a,j) * wm_cc3_interm_1(a, j, p, i) * wm_cc3_interm_29(i, q)
term(65) = term(65) + r1(vrdav_Rr, a,j) * wm_cc3_interm_1(a, p, j, i) * wm_cc3_interm_29(i, q)
term(66) = term(66) + t1(a,q) * wm_cc3_interm_0(i, j) * wm_cc3_interm_34(a, p, j, i)
term(67) = term(67) + t1(a,q) * wm_cc3_interm_0(i, j) * wm_cc3_interm_34(a, j, p, i)
term(68) = term(68) + t1(a,q) * wm_cc3_interm_0(i, j) * wm_cc3_interm_35(a, p, j, i)
term(69) = term(69) + t1(a,q) * wm_cc3_interm_0(i, j) * wm_cc3_interm_35(a, j, p, i)
term(70) = term(70) + r1(vrdav_Rr, a,j) * wm_cc3_interm_2(a, p, j, i) * wm_cc3_interm_29(i, q)
term(71) = term(71) + r1(vrdav_Rr, a,j) * wm_cc3_interm_29(i, q) * wm_cc3_interm_3(a, p, j, i)
term(72) = term(72) + r1(vrdav_Rr, a,j) * wm_cc3_interm_2(a, j, p, i) * wm_cc3_interm_29(i, q)
term(73) = term(73) + r1(vrdav_Rr, a,j) * wm_cc3_interm_29(i, q) * wm_cc3_interm_3(a, j, p, i)
term(74) = term(74) + r1(vrdav_Rr, a,j) * wm_cc3_interm_1(a, j, i, q) * wm_cc3_interm_29(p, i)
term(75) = term(75) + r1(vrdav_Rr, a,j) * wm_cc3_interm_2(a, j, i, q) * wm_cc3_interm_29(p, i)
term(76) = term(76) + r1(vrdav_Rr, a,j) * wm_cc3_interm_29(p, i) * wm_cc3_interm_3(a, j, i, q)
term(77) = term(77) + r1(vrdav_Rr, a,j) * wm_cc3_interm_29(p, i) * wm_cc3_interm_36(a, j, i, q)
end do 
end do 
end do 

term(62) = term(62) * (-2.0d+0) 
term(63) = term(63) * 4.0d+0 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * 4.0d+0 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * 4.0d+0 
term(68) = term(68) * 3.9999999999999996d+0 
term(69) = term(69) * (-7.999999999999999d+0) 
term(70) = term(70) * (-2.0d+0) 
term(71) = term(71) * 3.9999999999999996d+0 
term(72) = term(72) * 4.0d+0 
term(73) = term(73) * (-7.999999999999999d+0) 
term(74) = -term(74) 
term(75) = term(75) * 4.0d+0 
term(76) = term(76) * (-8.0d+0) 
term(77) = term(77) * 2.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(78) = term(78) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,i,j,p) * t2(a,c,q,k) * wm_cc3_interm_29(k, j)
term(79) = term(79) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,p,j,i) * t2(a,c,q,k) * wm_cc3_interm_29(k, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(78) = term(78) * (-1.9999999999999998d+0) 
term(79) = term(79) * 3.9999999999999996d+0 

do j = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(80) = term(80) + r2(vrdav_Rr, b,i,c,j) * t3(nocc, nactive, a,c,d,j,i,p) * t1(a,q) * wm_cc3_interm_22(b, d)
term(81) = term(81) + r2(vrdav_Rr, b,i,c,j) * t3(nocc, nactive, a,c,d,p,i,j) * t1(a,q) * wm_cc3_interm_22(b, d)
end do 
end do 
end do 
end do 
end do 
end do 

term(80) = term(80) * (-2.0d+0) 
term(81) = term(81) * 4.0d+0 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(82) = term(82) + r2(vrdav_Rr, b,i,c,j) * t3(nocc, nactive, a,c,d,i,j,p) * t1(a,q) * wm_cc3_interm_22(b, d)
term(83) = term(83) + r2(vrdav_Rr, b,i,c,j) * t3(nocc, nactive, a,c,d,p,j,i) * t1(a,q) * wm_cc3_interm_22(b, d)
end do 
end do 
end do 
end do 
end do 
end do 

term(82) = term(82) * 4.0d+0 
term(83) = term(83) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(84) = term(84) + r2(vrdav_Rr, b,i,c,j) * t3(nocc, nactive, a,c,d,i,p,j) * t1(a,q) * wm_cc3_interm_22(b, d)
end do 
end do 
end do 
end do 
end do 
end do 

term(84) = term(84) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(85) = term(85) + r2(vrdav_Rr, b,i,c,j) * t3(nocc, nactive, a,c,d,j,p,i) * t1(a,q) * wm_cc3_interm_22(b, d)
end do 
end do 
end do 
end do 
end do 
end do 

term(85) = term(85) * 4.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(86) = term(86) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,j,k,i) * t1(a,q) * wm_cc3_interm_5(c, p, k, j)
term(87) = term(87) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,j,k,i) * t1(a,q) * wm_cc3_interm_5(c, p, j, k)
term(88) = term(88) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,j,p,i) * t2(a,c,q,k) * wm_cc3_interm_29(k, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(86) = term(86) * (-2.0d+0) 
term(87) = term(87) * 4.0d+0 
term(88) = term(88) * (-1.9999999999999998d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(89) = term(89) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,j,k,p) * t1(a,q) * wm_cc3_interm_5(c, i, k, j)
term(90) = term(90) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,j,k,p) * t1(a,q) * wm_cc3_interm_5(c, i, j, k)
term(91) = term(91) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,p,k,j) * t1(a,q) * wm_cc3_interm_5(c, i, k, j)
term(92) = term(92) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,p,k,j) * t1(a,q) * wm_cc3_interm_5(c, i, j, k)
term(93) = term(93) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,p,i,j) * t2(a,c,q,k) * wm_cc3_interm_29(k, j)
term(94) = term(94) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,j,i,p) * t2(a,c,q,k) * wm_cc3_interm_29(k, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(89) = term(89) * 4.0d+0 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * (-7.999999999999999d+0) 
term(92) = term(92) * 4.0d+0 
term(93) = term(93) * (-7.999999999999999d+0) 
term(94) = term(94) * 3.9999999999999996d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(95) = term(95) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,i,k,j) * t1(a,q) * wm_cc3_interm_5(c, p, k, j)
term(96) = term(96) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,i,k,j) * t1(a,q) * wm_cc3_interm_5(c, p, j, k)
term(97) = term(97) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,i,p,j) * t2(a,c,q,k) * wm_cc3_interm_29(k, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(95) = term(95) * 3.9999999999999996d+0 
term(96) = term(96) * (-2.0d+0) 
term(97) = term(97) * 3.9999999999999996d+0 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(98) = term(98) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,k,p,j) * t1(a,q) * wm_cc3_interm_5(c, i, k, j)
term(99) = term(99) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,k,p,j) * t1(a,q) * wm_cc3_interm_5(c, i, j, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(98) = term(98) * 3.9999999999999996d+0 
term(99) = term(99) * (-2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(100) = term(100) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,k,i,j) * t1(a,q) * wm_cc3_interm_5(c, p, k, j)
term(101) = term(101) + r1(vrdav_Rr, b,i) * t3(nocc, nactive, a,b,c,k,i,j) * t1(a,q) * wm_cc3_interm_5(c, p, j, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(100) = term(100) * (-7.999999999999999d+0) 
term(101) = term(101) * 4.0d+0 


    calc_D_oo_wm_cc3 = zero
    do s = 0, 101
    calc_D_oo_wm_cc3 = calc_D_oo_wm_cc3 + term(s)
    end do

    end function calc_D_oo_wm_cc3
    
    function calc_D_ov_wm_cc3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_cc3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, a, k, b, j 
    real(F64), dimension(0:21) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + wm_cc3_interm_0(i, p) * wm_cc3_interm_9(q, i)
term(1) = term(1) + wm_cc3_interm_0(i, p) * wm_cc3_interm_15(q, i)
term(2) = term(2) + wm_cc3_interm_0(i, p) * wm_cc3_interm_20(q, i)
term(3) = term(3) + wm_cc3_interm_0(i, p) * wm_cc3_interm_21(q, i)
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-3.9999999999999996d+0) 
term(3) = term(3) * 1.9999999999999998d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + r1(vrdav_Rl, q,i) * r1(vrdav_Rr, a,j) * wm_cc3_interm_1(a, j, p, i)
term(5) = term(5) + r1(vrdav_Rl, q,i) * r1(vrdav_Rr, a,j) * wm_cc3_interm_1(a, p, j, i)
term(6) = term(6) + r1(vrdav_Rl, q,i) * r1(vrdav_Rr, a,j) * wm_cc3_interm_2(a, p, j, i)
term(7) = term(7) + r1(vrdav_Rl, q,i) * r1(vrdav_Rr, a,j) * wm_cc3_interm_3(a, p, j, i)
term(8) = term(8) + r1(vrdav_Rl, q,i) * r1(vrdav_Rr, a,j) * wm_cc3_interm_2(a, j, p, i)
term(9) = term(9) + r1(vrdav_Rl, q,i) * r1(vrdav_Rr, a,j) * wm_cc3_interm_3(a, j, p, i)
end do 
end do 
end do 

term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * 3.9999999999999996d+0 
term(8) = term(8) * 4.0d+0 
term(9) = term(9) * (-7.999999999999999d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(10) = term(10) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_cc3_interm_18(b, p, i, k)
term(11) = term(11) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_cc3_interm_18(b, p, k, i)
term(12) = term(12) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_cc3_interm_18(b, p, k, j)
term(13) = term(13) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_cc3_interm_18(b, p, j, k)
end do 
end do 
end do 
end do 
end do 

term(10) = term(10) * 4.0d+0 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * 4.0d+0 
term(13) = term(13) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(14) = term(14) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_cc3_interm_18(b, p, i, k)
term(15) = term(15) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_cc3_interm_18(b, p, k, i)
end do 
end do 
end do 
end do 
end do 

term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * 4.0d+0 

do a = nocc + 1, nactive 
term(16) = term(16) + wm_cc3_interm_22(q, a) * wm_cc3_interm_23(a, p)
term(17) = term(17) + wm_cc3_interm_22(q, a) * wm_cc3_interm_24(a, p)
term(18) = term(18) + wm_cc3_interm_22(q, a) * wm_cc3_interm_25(a, p)
term(19) = term(19) + wm_cc3_interm_22(q, a) * wm_cc3_interm_26(a, p)
term(20) = term(20) + wm_cc3_interm_22(q, a) * wm_cc3_interm_27(a, p)
term(21) = term(21) + wm_cc3_interm_22(q, a) * wm_cc3_interm_28(a, p)
end do 

term(16) = -term(16) 
term(17) = term(17) * 2.0d+0 
term(18) = -term(18) 
term(19) = term(19) * 2.0d+0 
term(20) = term(20) * 2.0d+0 
term(21) = term(21) * (-4.0d+0) 


    calc_D_ov_wm_cc3 = zero
    do s = 0, 21
    calc_D_ov_wm_cc3 = calc_D_ov_wm_cc3 + term(s)
    end do

    end function calc_D_ov_wm_cc3
    
    function calc_D_vo_wm_cc3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_cc3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, b, i, a, k, c 
    real(F64), dimension(0:86) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,i,j) * wm_cc3_interm_4(b, j)
term(1) = term(1) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_cc3_interm_4(b, j)
term(2) = term(2) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_cc3_interm_6(b, j)
term(3) = term(3) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,i,j) * wm_cc3_interm_6(b, j)
term(4) = term(4) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_cc3_interm_17(b, j)
term(5) = term(5) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_cc3_interm_19(b, j)
end do 
end do 
end do 
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * 3.9999999999999996d+0 
term(2) = term(2) * (-8.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * 4.0d+0 
term(5) = term(5) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(6) = term(6) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,i,q,j) * wm_cc3_interm_4(b, j)
term(7) = term(7) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,i,q,j) * wm_cc3_interm_6(b, j)
end do 
end do 
end do 
end do 

term(6) = term(6) * 3.9999999999999996d+0 
term(7) = term(7) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(8) = term(8) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,j,q,i) * wm_cc3_interm_4(b, j)
term(9) = term(9) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,j,q,i) * wm_cc3_interm_6(b, j)
end do 
end do 
end do 
end do 

term(8) = term(8) * (-1.9999999999999998d+0) 
term(9) = term(9) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(10) = term(10) + wm_cc3_interm_29(i, j) * wm_cc3_interm_30(p, i, q, j)
term(11) = term(11) + wm_cc3_interm_29(i, j) * wm_cc3_interm_30(p, i, j, q)
term(12) = term(12) + wm_cc3_interm_29(i, j) * wm_cc3_interm_31(p, i, j, q)
term(13) = term(13) + wm_cc3_interm_29(i, j) * wm_cc3_interm_32(p, i, j, q)
term(14) = term(14) + wm_cc3_interm_29(i, j) * wm_cc3_interm_31(p, i, q, j)
term(15) = term(15) + wm_cc3_interm_29(i, j) * wm_cc3_interm_32(p, i, q, j)
end do 
end do 

term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * 4.0d+0 
term(12) = term(12) * 4.0d+0 
term(13) = term(13) * (-8.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_4(b, j)
term(17) = term(17) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,i,j,q) * wm_cc3_interm_4(b, j)
term(18) = term(18) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_6(b, j)
term(19) = term(19) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,i,j,q) * wm_cc3_interm_6(b, j)
term(20) = term(20) + r1(vrdav_Rl, b,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_17(a, j)
term(21) = term(21) + r1(vrdav_Rl, b,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_17(a, i)
term(22) = term(22) + r1(vrdav_Rl, b,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_19(a, j)
term(23) = term(23) + r1(vrdav_Rl, b,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_19(a, i)
term(24) = term(24) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_17(b, i)
term(25) = term(25) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_19(b, i)
term(26) = term(26) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_19(b, j)
term(27) = term(27) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_17(b, j)
term(28) = term(28) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,i,j,q) * wm_cc3_interm_19(b, j)
term(29) = term(29) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,i,j,q) * wm_cc3_interm_17(b, j)
end do 
end do 
end do 
end do 

term(16) = term(16) * 3.9999999999999996d+0 
term(17) = term(17) * (-7.999999999999999d+0) 
term(18) = term(18) * (-7.999999999999999d+0) 
term(19) = term(19) * 15.999999999999998d+0 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * 4.0d+0 
term(22) = term(22) * 4.0d+0 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * 4.0d+0 
term(26) = term(26) * (-8.0d+0) 
term(27) = term(27) * 4.0d+0 
term(28) = term(28) * 16.0d+0 
term(29) = term(29) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(30) = term(30) + r1(vrdav_Rl, a,j) * r1(vrdav_Rr, p,i) * wm_cc3_interm_11(a, i, j, q)
term(31) = term(31) + r1(vrdav_Rl, a,j) * r1(vrdav_Rr, p,i) * wm_cc3_interm_13(a, i, j, q)
term(32) = term(32) + r1(vrdav_Rl, a,j) * r1(vrdav_Rr, p,i) * wm_cc3_interm_12(a, i, j, q)
term(33) = term(33) + r1(vrdav_Rl, a,j) * r1(vrdav_Rr, p,i) * wm_cc3_interm_16(a, i, j, q)
term(34) = term(34) + r1(vrdav_Rl, a,j) * r1(vrdav_Rr, p,i) * wm_cc3_interm_11(a, i, q, j)
term(35) = term(35) + r1(vrdav_Rl, a,j) * r1(vrdav_Rr, p,i) * wm_cc3_interm_16(a, i, q, j)
end do 
end do 
end do 

term(30) = term(30) * 3.9999999999999996d+0 
term(31) = term(31) * (-2.0d+0) 
term(32) = term(32) * 4.0d+0 
term(33) = term(33) * (-7.999999999999999d+0) 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * 3.9999999999999996d+0 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(36) = term(36) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,j,i) * wm_cc3_interm_22(a, c)
term(37) = term(37) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_cc3_interm_22(a, c)
end do 
end do 
end do 
end do 
end do 

term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * 4.0d+0 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(38) = term(38) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,i,j) * wm_cc3_interm_22(a, c)
term(39) = term(39) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_cc3_interm_22(a, c)
end do 
end do 
end do 
end do 
end do 

term(38) = term(38) * 4.0d+0 
term(39) = term(39) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(40) = term(40) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,q,j) * wm_cc3_interm_22(a, c)
end do 
end do 
end do 
end do 
end do 

term(40) = term(40) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(41) = term(41) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,q,i) * wm_cc3_interm_22(a, c)
end do 
end do 
end do 
end do 
end do 

term(41) = term(41) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(42) = term(42) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_cc3_interm_33(c, a)
end do 
end do 
end do 
end do 
end do 

term(42) = -term(42) 

term(43) = term(43) + wm_cc3_interm_8 * wm_cc3_interm_9(p, q)
term(44) = term(44) + wm_cc3_interm_15(p, q) * wm_cc3_interm_8
term(45) = term(45) + wm_cc3_interm_20(p, q) * wm_cc3_interm_8
term(46) = term(46) + wm_cc3_interm_21(p, q) * wm_cc3_interm_8

term(43) = term(43) * 3.9999999999999996d+0 
term(44) = term(44) * (-7.999999999999999d+0) 
term(45) = term(45) * 7.999999999999999d+0 
term(46) = term(46) * (-3.9999999999999996d+0) 

do a = nocc + 1, nactive 
term(47) = term(47) + wm_cc3_interm_10(a, p) * wm_cc3_interm_9(a, q)
term(48) = term(48) + wm_cc3_interm_10(a, p) * wm_cc3_interm_15(a, q)
term(49) = term(49) + wm_cc3_interm_10(a, p) * wm_cc3_interm_20(a, q)
term(50) = term(50) + wm_cc3_interm_10(a, p) * wm_cc3_interm_21(a, q)
end do 

term(47) = term(47) * (-2.0d+0) 
term(48) = term(48) * 3.9999999999999996d+0 
term(49) = term(49) * (-3.9999999999999996d+0) 
term(50) = term(50) * 1.9999999999999998d+0 

do i = 1, nocc 
term(51) = term(51) + wm_cc3_interm_25(p, i) * wm_cc3_interm_29(q, i)
term(52) = term(52) + wm_cc3_interm_26(p, i) * wm_cc3_interm_29(q, i)
term(53) = term(53) + wm_cc3_interm_23(p, i) * wm_cc3_interm_29(q, i)
term(54) = term(54) + wm_cc3_interm_24(p, i) * wm_cc3_interm_29(q, i)
term(55) = term(55) + wm_cc3_interm_27(p, i) * wm_cc3_interm_29(q, i)
term(56) = term(56) + wm_cc3_interm_28(p, i) * wm_cc3_interm_29(q, i)
end do 

term(51) = -term(51) 
term(52) = term(52) * 2.0d+0 
term(53) = -term(53) 
term(54) = term(54) * 2.0d+0 
term(55) = term(55) * 2.0d+0 
term(56) = term(56) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(57) = term(57) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,i,j) * wm_cc3_interm_5(b, q, k, j)
term(58) = term(58) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,q,j) * wm_cc3_interm_5(b, i, k, j)
term(59) = term(59) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,q,j) * wm_cc3_interm_5(b, i, j, k)
term(60) = term(60) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,i,j) * wm_cc3_interm_5(b, q, j, k)
end do 
end do 
end do 
end do 
end do 

term(57) = term(57) * (-2.0d+0) 
term(58) = term(58) * 4.0d+0 
term(59) = term(59) * (-2.0d+0) 
term(60) = term(60) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(61) = term(61) + r1(vrdav_Rl, b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_cc3_interm_18(a, j, k, i)
term(62) = term(62) + r1(vrdav_Rl, b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_cc3_interm_18(a, j, i, k)
term(63) = term(63) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_cc3_interm_18(b, j, i, k)
term(64) = term(64) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_cc3_interm_18(b, j, k, i)
end do 
end do 
end do 
end do 
end do 

term(61) = term(61) * 4.0d+0 
term(62) = term(62) * (-2.0d+0) 
term(63) = term(63) * 4.0d+0 
term(64) = term(64) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(65) = term(65) + wm_cc3_interm_0(i, j) * wm_cc3_interm_1(p, q, j, i)
term(66) = term(66) + wm_cc3_interm_0(i, j) * wm_cc3_interm_1(p, j, q, i)
term(67) = term(67) + wm_cc3_interm_0(i, j) * wm_cc3_interm_2(p, j, q, i)
term(68) = term(68) + wm_cc3_interm_0(i, j) * wm_cc3_interm_2(p, q, j, i)
term(69) = term(69) + wm_cc3_interm_0(i, j) * wm_cc3_interm_3(p, j, q, i)
term(70) = term(70) + wm_cc3_interm_0(i, j) * wm_cc3_interm_3(p, q, j, i)
term(71) = term(71) + wm_cc3_interm_0(i, j) * wm_cc3_interm_7(p, j, q, i)
term(72) = term(72) + wm_cc3_interm_0(i, j) * wm_cc3_interm_12(p, j, i, q)
term(73) = term(73) + wm_cc3_interm_0(i, j) * wm_cc3_interm_13(p, j, i, q)
term(74) = term(74) + wm_cc3_interm_0(i, j) * wm_cc3_interm_14(p, j, q, i)
term(75) = term(75) + wm_cc3_interm_0(i, j) * wm_cc3_interm_7(p, j, i, q)
term(76) = term(76) + wm_cc3_interm_0(i, j) * wm_cc3_interm_14(p, j, i, q)
end do 
end do 

term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * 4.0d+0 
term(67) = term(67) * (-2.0d+0) 
term(68) = term(68) * 4.0d+0 
term(69) = term(69) * 3.9999999999999996d+0 
term(70) = term(70) * (-7.999999999999999d+0) 
term(71) = term(71) * (-1.9999999999999998d+0) 
term(72) = term(72) * (-2.0d+0) 
term(73) = term(73) * 4.0d+0 
term(74) = term(74) * 3.9999999999999996d+0 
term(75) = term(75) * 4.0d+0 
term(76) = term(76) * (-7.999999999999999d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(77) = term(77) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,i) * wm_cc3_interm_5(b, q, k, j)
term(78) = term(78) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,i) * wm_cc3_interm_5(b, q, j, k)
end do 
end do 
end do 
end do 
end do 

term(77) = term(77) * 3.9999999999999996d+0 
term(78) = term(78) * (-2.0d+0) 

do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(79) = term(79) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_cc3_interm_5(b, i, k, j)
term(80) = term(80) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_cc3_interm_5(b, i, j, k)
term(81) = term(81) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_cc3_interm_18(b, i, k, j)
term(82) = term(82) + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_cc3_interm_18(b, i, j, k)
end do 
end do 
end do 
end do 
end do 

term(79) = term(79) * (-7.999999999999999d+0) 
term(80) = term(80) * 4.0d+0 
term(81) = term(81) * (-8.0d+0) 
term(82) = term(82) * 4.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(83) = term(83) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,k,j) * wm_cc3_interm_5(b, i, j, k)
term(84) = term(84) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,q,k,j) * wm_cc3_interm_5(b, i, k, j)
end do 
end do 
end do 
end do 
end do 

term(83) = term(83) * 3.9999999999999996d+0 
term(84) = term(84) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(85) = term(85) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,i,k,j) * wm_cc3_interm_5(b, q, j, k)
term(86) = term(86) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,i,k,j) * wm_cc3_interm_5(b, q, k, j)
end do 
end do 
end do 
end do 
end do 

term(85) = term(85) * (-7.999999999999999d+0) 
term(86) = term(86) * 4.0d+0 


    calc_D_vo_wm_cc3 = zero
    do s = 0, 86
    calc_D_vo_wm_cc3 = calc_D_vo_wm_cc3 + term(s)
    end do

    end function calc_D_vo_wm_cc3
    
    function calc_D_vv_wm_cc3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_cc3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, b, j, a, l, c 
    real(F64), dimension(0:95) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,i,j,k) * t1(q,i) * wm_cc3_interm_4(b, k)
term(1) = term(1) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,i,j,k) * t1(q,i) * wm_cc3_interm_6(b, k)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * 1.9999999999999998d+0 
term(1) = term(1) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(2) = term(2) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,j,i,k) * t1(q,i) * wm_cc3_interm_4(b, k)
term(3) = term(3) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,j,i,k) * t1(q,i) * wm_cc3_interm_6(b, k)
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * (-3.9999999999999996d+0) 
term(3) = term(3) * 8.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,k,i,j) * t1(q,i) * wm_cc3_interm_4(b, k)
term(5) = term(5) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,k,i,j) * t1(q,i) * wm_cc3_interm_6(b, k)
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * 1.9999999999999998d+0 
term(5) = term(5) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(6) = term(6) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,k,j,i) * t1(q,i) * wm_cc3_interm_4(b, k)
term(7) = term(7) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,k,j,i) * t1(q,i) * wm_cc3_interm_6(b, k)
end do 
end do 
end do 
end do 
end do 

term(6) = term(6) * (-3.9999999999999996d+0) 
term(7) = term(7) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(8) = term(8) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,i,k,j) * t1(q,i) * wm_cc3_interm_4(b, k)
term(9) = term(9) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,i,k,j) * t1(q,i) * wm_cc3_interm_6(b, k)
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * (-3.9999999999999996d+0) 
term(9) = term(9) * 7.999999999999999d+0 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(10) = term(10) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,j,k,i) * t1(q,i) * wm_cc3_interm_4(b, k)
term(11) = term(11) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,j,k,i) * t1(q,i) * wm_cc3_interm_6(b, k)
end do 
end do 
end do 
end do 
end do 

term(10) = term(10) * 7.999999999999999d+0 
term(11) = term(11) * (-15.999999999999998d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(12) = term(12) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, a,k) * t1(q,j) * wm_cc3_interm_1(a, k, j, i)
term(13) = term(13) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, a,k) * t1(q,j) * wm_cc3_interm_2(a, k, j, i)
term(14) = term(14) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, a,k) * t1(q,j) * wm_cc3_interm_3(a, k, j, i)
end do 
end do 
end do 
end do 

term(12) = term(12) * 2.0d+0 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * 7.999999999999999d+0 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(15) = term(15) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, a,k) * t1(q,j) * wm_cc3_interm_1(a, j, k, i)
term(16) = term(16) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, a,k) * t1(q,j) * wm_cc3_interm_2(a, j, k, i)
term(17) = term(17) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, a,k) * t1(q,j) * wm_cc3_interm_3(a, j, k, i)
end do 
end do 
end do 
end do 

term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * 2.0d+0 
term(17) = term(17) * (-3.9999999999999996d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(18) = term(18) + t1(q,i) * wm_cc3_interm_22(p, a) * wm_cc3_interm_23(a, i)
term(19) = term(19) + t1(q,i) * wm_cc3_interm_22(p, a) * wm_cc3_interm_24(a, i)
term(20) = term(20) + t1(q,i) * wm_cc3_interm_22(p, a) * wm_cc3_interm_25(a, i)
term(21) = term(21) + t1(q,i) * wm_cc3_interm_22(p, a) * wm_cc3_interm_26(a, i)
term(22) = term(22) + t1(q,i) * wm_cc3_interm_22(p, a) * wm_cc3_interm_27(a, i)
term(23) = term(23) + t1(q,i) * wm_cc3_interm_22(p, a) * wm_cc3_interm_28(a, i)
end do 
end do 

term(19) = term(19) * (-2.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(24) = term(24) + t1(q,j) * wm_cc3_interm_25(p, i) * wm_cc3_interm_29(j, i)
term(25) = term(25) + t1(q,j) * wm_cc3_interm_26(p, i) * wm_cc3_interm_29(j, i)
term(26) = term(26) + t1(q,j) * wm_cc3_interm_23(p, i) * wm_cc3_interm_29(j, i)
term(27) = term(27) + t1(q,j) * wm_cc3_interm_24(p, i) * wm_cc3_interm_29(j, i)
term(28) = term(28) + t1(q,j) * wm_cc3_interm_27(p, i) * wm_cc3_interm_29(j, i)
term(29) = term(29) + t1(q,j) * wm_cc3_interm_28(p, i) * wm_cc3_interm_29(j, i)
end do 
end do 

term(25) = term(25) * (-2.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (-2.0d+0) 
term(29) = term(29) * 4.0d+0 

do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(30) = term(30) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,j,k) * t1(q,i) * wm_cc3_interm_5(b, i, l, k)
term(31) = term(31) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,j,k) * t1(q,i) * wm_cc3_interm_5(b, i, k, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(30) = term(30) * 2.0d+0 
term(31) = term(31) * (-4.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(32) = term(32) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,i,k) * t1(q,i) * wm_cc3_interm_5(b, j, l, k)
term(33) = term(33) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,i,k) * t1(q,i) * wm_cc3_interm_5(b, j, k, l)
term(34) = term(34) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,i,k) * t2(b,q,k,i) * wm_cc3_interm_29(j, l)
term(35) = term(35) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,j,k) * t2(b,q,k,i) * wm_cc3_interm_29(i, l)
term(36) = term(36) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,i,k) * t2(b,q,l,i) * wm_cc3_interm_29(j, k)
term(37) = term(37) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,j,k) * t2(b,q,l,i) * wm_cc3_interm_29(i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * 2.0d+0 
term(34) = term(34) * (-3.9999999999999996d+0) 
term(35) = term(35) * 1.9999999999999998d+0 
term(36) = term(36) * 1.9999999999999998d+0 
term(37) = term(37) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(38) = term(38) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,k,j) * t1(q,i) * wm_cc3_interm_5(b, i, l, k)
term(39) = term(39) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,k,j) * t1(q,i) * wm_cc3_interm_5(b, i, k, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(38) = term(38) * (-3.9999999999999996d+0) 
term(39) = term(39) * 2.0d+0 

do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(40) = term(40) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,k,i) * t1(q,i) * wm_cc3_interm_5(b, j, l, k)
term(41) = term(41) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,k,i) * t1(q,i) * wm_cc3_interm_5(b, j, k, l)
term(42) = term(42) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,k,i) * t2(b,q,k,i) * wm_cc3_interm_29(j, l)
term(43) = term(43) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,k,i) * t2(b,q,l,i) * wm_cc3_interm_29(j, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(40) = term(40) * 7.999999999999999d+0 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * 7.999999999999999d+0 
term(43) = term(43) * (-3.9999999999999996d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(44) = term(44) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,i,l,k) * t1(q,i) * wm_cc3_interm_5(b, j, k, l)
term(45) = term(45) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,i,l,k) * t1(q,i) * wm_cc3_interm_5(b, j, l, k)
term(46) = term(46) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,i,l,k) * t2(b,q,k,i) * wm_cc3_interm_29(j, l)
term(47) = term(47) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,i,l,k) * t2(b,q,l,i) * wm_cc3_interm_29(j, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(44) = term(44) * (-3.9999999999999996d+0) 
term(45) = term(45) * 2.0d+0 
term(46) = term(46) * 1.9999999999999998d+0 
term(47) = term(47) * (-3.9999999999999996d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(48) = term(48) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,j,l,k) * t1(q,i) * wm_cc3_interm_5(b, i, k, l)
term(49) = term(49) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,j,l,k) * t1(q,i) * wm_cc3_interm_5(b, i, l, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(48) = term(48) * 7.999999999999999d+0 
term(49) = term(49) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(50) = term(50) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, b,c,p,k,i,j) * t2(c,q,k,i) * wm_cc3_interm_22(a, b)
end do 
end do 
end do 
end do 
end do 
end do 

term(50) = term(50) * 1.9999999999999998d+0 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(51) = term(51) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, b,c,p,j,i,k) * t2(c,q,k,i) * wm_cc3_interm_22(a, b)
end do 
end do 
end do 
end do 
end do 
end do 

term(51) = term(51) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(52) = term(52) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,i,j) * t1(b,l) * wm_cc3_interm_5(q, l, j, k)
term(53) = term(53) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,i,j) * t1(b,l) * wm_cc3_interm_5(q, l, k, j)
term(54) = term(54) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,k,i,j) * t2(b,q,l,i) * wm_cc3_interm_29(l, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(52) = term(52) * 1.9999999999999998d+0 
term(53) = term(53) * (-3.9999999999999996d+0) 
term(54) = term(54) * 1.9999999999999998d+0 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(55) = term(55) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,i) * t1(b,l) * wm_cc3_interm_5(q, l, j, k)
term(56) = term(56) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,k,j,i) * t1(b,l) * wm_cc3_interm_5(q, l, k, j)
term(57) = term(57) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,k,j,i) * t2(b,q,l,i) * wm_cc3_interm_29(l, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(55) = term(55) * (-3.9999999999999996d+0) 
term(56) = term(56) * 1.9999999999999998d+0 
term(57) = term(57) * (-3.9999999999999996d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(58) = term(58) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, b,c,p,i,j,k) * t2(c,q,k,i) * wm_cc3_interm_22(a, b)
end do 
end do 
end do 
end do 
end do 
end do 

term(58) = term(58) * 1.9999999999999998d+0 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(59) = term(59) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, b,c,p,i,k,j) * t2(c,q,k,i) * wm_cc3_interm_22(a, b)
term(60) = term(60) + r2(vrdav_Rr, a,j,b,k) * t3(nocc, nactive, b,c,p,i,k,j) * t1(q,i) * wm_cc3_interm_22(a, c)
end do 
end do 
end do 
end do 
end do 
end do 

term(59) = term(59) * (-3.9999999999999996d+0) 
term(60) = term(60) * 2.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(61) = term(61) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, b,c,p,k,j,i) * t2(c,q,k,i) * wm_cc3_interm_22(a, b)
end do 
end do 
end do 
end do 
end do 
end do 

term(61) = term(61) * (-3.9999999999999996d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(62) = term(62) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, b,c,p,j,k,i) * t2(c,q,k,i) * wm_cc3_interm_22(a, b)
term(63) = term(63) + r2(vrdav_Rr, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * t1(q,i) * wm_cc3_interm_22(a, c)
end do 
end do 
end do 
end do 
end do 
end do 

term(62) = term(62) * 7.999999999999999d+0 
term(63) = term(63) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(64) = term(64) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,i,k,j) * t1(b,l) * wm_cc3_interm_5(q, l, j, k)
term(65) = term(65) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,p,i,k,j) * t1(b,l) * wm_cc3_interm_5(q, l, k, j)
term(66) = term(66) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,i,k,j) * t2(b,q,l,i) * wm_cc3_interm_29(l, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(64) = term(64) * (-3.9999999999999996d+0) 
term(65) = term(65) * 7.999999999999999d+0 
term(66) = term(66) * (-3.9999999999999996d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(67) = term(67) + r1(vrdav_Rr, b,j) * t3(nocc, nactive, a,b,c,j,k,i) * t2(c,q,k,i) * wm_cc3_interm_22(p, a)
end do 
end do 
end do 
end do 
end do 
end do 

term(67) = term(67) * 1.9999999999999998d+0 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(68) = term(68) + r1(vrdav_Rr, b,j) * t3(nocc, nactive, a,b,c,k,j,i) * t2(c,q,k,i) * wm_cc3_interm_22(p, a)
end do 
end do 
end do 
end do 
end do 
end do 

term(68) = term(68) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(69) = term(69) + r1(vrdav_Rr, b,j) * t3(nocc, nactive, a,b,c,k,i,j) * t2(c,q,k,i) * wm_cc3_interm_22(p, a)
end do 
end do 
end do 
end do 
end do 
end do 

term(69) = term(69) * 1.9999999999999998d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(70) = term(70) + r1(vrdav_Rr, b,j) * t3(nocc, nactive, a,b,c,j,i,k) * t2(c,q,k,i) * wm_cc3_interm_22(p, a)
end do 
end do 
end do 
end do 
end do 
end do 

term(70) = term(70) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(71) = term(71) + r1(vrdav_Rr, b,j) * t3(nocc, nactive, a,b,c,i,k,j) * t2(c,q,k,i) * wm_cc3_interm_22(p, a)
end do 
end do 
end do 
end do 
end do 
end do 

term(71) = term(71) * (-3.9999999999999996d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(72) = term(72) + r1(vrdav_Rr, b,j) * t3(nocc, nactive, a,b,c,i,j,k) * t2(c,q,k,i) * wm_cc3_interm_22(p, a)
end do 
end do 
end do 
end do 
end do 
end do 

term(72) = term(72) * 7.999999999999999d+0 

do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(73) = term(73) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,k,j) * t2(b,q,k,i) * wm_cc3_interm_29(i, l)
term(74) = term(74) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,l,k,j) * t2(b,q,l,i) * wm_cc3_interm_29(i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(73) = term(73) * (-3.9999999999999996d+0) 
term(74) = term(74) * 1.9999999999999998d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(75) = term(75) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,j,l,k) * t2(b,q,k,i) * wm_cc3_interm_29(i, l)
term(76) = term(76) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,j,i,k) * t2(b,q,l,i) * wm_cc3_interm_29(l, k)
term(77) = term(77) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,j,l,k) * t2(b,q,l,i) * wm_cc3_interm_29(i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(75) = term(75) * (-3.9999999999999996d+0) 
term(76) = term(76) * (-3.9999999999999996d+0) 
term(77) = term(77) * 7.999999999999999d+0 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(78) = term(78) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,j,k,i) * t2(b,q,l,i) * wm_cc3_interm_29(l, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(78) = term(78) * 7.999999999999999d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(79) = term(79) + r1(vrdav_Rr, a,j) * t3(nocc, nactive, a,b,p,i,j,k) * t2(b,q,l,i) * wm_cc3_interm_29(l, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(79) = term(79) * 1.9999999999999998d+0 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(80) = term(80) + r2(vrdav_Rr, a,j,b,k) * t3(nocc, nactive, b,c,p,i,j,k) * t1(q,i) * wm_cc3_interm_22(a, c)
end do 
end do 
end do 
end do 
end do 
end do 

term(80) = term(80) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(81) = term(81) + r2(vrdav_Rr, a,j,b,k) * t3(nocc, nactive, b,c,p,j,i,k) * t1(q,i) * wm_cc3_interm_22(a, c)
end do 
end do 
end do 
end do 
end do 
end do 

term(81) = term(81) * 2.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(82) = term(82) + r2(vrdav_Rr, a,j,b,k) * t3(nocc, nactive, b,c,p,k,i,j) * t1(q,i) * wm_cc3_interm_22(a, c)
end do 
end do 
end do 
end do 
end do 
end do 

term(82) = term(82) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(83) = term(83) + r2(vrdav_Rr, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * t1(q,i) * wm_cc3_interm_22(a, c)
end do 
end do 
end do 
end do 
end do 
end do 

term(83) = term(83) * 8.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(84) = term(84) + t1(q,i) * wm_cc3_interm_0(j, k) * wm_cc3_interm_1(p, i, k, j)
term(85) = term(85) + t1(q,i) * wm_cc3_interm_0(j, k) * wm_cc3_interm_2(p, i, k, j)
term(86) = term(86) + t1(q,i) * wm_cc3_interm_0(j, k) * wm_cc3_interm_3(p, i, k, j)
end do 
end do 
end do 

term(84) = term(84) * 2.0d+0 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * 7.999999999999999d+0 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(87) = term(87) + t1(q,i) * wm_cc3_interm_0(j, k) * wm_cc3_interm_1(p, k, i, j)
term(88) = term(88) + t1(q,i) * wm_cc3_interm_0(j, k) * wm_cc3_interm_2(p, k, i, j)
term(89) = term(89) + t1(q,i) * wm_cc3_interm_0(j, k) * wm_cc3_interm_3(p, k, i, j)
end do 
end do 
end do 

term(87) = term(87) * (-4.0d+0) 
term(88) = term(88) * 2.0d+0 
term(89) = term(89) * (-3.9999999999999996d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(90) = term(90) + t1(q,i) * wm_cc3_interm_29(j, k) * wm_cc3_interm_30(p, j, i, k)
term(91) = term(91) + t1(q,i) * wm_cc3_interm_29(j, k) * wm_cc3_interm_31(p, j, i, k)
term(92) = term(92) + t1(q,i) * wm_cc3_interm_29(j, k) * wm_cc3_interm_32(p, j, i, k)
end do 
end do 
end do 

term(90) = term(90) * 2.0d+0 
term(91) = term(91) * 2.0d+0 
term(92) = term(92) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(93) = term(93) + t1(q,i) * wm_cc3_interm_29(j, k) * wm_cc3_interm_30(p, j, k, i)
term(94) = term(94) + t1(q,i) * wm_cc3_interm_29(j, k) * wm_cc3_interm_31(p, j, k, i)
term(95) = term(95) + t1(q,i) * wm_cc3_interm_29(j, k) * wm_cc3_interm_32(p, j, k, i)
end do 
end do 
end do 

term(93) = term(93) * (-4.0d+0) 
term(94) = term(94) * (-4.0d+0) 
term(95) = term(95) * 8.0d+0 


    calc_D_vv_wm_cc3 = zero
    do s = 0, 95
    calc_D_vv_wm_cc3 = calc_D_vv_wm_cc3 + term(s)
    end do

    end function calc_D_vv_wm_cc3



end module density_exc_exc_functions_cc3
