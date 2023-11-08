module davidson_parallel_idx

      implicit none
      integer, dimension(:), allocatable :: bj_offset
      integer, dimension(:), allocatable :: ai_offset
      integer, dimension(:), allocatable :: aiaj_offset_i, aiaj_offset_j

contains

      subroutine triang_idx_aiai(k, npair, nocc, a, as, b, bs, i, is, j, js, ai, bj)
            !
            ! ai = bj
            ! virtual indices changes faster than occupied
            ! ilosc elementow 
            ! npair
            !
            integer, intent(in) :: k
            integer, intent(in) :: npair
            integer, intent(in) :: nocc
            integer, intent(out) :: a, b, i ,j, ai, bj, as, bs, is, js

            ai = k
            bj = k

            a = (ai - 1) / nocc + nocc + 1
            as = a
            i = ai - nocc * (a - (nocc + 1))
            is = i
            b = (bj - 1) / nocc + nocc + 1
            bs = b
            j = bj -nocc * (b - (nocc + 1))
            js = j

      end subroutine triang_idx_aiai

      subroutine triang_idx_aiaj_inej(k, nvirt0, nocc0, npair, nocc, a, as, b, bs, i, is, j, js, ai, bj)
            !
            ! ai > aj i!=j
            ! ai changes faster then bj
            ! virtual indices changes faster than occupied
            ! ilosc elementow
            ! ((nocc - 1) * nocc * nvirt)/2 
            !
            integer, intent(in) :: k
            integer, intent(in) :: npair
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0, nocc0
            integer, intent(out) :: a, b, i ,j, ai, bj, as, bs, is, js
            integer :: v, s, in1
            in1 = k - (k-1)/(((nocc-1) * nocc)/2) * (((nocc-1) * nocc)/2)

            a = (k-1) / (((nocc-1) * nocc)/2) + nvirt0
            as = a
            b = a
            bs = b
            
            i = nocc0 - 1 + aiaj_offset_i(in1) 
            is = i
            j = nocc0 - 1 + aiaj_offset_j(in1)
            js = j

            ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            bj = (b - nvirt0) * nocc + (j - nocc0) + 1

      end subroutine triang_idx_aiaj_inej

      subroutine triang_idx_aiaj(k, nvirt0, nocc0, npair, nocc, a, as, b, bs, i, is, j, js, ai, bj)
            !
            ! ai > aj
            ! ai changes faster then bj
            ! virtual indices changes faster than occupied
            ! ilosc elementow
            ! nvirt * nocc * (nocc - 1) / 2
            !
            integer, intent(in) :: k
            integer, intent(in) :: npair
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt0, nocc0
            integer, intent(out) :: a, b, i ,j, ai, bj, as, bs, is, js
            integer :: v, s

            a = (k-1) / ((nocc-1) * nocc) + nvirt0
            as = a
            b = a
            bs = b
            i = (k-1)/(nocc-1) + nocc0 - (k-1) / ((nocc-1) * nocc) * nocc
            is = i

            s = k + 1 - (k-1) / ((nocc-1)) * (nocc -1)

            v = (1 + sign(1, -abs(i-s)) ) / 2
            j = s-v*s + v*1
            js = j

            ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            bj = (b - nvirt0) * nocc + (j - nocc0) + 1

      end subroutine triang_idx_aiaj

      subroutine triang_idx_aibi(k, npair, nocc, a, as, b, bs, i, is, j, js, ai, bj)
            !
            ! ai > bj a!= b i=j
            ! ai changes faster then bj
            ! virtual indices changes faster than occupied
            ! ilosc elementow
            ! nocc *(nvirt *(nvirt-1) / 2)
            !
            integer, intent(in) :: k
            integer, intent(in) :: npair
            integer, intent(in) :: nocc
            integer, intent(out) :: a, b, i ,j, ai, bj, as, bs, is, js
            integer :: in1

            in1 = (k-1)/(nocc)            

            bj = k - ((k-1)/nocc) * nocc + bj_offset(in1) * nocc
            ai = k - nocc * ((k-1) / nocc) + nocc + ai_offset(in1) * nocc

            a = (ai - 1) / nocc + nocc + 1
            as = a
            i = ai - nocc * (a - (nocc + 1))
            is = i
            b = (bj - 1) / nocc + nocc + 1
            bs = b
            j = bj -nocc * (b - (nocc + 1))
            js = j

      end subroutine triang_idx_aibi

      subroutine triang_idx_aibj_aneb(k, npair, nocc, a, as, b, bs, i, is, j, js, ai, bj)
            !
            ! ai > bj, a!=b
            ! ai changes faster then bj
            ! virtual indices changes faster than occupied
            ! ilosc elementow
            ! npair * (npair - nocc) / 2
            !
            integer, intent(in) :: k
            integer, intent(in) :: npair
            integer, intent(in) :: nocc
            integer, intent(out) :: a, b, i ,j, ai, bj, as, bs, is, js
            integer :: in1

            in1 = (k-1)/(nocc**2)            
            bj = (k-1)/nocc + 1 -((k-1)/(nocc**2)) * nocc + bj_offset(in1) * nocc
            ai = k - nocc * ((k-1) / nocc) + nocc + ai_offset(in1) * nocc

            a = (ai - 1) / nocc + nocc + 1 
            as = a
            i = ai - nocc * (a  - (nocc + 1))
            is = i
            b = (bj - 1) / nocc + nocc + 1
            bs = b
            j = bj -nocc * (b - (nocc + 1))
            js = j

      end subroutine triang_idx_aibj_aneb

      subroutine fill_offset(nocc, nvirt, npair)

            integer, intent(in) :: nvirt, nocc, npair
            integer :: i, j, k, l

            allocate(bj_offset(0:nvirt *(nvirt-1) / 2 - 1))
            allocate(ai_offset(0:nvirt *(nvirt-1) / 2 - 1))
            allocate(aiaj_offset_i(1:(nocc*(nocc-1))/2 ))
            allocate(aiaj_offset_j(1:(nocc*(nocc-1))/2 ))

            ai_offset = 0
            bj_offset = 0
            aiaj_offset_i = 0
            aiaj_offset_j = 0

            k = 1
            l = nvirt *(nvirt-1) / 2 - 1
            do i = 1, nvirt -1
                  do j = 1, k
                        bj_offset(l) = nvirt - 1 - k
                        l = l -1
                  end do
                  k = k + 1
            end do

            k = 1
            l = 0
            do i = 1, nvirt -1
                  do j = k, nvirt -1
                        ai_offset(l) = j - 1
                        l = l + 1
                  end do
                  k = k + 1
            end do

           l = 1
            do i = 2, nocc
                  do j = 1, i-1
                        aiaj_offset_i(l) = i
                        aiaj_offset_j(l) = j
                        l = l + 1
                  end do
            end do

      end subroutine fill_offset

      subroutine fill_offset_free()

            deallocate(bj_offset)
            deallocate(ai_offset)
            deallocate(aiaj_offset_i)
            deallocate(aiaj_offset_j)

      end subroutine fill_offset_free

end module davidson_parallel_idx
