module rpasimple
      
      use ccsd_transformed_integrals

      double precision, allocatable, dimension(:)       :: work, Etdhf
      double precision, allocatable, dimension(:,:)     :: macA, macB, AmB, ApB, omegarpa

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !trzeba podac: nocc, nvirt
      !trzeba zaimprotować: oovv, ovov, eorb
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

      subroutine odpalanie_rpa(nocc,nvirt, eorb)
            integer, intent(in)                               :: nocc,nvirt
            double precision, dimension(:), intent(in)        :: eorb
            integer                                           :: nrpa

            nrpa = nocc * nvirt
            print*, 'task RPA'

            call zerowanie(nocc, nvirt)

            call test_mp2(nocc, nvirt, eorb)
            call make_A_B(nocc, nvirt, eorb)
            call diagrpamatricesXpY(nrpa)
            call energia_rpa_od_alpha(nrpa)

            call wywalanie()

      end subroutine odpalanie_rpa

      subroutine wywalanie()
            deallocate(macA)
            deallocate(macB)
            deallocate(omegarpa)
            deallocate(work)
            deallocate(Etdhf)
            deallocate(ApB)
            deallocate(AmB)
      end subroutine wywalanie

      subroutine zerowanie(nocc, nvirt)
            integer, intent(in) :: nocc, nvirt
            integer :: i, j

            allocate(work(5*nvirt*nocc*nvirt*nocc))
            allocate(Etdhf(nocc*nvirt))
            allocate(macA(nocc*nvirt,nocc*nvirt))
            allocate(macB(nocc*nvirt,nocc*nvirt))
            allocate(ApB(nocc*nvirt,nocc*nvirt))
            allocate(AmB(nocc*nvirt,nocc*nvirt))
            allocate(omegarpa(nocc*nvirt,nocc*nvirt))
            do i = 1, nocc*nvirt
                  do j = 1, nocc*nvirt
                        macA(i, j) = 0d0
                        macB(i, j) = 0d0
                        ApB(i, j) = 0d0
                        AmB(i, j) = 0d0
                        omegarpa(i, j) = 0d0
                  end do
            end do
            do i = 1, nocc*nvirt
                  Etdhf(i) = 0d0
                  work(i) = 0d0
            end do
      end subroutine zerowanie

      subroutine test_mp2(nocc, nvirt, eorb)
            double precision :: mp2
            integer, intent(in) :: nocc, nvirt
            double precision, dimension(:), intent(in) :: eorb
            integer :: ia,ib,ik,ij
            mp2=0d0
            do  ik=1,nocc
                  do  ia=1,nvirt
                        do  ij=1,nocc
                              do  ib=1,nvirt        
                                    mp2=mp2 + (4d0* vovo(ia,ik,ib,ij)**2 - 2d0* vovo(ia,ik,ib,ij)*vovo(ib,ik,ia,ij))&
                                         &/(eorb(ik)+eorb(ij)-eorb(nocc+ia)-eorb(nocc+ib))
                              end do
                        end do
                  end do
            end do
            mp2 = mp2 * 0.5d0
      end subroutine test_mp2

      subroutine make_A_B(nocc, nvirt, eorb)
            integer :: iar,ia,ir,ibs,ib,is,i,j
            integer, intent(in) :: nocc, nvirt
            double precision, dimension(:), intent(in) :: eorb
            iar = 0
            do ia = 1, nocc
                  do ir = 1, nvirt
                        iar = iar + 1
                        ibs = 0
                        do ib = 1, nocc
                              do is = 1, nvirt        
                                    ibs = ibs + 1
                                    macA(iar, ibs) = (2d0 * vovo(ir,ia,is,ib) - vvoo(ir,is,ia,ib))
                                    if(ia.eq.ib.and.ir.eq.is) then
                                          macA(iar, ibs) = macA(iar, ibs) + (eorb(ir + nocc) - eorb(ia))
                                    end if
                                    macB(iar, ibs) = (2d0 * vovo(ir,ia,is,ib) - vovo(is,ia,ir,ib))
                              end do
                        end do
                  end do
            end do
            do i = 1, nocc*nvirt
                  do j = 1, nocc*nvirt
                        ApB(i,j) = macA(i,j) + macB(i,j)
                        AmB(i,j) = macA(i,j) - macB(i,j)
                  end do
            end do
      end subroutine make_A_B

      subroutine diagrpamatricesXpY(nrpa)
            IMPLICIT DOUBLE PRECISION (A-H,O-Z)
            IMPLICIT INTEGER (I-N)
            integer, intent(in) :: nrpa
            integer :: ii
            call dsygv(3, 'V', 'U', nrpa, ApB, nrpa, ApB, nrpa, Etdhf, work, -1, info)
            lwork = work(1)
            call dsygv(3, 'V', 'U', nrpa, ApB, nrpa, AmB, nrpa, Etdhf, work, lwork, info)
            do ii = 1, nrpa
                  omegarpa(ii, ii) = dsqrt(Etdhf(ii))
            end do
333         format(2I6,D18.8)
      end subroutine diagrpamatricesXpY

      subroutine energia_rpa_od_alpha(nrpa)
            integer :: q
            integer, intent(in) :: nrpa
            double precision :: xxxxx
            xxxxx = 0d0
            do q = 1, nrpa
                  xxxxx = xxxxx + omegarpa(q, q) - macA(q, q)
            end do
            print*, '----------------------------------'
            print*, 'Energia RPA od alpha'
            print*, 'E = ',0.5d0 * xxxxx 
      end subroutine energia_rpa_od_alpha

end module rpasimple
