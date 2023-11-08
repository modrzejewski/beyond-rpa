module drv_mp2
      use math_constants
      use arithmetic
      use gparam
      use basis
      use initialize
      use display
      use scf
      use scf_definitions
      use mp2

      implicit none

contains

      subroutine task_mp2_sp(molecule)
            type(tmolecule), intent(in) :: molecule

            double precision, dimension(:, :), allocatable :: mocoeff
            double precision, dimension(:), allocatable :: eorb
            double precision :: erhf, emp2, etot
            type(TSCFOutput) :: scfresults
            type(TSCFParams) :: SCFParams
            integer :: nocc, nvirt

            call data_load(molecule)
            call init_modules()

            if (IMG_ISMASTER) then
                  allocate(mocoeff(NORB, NORB))
                  allocate(eorb(NORB))
                  !
                  ! Perform self-consistent RHF
                  !
                  call SCFParams%init(XCF_HF)
                  call ksdriver(scfresults, SCFParams, mocoeff, eorb)
                  nocc = ne / 2
                  nvirt = norb - nocc - scfresults%nexcluded
                  erhf = scfresults%EtotDFT
                  !
                  ! Use RHF MO coefficients to generate MP2 correlation
                  !
                  call mp2corr(eorb, mocoeff, nocc, nvirt, 0, emp2)
                  etot = erhf + emp2
            
                  call toprule()
                  call msg("INTEGRAL DIRECT MP2 MODULE COMPLETED")
                  call midrule()
                  call dmsg("HF TOTAL ENERGY", erhf)
                  call dmsg("MP2 CORRELATION", emp2)
                  call dmsg("HF+MP2 ENERGY", etot)

                  deallocate(mocoeff)
                  deallocate(eorb)
            else
                  call ksgen_slave()
                  !
                  ! FIXME: No MPI parallelization for MP2.
                  !
            end if

            call free_modules()
            call data_free()
      end subroutine task_mp2_sp
end module drv_mp2
