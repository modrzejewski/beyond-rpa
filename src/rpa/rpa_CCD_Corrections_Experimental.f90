module rpa_CCD_Corrections_Experimental
      use arithmetic      
      use real_linalg
      use rpa_definitions
      use clock
      
      implicit none

contains

      subroutine rpa_CCD_corrections_FullSet(Energy, Zgh, Yga, Xgi, OccEnergies, VirtEnergies, &
            Uaim, Am, NOcc, NVirt, NVecsT2, NGridTHC)
            !
            ! Energy
            !                  Output of the subprogram. On extit, this subroutine
            !                  is expected to update the total energy components
            !                  corresponding to the beyond-RPA corrections. Note
            !                  that Energy contains other components as well,
            !                  e.g., EtotHF, EcRPA, ... All those components are
            !                  processed at the end of each run to display
            !                  the summary of energy contributions.
            !
            ! The energy components should be referenced using the pointers
            ! (indices) defined as parameters in rpa_definitions. The pointers
            ! corresponding to the beyond-RPA corrections are as follows.
            !
            ! Energy(RPA_ENERGY_CUMULANT_1B) = Ec1b ! note that there is no 1a
            ! Energy(RPA_ENERGY_CUMULANT_2B) = Ec2b ! note that there is no 2a
            ! Energy(RPA_ENERGY_CUMULANT_2C) = Ec2c
            ! Energy(RPA_ENERGY_CUMULANT_2D) = Ec2d
            !                              ...
            ! Energy(RPA_ENERGY_CUMULANT_2L) = Ec2l
            !                  
            !
            ! NOcc, NVirt
            !                  Number of occupied (virtual) orbitals
            ! NVecsT2
            !                  Number of the eigenvectors of T2
            ! NGridTHC
            !                  Number of THC vectors, also referred to
            !                  as THC grid points
            !
            ! Zgh              THC grid representation of the Coulomb
            !                  operator
            ! Yga, Xgi         THC collocation matrices transformed to
            !                  the basis of virtual (occupied) MOs.
            !
            ! Example of THC usage:
            !
            ! (ai|bc) = Sum(g=1,NGridTHC,h=1,NGridTHC) Yga(g,a)*Xgi(g,i)*Yga(h,b)*Yga(h,c)*Zgh(g,h)
            !
            ! OccEnergies
            ! VirtEnergies
            !                  Energies of occupied (virtual) orbitals:
            !                  OccEnergies(i), i = 1, ..., NOcc
            !                  VirtEnergies(a), a = 1, ..., NVirt
            !
            ! Uaim
            ! Am
            !                  Eigenvectors and eigenvalues of T2
            !                  Uaim(a,i,mu), Am(mu)
            !                  a = 1, ..., NVirt
            !                  i = 1, ..., NOcc
            !                  mu = 1, ..., NVecsT2
            !                  
            !
            !
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NVecsT2
            integer, intent(in)                                    :: NGridTHC
            real(F64), dimension(:), intent(inout)                 :: Energy
            real(F64), dimension(:, :), intent(in)                 :: Zgh
            real(F64), dimension(NGridTHC, NVirt), intent(in)      :: Yga
            real(F64), dimension(NGridTHC, NOcc), intent(in)       :: Xgi
            real(F64), dimension(NOcc), intent(in)                 :: OccEnergies
            real(F64), dimension(NVirt), intent(in)                :: VirtEnergies
            real(F64), dimension(NVirt, NOcc, NVecsT2), intent(in) :: Uaim
            real(F64), dimension(:), intent(in)                    :: Am

            type(TClock) :: timer_total, timer

            call msg("CCD corrections to RPA correlation energy (experimental version)")
            call clock_start(timer_total)
            call clock_start(timer)

            ! ---------------------- przykład obliczenia wkładów 1b, 2g ----
            ! ------------------ można usunąć później ----------------------
            block
                  integer :: a, i, mu, g, h
                  real(F64) :: Ec2g, Ec1b
                  real(F64), dimension(:, :), allocatable :: YXUgh

                  print *, "----------------- calculation --"
                  
                  allocate(YXUgh(NGridTHC, NGridTHC))

                  Ec1b = ZERO
                  Ec2g = ZERO
                  do mu = 1, NVecsT2
                        print *, "mu=", mu, "Am(mu) =" , Am(mu)
                        
                        YXUgh = ZERO
                        do h = 1, NGridTHC
                              do g = 1, NGridTHC
                                    !
                                    ! [YU](gamma,i,mu) = Sum(a) Y(gamma,a)*U(a,i,mu)
                                    !
                                    ! [YXU](gamma,delta,mu) = Sum(i) X(delta,i)*[YU](gamma,i,mu)
                                    !
                                    do i = 1, NOcc
                                          do a = 1, NVirt
                                                YXUgh(g, h) = YXUgh(g, h) + Yga(g, a) * Xgi(h, i) * Uaim(a, i, mu)
                                          end do
                                    end do
                              end do
                        end do
                        !
                        ! THC formulas for 1b, 2g (see Eqs. 59, 60 in the JCTC paper)
                        !
                        do h = 1, NGridTHC
                              do g = 1, NGridTHC
                                    Ec2g = Ec2g + YXUgh(g, h)**2 * Zgh(g, h) * Am(mu)**2
                                    Ec1b = Ec1b + YXUgh(g, h) * YXUgh(h, g) * Zgh(g, h) * Am(mu)
                              end do
                        end do
                  end do
                  Ec1b = -ONE * Ec1b
                  Ec2g = -FOUR * Ec2g
                  !
                  ! Save the energy components in the output array
                  !
                  Energy(RPA_ENERGY_CUMULANT_1B) = Ec1b
                  Energy(RPA_ENERGY_CUMULANT_2G) = Ec2g
            end block
            ! --------------------------------------------------------------

            
            !
            ! Rescale the energy terms to get the correct MBPT prefactors.
            ! After scaling by 1/2, the 1b term is equivalent to SOSEX.
            !
            Energy(RPA_ENERGY_CUMULANT_1B) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_1B)
            Energy(RPA_ENERGY_CUMULANT_2B) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_2B)
            Energy(RPA_ENERGY_CUMULANT_2C) = (ONE/TWO) * Energy(RPA_ENERGY_CUMULANT_2C)
            call msg("CCD corrections (experimental version) computed in " // str(clock_readwall(timer_total),d=1) // " seconds")
      end subroutine rpa_CCD_corrections_FullSet
end module rpa_CCD_Corrections_Experimental
