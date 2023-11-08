module modrzej2012_c_energy
      use arithmetic
      use math_constants

      implicit none

      real(F64), parameter :: MLRCS12_C_RHO_THRESH = 1.0E-10_F64
      real(F64), parameter :: MLRCS12_C_TAU_THRESH = 1.0E-10_F64
      real(F64), parameter :: MLRCS12_C_QS_THRESH = 1.0E-15_F64

contains

      subroutine calcdaa(rs, rsrhoa, chi, daa, daarhoa, daachi, gparam)
            !
            ! Daa parameter should satisfy daa >= 0.5819 / rs
            ! for Vc(alpha-alpha) to be non-positive for 
            ! every lambda >= 0. 
            !
            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: rsrhoa
            real(F64), intent(in)  :: chi
            real(F64), intent(out) :: daa
            real(F64), intent(out) :: daarhoa
            real(F64), intent(out) :: daachi
            real(F64), intent(in)  :: gparam
            !
            ! The value of Fab has been optimized to yield accurate 
            ! parallel spin component of Vc (lambda = 1) function in the spin - 
            ! compensated HEG regime.
            !
            real(F64), parameter :: faa = 2.6422_F64
            real(F64) :: gaa
            real(F64) :: a0, a1
            
            gaa = gparam
            a0 = faa / rs
            a1 = -faa / rs**2 * rsrhoa
            daa = a0 + gaa * chi
            daarhoa = a1
            daachi = gaa
      end subroutine calcdaa

      
      subroutine calcdab(rs, rsrhoa, rsrhob, chi, dab, dabrhoa, dabrhob, dabchi, gparam)
            !
            ! Dab should satisfy dab >= 0.6834 / rs in order to
            ! Vc(alpha-beta) be non-positive for every lambda >= 0.
            !
            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: rsrhoa
            real(F64), intent(in)  :: rsrhob
            real(F64), intent(in)  :: chi
            real(F64), intent(out) :: dab
            real(F64), intent(out) :: dabrhoa
            real(F64), intent(out) :: dabrhob
            real(F64), intent(out) :: dabchi
            real(F64), intent(in)  :: gparam
            !
            ! The value of Fab has been optimized to yield accurate 
            ! opposite spin component of Vc (lambda = 1) function in the spin - 
            ! compensated HEG regime
            !
            real(F64), parameter :: fab = 2.1070_F64
            real(F64) :: gab
            real(F64) :: a0, a1a, a1b
            real(F64) :: rs2

            gab = gparam
            rs2 = rs**2
            a0 = fab / rs
            a1a = -fab / rs2 * rsrhoa
            a1b = -fab / rs2 * rsrhob

            dab = a0 + gab * chi
            dabrhoa = a1a
            dabrhob = a1b
            dabchi  = gab
      end subroutine calcdab


      subroutine mlrcs12_c(eps, ec_rho, ec_sigma, ec_tau, rho, sigma, tau, gparam)
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: ec_rho
            real(F64), intent(out) :: ec_sigma
            real(F64), intent(out) :: ec_tau
            real(F64), intent(in)  :: rho
            real(F64), intent(in)  :: sigma
            real(F64), intent(in)  :: tau
            real(F64), intent(in)  :: gparam
            
            real(F64) :: rho13, rho43, rho83
            real(F64) :: rs, rsrho, rsrhoa
            real(F64) :: rsaa, rsaa_rhoa
            real(F64) :: rsab, rsab_rhoa, rsab_rhob
            real(F64) :: eopp, eopp_rho, eopp_sigma
            real(F64) :: epar, epar_rho, epar_sigma, epar_tau

            real(F64) :: rhoa, rhob, sigma_aa, sigma_ab, taua
            real(F64) :: daa, dab
            real(F64) :: daarhoa, daachi
            real(F64) :: dabrhoa, dabrhob, dabchi
            
            real(F64) :: qa, qa_taua, qa_sigma_aa, qa_rhoa
            real(F64) :: vcab, vcab_rs, vcab_rhoa, vcab_rhob, vcab_dab
            real(F64) :: vcaa, vcaa_rs, vcaa_rhoa, vcaa_qa, vcaa_daa

            real(F64) :: tt
            real(F64) :: chi, chi_sigma, chi_rho

            real(F64), parameter :: seitz_coeff = (THREE/(FOUR * PI))**(ONE/THREE)
            
            if (rho > TWO*MLRCS12_C_RHO_THRESH) then
                  rho13 = rho**(ONE/THREE)
                  rho43 = rho * rho13
                  rho83 = rho43**2

                  rs = seitz_coeff / rho13
                  rsrho = -(ONE/THREE) * rs / rho
                  rsrhoa = rsrho
                  rsaa = rs
                  rsaa_rhoa = rsrho * two
                  rsab = rs
                  rsab_rhoa = rsrho
                  rsab_rhob = rsrho

                  chi = sigma / (rs * rho83)
                  chi_sigma = one / (rs * rho83)
                  chi_rho = -eight / three * chi / rho - chi / rs * rsrho

                  rhoa = (ONE/TWO) * rho
                  rhob = (ONE/TWO) * rho
                  sigma_aa = (ONE/FOUR) * sigma
                  sigma_ab = (ONE/FOUR) * sigma
                  taua = (ONE/TWO) * tau                  
                  qa = taua - (ONE/FOUR) * sigma_aa / rhoa
                  if (taua > MLRCS12_C_TAU_THRESH .and. qa > MLRCS12_C_QS_THRESH) then
                        qa_rhoa = (ONE/FOUR) * sigma_aa / (rhoa**2)
                        qa_sigma_aa = -(ONE/FOUR) / rhoa
                        qa_taua = one
                        call calcdaa(rsaa, rsaa_rhoa, chi, daa, daarhoa, daachi, gparam)
                        call mlrcs12_vcaa(rs, rhoa, qa, daa, vcaa, vcaa_rs, &
                              vcaa_rhoa, vcaa_qa, vcaa_daa)
                        tt = two * vcaa_daa * daachi
                        epar = two * vcaa
                        epar_rho = vcaa_rhoa + two * vcaa_rs * rsrho + vcaa_qa * qa_rhoa &
                              + vcaa_daa * daarhoa + tt * chi_rho
                        epar_sigma = (ONE/TWO) * vcaa_qa * qa_sigma_aa + tt * chi_sigma
                        epar_tau = vcaa_qa * qa_taua
                  else
                        epar = ZERO
                        epar_rho = ZERO
                        epar_sigma = ZERO
                        epar_tau = ZERO
                  end if

                  call calcdab(rsab, rsab_rhoa, rsab_rhob, chi, dab, dabrhoa, dabrhob, &
                        dabchi, gparam)
                  call mlrcs12_vcab(rs, rhoa, rhob, dab, vcab, vcab_rs, &
                        vcab_rhoa, vcab_rhob, vcab_dab)
                  tt = two * vcab_dab * dabchi
                  eopp = two * vcab
                  eopp_rho =  vcab_rhoa + vcab_rhob + two * vcab_rs * rsrho + tt * chi_rho &
                        + vcab_dab * (dabrhoa + dabrhob)
                  eopp_sigma = tt * chi_sigma
                  !
                  ! EPS is defined as correlation energy density
                  ! so EOPP and EPAR contributions should be
                  ! divided by RHO
                  !
                  eps = (eopp + epar) / rho
                  ec_rho = eopp_rho + epar_rho
                  ec_sigma = eopp_sigma + epar_sigma
                  ec_tau = epar_tau
            else
                  eps = ZERO
                  ec_rho = ZERO
                  ec_sigma = ZERO
                  ec_tau = ZERO
            end if
      end subroutine mlrcs12_c


      subroutine u_mlrcs12_c(eps, ec_rhoa, ec_rhob, ec_sigaa, ec_sigab, ec_sigbb, ec_taua, ec_taub, &
            rhoa, rhob, sigma_aa, sigma_ab, sigma_bb, taua, taub, gparam)
            real(F64), intent(out) :: eps
            real(F64), intent(out) :: ec_rhoa
            real(F64), intent(out) :: ec_rhob
            real(F64), intent(out) :: ec_sigaa
            real(F64), intent(out) :: ec_sigab
            real(F64), intent(out) :: ec_sigbb
            real(F64), intent(out) :: ec_taua
            real(F64), intent(out) :: ec_taub
            real(F64), intent(in)  :: rhoa
            real(F64), intent(in)  :: rhob
            real(F64), intent(in)  :: sigma_aa
            real(F64), intent(in)  :: sigma_ab
            real(F64), intent(in)  :: sigma_bb
            real(F64), intent(in)  :: taua
            real(F64), intent(in)  :: taub
            real(F64), intent(in)  :: gparam
            
            real(F64) :: rs, rsrho
            real(F64) :: rsaa, rsaa_rhoa
            real(F64) :: rsbb, rsbb_rhob
            real(F64) :: rsab, rsab_rhoa, rsab_rhob
            real(F64) :: rhoa13, rhob13
            real(F64) :: sigmatot, rhotot
            real(F64) :: chi, chi_rhoa, chi_rhob, chi_sigma_aa
            real(F64) :: chi_sigma_bb, chi_sigma_ab
            real(F64) :: rhotot13, rhotot43, rhotot83
            real(F64) :: daa, daarhoa, daachi
            real(F64) :: dbb, dbbrhob, dbbchi
            real(F64) :: dab, dabrhoa, dabrhob, dabchi
            real(F64) :: qa, qa_taua, qa_sigma_aa, qa_rhoa
            real(F64) :: qb, qb_taub, qb_sigma_bb, qb_rhob
            real(F64) :: vcab, vcab_rsab, vcab_rhoa, vcab_rhob, vcab_dab
            real(F64) :: vcaa, vcaa_rsaa, vcaa_rhoa, vcaa_qa, vcaa_daa
            real(F64) :: vcbb, vcbb_rsbb, vcbb_rhob, vcbb_qb, vcbb_dbb
            real(F64) :: tt

            real(F64), parameter :: seitz_coeff = (THREE/(FOUR * PI))**(ONE/THREE)
            real(F64), parameter :: rsab_coeff = (THREE/PI)**(ONE/THREE)
            real(F64), parameter :: rsaa_coeff = (ONE/TWO) * rsab_coeff
            real(F64), parameter :: rsbb_coeff = (ONE/TWO) * rsab_coeff

            eps = ZERO
            ec_rhoa = ZERO
            ec_rhob = ZERO
            ec_sigaa = ZERO
            ec_sigab = ZERO
            ec_sigbb = ZERO
            ec_taua = ZERO
            ec_taub = ZERO

            rhotot = rhoa + rhob
            if (rhotot > TWO*MLRCS12_C_RHO_THRESH) then
                  rhotot13 = rhotot**(ONE/THREE)
                  rhotot43 = rhotot13 * rhotot
                  rhotot83 = rhotot43**2
                  rs = seitz_coeff / rhotot13
                  rsrho = -(ONE/THREE) * rs / rhotot
                  sigmatot = sigma_aa + TWO * sigma_ab + sigma_bb
                  chi = sigmatot / (rs * rhotot83)
                  chi_sigma_aa = one / (rs * rhotot83)
                  chi_sigma_bb = chi_sigma_aa
                  chi_sigma_ab = two * chi_sigma_aa
                  chi_rhoa = -(EIGHT/THREE) * chi / rhotot - chi / rs * rsrho
                  chi_rhob = chi_rhoa

                  if (rhoa > MLRCS12_C_RHO_THRESH) then
                        !
                        ! Qa is always be non-negative
                        ! due to the Schwartz inequality.
                        ! Might be slightly negative due to
                        ! numerical errors.
                        !
                        qa = taua - (ONE/FOUR) * sigma_aa / rhoa
                        if (taua > MLRCS12_C_TAU_THRESH .and. qa > MLRCS12_C_QS_THRESH) then
                              rhoa13 = rhoa**(ONE/THREE)
                              rsaa = rsaa_coeff / rhoa13
                              rsaa_rhoa = -(ONE/THREE) * rsaa / rhoa
                              qa_rhoa = (ONE/FOUR) * sigma_aa / (rhoa**2)
                              qa_sigma_aa = -(ONE/FOUR) / rhoa
                              qa_taua = one
                              call calcdaa(rsaa, rsaa_rhoa, chi, daa, daarhoa, daachi, gparam)
                              call mlrcs12_vcaa(rsaa, rhoa, qa, daa, vcaa, vcaa_rsaa, &
                                    vcaa_rhoa, vcaa_qa, vcaa_daa)
                              tt = vcaa_daa * daachi
                              eps       = eps + vcaa / rhotot
                              ec_rhoa   = ec_rhoa + (vcaa_rhoa + vcaa_rsaa * rsaa_rhoa &
                                    + vcaa_qa * qa_rhoa + vcaa_daa * daarhoa) + tt * chi_rhoa
                              ec_rhob   = ec_rhob + tt * chi_rhob
                              ec_sigaa = ec_sigaa + vcaa_qa * qa_sigma_aa + tt * chi_sigma_aa
                              ec_sigab = ec_sigab + tt * chi_sigma_ab
                              ec_sigbb = ec_sigbb + tt * chi_sigma_bb
                              ec_taua   = ec_taua + vcaa_qa * qa_taua
                        end if
                  end if

                  if (rhob > MLRCS12_C_RHO_THRESH) then
                        qb = taub - (ONE/FOUR) * sigma_bb / rhob
                        if (taub > MLRCS12_C_TAU_THRESH .and. qb > MLRCS12_C_QS_THRESH) then
                              rhob13 = rhob**(ONE/THREE)
                              rsbb = rsbb_coeff / rhob13
                              rsbb_rhob = -(ONE/THREE) * rsbb / rhob
                              qb_rhob = (ONE/FOUR) * sigma_bb / (rhob**2)
                              qb_sigma_bb = -(ONE/FOUR) / rhob
                              qb_taub = one
                              call calcdaa(rsbb, rsbb_rhob, chi, dbb, dbbrhob, dbbchi, gparam)
                              call mlrcs12_vcaa(rsbb, rhob, qb, dbb, vcbb, vcbb_rsbb, &
                                    vcbb_rhob, vcbb_qb, vcbb_dbb)
                              tt = vcbb_dbb * dbbchi
                              eps       = eps + vcbb / rhotot
                              ec_rhoa   = ec_rhoa + tt * chi_rhoa
                              ec_rhob   = ec_rhob + (vcbb_rhob + vcbb_rsbb * rsbb_rhob &
                                    + vcbb_qb * qb_rhob + vcbb_dbb * dbbrhob) + tt * chi_rhob
                              ec_sigaa  = ec_sigaa + tt * chi_sigma_aa
                              ec_sigab  = ec_sigab + tt * chi_sigma_ab
                              ec_sigbb  = ec_sigbb + vcbb_qb * qb_sigma_bb + tt * chi_sigma_bb
                              ec_taub   = ec_taub + vcbb_qb * qb_taub
                        end if
                  end if

                  if ((rhoa > MLRCS12_C_RHO_THRESH) .and. & 
                        (rhob > MLRCS12_C_RHO_THRESH)) then                        
                        rhoa13 = rhoa**(ONE/THREE)
                        rhob13 = rhob**(ONE/THREE)
                        rsab = rsab_coeff / (rhoa13 + rhob13)
                        tt = -(ONE/THREE) * rsab_coeff / (rhoa13 + rhob13)**2
                        rsab_rhoa = tt / rhoa13**2
                        rsab_rhob = tt / rhob13**2
                        call calcdab(rsab, rsab_rhoa, rsab_rhob, chi, dab, &
                              dabrhoa, dabrhob, dabchi, gparam)
                        call mlrcs12_vcab(rsab, rhoa, rhob, dab, vcab, vcab_rsab, &
                              vcab_rhoa, vcab_rhob, vcab_dab)
                        tt = two * vcab_dab * dabchi
                        eps = eps + two * vcab / rhotot
                        ec_rhoa = ec_rhoa + two * (vcab_rhoa + &
                              vcab_rsab * rsab_rhoa + vcab_dab * dabrhoa) + tt * chi_rhoa
                        ec_rhob = ec_rhob + two * (vcab_rhob + &
                              vcab_rsab * rsab_rhob + vcab_dab * dabrhob) + tt * chi_rhob
                        ec_sigaa = ec_sigaa + tt * chi_sigma_aa
                        ec_sigab = ec_sigab + tt * chi_sigma_ab
                        ec_sigbb = ec_sigbb + tt * chi_sigma_bb
                  end if
            end if
      end subroutine u_mlrcs12_c


      subroutine mlrcs12_vcab(rs, rhoa, rhob, dab, vcab, vcab_rs, &
            vcab_rhoa, vcab_rhob, vcab_dab)

            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: rhoa
            real(F64), intent(in)  :: rhob
            real(F64), intent(in)  :: dab
            real(F64), intent(out) :: vcab
            real(F64), intent(out) :: vcab_rs
            real(F64), intent(out) :: vcab_rhoa
            real(F64), intent(out) :: vcab_rhob
            real(F64), intent(out) :: vcab_dab

            real(F64) :: a0opp, a0opprs
            real(F64) :: la0opp, la0opprs
            real(F64) :: aab, aab_rhob, aab_rs
            real(F64) :: bab, bab_rhob, bab_rs, bab_dab
            real(F64) :: dab2, dab3, dab4
            real(F64) :: vcab_aab, vcab_bab

            dab2 = dab**2
            dab3 = dab2 * dab
            dab4 = dab2 * dab2

            call mlrcs12_a0opp_averaged(rs, a0opp, a0opprs)
            call mlrcs12_la0opp_averaged(rs, la0opp, la0opprs)
            
            aab = rhob * a0opp - rhob
            bab = rhob * la0opp + dab * aab
            vcab = PI * rhoa * (bab + aab * dab) / dab3
            vcab_rhoa = PI * (bab + aab * dab) / dab3
            vcab_bab = PI * rhoa / dab3
            vcab_aab = PI * rhoa / dab2

            aab_rhob = a0opp - one
            aab_rs = rhob * a0opprs
            bab_rhob = la0opp + dab * aab_rhob
            bab_rs = rhob * la0opprs + dab * aab_rs
            bab_dab = aab

            vcab_rhob = vcab_aab * aab_rhob + vcab_bab * bab_rhob
            vcab_dab = PI * rhoa * (-three * bab / dab4 - two * aab / dab3) + vcab_bab * bab_dab
            vcab_rs = vcab_aab * aab_rs + vcab_bab * bab_rs
      end subroutine mlrcs12_vcab


      subroutine mlrcs12_vcaa(rs, rhoa, qa, daa, vcaa, vcaa_rs, &
            vcaa_rhoa, vcaa_qa, vcaa_daa)

            real(F64), intent(in)  :: rs
            real(F64), intent(in)  :: rhoa
            real(F64), intent(in)  :: qa
            real(F64), intent(in)  :: daa
            real(F64), intent(out) :: vcaa
            real(F64), intent(out) :: vcaa_rs
            real(F64), intent(out) :: vcaa_rhoa
            real(F64), intent(out) :: vcaa_qa
            real(F64), intent(out) :: vcaa_daa

            real(F64) :: a2par, a2parrs
            real(F64) :: la2par, la2parrs
            real(F64) :: aaa, aaa_rs, aaa_qa
            real(F64) :: baa, baa_rs, baa_daa, baa_qa
            real(F64) :: daa2, daa3, daa4, daa5, daa6
            real(F64) :: vcaa_aaa, vcaa_baa

            daa2 = daa**2
            daa3 = daa2 * daa
            daa4 = daa2 * daa2
            daa5 = daa3 * daa2
            daa6 = daa3 * daa3

            call mlrcs12_a2par_averaged(rs, a2par, a2parrs)
            call mlrcs12_la2par_averaged(rs, la2par, la2parrs)
            
            aaa = (ONE/THREE) * (qa * a2par - qa)
            baa = (ONE/SIX) * qa * la2par + aaa * daa
            vcaa = PI * rhoa * (eight * baa + four * aaa * daa) / daa5
            vcaa_rhoa = PI * (eight * baa + four * aaa * daa) / daa5
            vcaa_baa = PI * rhoa * eight / daa5
            vcaa_aaa = PI * rhoa * four / daa4

            aaa_qa = (ONE/THREE) * (a2par - one)
            aaa_rs = (ONE/THREE) * qa * a2parrs
            baa_qa = (ONE/SIX) * la2par + daa * aaa_qa
            baa_rs = (ONE/SIX) * qa * la2parrs + daa * aaa_rs
            baa_daa = aaa

            vcaa_qa = vcaa_aaa * aaa_qa + vcaa_baa * baa_qa
            vcaa_daa = PI * rhoa * (-40_F64 * baa - 16_F64 * daa * aaa) / daa6 + vcaa_baa * baa_daa
            vcaa_rs = vcaa_aaa * aaa_rs + vcaa_baa * baa_rs
      end subroutine mlrcs12_vcaa


      subroutine mlrcs12_a2par_averaged(rs, a2par, a2parrs)
            ! ----------------------------------------------------------
            ! Second order coefficient of short range expansion
            ! of parallel spin pair distribution function. Interpolation
            ! satisfies UEG high-density limit. Parametrization based on
            ! solution of Overhauser equation by Gori-Giorgi and Perdew
            ! (no prefactor).
            ! ----------------------------------------------------------
            ! 1. Gori-Giorgi, P., Perdew, J., Short-range correlation
            !    in the uniform electron gas: Extended Overhauser
            !    model, Phys. Rev. B., 64, 155102(2001).
            ! ---------------------------------------------------------
            ! RS     - Seitz radius corresponding to RHO density. 
            !          RS = (3/(4PI))**(1/3) * RHO**(-1/3)
            ! A2PAR  - Output, value of the pair distribution function
            ! A2PARRS- Output, value of derivative of pair distribution 
            !          function with respect to RS
            !
            real(F64), intent(in)  :: rs
            real(F64), intent(out) :: a2par
            real(F64), intent(out) :: a2parrs
            !
            ! A2PAR = 1/rs*((-a + b*rs + c*rs^2)*exp(-alpha*rs) + a)
            !
            real(F64), parameter :: a = 1.77482197431142_F64   
            real(F64), parameter :: b = 0.012134089098266_F64   
            real(F64), parameter :: c = -0.00474308300395257_F64    
            real(F64), parameter :: alpha = 0.5566_F64
            
            real(F64) :: rs2
            real(F64) :: exp0, exp1, f0, f1           
            
            rs2 = rs**2
            
            f0 = -a + b * rs + c * rs2 
            f1 = b + two * c * rs
            exp0 = exp(-alpha * rs)
            exp1 = -alpha * exp0
            
            a2par = (f0 * exp0 + a) / rs
            a2parrs = (f1 * exp0 + f0 * exp1) / rs - (f0 * exp0 + a) / rs2
      end subroutine mlrcs12_a2par_averaged


      subroutine mlrcs12_la2par_averaged(rs, la2par, la2parrs)
            !
            ! (LAMBDA * A2_{PAR} averaged over the coupling constant
            !
            real(F64), intent(in)  :: rs
            real(F64), intent(out) :: la2par
            real(F64), intent(out) :: la2parrs
            !
            ! LA2PAR = 1/rs^2*((-a + b*rs + c*rs^2 + d*rs^3)*exp(-alpha*rs) + a)
            !
            real(F64), parameter :: a = 3.20453025029445_F64
            real(F64), parameter :: b = -1.78364153731389_F64
            real(F64), parameter :: c = 0.00361256016554491_F64
            real(F64), parameter :: d = -0.00474308300395257_F64
            real(F64), parameter :: alpha = 0.5566_F64
            
            real(F64) :: rs2, rs3
            real(F64) :: exp0, exp1, f0, f1           
            
            rs2 = rs**2
            rs3 = rs2 * rs
            
            f0 = -a + b * rs + c * rs2 + d * rs3
            f1 = b + two * c * rs + three * d * rs2
            exp0 = exp(-alpha * rs)
            exp1 = -alpha * exp0
            
            la2par = (f0 * exp0 + a) / rs2
            la2parrs = (f1 * exp0 + f0 * exp1) / rs2 - two * (f0 * exp0 + a) / rs3
      end subroutine mlrcs12_la2par_averaged


      subroutine mlrcs12_a0opp_averaged(rs, a0opp, a0opprs)
            ! ---------------------------------------------------------
            ! On-top opposite spin pair distribution function.
            ! Interpolation satisfying UEG high-density limit.
            ! Parametrization is based on solution of Overhauser
            ! equation by Gori-Giorgi and Perdew.
            ! ---------------------------------------------------------
            ! 1. Gori-Giorgi, P., Perdew, J., Short-range correlation
            !    in the uniform electron gas: Extended Overhauser
            !    model, Phys. Rev. B., 64, 155102(2001).
            ! ---------------------------------------------------------
            ! RS     - Seitz radius corresponding to RHO density. 
            !          RS = (3/(4PI))**(1/3) * RHO**(-1/3)
            ! A0OPP  - Output, value of the pair distribution function
            ! A0OPPRS- Output, value of derivative of pair distribution 
            !          function with respect to RS
            !
            real(F64), intent(in)  :: rs
            real(F64), intent(out) :: a0opp
            real(F64), intent(out) :: a0opprs
            !
            ! A0OPP = 1/rs*((-a + b*rs + c*rs^2 + d*rs3 + e*rs^4)*exp(-alpha*rs) + a)
            !
            real(F64), parameter :: a = 1.69629965463158_F64
            real(F64), parameter :: b = -0.276295860144799_F64
            real(F64), parameter :: c = -0.0935925025864735_F64
            real(F64), parameter :: d = 0.00383700035131244_F64  
            real(F64), parameter :: e = -0.00247076023391813_F64 
            real(F64), parameter :: alpha = 0.7524_F64
            
            real(F64) :: rs2, rs3, rs4
            real(F64) :: exp0, exp1, f0, f1
            
            rs2 = rs**2
            rs3 = rs2 * rs
            rs4 = rs2 * rs2
            
            f0 = -a + b * rs + c * rs2 + d * rs3 + e * rs4
            f1 = b + two * c * rs + three * d * rs2 + four * e * rs3
            exp0 = exp(-alpha * rs)
            exp1 = -alpha * exp0
            
            a0opp = (f0 * exp0 + a) / rs
            a0opprs = (f1 * exp0 + f0 * exp1) / rs - (f0 * exp0 + a) / rs2
      end subroutine mlrcs12_a0opp_averaged


      subroutine mlrcs12_la0opp_averaged(rs, la0opp, la0opprs)
            !
            ! (LAMBDA * A0_{OPP} averaged over the coupling constant
            !
            real(F64), intent(in)  :: rs
            real(F64), intent(out) :: la0opp
            real(F64), intent(out) :: la0opprs
            !
            ! A0OPP = 1/rs^2*((-a + b*rs + c*rs^2 
            !                   + d*rs^3 + e*rs^4 + f*rs^5)*exp(-alpha*rs) + a)
            !
            real(F64), parameter :: a = 3.3561331935824_F64
            real(F64), parameter :: b = -2.52515461485139_F64
            real(F64), parameter :: c = -0.449963166107094_F64
            real(F64), parameter :: d = -0.105950762059659_F64
            real(F64), parameter :: e = 0.000553161656578094_F64 
            real(F64), parameter :: f = -0.00247076023391813_F64
            real(F64), parameter :: alpha = 0.7524_F64
            
            real(F64) :: rs2, rs3, rs4, rs5
            real(F64) :: exp0, exp1, f0, f1
            
            rs2 = rs**2
            rs3 = rs2 * rs
            rs4 = rs2 * rs2
            rs5 = rs4 * rs
            
            f0 = -a + b * rs + c * rs2 + d * rs3 + e * rs4 + f * rs5
            f1 = b + two * c * rs + three * d * rs2 + four * e * rs3 + five * f * rs4
            exp0 = exp(-alpha * rs)
            exp1 = -alpha * exp0
            
            la0opp = (f0 * exp0 + a) / rs2
            la0opprs = (f1 * exp0 + f0 * exp1) / rs2 - two * (f0 * exp0 + a) / rs3 
      end subroutine mlrcs12_la0opp_averaged
end module modrzej2012_c_energy
