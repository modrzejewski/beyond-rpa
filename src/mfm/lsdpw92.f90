module lsdpw92
contains

      SUBROUTINE LSDAC(RS, ZETA, POTLC, DLDS, DLDZ)
            DOUBLE PRECISION :: RS, ZETA
            DOUBLE PRECISION :: POTLC, DLDS, DLDZ

            DOUBLE PRECISION :: ALFC, VCUP, VCDN

            CALL CORLSD(RS, ZETA, POTLC, VCUP, VCDN, DLDS, DLDZ, ALFC)

            RETURN
      END SUBROUTINE LSDAC


      SUBROUTINE CORLSD(RS,ZET,EC,VCUP,VCDN,ECRS,ECZET,ALFC)
            !
            !  UNIFORM-GAS CORRELATION OF PERDEW AND WANG 1991
            !  INPUT: SEITZ RADIUS (RS), RELATIVE SPIN POLARIZATION (ZET)
            !  OUTPUT: CORRELATION ENERGY PER ELECTRON (EC), UP- AND DOWN-SPIN
            !     POTENTIALS (VCUP,VCDN), DERIVATIVES OF EC WRT RS (ECRS) & ZET (ECZET)
            !  OUTPUT: CORRELATION CONTRIBUTION (ALFC) TO THE SPIN STIFFNESS
            !
            IMPLICIT double precision (A-H,O-Z)
            double precision, parameter :: gam = 0.5198421D0
            double precision, parameter :: fzz = 1.709921D0
            double precision, parameter :: thrd=0.333333333333D0
            double precision, parameter :: thrd4=1.333333333333D0

            F = ((1.D0+ZET)**THRD4+(1.D0-ZET)**THRD4-2.D0)/GAM
            CALL GCOR(0.0310907D0,0.21370D0,7.5957D0,3.5876D0,1.6382D0, &
                  0.49294D0,1.00D0,RS,EU,EURS)
            CALL GCOR(0.01554535D0,0.20548D0,14.1189D0,6.1977D0,3.3662D0, &
                  0.62517D0,1.00D0,RS,EP,EPRS)
            CALL GCOR(0.0168869D0,0.11125D0,10.357D0,3.6231D0,0.88026D0, &
                  0.49671D0,1.00D0,RS,ALFM,ALFRSM)
            !
            !  ALFM IS MINUS THE SPIN STIFFNESS ALFC
            !
            ALFC = -ALFM
            Z4 = ZET**4
            EC = EU*(1.D0-F*Z4)+EP*F*Z4-ALFM*F*(1.D0-Z4)/FZZ
            !
            !  ENERGY DONE. NOW THE POTENTIAL:
            !
            ECRS = EURS*(1.D0-F*Z4)+EPRS*F*Z4-ALFRSM*F*(1.D0-Z4)/FZZ
            FZ = THRD4*((1.D0+ZET)**THRD-(1.D0-ZET)**THRD)/GAM
            ECZET = 4.D0*(ZET**3)*F*(EP-EU+ALFM/FZZ)+FZ*(Z4*EP-Z4*EU &
                  -(1.D0-Z4)*ALFM/FZZ)
            COMM = EC -RS*ECRS/3.D0-ZET*ECZET
            VCUP = COMM + ECZET
            VCDN = COMM - ECZET
            RETURN
      END SUBROUTINE CORLSD


      SUBROUTINE GCOR(A,A1,B1,B2,B3,B4,P,RS,GG,GGRS)
            !
            !  CALLED BY SUBROUTINE CORLSD
            !
            IMPLICIT double precision (A-H,O-Z)
            P1 = P + 1.D0
            Q0 = -2.D0*A*(1.D0+A1*RS)
            RS12 = DSQRT(RS)
            RS32 = RS12**3
            RSP = RS**P
            Q1 = 2.D0*A*(B1*RS12+B2*RS+B3*RS32+B4*RS*RSP)
            Q2 = DLOG(1.D0+1.D0/Q1)
            GG = Q0*Q2
            Q3 = A*(B1/RS12+2.D0*B2+3.D0*B3*RS12+2.D0*B4*P1*RSP)
            GGRS = -2.D0*A*A1*Q2-Q0*Q3/(Q1**2+Q1)
            RETURN
      END SUBROUTINE GCOR
end module lsdpw92
