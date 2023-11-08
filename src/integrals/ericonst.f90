module ericonst
      use gparam

      implicit none

      integer, parameter :: BRA1_S = 2**0
      integer, parameter :: BRA1_P = 2**1
      integer, parameter :: BRA1_D = 2**2
      integer, parameter :: BRA1_F = 2**3
      integer, parameter :: BRA1_L = 2**4

      integer, parameter :: BRA2_S = 2**5
      integer, parameter :: BRA2_P = 2**6
      integer, parameter :: BRA2_D = 2**7
      integer, parameter :: BRA2_F = 2**8
      integer, parameter :: BRA2_L = 2**9

      integer, parameter :: KET1_S = 2**10
      integer, parameter :: KET1_P = 2**11
      integer, parameter :: KET1_D = 2**12
      integer, parameter :: KET1_F = 2**13
      integer, parameter :: KET1_L = 2**14

      integer, parameter :: KET2_S = 2**15
      integer, parameter :: KET2_P = 2**16
      integer, parameter :: KET2_D = 2**17
      integer, parameter :: KET2_F = 2**18
      integer, parameter :: KET2_L = 2**19

      integer, parameter :: KET_SS = ior(KET1_S, KET2_S)
      integer, parameter :: KET_SP = ior(KET1_S, KET2_P)
      integer, parameter :: KET_SD = ior(KET1_S, KET2_D)
      integer, parameter :: KET_SF = ior(KET1_S, KET2_F)
      integer, parameter :: KET_SL = ior(KET1_S, KET2_L)

      integer, parameter :: KET_PS = ior(KET1_P, KET2_S)
      integer, parameter :: KET_PP = ior(KET1_P, KET2_P)
      integer, parameter :: KET_PD = ior(KET1_P, KET2_D)
      integer, parameter :: KET_PF = ior(KET1_P, KET2_F)
      integer, parameter :: KET_PL = ior(KET1_P, KET2_L)

      integer, parameter :: KET_DS = ior(KET1_D, KET2_S)
      integer, parameter :: KET_DP = ior(KET1_D, KET2_P)
      integer, parameter :: KET_DD = ior(KET1_D, KET2_D)
      integer, parameter :: KET_DF = ior(KET1_D, KET2_F)
      integer, parameter :: KET_DL = ior(KET1_D, KET2_L)

      integer, parameter :: KET_FS = ior(KET1_F, KET2_S)
      integer, parameter :: KET_FP = ior(KET1_F, KET2_P)
      integer, parameter :: KET_FD = ior(KET1_F, KET2_D)
      integer, parameter :: KET_FF = ior(KET1_F, KET2_F)
      integer, parameter :: KET_FL = ior(KET1_F, KET2_L)

      integer, parameter :: KET_LS = ior(KET1_L, KET2_S)
      integer, parameter :: KET_LP = ior(KET1_L, KET2_P)
      integer, parameter :: KET_LD = ior(KET1_L, KET2_D)
      integer, parameter :: KET_LF = ior(KET1_L, KET2_F)
      integer, parameter :: KET_LL = ior(KET1_L, KET2_L)

      integer, parameter :: BRA_SS = ior(BRA1_S, BRA2_S)
      integer, parameter :: BRA_SP = ior(BRA1_S, BRA2_P)
      integer, parameter :: BRA_SD = ior(BRA1_S, BRA2_D)
      integer, parameter :: BRA_SF = ior(BRA1_S, BRA2_F)
      integer, parameter :: BRA_SL = ior(BRA1_S, BRA2_L)

      integer, parameter :: BRA_PS = ior(BRA1_P, BRA2_S)
      integer, parameter :: BRA_PP = ior(BRA1_P, BRA2_P)
      integer, parameter :: BRA_PD = ior(BRA1_P, BRA2_D)
      integer, parameter :: BRA_PF = ior(BRA1_P, BRA2_F)
      integer, parameter :: BRA_PL = ior(BRA1_P, BRA2_L)

      integer, parameter :: BRA_DS = ior(BRA1_D, BRA2_S)
      integer, parameter :: BRA_DP = ior(BRA1_D, BRA2_P)
      integer, parameter :: BRA_DD = ior(BRA1_D, BRA2_D)
      integer, parameter :: BRA_DF = ior(BRA1_D, BRA2_F)
      integer, parameter :: BRA_DL = ior(BRA1_D, BRA2_L)

      integer, parameter :: BRA_FS = ior(BRA1_F, BRA2_S)
      integer, parameter :: BRA_FP = ior(BRA1_F, BRA2_P)
      integer, parameter :: BRA_FD = ior(BRA1_F, BRA2_D)
      integer, parameter :: BRA_FF = ior(BRA1_F, BRA2_F)
      integer, parameter :: BRA_FL = ior(BRA1_F, BRA2_L)

      integer, parameter :: BRA_LS = ior(BRA1_L, BRA2_S)
      integer, parameter :: BRA_LP = ior(BRA1_L, BRA2_P)
      integer, parameter :: BRA_LD = ior(BRA1_L, BRA2_D)
      integer, parameter :: BRA_LF = ior(BRA1_L, BRA2_F)
      integer, parameter :: BRA_LL = ior(BRA1_L, BRA2_L)

      integer, parameter :: ERI_SSSS = ior(BRA_SS, KET_SS)

      integer, parameter :: ERI_SPSS = ior(BRA_SP, KET_SS)
      integer, parameter :: ERI_SSSP = ior(BRA_SS, KET_SP)
      integer, parameter :: ERI_PSSS = ior(BRA_PS, KET_SS)
      integer, parameter :: ERI_SSPS = ior(BRA_SS, KET_PS)

      integer, parameter :: ERI_SPSP = ior(BRA_SP, KET_SP)
      integer, parameter :: ERI_PSSP = ior(BRA_PS, KET_SP)
      integer, parameter :: ERI_SPPS = ior(BRA_SP, KET_PS)
      integer, parameter :: ERI_PSPS = ior(BRA_PS, KET_PS)

      integer, parameter :: ERI_PPSS = ior(BRA_PP, KET_SS)
      integer, parameter :: ERI_SSPP = ior(BRA_SS, KET_PP)

      integer, parameter :: ERI_PPSP = ior(BRA_PP, KET_SP)
      integer, parameter :: ERI_PPPS = ior(BRA_PP, KET_PS)
      integer, parameter :: ERI_SPPP = ior(BRA_SP, KET_PP)
      integer, parameter :: ERI_PSPP = ior(BRA_PS, KET_PP)

      integer, parameter :: ERI_PPPP = ior(BRA_PP, KET_PP)

      integer, parameter :: ERI_LLPP = ior(BRA_LL, KET_PP)
      integer, parameter :: ERI_PPLL = ior(BRA_PP, KET_LL)

      integer, parameter :: ERI_LLSP = ior(BRA_LL, KET_SP)
      integer, parameter :: ERI_LLPS = ior(BRA_LL, KET_PS)
      integer, parameter :: ERI_SPLL = ior(BRA_SP, KET_LL)
      integer, parameter :: ERI_PSLL = ior(BRA_PS, KET_LL)

      integer, parameter :: ERI_LLSS = ior(BRA_LL, KET_SS)
      integer, parameter :: ERI_SSLL = ior(BRA_SS, KET_LL)

      integer, parameter :: ERI_DSSS = ior(BRA_DS, KET_SS)
      integer, parameter :: ERI_SDSS = ior(BRA_SD, KET_SS)
      integer, parameter :: ERI_SSDS = ior(BRA_SS, KET_DS)
      integer, parameter :: ERI_SSSD = ior(BRA_SS, KET_SD)

      integer, parameter :: ERI_DDSS = ior(BRA_DD, KET_SS)
      integer, parameter :: ERI_SSDD = ior(BRA_SS, KET_DD)

      integer, parameter :: ERI_DSDS = ior(BRA_DS, KET_DS)
      integer, parameter :: ERI_SDDS = ior(BRA_SD, KET_DS)
      integer, parameter :: ERI_SDSD = ior(BRA_SD, KET_SD)
      integer, parameter :: ERI_DSSD = ior(BRA_DS, KET_SD)

      integer, parameter :: ERI_DPPP = ior(BRA_DP, KET_PP)
      integer, parameter :: ERI_PDPP = ior(BRA_PD, KET_PP)
      integer, parameter :: ERI_PPDP = ior(BRA_PP, KET_DP)
      integer, parameter :: ERI_PPPD = ior(BRA_PP, KET_PD)

      integer, parameter :: ERI_DPSS = ior(BRA_DP, KET_SS)
      integer, parameter :: ERI_PDSS = ior(BRA_PD, KET_SS)
      integer, parameter :: ERI_SSDP = ior(BRA_SS, KET_DP)
      integer, parameter :: ERI_SSPD = ior(BRA_SS, KET_PD)

      integer, parameter :: ERI_DSPP = ior(BRA_DS, KET_PP)
      integer, parameter :: ERI_SDPP = ior(BRA_SD, KET_PP)
      integer, parameter :: ERI_PPDS = ior(BRA_PP, KET_DS)
      integer, parameter :: ERI_PPSD = ior(BRA_PP, KET_SD)

      integer, parameter :: ERI_DDPS = ior(BRA_DD, KET_PS)
      integer, parameter :: ERI_DDSP = ior(BRA_DD, KET_SP)
      integer, parameter :: ERI_PSDD = ior(BRA_PS, KET_DD)
      integer, parameter :: ERI_SPDD = ior(BRA_SP, KET_DD)

      integer, parameter :: ERI_FSSS = ior(BRA_FS, KET_SS)
      integer, parameter :: ERI_SFSS = ior(BRA_SF, KET_SS)
      integer, parameter :: ERI_SSFS = ior(BRA_SS, KET_FS)
      integer, parameter :: ERI_SSSF = ior(BRA_SS, KET_SF)

      integer, parameter :: ERI_FPSS = ior(BRA_FP, KET_SS)
      integer, parameter :: ERI_PFSS = ior(BRA_PF, KET_SS)
      integer, parameter :: ERI_SSFP = ior(BRA_SS, KET_FP)
      integer, parameter :: ERI_SSPF = ior(BRA_SS, KET_PF)

      integer, parameter :: ERI_FDSS = ior(BRA_FD, KET_SS)
      integer, parameter :: ERI_DFSS = ior(BRA_DF, KET_SS)
      integer, parameter :: ERI_SSFD = ior(BRA_SS, KET_FD)
      integer, parameter :: ERI_SSDF = ior(BRA_SS, KET_DF)
      
      integer, parameter :: ERI_FSPP = ior(BRA_FS, KET_PP)
      integer, parameter :: ERI_SFPP = ior(BRA_SF, KET_PP)
      integer, parameter :: ERI_PPFS = ior(BRA_PP, KET_FS)
      integer, parameter :: ERI_PPSF = ior(BRA_PP, KET_SF)
      !
      ! --------------- Canonical order ----------------
      ! Sum of exponents should be unique to each pair.
      ! ERI_C_X * ERI_C_Y == ERI_C_X' * ERI_C_Y' =>
      ! (X == X' and Y == Y') or (X = Y' and Y = X')
      !
      integer, parameter :: ERI_C_S  = 2**1
      integer, parameter :: ERI_C_P  = 2**2
      integer, parameter :: ERI_C_D  = 2**5
      integer, parameter :: ERI_C_F  = 2**7
      integer, parameter :: ERI_C_L  = 2**14

      integer, parameter :: ERI_C_SS = ERI_C_S * ERI_C_S
      integer, parameter :: ERI_C_PS = ERI_C_P * ERI_C_S
      integer, parameter :: ERI_C_DS = ERI_C_D * ERI_C_S
      integer, parameter :: ERI_C_FS = ERI_C_F * ERI_C_S
      integer, parameter :: ERI_C_LS = ERI_C_L * ERI_C_S

      integer, parameter :: ERI_C_PP = ERI_C_P * ERI_C_P
      integer, parameter :: ERI_C_DP = ERI_C_D * ERI_C_P
      integer, parameter :: ERI_C_FP = ERI_C_F * ERI_C_P
      integer, parameter :: ERI_C_LP = ERI_C_L * ERI_C_P

      integer, parameter :: ERI_C_DD = ERI_C_D * ERI_C_D
      integer, parameter :: ERI_C_FD = ERI_C_F * ERI_C_D
      integer, parameter :: ERI_C_LD = ERI_C_L * ERI_C_D

      integer, parameter :: ERI_C_FF = ERI_C_F * ERI_C_F
      integer, parameter :: ERI_C_FL = ERI_C_F * ERI_C_L

      integer, parameter :: ERI_C_LL = ERI_C_L * ERI_C_L
      !
      ! ------------------ Quartets ------------
      !
      integer, parameter :: ERI_C_SSSS = ior(ERI_C_SS, ERI_C_SS)
      integer, parameter :: ERI_C_PSSS = ior(ERI_C_PS, ERI_C_SS)
      integer, parameter :: ERI_C_PSPS = ior(ERI_C_PS, ERI_C_PS)
      integer, parameter :: ERI_C_PPSS = ior(ERI_C_PP, ERI_C_SS)
      integer, parameter :: ERI_C_PPPS = ior(ERI_C_PP, ERI_C_PS)
      integer, parameter :: ERI_C_PPPP = ior(ERI_C_PP, ERI_C_PP)
      integer, parameter :: ERI_C_LLSS = ior(ERI_C_LL, ERI_C_SS)
      integer, parameter :: ERI_C_LLPS = ior(ERI_C_LL, ERI_C_PS)
      integer, parameter :: ERI_C_LLPP = ior(ERI_C_LL, ERI_C_PP)
      integer, parameter :: ERI_C_DSSS = ior(ERI_C_DS, ERI_C_SS)
      integer, parameter :: ERI_C_DSDS = ior(ERI_C_DS, ERI_C_DS)
      integer, parameter :: ERI_C_DDSS = ior(ERI_C_DD, ERI_C_SS)
      integer, parameter :: ERI_C_DPPP = ior(ERI_C_DP, ERI_C_PP)
      integer, parameter :: ERI_C_DPSS = ior(ERI_C_DP, ERI_C_SS)
      integer, parameter :: ERI_C_DSPS = ior(ERI_C_DS, ERI_C_PS)
      integer, parameter :: ERI_C_DSPP = ior(ERI_C_DS, ERI_C_PP)
      integer, parameter :: ERI_C_DPPS = ior(ERI_C_DP, ERI_C_PS)
      integer, parameter :: ERI_C_DDPS = ior(ERI_C_DD, ERI_C_PS)
      integer, parameter :: ERI_C_DDPP = ior(ERI_C_DD, ERI_C_PP)
      integer, parameter :: ERI_C_DDDS = ior(ERI_C_DD, ERI_C_DS)
      integer, parameter :: ERI_C_DPDS = ior(ERI_C_DP, ERI_C_DS)
      integer, parameter :: ERI_C_FSSS = ior(ERI_C_FS, ERI_C_SS)
      integer, parameter :: ERI_C_FSPS = ior(ERI_C_FS, ERI_C_PS)
      integer, parameter :: ERI_C_FSDS = ior(ERI_C_FS, ERI_C_DS)
      integer, parameter :: ERI_C_FPSS = ior(ERI_C_FP, ERI_C_SS)
      integer, parameter :: ERI_C_FPPS = ior(ERI_C_FP, ERI_C_PS)
      integer, parameter :: ERI_C_FDSS = ior(ERI_C_FD, ERI_C_SS)
      integer, parameter :: ERI_C_FSPP = ior(ERI_C_FS, ERI_C_PP)
      integer, parameter :: ERI_C_LDSS = ior(ERI_C_LD, ERI_C_SS)
      integer, parameter :: ERI_C_LDPS = ior(ERI_C_LD, ERI_C_PS)
      integer, parameter :: ERI_C_LDPP = ior(ERI_C_LD, ERI_C_PP)
      integer, parameter :: ERI_C_LPSS = ior(ERI_C_LP, ERI_C_SS)
      integer, parameter :: ERI_C_LPPS = ior(ERI_C_LP, ERI_C_PS)
      integer, parameter :: ERI_C_LPPP = ior(ERI_C_LP, ERI_C_PP)
      integer, parameter :: ERI_C_LSSS = ior(ERI_C_LS, ERI_C_SS)
      integer, parameter :: ERI_C_LSPS = ior(ERI_C_LS, ERI_C_PS)
      integer, parameter :: ERI_C_LSPP = ior(ERI_C_LS, ERI_C_PP)

      integer, private :: k
      integer, dimension(0:max_l), parameter :: ERI_BRA1_IDX = &
        (/BRA1_S, BRA1_P, BRA1_D, BRA1_F, (BRA1_L, k = 4, max_l)/)
      integer, dimension(0:max_l), parameter :: ERI_BRA2_IDX = &
        (/BRA2_S, BRA2_P, BRA2_D, BRA2_F, (BRA2_L, k = 4, max_l)/)
      integer, dimension(0:max_l), parameter :: ERI_KET1_IDX = &
        (/KET1_S, KET1_P, KET1_D, KET1_F, (KET1_L, k = 4, max_l)/)
      integer, dimension(0:max_l), parameter :: ERI_KET2_IDX = &
        (/KET2_S, KET2_P, KET2_D, KET2_F, (KET2_L, k = 4, max_l)/)

      integer, dimension(0:max_l), parameter :: ERI_IDX = &
        (/ERI_C_S, ERI_C_P, ERI_C_D, ERI_C_F, (ERI_C_L, k = 4, max_l)/)

contains

      subroutine eritype(permutation, canonical, a, b, c, d)
            !
            ! Detect the type of a two-electron integral
            !
            integer, intent(out) :: permutation, canonical
            integer, intent(in)  :: a, b, c, d

            integer :: bra, ket
            integer :: idxa, idxb, idxc, idxd

            idxa = ERI_BRA1_IDX(a)
            idxb = ERI_BRA2_IDX(b)
            idxc = ERI_KET1_IDX(c)
            idxd = ERI_KET2_IDX(d)

            permutation = ior(idxa, idxb)
            permutation = ior(permutation, idxc)
            permutation = ior(permutation, idxd)

            idxa = ERI_IDX(a)
            idxb = ERI_IDX(b)
            idxc = ERI_IDX(c)
            idxd = ERI_IDX(d)

            bra = idxa * idxb
            ket = idxc * idxd
            canonical = ior(bra, ket)
      end subroutine eritype
end module ericonst
