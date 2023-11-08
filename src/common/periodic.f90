module periodic
      use arithmetic
      use string
      use gparam

      implicit none

      character(len=2), dimension(KNOWN_ELEMENTS) :: ELNAME_SHORT = [ &
            "H ", "HE", "LI", "BE", "B ", &
            "C ", "N ", "O ", "F ", "NE", &
            "NA", "MG", "AL", "SI", "P ", &
            "S ", "CL", "AR", "K ", "CA", &
            "SC", "TI", "V ", "CR", "MN", &
            "FE", "CO", "NI", "CU", "ZN", &
            "GA", "GE", "AS", "SE", "BR", &
            "KR", "RB", "SR", "Y ", "ZR", &
            "NB", "MO", "TC", "RU", "RH", &
            "PD", "AG", "CD", "IN", "SN", &
            "SB", "TE", "I ", "XE", "CS", &
            "BA", "LA", "CE", "PR", "ND", &
            "PM", "SM", "EU", "GD", "TB", &
            "DY", "HO", "ER", "TM", "YB", &
            "LU", "HF", "TA", "W ", "RE", &
            "OS", "IR", "PT", "AU", "HG", &
            "TL", "PB", "BI", "PO", "AT", &
            "RN"]

      character(len=12), dimension(KNOWN_ELEMENTS) :: ELNAME_LONG = [ &
            "HYDROGEN    ", "HELIUM      ", "LITHIUM     ", "BERYLLIUM   ", "BORON       ", &
            "CARBON      ", "NITROGEN    ", "OXYGEN      ", "FLUORINE    ", "NEON        ", &
            "SODIUM      ", "MAGNESIUM   ", "ALUMINUM    ", "SILICON     ", "PHOSPHORUS  ", &
            "SULFUR      ", "CHLORINE    ", "ARGON       ", "POTASSIUM   ", "CALCIUM     ", &
            "SCANDIUM    ", "TITANIUM    ", "VANADIUM    ", "CHROMIUM    ", "MANGANESE   ", &
            "IRON        ", "COBALT      ", "NICKEL      ", "COPPER      ", "ZINC        ", &
            "GALLIUM     ", "GERMANIUM   ", "ARSENIC     ", "SELENIUM    ", "BROMINE     ", &
            "KRYPTON     ", "RUBIDIUM    ", "STRONTIUM   ", "YTTRIUM     ", "ZIRCONIUM   ", &
            "NIOBIUM     ", "MOLYBDENUM  ", "TECHNETIUM  ", "RUTHENIUM   ", "RHODIUM     ", &
            "PALLADIUM   ", "SILVER      ", "CADMIUM     ", "INDIUM      ", "TIN         ", &
            "ANTIMONY    ", "TELLURIUM   ", "IODINE      ", "XENON       ", "CESIUM      ", &
            "BARIUM      ", "LANTHANUM   ", "CERIUM      ", "PRASEODYMIUM", "NEODYMIUM   ", &
            "PROMETHIUM  ", "SAMARIUM    ", "EUROPIUM    ", "GADOLINIUM  ", "TERBIUM     ", &
            "DYSPROSIUM  ", "HOLMIUM     ", "ERBIUM      ", "THULIUM     ", "YTTERBIUM   ", &
            "LUTETIUM    ", "HAFNIUM     ", "TANTALUM    ", "TUNGSTEN    ", "RHENIUM     ", &
            "OSMIUM      ", "IRIDIUM     ", "PLATINUM    ", "GOLD        ", "MERCURY     ", &
            "THALLIUM    ", "LEAD        ", "BISMUTH     ", "POLONIUM    ", "ASTATINE    ", &
            "RADON       "]
      ! ---------------------------------------------------------------
      ! ATOMIC RADII (USED FOR INTEGRATION ON THE MOLECULAR GRID)
      ! ---------------------------------------------------------------
      ! 1. Gill, P., Johnson, B.G., Pople, J.A., 
      !    A standard grid for density functional calculations,
      !    Chem. Phys. Lett. 209, 506 (1993)
      ! 2. Ghosh, D. C. and Biswas, R., Theoretical Calculation of
      !    Absolute Radii of Atoms and Ions. Part 1. The Atomic Radii,
      !    Int. J. Mol. Sci. 3, 87 (2002)
      ! ---------------------------------------------------------------
      ! All values are written in atomic units. Radii for H...Ar are
      ! taken from Table 1 in [1]. Radii for K...Lr are taken from
      ! Table 1 in [2].
      !
      real(F64), dimension(103), parameter   :: ATOMIC_RADII = [ &
            1.0000d+0, & ! 1  H    
            0.5882d+0, & ! 2  He
            
            3.0769d+0, & ! 3  Li
            2.0513d+0, & ! 4  Be
            1.5385d+0, & ! 5  B
            1.2308d+0, & ! 6  C
            1.0256d+0, & ! 7  N
            0.8791d+0, & ! 8  O
            0.7692d+0, & ! 9  F
            0.6838d+0, & ! 10 Ne
            
            4.0909d+0, & ! 11 Na
            3.1579d+0, & ! 12 Mg
            2.5714d+0, & ! 13 Al
            2.1687d+0, & ! 14 Si
            1.8750d+0, & ! 15 P
            1.6514d+0, & ! 16 S
            1.4754d+0, & ! 17 Cl
            1.3333d+0, & ! 18 Ar
            
            6.7270d+0, & ! 19 K
            5.1928d+0, & ! 20
            4.9333d+0, & ! 21
            4.6980d+0, & ! 22
            4.4847d+0, & ! 23
            4.2899d+0, & ! 24
            4.1109d+0, & ! 25
            3.9467d+0, & ! 26
            3.7946d+0, & ! 27
            3.6542d+0, & ! 28
            3.5240d+0, & ! 29
            3.4023d+0, & ! 30
            2.9599d+0, & ! 31
            2.6195d+0, & ! 32
            2.3491d+0, & ! 33
            2.1295d+0, & ! 34
            1.9474d+0, &
            1.7939d+0, &
            9.0907d+0, &
            7.0175d+0, &
            6.6666d+0, &
            6.3491d+0, &
            6.0605d+0, &
            5.7971d+0, &
            5.5554d+0, &
            5.3332d+0, &
            5.1281d+0, &
            4.9382d+0, &
            4.7619d+0, &
            4.5977d+0, &
            4.0000d+0, &
            3.5398d+0, &
            3.1746d+0, &
            2.8777d+0, &
            2.6316d+0, &
            2.4241d+0, &
            11.4546d+0, &
            8.8417d+0, &
            7.2002d+0, &
            6.0723d+0, &
            5.2497d+0, &
            4.6238d+0, &
            4.1311d+0, &
            3.7333d+0, &
            3.4053d+0, &
            3.1303d+0, &
            2.8966d+0, &
            2.6951d+0, &
            2.5199d+0, &
            2.3661d+0, &
            2.2301d+0, &
            2.1087d+0, &
            1.9999d+0, &
            1.9047d+0, &
            1.8130d+0, &
            1.7319d+0, &
            1.6579d+0, &
            1.5898d+0, &
            1.5462d+0, &
            1.4695d+0, &
            1.4158d+0, &
            5.7894d+0, &
            5.0399d+0, &
            4.4603d+0, &
            4.0000d+0, &
            3.6258d+0, &
            3.3157d+0, &
            3.0546d+0, &
            13.6824d+0, &
            10.5611d+0, &
            10.0327d+0, &
            9.5562d+0, &
            6.9999d+0, &
            6.0806d+0, &
            5.3749d+0, &
            4.4590d+0, &
            4.0676d+0, &
            3.9868d+0, &
            3.4597d+0, &
            3.2191d+0, &
            3.0100d+0, &
            2.8263d+0, &
            2.6638d+0, &
            2.5188d+0, &
            2.4876d+0 ] ! 103 Lr

      !
      ! Average isotope masses from the NIST database
      ! http://physics.nist.gov/PhysRefData/Handbook/periodictable.htm
      !
      real(F64), dimension(10), parameter   :: ATOMIC_MASS_AV = [ &
            1.0079759263d+0, & ! 1 H    
            4.0025986385d+0,  & ! 2 He
            6.94093685d+0, & ! 3 Li    
            9.012182d+0, & ! 4 Be    
            10.811027768d+0, & ! 5 B
            12.011036905d+0, & ! 6 C    
            14.0067630258d+0, & ! 7 N    
            16.0006851933d+0, & ! 8 O    
           18.9984032d+0, &! 9 F
           20.1800414916d+0] ! 10 Ne

      !                                                                                                        
      ! Masses of isotopes of highest abundance from the NIST database                                                                    
      ! http://physics.nist.gov/PhysRefData/Handbook/periodictable.htm                                                           
      !
      real(F64), dimension(52), parameter   :: ATOMIC_MASS_HI = [ &
            1.007825d+0, & ! 1 H                                                                                                       
            4.00260d+0,  & ! 2 He                                                                                             
            7.016003d+0, & ! 3 Li                                                                                         
            9.012182d+0, & ! 4 Be                                                                                            
            11.009305d+0, & ! 5 B                                                                                
            12.000000d+0, & ! 6 C                                                                                   
            14.003074d+0, & ! 7 N                                                                                   
            15.994915d+0, & ! 8 O                                                                                     
            18.9984032d+0, &! 9 F  
            19.992435d+0, & ! 10 Ne
            22.989767d+0, & ! 11 Na
            23.985042d+0, &! 12 Mg
            26.981540d+0, &! 13 Al
            27.976927d+0, &! 14  Si
            30.973762d+0, &! 15 P
            31.972070d+0, &! 16 S
            34.968852d+0, &! 17 Cl
            39.962384d+0, &! 18 Ar
            38.963707d+0 , &! 19 K
            39.962591d+0, &! 20 Ca
            44.955910d+0, &! 21 Sc
            47.947947d+0, &! 22 Ti
            50.943962d+0, &! 23 V
            51.940509d+0, &! 24 Cr
            54.938047d+0, &! 25 Mn
            55.934939d+0, &! 26 Fe
            58.933198d+0, &! 27 Co
            57.935346d+0, &! 28 Ni
            62.939598d+0, &! 29 Cu
            63.929145d+0, &! 30 Zn
            68.925580d+0, &! 31 Ga
            73.921177d+0, &! 32 Ge
            74.921594d+0, &! 33 As
            79.916520d+0, &! 34 Se
            78.918336d+0, &! 35 Br
            83.911507d+0, &! 36 Kr
            84.911794d+0, &! 37 Rb
            87.905619d+0, &! 38 Sr
            88.905849d+0, &! 39 Y
            89.904703d+0, &! 40 Zr
            92.906377d+0, &! 41 Nb
            95.904678d+0, &! 42 Mo
            96.906364d+0, &! 43 97 Tc
            101.904348d+0, &! 44 Ru
            102.905500d+0, &! 45 Rh
            105.903478d+0, &! 46 Pd
            106.905092d+0, &! 47  Ag
            113.903357d+0, &! 48 Cd
            114.903800d+0, &! 49 In
            119.902220d+0, &! 50 Sn
            120.903821d+0, &! 51 Sb
            129.906229d+0]! 52 Te
      !
      ! Ground-state atomic shell configuration: (S, P, D, F),
      ! where S is the number of electrons in the S shell,
      ! P is the number of electrons in the P shell, etc.
      !
      integer, dimension(4*KNOWN_ELEMENTS), parameter :: GS_SPDF_CONFIG = [ &
            1, 0, 0, 0, & ! 1  H    
            2, 0, 0, 0, & ! 2  He
            
            1, 0, 0, 0, & ! 3  Li
            2, 0, 0, 0, & ! 4  Be
            2, 1, 0, 0, & ! 5  B
            2, 2, 0, 0, & ! 6  C
            2, 3, 0, 0, & ! 7  N
            2, 4, 0, 0, & ! 8  O
            2, 5, 0, 0, & ! 9  F
            2, 6, 0, 0, & ! 10 Ne
            
            1, 0, 0, 0, & ! 11 Na
            2, 0, 0, 0, & ! 12 Mg
            2, 1, 0, 0, & ! 13 Al
            2, 2, 0, 0, & ! 14 Si
            2, 3, 0, 0, & ! 15 P
            2, 4, 0, 0, & ! 16 S
            2, 5, 0, 0, & ! 17 Cl
            2, 6, 0, 0, & ! 18 Ar
            
            1, 0, 0, 0, & ! 19 K
            2, 0, 0, 0, & ! 20 Ca
            2, 0, 1, 0, & ! 21 Sc
            2, 0, 2, 0, & ! 22 Ti
            2, 0, 3, 0, & ! 23 V
            1, 0, 5, 0, & ! 24 Cr
            2, 0, 5, 0, & ! 25 Mn
            2, 0, 6, 0, & ! 26 Fe
            2, 0, 7, 0, & ! 27 Co
            2, 0, 8, 0, & ! 28 Ni
            1, 0, 10, 0, & ! 29 Cu
            2, 0, 10, 0, & ! 30 Zn
            2, 1, 10, 0, & ! 31 Ga
            2, 2, 10, 0, & ! 32 Ge
            2, 3, 10, 0, & ! 33 As
            2, 4, 10, 0, & ! 34 Se
            2, 5, 10, 0, & ! 35 Br
            2, 6, 10, 0, & ! 36 Kr
            
            1, 0, 0, 0, & ! 37 Rb
            2, 0, 0, 0, & ! 38 Sr
            2, 0, 1, 0, & ! 39 Y
            2, 0, 2, 0, & ! 40 Zr
            1, 0, 4, 0, & ! 41 Nb
            1, 0, 5, 0, & ! 42 Mo
            2, 0, 5, 0, & ! 43 Tc
            1, 0, 7, 0, & ! 44 Ru
            1, 0, 8, 0, & ! 45 Rh
            0, 0, 10, 0, & ! 46 Pd
            1, 0, 10, 0, & ! 47 Ag
            2, 0, 10, 0, & ! 48 Cd
            2, 1, 10, 0, & ! 49 In
            2, 2, 10, 0, & ! 50 Sn
            2, 3, 10, 0, & ! 51 Sb
            2, 4, 10, 0, & ! 52 Te
            2, 5, 10, 0, & ! 53 I
            2, 6, 10, 0, & ! 54 Xe
            
            1, 0, 0, 0, & ! 55 Cs
            2, 0, 0, 0, & ! 56 Ba
            2, 0, 1, 0, & ! 57 La
            2, 0, 1, 1, & ! 58 Ce
            2, 0, 0, 3, & ! 59 Pr
            2, 0, 0, 4, & ! 60 Nd
            2, 0, 0, 5, & ! 61 Pm
            2, 0, 0, 6, & ! 62 Sm
            2, 0, 0, 7, & ! 63 Eu
            2, 0, 1, 7, & ! 64 Gd
            2, 0, 0, 9, & ! 65 Tb
            2, 0, 0, 10, & ! 66 Dy
            2, 0, 0, 11, & ! 67 Ho
            2, 0, 0, 12, & ! 68 Er
            2, 0, 0, 13, & ! 69 Tm
            2, 0, 0, 14, & ! 70 Yb
            2, 0, 1, 14, & ! 71 Lu
            2, 0, 2, 14, & ! 72 Hf
            2, 0, 3, 14, & ! 73 Ta
            2, 0, 4, 14, & ! 74 W
            2, 0, 5, 14, & ! 75 Re
            2, 0, 6, 14, & ! 76 Os
            2, 0, 7, 14, & ! 77 Ir
            1, 0, 9, 14,  & ! 78 Pt
            1, 0, 10, 14, & ! 79 Au
            2, 0, 10, 14, & ! 80 Hg
            2, 1, 10, 14, & ! 81 Tl
            2, 2, 10, 14, & ! 82 Pb
            2, 3, 10, 14, & ! 83 Bi
            2, 4, 10, 14, & ! 84 Po
            2, 5, 10, 14, & ! 85 At
            2, 6, 10, 14 & ! 86 Rn
            ]

contains

      pure function chemical_formula(molecule)
            !
            ! Compute chemical formula of a given molecule.
            !
            character(:), allocatable         :: chemical_formula
            type(tmolecule), intent(in)       :: molecule
            
            character(:), allocatable :: s1, s2
            integer :: n_total, n_real
            integer :: k, l, tz, tn, n_ghosts, s
            
            n_total = molecule%natom
            n_real = 0
            do s = 1, 2
                  n_real = n_real + molecule%real_atoms(2, s) - molecule%real_atoms(1, s) + 1
            end do
            s1 = ""
            s2 = ""
            do k = 1, molecule%nelement
                  tz = molecule%zlist(k)
                  if (n_total /= n_real) then
                        tn = 0
                        do s = 1, 2
                              do l = molecule%real_atoms(1, s), molecule%real_atoms(2, s)
                                    if (molecule%inuclz(l) == tz) tn = tn + 1
                              end do
                        end do
                  else
                        tn = molecule%zcount(k)
                  end if
                  if (tn > 0) then
                        s1 = s1 // trim(ELNAME_SHORT(tz))
                        if (tn > 1) then
                              s1 = s1 // str(tn)
                        end if
                  end if
                  n_ghosts = molecule%zcount(k) - tn
                  if (n_ghosts > 0) then
                        s2 = s2 // trim(ELNAME_SHORT(tz))
                        if (n_ghosts > 1) then
                              s2 = s2 // str(n_ghosts)
                        end if
                  end if
            end do
            if (len(s2) > 0) then
                  chemical_formula = s1 // " + ghost centers " // s2
            else 
                  chemical_formula = s1
            end if
      end function chemical_formula


      pure function unpaired_electrons(z)
            !
            ! Number of unpaired electrons in the ground-state
            ! configuration of the element Z.
            !
            integer :: unpaired_electrons
            integer, intent(in) :: z

            integer :: s, p, d, f

            s = GS_SPDF_CONFIG(4 * (z - 1) + 1)
            p = GS_SPDF_CONFIG(4 * (z - 1) + 2)
            d = GS_SPDF_CONFIG(4 * (z - 1) + 3)
            f = GS_SPDF_CONFIG(4 * (z - 1) + 4)

            if (p > 3) p = 3 - (p - 3)
            if (d > 5) d = 5 - (d - 5)
            if (f > 7) f = 7 - (f - 7)

            unpaired_electrons = modulo(s, 2) + p + d + f
      end function unpaired_electrons


      function znumber_long(longname)
            ! -----------------------------------------------------
            ! Determine the atomic number, Z, of the element whose
            ! non-abbreviated name is LONGNAME:
            ! "HYDROGEN" -> 1
            ! -----------------------------------------------------
            ! ZNUMBER_LONG - Returned value, the atomic number if
            !                element corresponding to LONGNAME is
            !                found, 0 otherwise.
            ! SHORTNAME    - Non-abbreviated name of chemical element:
            !                "Helium", "Carbon", "Bromine", ...
            !                The result is not case-sensitive
            !
            integer                  :: znumber_long
            character(*), intent(in) :: longname

            character(:), allocatable :: s
            integer :: k

            s = uppercase(longname)
            !
            ! Special case: "PHOSPHOROUS" name is used
            ! in definitions of basis sets
            !
            if (s .eq. "PHOSPHOROUS") s = "PHOSPHORUS"
            znumber_long = 0

            kloop: do k = 1, KNOWN_ELEMENTS
                  if (elname_long(k) .eq. s) then
                        znumber_long = k
                  end if
            end do kloop
      end function znumber_long


      function znumber_short(shortname)
            ! -----------------------------------------------------------
            ! Determine the atomic number, Z, of the element whose
            ! abbreviated name is SHORTNAME:
            ! "H" -> 1
            ! -----------------------------------------------------------
            ! ZNUMBER_SHOR,   Output, the atomic number of the element
            !                 element corresponding to SHORTNAME. If
            !                 the element is not found, 0 is returned.
            ! SHORTNAME     - Abbreviated name of the chemical element:
            !                 "He", "C", "Br", ... 
            !
            integer                  :: znumber_short
            character(*), intent(in) :: shortname

            character(:), allocatable :: s
            integer :: k

            s = uppercase(shortname)
            znumber_short = 0

            kloop: do k = 1, KNOWN_ELEMENTS
                  if (elname_short(k) .eq. s) then
                        znumber_short = k
                  end if
            end do kloop
      end function znumber_short


      function periodic_table_row(z)
            integer :: periodic_table_row
            integer, intent(in) :: z
            
            if (z <= 2) then
                  periodic_table_row = 0
            else if (z <= 10) then
                  periodic_table_row = 1
            else if (z <= 18) then
                  periodic_table_row = 2
            else if (z <= 36) then
                  periodic_table_row = 3
            else if (z <= 54) then
                  periodic_table_row = 4
            else if (z <= 86) then
                  periodic_table_row = 5
            else
                  periodic_table_row = 6
            end if
      end function periodic_table_row
end module periodic
