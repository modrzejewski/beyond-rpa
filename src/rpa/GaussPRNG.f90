module GaussPRNG
      use arithmetic
      use math_constants

      implicit none

      integer, parameter :: GRAND_MAXI = 128
      !
      ! The ai's satisfy the equation Erfc(ai/Sqrt(2)) = 1/2**i
      ! The following values are computed with Mathematica,
      ! using NSolve[Erfc[ai/Sqrt[2]] == 1/2^i, ai, WorkingPrecision -> 34]
      !
      real(F64), dimension(0:GRAND_MAXI), parameter :: GRAND_INTERVALS = [ &
            0.0_F64, &
            0.67448975019608174320222701454131_F64, &
            1.1503493803760081782967653108306_F64, &
            1.5341205443525463117083990590372_F64, &
            1.8627318674216514554876809785582_F64, &
            2.1538746940614562129637042051477_F64, &
            2.417559016236505061849151151963_F64, &
            2.6600674686174596585838288699973_F64, &
            2.8856349124267571473876066463246_F64, &
            3.0972690781987844623648304970553_F64, &
            3.2971933456919633418290931689466_F64, &
            3.4871041041144311068301380444131_F64, &
            3.6683292851213230192197251369008_F64, &
            3.8419306855019108708432579657525_F64, &
            4.0087725941685849622531508587838_F64, &
            4.1695693233491057549977384999754_F64, &
            4.3249190408260462571743685990921_F64, &
            4.4753284246542033544669618439137_F64, &
            4.6212310014992471565739450056795_F64, &
            4.7630010342678139569885544192963_F64, &
            4.9009642079631930118395023365834_F64, &
            5.0354059694639271537386235000074_F64, &
            5.1665781197287531132862060301433_F64, &
            5.2947040848545980574104648367678_F64, &
            5.4199831749168679884054044233087_F64, &
            5.5425940578029397674373528568095_F64, &
            5.6626976174594386654152202926359_F64, &
            5.7804393244789342266941498370295_F64, &
            5.8959512167395699216835183823604_F64, &
            6.0093535655307438931806086747466_F64, &
            6.1207562859719408103974765521651_F64, &
            6.2302601379890431630251481505432_F64, &
            6.3379577545537892524932512818075_F64, &
            6.4439345265385642153068554274562_F64, &
            6.5482693678317307553876155294668_F64, &
            6.6510353798930105466578608444834_F64, &
            6.7523004314070149646076266175837_F64, &
            6.8521276658960675309970448296352_F64, &
            6.9505759479167499332852486967034_F64, &
            7.0477002566644087253509923066986_F64, &
            7.1435520343521893331061788259325_F64, &
            7.2381794955440658536990344473057_F64, &
            7.3316279026493270887099207298215_F64, &
            7.4239398119859832402501202079859_F64, &
            7.515155294158907580400652040832_F64, &
            7.605312131948748954637308969796_F64, &
            7.694445998448802432961192213062_F64, &
            7.782590617802448092227147175434_F64, &
            7.869777910570139124019229238309_F64, &
            7.956038125481530962217996507548_F64, &
            8.041399959096542343814820578461_F64, &
            8.125890664701906858507992453056_F64, &
            8.209536151601386855630768778666_F64, &
            8.292361075813595538234152313779_F64, &
            8.374388923067456451026748923631_F64, &
            8.455642084878544042578659875283_F64, &
            8.536141928397261633942916506158_F64, &
            8.615908860639834902281675283185_F64, &
            8.694962387643603495989791048868_F64, &
            8.773321169027551681853587361677_F64, &
            8.851003068386146868244343964764_F64, &
            8.92802519989827058667724969013_F64, &
            9.004403971492415824312112003285_F64, &
            9.080155124873612669222092894349_F64, &
            9.155293772686072545996483742374_F64, &
            9.229834433057748687755273205772_F64, &
            9.303791061748417394620640962646_F64, &
            9.377177082101080295438093408861_F64, &
            9.450005412977119507174710092729_F64, &
            9.522288494838401624326978828053_F64, &
            9.594038314124160879278510589347_F64, &
            9.665266426056770041645340158773_F64, &
            9.735983975998233335137502796005_F64, &
            9.806201719468238336249862138064_F64, &
            9.875930040924735097605125491573_F64, &
            9.945178971399141224383070828697_F64, &
            10.013958205070288285536528317934_F64, &
            10.082277114854028817964000339858_F64, &
            10.150144767078927515821092949295_F64, &
            10.217569935312588767161416383236_F64, &
            10.284561113397858431140960550036_F64, &
            10.351126527753321515432657181594_F64, &
            10.417274148988147011071583194338_F64, &
            10.48301170287736040380820146348_F64, &
            10.548346680740012415656100759897_F64, &
            10.613286349259423063541171248494_F64, &
            10.677837759781680927862533655667_F64, &
            10.742007757125839931381804274597_F64, &
            10.805802987936754393910930001172_F64, &
            10.869229908609204860842806160611_F64, &
            10.932294792809871843677168611679_F64, &
            10.99500373862179394526318189691_F64, &
            11.057362675334184558012115060444_F64, &
            11.119377369898862789425520838286_F64, &
            11.181053433073066348864390804333_F64, &
            11.242396325267045012221223948466_F64, &
            11.303411362113572336998837535408_F64, &
            11.364103719775350950532224419493_F64, &
            11.424478440005214337652410480651_F64, &
            11.484540434973037807216290879058_F64, &
            11.544294491872356164418111609072_F64, &
            11.603745277318839172501267513762_F64, &
            11.662897341551992368686750320443_F64, &
            11.721755122450724958784769743873_F64, &
            11.780322949372753590814935971986_F64, &
            11.838605046827186472767433033285_F64, &
            11.89660553798905261764156185432_F64, &
            11.954328448064002387094626147534_F64, &
            12.011777707510904698566073244639_F64, &
            12.06895715512960028197978197606_F64, &
            12.125870541020636503010594337013_F64, &
            12.182521529423405027167278602901_F64, &
            12.23891370143872671192368984472_F64, &
            12.295050557641576504819523759727_F64, &
            12.35093552058931289061405623546_F64, &
            12.406571937230469825643220149436_F64, &
            12.461963081218882522118453392911_F64, &
            12.51711215513765042996351108265_F64, &
            12.572022292637189959024940272817_F64, &
            12.62669656049139464896644140009_F64, &
            12.681137960575700485865264075903_F64, &
            12.735349431770647831936868092262_F64, &
            12.789333851794338009005219530528_F64, &
            12.843094038967001063897103400467_F64, &
            12.896632753910720820513114984832_F64, &
            12.949952701187203227766058586427_F64, &
            13.003056530876323541625630191306_F64, &
            13.055946840098046382981978997736_F64, &
            13.108626174480179589435190608675_F64 &
            ]

contains

      subroutine chi2_GRAND(Chi2, NSamples, NBins, BM)
            !
            ! Compute the Chi**2 statistic for the GRAND pseudo-random
            ! numbers.
            !
            ! The normal variates are transformed according to
            ! uk = Int(-Inf,gk) (1/Sqrt(2Pi) Exp(-1/2 t**2) dt
            ! = 1/2 (1 + Erf(gk/Sqrt(2)),
            ! where uk is uniformly distributed on (0, 1) and
            ! gk is generated with GRAND. The chi**2 statistic
            ! is computed to test if uk's are uniform.
            !
            ! 1. Brent, R.P., Commun. ACM 17, 704 (1974);
            !    doi: 10.1145/361604.361629
            !
            real(F64), intent(out) :: Chi2
            integer, intent(in) :: NSamples
            integer, intent(in) :: NBins
            logical, intent(in) :: BM

            integer :: k, l
            real(F64) :: interval
            real(F64), dimension(:), allocatable :: G
            real(F64) :: u, NlExp
            integer, dimension(:), allocatable :: hist

            allocate(hist(NBins))
            allocate(G(NSamples))
            if (BM) then
                  call BoxMuller(G, NSamples)
            else
                  call GRAND(G, NSamples)
            end if
            interval = ONE / NBins
            hist = 0
            do k = 1, NSamples
                  u = ONE/TWO * (ONE + erf(G(k)/Sqrt(TWO)))
                  l = floor(u / interval) + 1
                  hist(l) = hist(l) + 1
            end do
            NlExp = real(NSamples, F64) / real(NBins, F64)
            Chi2 = 0
            do l = 1, NBins
                  Chi2 = Chi2 + (hist(l) - NlExp)**2 / NlExp
            end do
      end subroutine chi2_GRAND
      

      subroutine BoxMuller(G, n)
            real(F64), dimension(*), intent(out) :: G
            integer, intent(in) :: n
            integer :: k
            real(F64) :: u, v
            
            do k = 1, n / 2
                  call random_number(u)
                  call random_number(v)
                  G(2*k-1) = Sqrt(-TWO * log(u)) * cos(TWO*PI*v)
                  G(2*k) = Sqrt(-TWO * log(u)) * sin(TWO*PI*v)
            end do
            if (modulo(n, 2) == 1) then
                  call random_number(u)
                  call random_number(v)
                  G(n) = Sqrt(-TWO * log(u)) * cos(TWO*PI*v)
            end if
      end subroutine BoxMuller
      

      subroutine GRAND(G, n)
            !
            ! Generate n Gaussian pseudo-random numbers.
            ! Uses the GRAND algorithm of Richard P. Brent.
            !
            ! 1. Brent, R.P., Commun. ACM 17, 704 (1974);
            !    doi: 10.1145/361604.361629
            !
            real(F64), dimension(*), intent(out) :: G
            integer, intent(in) :: n

            real(F64) :: u, v, w, a, b
            integer :: k, l, i

            call random_number(u)
            kloop: do k = 1, n
                  !
                  ! Select i with a probability 1/2**i
                  !
                  i = GRAND_MAXI
                  FindI: do l = 1, GRAND_MAXI
                        u = TWO * u
                        if (u < ONE) then
                              i = l
                              exit FindI
                        else
                              u = u - ONE
                        end if
                  end do FindI
                  a = GRAND_INTERVALS(i-1)
                  b = GRAND_INTERVALS(i)
                  xloop: do
                        w = (b - a) * u
                        !
                        ! x = a + w
                        ! v = (x**2 - a**2) / TWO
                        !
                        v = w * (w / TWO + a)
                        uvloop: do
                              call random_number(u)
                              if (u >= v) then
                                    exit uvloop
                              else
                                    call random_number(v)
                                    if (u > v) then
                                          cycle uvloop
                                    else
                                          u = (v - u) / (ONE - u)
                                          cycle xloop
                                    end if
                              end if
                        end do uvloop
                        u = (u - v) / (ONE - v)
                        u = TWO * u
                        if (u < ONE) then
                              G(k) = -(a + w)
                        else
                              u = u - ONE
                              G(k) = a + w
                        end if
                        cycle kloop
                  end do xloop
            end do kloop
      end subroutine GRAND
end module GaussPRNG
