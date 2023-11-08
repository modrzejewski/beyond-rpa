module so_cc3_pt3b
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
    use so_cc3_pt3a

    !
    ! File generated automatically on 2018-04-18 11:46:20
    !

contains    
    function calc_D_oo_wm_so_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, k, b 
    real(F64), dimension(0:881) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_161_so_pt3(a,i,j,q) * wm_interm_1_so_pt3(a,j,p,i)
term(1) = term(1) + wm_interm_161_so_pt3(a,i,j,q) * wm_interm_2_so_pt3(a,p,j,i)
term(2) = term(2) + wm_interm_161_so_pt3(a,i,j,q) * wm_interm_3_so_pt3(a,j,p,i)
term(3) = term(3) + wm_interm_162_so_pt3(a,i,j,q) * wm_interm_2_so_pt3(a,j,p,i)
term(4) = term(4) + wm_interm_162_so_pt3(a,i,j,q) * wm_interm_1_so_pt3(a,p,j,i)
term(5) = term(5) + wm_interm_162_so_pt3(a,i,j,q) * wm_interm_3_so_pt3(a,p,j,i)
term(6) = term(6) + wm_interm_163_so_pt3(a,i,j,q) * wm_interm_1_so_pt3(a,j,p,i)
term(7) = term(7) + wm_interm_163_so_pt3(a,i,j,q) * wm_interm_2_so_pt3(a,p,j,i)
term(8) = term(8) + wm_interm_163_so_pt3(a,i,j,q) * wm_interm_3_so_pt3(a,j,p,i)
term(9) = term(9) + wm_interm_163_so_pt3(a,i,j,q) * wm_interm_2_so_pt3(a,j,p,i)
term(10) = term(10) + wm_interm_163_so_pt3(a,i,j,q) * wm_interm_1_so_pt3(a,p,j,i)
term(11) = term(11) + wm_interm_163_so_pt3(a,i,j,q) * wm_interm_3_so_pt3(a,p,j,i)
term(12) = term(12) + wm_interm_180_so_pt3(a,i,j,q) * wm_interm_31_so_pt3(a,p,j,i)
term(13) = term(13) + wm_interm_180_so_pt3(a,i,j,q) * wm_interm_35_so_pt3(a,p,j,i)
term(14) = term(14) + wm_interm_181_so_pt3(a,i,j,q) * wm_interm_31_so_pt3(a,p,j,i)
term(15) = term(15) + wm_interm_181_so_pt3(a,i,j,q) * wm_interm_35_so_pt3(a,p,j,i)
term(16) = term(16) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_178_so_pt3(a,p,j,i)
term(17) = term(17) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_174_so_pt3(a,p,j,i)
term(18) = term(18) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_175_so_pt3(a,p,j,i)
term(19) = term(19) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_173_so_pt3(a,p,j,i)
term(20) = term(20) + wm_interm_192_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(21) = term(21) + wm_interm_193_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(22) = term(22) + wm_interm_194_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(23) = term(23) + wm_interm_195_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(24) = term(24) + wm_interm_196_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(25) = term(25) + wm_interm_197_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(26) = term(26) + wm_interm_198_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(27) = term(27) + wm_interm_199_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(28) = term(28) + wm_interm_200_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(29) = term(29) + wm_interm_210_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(30) = term(30) + wm_interm_211_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(31) = term(31) + wm_interm_212_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(32) = term(32) + wm_interm_213_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(33) = term(33) + wm_interm_214_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
term(34) = term(34) + wm_interm_215_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,j,i)
end do 
end do 
end do 

term(2) = term(2) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (4.0d+0) 
term(7) = term(7) * (4.0d+0) 
term(8) = term(8) * (-8.0d+0) 
term(9) = term(9) * (-3.9999999999999996d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (8.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(15) = term(15) * (-2.0000000000000004d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (-8.0d+0) 
term(18) = term(18) * (8.0d+0) 
term(19) = term(19) * (3.0d+0) 
term(20) = term(20) * (-1.0d+0) 
term(21) = term(21) * (2.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (0.5d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (0.5d+0) 
term(26) = term(26) * (0.5d+0) 
term(27) = term(27) * (0.5d+0) 
term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (4.0d+0) 
term(31) = term(31) * (2.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (2.0d+0) 
term(34) = term(34) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(35) = term(35) + wm_interm_161_so_pt3(a,i,q,j) * wm_interm_1_so_pt3(a,j,p,i)
term(36) = term(36) + wm_interm_161_so_pt3(a,i,q,j) * wm_interm_2_so_pt3(a,j,p,i)
term(37) = term(37) + wm_interm_161_so_pt3(a,i,q,j) * wm_interm_1_so_pt3(a,p,j,i)
term(38) = term(38) + wm_interm_161_so_pt3(a,i,q,j) * wm_interm_2_so_pt3(a,p,j,i)
term(39) = term(39) + wm_interm_161_so_pt3(a,i,q,j) * wm_interm_3_so_pt3(a,j,p,i)
term(40) = term(40) + wm_interm_161_so_pt3(a,i,q,j) * wm_interm_3_so_pt3(a,p,j,i)
term(41) = term(41) + wm_interm_162_so_pt3(a,i,q,j) * wm_interm_2_so_pt3(a,j,p,i)
term(42) = term(42) + wm_interm_162_so_pt3(a,i,q,j) * wm_interm_1_so_pt3(a,j,p,i)
term(43) = term(43) + wm_interm_162_so_pt3(a,i,q,j) * wm_interm_2_so_pt3(a,p,j,i)
term(44) = term(44) + wm_interm_162_so_pt3(a,i,q,j) * wm_interm_1_so_pt3(a,p,j,i)
term(45) = term(45) + wm_interm_162_so_pt3(a,i,q,j) * wm_interm_3_so_pt3(a,p,j,i)
term(46) = term(46) + wm_interm_162_so_pt3(a,i,q,j) * wm_interm_3_so_pt3(a,j,p,i)
term(47) = term(47) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_85_so_pt3(a,q,j,i)
term(48) = term(48) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_86_so_pt3(a,q,j,i)
term(49) = term(49) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_87_so_pt3(a,q,j,i)
term(50) = term(50) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_90_so_pt3(a,q,j,i)
term(51) = term(51) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_91_so_pt3(a,q,i,j)
term(52) = term(52) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_90_so_pt3(a,q,i,j)
term(53) = term(53) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_85_so_pt3(a,q,i,j)
term(54) = term(54) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_86_so_pt3(a,q,i,j)
term(55) = term(55) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_87_so_pt3(a,q,i,j)
term(56) = term(56) + wm_interm_163_so_pt3(a,i,q,j) * wm_interm_1_so_pt3(a,j,p,i)
term(57) = term(57) + wm_interm_163_so_pt3(a,i,q,j) * wm_interm_2_so_pt3(a,j,p,i)
term(58) = term(58) + wm_interm_163_so_pt3(a,i,q,j) * wm_interm_1_so_pt3(a,p,j,i)
term(59) = term(59) + wm_interm_163_so_pt3(a,i,q,j) * wm_interm_2_so_pt3(a,p,j,i)
term(60) = term(60) + wm_interm_163_so_pt3(a,i,q,j) * wm_interm_3_so_pt3(a,j,p,i)
term(61) = term(61) + wm_interm_163_so_pt3(a,i,q,j) * wm_interm_3_so_pt3(a,p,j,i)
term(62) = term(62) + wm_interm_114_so_pt3(a,q,i,j) * wm_interm_5_so_pt3(a,p,j,i)
term(63) = term(63) + wm_interm_115_so_pt3(a,q,i,j) * wm_interm_5_so_pt3(a,p,j,i)
term(64) = term(64) + wm_interm_116_so_pt3(a,q,i,j) * wm_interm_5_so_pt3(a,p,j,i)
term(65) = term(65) + wm_interm_118_so_pt3(a,q,i,j) * wm_interm_5_so_pt3(a,p,j,i)
term(66) = term(66) + wm_interm_119_so_pt3(a,q,i,j) * wm_interm_5_so_pt3(a,p,i,j)
term(67) = term(67) + wm_interm_118_so_pt3(a,q,i,j) * wm_interm_5_so_pt3(a,p,i,j)
term(68) = term(68) + wm_interm_114_so_pt3(a,q,i,j) * wm_interm_5_so_pt3(a,p,i,j)
term(69) = term(69) + wm_interm_115_so_pt3(a,q,i,j) * wm_interm_5_so_pt3(a,p,i,j)
term(70) = term(70) + wm_interm_116_so_pt3(a,q,i,j) * wm_interm_5_so_pt3(a,p,i,j)
term(71) = term(71) + wm_interm_162_so_pt3(a,p,i,j) * wm_interm_1_so_pt3(a,j,i,q)
term(72) = term(72) + wm_interm_161_so_pt3(a,p,i,j) * wm_interm_1_so_pt3(a,i,j,q)
term(73) = term(73) + wm_interm_161_so_pt3(a,p,i,j) * wm_interm_1_so_pt3(a,j,i,q)
term(74) = term(74) + wm_interm_5_so_pt3(a,i,q,j) * wm_interm_85_so_pt3(a,i,j,p)
term(75) = term(75) + wm_interm_5_so_pt3(a,i,q,j) * wm_interm_86_so_pt3(a,i,j,p)
term(76) = term(76) + wm_interm_5_so_pt3(a,i,q,j) * wm_interm_87_so_pt3(a,i,j,p)
term(77) = term(77) + wm_interm_162_so_pt3(a,p,i,j) * wm_interm_2_so_pt3(a,i,j,q)
term(78) = term(78) + wm_interm_5_so_pt3(a,i,q,j) * wm_interm_90_so_pt3(a,i,j,p)
term(79) = term(79) + wm_interm_161_so_pt3(a,p,i,j) * wm_interm_2_so_pt3(a,j,i,q)
term(80) = term(80) + wm_interm_5_so_pt3(a,i,q,j) * wm_interm_91_so_pt3(a,i,p,j)
term(81) = term(81) + wm_interm_161_so_pt3(a,p,i,j) * wm_interm_2_so_pt3(a,i,j,q)
term(82) = term(82) + wm_interm_5_so_pt3(a,i,q,j) * wm_interm_90_so_pt3(a,i,p,j)
term(83) = term(83) + wm_interm_162_so_pt3(a,p,i,j) * wm_interm_3_so_pt3(a,j,i,q)
term(84) = term(84) + wm_interm_161_so_pt3(a,p,i,j) * wm_interm_3_so_pt3(a,i,j,q)
term(85) = term(85) + wm_interm_161_so_pt3(a,p,i,j) * wm_interm_3_so_pt3(a,j,i,q)
term(86) = term(86) + wm_interm_5_so_pt3(a,i,q,j) * wm_interm_85_so_pt3(a,i,p,j)
term(87) = term(87) + wm_interm_5_so_pt3(a,i,q,j) * wm_interm_86_so_pt3(a,i,p,j)
term(88) = term(88) + wm_interm_5_so_pt3(a,i,q,j) * wm_interm_87_so_pt3(a,i,p,j)
term(89) = term(89) + wm_interm_5_so_pt3(a,i,j,q) * wm_interm_85_so_pt3(a,i,p,j)
term(90) = term(90) + wm_interm_5_so_pt3(a,i,j,q) * wm_interm_86_so_pt3(a,i,p,j)
term(91) = term(91) + wm_interm_5_so_pt3(a,i,j,q) * wm_interm_87_so_pt3(a,i,p,j)
term(92) = term(92) + wm_interm_5_so_pt3(a,i,j,q) * wm_interm_91_so_pt3(a,i,j,p)
term(93) = term(93) + wm_interm_5_so_pt3(a,i,j,q) * wm_interm_90_so_pt3(a,i,p,j)
term(94) = term(94) + wm_interm_5_so_pt3(a,i,j,q) * wm_interm_91_so_pt3(a,i,p,j)
term(95) = term(95) + wm_interm_162_so_pt3(a,p,i,j) * wm_interm_167_so_pt3(a,j,i,q)
term(96) = term(96) + wm_interm_161_so_pt3(a,p,i,j) * wm_interm_167_so_pt3(a,i,j,q)
term(97) = term(97) + wm_interm_161_so_pt3(a,p,i,j) * wm_interm_167_so_pt3(a,j,i,q)
term(98) = term(98) + wm_interm_163_so_pt3(a,p,i,j) * wm_interm_1_so_pt3(a,i,j,q)
term(99) = term(99) + wm_interm_163_so_pt3(a,p,i,j) * wm_interm_1_so_pt3(a,j,i,q)
term(100) = term(100) + wm_interm_114_so_pt3(a,i,j,p) * wm_interm_5_so_pt3(a,i,q,j)
term(101) = term(101) + wm_interm_115_so_pt3(a,i,j,p) * wm_interm_5_so_pt3(a,i,q,j)
term(102) = term(102) + wm_interm_116_so_pt3(a,i,j,p) * wm_interm_5_so_pt3(a,i,q,j)
term(103) = term(103) + wm_interm_163_so_pt3(a,p,i,j) * wm_interm_2_so_pt3(a,j,i,q)
term(104) = term(104) + wm_interm_118_so_pt3(a,i,j,p) * wm_interm_5_so_pt3(a,i,q,j)
term(105) = term(105) + wm_interm_119_so_pt3(a,i,p,j) * wm_interm_5_so_pt3(a,i,q,j)
term(106) = term(106) + wm_interm_163_so_pt3(a,p,i,j) * wm_interm_2_so_pt3(a,i,j,q)
term(107) = term(107) + wm_interm_118_so_pt3(a,i,p,j) * wm_interm_5_so_pt3(a,i,q,j)
term(108) = term(108) + wm_interm_163_so_pt3(a,p,i,j) * wm_interm_3_so_pt3(a,i,j,q)
term(109) = term(109) + wm_interm_163_so_pt3(a,p,i,j) * wm_interm_3_so_pt3(a,j,i,q)
term(110) = term(110) + wm_interm_114_so_pt3(a,i,p,j) * wm_interm_5_so_pt3(a,i,q,j)
term(111) = term(111) + wm_interm_115_so_pt3(a,i,p,j) * wm_interm_5_so_pt3(a,i,q,j)
term(112) = term(112) + wm_interm_116_so_pt3(a,i,p,j) * wm_interm_5_so_pt3(a,i,q,j)
term(113) = term(113) + wm_interm_114_so_pt3(a,i,p,j) * wm_interm_5_so_pt3(a,i,j,q)
term(114) = term(114) + wm_interm_115_so_pt3(a,i,p,j) * wm_interm_5_so_pt3(a,i,j,q)
term(115) = term(115) + wm_interm_116_so_pt3(a,i,p,j) * wm_interm_5_so_pt3(a,i,j,q)
term(116) = term(116) + wm_interm_119_so_pt3(a,i,j,p) * wm_interm_5_so_pt3(a,i,j,q)
term(117) = term(117) + wm_interm_118_so_pt3(a,i,p,j) * wm_interm_5_so_pt3(a,i,j,q)
term(118) = term(118) + wm_interm_119_so_pt3(a,i,p,j) * wm_interm_5_so_pt3(a,i,j,q)
term(119) = term(119) + wm_interm_163_so_pt3(a,p,i,j) * wm_interm_167_so_pt3(a,i,j,q)
term(120) = term(120) + wm_interm_163_so_pt3(a,p,i,j) * wm_interm_167_so_pt3(a,j,i,q)
term(121) = term(121) + wm_interm_10_so_pt3(a,i,p,j) * wm_interm_173_so_pt3(a,j,q,i)
term(122) = term(122) + wm_interm_10_so_pt3(a,i,p,j) * wm_interm_174_so_pt3(a,j,q,i)
term(123) = term(123) + wm_interm_10_so_pt3(a,i,p,j) * wm_interm_174_so_pt3(a,j,i,q)
term(124) = term(124) + wm_interm_10_so_pt3(a,i,p,j) * wm_interm_173_so_pt3(a,j,i,q)
term(125) = term(125) + wm_interm_10_so_pt3(a,i,p,j) * wm_interm_175_so_pt3(a,j,i,q)
term(126) = term(126) + wm_interm_10_so_pt3(a,i,p,j) * wm_interm_175_so_pt3(a,j,q,i)
term(127) = term(127) + wm_interm_10_so_pt3(a,p,i,j) * wm_interm_173_so_pt3(a,j,i,q)
term(128) = term(128) + wm_interm_10_so_pt3(a,p,i,j) * wm_interm_174_so_pt3(a,j,i,q)
term(129) = term(129) + wm_interm_10_so_pt3(a,p,i,j) * wm_interm_174_so_pt3(a,j,q,i)
term(130) = term(130) + wm_interm_10_so_pt3(a,p,i,j) * wm_interm_175_so_pt3(a,j,q,i)
term(131) = term(131) + wm_interm_10_so_pt3(a,p,i,j) * wm_interm_173_so_pt3(a,j,q,i)
term(132) = term(132) + wm_interm_10_so_pt3(a,p,i,j) * wm_interm_175_so_pt3(a,j,i,q)
term(133) = term(133) + wm_interm_12_so_pt3(a,i,q,j) * wm_interm_176_so_pt3(a,p,i,j)
term(134) = term(134) + wm_interm_13_so_pt3(a,i,q,j) * wm_interm_176_so_pt3(a,p,i,j)
term(135) = term(135) + wm_interm_14_so_pt3(a,i,q,j) * wm_interm_176_so_pt3(a,p,i,j)
term(136) = term(136) + wm_interm_15_so_pt3(a,i,q,j) * wm_interm_176_so_pt3(a,p,i,j)
term(137) = term(137) + wm_interm_12_so_pt3(a,q,i,j) * wm_interm_176_so_pt3(a,p,i,j)
term(138) = term(138) + wm_interm_13_so_pt3(a,q,i,j) * wm_interm_176_so_pt3(a,p,i,j)
term(139) = term(139) + wm_interm_12_so_pt3(a,q,i,j) * wm_interm_176_so_pt3(a,i,p,j)
term(140) = term(140) + wm_interm_13_so_pt3(a,q,i,j) * wm_interm_176_so_pt3(a,i,p,j)
term(141) = term(141) + wm_interm_15_so_pt3(a,i,q,j) * wm_interm_176_so_pt3(a,i,p,j)
term(142) = term(142) + wm_interm_14_so_pt3(a,i,q,j) * wm_interm_176_so_pt3(a,i,p,j)
term(143) = term(143) + wm_interm_12_so_pt3(a,i,q,j) * wm_interm_176_so_pt3(a,i,p,j)
term(144) = term(144) + wm_interm_13_so_pt3(a,i,q,j) * wm_interm_176_so_pt3(a,i,p,j)
term(145) = term(145) + wm_interm_10_so_pt3(a,i,p,j) * wm_interm_178_so_pt3(a,j,i,q)
term(146) = term(146) + wm_interm_10_so_pt3(a,i,p,j) * wm_interm_178_so_pt3(a,j,q,i)
term(147) = term(147) + wm_interm_10_so_pt3(a,p,i,j) * wm_interm_178_so_pt3(a,j,q,i)
term(148) = term(148) + wm_interm_10_so_pt3(a,p,i,j) * wm_interm_178_so_pt3(a,j,i,q)
term(149) = term(149) + wm_interm_15_so_pt3(a,i,q,j) * wm_interm_179_so_pt3(a,p,i,j)
term(150) = term(150) + wm_interm_13_so_pt3(a,q,i,j) * wm_interm_179_so_pt3(a,p,i,j)
term(151) = term(151) + wm_interm_12_so_pt3(a,q,i,j) * wm_interm_179_so_pt3(a,p,i,j)
term(152) = term(152) + wm_interm_12_so_pt3(a,i,q,j) * wm_interm_179_so_pt3(a,p,i,j)
term(153) = term(153) + wm_interm_14_so_pt3(a,i,q,j) * wm_interm_179_so_pt3(a,p,i,j)
term(154) = term(154) + wm_interm_13_so_pt3(a,i,q,j) * wm_interm_179_so_pt3(a,p,i,j)
term(155) = term(155) + wm_interm_14_so_pt3(a,i,q,j) * wm_interm_179_so_pt3(a,i,p,j)
term(156) = term(156) + wm_interm_13_so_pt3(a,i,q,j) * wm_interm_179_so_pt3(a,i,p,j)
term(157) = term(157) + wm_interm_12_so_pt3(a,i,q,j) * wm_interm_179_so_pt3(a,i,p,j)
term(158) = term(158) + wm_interm_12_so_pt3(a,q,i,j) * wm_interm_179_so_pt3(a,i,p,j)
term(159) = term(159) + wm_interm_15_so_pt3(a,i,q,j) * wm_interm_179_so_pt3(a,i,p,j)
term(160) = term(160) + wm_interm_13_so_pt3(a,q,i,j) * wm_interm_179_so_pt3(a,i,p,j)
term(161) = term(161) + wm_interm_181_so_pt3(a,i,j,q) * wm_interm_31_so_pt3(a,p,i,j)
term(162) = term(162) + wm_interm_181_so_pt3(a,i,j,q) * wm_interm_35_so_pt3(a,p,i,j)
term(163) = term(163) + wm_interm_180_so_pt3(a,i,j,q) * wm_interm_31_so_pt3(a,p,i,j)
term(164) = term(164) + wm_interm_180_so_pt3(a,i,j,q) * wm_interm_35_so_pt3(a,p,i,j)
term(165) = term(165) + wm_interm_125_so_pt3(a,p,i,j) * wm_interm_41_so_pt3(a,q,j,i)
term(166) = term(166) + wm_interm_126_so_pt3(a,p,i,j) * wm_interm_41_so_pt3(a,q,j,i)
term(167) = term(167) + wm_interm_125_so_pt3(a,p,i,j) * wm_interm_41_so_pt3(a,q,i,j)
term(168) = term(168) + wm_interm_126_so_pt3(a,p,i,j) * wm_interm_41_so_pt3(a,q,i,j)
term(169) = term(169) + wm_interm_135_so_pt3(a,p,i,j) * wm_interm_41_so_pt3(a,q,j,i)
term(170) = term(170) + wm_interm_181_so_pt3(a,i,j,q) * wm_interm_37_so_pt3(a,p,i,j)
term(171) = term(171) + wm_interm_135_so_pt3(a,p,i,j) * wm_interm_41_so_pt3(a,q,i,j)
term(172) = term(172) + wm_interm_181_so_pt3(a,i,j,q) * wm_interm_39_so_pt3(a,p,i,j)
term(173) = term(173) + wm_interm_180_so_pt3(a,i,j,q) * wm_interm_39_so_pt3(a,p,i,j)
term(174) = term(174) + wm_interm_180_so_pt3(a,i,j,q) * wm_interm_37_so_pt3(a,p,i,j)
term(175) = term(175) + wm_interm_138_so_pt3(a,p,i,j) * wm_interm_41_so_pt3(a,q,i,j)
term(176) = term(176) + wm_interm_138_so_pt3(a,p,i,j) * wm_interm_41_so_pt3(a,q,j,i)
term(177) = term(177) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_174_so_pt3(a,p,i,j)
term(178) = term(178) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_175_so_pt3(a,p,i,j)
term(179) = term(179) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_178_so_pt3(a,p,i,j)
term(180) = term(180) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_173_so_pt3(a,p,i,j)
term(181) = term(181) + wm_interm_12_so_pt3(a,i,j,p) * wm_interm_179_so_pt3(a,i,j,q)
term(182) = term(182) + wm_interm_12_so_pt3(a,i,j,p) * wm_interm_179_so_pt3(a,j,i,q)
term(183) = term(183) + wm_interm_12_so_pt3(a,i,j,p) * wm_interm_176_so_pt3(a,j,i,q)
term(184) = term(184) + wm_interm_12_so_pt3(a,i,j,p) * wm_interm_176_so_pt3(a,i,j,q)
term(185) = term(185) + wm_interm_14_so_pt3(a,i,j,p) * wm_interm_179_so_pt3(a,i,j,q)
term(186) = term(186) + wm_interm_15_so_pt3(a,i,j,p) * wm_interm_176_so_pt3(a,i,j,q)
term(187) = term(187) + wm_interm_13_so_pt3(a,i,j,p) * wm_interm_179_so_pt3(a,i,j,q)
term(188) = term(188) + wm_interm_13_so_pt3(a,i,j,p) * wm_interm_176_so_pt3(a,j,i,q)
term(189) = term(189) + wm_interm_14_so_pt3(a,i,j,p) * wm_interm_176_so_pt3(a,i,j,q)
term(190) = term(190) + wm_interm_15_so_pt3(a,i,j,p) * wm_interm_179_so_pt3(a,i,j,q)
term(191) = term(191) + wm_interm_13_so_pt3(a,i,j,p) * wm_interm_179_so_pt3(a,j,i,q)
term(192) = term(192) + wm_interm_13_so_pt3(a,i,j,p) * wm_interm_176_so_pt3(a,i,j,q)
term(193) = term(193) + wm_interm_181_so_pt3(a,i,p,j) * wm_interm_31_so_pt3(a,j,q,i)
term(194) = term(194) + wm_interm_180_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,q,i)
term(195) = term(195) + wm_interm_181_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,i,q)
term(196) = term(196) + wm_interm_181_so_pt3(a,i,p,j) * wm_interm_31_so_pt3(a,j,i,q)
term(197) = term(197) + wm_interm_180_so_pt3(a,i,p,j) * wm_interm_31_so_pt3(a,j,i,q)
term(198) = term(198) + wm_interm_180_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,i,q)
term(199) = term(199) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_41_so_pt3(a,i,j,p)
term(200) = term(200) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_41_so_pt3(a,i,j,p)
term(201) = term(201) + wm_interm_181_so_pt3(a,i,p,j) * wm_interm_35_so_pt3(a,j,q,i)
term(202) = term(202) + wm_interm_180_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,q,i)
term(203) = term(203) + wm_interm_181_so_pt3(a,i,p,j) * wm_interm_37_so_pt3(a,j,i,q)
term(204) = term(204) + wm_interm_180_so_pt3(a,p,i,j) * wm_interm_37_so_pt3(a,j,i,q)
term(205) = term(205) + wm_interm_138_so_pt3(a,i,j,q) * wm_interm_41_so_pt3(a,i,j,p)
term(206) = term(206) + wm_interm_135_so_pt3(a,i,q,j) * wm_interm_41_so_pt3(a,i,j,p)
term(207) = term(207) + wm_interm_181_so_pt3(a,i,p,j) * wm_interm_39_so_pt3(a,j,i,q)
term(208) = term(208) + wm_interm_180_so_pt3(a,i,p,j) * wm_interm_39_so_pt3(a,j,i,q)
term(209) = term(209) + wm_interm_181_so_pt3(a,p,i,j) * wm_interm_39_so_pt3(a,j,i,q)
term(210) = term(210) + wm_interm_181_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,i,q)
term(211) = term(211) + wm_interm_181_so_pt3(a,i,p,j) * wm_interm_35_so_pt3(a,j,i,q)
term(212) = term(212) + wm_interm_180_so_pt3(a,i,p,j) * wm_interm_35_so_pt3(a,j,i,q)
term(213) = term(213) + wm_interm_180_so_pt3(a,p,i,j) * wm_interm_39_so_pt3(a,j,i,q)
term(214) = term(214) + wm_interm_180_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,i,q)
term(215) = term(215) + wm_interm_138_so_pt3(a,i,j,q) * wm_interm_41_so_pt3(a,i,p,j)
term(216) = term(216) + wm_interm_138_so_pt3(a,i,q,j) * wm_interm_41_so_pt3(a,i,p,j)
term(217) = term(217) + wm_interm_135_so_pt3(a,i,j,q) * wm_interm_41_so_pt3(a,i,p,j)
term(218) = term(218) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_41_so_pt3(a,i,p,j)
term(219) = term(219) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_41_so_pt3(a,i,p,j)
term(220) = term(220) + wm_interm_135_so_pt3(a,i,q,j) * wm_interm_41_so_pt3(a,i,p,j)
term(221) = term(221) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_41_so_pt3(a,i,p,j)
term(222) = term(222) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_41_so_pt3(a,i,p,j)
term(223) = term(223) + wm_interm_196_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(224) = term(224) + wm_interm_201_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(225) = term(225) + wm_interm_197_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(226) = term(226) + wm_interm_198_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(227) = term(227) + wm_interm_199_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(228) = term(228) + wm_interm_200_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(229) = term(229) + wm_interm_192_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(230) = term(230) + wm_interm_193_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(231) = term(231) + wm_interm_194_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(232) = term(232) + wm_interm_208_so_pt3(a,i,q,j) * wm_interm_54_so_pt3(a,p,i,j)
term(233) = term(233) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_54_so_pt3(a,p,i,j)
term(234) = term(234) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_54_so_pt3(a,p,i,j)
term(235) = term(235) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_54_so_pt3(a,i,p,j)
term(236) = term(236) + wm_interm_209_so_pt3(a,i,q,j) * wm_interm_54_so_pt3(a,i,p,j)
term(237) = term(237) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_54_so_pt3(a,i,p,j)
term(238) = term(238) + wm_interm_208_so_pt3(a,i,q,j) * wm_interm_57_so_pt3(a,i,p,j)
term(239) = term(239) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_57_so_pt3(a,i,p,j)
term(240) = term(240) + wm_interm_208_so_pt3(a,i,q,j) * wm_interm_58_so_pt3(a,p,i,j)
term(241) = term(241) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_58_so_pt3(a,p,i,j)
term(242) = term(242) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_57_so_pt3(a,i,p,j)
term(243) = term(243) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_58_so_pt3(a,p,i,j)
term(244) = term(244) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_57_so_pt3(a,p,i,j)
term(245) = term(245) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_58_so_pt3(a,i,p,j)
term(246) = term(246) + wm_interm_209_so_pt3(a,i,q,j) * wm_interm_57_so_pt3(a,p,i,j)
term(247) = term(247) + wm_interm_209_so_pt3(a,i,q,j) * wm_interm_58_so_pt3(a,i,p,j)
term(248) = term(248) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_57_so_pt3(a,p,i,j)
term(249) = term(249) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_58_so_pt3(a,i,p,j)
term(250) = term(250) + wm_interm_208_so_pt3(a,i,q,j) * wm_interm_59_so_pt3(a,p,i,j)
term(251) = term(251) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_59_so_pt3(a,p,i,j)
term(252) = term(252) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_59_so_pt3(a,p,i,j)
term(253) = term(253) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_59_so_pt3(a,i,p,j)
term(254) = term(254) + wm_interm_209_so_pt3(a,i,q,j) * wm_interm_59_so_pt3(a,i,p,j)
term(255) = term(255) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_59_so_pt3(a,i,p,j)
term(256) = term(256) + wm_interm_209_so_pt3(a,i,q,j) * wm_interm_60_so_pt3(a,p,i,j)
term(257) = term(257) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_60_so_pt3(a,p,i,j)
term(258) = term(258) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_60_so_pt3(a,i,p,j)
term(259) = term(259) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_60_so_pt3(a,p,i,j)
term(260) = term(260) + wm_interm_208_so_pt3(a,i,q,j) * wm_interm_60_so_pt3(a,i,p,j)
term(261) = term(261) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_60_so_pt3(a,i,p,j)
term(262) = term(262) + wm_interm_209_so_pt3(a,i,q,j) * wm_interm_61_so_pt3(a,p,i,j)
term(263) = term(263) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_61_so_pt3(a,p,i,j)
term(264) = term(264) + wm_interm_209_so_pt3(a,q,i,j) * wm_interm_61_so_pt3(a,i,p,j)
term(265) = term(265) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_61_so_pt3(a,p,i,j)
term(266) = term(266) + wm_interm_208_so_pt3(a,i,q,j) * wm_interm_61_so_pt3(a,i,p,j)
term(267) = term(267) + wm_interm_208_so_pt3(a,q,i,j) * wm_interm_61_so_pt3(a,i,p,j)
term(268) = term(268) + wm_interm_213_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(269) = term(269) + wm_interm_212_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(270) = term(270) + wm_interm_214_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(271) = term(271) + wm_interm_215_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(272) = term(272) + wm_interm_210_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(273) = term(273) + wm_interm_211_so_pt3(a,i,j,q) * wm_interm_52_so_pt3(a,p,i,j)
term(274) = term(274) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_54_so_pt3(a,p,i,j)
term(275) = term(275) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_54_so_pt3(a,p,i,j)
term(276) = term(276) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_54_so_pt3(a,i,p,j)
term(277) = term(277) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_54_so_pt3(a,i,p,j)
term(278) = term(278) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_57_so_pt3(a,i,p,j)
term(279) = term(279) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_57_so_pt3(a,i,p,j)
term(280) = term(280) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_58_so_pt3(a,p,i,j)
term(281) = term(281) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_58_so_pt3(a,p,i,j)
term(282) = term(282) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_57_so_pt3(a,p,i,j)
term(283) = term(283) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_58_so_pt3(a,i,p,j)
term(284) = term(284) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_57_so_pt3(a,p,i,j)
term(285) = term(285) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_58_so_pt3(a,i,p,j)
term(286) = term(286) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_59_so_pt3(a,p,i,j)
term(287) = term(287) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_59_so_pt3(a,p,i,j)
term(288) = term(288) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_59_so_pt3(a,i,p,j)
term(289) = term(289) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_59_so_pt3(a,i,p,j)
term(290) = term(290) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_60_so_pt3(a,p,i,j)
term(291) = term(291) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_60_so_pt3(a,p,i,j)
term(292) = term(292) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_60_so_pt3(a,i,p,j)
term(293) = term(293) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_60_so_pt3(a,i,p,j)
term(294) = term(294) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_61_so_pt3(a,p,i,j)
term(295) = term(295) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_61_so_pt3(a,p,i,j)
term(296) = term(296) + wm_interm_219_so_pt3(a,i,q,j) * wm_interm_61_so_pt3(a,i,p,j)
term(297) = term(297) + wm_interm_219_so_pt3(a,q,i,j) * wm_interm_61_so_pt3(a,i,p,j)
term(298) = term(298) + wm_interm_199_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,q,i)
term(299) = term(299) + wm_interm_200_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,q,i)
term(300) = term(300) + wm_interm_198_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,q,i)
term(301) = term(301) + wm_interm_195_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,q,i)
term(302) = term(302) + wm_interm_197_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,q,i)
term(303) = term(303) + wm_interm_201_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,q,i)
term(304) = term(304) + wm_interm_196_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,q,i)
term(305) = term(305) + wm_interm_195_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,q,i)
term(306) = term(306) + wm_interm_201_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,q,i)
term(307) = term(307) + wm_interm_192_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,q,i)
term(308) = term(308) + wm_interm_193_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,q,i)
term(309) = term(309) + wm_interm_194_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,q,i)
term(310) = term(310) + wm_interm_199_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(311) = term(311) + wm_interm_200_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(312) = term(312) + wm_interm_198_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(313) = term(313) + wm_interm_196_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(314) = term(314) + wm_interm_201_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(315) = term(315) + wm_interm_197_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(316) = term(316) + wm_interm_195_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(317) = term(317) + wm_interm_196_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(318) = term(318) + wm_interm_199_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(319) = term(319) + wm_interm_200_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(320) = term(320) + wm_interm_197_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(321) = term(321) + wm_interm_198_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(322) = term(322) + wm_interm_192_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(323) = term(323) + wm_interm_193_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(324) = term(324) + wm_interm_194_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(325) = term(325) + wm_interm_192_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(326) = term(326) + wm_interm_193_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(327) = term(327) + wm_interm_194_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(328) = term(328) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_57_so_pt3(a,j,i,q)
term(329) = term(329) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_57_so_pt3(a,i,j,q)
term(330) = term(330) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_54_so_pt3(a,i,j,q)
term(331) = term(331) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_54_so_pt3(a,j,i,q)
term(332) = term(332) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_60_so_pt3(a,j,i,q)
term(333) = term(333) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_60_so_pt3(a,i,j,q)
term(334) = term(334) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_58_so_pt3(a,i,j,q)
term(335) = term(335) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_58_so_pt3(a,j,i,q)
term(336) = term(336) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_59_so_pt3(a,i,j,q)
term(337) = term(337) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_59_so_pt3(a,j,i,q)
term(338) = term(338) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_61_so_pt3(a,j,i,q)
term(339) = term(339) + wm_interm_209_so_pt3(a,i,j,p) * wm_interm_61_so_pt3(a,i,j,q)
term(340) = term(340) + wm_interm_208_so_pt3(a,i,j,p) * wm_interm_54_so_pt3(a,j,i,q)
term(341) = term(341) + wm_interm_208_so_pt3(a,i,j,p) * wm_interm_57_so_pt3(a,i,j,q)
term(342) = term(342) + wm_interm_208_so_pt3(a,i,j,p) * wm_interm_58_so_pt3(a,j,i,q)
term(343) = term(343) + wm_interm_208_so_pt3(a,i,j,p) * wm_interm_60_so_pt3(a,i,j,q)
term(344) = term(344) + wm_interm_208_so_pt3(a,i,j,p) * wm_interm_61_so_pt3(a,i,j,q)
term(345) = term(345) + wm_interm_208_so_pt3(a,i,j,p) * wm_interm_59_so_pt3(a,j,i,q)
term(346) = term(346) + wm_interm_214_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,q,i)
term(347) = term(347) + wm_interm_215_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,q,i)
term(348) = term(348) + wm_interm_212_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,q,i)
term(349) = term(349) + wm_interm_213_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,q,i)
term(350) = term(350) + wm_interm_213_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,q,i)
term(351) = term(351) + wm_interm_212_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,q,i)
term(352) = term(352) + wm_interm_210_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,q,i)
term(353) = term(353) + wm_interm_211_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,q,i)
term(354) = term(354) + wm_interm_214_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(355) = term(355) + wm_interm_215_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(356) = term(356) + wm_interm_213_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(357) = term(357) + wm_interm_212_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(358) = term(358) + wm_interm_212_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(359) = term(359) + wm_interm_213_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(360) = term(360) + wm_interm_214_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(361) = term(361) + wm_interm_215_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(362) = term(362) + wm_interm_210_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(363) = term(363) + wm_interm_211_so_pt3(a,i,p,j) * wm_interm_52_so_pt3(a,j,i,q)
term(364) = term(364) + wm_interm_210_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(365) = term(365) + wm_interm_211_so_pt3(a,p,i,j) * wm_interm_52_so_pt3(a,j,i,q)
term(366) = term(366) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_57_so_pt3(a,i,j,q)
term(367) = term(367) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_57_so_pt3(a,j,i,q)
term(368) = term(368) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_54_so_pt3(a,j,i,q)
term(369) = term(369) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_54_so_pt3(a,i,j,q)
term(370) = term(370) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_60_so_pt3(a,i,j,q)
term(371) = term(371) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_60_so_pt3(a,j,i,q)
term(372) = term(372) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_58_so_pt3(a,j,i,q)
term(373) = term(373) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_58_so_pt3(a,i,j,q)
term(374) = term(374) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_59_so_pt3(a,j,i,q)
term(375) = term(375) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_59_so_pt3(a,i,j,q)
term(376) = term(376) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_61_so_pt3(a,i,j,q)
term(377) = term(377) + wm_interm_219_so_pt3(a,i,j,p) * wm_interm_61_so_pt3(a,j,i,q)
end do 
end do 
end do 

term(35) = term(35) * (-2.0d+0) 
term(38) = term(38) * (-1.9999999999999998d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (-1.9999999999999998d+0) 
term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (-2.0d+0) 
term(48) = term(48) * (-2.0d+0) 
term(52) = term(52) * (-1.9999999999999998d+0) 
term(53) = term(53) * (-2.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (-2.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (4.0d+0) 
term(58) = term(58) * (4.0d+0) 
term(59) = term(59) * (-3.9999999999999996d+0) 
term(60) = term(60) * (8.0d+0) 
term(61) = term(61) * (-8.0d+0) 
term(62) = term(62) * (2.0d+0) 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (1.9999999999999998d+0) 
term(66) = term(66) * (1.9999999999999998d+0) 
term(67) = term(67) * (-3.9999999999999996d+0) 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (8.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(73) = term(73) * (-2.0d+0) 
term(75) = term(75) * (-2.0d+0) 
term(77) = term(77) * (0.5d+0) 
term(79) = term(79) * (0.5d+0) 
term(81) = term(81) * (-1.0d+0) 
term(82) = term(82) * (-1.9999999999999998d+0) 
term(83) = term(83) * (-1.9999999999999998d+0) 
term(84) = term(84) * (-1.9999999999999998d+0) 
term(85) = term(85) * (3.9999999999999996d+0) 
term(86) = term(86) * (-2.0d+0) 
term(87) = term(87) * (4.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(90) = term(90) * (-2.0d+0) 
term(94) = term(94) * (-1.9999999999999998d+0) 
term(95) = term(95) * (0.5d+0) 
term(96) = term(96) * (0.5d+0) 
term(97) = term(97) * (-1.0d+0) 
term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (2.0d+0) 
term(101) = term(101) * (-4.0d+0) 
term(102) = term(102) * (2.0d+0) 
term(103) = term(103) * (1.9999999999999998d+0) 
term(104) = term(104) * (1.9999999999999998d+0) 
term(105) = term(105) * (1.9999999999999998d+0) 
term(106) = term(106) * (-2.0d+0) 
term(107) = term(107) * (-3.9999999999999996d+0) 
term(108) = term(108) * (-8.0d+0) 
term(109) = term(109) * (7.999999999999999d+0) 
term(110) = term(110) * (-4.0d+0) 
term(111) = term(111) * (8.0d+0) 
term(112) = term(112) * (-4.0d+0) 
term(113) = term(113) * (2.0d+0) 
term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * (2.0d+0) 
term(116) = term(116) * (1.9999999999999998d+0) 
term(117) = term(117) * (1.9999999999999998d+0) 
term(118) = term(118) * (-3.9999999999999996d+0) 
term(119) = term(119) * (1.9999999999999998d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * (3.0d+0) 
term(122) = term(122) * (-8.0d+0) 
term(123) = term(123) * (6.0d+0) 
term(124) = term(124) * (-2.0d+0) 
term(125) = term(125) * (-8.0d+0) 
term(126) = term(126) * (8.0d+0) 
term(127) = term(127) * (3.0d+0) 
term(128) = term(128) * (-4.0d+0) 
term(129) = term(129) * (6.0d+0) 
term(130) = term(130) * (-8.0d+0) 
term(131) = term(131) * (-4.0d+0) 
term(132) = term(132) * (8.0d+0) 
term(133) = term(133) * (3.0d+0) 
term(134) = term(134) * (-4.0d+0) 
term(135) = term(135) * (3.0d+0) 
term(136) = term(136) * (-4.0d+0) 
term(137) = term(137) * (-2.0d+0) 
term(138) = term(138) * (4.0d+0) 
term(139) = term(139) * (3.0d+0) 
term(140) = term(140) * (-4.0d+0) 
term(141) = term(141) * (3.0d+0) 
term(142) = term(142) * (-2.0d+0) 
term(143) = term(143) * (-4.0d+0) 
term(144) = term(144) * (4.0d+0) 
term(145) = term(145) * (3.0d+0) 
term(146) = term(146) * (-2.0d+0) 
term(147) = term(147) * (3.0d+0) 
term(148) = term(148) * (-4.0d+0) 
term(149) = term(149) * (3.0d+0) 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * (3.0d+0) 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * (-2.0d+0) 
term(154) = term(154) * (4.0d+0) 
term(155) = term(155) * (3.0d+0) 
term(156) = term(156) * (-4.0d+0) 
term(157) = term(157) * (3.0d+0) 
term(158) = term(158) * (-2.0d+0) 
term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * (4.0d+0) 
term(161) = term(161) * (-2.0000000000000004d+0) 
term(162) = term(162) * (4.000000000000001d+0) 
term(164) = term(164) * (-2.0000000000000004d+0) 
term(165) = term(165) * (2.0d+0) 
term(166) = term(166) * (-4.0d+0) 
term(167) = term(167) * (-4.0d+0) 
term(168) = term(168) * (8.0d+0) 
term(171) = term(171) * (-2.0d+0) 
term(172) = term(172) * (-2.0d+0) 
term(174) = term(174) * (-2.0d+0) 
term(176) = term(176) * (-2.0d+0) 
term(177) = term(177) * (6.0d+0) 
term(178) = term(178) * (-8.0d+0) 
term(179) = term(179) * (3.0d+0) 
term(180) = term(180) * (-2.0d+0) 
term(181) = term(181) * (3.0d+0) 
term(182) = term(182) * (-4.0d+0) 
term(183) = term(183) * (3.0d+0) 
term(184) = term(184) * (-4.0d+0) 
term(185) = term(185) * (3.0d+0) 
term(186) = term(186) * (3.0d+0) 
term(187) = term(187) * (-4.0d+0) 
term(188) = term(188) * (-4.0d+0) 
term(189) = term(189) * (-2.0d+0) 
term(190) = term(190) * (-2.0d+0) 
term(191) = term(191) * (4.0d+0) 
term(192) = term(192) * (4.0d+0) 
term(196) = term(196) * (-2.0d+0) 
term(198) = term(198) * (-2.0d+0) 
term(199) = term(199) * (2.0d+0) 
term(200) = term(200) * (-4.0d+0) 
term(201) = term(201) * (-1.9999999999999998d+0) 
term(202) = term(202) * (-1.9999999999999998d+0) 
term(207) = term(207) * (-2.0d+0) 
term(210) = term(210) * (-2.0d+0) 
term(211) = term(211) * (4.0d+0) 
term(212) = term(212) * (-2.0d+0) 
term(213) = term(213) * (-2.0d+0) 
term(214) = term(214) * (4.0d+0) 
term(215) = term(215) * (-2.0d+0) 
term(218) = term(218) * (-4.0d+0) 
term(219) = term(219) * (8.0d+0) 
term(220) = term(220) * (-2.0d+0) 
term(221) = term(221) * (1.9999999999999998d+0) 
term(222) = term(222) * (-3.9999999999999996d+0) 
term(223) = term(223) * (0.5d+0) 
term(224) = term(224) * (0.5d+0) 
term(225) = term(225) * (-1.0d+0) 
term(226) = term(226) * (-1.0d+0) 
term(227) = term(227) * (-1.0d+0) 
term(228) = term(228) * (2.0d+0) 
term(229) = term(229) * (0.5d+0) 
term(230) = term(230) * (-1.0d+0) 
term(231) = term(231) * (0.5d+0) 
term(232) = term(232) * (0.5d+0) 
term(233) = term(233) * (-1.0d+0) 
term(234) = term(234) * (0.5d+0) 
term(235) = term(235) * (0.5d+0) 
term(236) = term(236) * (0.5d+0) 
term(237) = term(237) * (-1.0d+0) 
term(238) = term(238) * (0.5d+0) 
term(239) = term(239) * (-1.0d+0) 
term(240) = term(240) * (-1.0d+0) 
term(241) = term(241) * (2.0d+0) 
term(242) = term(242) * (0.5d+0) 
term(243) = term(243) * (-1.0d+0) 
term(244) = term(244) * (0.5d+0) 
term(245) = term(245) * (-1.0d+0) 
term(246) = term(246) * (0.5d+0) 
term(247) = term(247) * (-1.0d+0) 
term(248) = term(248) * (-1.0d+0) 
term(249) = term(249) * (2.0d+0) 
term(250) = term(250) * (0.5d+0) 
term(251) = term(251) * (-1.0d+0) 
term(252) = term(252) * (0.5d+0) 
term(253) = term(253) * (0.5d+0) 
term(254) = term(254) * (0.5d+0) 
term(255) = term(255) * (-1.0d+0) 
term(256) = term(256) * (-1.0d+0) 
term(257) = term(257) * (2.0d+0) 
term(258) = term(258) * (-1.0d+0) 
term(259) = term(259) * (-1.0d+0) 
term(260) = term(260) * (-1.0d+0) 
term(261) = term(261) * (2.0d+0) 
term(262) = term(262) * (0.5d+0) 
term(263) = term(263) * (-1.0d+0) 
term(264) = term(264) * (0.5d+0) 
term(265) = term(265) * (0.5d+0) 
term(266) = term(266) * (0.5d+0) 
term(267) = term(267) * (-1.0d+0) 
term(268) = term(268) * (2.0d+0) 
term(269) = term(269) * (-2.0d+0) 
term(270) = term(270) * (-4.0d+0) 
term(271) = term(271) * (4.0d+0) 
term(272) = term(272) * (2.0d+0) 
term(273) = term(273) * (-2.0d+0) 
term(274) = term(274) * (2.0d+0) 
term(275) = term(275) * (-2.0d+0) 
term(276) = term(276) * (2.0d+0) 
term(277) = term(277) * (-2.0d+0) 
term(278) = term(278) * (2.0d+0) 
term(279) = term(279) * (-2.0d+0) 
term(280) = term(280) * (-4.0d+0) 
term(281) = term(281) * (4.0d+0) 
term(282) = term(282) * (2.0d+0) 
term(283) = term(283) * (-4.0d+0) 
term(284) = term(284) * (-2.0d+0) 
term(285) = term(285) * (4.0d+0) 
term(286) = term(286) * (2.0d+0) 
term(287) = term(287) * (-2.0d+0) 
term(288) = term(288) * (2.0d+0) 
term(289) = term(289) * (-2.0d+0) 
term(290) = term(290) * (-4.0d+0) 
term(291) = term(291) * (4.0d+0) 
term(292) = term(292) * (-4.0d+0) 
term(293) = term(293) * (4.0d+0) 
term(294) = term(294) * (2.0d+0) 
term(295) = term(295) * (-2.0d+0) 
term(296) = term(296) * (2.0d+0) 
term(297) = term(297) * (-2.0d+0) 
term(298) = term(298) * (0.5d+0) 
term(299) = term(299) * (-1.0d+0) 
term(300) = term(300) * (0.5d+0) 
term(301) = term(301) * (0.5d+0) 
term(302) = term(302) * (0.5d+0) 
term(303) = term(303) * (-1.0d+0) 
term(304) = term(304) * (0.5d+0) 
term(305) = term(305) * (-1.0d+0) 
term(306) = term(306) * (0.5d+0) 
term(307) = term(307) * (0.5d+0) 
term(308) = term(308) * (-1.0d+0) 
term(309) = term(309) * (0.5d+0) 
term(310) = term(310) * (0.5d+0) 
term(311) = term(311) * (-1.0d+0) 
term(312) = term(312) * (0.5d+0) 
term(313) = term(313) * (0.5d+0) 
term(314) = term(314) * (0.5d+0) 
term(315) = term(315) * (-1.0d+0) 
term(316) = term(316) * (0.5d+0) 
term(317) = term(317) * (-1.0d+0) 
term(318) = term(318) * (-1.0d+0) 
term(319) = term(319) * (2.0d+0) 
term(320) = term(320) * (0.5d+0) 
term(321) = term(321) * (-1.0d+0) 
term(322) = term(322) * (0.5d+0) 
term(323) = term(323) * (-1.0d+0) 
term(324) = term(324) * (0.5d+0) 
term(325) = term(325) * (-1.0d+0) 
term(326) = term(326) * (2.0d+0) 
term(327) = term(327) * (-1.0d+0) 
term(328) = term(328) * (0.5d+0) 
term(329) = term(329) * (-1.0d+0) 
term(330) = term(330) * (0.5d+0) 
term(331) = term(331) * (-1.0d+0) 
term(332) = term(332) * (-1.0d+0) 
term(333) = term(333) * (2.0d+0) 
term(334) = term(334) * (-1.0d+0) 
term(335) = term(335) * (2.0d+0) 
term(336) = term(336) * (0.5d+0) 
term(337) = term(337) * (-1.0d+0) 
term(338) = term(338) * (0.5d+0) 
term(339) = term(339) * (-1.0d+0) 
term(340) = term(340) * (0.5d+0) 
term(341) = term(341) * (0.5d+0) 
term(342) = term(342) * (-1.0d+0) 
term(343) = term(343) * (-1.0d+0) 
term(344) = term(344) * (0.5d+0) 
term(345) = term(345) * (0.5d+0) 
term(346) = term(346) * (2.0d+0) 
term(347) = term(347) * (-2.0d+0) 
term(348) = term(348) * (2.0d+0) 
term(349) = term(349) * (-2.0d+0) 
term(350) = term(350) * (2.0d+0) 
term(351) = term(351) * (-2.0d+0) 
term(352) = term(352) * (2.0d+0) 
term(353) = term(353) * (-2.0d+0) 
term(354) = term(354) * (2.0d+0) 
term(355) = term(355) * (-2.0d+0) 
term(356) = term(356) * (2.0d+0) 
term(357) = term(357) * (-2.0d+0) 
term(358) = term(358) * (2.0d+0) 
term(359) = term(359) * (-2.0d+0) 
term(360) = term(360) * (-4.0d+0) 
term(361) = term(361) * (4.0d+0) 
term(362) = term(362) * (2.0d+0) 
term(363) = term(363) * (-2.0d+0) 
term(364) = term(364) * (-4.0d+0) 
term(365) = term(365) * (4.0d+0) 
term(366) = term(366) * (2.0d+0) 
term(367) = term(367) * (-2.0d+0) 
term(368) = term(368) * (2.0d+0) 
term(369) = term(369) * (-2.0d+0) 
term(370) = term(370) * (-4.0d+0) 
term(371) = term(371) * (4.0d+0) 
term(372) = term(372) * (-4.0d+0) 
term(373) = term(373) * (4.0d+0) 
term(374) = term(374) * (2.0d+0) 
term(375) = term(375) * (-2.0d+0) 
term(376) = term(376) * (2.0d+0) 
term(377) = term(377) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(378) = term(378) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,p,k,j,i,q)
term(379) = term(379) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,p,k,j,q,i)
term(380) = term(380) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,k,p,j,i,q)
term(381) = term(381) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,k,p,j,q,i)
end do 
end do 
end do 
end do 

term(378) = term(378) * (3.0d+0) 
term(379) = term(379) * (-4.0d+0) 
term(380) = term(380) * (-4.0d+0) 
term(381) = term(381) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(382) = term(382) + wm_interm_205_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(383) = term(383) + wm_interm_206_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(384) = term(384) + wm_interm_206_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(385) = term(385) + wm_interm_204_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(386) = term(386) + wm_interm_207_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(387) = term(387) + wm_interm_207_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(388) = term(388) + wm_interm_202_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(389) = term(389) + wm_interm_203_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(390) = term(390) + wm_interm_203_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(391) = term(391) + wm_interm_218_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(392) = term(392) + wm_interm_218_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(393) = term(393) + wm_interm_217_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(394) = term(394) + wm_interm_217_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(395) = term(395) + wm_interm_216_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(396) = term(396) + wm_interm_216_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(397) = term(397) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,j,p,i,q,k)
term(398) = term(398) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,p,j,i,k,q)
term(399) = term(399) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,p,j,i,k,q)
term(400) = term(400) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,p,j,i,k,q)
term(401) = term(401) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,j,p,i,k,q)
term(402) = term(402) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,j,p,i,k,q)
term(403) = term(403) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,j,p,i,q,k)
term(404) = term(404) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,p,j,i,q,k)
term(405) = term(405) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,p,j,i,k,q)
term(406) = term(406) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,p,j,i,k,q)
term(407) = term(407) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,j,p,i,k,q)
term(408) = term(408) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,j,p,i,k,q)
term(409) = term(409) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,j,p,i,q,k)
term(410) = term(410) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,j,p,i,q,k)
term(411) = term(411) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,p,j,i,k,q)
term(412) = term(412) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,p,j,i,k,q)
term(413) = term(413) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,p,j,i,q,k)
term(414) = term(414) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,j,p,i,k,q)
term(415) = term(415) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,p,j,i,k,q)
term(416) = term(416) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,j,p,i,k,q)
end do 
end do 
end do 
end do 

term(382) = term(382) * (0.16666666666666666d+0) 
term(383) = term(383) * (0.16666666666666666d+0) 
term(384) = term(384) * (-0.3333333333333333d+0) 
term(385) = term(385) * (-0.3333333333333333d+0) 
term(386) = term(386) * (-0.3333333333333333d+0) 
term(387) = term(387) * (0.6666666666666666d+0) 
term(388) = term(388) * (0.16666666666666666d+0) 
term(389) = term(389) * (0.16666666666666666d+0) 
term(390) = term(390) * (-0.3333333333333333d+0) 
term(391) = term(391) * (0.6666666666666666d+0) 
term(392) = term(392) * (-0.6666666666666666d+0) 
term(393) = term(393) * (-1.3333333333333333d+0) 
term(394) = term(394) * (1.3333333333333333d+0) 
term(395) = term(395) * (0.6666666666666666d+0) 
term(396) = term(396) * (-0.6666666666666666d+0) 
term(397) = term(397) * (0.16666666666666666d+0) 
term(398) = term(398) * (0.16666666666666666d+0) 
term(399) = term(399) * (-0.3333333333333333d+0) 
term(400) = term(400) * (0.16666666666666666d+0) 
term(401) = term(401) * (0.16666666666666666d+0) 
term(402) = term(402) * (-0.3333333333333333d+0) 
term(403) = term(403) * (0.16666666666666666d+0) 
term(404) = term(404) * (0.16666666666666666d+0) 
term(405) = term(405) * (-0.3333333333333333d+0) 
term(406) = term(406) * (0.16666666666666666d+0) 
term(407) = term(407) * (0.16666666666666666d+0) 
term(408) = term(408) * (-0.3333333333333333d+0) 
term(409) = term(409) * (-0.6666666666666666d+0) 
term(410) = term(410) * (0.6666666666666666d+0) 
term(411) = term(411) * (-0.6666666666666666d+0) 
term(412) = term(412) * (0.6666666666666666d+0) 
term(413) = term(413) * (0.6666666666666666d+0) 
term(414) = term(414) * (0.6666666666666666d+0) 
term(415) = term(415) * (-0.6666666666666666d+0) 
term(416) = term(416) * (-0.6666666666666666d+0) 

do a = nocc + 1, nactive 
term(417) = term(417) + wm_interm_97_so_pt3(a,p) * wm_interm_9_so_pt3(a,q)
term(418) = term(418) + wm_interm_98_so_pt3(a,p) * wm_interm_9_so_pt3(a,q)
term(419) = term(419) + wm_interm_99_so_pt3(a,p) * wm_interm_9_so_pt3(a,q)
term(420) = term(420) + wm_interm_8_so_pt3(a,q) * wm_interm_97_so_pt3(a,p)
term(421) = term(421) + wm_interm_8_so_pt3(a,q) * wm_interm_98_so_pt3(a,p)
term(422) = term(422) + wm_interm_8_so_pt3(a,q) * wm_interm_99_so_pt3(a,p)
term(423) = term(423) + wm_interm_122_so_pt3(a,p) * wm_interm_9_so_pt3(a,q)
term(424) = term(424) + wm_interm_123_so_pt3(a,p) * wm_interm_9_so_pt3(a,q)
term(425) = term(425) + wm_interm_124_so_pt3(a,p) * wm_interm_9_so_pt3(a,q)
term(426) = term(426) + wm_interm_122_so_pt3(a,p) * wm_interm_8_so_pt3(a,q)
term(427) = term(427) + wm_interm_123_so_pt3(a,p) * wm_interm_8_so_pt3(a,q)
term(428) = term(428) + wm_interm_124_so_pt3(a,p) * wm_interm_8_so_pt3(a,q)
term(429) = term(429) + wm_interm_170_so_pt3(a,p) * wm_interm_19_so_pt3(a,q)
term(430) = term(430) + wm_interm_170_so_pt3(a,p) * wm_interm_20_so_pt3(a,q)
term(431) = term(431) + wm_interm_170_so_pt3(a,p) * wm_interm_21_so_pt3(a,q)
term(432) = term(432) + wm_interm_170_so_pt3(a,p) * wm_interm_22_so_pt3(a,q)
term(433) = term(433) + wm_interm_171_so_pt3(a,p) * wm_interm_19_so_pt3(a,q)
term(434) = term(434) + wm_interm_171_so_pt3(a,p) * wm_interm_20_so_pt3(a,q)
term(435) = term(435) + wm_interm_171_so_pt3(a,p) * wm_interm_21_so_pt3(a,q)
term(436) = term(436) + wm_interm_171_so_pt3(a,p) * wm_interm_22_so_pt3(a,q)
term(437) = term(437) + wm_interm_186_so_pt3(a,p) * wm_interm_33_so_pt3(a,q)
term(438) = term(438) + wm_interm_186_so_pt3(a,p) * wm_interm_34_so_pt3(a,q)
term(439) = term(439) + wm_interm_186_so_pt3(a,p) * wm_interm_36_so_pt3(a,q)
term(440) = term(440) + wm_interm_186_so_pt3(a,p) * wm_interm_38_so_pt3(a,q)
term(441) = term(441) + wm_interm_136_so_pt3(a,q) * wm_interm_46_so_pt3(a,p)
term(442) = term(442) + wm_interm_137_so_pt3(a,q) * wm_interm_46_so_pt3(a,p)
term(443) = term(443) + wm_interm_134_so_pt3(a,q) * wm_interm_46_so_pt3(a,p)
term(444) = term(444) + wm_interm_127_so_pt3(a,q) * wm_interm_46_so_pt3(a,p)
term(445) = term(445) + wm_interm_187_so_pt3(a,q) * wm_interm_62_so_pt3(a,p)
term(446) = term(446) + wm_interm_187_so_pt3(a,q) * wm_interm_64_so_pt3(a,p)
term(447) = term(447) + wm_interm_188_so_pt3(a,q) * wm_interm_62_so_pt3(a,p)
term(448) = term(448) + wm_interm_188_so_pt3(a,q) * wm_interm_64_so_pt3(a,p)
term(449) = term(449) + wm_interm_189_so_pt3(a,q) * wm_interm_62_so_pt3(a,p)
term(450) = term(450) + wm_interm_189_so_pt3(a,q) * wm_interm_64_so_pt3(a,p)
term(451) = term(451) + wm_interm_187_so_pt3(a,q) * wm_interm_65_so_pt3(a,p)
term(452) = term(452) + wm_interm_187_so_pt3(a,q) * wm_interm_66_so_pt3(a,p)
term(453) = term(453) + wm_interm_188_so_pt3(a,q) * wm_interm_65_so_pt3(a,p)
term(454) = term(454) + wm_interm_188_so_pt3(a,q) * wm_interm_66_so_pt3(a,p)
term(455) = term(455) + wm_interm_189_so_pt3(a,q) * wm_interm_65_so_pt3(a,p)
term(456) = term(456) + wm_interm_189_so_pt3(a,q) * wm_interm_66_so_pt3(a,p)
term(457) = term(457) + wm_interm_190_so_pt3(a,q) * wm_interm_62_so_pt3(a,p)
term(458) = term(458) + wm_interm_190_so_pt3(a,q) * wm_interm_64_so_pt3(a,p)
term(459) = term(459) + wm_interm_191_so_pt3(a,q) * wm_interm_62_so_pt3(a,p)
term(460) = term(460) + wm_interm_191_so_pt3(a,q) * wm_interm_64_so_pt3(a,p)
term(461) = term(461) + wm_interm_190_so_pt3(a,q) * wm_interm_65_so_pt3(a,p)
term(462) = term(462) + wm_interm_190_so_pt3(a,q) * wm_interm_66_so_pt3(a,p)
term(463) = term(463) + wm_interm_191_so_pt3(a,q) * wm_interm_65_so_pt3(a,p)
term(464) = term(464) + wm_interm_191_so_pt3(a,q) * wm_interm_66_so_pt3(a,p)
term(465) = term(465) + wm_interm_220_so_pt3(a,p) * wm_interm_48_so_pt3(a,q)
term(466) = term(466) + wm_interm_221_so_pt3(a,p) * wm_interm_48_so_pt3(a,q)
term(467) = term(467) + wm_interm_222_so_pt3(a,p) * wm_interm_48_so_pt3(a,q)
term(468) = term(468) + wm_interm_220_so_pt3(a,p) * wm_interm_53_so_pt3(a,q)
term(469) = term(469) + wm_interm_221_so_pt3(a,p) * wm_interm_53_so_pt3(a,q)
term(470) = term(470) + wm_interm_222_so_pt3(a,p) * wm_interm_53_so_pt3(a,q)
term(471) = term(471) + wm_interm_227_so_pt3(a,p) * wm_interm_48_so_pt3(a,q)
term(472) = term(472) + wm_interm_228_so_pt3(a,p) * wm_interm_48_so_pt3(a,q)
term(473) = term(473) + wm_interm_227_so_pt3(a,p) * wm_interm_53_so_pt3(a,q)
term(474) = term(474) + wm_interm_228_so_pt3(a,p) * wm_interm_53_so_pt3(a,q)
end do 

term(418) = term(418) * (-2.0d+0) 
term(420) = term(420) * (-2.0d+0) 
term(421) = term(421) * (4.0d+0) 
term(422) = term(422) * (-2.0d+0) 
term(423) = term(423) * (2.0d+0) 
term(424) = term(424) * (-4.0d+0) 
term(425) = term(425) * (2.0d+0) 
term(426) = term(426) * (-4.0d+0) 
term(427) = term(427) * (8.0d+0) 
term(428) = term(428) * (-4.0d+0) 
term(429) = term(429) * (-16.0d+0) 
term(430) = term(430) * (8.0d+0) 
term(431) = term(431) * (12.0d+0) 
term(432) = term(432) * (-4.0d+0) 
term(433) = term(433) * (8.0d+0) 
term(434) = term(434) * (-4.0d+0) 
term(435) = term(435) * (-6.0d+0) 
term(436) = term(436) * (2.0d+0) 
term(437) = term(437) * (-1.9999999999999998d+0) 
term(438) = term(438) * (1.9999999999999996d+0) 
term(439) = term(439) * (3.9999999999999996d+0) 
term(440) = term(440) * (-3.9999999999999996d+0) 
term(441) = term(441) * (2.0d+0) 
term(442) = term(442) * (-4.0d+0) 
term(443) = term(443) * (-1.9999999999999998d+0) 
term(444) = term(444) * (3.9999999999999996d+0) 
term(445) = term(445) * (-3.9999999999999996d+0) 
term(446) = term(446) * (4.0d+0) 
term(447) = term(447) * (1.9999999999999998d+0) 
term(448) = term(448) * (-2.0d+0) 
term(449) = term(449) * (1.9999999999999998d+0) 
term(450) = term(450) * (-2.0d+0) 
term(451) = term(451) * (1.9999999999999998d+0) 
term(452) = term(452) * (-2.0d+0) 
term(453) = term(453) * (-0.9999999999999999d+0) 
term(455) = term(455) * (-0.9999999999999999d+0) 
term(457) = term(457) * (-7.999999999999999d+0) 
term(458) = term(458) * (8.0d+0) 
term(459) = term(459) * (7.999999999999999d+0) 
term(460) = term(460) * (-8.0d+0) 
term(461) = term(461) * (3.9999999999999996d+0) 
term(462) = term(462) * (-4.0d+0) 
term(463) = term(463) * (-3.9999999999999996d+0) 
term(464) = term(464) * (4.0d+0) 
term(465) = term(465) * (-1.9999999999999998d+0) 
term(466) = term(466) * (3.9999999999999996d+0) 
term(467) = term(467) * (-1.9999999999999998d+0) 
term(469) = term(469) * (-1.9999999999999998d+0) 
term(471) = term(471) * (-7.999999999999999d+0) 
term(472) = term(472) * (7.999999999999999d+0) 
term(473) = term(473) * (3.9999999999999996d+0) 
term(474) = term(474) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(475) = term(475) + wm_interm_5_so_pt3(a,i,j,k) * wm_interm_88_so_pt3(a,q,i,k,p,j)
term(476) = term(476) + wm_interm_5_so_pt3(a,i,j,k) * wm_interm_88_so_pt3(a,i,q,p,k,j)
term(477) = term(477) + wm_interm_5_so_pt3(a,i,j,k) * wm_interm_88_so_pt3(a,q,i,p,k,j)
term(478) = term(478) + wm_interm_5_so_pt3(a,i,j,k) * wm_interm_88_so_pt3(a,i,q,k,j,p)
term(479) = term(479) + wm_interm_5_so_pt3(a,i,j,k) * wm_interm_88_so_pt3(a,q,i,k,j,p)
term(480) = term(480) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,p,k,i,q,j)
term(481) = term(481) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,p,k,i,j,q)
term(482) = term(482) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,k,p,i,j,q)
term(483) = term(483) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,k,p,i,q,j)
end do 
end do 
end do 
end do 

term(477) = term(477) * (-2.0d+0) 
term(479) = term(479) * (-1.9999999999999998d+0) 
term(480) = term(480) * (3.0d+0) 
term(481) = term(481) * (-4.0d+0) 
term(482) = term(482) * (3.0d+0) 
term(483) = term(483) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(484) = term(484) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,p,k,q,j,i)
term(485) = term(485) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,k,p,q,j,i)
end do 
end do 
end do 
end do 

term(484) = term(484) * (4.0d+0) 
term(485) = term(485) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(486) = term(486) + wm_interm_85_so_pt3(a,q,i,p) * wm_interm_8_so_pt3(a,i)
term(487) = term(487) + wm_interm_86_so_pt3(a,q,i,p) * wm_interm_8_so_pt3(a,i)
term(488) = term(488) + wm_interm_87_so_pt3(a,q,i,p) * wm_interm_8_so_pt3(a,i)
term(489) = term(489) + wm_interm_8_so_pt3(a,i) * wm_interm_90_so_pt3(a,q,i,p)
term(490) = term(490) + wm_interm_8_so_pt3(a,i) * wm_interm_91_so_pt3(a,q,p,i)
term(491) = term(491) + wm_interm_8_so_pt3(a,i) * wm_interm_90_so_pt3(a,q,p,i)
term(492) = term(492) + wm_interm_85_so_pt3(a,q,p,i) * wm_interm_8_so_pt3(a,i)
term(493) = term(493) + wm_interm_86_so_pt3(a,q,p,i) * wm_interm_8_so_pt3(a,i)
term(494) = term(494) + wm_interm_87_so_pt3(a,q,p,i) * wm_interm_8_so_pt3(a,i)
term(495) = term(495) + wm_interm_85_so_pt3(a,q,i,p) * wm_interm_9_so_pt3(a,i)
term(496) = term(496) + wm_interm_86_so_pt3(a,q,i,p) * wm_interm_9_so_pt3(a,i)
term(497) = term(497) + wm_interm_87_so_pt3(a,q,i,p) * wm_interm_9_so_pt3(a,i)
term(498) = term(498) + wm_interm_90_so_pt3(a,q,i,p) * wm_interm_9_so_pt3(a,i)
term(499) = term(499) + wm_interm_91_so_pt3(a,q,p,i) * wm_interm_9_so_pt3(a,i)
term(500) = term(500) + wm_interm_90_so_pt3(a,q,p,i) * wm_interm_9_so_pt3(a,i)
term(501) = term(501) + wm_interm_85_so_pt3(a,q,p,i) * wm_interm_9_so_pt3(a,i)
term(502) = term(502) + wm_interm_86_so_pt3(a,q,p,i) * wm_interm_9_so_pt3(a,i)
term(503) = term(503) + wm_interm_87_so_pt3(a,q,p,i) * wm_interm_9_so_pt3(a,i)
term(504) = term(504) + wm_interm_114_so_pt3(a,q,i,p) * wm_interm_8_so_pt3(a,i)
term(505) = term(505) + wm_interm_115_so_pt3(a,q,i,p) * wm_interm_8_so_pt3(a,i)
term(506) = term(506) + wm_interm_116_so_pt3(a,q,i,p) * wm_interm_8_so_pt3(a,i)
term(507) = term(507) + wm_interm_118_so_pt3(a,q,i,p) * wm_interm_8_so_pt3(a,i)
term(508) = term(508) + wm_interm_119_so_pt3(a,q,p,i) * wm_interm_8_so_pt3(a,i)
term(509) = term(509) + wm_interm_118_so_pt3(a,q,p,i) * wm_interm_8_so_pt3(a,i)
term(510) = term(510) + wm_interm_114_so_pt3(a,q,p,i) * wm_interm_8_so_pt3(a,i)
term(511) = term(511) + wm_interm_115_so_pt3(a,q,p,i) * wm_interm_8_so_pt3(a,i)
term(512) = term(512) + wm_interm_116_so_pt3(a,q,p,i) * wm_interm_8_so_pt3(a,i)
term(513) = term(513) + wm_interm_114_so_pt3(a,q,i,p) * wm_interm_9_so_pt3(a,i)
term(514) = term(514) + wm_interm_115_so_pt3(a,q,i,p) * wm_interm_9_so_pt3(a,i)
term(515) = term(515) + wm_interm_116_so_pt3(a,q,i,p) * wm_interm_9_so_pt3(a,i)
term(516) = term(516) + wm_interm_118_so_pt3(a,q,i,p) * wm_interm_9_so_pt3(a,i)
term(517) = term(517) + wm_interm_119_so_pt3(a,q,p,i) * wm_interm_9_so_pt3(a,i)
term(518) = term(518) + wm_interm_118_so_pt3(a,q,p,i) * wm_interm_9_so_pt3(a,i)
term(519) = term(519) + wm_interm_114_so_pt3(a,q,p,i) * wm_interm_9_so_pt3(a,i)
term(520) = term(520) + wm_interm_115_so_pt3(a,q,p,i) * wm_interm_9_so_pt3(a,i)
term(521) = term(521) + wm_interm_116_so_pt3(a,q,p,i) * wm_interm_9_so_pt3(a,i)
term(522) = term(522) + wm_interm_164_so_pt3(a,i) * wm_interm_1_so_pt3(a,p,i,q)
term(523) = term(523) + wm_interm_165_so_pt3(a,i) * wm_interm_1_so_pt3(a,p,i,q)
term(524) = term(524) + wm_interm_166_so_pt3(a,i) * wm_interm_1_so_pt3(a,p,i,q)
term(525) = term(525) + wm_interm_164_so_pt3(a,i) * wm_interm_2_so_pt3(a,i,p,q)
term(526) = term(526) + wm_interm_165_so_pt3(a,i) * wm_interm_2_so_pt3(a,i,p,q)
term(527) = term(527) + wm_interm_166_so_pt3(a,i) * wm_interm_2_so_pt3(a,i,p,q)
term(528) = term(528) + wm_interm_164_so_pt3(a,i) * wm_interm_3_so_pt3(a,p,i,q)
term(529) = term(529) + wm_interm_165_so_pt3(a,i) * wm_interm_3_so_pt3(a,p,i,q)
term(530) = term(530) + wm_interm_166_so_pt3(a,i) * wm_interm_3_so_pt3(a,p,i,q)
term(531) = term(531) + wm_interm_5_so_pt3(a,p,i,q) * wm_interm_97_so_pt3(a,i)
term(532) = term(532) + wm_interm_5_so_pt3(a,p,i,q) * wm_interm_98_so_pt3(a,i)
term(533) = term(533) + wm_interm_5_so_pt3(a,p,i,q) * wm_interm_99_so_pt3(a,i)
term(534) = term(534) + wm_interm_164_so_pt3(a,i) * wm_interm_167_so_pt3(a,p,i,q)
term(535) = term(535) + wm_interm_165_so_pt3(a,i) * wm_interm_167_so_pt3(a,p,i,q)
term(536) = term(536) + wm_interm_166_so_pt3(a,i) * wm_interm_167_so_pt3(a,p,i,q)
term(537) = term(537) + wm_interm_168_so_pt3(a,i) * wm_interm_1_so_pt3(a,p,i,q)
term(538) = term(538) + wm_interm_169_so_pt3(a,i) * wm_interm_1_so_pt3(a,p,i,q)
term(539) = term(539) + wm_interm_168_so_pt3(a,i) * wm_interm_2_so_pt3(a,i,p,q)
term(540) = term(540) + wm_interm_169_so_pt3(a,i) * wm_interm_2_so_pt3(a,i,p,q)
term(541) = term(541) + wm_interm_168_so_pt3(a,i) * wm_interm_3_so_pt3(a,p,i,q)
term(542) = term(542) + wm_interm_169_so_pt3(a,i) * wm_interm_3_so_pt3(a,p,i,q)
term(543) = term(543) + wm_interm_122_so_pt3(a,i) * wm_interm_5_so_pt3(a,p,i,q)
term(544) = term(544) + wm_interm_123_so_pt3(a,i) * wm_interm_5_so_pt3(a,p,i,q)
term(545) = term(545) + wm_interm_124_so_pt3(a,i) * wm_interm_5_so_pt3(a,p,i,q)
term(546) = term(546) + wm_interm_167_so_pt3(a,p,i,q) * wm_interm_168_so_pt3(a,i)
term(547) = term(547) + wm_interm_167_so_pt3(a,p,i,q) * wm_interm_169_so_pt3(a,i)
term(548) = term(548) + wm_interm_180_so_pt3(a,p,i,q) * wm_interm_33_so_pt3(a,i)
term(549) = term(549) + wm_interm_180_so_pt3(a,p,i,q) * wm_interm_36_so_pt3(a,i)
term(550) = term(550) + wm_interm_180_so_pt3(a,i,p,q) * wm_interm_33_so_pt3(a,i)
term(551) = term(551) + wm_interm_180_so_pt3(a,i,p,q) * wm_interm_36_so_pt3(a,i)
term(552) = term(552) + wm_interm_181_so_pt3(a,p,i,q) * wm_interm_33_so_pt3(a,i)
term(553) = term(553) + wm_interm_181_so_pt3(a,p,i,q) * wm_interm_36_so_pt3(a,i)
term(554) = term(554) + wm_interm_181_so_pt3(a,i,p,q) * wm_interm_33_so_pt3(a,i)
term(555) = term(555) + wm_interm_181_so_pt3(a,i,p,q) * wm_interm_36_so_pt3(a,i)
term(556) = term(556) + wm_interm_181_so_pt3(a,p,i,q) * wm_interm_34_so_pt3(a,i)
term(557) = term(557) + wm_interm_181_so_pt3(a,p,i,q) * wm_interm_38_so_pt3(a,i)
term(558) = term(558) + wm_interm_180_so_pt3(a,p,i,q) * wm_interm_34_so_pt3(a,i)
term(559) = term(559) + wm_interm_180_so_pt3(a,p,i,q) * wm_interm_38_so_pt3(a,i)
term(560) = term(560) + wm_interm_127_so_pt3(a,i) * wm_interm_41_so_pt3(a,q,i,p)
term(561) = term(561) + wm_interm_127_so_pt3(a,i) * wm_interm_41_so_pt3(a,q,p,i)
term(562) = term(562) + wm_interm_180_so_pt3(a,i,p,q) * wm_interm_34_so_pt3(a,i)
term(563) = term(563) + wm_interm_180_so_pt3(a,i,p,q) * wm_interm_38_so_pt3(a,i)
term(564) = term(564) + wm_interm_134_so_pt3(a,i) * wm_interm_41_so_pt3(a,q,i,p)
term(565) = term(565) + wm_interm_134_so_pt3(a,i) * wm_interm_41_so_pt3(a,q,p,i)
term(566) = term(566) + wm_interm_181_so_pt3(a,i,p,q) * wm_interm_34_so_pt3(a,i)
term(567) = term(567) + wm_interm_181_so_pt3(a,i,p,q) * wm_interm_38_so_pt3(a,i)
term(568) = term(568) + wm_interm_136_so_pt3(a,i) * wm_interm_41_so_pt3(a,q,p,i)
term(569) = term(569) + wm_interm_136_so_pt3(a,i) * wm_interm_41_so_pt3(a,q,i,p)
term(570) = term(570) + wm_interm_137_so_pt3(a,i) * wm_interm_41_so_pt3(a,q,i,p)
term(571) = term(571) + wm_interm_137_so_pt3(a,i) * wm_interm_41_so_pt3(a,q,p,i)
term(572) = term(572) + wm_interm_12_so_pt3(a,q,i,p) * wm_interm_170_so_pt3(a,i)
term(573) = term(573) + wm_interm_12_so_pt3(a,i,q,p) * wm_interm_170_so_pt3(a,i)
term(574) = term(574) + wm_interm_15_so_pt3(a,i,q,p) * wm_interm_170_so_pt3(a,i)
term(575) = term(575) + wm_interm_13_so_pt3(a,q,i,p) * wm_interm_170_so_pt3(a,i)
term(576) = term(576) + wm_interm_14_so_pt3(a,i,q,p) * wm_interm_170_so_pt3(a,i)
term(577) = term(577) + wm_interm_13_so_pt3(a,i,q,p) * wm_interm_170_so_pt3(a,i)
term(578) = term(578) + wm_interm_12_so_pt3(a,q,i,p) * wm_interm_171_so_pt3(a,i)
term(579) = term(579) + wm_interm_12_so_pt3(a,i,q,p) * wm_interm_171_so_pt3(a,i)
term(580) = term(580) + wm_interm_15_so_pt3(a,i,q,p) * wm_interm_171_so_pt3(a,i)
term(581) = term(581) + wm_interm_13_so_pt3(a,q,i,p) * wm_interm_171_so_pt3(a,i)
term(582) = term(582) + wm_interm_14_so_pt3(a,i,q,p) * wm_interm_171_so_pt3(a,i)
term(583) = term(583) + wm_interm_13_so_pt3(a,i,q,p) * wm_interm_171_so_pt3(a,i)
term(584) = term(584) + wm_interm_179_so_pt3(a,i,p,q) * wm_interm_19_so_pt3(a,i)
term(585) = term(585) + wm_interm_179_so_pt3(a,i,p,q) * wm_interm_20_so_pt3(a,i)
term(586) = term(586) + wm_interm_176_so_pt3(a,p,i,q) * wm_interm_19_so_pt3(a,i)
term(587) = term(587) + wm_interm_176_so_pt3(a,p,i,q) * wm_interm_20_so_pt3(a,i)
term(588) = term(588) + wm_interm_179_so_pt3(a,i,p,q) * wm_interm_21_so_pt3(a,i)
term(589) = term(589) + wm_interm_176_so_pt3(a,p,i,q) * wm_interm_21_so_pt3(a,i)
term(590) = term(590) + wm_interm_179_so_pt3(a,i,p,q) * wm_interm_22_so_pt3(a,i)
term(591) = term(591) + wm_interm_176_so_pt3(a,p,i,q) * wm_interm_22_so_pt3(a,i)
term(592) = term(592) + wm_interm_10_so_pt3(a,p,i,q) * wm_interm_182_so_pt3(a,i)
term(593) = term(593) + wm_interm_10_so_pt3(a,p,i,q) * wm_interm_183_so_pt3(a,i)
term(594) = term(594) + wm_interm_10_so_pt3(a,p,i,q) * wm_interm_184_so_pt3(a,i)
term(595) = term(595) + wm_interm_10_so_pt3(a,p,i,q) * wm_interm_185_so_pt3(a,i)
term(596) = term(596) + wm_interm_186_so_pt3(a,i) * wm_interm_31_so_pt3(a,p,q,i)
term(597) = term(597) + wm_interm_186_so_pt3(a,i) * wm_interm_35_so_pt3(a,p,q,i)
term(598) = term(598) + wm_interm_186_so_pt3(a,i) * wm_interm_37_so_pt3(a,p,i,q)
term(599) = term(599) + wm_interm_138_so_pt3(a,p,q,i) * wm_interm_46_so_pt3(a,i)
term(600) = term(600) + wm_interm_135_so_pt3(a,p,i,q) * wm_interm_46_so_pt3(a,i)
term(601) = term(601) + wm_interm_125_so_pt3(a,p,i,q) * wm_interm_46_so_pt3(a,i)
term(602) = term(602) + wm_interm_126_so_pt3(a,p,i,q) * wm_interm_46_so_pt3(a,i)
term(603) = term(603) + wm_interm_192_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(604) = term(604) + wm_interm_193_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(605) = term(605) + wm_interm_194_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(606) = term(606) + wm_interm_195_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(607) = term(607) + wm_interm_196_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(608) = term(608) + wm_interm_197_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(609) = term(609) + wm_interm_194_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(610) = term(610) + wm_interm_192_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(611) = term(611) + wm_interm_193_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(612) = term(612) + wm_interm_198_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(613) = term(613) + wm_interm_198_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(614) = term(614) + wm_interm_199_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(615) = term(615) + wm_interm_199_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(616) = term(616) + wm_interm_200_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(617) = term(617) + wm_interm_200_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(618) = term(618) + wm_interm_196_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(619) = term(619) + wm_interm_201_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(620) = term(620) + wm_interm_197_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(621) = term(621) + wm_interm_195_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(622) = term(622) + wm_interm_196_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(623) = term(623) + wm_interm_197_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(624) = term(624) + wm_interm_198_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(625) = term(625) + wm_interm_199_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(626) = term(626) + wm_interm_200_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(627) = term(627) + wm_interm_192_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(628) = term(628) + wm_interm_193_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(629) = term(629) + wm_interm_194_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(630) = term(630) + wm_interm_196_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(631) = term(631) + wm_interm_192_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(632) = term(632) + wm_interm_193_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(633) = term(633) + wm_interm_201_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(634) = term(634) + wm_interm_197_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(635) = term(635) + wm_interm_194_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(636) = term(636) + wm_interm_198_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(637) = term(637) + wm_interm_199_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(638) = term(638) + wm_interm_200_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(639) = term(639) + wm_interm_210_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(640) = term(640) + wm_interm_211_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(641) = term(641) + wm_interm_212_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(642) = term(642) + wm_interm_213_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(643) = term(643) + wm_interm_210_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(644) = term(644) + wm_interm_211_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(645) = term(645) + wm_interm_214_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(646) = term(646) + wm_interm_214_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(647) = term(647) + wm_interm_215_so_pt3(a,p,i,q) * wm_interm_48_so_pt3(a,i)
term(648) = term(648) + wm_interm_215_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(649) = term(649) + wm_interm_213_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(650) = term(650) + wm_interm_212_so_pt3(a,i,p,q) * wm_interm_48_so_pt3(a,i)
term(651) = term(651) + wm_interm_212_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(652) = term(652) + wm_interm_213_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(653) = term(653) + wm_interm_214_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(654) = term(654) + wm_interm_215_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(655) = term(655) + wm_interm_210_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(656) = term(656) + wm_interm_211_so_pt3(a,p,i,q) * wm_interm_53_so_pt3(a,i)
term(657) = term(657) + wm_interm_213_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(658) = term(658) + wm_interm_210_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(659) = term(659) + wm_interm_211_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(660) = term(660) + wm_interm_212_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(661) = term(661) + wm_interm_214_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(662) = term(662) + wm_interm_215_so_pt3(a,i,p,q) * wm_interm_53_so_pt3(a,i)
term(663) = term(663) + wm_interm_209_so_pt3(a,i,q,p) * wm_interm_62_so_pt3(a,i)
term(664) = term(664) + wm_interm_209_so_pt3(a,q,i,p) * wm_interm_62_so_pt3(a,i)
term(665) = term(665) + wm_interm_209_so_pt3(a,i,q,p) * wm_interm_64_so_pt3(a,i)
term(666) = term(666) + wm_interm_209_so_pt3(a,q,i,p) * wm_interm_64_so_pt3(a,i)
term(667) = term(667) + wm_interm_208_so_pt3(a,q,i,p) * wm_interm_62_so_pt3(a,i)
term(668) = term(668) + wm_interm_208_so_pt3(a,q,i,p) * wm_interm_64_so_pt3(a,i)
term(669) = term(669) + wm_interm_209_so_pt3(a,i,q,p) * wm_interm_65_so_pt3(a,i)
term(670) = term(670) + wm_interm_209_so_pt3(a,q,i,p) * wm_interm_65_so_pt3(a,i)
term(671) = term(671) + wm_interm_209_so_pt3(a,i,q,p) * wm_interm_66_so_pt3(a,i)
term(672) = term(672) + wm_interm_209_so_pt3(a,q,i,p) * wm_interm_66_so_pt3(a,i)
term(673) = term(673) + wm_interm_208_so_pt3(a,q,i,p) * wm_interm_65_so_pt3(a,i)
term(674) = term(674) + wm_interm_208_so_pt3(a,q,i,p) * wm_interm_66_so_pt3(a,i)
term(675) = term(675) + wm_interm_219_so_pt3(a,q,i,p) * wm_interm_62_so_pt3(a,i)
term(676) = term(676) + wm_interm_219_so_pt3(a,i,q,p) * wm_interm_62_so_pt3(a,i)
term(677) = term(677) + wm_interm_219_so_pt3(a,q,i,p) * wm_interm_64_so_pt3(a,i)
term(678) = term(678) + wm_interm_219_so_pt3(a,i,q,p) * wm_interm_64_so_pt3(a,i)
term(679) = term(679) + wm_interm_219_so_pt3(a,q,i,p) * wm_interm_65_so_pt3(a,i)
term(680) = term(680) + wm_interm_219_so_pt3(a,i,q,p) * wm_interm_65_so_pt3(a,i)
term(681) = term(681) + wm_interm_219_so_pt3(a,q,i,p) * wm_interm_66_so_pt3(a,i)
term(682) = term(682) + wm_interm_219_so_pt3(a,i,q,p) * wm_interm_66_so_pt3(a,i)
term(683) = term(683) + wm_interm_187_so_pt3(a,i) * wm_interm_57_so_pt3(a,i,p,q)
term(684) = term(684) + wm_interm_187_so_pt3(a,i) * wm_interm_54_so_pt3(a,p,i,q)
term(685) = term(685) + wm_interm_187_so_pt3(a,i) * wm_interm_60_so_pt3(a,i,p,q)
term(686) = term(686) + wm_interm_187_so_pt3(a,i) * wm_interm_58_so_pt3(a,p,i,q)
term(687) = term(687) + wm_interm_187_so_pt3(a,i) * wm_interm_59_so_pt3(a,p,i,q)
term(688) = term(688) + wm_interm_187_so_pt3(a,i) * wm_interm_61_so_pt3(a,i,p,q)
term(689) = term(689) + wm_interm_188_so_pt3(a,i) * wm_interm_57_so_pt3(a,i,p,q)
term(690) = term(690) + wm_interm_188_so_pt3(a,i) * wm_interm_54_so_pt3(a,p,i,q)
term(691) = term(691) + wm_interm_188_so_pt3(a,i) * wm_interm_60_so_pt3(a,i,p,q)
term(692) = term(692) + wm_interm_188_so_pt3(a,i) * wm_interm_58_so_pt3(a,p,i,q)
term(693) = term(693) + wm_interm_188_so_pt3(a,i) * wm_interm_59_so_pt3(a,p,i,q)
term(694) = term(694) + wm_interm_188_so_pt3(a,i) * wm_interm_61_so_pt3(a,i,p,q)
term(695) = term(695) + wm_interm_189_so_pt3(a,i) * wm_interm_57_so_pt3(a,i,p,q)
term(696) = term(696) + wm_interm_189_so_pt3(a,i) * wm_interm_54_so_pt3(a,p,i,q)
term(697) = term(697) + wm_interm_189_so_pt3(a,i) * wm_interm_60_so_pt3(a,i,p,q)
term(698) = term(698) + wm_interm_189_so_pt3(a,i) * wm_interm_58_so_pt3(a,p,i,q)
term(699) = term(699) + wm_interm_189_so_pt3(a,i) * wm_interm_59_so_pt3(a,p,i,q)
term(700) = term(700) + wm_interm_189_so_pt3(a,i) * wm_interm_61_so_pt3(a,i,p,q)
term(701) = term(701) + wm_interm_190_so_pt3(a,i) * wm_interm_57_so_pt3(a,i,p,q)
term(702) = term(702) + wm_interm_190_so_pt3(a,i) * wm_interm_54_so_pt3(a,p,i,q)
term(703) = term(703) + wm_interm_190_so_pt3(a,i) * wm_interm_60_so_pt3(a,i,p,q)
term(704) = term(704) + wm_interm_190_so_pt3(a,i) * wm_interm_58_so_pt3(a,p,i,q)
term(705) = term(705) + wm_interm_190_so_pt3(a,i) * wm_interm_59_so_pt3(a,p,i,q)
term(706) = term(706) + wm_interm_190_so_pt3(a,i) * wm_interm_61_so_pt3(a,i,p,q)
term(707) = term(707) + wm_interm_191_so_pt3(a,i) * wm_interm_57_so_pt3(a,i,p,q)
term(708) = term(708) + wm_interm_191_so_pt3(a,i) * wm_interm_54_so_pt3(a,p,i,q)
term(709) = term(709) + wm_interm_191_so_pt3(a,i) * wm_interm_60_so_pt3(a,i,p,q)
term(710) = term(710) + wm_interm_191_so_pt3(a,i) * wm_interm_58_so_pt3(a,p,i,q)
term(711) = term(711) + wm_interm_191_so_pt3(a,i) * wm_interm_59_so_pt3(a,p,i,q)
term(712) = term(712) + wm_interm_191_so_pt3(a,i) * wm_interm_61_so_pt3(a,i,p,q)
term(713) = term(713) + wm_interm_220_so_pt3(a,i) * wm_interm_52_so_pt3(a,p,q,i)
term(714) = term(714) + wm_interm_221_so_pt3(a,i) * wm_interm_52_so_pt3(a,p,q,i)
term(715) = term(715) + wm_interm_222_so_pt3(a,i) * wm_interm_52_so_pt3(a,p,q,i)
term(716) = term(716) + wm_interm_227_so_pt3(a,i) * wm_interm_52_so_pt3(a,p,q,i)
term(717) = term(717) + wm_interm_228_so_pt3(a,i) * wm_interm_52_so_pt3(a,p,q,i)
end do 
end do 

term(486) = term(486) * (-2.0d+0) 
term(487) = term(487) * (4.0d+0) 
term(488) = term(488) * (-2.0d+0) 
term(489) = term(489) * (-2.0d+0) 
term(490) = term(490) * (-2.0d+0) 
term(491) = term(491) * (4.0d+0) 
term(492) = term(492) * (3.9999999999999996d+0) 
term(493) = term(493) * (-7.999999999999999d+0) 
term(494) = term(494) * (3.9999999999999996d+0) 
term(496) = term(496) * (-2.0d+0) 
term(500) = term(500) * (-2.0d+0) 
term(501) = term(501) * (-1.9999999999999998d+0) 
term(502) = term(502) * (3.9999999999999996d+0) 
term(503) = term(503) * (-1.9999999999999998d+0) 
term(504) = term(504) * (-4.0d+0) 
term(505) = term(505) * (8.0d+0) 
term(506) = term(506) * (-4.0d+0) 
term(507) = term(507) * (-4.0d+0) 
term(508) = term(508) * (-4.0d+0) 
term(509) = term(509) * (8.0d+0) 
term(510) = term(510) * (7.999999999999999d+0) 
term(511) = term(511) * (-15.999999999999998d+0) 
term(512) = term(512) * (7.999999999999999d+0) 
term(513) = term(513) * (2.0d+0) 
term(514) = term(514) * (-4.0d+0) 
term(515) = term(515) * (2.0d+0) 
term(516) = term(516) * (2.0d+0) 
term(517) = term(517) * (2.0d+0) 
term(518) = term(518) * (-4.0d+0) 
term(519) = term(519) * (-3.9999999999999996d+0) 
term(520) = term(520) * (7.999999999999999d+0) 
term(521) = term(521) * (-3.9999999999999996d+0) 
term(523) = term(523) * (-2.0d+0) 
term(525) = term(525) * (0.3333333333333333d+0) 
term(526) = term(526) * (-0.6666666666666666d+0) 
term(527) = term(527) * (0.3333333333333333d+0) 
term(528) = term(528) * (-2.0d+0) 
term(529) = term(529) * (4.0d+0) 
term(530) = term(530) * (-2.0d+0) 
term(532) = term(532) * (-2.0d+0) 
term(534) = term(534) * (0.6666666666666666d+0) 
term(535) = term(535) * (-1.3333333333333333d+0) 
term(536) = term(536) * (0.6666666666666666d+0) 
term(537) = term(537) * (4.0d+0) 
term(538) = term(538) * (-4.0d+0) 
term(539) = term(539) * (1.3333333333333333d+0) 
term(540) = term(540) * (-1.3333333333333333d+0) 
term(541) = term(541) * (-8.0d+0) 
term(542) = term(542) * (8.0d+0) 
term(543) = term(543) * (2.0d+0) 
term(544) = term(544) * (-4.0d+0) 
term(545) = term(545) * (2.0d+0) 
term(546) = term(546) * (2.6666666666666665d+0) 
term(547) = term(547) * (-2.6666666666666665d+0) 
term(548) = term(548) * (1.9999999999999998d+0) 
term(549) = term(549) * (-3.9999999999999996d+0) 
term(550) = term(550) * (-0.9999999999999999d+0) 
term(551) = term(551) * (1.9999999999999998d+0) 
term(552) = term(552) * (-0.9999999999999999d+0) 
term(553) = term(553) * (1.9999999999999998d+0) 
term(554) = term(554) * (1.9999999999999998d+0) 
term(555) = term(555) * (-3.9999999999999996d+0) 
term(557) = term(557) * (-2.0d+0) 
term(558) = term(558) * (-2.0d+0) 
term(559) = term(559) * (4.0d+0) 
term(560) = term(560) * (4.0d+0) 
term(561) = term(561) * (-8.0d+0) 
term(563) = term(563) * (-2.0d+0) 
term(564) = term(564) * (-2.0d+0) 
term(565) = term(565) * (4.0d+0) 
term(566) = term(566) * (-2.0d+0) 
term(567) = term(567) * (4.0d+0) 
term(568) = term(568) * (-4.0d+0) 
term(569) = term(569) * (2.0d+0) 
term(570) = term(570) * (-4.0d+0) 
term(571) = term(571) * (8.0d+0) 
term(572) = term(572) * (-12.0d+0) 
term(573) = term(573) * (16.0d+0) 
term(574) = term(574) * (-12.0d+0) 
term(575) = term(575) * (16.0d+0) 
term(576) = term(576) * (8.0d+0) 
term(577) = term(577) * (-16.0d+0) 
term(578) = term(578) * (6.0d+0) 
term(579) = term(579) * (-8.0d+0) 
term(580) = term(580) * (6.0d+0) 
term(581) = term(581) * (-8.0d+0) 
term(582) = term(582) * (-4.0d+0) 
term(583) = term(583) * (8.0d+0) 
term(584) = term(584) * (4.0d+0) 
term(585) = term(585) * (-2.0d+0) 
term(586) = term(586) * (4.0d+0) 
term(587) = term(587) * (-2.0d+0) 
term(588) = term(588) * (-3.0d+0) 
term(589) = term(589) * (-3.0d+0) 
term(592) = term(592) * (6.0d+0) 
term(593) = term(593) * (-8.0d+0) 
term(594) = term(594) * (-2.0d+0) 
term(595) = term(595) * (4.0d+0) 
term(596) = term(596) * (2.0d+0) 
term(597) = term(597) * (-4.0d+0) 
term(598) = term(598) * (1.9999999999999998d+0) 
term(601) = term(601) * (2.0d+0) 
term(602) = term(602) * (-4.0d+0) 
term(603) = term(603) * (2.0d+0) 
term(604) = term(604) * (-4.0d+0) 
term(605) = term(605) * (2.0d+0) 
term(606) = term(606) * (-1.0d+0) 
term(607) = term(607) * (2.0d+0) 
term(608) = term(608) * (-1.0d+0) 
term(609) = term(609) * (-1.0d+0) 
term(610) = term(610) * (-1.0d+0) 
term(611) = term(611) * (2.0d+0) 
term(612) = term(612) * (-1.0d+0) 
term(613) = term(613) * (2.0d+0) 
term(614) = term(614) * (-1.0d+0) 
term(615) = term(615) * (2.0d+0) 
term(616) = term(616) * (2.0d+0) 
term(617) = term(617) * (-4.0d+0) 
term(618) = term(618) * (-1.0d+0) 
term(619) = term(619) * (-1.0d+0) 
term(620) = term(620) * (2.0d+0) 
term(621) = term(621) * (0.5d+0) 
term(622) = term(622) * (-1.0d+0) 
term(623) = term(623) * (0.5d+0) 
term(624) = term(624) * (0.5d+0) 
term(625) = term(625) * (0.5d+0) 
term(626) = term(626) * (-1.0d+0) 
term(627) = term(627) * (-1.0d+0) 
term(628) = term(628) * (2.0d+0) 
term(629) = term(629) * (-1.0d+0) 
term(630) = term(630) * (0.5d+0) 
term(631) = term(631) * (0.5d+0) 
term(632) = term(632) * (-1.0d+0) 
term(633) = term(633) * (0.5d+0) 
term(634) = term(634) * (-1.0d+0) 
term(635) = term(635) * (0.5d+0) 
term(636) = term(636) * (-1.0d+0) 
term(637) = term(637) * (-1.0d+0) 
term(638) = term(638) * (2.0d+0) 
term(639) = term(639) * (8.0d+0) 
term(640) = term(640) * (-8.0d+0) 
term(641) = term(641) * (-4.0d+0) 
term(642) = term(642) * (4.0d+0) 
term(643) = term(643) * (-4.0d+0) 
term(644) = term(644) * (4.0d+0) 
term(645) = term(645) * (-4.0d+0) 
term(646) = term(646) * (8.0d+0) 
term(647) = term(647) * (4.0d+0) 
term(648) = term(648) * (-8.0d+0) 
term(649) = term(649) * (-4.0d+0) 
term(650) = term(650) * (4.0d+0) 
term(651) = term(651) * (2.0d+0) 
term(652) = term(652) * (-2.0d+0) 
term(653) = term(653) * (2.0d+0) 
term(654) = term(654) * (-2.0d+0) 
term(655) = term(655) * (-4.0d+0) 
term(656) = term(656) * (4.0d+0) 
term(657) = term(657) * (2.0d+0) 
term(658) = term(658) * (2.0d+0) 
term(659) = term(659) * (-2.0d+0) 
term(660) = term(660) * (-2.0d+0) 
term(661) = term(661) * (-4.0d+0) 
term(662) = term(662) * (4.0d+0) 
term(663) = term(663) * (-1.9999999999999998d+0) 
term(664) = term(664) * (3.9999999999999996d+0) 
term(665) = term(665) * (2.0d+0) 
term(666) = term(666) * (-4.0d+0) 
term(667) = term(667) * (-1.9999999999999998d+0) 
term(668) = term(668) * (2.0d+0) 
term(670) = term(670) * (-1.9999999999999998d+0) 
term(671) = term(671) * (-1.0d+0) 
term(672) = term(672) * (2.0d+0) 
term(674) = term(674) * (-1.0d+0) 
term(675) = term(675) * (-7.999999999999999d+0) 
term(676) = term(676) * (7.999999999999999d+0) 
term(677) = term(677) * (8.0d+0) 
term(678) = term(678) * (-8.0d+0) 
term(679) = term(679) * (3.9999999999999996d+0) 
term(680) = term(680) * (-3.9999999999999996d+0) 
term(681) = term(681) * (-4.0d+0) 
term(682) = term(682) * (4.0d+0) 
term(685) = term(685) * (-2.0d+0) 
term(686) = term(686) * (-2.0d+0) 
term(689) = term(689) * (-0.5d+0) 
term(690) = term(690) * (-0.5d+0) 
term(693) = term(693) * (-0.5d+0) 
term(694) = term(694) * (-0.5d+0) 
term(695) = term(695) * (-0.5d+0) 
term(696) = term(696) * (-0.5d+0) 
term(699) = term(699) * (-0.5d+0) 
term(700) = term(700) * (-0.5d+0) 
term(701) = term(701) * (2.0d+0) 
term(702) = term(702) * (2.0d+0) 
term(703) = term(703) * (-4.0d+0) 
term(704) = term(704) * (-4.0d+0) 
term(705) = term(705) * (2.0d+0) 
term(706) = term(706) * (2.0d+0) 
term(707) = term(707) * (-2.0d+0) 
term(708) = term(708) * (-2.0d+0) 
term(709) = term(709) * (4.0d+0) 
term(710) = term(710) * (4.0d+0) 
term(711) = term(711) * (-2.0d+0) 
term(712) = term(712) * (-2.0d+0) 
term(714) = term(714) * (-1.9999999999999998d+0) 
term(716) = term(716) * (3.9999999999999996d+0) 
term(717) = term(717) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(718) = term(718) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,p,k,q,i,j)
term(719) = term(719) + wm_interm_10_so_pt3(a,i,j,k) * wm_interm_172_so_pt3(a,k,p,q,i,j)
end do 
end do 
end do 
end do 

term(718) = term(718) * (-2.0d+0) 
term(719) = term(719) * (3.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(720) = term(720) + wm_interm_5_so_pt3(a,i,j,k) * wm_interm_88_so_pt3(a,q,i,j,k,p)
term(721) = term(721) + wm_interm_5_so_pt3(a,i,j,k) * wm_interm_88_so_pt3(a,q,i,j,p,k)
term(722) = term(722) + wm_interm_5_so_pt3(a,i,j,k) * wm_interm_88_so_pt3(a,i,q,p,j,k)
term(723) = term(723) + wm_interm_5_so_pt3(a,i,j,k) * wm_interm_88_so_pt3(a,q,i,p,j,k)
term(724) = term(724) + wm_interm_117_so_pt3(a,q,i,j,p,k) * wm_interm_5_so_pt3(a,i,k,j)
term(725) = term(725) + wm_interm_117_so_pt3(a,i,q,p,j,k) * wm_interm_5_so_pt3(a,i,k,j)
term(726) = term(726) + wm_interm_117_so_pt3(a,q,i,p,j,k) * wm_interm_5_so_pt3(a,i,k,j)
term(727) = term(727) + wm_interm_117_so_pt3(a,i,q,j,k,p) * wm_interm_5_so_pt3(a,i,k,j)
term(728) = term(728) + wm_interm_117_so_pt3(a,q,i,j,k,p) * wm_interm_5_so_pt3(a,i,k,j)
term(729) = term(729) + wm_interm_117_so_pt3(a,q,i,j,k,p) * wm_interm_5_so_pt3(a,i,j,k)
term(730) = term(730) + wm_interm_117_so_pt3(a,q,i,j,p,k) * wm_interm_5_so_pt3(a,i,j,k)
term(731) = term(731) + wm_interm_117_so_pt3(a,i,q,p,j,k) * wm_interm_5_so_pt3(a,i,j,k)
term(732) = term(732) + wm_interm_117_so_pt3(a,q,i,p,j,k) * wm_interm_5_so_pt3(a,i,j,k)
term(733) = term(733) + wm_interm_102_so_pt3(a,p,i,j,k,q) * wm_interm_162_so_pt3(a,k,i,j)
term(734) = term(734) + wm_interm_102_so_pt3(a,p,i,j,k,q) * wm_interm_162_so_pt3(a,k,j,i)
term(735) = term(735) + wm_interm_102_so_pt3(a,p,i,j,k,q) * wm_interm_161_so_pt3(a,k,j,i)
term(736) = term(736) + wm_interm_102_so_pt3(a,i,p,j,k,q) * wm_interm_162_so_pt3(a,k,j,i)
term(737) = term(737) + wm_interm_102_so_pt3(a,i,p,j,k,q) * wm_interm_161_so_pt3(a,k,i,j)
term(738) = term(738) + wm_interm_102_so_pt3(a,i,p,j,k,q) * wm_interm_161_so_pt3(a,k,j,i)
term(739) = term(739) + wm_interm_102_so_pt3(a,p,i,j,k,q) * wm_interm_163_so_pt3(a,k,j,i)
term(740) = term(740) + wm_interm_102_so_pt3(a,p,i,j,k,q) * wm_interm_163_so_pt3(a,k,i,j)
term(741) = term(741) + wm_interm_102_so_pt3(a,i,p,j,k,q) * wm_interm_163_so_pt3(a,k,i,j)
term(742) = term(742) + wm_interm_102_so_pt3(a,i,p,j,k,q) * wm_interm_163_so_pt3(a,k,j,i)
term(743) = term(743) + wm_interm_176_so_pt3(a,i,j,k) * wm_interm_177_so_pt3(a,j,i,q,p,k)
term(744) = term(744) + wm_interm_176_so_pt3(a,i,j,k) * wm_interm_177_so_pt3(a,j,i,q,k,p)
term(745) = term(745) + wm_interm_176_so_pt3(a,i,j,k) * wm_interm_177_so_pt3(a,q,i,j,k,p)
term(746) = term(746) + wm_interm_176_so_pt3(a,i,j,k) * wm_interm_177_so_pt3(a,i,j,q,p,k)
term(747) = term(747) + wm_interm_176_so_pt3(a,i,j,k) * wm_interm_177_so_pt3(a,q,i,j,p,k)
term(748) = term(748) + wm_interm_176_so_pt3(a,i,j,k) * wm_interm_177_so_pt3(a,i,j,q,k,p)
term(749) = term(749) + wm_interm_177_so_pt3(a,q,i,j,p,k) * wm_interm_179_so_pt3(a,i,j,k)
term(750) = term(750) + wm_interm_177_so_pt3(a,i,j,q,k,p) * wm_interm_179_so_pt3(a,i,j,k)
term(751) = term(751) + wm_interm_177_so_pt3(a,i,j,q,p,k) * wm_interm_179_so_pt3(a,i,j,k)
term(752) = term(752) + wm_interm_177_so_pt3(a,q,i,j,k,p) * wm_interm_179_so_pt3(a,i,j,k)
term(753) = term(753) + wm_interm_177_so_pt3(a,i,j,q,p,k) * wm_interm_179_so_pt3(a,j,i,k)
term(754) = term(754) + wm_interm_177_so_pt3(a,i,j,q,k,p) * wm_interm_179_so_pt3(a,j,i,k)
term(755) = term(755) + wm_interm_143_so_pt3(a,i,p,q,j,k) * wm_interm_41_so_pt3(a,i,k,j)
term(756) = term(756) + wm_interm_143_so_pt3(a,p,i,j,q,k) * wm_interm_41_so_pt3(a,i,k,j)
term(757) = term(757) + wm_interm_144_so_pt3(a,i,p,q,j,k) * wm_interm_181_so_pt3(a,j,k,i)
term(758) = term(758) + wm_interm_144_so_pt3(a,p,i,q,j,k) * wm_interm_180_so_pt3(a,j,k,i)
term(759) = term(759) + wm_interm_143_so_pt3(a,i,p,q,j,k) * wm_interm_41_so_pt3(a,i,j,k)
term(760) = term(760) + wm_interm_143_so_pt3(a,p,i,j,q,k) * wm_interm_41_so_pt3(a,i,j,k)
term(761) = term(761) + wm_interm_144_so_pt3(a,p,i,j,k,q) * wm_interm_181_so_pt3(a,k,j,i)
term(762) = term(762) + wm_interm_144_so_pt3(a,p,i,j,k,q) * wm_interm_180_so_pt3(a,k,j,i)
term(763) = term(763) + wm_interm_144_so_pt3(a,p,i,j,k,q) * wm_interm_181_so_pt3(a,j,k,i)
term(764) = term(764) + wm_interm_144_so_pt3(a,p,i,j,k,q) * wm_interm_180_so_pt3(a,j,k,i)
term(765) = term(765) + wm_interm_143_so_pt3(a,i,p,j,k,q) * wm_interm_41_so_pt3(a,i,k,j)
term(766) = term(766) + wm_interm_143_so_pt3(a,p,i,j,k,q) * wm_interm_41_so_pt3(a,i,j,k)
term(767) = term(767) + wm_interm_202_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(768) = term(768) + wm_interm_202_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(769) = term(769) + wm_interm_203_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(770) = term(770) + wm_interm_202_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(771) = term(771) + wm_interm_202_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(772) = term(772) + wm_interm_203_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(773) = term(773) + wm_interm_204_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(774) = term(774) + wm_interm_204_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(775) = term(775) + wm_interm_205_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(776) = term(776) + wm_interm_206_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(777) = term(777) + wm_interm_206_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(778) = term(778) + wm_interm_207_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(779) = term(779) + wm_interm_204_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(780) = term(780) + wm_interm_204_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(781) = term(781) + wm_interm_205_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(782) = term(782) + wm_interm_205_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(783) = term(783) + wm_interm_207_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(784) = term(784) + wm_interm_206_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(785) = term(785) + wm_interm_205_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(786) = term(786) + wm_interm_206_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(787) = term(787) + wm_interm_206_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(788) = term(788) + wm_interm_205_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(789) = term(789) + wm_interm_205_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(790) = term(790) + wm_interm_206_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(791) = term(791) + wm_interm_202_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(792) = term(792) + wm_interm_202_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(793) = term(793) + wm_interm_203_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(794) = term(794) + wm_interm_203_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(795) = term(795) + wm_interm_203_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(796) = term(796) + wm_interm_203_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(797) = term(797) + wm_interm_204_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(798) = term(798) + wm_interm_205_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(799) = term(799) + wm_interm_205_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(800) = term(800) + wm_interm_206_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(801) = term(801) + wm_interm_207_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(802) = term(802) + wm_interm_207_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(803) = term(803) + wm_interm_204_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(804) = term(804) + wm_interm_204_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(805) = term(805) + wm_interm_207_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(806) = term(806) + wm_interm_204_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(807) = term(807) + wm_interm_207_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(808) = term(808) + wm_interm_207_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(809) = term(809) + wm_interm_202_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(810) = term(810) + wm_interm_202_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(811) = term(811) + wm_interm_203_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(812) = term(812) + wm_interm_216_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(813) = term(813) + wm_interm_216_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(814) = term(814) + wm_interm_216_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(815) = term(815) + wm_interm_216_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(816) = term(816) + wm_interm_217_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(817) = term(817) + wm_interm_217_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(818) = term(818) + wm_interm_218_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(819) = term(819) + wm_interm_218_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(820) = term(820) + wm_interm_217_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(821) = term(821) + wm_interm_217_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(822) = term(822) + wm_interm_218_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(823) = term(823) + wm_interm_218_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(824) = term(824) + wm_interm_218_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(825) = term(825) + wm_interm_218_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(826) = term(826) + wm_interm_218_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(827) = term(827) + wm_interm_218_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(828) = term(828) + wm_interm_216_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(829) = term(829) + wm_interm_216_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(830) = term(830) + wm_interm_216_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(831) = term(831) + wm_interm_216_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(832) = term(832) + wm_interm_217_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(833) = term(833) + wm_interm_218_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(834) = term(834) + wm_interm_218_so_pt3(a,i,j,p,q,k) * wm_interm_52_so_pt3(a,k,i,j)
term(835) = term(835) + wm_interm_217_so_pt3(a,i,j,p,k,q) * wm_interm_52_so_pt3(a,k,i,j)
term(836) = term(836) + wm_interm_217_so_pt3(a,p,i,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(837) = term(837) + wm_interm_217_so_pt3(a,p,i,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(838) = term(838) + wm_interm_217_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(839) = term(839) + wm_interm_217_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(840) = term(840) + wm_interm_216_so_pt3(a,i,p,j,k,q) * wm_interm_52_so_pt3(a,k,j,i)
term(841) = term(841) + wm_interm_216_so_pt3(a,i,p,j,q,k) * wm_interm_52_so_pt3(a,k,j,i)
term(842) = term(842) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,i,p,j,q,k)
term(843) = term(843) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,i,p,j,q,k)
term(844) = term(844) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,i,j,p,q,k)
term(845) = term(845) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,j,i,p,q,k)
term(846) = term(846) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,i,j,p,q,k)
term(847) = term(847) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,p,i,j,k,q)
term(848) = term(848) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,p,i,j,k,q)
term(849) = term(849) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,p,i,j,q,k)
term(850) = term(850) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,j,i,p,k,q)
term(851) = term(851) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,i,j,p,q,k)
term(852) = term(852) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,i,p,j,k,q)
term(853) = term(853) + wm_interm_209_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,j,i,p,q,k)
term(854) = term(854) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,i,p,j,q,k)
term(855) = term(855) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,i,p,j,q,k)
term(856) = term(856) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,i,j,p,q,k)
term(857) = term(857) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,j,i,p,q,k)
term(858) = term(858) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,i,j,p,q,k)
term(859) = term(859) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,p,i,j,q,k)
term(860) = term(860) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,p,i,j,k,q)
term(861) = term(861) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,p,i,j,k,q)
term(862) = term(862) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,j,i,p,q,k)
term(863) = term(863) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,i,j,p,k,q)
term(864) = term(864) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,i,p,j,k,q)
term(865) = term(865) + wm_interm_208_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,j,i,p,k,q)
term(866) = term(866) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,i,p,j,q,k)
term(867) = term(867) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,j,i,p,q,k)
term(868) = term(868) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,i,j,p,q,k)
term(869) = term(869) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,j,i,p,q,k)
term(870) = term(870) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,p,i,j,k,q)
term(871) = term(871) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,p,i,j,k,q)
term(872) = term(872) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,p,i,j,k,q)
term(873) = term(873) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,i,j,p,k,q)
term(874) = term(874) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,i,p,j,k,q)
term(875) = term(875) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,j,i,p,q,k)
term(876) = term(876) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,i,j,p,q,k)
term(877) = term(877) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,i,p,j,k,q)
term(878) = term(878) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_224_so_pt3(a,i,p,j,q,k)
term(879) = term(879) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,i,j,p,q,k)
term(880) = term(880) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_225_so_pt3(a,p,i,j,q,k)
term(881) = term(881) + wm_interm_219_so_pt3(a,i,j,k) * wm_interm_223_so_pt3(a,j,i,p,k,q)
end do 
end do 
end do 
end do 

term(721) = term(721) * (-2.0d+0) 
term(722) = term(722) * (-2.0d+0) 
term(723) = term(723) * (4.0d+0) 
term(724) = term(724) * (2.0d+0) 
term(725) = term(725) * (2.0d+0) 
term(726) = term(726) * (-4.0d+0) 
term(727) = term(727) * (1.9999999999999998d+0) 
term(728) = term(728) * (-3.9999999999999996d+0) 
term(729) = term(729) * (1.9999999999999998d+0) 
term(730) = term(730) * (-4.0d+0) 
term(731) = term(731) * (-4.0d+0) 
term(732) = term(732) * (8.0d+0) 
term(734) = term(734) * (-1.9999999999999998d+0) 
term(738) = term(738) * (-2.0d+0) 
term(739) = term(739) * (4.0d+0) 
term(740) = term(740) * (-3.9999999999999996d+0) 
term(741) = term(741) * (4.0d+0) 
term(742) = term(742) * (-4.0d+0) 
term(743) = term(743) * (3.0d+0) 
term(744) = term(744) * (-2.0d+0) 
term(745) = term(745) * (3.0d+0) 
term(746) = term(746) * (-4.0d+0) 
term(747) = term(747) * (-4.0d+0) 
term(748) = term(748) * (4.0d+0) 
term(749) = term(749) * (3.0d+0) 
term(750) = term(750) * (-2.0d+0) 
term(751) = term(751) * (3.0d+0) 
term(752) = term(752) * (-4.0d+0) 
term(753) = term(753) * (-4.0d+0) 
term(754) = term(754) * (4.0d+0) 
term(759) = term(759) * (-2.0d+0) 
term(760) = term(760) * (-2.0d+0) 
term(762) = term(762) * (-1.9999999999999998d+0) 
term(763) = term(763) * (-1.9999999999999998d+0) 
term(767) = term(767) * (0.16666666666666666d+0) 
term(768) = term(768) * (-0.3333333333333333d+0) 
term(769) = term(769) * (0.16666666666666666d+0) 
term(770) = term(770) * (-0.3333333333333333d+0) 
term(771) = term(771) * (0.6666666666666666d+0) 
term(772) = term(772) * (-0.3333333333333333d+0) 
term(773) = term(773) * (0.16666666666666666d+0) 
term(774) = term(774) * (-0.3333333333333333d+0) 
term(775) = term(775) * (0.16666666666666666d+0) 
term(776) = term(776) * (0.16666666666666666d+0) 
term(777) = term(777) * (-0.3333333333333333d+0) 
term(778) = term(778) * (0.16666666666666666d+0) 
term(779) = term(779) * (-0.3333333333333333d+0) 
term(780) = term(780) * (0.6666666666666666d+0) 
term(781) = term(781) * (0.16666666666666666d+0) 
term(782) = term(782) * (-0.3333333333333333d+0) 
term(783) = term(783) * (-0.3333333333333333d+0) 
term(784) = term(784) * (0.16666666666666666d+0) 
term(785) = term(785) * (-0.3333333333333333d+0) 
term(786) = term(786) * (-0.3333333333333333d+0) 
term(787) = term(787) * (0.6666666666666666d+0) 
term(788) = term(788) * (-0.3333333333333333d+0) 
term(789) = term(789) * (0.6666666666666666d+0) 
term(790) = term(790) * (-0.3333333333333333d+0) 
term(791) = term(791) * (0.16666666666666666d+0) 
term(792) = term(792) * (-0.3333333333333333d+0) 
term(793) = term(793) * (0.16666666666666666d+0) 
term(794) = term(794) * (-0.3333333333333333d+0) 
term(795) = term(795) * (-0.3333333333333333d+0) 
term(796) = term(796) * (0.6666666666666666d+0) 
term(797) = term(797) * (0.16666666666666666d+0) 
term(798) = term(798) * (0.16666666666666666d+0) 
term(799) = term(799) * (-0.3333333333333333d+0) 
term(800) = term(800) * (0.16666666666666666d+0) 
term(801) = term(801) * (0.16666666666666666d+0) 
term(802) = term(802) * (-0.3333333333333333d+0) 
term(803) = term(803) * (0.16666666666666666d+0) 
term(804) = term(804) * (-0.3333333333333333d+0) 
term(805) = term(805) * (0.16666666666666666d+0) 
term(806) = term(806) * (0.16666666666666666d+0) 
term(807) = term(807) * (0.16666666666666666d+0) 
term(808) = term(808) * (-0.3333333333333333d+0) 
term(809) = term(809) * (0.16666666666666666d+0) 
term(810) = term(810) * (-0.3333333333333333d+0) 
term(811) = term(811) * (0.16666666666666666d+0) 
term(812) = term(812) * (0.6666666666666666d+0) 
term(813) = term(813) * (-0.6666666666666666d+0) 
term(814) = term(814) * (-1.3333333333333333d+0) 
term(815) = term(815) * (1.3333333333333333d+0) 
term(816) = term(816) * (0.6666666666666666d+0) 
term(817) = term(817) * (-0.6666666666666666d+0) 
term(818) = term(818) * (0.6666666666666666d+0) 
term(819) = term(819) * (-0.6666666666666666d+0) 
term(820) = term(820) * (-1.3333333333333333d+0) 
term(821) = term(821) * (1.3333333333333333d+0) 
term(822) = term(822) * (0.6666666666666666d+0) 
term(823) = term(823) * (-0.6666666666666666d+0) 
term(824) = term(824) * (-1.3333333333333333d+0) 
term(825) = term(825) * (1.3333333333333333d+0) 
term(826) = term(826) * (-1.3333333333333333d+0) 
term(827) = term(827) * (1.3333333333333333d+0) 
term(828) = term(828) * (0.6666666666666666d+0) 
term(829) = term(829) * (-1.3333333333333333d+0) 
term(830) = term(830) * (-0.6666666666666666d+0) 
term(831) = term(831) * (1.3333333333333333d+0) 
term(832) = term(832) * (0.6666666666666666d+0) 
term(833) = term(833) * (0.6666666666666666d+0) 
term(834) = term(834) * (-0.6666666666666666d+0) 
term(835) = term(835) * (-0.6666666666666666d+0) 
term(836) = term(836) * (0.6666666666666666d+0) 
term(837) = term(837) * (-0.6666666666666666d+0) 
term(838) = term(838) * (0.6666666666666666d+0) 
term(839) = term(839) * (-0.6666666666666666d+0) 
term(840) = term(840) * (0.6666666666666666d+0) 
term(841) = term(841) * (-0.6666666666666666d+0) 
term(842) = term(842) * (-0.3333333333333333d+0) 
term(843) = term(843) * (0.16666666666666666d+0) 
term(844) = term(844) * (0.16666666666666666d+0) 
term(845) = term(845) * (0.16666666666666666d+0) 
term(846) = term(846) * (-0.3333333333333333d+0) 
term(847) = term(847) * (-0.3333333333333333d+0) 
term(848) = term(848) * (0.16666666666666666d+0) 
term(849) = term(849) * (0.16666666666666666d+0) 
term(850) = term(850) * (0.16666666666666666d+0) 
term(851) = term(851) * (0.16666666666666666d+0) 
term(852) = term(852) * (0.16666666666666666d+0) 
term(853) = term(853) * (-0.3333333333333333d+0) 
term(854) = term(854) * (-0.3333333333333333d+0) 
term(855) = term(855) * (0.16666666666666666d+0) 
term(856) = term(856) * (0.16666666666666666d+0) 
term(857) = term(857) * (0.16666666666666666d+0) 
term(858) = term(858) * (-0.3333333333333333d+0) 
term(859) = term(859) * (-0.3333333333333333d+0) 
term(860) = term(860) * (0.16666666666666666d+0) 
term(861) = term(861) * (0.16666666666666666d+0) 
term(862) = term(862) * (0.16666666666666666d+0) 
term(863) = term(863) * (0.16666666666666666d+0) 
term(864) = term(864) * (0.16666666666666666d+0) 
term(865) = term(865) * (-0.3333333333333333d+0) 
term(866) = term(866) * (0.6666666666666666d+0) 
term(867) = term(867) * (0.6666666666666666d+0) 
term(868) = term(868) * (0.6666666666666666d+0) 
term(869) = term(869) * (-0.6666666666666666d+0) 
term(870) = term(870) * (0.6666666666666666d+0) 
term(871) = term(871) * (-0.6666666666666666d+0) 
term(872) = term(872) * (0.6666666666666666d+0) 
term(873) = term(873) * (0.6666666666666666d+0) 
term(874) = term(874) * (0.6666666666666666d+0) 
term(875) = term(875) * (0.6666666666666666d+0) 
term(876) = term(876) * (-0.6666666666666666d+0) 
term(877) = term(877) * (-0.6666666666666666d+0) 
term(878) = term(878) * (-0.6666666666666666d+0) 
term(879) = term(879) * (-0.6666666666666666d+0) 
term(880) = term(880) * (-0.6666666666666666d+0) 
term(881) = term(881) * (-0.6666666666666666d+0) 


    calc_D_oo_wm_so_cc3_pt3 = zero
    do s = 0, 881
    calc_D_oo_wm_so_cc3_pt3 = calc_D_oo_wm_so_cc3_pt3 + term(s)
    end do

    end function calc_D_oo_wm_so_cc3_pt3
    
    function calc_D_ov_wm_so_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, l, b, c 
    real(F64), dimension(0:647) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,p,j,i)
term(1) = term(1) + wm_interm_0_so_pt3(q,a,i,j) * wm_interm_2_so_pt3(a,j,p,i)
term(2) = term(2) + wm_interm_0_so_pt3(q,a,i,j) * wm_interm_3_so_pt3(a,p,j,i)
term(3) = term(3) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_6_so_pt3(q,a,j,i)
term(4) = term(4) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_7_so_pt3(q,a,i,j)
term(5) = term(5) + wm_interm_5_so_pt3(a,p,i,j) * wm_interm_6_so_pt3(q,a,i,j)
term(6) = term(6) + wm_interm_16_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(q,a,i,j)
term(7) = term(7) + wm_interm_17_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(q,a,i,j)
term(8) = term(8) + wm_interm_12_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(q,a,i,j)
term(9) = term(9) + wm_interm_14_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(q,a,i,j)
term(10) = term(10) + wm_interm_13_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(q,a,i,j)
term(11) = term(11) + wm_interm_15_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(q,a,i,j)
term(12) = term(12) + wm_interm_12_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(a,q,i,j)
term(13) = term(13) + wm_interm_15_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(a,q,i,j)
term(14) = term(14) + wm_interm_14_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(a,q,i,j)
term(15) = term(15) + wm_interm_13_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(a,q,i,j)
term(16) = term(16) + wm_interm_50_so_pt3(a,q,i,j) * wm_interm_52_so_pt3(a,p,i,j)
term(17) = term(17) + wm_interm_51_so_pt3(a,q,i,j) * wm_interm_52_so_pt3(a,p,i,j)
term(18) = term(18) + wm_interm_51_so_pt3(q,a,i,j) * wm_interm_52_so_pt3(a,p,i,j)
term(19) = term(19) + wm_interm_47_so_pt3(q,a,i,j) * wm_interm_52_so_pt3(a,p,i,j)
term(20) = term(20) + wm_interm_50_so_pt3(q,a,i,j) * wm_interm_52_so_pt3(a,p,i,j)
term(21) = term(21) + wm_interm_54_so_pt3(a,p,i,j) * wm_interm_56_so_pt3(a,q,i,j)
term(22) = term(22) + wm_interm_56_so_pt3(a,q,i,j) * wm_interm_57_so_pt3(a,i,p,j)
term(23) = term(23) + wm_interm_56_so_pt3(a,q,i,j) * wm_interm_58_so_pt3(a,p,i,j)
term(24) = term(24) + wm_interm_56_so_pt3(a,q,i,j) * wm_interm_59_so_pt3(a,p,i,j)
term(25) = term(25) + wm_interm_47_so_pt3(q,a,i,j) * wm_interm_52_so_pt3(a,p,j,i)
term(26) = term(26) + wm_interm_56_so_pt3(a,q,i,j) * wm_interm_60_so_pt3(a,i,p,j)
term(27) = term(27) + wm_interm_47_so_pt3(a,q,i,j) * wm_interm_52_so_pt3(a,p,j,i)
term(28) = term(28) + wm_interm_49_so_pt3(a,q,i,j) * wm_interm_52_so_pt3(a,p,i,j)
term(29) = term(29) + wm_interm_50_so_pt3(q,a,i,j) * wm_interm_52_so_pt3(a,p,j,i)
term(30) = term(30) + wm_interm_49_so_pt3(q,a,i,j) * wm_interm_52_so_pt3(a,p,j,i)
term(31) = term(31) + wm_interm_56_so_pt3(a,q,i,j) * wm_interm_61_so_pt3(a,i,p,j)
term(32) = term(32) + wm_interm_49_so_pt3(a,q,i,j) * wm_interm_52_so_pt3(a,p,j,i)
term(33) = term(33) + wm_interm_51_so_pt3(a,q,i,j) * wm_interm_52_so_pt3(a,p,j,i)
term(34) = term(34) + wm_interm_1_so_pt3(a,i,p,j) * wm_interm_73_so_pt3(q,a,j,i)
term(35) = term(35) + wm_interm_1_so_pt3(a,i,p,j) * wm_interm_74_so_pt3(q,a,j,i)
term(36) = term(36) + wm_interm_2_so_pt3(a,i,p,j) * wm_interm_74_so_pt3(q,a,j,i)
term(37) = term(37) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_74_so_pt3(q,a,j,i)
term(38) = term(38) + wm_interm_2_so_pt3(a,p,i,j) * wm_interm_73_so_pt3(q,a,j,i)
term(39) = term(39) + wm_interm_2_so_pt3(a,p,i,j) * wm_interm_74_so_pt3(q,a,j,i)
term(40) = term(40) + wm_interm_3_so_pt3(a,i,p,j) * wm_interm_73_so_pt3(q,a,j,i)
term(41) = term(41) + wm_interm_3_so_pt3(a,i,p,j) * wm_interm_74_so_pt3(q,a,j,i)
term(42) = term(42) + wm_interm_3_so_pt3(a,p,i,j) * wm_interm_74_so_pt3(q,a,j,i)
term(43) = term(43) + wm_interm_2_so_pt3(a,i,p,j) * wm_interm_75_so_pt3(q,a,j,i)
term(44) = term(44) + wm_interm_2_so_pt3(a,i,p,j) * wm_interm_76_so_pt3(q,a,j,i)
term(45) = term(45) + wm_interm_1_so_pt3(a,i,p,j) * wm_interm_76_so_pt3(q,a,j,i)
term(46) = term(46) + wm_interm_2_so_pt3(a,p,i,j) * wm_interm_76_so_pt3(q,a,j,i)
term(47) = term(47) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_75_so_pt3(q,a,j,i)
term(48) = term(48) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_76_so_pt3(q,a,j,i)
term(49) = term(49) + wm_interm_3_so_pt3(a,p,i,j) * wm_interm_75_so_pt3(q,a,j,i)
term(50) = term(50) + wm_interm_3_so_pt3(a,p,i,j) * wm_interm_76_so_pt3(q,a,j,i)
term(51) = term(51) + wm_interm_3_so_pt3(a,i,p,j) * wm_interm_76_so_pt3(q,a,j,i)
term(52) = term(52) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_79_so_pt3(q,a,j,i)
term(53) = term(53) + wm_interm_2_so_pt3(a,i,p,j) * wm_interm_79_so_pt3(q,a,j,i)
term(54) = term(54) + wm_interm_3_so_pt3(a,p,i,j) * wm_interm_79_so_pt3(q,a,j,i)
term(55) = term(55) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_81_so_pt3(q,a,j,i)
term(56) = term(56) + wm_interm_2_so_pt3(a,i,p,j) * wm_interm_81_so_pt3(q,a,j,i)
term(57) = term(57) + wm_interm_3_so_pt3(a,p,i,j) * wm_interm_81_so_pt3(q,a,j,i)
term(58) = term(58) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_83_so_pt3(q,a,j,i)
term(59) = term(59) + wm_interm_2_so_pt3(a,i,p,j) * wm_interm_83_so_pt3(q,a,j,i)
term(60) = term(60) + wm_interm_3_so_pt3(a,p,i,j) * wm_interm_83_so_pt3(q,a,j,i)
term(61) = term(61) + wm_interm_85_so_pt3(a,i,j,p) * wm_interm_89_so_pt3(q,a,i,j)
term(62) = term(62) + wm_interm_86_so_pt3(a,i,j,p) * wm_interm_89_so_pt3(q,a,i,j)
term(63) = term(63) + wm_interm_87_so_pt3(a,i,j,p) * wm_interm_89_so_pt3(q,a,i,j)
term(64) = term(64) + wm_interm_89_so_pt3(q,a,i,j) * wm_interm_90_so_pt3(a,i,j,p)
term(65) = term(65) + wm_interm_89_so_pt3(q,a,i,j) * wm_interm_91_so_pt3(a,i,p,j)
term(66) = term(66) + wm_interm_89_so_pt3(q,a,i,j) * wm_interm_90_so_pt3(a,i,p,j)
term(67) = term(67) + wm_interm_85_so_pt3(a,i,p,j) * wm_interm_89_so_pt3(q,a,i,j)
term(68) = term(68) + wm_interm_86_so_pt3(a,i,p,j) * wm_interm_89_so_pt3(q,a,i,j)
term(69) = term(69) + wm_interm_87_so_pt3(a,i,p,j) * wm_interm_89_so_pt3(q,a,i,j)
term(70) = term(70) + wm_interm_85_so_pt3(a,i,j,p) * wm_interm_95_so_pt3(q,a,i,j)
term(71) = term(71) + wm_interm_86_so_pt3(a,i,j,p) * wm_interm_95_so_pt3(q,a,i,j)
term(72) = term(72) + wm_interm_87_so_pt3(a,i,j,p) * wm_interm_95_so_pt3(q,a,i,j)
term(73) = term(73) + wm_interm_90_so_pt3(a,i,j,p) * wm_interm_95_so_pt3(q,a,i,j)
term(74) = term(74) + wm_interm_91_so_pt3(a,i,p,j) * wm_interm_95_so_pt3(q,a,i,j)
term(75) = term(75) + wm_interm_90_so_pt3(a,i,p,j) * wm_interm_95_so_pt3(q,a,i,j)
term(76) = term(76) + wm_interm_85_so_pt3(a,i,p,j) * wm_interm_95_so_pt3(q,a,i,j)
term(77) = term(77) + wm_interm_86_so_pt3(a,i,p,j) * wm_interm_95_so_pt3(q,a,i,j)
term(78) = term(78) + wm_interm_87_so_pt3(a,i,p,j) * wm_interm_95_so_pt3(q,a,i,j)
term(79) = term(79) + wm_interm_85_so_pt3(a,i,p,j) * wm_interm_96_so_pt3(q,a,i,j)
term(80) = term(80) + wm_interm_86_so_pt3(a,i,p,j) * wm_interm_96_so_pt3(q,a,i,j)
term(81) = term(81) + wm_interm_87_so_pt3(a,i,p,j) * wm_interm_96_so_pt3(q,a,i,j)
term(82) = term(82) + wm_interm_91_so_pt3(a,i,j,p) * wm_interm_96_so_pt3(q,a,i,j)
term(83) = term(83) + wm_interm_90_so_pt3(a,i,p,j) * wm_interm_96_so_pt3(q,a,i,j)
term(84) = term(84) + wm_interm_91_so_pt3(a,i,p,j) * wm_interm_96_so_pt3(q,a,i,j)
term(85) = term(85) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_85_so_pt3(a,i,j,p)
term(86) = term(86) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_86_so_pt3(a,i,j,p)
term(87) = term(87) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_87_so_pt3(a,i,j,p)
term(88) = term(88) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_90_so_pt3(a,i,j,p)
term(89) = term(89) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_91_so_pt3(a,i,p,j)
term(90) = term(90) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_90_so_pt3(a,i,p,j)
term(91) = term(91) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_85_so_pt3(a,i,p,j)
term(92) = term(92) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_86_so_pt3(a,i,p,j)
term(93) = term(93) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_87_so_pt3(a,i,p,j)
term(94) = term(94) + wm_interm_107_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,j,p,i)
term(95) = term(95) + wm_interm_108_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,j,p,i)
term(96) = term(96) + wm_interm_108_so_pt3(q,a,i,j) * wm_interm_2_so_pt3(a,j,p,i)
term(97) = term(97) + wm_interm_108_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,p,j,i)
term(98) = term(98) + wm_interm_107_so_pt3(q,a,i,j) * wm_interm_2_so_pt3(a,p,j,i)
term(99) = term(99) + wm_interm_108_so_pt3(q,a,i,j) * wm_interm_2_so_pt3(a,p,j,i)
term(100) = term(100) + wm_interm_107_so_pt3(q,a,i,j) * wm_interm_3_so_pt3(a,j,p,i)
term(101) = term(101) + wm_interm_108_so_pt3(q,a,i,j) * wm_interm_3_so_pt3(a,j,p,i)
term(102) = term(102) + wm_interm_108_so_pt3(q,a,i,j) * wm_interm_3_so_pt3(a,p,j,i)
term(103) = term(103) + wm_interm_107_so_pt3(q,a,i,j) * wm_interm_2_so_pt3(a,j,p,i)
term(104) = term(104) + wm_interm_107_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,p,j,i)
term(105) = term(105) + wm_interm_107_so_pt3(q,a,i,j) * wm_interm_3_so_pt3(a,p,j,i)
term(106) = term(106) + wm_interm_110_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,p,j,i)
term(107) = term(107) + wm_interm_110_so_pt3(q,a,i,j) * wm_interm_2_so_pt3(a,j,p,i)
term(108) = term(108) + wm_interm_110_so_pt3(q,a,i,j) * wm_interm_3_so_pt3(a,p,j,i)
term(109) = term(109) + wm_interm_112_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,p,j,i)
term(110) = term(110) + wm_interm_112_so_pt3(q,a,i,j) * wm_interm_2_so_pt3(a,j,p,i)
term(111) = term(111) + wm_interm_112_so_pt3(q,a,i,j) * wm_interm_3_so_pt3(a,p,j,i)
term(112) = term(112) + wm_interm_114_so_pt3(a,i,j,p) * wm_interm_89_so_pt3(q,a,i,j)
term(113) = term(113) + wm_interm_115_so_pt3(a,i,j,p) * wm_interm_89_so_pt3(q,a,i,j)
term(114) = term(114) + wm_interm_116_so_pt3(a,i,j,p) * wm_interm_89_so_pt3(q,a,i,j)
term(115) = term(115) + wm_interm_118_so_pt3(a,i,j,p) * wm_interm_89_so_pt3(q,a,i,j)
term(116) = term(116) + wm_interm_119_so_pt3(a,i,p,j) * wm_interm_89_so_pt3(q,a,i,j)
term(117) = term(117) + wm_interm_118_so_pt3(a,i,p,j) * wm_interm_89_so_pt3(q,a,i,j)
term(118) = term(118) + wm_interm_114_so_pt3(a,i,p,j) * wm_interm_89_so_pt3(q,a,i,j)
term(119) = term(119) + wm_interm_115_so_pt3(a,i,p,j) * wm_interm_89_so_pt3(q,a,i,j)
term(120) = term(120) + wm_interm_116_so_pt3(a,i,p,j) * wm_interm_89_so_pt3(q,a,i,j)
term(121) = term(121) + wm_interm_114_so_pt3(a,i,j,p) * wm_interm_95_so_pt3(q,a,i,j)
term(122) = term(122) + wm_interm_115_so_pt3(a,i,j,p) * wm_interm_95_so_pt3(q,a,i,j)
term(123) = term(123) + wm_interm_116_so_pt3(a,i,j,p) * wm_interm_95_so_pt3(q,a,i,j)
term(124) = term(124) + wm_interm_118_so_pt3(a,i,j,p) * wm_interm_95_so_pt3(q,a,i,j)
term(125) = term(125) + wm_interm_119_so_pt3(a,i,p,j) * wm_interm_95_so_pt3(q,a,i,j)
term(126) = term(126) + wm_interm_118_so_pt3(a,i,p,j) * wm_interm_95_so_pt3(q,a,i,j)
term(127) = term(127) + wm_interm_114_so_pt3(a,i,p,j) * wm_interm_95_so_pt3(q,a,i,j)
term(128) = term(128) + wm_interm_115_so_pt3(a,i,p,j) * wm_interm_95_so_pt3(q,a,i,j)
term(129) = term(129) + wm_interm_116_so_pt3(a,i,p,j) * wm_interm_95_so_pt3(q,a,i,j)
term(130) = term(130) + wm_interm_114_so_pt3(a,i,p,j) * wm_interm_96_so_pt3(q,a,i,j)
term(131) = term(131) + wm_interm_115_so_pt3(a,i,p,j) * wm_interm_96_so_pt3(q,a,i,j)
term(132) = term(132) + wm_interm_116_so_pt3(a,i,p,j) * wm_interm_96_so_pt3(q,a,i,j)
term(133) = term(133) + wm_interm_119_so_pt3(a,i,j,p) * wm_interm_96_so_pt3(q,a,i,j)
term(134) = term(134) + wm_interm_118_so_pt3(a,i,p,j) * wm_interm_96_so_pt3(q,a,i,j)
term(135) = term(135) + wm_interm_119_so_pt3(a,i,p,j) * wm_interm_96_so_pt3(q,a,i,j)
term(136) = term(136) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_114_so_pt3(a,i,j,p)
term(137) = term(137) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_115_so_pt3(a,i,j,p)
term(138) = term(138) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_116_so_pt3(a,i,j,p)
term(139) = term(139) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_118_so_pt3(a,i,j,p)
term(140) = term(140) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_119_so_pt3(a,i,p,j)
term(141) = term(141) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_118_so_pt3(a,i,p,j)
term(142) = term(142) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_114_so_pt3(a,i,p,j)
term(143) = term(143) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_115_so_pt3(a,i,p,j)
term(144) = term(144) + wm_interm_103_so_pt3(q,a,i,j) * wm_interm_116_so_pt3(a,i,p,j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-1.9999999999999998d+0) 
term(2) = term(2) * (4.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-8.0d+0) 
term(7) = term(7) * (8.0d+0) 
term(8) = term(8) * (6.0d+0) 
term(9) = term(9) * (6.0d+0) 
term(10) = term(10) * (-8.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (-16.0d+0) 
term(13) = term(13) * (12.0d+0) 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * (16.0d+0) 
term(16) = term(16) * (-0.3333333333333333d+0) 
term(17) = term(17) * (1.3333333333333333d+0) 
term(18) = term(18) * (-0.6666666666666666d+0) 
term(19) = term(19) * (-0.6666666666666666d+0) 
term(20) = term(20) * (0.6666666666666666d+0) 
term(21) = term(21) * (-1.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (1.3333333333333333d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (-0.6666666666666666d+0) 
term(28) = term(28) * (-0.3333333333333333d+0) 
term(29) = term(29) * (-0.3333333333333333d+0) 
term(30) = term(30) * (-0.3333333333333333d+0) 
term(31) = term(31) * (-1.0d+0) 
term(32) = term(32) * (0.6666666666666666d+0) 
term(33) = term(33) * (-0.6666666666666666d+0) 
term(35) = term(35) * (-2.0d+0) 
term(39) = term(39) * (-1.9999999999999998d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(44) = term(44) * (-1.9999999999999998d+0) 
term(48) = term(48) * (-2.0d+0) 
term(49) = term(49) * (-2.0d+0) 
term(50) = term(50) * (4.0d+0) 
term(51) = term(51) * (-2.0d+0) 
term(54) = term(54) * (-2.0d+0) 
term(57) = term(57) * (-2.0d+0) 
term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * (-1.9999999999999998d+0) 
term(60) = term(60) * (4.0d+0) 
term(61) = term(61) * (-2.0d+0) 
term(62) = term(62) * (4.0d+0) 
term(63) = term(63) * (-2.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (3.9999999999999996d+0) 
term(68) = term(68) * (-7.999999999999999d+0) 
term(69) = term(69) * (3.9999999999999996d+0) 
term(71) = term(71) * (-2.0d+0) 
term(75) = term(75) * (-2.0d+0) 
term(76) = term(76) * (-1.9999999999999998d+0) 
term(77) = term(77) * (3.9999999999999996d+0) 
term(78) = term(78) * (-1.9999999999999998d+0) 
term(80) = term(80) * (-1.9999999999999998d+0) 
term(84) = term(84) * (-2.0d+0) 
term(86) = term(86) * (-2.0d+0) 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * (-1.9999999999999998d+0) 
term(92) = term(92) * (3.9999999999999996d+0) 
term(93) = term(93) * (-1.9999999999999998d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (3.9999999999999996d+0) 
term(97) = term(97) * (4.0d+0) 
term(98) = term(98) * (3.9999999999999996d+0) 
term(99) = term(99) * (-3.9999999999999996d+0) 
term(100) = term(100) * (-8.0d+0) 
term(101) = term(101) * (8.0d+0) 
term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * (-3.9999999999999996d+0) 
term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * (8.0d+0) 
term(106) = term(106) * (4.0d+0) 
term(107) = term(107) * (3.9999999999999996d+0) 
term(108) = term(108) * (-8.0d+0) 
term(109) = term(109) * (-4.0d+0) 
term(110) = term(110) * (-3.9999999999999996d+0) 
term(111) = term(111) * (8.0d+0) 
term(112) = term(112) * (-4.0d+0) 
term(113) = term(113) * (8.0d+0) 
term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (8.0d+0) 
term(118) = term(118) * (7.999999999999999d+0) 
term(119) = term(119) * (-15.999999999999998d+0) 
term(120) = term(120) * (7.999999999999999d+0) 
term(121) = term(121) * (2.0d+0) 
term(122) = term(122) * (-4.0d+0) 
term(123) = term(123) * (2.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (2.0d+0) 
term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * (-3.9999999999999996d+0) 
term(128) = term(128) * (7.999999999999999d+0) 
term(129) = term(129) * (-3.9999999999999996d+0) 
term(130) = term(130) * (1.9999999999999998d+0) 
term(131) = term(131) * (-3.9999999999999996d+0) 
term(132) = term(132) * (1.9999999999999998d+0) 
term(133) = term(133) * (2.0d+0) 
term(134) = term(134) * (2.0d+0) 
term(135) = term(135) * (-4.0d+0) 
term(136) = term(136) * (2.0d+0) 
term(137) = term(137) * (-4.0d+0) 
term(138) = term(138) * (2.0d+0) 
term(139) = term(139) * (2.0d+0) 
term(140) = term(140) * (2.0d+0) 
term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * (-3.9999999999999996d+0) 
term(143) = term(143) * (7.999999999999999d+0) 
term(144) = term(144) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(145) = term(145) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,p,l,k,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(145) = term(145) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(146) = term(146) + wm_interm_111_so_pt3(i,j) * wm_interm_2_so_pt3(q,p,j,i)
term(147) = term(147) + wm_interm_111_so_pt3(i,j) * wm_interm_1_so_pt3(q,j,p,i)
term(148) = term(148) + wm_interm_111_so_pt3(i,j) * wm_interm_3_so_pt3(q,j,p,i)
term(149) = term(149) + wm_interm_113_so_pt3(i,j) * wm_interm_2_so_pt3(q,p,j,i)
term(150) = term(150) + wm_interm_113_so_pt3(i,j) * wm_interm_1_so_pt3(q,j,p,i)
term(151) = term(151) + wm_interm_113_so_pt3(i,j) * wm_interm_3_so_pt3(q,j,p,i)
end do 
end do 

term(146) = term(146) * (3.9999999999999996d+0) 
term(147) = term(147) * (4.0d+0) 
term(148) = term(148) * (-8.0d+0) 
term(149) = term(149) * (-3.9999999999999996d+0) 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(152) = term(152) + wm_interm_12_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(a,q,j,i)
term(153) = term(153) + wm_interm_13_so_pt3(a,i,j,p) * wm_interm_18_so_pt3(a,q,j,i)
end do 
end do 
end do 

term(152) = term(152) * (12.0d+0) 
term(153) = term(153) * (-16.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(154) = term(154) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,p,j,i) * wm_interm_100_so_pt3(a,c)
term(155) = term(155) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,p,j,i) * wm_interm_100_so_pt3(b,c)
term(156) = term(156) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,p,j,i) * wm_interm_101_so_pt3(a,c)
term(157) = term(157) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,p,j,i) * wm_interm_101_so_pt3(b,c)
term(158) = term(158) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,p,j,i) * wm_interm_100_so_pt3(a,c)
term(159) = term(159) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,p,j,i) * wm_interm_101_so_pt3(a,c)
end do 
end do 
end do 
end do 
end do 

term(154) = term(154) * (-1.9999999999999998d+0) 
term(155) = term(155) * (3.9999999999999996d+0) 
term(157) = term(157) * (-1.9999999999999998d+0) 
term(158) = term(158) * (-8.0d+0) 
term(159) = term(159) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(160) = term(160) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_25_so_pt3(a,b,i,j)
term(161) = term(161) + s1(a,i) * s2(b,q,p,j) * wm_interm_23_so_pt3(a,b,j,i)
term(162) = term(162) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_26_so_pt3(a,b,i,j)
term(163) = term(163) + s1(a,i) * s2(b,q,p,j) * wm_interm_24_so_pt3(a,b,j,i)
term(164) = term(164) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_28_so_pt3(a,b,i,j)
term(165) = term(165) + s1(a,i) * s2(b,q,p,j) * wm_interm_23_so_pt3(a,b,i,j)
term(166) = term(166) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_27_so_pt3(a,b,i,j)
term(167) = term(167) + s1(a,i) * s2(b,q,p,j) * wm_interm_24_so_pt3(a,b,i,j)
term(168) = term(168) + s1(a,p) * s2(b,q,j,i) * wm_interm_23_so_pt3(a,b,i,j)
term(169) = term(169) + s1(a,p) * s2(b,q,j,i) * wm_interm_24_so_pt3(a,b,i,j)
end do 
end do 
end do 
end do 

term(160) = term(160) * (8.0d+0) 
term(161) = term(161) * (6.0d+0) 
term(162) = term(162) * (3.0d+0) 
term(163) = term(163) * (-4.0d+0) 
term(164) = term(164) * (-4.0d+0) 
term(165) = term(165) * (-4.0d+0) 
term(166) = term(166) * (-4.0d+0) 
term(167) = term(167) * (4.0d+0) 
term(168) = term(168) * (-16.0d+0) 
term(169) = term(169) * (8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(170) = term(170) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,j) * wm_interm_73_so_pt3(c,a,k,i)
term(171) = term(171) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,j) * wm_interm_76_so_pt3(c,a,k,i)
term(172) = term(172) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,j) * wm_interm_75_so_pt3(c,a,k,i)
term(173) = term(173) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,i) * wm_interm_74_so_pt3(c,a,k,j)
term(174) = term(174) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,i) * wm_interm_75_so_pt3(c,a,k,j)
term(175) = term(175) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,i) * wm_interm_76_so_pt3(c,a,k,j)
term(176) = term(176) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,i) * wm_interm_79_so_pt3(c,a,k,j)
term(177) = term(177) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,j) * wm_interm_79_so_pt3(c,a,k,i)
term(178) = term(178) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,i) * wm_interm_81_so_pt3(c,a,k,j)
term(179) = term(179) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,j) * wm_interm_81_so_pt3(c,a,k,i)
term(180) = term(180) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,i) * wm_interm_83_so_pt3(c,a,k,j)
term(181) = term(181) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,j) * wm_interm_83_so_pt3(c,a,k,i)
term(182) = term(182) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,j) * wm_interm_107_so_pt3(c,a,k,i)
term(183) = term(183) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,j) * wm_interm_108_so_pt3(c,a,k,i)
term(184) = term(184) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,i) * wm_interm_108_so_pt3(c,a,k,j)
term(185) = term(185) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,i) * wm_interm_107_so_pt3(c,a,k,j)
term(186) = term(186) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,i) * wm_interm_110_so_pt3(c,a,k,j)
term(187) = term(187) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,j) * wm_interm_110_so_pt3(c,a,k,i)
term(188) = term(188) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,i) * wm_interm_112_so_pt3(c,a,k,j)
term(189) = term(189) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,k,j) * wm_interm_112_so_pt3(c,a,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(172) = term(172) * (-1.9999999999999998d+0) 
term(175) = term(175) * (-2.0d+0) 
term(177) = term(177) * (-1.9999999999999998d+0) 
term(179) = term(179) * (-1.9999999999999998d+0) 
term(180) = term(180) * (-2.0d+0) 
term(181) = term(181) * (3.9999999999999996d+0) 
term(182) = term(182) * (3.9999999999999996d+0) 
term(183) = term(183) * (-3.9999999999999996d+0) 
term(184) = term(184) * (4.0d+0) 
term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * (4.0d+0) 
term(187) = term(187) * (-7.999999999999999d+0) 
term(188) = term(188) * (-4.0d+0) 
term(189) = term(189) * (7.999999999999999d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(190) = term(190) + wm_interm_102_so_pt3(q,i,j,p,k,l) * wm_interm_78_so_pt3(k,l,j,i)
term(191) = term(191) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_88_so_pt3(q,j,i,k,p,l)
term(192) = term(192) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_88_so_pt3(q,j,i,p,k,l)
term(193) = term(193) + wm_interm_102_so_pt3(q,i,j,p,k,l) * wm_interm_109_so_pt3(k,l,j,i)
term(194) = term(194) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_117_so_pt3(q,j,i,k,p,l)
term(195) = term(195) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_117_so_pt3(q,j,i,p,k,l)
end do 
end do 
end do 
end do 

term(190) = term(190) * (0.5d+0) 
term(191) = term(191) * (0.5d+0) 
term(192) = term(192) * (-1.0d+0) 
term(193) = term(193) * (-2.0d+0) 
term(195) = term(195) * (-2.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(196) = term(196) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_88_so_pt3(q,i,j,l,p,k)
term(197) = term(197) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_88_so_pt3(q,i,j,p,l,k)
term(198) = term(198) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_117_so_pt3(q,i,j,l,p,k)
term(199) = term(199) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_117_so_pt3(q,i,j,p,l,k)
end do 
end do 
end do 
end do 

term(196) = term(196) * (0.5d+0) 
term(197) = term(197) * (-1.0d+0) 
term(199) = term(199) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(200) = term(200) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,q,p,k,j) * wm_interm_5_so_pt3(b,i,k,j)
term(201) = term(201) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,q,p,k,j) * wm_interm_5_so_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(200) = term(200) * (-2.0d+0) 
term(201) = term(201) * (4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(202) = term(202) + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,q,k,p,j) * wm_interm_5_so_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(202) = term(202) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(203) = term(203) + r3(vrdav_Rl, a,p,b,j,q,i) * r1(vrdav_Rr, b,k) * wm_interm_52_so_pt3(a,k,j,i)
term(204) = term(204) + r3(vrdav_Rl, a,p,b,j,q,i) * r1(vrdav_Rr, a,k) * wm_interm_52_so_pt3(b,k,j,i)
term(205) = term(205) + r3(vrdav_Rl, a,p,b,j,q,i) * r1(vrdav_Rr, a,k) * wm_interm_52_so_pt3(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(203) = term(203) * (-1.9999999999999998d+0) 
term(204) = term(204) * (3.9999999999999996d+0) 
term(205) = term(205) * (-1.9999999999999998d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(206) = term(206) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,k,l,p,i,j)
term(207) = term(207) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,l,k,p,i,j)
term(208) = term(208) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,p,k,l,i,j)
term(209) = term(209) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,k,l,p,i,j)
term(210) = term(210) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,l,k,p,i,j)
term(211) = term(211) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,p,k,l,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(206) = term(206) * (0.5d+0) 
term(207) = term(207) * (-1.0d+0) 
term(208) = term(208) * (0.5d+0) 
term(209) = term(209) * (2.0d+0) 
term(210) = term(210) * (-2.0d+0) 
term(211) = term(211) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(212) = term(212) + r2(vrdav_Rl, a,p,q,i) * r2p(vrdav_Rr, a,j,b,k) * wm_interm_1_so_pt3(b,j,k,i)
term(213) = term(213) + r2(vrdav_Rl, a,p,q,i) * r2p(vrdav_Rr, a,j,b,k) * wm_interm_1_so_pt3(b,k,j,i)
term(214) = term(214) + r2(vrdav_Rl, a,p,q,i) * r2p(vrdav_Rr, a,j,b,k) * wm_interm_2_so_pt3(b,k,j,i)
term(215) = term(215) + r2(vrdav_Rl, a,p,q,i) * r2p(vrdav_Rr, a,j,b,k) * wm_interm_2_so_pt3(b,j,k,i)
term(216) = term(216) + r2(vrdav_Rl, a,p,q,i) * r2p(vrdav_Rr, a,j,b,k) * wm_interm_3_so_pt3(b,j,k,i)
term(217) = term(217) + r2(vrdav_Rl, a,p,q,i) * r2p(vrdav_Rr, a,j,b,k) * wm_interm_3_so_pt3(b,k,j,i)
term(218) = term(218) + r2(vrdav_Rl, a,p,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_1_so_pt3(b,j,k,i)
term(219) = term(219) + r2(vrdav_Rl, a,p,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_1_so_pt3(b,k,j,i)
term(220) = term(220) + r2(vrdav_Rl, a,p,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_2_so_pt3(b,k,j,i)
term(221) = term(221) + r2(vrdav_Rl, a,p,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_2_so_pt3(b,j,k,i)
term(222) = term(222) + r2(vrdav_Rl, a,p,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_3_so_pt3(b,j,k,i)
term(223) = term(223) + r2(vrdav_Rl, a,p,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_3_so_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(213) = term(213) * (-2.0d+0) 
term(215) = term(215) * (-1.9999999999999998d+0) 
term(216) = term(216) * (-2.0d+0) 
term(217) = term(217) * (4.0d+0) 
term(218) = term(218) * (4.0d+0) 
term(219) = term(219) * (-4.0d+0) 
term(220) = term(220) * (4.0d+0) 
term(221) = term(221) * (-3.9999999999999996d+0) 
term(222) = term(222) * (-8.0d+0) 
term(223) = term(223) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(224) = term(224) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,l) * wm_interm_102_so_pt3(b,l,k,p,i,j)
term(225) = term(225) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,l) * wm_interm_102_so_pt3(b,p,k,l,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(224) = term(224) * (0.5d+0) 
term(225) = term(225) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(226) = term(226) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,l) * wm_interm_102_so_pt3(b,p,l,k,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(226) = term(226) * (0.5d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(227) = term(227) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,k,p,l,j,i)
term(228) = term(228) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,k,p,l,j,i)
term(229) = term(229) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,p,k,l,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(227) = term(227) * (0.5d+0) 
term(228) = term(228) * (2.0d+0) 
term(229) = term(229) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(230) = term(230) + r2(vrdav_Rl, a,p,q,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_2_so_pt3(b,j,k,i)
term(231) = term(231) + r2(vrdav_Rl, a,p,q,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_1_so_pt3(b,k,j,i)
term(232) = term(232) + r2(vrdav_Rl, a,p,q,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_3_so_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(232) = term(232) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(233) = term(233) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,l) * wm_interm_102_so_pt3(b,p,k,l,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(233) = term(233) * (0.5d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(234) = term(234) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_85_so_pt3(b,i,j,k)
term(235) = term(235) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_86_so_pt3(b,i,j,k)
term(236) = term(236) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_87_so_pt3(b,i,j,k)
term(237) = term(237) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_90_so_pt3(b,i,j,k)
term(238) = term(238) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_114_so_pt3(b,i,j,k)
term(239) = term(239) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_115_so_pt3(b,i,j,k)
term(240) = term(240) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_116_so_pt3(b,i,j,k)
term(241) = term(241) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_118_so_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(235) = term(235) * (-2.0d+0) 
term(238) = term(238) * (2.0d+0) 
term(239) = term(239) * (-4.0d+0) 
term(240) = term(240) * (2.0d+0) 
term(241) = term(241) * (1.9999999999999998d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(242) = term(242) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_91_so_pt3(b,i,k,j)
term(243) = term(243) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_90_so_pt3(b,i,k,j)
term(244) = term(244) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_85_so_pt3(b,i,k,j)
term(245) = term(245) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_86_so_pt3(b,i,k,j)
term(246) = term(246) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_87_so_pt3(b,i,k,j)
term(247) = term(247) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_119_so_pt3(b,i,k,j)
term(248) = term(248) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_118_so_pt3(b,i,k,j)
term(249) = term(249) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_114_so_pt3(b,i,k,j)
term(250) = term(250) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_115_so_pt3(b,i,k,j)
term(251) = term(251) + r2(vrdav_Rl, a,p,q,i) * t2(a,b,k,j) * wm_interm_116_so_pt3(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(243) = term(243) * (-1.9999999999999998d+0) 
term(244) = term(244) * (-2.0d+0) 
term(245) = term(245) * (4.0d+0) 
term(246) = term(246) * (-2.0d+0) 
term(247) = term(247) * (1.9999999999999998d+0) 
term(248) = term(248) * (-3.9999999999999996d+0) 
term(249) = term(249) * (-4.0d+0) 
term(250) = term(250) * (8.0d+0) 
term(251) = term(251) * (-4.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(252) = term(252) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_88_so_pt3(q,j,i,p,l,k)
term(253) = term(253) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_117_so_pt3(q,j,i,p,l,k)
end do 
end do 
end do 
end do 

term(252) = term(252) * (0.5d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(254) = term(254) + wm_interm_2_so_pt3(q,i,j,k) * wm_interm_77_so_pt3(p,k,i,j)
term(255) = term(255) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_77_so_pt3(p,k,i,j)
term(256) = term(256) + wm_interm_3_so_pt3(q,i,j,k) * wm_interm_77_so_pt3(p,k,i,j)
term(257) = term(257) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_78_so_pt3(p,k,i,j)
term(258) = term(258) + wm_interm_3_so_pt3(q,i,j,k) * wm_interm_78_so_pt3(p,k,i,j)
term(259) = term(259) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_77_so_pt3(k,p,i,j)
term(260) = term(260) + wm_interm_3_so_pt3(q,i,j,k) * wm_interm_77_so_pt3(k,p,i,j)
term(261) = term(261) + wm_interm_2_so_pt3(q,i,j,k) * wm_interm_78_so_pt3(k,p,i,j)
term(262) = term(262) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_78_so_pt3(k,p,i,j)
term(263) = term(263) + wm_interm_3_so_pt3(q,i,j,k) * wm_interm_78_so_pt3(k,p,i,j)
term(264) = term(264) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_85_so_pt3(q,i,j,k)
term(265) = term(265) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_86_so_pt3(q,i,j,k)
term(266) = term(266) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_87_so_pt3(q,i,j,k)
term(267) = term(267) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_85_so_pt3(q,i,k,j)
term(268) = term(268) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_86_so_pt3(q,i,k,j)
term(269) = term(269) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_90_so_pt3(q,i,k,j)
term(270) = term(270) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_91_so_pt3(q,i,k,j)
term(271) = term(271) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_91_so_pt3(q,i,j,k)
term(272) = term(272) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_87_so_pt3(q,i,k,j)
term(273) = term(273) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_91_so_pt3(q,i,k,j)
term(274) = term(274) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_90_so_pt3(q,i,j,k)
term(275) = term(275) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_91_so_pt3(q,i,j,k)
term(276) = term(276) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_114_so_pt3(q,i,j,k)
term(277) = term(277) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_115_so_pt3(q,i,j,k)
term(278) = term(278) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_116_so_pt3(q,i,j,k)
term(279) = term(279) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_114_so_pt3(q,i,k,j)
term(280) = term(280) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_115_so_pt3(q,i,k,j)
term(281) = term(281) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_118_so_pt3(q,i,k,j)
term(282) = term(282) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_119_so_pt3(q,i,k,j)
term(283) = term(283) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_119_so_pt3(q,i,j,k)
term(284) = term(284) + wm_interm_105_so_pt3(i,p,j,k) * wm_interm_116_so_pt3(q,i,k,j)
term(285) = term(285) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_119_so_pt3(q,i,k,j)
term(286) = term(286) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_118_so_pt3(q,i,j,k)
term(287) = term(287) + wm_interm_105_so_pt3(p,i,j,k) * wm_interm_119_so_pt3(q,i,j,k)
end do 
end do 
end do 

term(254) = term(254) * (0.49999999999999994d+0) 
term(255) = term(255) * (-1.0d+0) 
term(256) = term(256) * (2.0d+0) 
term(257) = term(257) * (0.5d+0) 
term(258) = term(258) * (-1.0d+0) 
term(259) = term(259) * (0.5d+0) 
term(260) = term(260) * (-1.0d+0) 
term(261) = term(261) * (0.49999999999999994d+0) 
term(262) = term(262) * (-1.0d+0) 
term(263) = term(263) * (2.0d+0) 
term(264) = term(264) * (0.5d+0) 
term(265) = term(265) * (-1.0d+0) 
term(266) = term(266) * (0.5d+0) 
term(267) = term(267) * (0.5d+0) 
term(268) = term(268) * (-1.0d+0) 
term(269) = term(269) * (0.5d+0) 
term(270) = term(270) * (-1.0d+0) 
term(271) = term(271) * (0.5d+0) 
term(272) = term(272) * (0.5d+0) 
term(273) = term(273) * (0.5d+0) 
term(274) = term(274) * (0.5d+0) 
term(275) = term(275) * (-1.0d+0) 
term(277) = term(277) * (-2.0d+0) 
term(280) = term(280) * (-2.0d+0) 
term(282) = term(282) * (-2.0d+0) 
term(287) = term(287) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(288) = term(288) + wm_interm_2_so_pt3(q,i,j,k) * wm_interm_77_so_pt3(p,k,j,i)
term(289) = term(289) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_77_so_pt3(p,k,j,i)
term(290) = term(290) + wm_interm_3_so_pt3(q,i,j,k) * wm_interm_77_so_pt3(p,k,j,i)
term(291) = term(291) + wm_interm_2_so_pt3(q,i,j,k) * wm_interm_78_so_pt3(p,k,j,i)
term(292) = term(292) + wm_interm_2_so_pt3(q,i,j,k) * wm_interm_77_so_pt3(k,p,j,i)
term(293) = term(293) + wm_interm_2_so_pt3(q,i,j,k) * wm_interm_78_so_pt3(k,p,j,i)
term(294) = term(294) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_78_so_pt3(k,p,j,i)
term(295) = term(295) + wm_interm_3_so_pt3(q,i,j,k) * wm_interm_78_so_pt3(k,p,j,i)
term(296) = term(296) + wm_interm_109_so_pt3(p,i,j,k) * wm_interm_2_so_pt3(q,j,k,i)
term(297) = term(297) + wm_interm_109_so_pt3(p,i,j,k) * wm_interm_2_so_pt3(q,k,j,i)
term(298) = term(298) + wm_interm_109_so_pt3(p,i,j,k) * wm_interm_1_so_pt3(q,k,j,i)
term(299) = term(299) + wm_interm_109_so_pt3(p,i,j,k) * wm_interm_1_so_pt3(q,j,k,i)
term(300) = term(300) + wm_interm_109_so_pt3(p,i,j,k) * wm_interm_3_so_pt3(q,k,j,i)
term(301) = term(301) + wm_interm_109_so_pt3(p,i,j,k) * wm_interm_3_so_pt3(q,j,k,i)
term(302) = term(302) + wm_interm_109_so_pt3(i,p,j,k) * wm_interm_1_so_pt3(q,j,k,i)
term(303) = term(303) + wm_interm_109_so_pt3(i,p,j,k) * wm_interm_2_so_pt3(q,k,j,i)
term(304) = term(304) + wm_interm_109_so_pt3(i,p,j,k) * wm_interm_3_so_pt3(q,j,k,i)
term(305) = term(305) + wm_interm_109_so_pt3(i,p,j,k) * wm_interm_2_so_pt3(q,j,k,i)
term(306) = term(306) + wm_interm_109_so_pt3(i,p,j,k) * wm_interm_1_so_pt3(q,k,j,i)
term(307) = term(307) + wm_interm_109_so_pt3(i,p,j,k) * wm_interm_3_so_pt3(q,k,j,i)
end do 
end do 
end do 

term(288) = term(288) * (-0.9999999999999999d+0) 
term(289) = term(289) * (0.5d+0) 
term(290) = term(290) * (-1.0d+0) 
term(291) = term(291) * (0.49999999999999994d+0) 
term(292) = term(292) * (0.49999999999999994d+0) 
term(293) = term(293) * (-0.9999999999999999d+0) 
term(294) = term(294) * (0.5d+0) 
term(295) = term(295) * (-1.0d+0) 
term(296) = term(296) * (2.0d+0) 
term(297) = term(297) * (-1.9999999999999998d+0) 
term(298) = term(298) * (2.0d+0) 
term(299) = term(299) * (-2.0d+0) 
term(300) = term(300) * (-4.0d+0) 
term(301) = term(301) * (4.0d+0) 
term(302) = term(302) * (2.0d+0) 
term(303) = term(303) * (2.0d+0) 
term(304) = term(304) * (-4.0d+0) 
term(305) = term(305) * (-1.9999999999999998d+0) 
term(306) = term(306) * (-2.0d+0) 
term(307) = term(307) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(308) = term(308) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,i) * wm_interm_92_so_pt3(c,a)
term(309) = term(309) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,i) * wm_interm_93_so_pt3(c,a)
term(310) = term(310) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,i) * wm_interm_94_so_pt3(c,a)
term(311) = term(311) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,i) * wm_interm_120_so_pt3(c,a)
term(312) = term(312) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,i) * wm_interm_121_so_pt3(c,a)
end do 
end do 
end do 
end do 
end do 

term(308) = term(308) * (-2.0d+0) 
term(309) = term(309) * (-2.0d+0) 
term(310) = term(310) * (4.0d+0) 
term(311) = term(311) * (-8.0d+0) 
term(312) = term(312) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(313) = term(313) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_25_so_pt3(a,b,j,i)
term(314) = term(314) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_26_so_pt3(a,b,j,i)
term(315) = term(315) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_27_so_pt3(a,b,j,i)
term(316) = term(316) + r1(vrdav_Rl, a,p) * s2(b,q,j,i) * wm_interm_28_so_pt3(a,b,j,i)
term(317) = term(317) + s1(a,p) * s2(b,q,j,i) * wm_interm_24_so_pt3(a,b,j,i)
term(318) = term(318) + s1(a,p) * s2(b,q,j,i) * wm_interm_23_so_pt3(a,b,j,i)
end do 
end do 
end do 
end do 

term(313) = term(313) * (-8.0d+0) 
term(314) = term(314) * (-4.0d+0) 
term(315) = term(315) * (6.0d+0) 
term(316) = term(316) * (3.0d+0) 
term(317) = term(317) * (-8.0d+0) 
term(318) = term(318) * (12.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(319) = term(319) + s1(a,p) * s2(b,q,j,i) * wm_interm_23_so_pt3(b,a,j,i)
term(320) = term(320) + s1(a,p) * s2(b,q,j,i) * wm_interm_24_so_pt3(b,a,j,i)
end do 
end do 
end do 
end do 

term(319) = term(319) * (-8.0d+0) 
term(320) = term(320) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(321) = term(321) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,p,i) * wm_interm_100_so_pt3(a,c)
term(322) = term(322) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,j,p,i) * wm_interm_100_so_pt3(b,c)
term(323) = term(323) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,p,i) * wm_interm_101_so_pt3(a,c)
term(324) = term(324) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,j,p,i) * wm_interm_101_so_pt3(b,c)
term(325) = term(325) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,p,i) * wm_interm_100_so_pt3(a,c)
term(326) = term(326) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,p,i) * wm_interm_101_so_pt3(a,c)
end do 
end do 
end do 
end do 
end do 

term(321) = term(321) * (3.9999999999999996d+0) 
term(322) = term(322) * (-1.9999999999999998d+0) 
term(323) = term(323) * (-1.9999999999999998d+0) 
term(325) = term(325) * (7.999999999999999d+0) 
term(326) = term(326) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(327) = term(327) + wm_interm_2_so_pt3(q,p,i,j) * wm_interm_4_so_pt3(j,i)
term(328) = term(328) + wm_interm_1_so_pt3(q,i,p,j) * wm_interm_4_so_pt3(j,i)
term(329) = term(329) + wm_interm_3_so_pt3(q,i,p,j) * wm_interm_4_so_pt3(j,i)
term(330) = term(330) + wm_interm_54_so_pt3(q,i,p,j) * wm_interm_55_so_pt3(i,j)
term(331) = term(331) + wm_interm_55_so_pt3(i,j) * wm_interm_57_so_pt3(q,p,i,j)
term(332) = term(332) + wm_interm_55_so_pt3(i,j) * wm_interm_58_so_pt3(q,i,p,j)
term(333) = term(333) + wm_interm_55_so_pt3(i,j) * wm_interm_59_so_pt3(q,i,p,j)
term(334) = term(334) + wm_interm_55_so_pt3(i,j) * wm_interm_60_so_pt3(q,p,i,j)
term(335) = term(335) + wm_interm_55_so_pt3(i,j) * wm_interm_61_so_pt3(q,p,i,j)
term(336) = term(336) + wm_interm_2_so_pt3(q,p,i,j) * wm_interm_80_so_pt3(j,i)
term(337) = term(337) + wm_interm_1_so_pt3(q,i,p,j) * wm_interm_80_so_pt3(j,i)
term(338) = term(338) + wm_interm_3_so_pt3(q,i,p,j) * wm_interm_80_so_pt3(j,i)
term(339) = term(339) + wm_interm_2_so_pt3(q,p,i,j) * wm_interm_82_so_pt3(j,i)
term(340) = term(340) + wm_interm_1_so_pt3(q,i,p,j) * wm_interm_82_so_pt3(j,i)
term(341) = term(341) + wm_interm_3_so_pt3(q,i,p,j) * wm_interm_82_so_pt3(j,i)
term(342) = term(342) + wm_interm_2_so_pt3(q,p,i,j) * wm_interm_84_so_pt3(j,i)
term(343) = term(343) + wm_interm_1_so_pt3(q,i,p,j) * wm_interm_84_so_pt3(j,i)
term(344) = term(344) + wm_interm_3_so_pt3(q,i,p,j) * wm_interm_84_so_pt3(j,i)
term(345) = term(345) + wm_interm_104_so_pt3(i,j) * wm_interm_85_so_pt3(q,i,p,j)
term(346) = term(346) + wm_interm_104_so_pt3(i,j) * wm_interm_86_so_pt3(q,i,p,j)
term(347) = term(347) + wm_interm_104_so_pt3(i,j) * wm_interm_87_so_pt3(q,i,p,j)
term(348) = term(348) + wm_interm_104_so_pt3(i,j) * wm_interm_91_so_pt3(q,i,j,p)
term(349) = term(349) + wm_interm_104_so_pt3(i,j) * wm_interm_90_so_pt3(q,i,p,j)
term(350) = term(350) + wm_interm_104_so_pt3(i,j) * wm_interm_91_so_pt3(q,i,p,j)
term(351) = term(351) + wm_interm_106_so_pt3(i,j) * wm_interm_85_so_pt3(q,i,p,j)
term(352) = term(352) + wm_interm_106_so_pt3(i,j) * wm_interm_86_so_pt3(q,i,p,j)
term(353) = term(353) + wm_interm_106_so_pt3(i,j) * wm_interm_87_so_pt3(q,i,p,j)
term(354) = term(354) + wm_interm_106_so_pt3(i,j) * wm_interm_91_so_pt3(q,i,j,p)
term(355) = term(355) + wm_interm_106_so_pt3(i,j) * wm_interm_90_so_pt3(q,i,p,j)
term(356) = term(356) + wm_interm_106_so_pt3(i,j) * wm_interm_91_so_pt3(q,i,p,j)
term(357) = term(357) + wm_interm_104_so_pt3(i,j) * wm_interm_114_so_pt3(q,i,p,j)
term(358) = term(358) + wm_interm_104_so_pt3(i,j) * wm_interm_115_so_pt3(q,i,p,j)
term(359) = term(359) + wm_interm_104_so_pt3(i,j) * wm_interm_116_so_pt3(q,i,p,j)
term(360) = term(360) + wm_interm_104_so_pt3(i,j) * wm_interm_119_so_pt3(q,i,j,p)
term(361) = term(361) + wm_interm_104_so_pt3(i,j) * wm_interm_118_so_pt3(q,i,p,j)
term(362) = term(362) + wm_interm_104_so_pt3(i,j) * wm_interm_119_so_pt3(q,i,p,j)
term(363) = term(363) + wm_interm_106_so_pt3(i,j) * wm_interm_114_so_pt3(q,i,p,j)
term(364) = term(364) + wm_interm_106_so_pt3(i,j) * wm_interm_115_so_pt3(q,i,p,j)
term(365) = term(365) + wm_interm_106_so_pt3(i,j) * wm_interm_116_so_pt3(q,i,p,j)
term(366) = term(366) + wm_interm_106_so_pt3(i,j) * wm_interm_119_so_pt3(q,i,j,p)
term(367) = term(367) + wm_interm_106_so_pt3(i,j) * wm_interm_118_so_pt3(q,i,p,j)
term(368) = term(368) + wm_interm_106_so_pt3(i,j) * wm_interm_119_so_pt3(q,i,p,j)
end do 
end do 

term(327) = term(327) * (-1.9999999999999998d+0) 
term(328) = term(328) * (-2.0d+0) 
term(329) = term(329) * (4.0d+0) 
term(330) = term(330) * (-1.0d+0) 
term(331) = term(331) * (-1.0d+0) 
term(332) = term(332) * (2.0d+0) 
term(333) = term(333) * (-1.0d+0) 
term(334) = term(334) * (2.0d+0) 
term(335) = term(335) * (-1.0d+0) 
term(338) = term(338) * (-2.0d+0) 
term(341) = term(341) * (-2.0d+0) 
term(342) = term(342) * (-1.9999999999999998d+0) 
term(343) = term(343) * (-2.0d+0) 
term(344) = term(344) * (4.0d+0) 
term(346) = term(346) * (-1.9999999999999998d+0) 
term(350) = term(350) * (-1.9999999999999998d+0) 
term(351) = term(351) * (-1.9999999999999998d+0) 
term(352) = term(352) * (3.9999999999999996d+0) 
term(353) = term(353) * (-1.9999999999999998d+0) 
term(354) = term(354) * (-1.9999999999999998d+0) 
term(355) = term(355) * (-1.9999999999999998d+0) 
term(356) = term(356) * (3.9999999999999996d+0) 
term(357) = term(357) * (1.9999999999999998d+0) 
term(358) = term(358) * (-3.9999999999999996d+0) 
term(359) = term(359) * (1.9999999999999998d+0) 
term(360) = term(360) * (1.9999999999999998d+0) 
term(361) = term(361) * (1.9999999999999998d+0) 
term(362) = term(362) * (-3.9999999999999996d+0) 
term(363) = term(363) * (-3.9999999999999996d+0) 
term(364) = term(364) * (7.999999999999999d+0) 
term(365) = term(365) * (-3.9999999999999996d+0) 
term(366) = term(366) * (-3.9999999999999996d+0) 
term(367) = term(367) * (-3.9999999999999996d+0) 
term(368) = term(368) * (7.999999999999999d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(369) = term(369) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,p,i,j) * wm_interm_100_so_pt3(b,c)
term(370) = term(370) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,p,i,j) * wm_interm_101_so_pt3(b,c)
term(371) = term(371) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,p,i,j) * wm_interm_100_so_pt3(a,c)
term(372) = term(372) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,p,i,j) * wm_interm_101_so_pt3(a,c)
end do 
end do 
end do 
end do 
end do 

term(369) = term(369) * (-1.9999999999999998d+0) 
term(371) = term(371) * (7.999999999999999d+0) 
term(372) = term(372) * (-3.9999999999999996d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(373) = term(373) + wm_interm_7_so_pt3(q,a,p,i) * wm_interm_8_so_pt3(a,i)
term(374) = term(374) + wm_interm_6_so_pt3(q,a,i,p) * wm_interm_8_so_pt3(a,i)
term(375) = term(375) + wm_interm_6_so_pt3(q,a,p,i) * wm_interm_8_so_pt3(a,i)
term(376) = term(376) + wm_interm_7_so_pt3(q,a,p,i) * wm_interm_9_so_pt3(a,i)
term(377) = term(377) + wm_interm_6_so_pt3(q,a,i,p) * wm_interm_9_so_pt3(a,i)
term(378) = term(378) + wm_interm_6_so_pt3(q,a,p,i) * wm_interm_9_so_pt3(a,i)
term(379) = term(379) + wm_interm_18_so_pt3(a,q,p,i) * wm_interm_19_so_pt3(a,i)
term(380) = term(380) + wm_interm_18_so_pt3(a,q,p,i) * wm_interm_20_so_pt3(a,i)
term(381) = term(381) + wm_interm_18_so_pt3(a,q,p,i) * wm_interm_21_so_pt3(a,i)
term(382) = term(382) + wm_interm_18_so_pt3(a,q,p,i) * wm_interm_22_so_pt3(a,i)
term(383) = term(383) + wm_interm_18_so_pt3(q,a,i,p) * wm_interm_19_so_pt3(a,i)
term(384) = term(384) + wm_interm_18_so_pt3(q,a,i,p) * wm_interm_20_so_pt3(a,i)
term(385) = term(385) + wm_interm_18_so_pt3(q,a,i,p) * wm_interm_21_so_pt3(a,i)
term(386) = term(386) + wm_interm_18_so_pt3(q,a,i,p) * wm_interm_22_so_pt3(a,i)
term(387) = term(387) + wm_interm_47_so_pt3(q,a,p,i) * wm_interm_48_so_pt3(a,i)
term(388) = term(388) + wm_interm_47_so_pt3(q,a,i,p) * wm_interm_48_so_pt3(a,i)
term(389) = term(389) + wm_interm_48_so_pt3(a,i) * wm_interm_49_so_pt3(q,a,p,i)
term(390) = term(390) + wm_interm_48_so_pt3(a,i) * wm_interm_50_so_pt3(q,a,p,i)
term(391) = term(391) + wm_interm_48_so_pt3(a,i) * wm_interm_50_so_pt3(q,a,i,p)
term(392) = term(392) + wm_interm_47_so_pt3(a,q,p,i) * wm_interm_48_so_pt3(a,i)
term(393) = term(393) + wm_interm_48_so_pt3(a,i) * wm_interm_49_so_pt3(a,q,p,i)
term(394) = term(394) + wm_interm_48_so_pt3(a,i) * wm_interm_51_so_pt3(q,a,i,p)
term(395) = term(395) + wm_interm_48_so_pt3(a,i) * wm_interm_49_so_pt3(a,q,i,p)
term(396) = term(396) + wm_interm_48_so_pt3(a,i) * wm_interm_50_so_pt3(a,q,i,p)
term(397) = term(397) + wm_interm_48_so_pt3(a,i) * wm_interm_51_so_pt3(a,q,p,i)
term(398) = term(398) + wm_interm_48_so_pt3(a,i) * wm_interm_51_so_pt3(a,q,i,p)
term(399) = term(399) + wm_interm_51_so_pt3(a,q,p,i) * wm_interm_53_so_pt3(a,i)
term(400) = term(400) + wm_interm_47_so_pt3(a,q,p,i) * wm_interm_53_so_pt3(a,i)
term(401) = term(401) + wm_interm_50_so_pt3(q,a,p,i) * wm_interm_53_so_pt3(a,i)
term(402) = term(402) + wm_interm_47_so_pt3(q,a,p,i) * wm_interm_53_so_pt3(a,i)
term(403) = term(403) + wm_interm_51_so_pt3(q,a,i,p) * wm_interm_53_so_pt3(a,i)
term(404) = term(404) + wm_interm_47_so_pt3(q,a,i,p) * wm_interm_53_so_pt3(a,i)
term(405) = term(405) + wm_interm_49_so_pt3(q,a,p,i) * wm_interm_53_so_pt3(a,i)
term(406) = term(406) + wm_interm_50_so_pt3(a,q,i,p) * wm_interm_53_so_pt3(a,i)
term(407) = term(407) + wm_interm_50_so_pt3(q,a,i,p) * wm_interm_53_so_pt3(a,i)
term(408) = term(408) + wm_interm_49_so_pt3(a,q,p,i) * wm_interm_53_so_pt3(a,i)
term(409) = term(409) + wm_interm_49_so_pt3(a,q,i,p) * wm_interm_53_so_pt3(a,i)
term(410) = term(410) + wm_interm_51_so_pt3(a,q,i,p) * wm_interm_53_so_pt3(a,i)
term(411) = term(411) + wm_interm_96_so_pt3(q,a,p,i) * wm_interm_97_so_pt3(a,i)
term(412) = term(412) + wm_interm_96_so_pt3(q,a,p,i) * wm_interm_98_so_pt3(a,i)
term(413) = term(413) + wm_interm_96_so_pt3(q,a,p,i) * wm_interm_99_so_pt3(a,i)
term(414) = term(414) + wm_interm_122_so_pt3(a,i) * wm_interm_96_so_pt3(q,a,p,i)
term(415) = term(415) + wm_interm_123_so_pt3(a,i) * wm_interm_96_so_pt3(q,a,p,i)
term(416) = term(416) + wm_interm_124_so_pt3(a,i) * wm_interm_96_so_pt3(q,a,p,i)
end do 
end do 

term(373) = term(373) * (4.0d+0) 
term(374) = term(374) * (4.0d+0) 
term(375) = term(375) * (-7.999999999999999d+0) 
term(376) = term(376) * (-2.0d+0) 
term(377) = term(377) * (-2.0d+0) 
term(378) = term(378) * (3.9999999999999996d+0) 
term(379) = term(379) * (-8.0d+0) 
term(380) = term(380) * (4.0d+0) 
term(381) = term(381) * (6.0d+0) 
term(382) = term(382) * (-2.0d+0) 
term(383) = term(383) * (-8.0d+0) 
term(384) = term(384) * (4.0d+0) 
term(385) = term(385) * (6.0d+0) 
term(386) = term(386) * (-2.0d+0) 
term(387) = term(387) * (-2.6666666666666665d+0) 
term(388) = term(388) * (1.3333333333333333d+0) 
term(389) = term(389) * (0.6666666666666666d+0) 
term(390) = term(390) * (0.6666666666666666d+0) 
term(391) = term(391) * (-1.3333333333333333d+0) 
term(392) = term(392) * (1.3333333333333333d+0) 
term(393) = term(393) * (-1.3333333333333333d+0) 
term(394) = term(394) * (1.3333333333333333d+0) 
term(395) = term(395) * (0.6666666666666666d+0) 
term(396) = term(396) * (0.6666666666666666d+0) 
term(397) = term(397) * (1.3333333333333333d+0) 
term(398) = term(398) * (-2.6666666666666665d+0) 
term(399) = term(399) * (-0.6666666666666666d+0) 
term(400) = term(400) * (-0.6666666666666666d+0) 
term(401) = term(401) * (-0.3333333333333333d+0) 
term(402) = term(402) * (1.3333333333333333d+0) 
term(403) = term(403) * (-0.6666666666666666d+0) 
term(404) = term(404) * (-0.6666666666666666d+0) 
term(405) = term(405) * (-0.3333333333333333d+0) 
term(406) = term(406) * (-0.3333333333333333d+0) 
term(407) = term(407) * (0.6666666666666666d+0) 
term(408) = term(408) * (0.6666666666666666d+0) 
term(409) = term(409) * (-0.3333333333333333d+0) 
term(410) = term(410) * (1.3333333333333333d+0) 
term(412) = term(412) * (-2.0d+0) 
term(414) = term(414) * (2.0d+0) 
term(415) = term(415) * (-4.0d+0) 
term(416) = term(416) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(417) = term(417) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_88_so_pt3(b,i,j,k,p,l)
term(418) = term(418) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_88_so_pt3(b,j,i,p,k,l)
term(419) = term(419) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_88_so_pt3(b,i,j,p,k,l)
term(420) = term(420) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_88_so_pt3(b,j,i,k,l,p)
term(421) = term(421) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_88_so_pt3(b,i,j,k,l,p)
term(422) = term(422) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_117_so_pt3(b,i,j,k,p,l)
term(423) = term(423) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_117_so_pt3(b,j,i,p,k,l)
term(424) = term(424) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_117_so_pt3(b,i,j,p,k,l)
term(425) = term(425) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_117_so_pt3(b,j,i,k,l,p)
term(426) = term(426) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_117_so_pt3(b,i,j,k,l,p)
end do 
end do 
end do 
end do 
end do 
end do 

term(419) = term(419) * (-2.0d+0) 
term(421) = term(421) * (-1.9999999999999998d+0) 
term(422) = term(422) * (2.0d+0) 
term(423) = term(423) * (2.0d+0) 
term(424) = term(424) * (-4.0d+0) 
term(425) = term(425) * (1.9999999999999998d+0) 
term(426) = term(426) * (-3.9999999999999996d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(427) = term(427) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,i) * wm_interm_92_so_pt3(c,a)
term(428) = term(428) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,i) * wm_interm_93_so_pt3(c,a)
term(429) = term(429) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,i) * wm_interm_94_so_pt3(c,a)
term(430) = term(430) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,i) * wm_interm_120_so_pt3(c,a)
term(431) = term(431) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,i) * wm_interm_121_so_pt3(c,a)
end do 
end do 
end do 
end do 
end do 

term(429) = term(429) * (-1.9999999999999998d+0) 
term(430) = term(430) * (3.9999999999999996d+0) 
term(431) = term(431) * (-3.9999999999999996d+0) 

do k = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(432) = term(432) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_88_so_pt3(b,i,j,l,k,p)
term(433) = term(433) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_88_so_pt3(b,i,j,l,p,k)
term(434) = term(434) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_88_so_pt3(b,j,i,p,l,k)
term(435) = term(435) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_88_so_pt3(b,i,j,p,l,k)
term(436) = term(436) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_117_so_pt3(b,i,j,l,k,p)
term(437) = term(437) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_117_so_pt3(b,i,j,l,p,k)
term(438) = term(438) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_117_so_pt3(b,j,i,p,l,k)
term(439) = term(439) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,l,k) * wm_interm_117_so_pt3(b,i,j,p,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(433) = term(433) * (-2.0d+0) 
term(434) = term(434) * (-2.0d+0) 
term(435) = term(435) * (4.0d+0) 
term(436) = term(436) * (1.9999999999999998d+0) 
term(437) = term(437) * (-4.0d+0) 
term(438) = term(438) * (-4.0d+0) 
term(439) = term(439) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(440) = term(440) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,k) * wm_interm_73_so_pt3(c,a,p,j)
term(441) = term(441) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,k) * wm_interm_74_so_pt3(c,a,p,j)
term(442) = term(442) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,k) * wm_interm_74_so_pt3(c,a,p,i)
term(443) = term(443) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,k) * wm_interm_73_so_pt3(c,a,p,i)
term(444) = term(444) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,j) * wm_interm_74_so_pt3(c,a,p,k)
term(445) = term(445) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,j) * wm_interm_73_so_pt3(c,a,p,k)
term(446) = term(446) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,i) * wm_interm_73_so_pt3(c,a,p,k)
term(447) = term(447) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,i) * wm_interm_74_so_pt3(c,a,p,k)
term(448) = term(448) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,k) * wm_interm_75_so_pt3(c,a,p,i)
term(449) = term(449) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,k) * wm_interm_76_so_pt3(c,a,p,j)
term(450) = term(450) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,i) * wm_interm_76_so_pt3(c,a,p,k)
term(451) = term(451) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,j) * wm_interm_75_so_pt3(c,a,p,k)
term(452) = term(452) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,k) * wm_interm_79_so_pt3(c,a,p,i)
term(453) = term(453) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,j) * wm_interm_79_so_pt3(c,a,p,k)
term(454) = term(454) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,k) * wm_interm_79_so_pt3(c,a,k,i)
term(455) = term(455) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,k) * wm_interm_81_so_pt3(c,a,p,i)
term(456) = term(456) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,j) * wm_interm_81_so_pt3(c,a,p,k)
term(457) = term(457) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,k) * wm_interm_81_so_pt3(c,a,k,i)
term(458) = term(458) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,k) * wm_interm_83_so_pt3(c,a,p,i)
term(459) = term(459) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,j) * wm_interm_83_so_pt3(c,a,p,k)
term(460) = term(460) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,k) * wm_interm_83_so_pt3(c,a,k,i)
term(461) = term(461) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,k) * wm_interm_73_so_pt3(c,a,k,i)
term(462) = term(462) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,k) * wm_interm_76_so_pt3(c,a,k,i)
term(463) = term(463) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,k) * wm_interm_75_so_pt3(c,a,k,i)
term(464) = term(464) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,k) * wm_interm_107_so_pt3(c,a,p,j)
term(465) = term(465) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,k) * wm_interm_108_so_pt3(c,a,p,j)
term(466) = term(466) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,k) * wm_interm_108_so_pt3(c,a,p,i)
term(467) = term(467) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,k) * wm_interm_107_so_pt3(c,a,p,i)
term(468) = term(468) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,j) * wm_interm_108_so_pt3(c,a,p,k)
term(469) = term(469) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,j) * wm_interm_107_so_pt3(c,a,p,k)
term(470) = term(470) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,i) * wm_interm_107_so_pt3(c,a,p,k)
term(471) = term(471) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,i) * wm_interm_108_so_pt3(c,a,p,k)
term(472) = term(472) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,k) * wm_interm_110_so_pt3(c,a,p,i)
term(473) = term(473) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,j) * wm_interm_110_so_pt3(c,a,p,k)
term(474) = term(474) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,k) * wm_interm_110_so_pt3(c,a,k,i)
term(475) = term(475) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,j,k) * wm_interm_112_so_pt3(c,a,p,i)
term(476) = term(476) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,i,j) * wm_interm_112_so_pt3(c,a,p,k)
term(477) = term(477) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,k) * wm_interm_112_so_pt3(c,a,k,i)
term(478) = term(478) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,k) * wm_interm_107_so_pt3(c,a,k,i)
term(479) = term(479) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,j,k) * wm_interm_108_so_pt3(c,a,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(441) = term(441) * (-2.0d+0) 
term(443) = term(443) * (-2.0d+0) 
term(445) = term(445) * (-1.9999999999999998d+0) 
term(447) = term(447) * (-1.9999999999999998d+0) 
term(454) = term(454) * (-2.0d+0) 
term(457) = term(457) * (-2.0d+0) 
term(458) = term(458) * (-2.0d+0) 
term(459) = term(459) * (-1.9999999999999998d+0) 
term(460) = term(460) * (4.0d+0) 
term(463) = term(463) * (-2.0d+0) 
term(464) = term(464) * (4.0d+0) 
term(465) = term(465) * (-4.0d+0) 
term(466) = term(466) * (4.0d+0) 
term(467) = term(467) * (-4.0d+0) 
term(468) = term(468) * (3.9999999999999996d+0) 
term(469) = term(469) * (-3.9999999999999996d+0) 
term(470) = term(470) * (3.9999999999999996d+0) 
term(471) = term(471) * (-3.9999999999999996d+0) 
term(472) = term(472) * (4.0d+0) 
term(473) = term(473) * (3.9999999999999996d+0) 
term(474) = term(474) * (-8.0d+0) 
term(475) = term(475) * (-4.0d+0) 
term(476) = term(476) * (-3.9999999999999996d+0) 
term(477) = term(477) * (8.0d+0) 
term(478) = term(478) * (4.0d+0) 
term(479) = term(479) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(480) = term(480) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,l) * wm_interm_102_so_pt3(b,p,l,k,j,i)
term(481) = term(481) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,l) * wm_interm_102_so_pt3(b,l,p,k,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(480) = term(480) * (-1.0d+0) 
term(481) = term(481) * (0.5d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(482) = term(482) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,j,k) * wm_interm_89_so_pt3(a,c,p,k)
term(483) = term(483) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,j,k) * wm_interm_95_so_pt3(a,c,p,k)
term(484) = term(484) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,j,k) * wm_interm_103_so_pt3(a,c,p,k)
term(485) = term(485) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,j,k) * wm_interm_89_so_pt3(a,c,p,k)
term(486) = term(486) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,j,k) * wm_interm_95_so_pt3(a,c,p,k)
term(487) = term(487) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,j,k) * wm_interm_103_so_pt3(a,c,p,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(482) = term(482) * (-2.0d+0) 
term(485) = term(485) * (-8.0d+0) 
term(486) = term(486) * (4.0d+0) 
term(487) = term(487) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(488) = term(488) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,i,k) * wm_interm_89_so_pt3(a,c,p,k)
term(489) = term(489) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,j,i,k) * wm_interm_89_so_pt3(b,c,p,k)
term(490) = term(490) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,i,k) * wm_interm_95_so_pt3(a,c,p,k)
term(491) = term(491) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,j,i,k) * wm_interm_95_so_pt3(b,c,p,k)
term(492) = term(492) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,i,k) * wm_interm_103_so_pt3(a,c,p,k)
term(493) = term(493) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,j,i,k) * wm_interm_103_so_pt3(b,c,p,k)
term(494) = term(494) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,i,k) * wm_interm_89_so_pt3(a,c,p,k)
term(495) = term(495) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,i,k) * wm_interm_95_so_pt3(a,c,p,k)
term(496) = term(496) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,i,k) * wm_interm_103_so_pt3(a,c,p,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(488) = term(488) * (4.0d+0) 
term(489) = term(489) * (-2.0d+0) 
term(490) = term(490) * (-2.0d+0) 
term(492) = term(492) * (-2.0d+0) 
term(494) = term(494) * (8.0d+0) 
term(495) = term(495) * (-4.0d+0) 
term(496) = term(496) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(497) = term(497) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,i,j) * wm_interm_89_so_pt3(a,c,p,k)
term(498) = term(498) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,i,j) * wm_interm_95_so_pt3(a,c,p,k)
term(499) = term(499) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,k,i,j) * wm_interm_96_so_pt3(b,c,p,k)
term(500) = term(500) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,i,j) * wm_interm_103_so_pt3(a,c,p,k)
term(501) = term(501) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,i,j) * wm_interm_89_so_pt3(a,c,p,k)
term(502) = term(502) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,i,j) * wm_interm_95_so_pt3(a,c,p,k)
term(503) = term(503) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,i,j) * wm_interm_96_so_pt3(a,c,p,k)
term(504) = term(504) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,i,j) * wm_interm_103_so_pt3(a,c,p,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(497) = term(497) * (-2.0d+0) 
term(501) = term(501) * (-8.0d+0) 
term(502) = term(502) * (4.0d+0) 
term(503) = term(503) * (-4.0d+0) 
term(504) = term(504) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(505) = term(505) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_89_so_pt3(a,c,p,k)
term(506) = term(506) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_89_so_pt3(b,c,p,k)
term(507) = term(507) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_95_so_pt3(a,c,p,k)
term(508) = term(508) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_95_so_pt3(b,c,p,k)
term(509) = term(509) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_96_so_pt3(a,c,p,k)
term(510) = term(510) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_96_so_pt3(b,c,p,k)
term(511) = term(511) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_103_so_pt3(a,c,p,k)
term(512) = term(512) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,k,j,i) * wm_interm_103_so_pt3(b,c,p,k)
term(513) = term(513) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_89_so_pt3(a,c,p,k)
term(514) = term(514) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_95_so_pt3(a,c,p,k)
term(515) = term(515) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_96_so_pt3(a,c,p,k)
term(516) = term(516) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,k,j,i) * wm_interm_103_so_pt3(a,c,p,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(505) = term(505) * (4.0d+0) 
term(506) = term(506) * (-2.0d+0) 
term(507) = term(507) * (-2.0d+0) 
term(510) = term(510) * (-2.0d+0) 
term(511) = term(511) * (-2.0d+0) 
term(513) = term(513) * (8.0d+0) 
term(514) = term(514) * (-4.0d+0) 
term(515) = term(515) * (4.0d+0) 
term(516) = term(516) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(517) = term(517) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,i) * wm_interm_73_so_pt3(c,a,p,j)
term(518) = term(518) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,i) * wm_interm_74_so_pt3(c,a,p,j)
term(519) = term(519) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,j) * wm_interm_74_so_pt3(c,a,p,i)
term(520) = term(520) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,j) * wm_interm_73_so_pt3(c,a,p,i)
term(521) = term(521) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,j) * wm_interm_75_so_pt3(c,a,p,i)
term(522) = term(522) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,i) * wm_interm_76_so_pt3(c,a,p,j)
term(523) = term(523) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,j) * wm_interm_79_so_pt3(c,a,p,i)
term(524) = term(524) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,j) * wm_interm_81_so_pt3(c,a,p,i)
term(525) = term(525) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,j) * wm_interm_83_so_pt3(c,a,p,i)
term(526) = term(526) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,i) * wm_interm_73_so_pt3(c,a,k,j)
term(527) = term(527) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,i) * wm_interm_74_so_pt3(c,a,k,j)
term(528) = term(528) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,j) * wm_interm_74_so_pt3(c,a,k,i)
term(529) = term(529) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,j) * wm_interm_73_so_pt3(c,a,k,i)
term(530) = term(530) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,j) * wm_interm_75_so_pt3(c,a,k,i)
term(531) = term(531) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,i) * wm_interm_76_so_pt3(c,a,k,j)
term(532) = term(532) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,j) * wm_interm_79_so_pt3(c,a,k,i)
term(533) = term(533) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,j) * wm_interm_81_so_pt3(c,a,k,i)
term(534) = term(534) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,j) * wm_interm_83_so_pt3(c,a,k,i)
term(535) = term(535) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,i) * wm_interm_107_so_pt3(c,a,p,j)
term(536) = term(536) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,i) * wm_interm_108_so_pt3(c,a,p,j)
term(537) = term(537) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,j) * wm_interm_108_so_pt3(c,a,p,i)
term(538) = term(538) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,j) * wm_interm_107_so_pt3(c,a,p,i)
term(539) = term(539) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,j) * wm_interm_110_so_pt3(c,a,p,i)
term(540) = term(540) + t3(nocc, nactive, a,b,q,j,k,i) * t2(b,c,k,j) * wm_interm_112_so_pt3(c,a,p,i)
term(541) = term(541) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,i) * wm_interm_107_so_pt3(c,a,k,j)
term(542) = term(542) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,i) * wm_interm_108_so_pt3(c,a,k,j)
term(543) = term(543) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,j) * wm_interm_108_so_pt3(c,a,k,i)
term(544) = term(544) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,j) * wm_interm_107_so_pt3(c,a,k,i)
term(545) = term(545) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,j) * wm_interm_110_so_pt3(c,a,k,i)
term(546) = term(546) + t3(nocc, nactive, a,b,q,j,p,i) * t2(b,c,k,j) * wm_interm_112_so_pt3(c,a,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(517) = term(517) * (-2.0d+0) 
term(518) = term(518) * (4.0d+0) 
term(519) = term(519) * (-2.0d+0) 
term(520) = term(520) * (4.0d+0) 
term(521) = term(521) * (-2.0d+0) 
term(522) = term(522) * (-2.0d+0) 
term(523) = term(523) * (-2.0d+0) 
term(524) = term(524) * (-2.0d+0) 
term(525) = term(525) * (4.0d+0) 
term(527) = term(527) * (-2.0d+0) 
term(529) = term(529) * (-2.0d+0) 
term(534) = term(534) * (-2.0d+0) 
term(535) = term(535) * (-8.0d+0) 
term(536) = term(536) * (8.0d+0) 
term(537) = term(537) * (-8.0d+0) 
term(538) = term(538) * (8.0d+0) 
term(539) = term(539) * (-8.0d+0) 
term(540) = term(540) * (8.0d+0) 
term(541) = term(541) * (4.0d+0) 
term(542) = term(542) * (-4.0d+0) 
term(543) = term(543) * (4.0d+0) 
term(544) = term(544) * (-4.0d+0) 
term(545) = term(545) * (4.0d+0) 
term(546) = term(546) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(547) = term(547) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,k,j) * wm_interm_89_so_pt3(a,c,p,k)
term(548) = term(548) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,k,j) * wm_interm_95_so_pt3(a,c,p,k)
term(549) = term(549) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,k,j) * wm_interm_96_so_pt3(a,c,p,k)
term(550) = term(550) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,k,j) * wm_interm_103_so_pt3(a,c,p,k)
term(551) = term(551) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,k,j) * wm_interm_89_so_pt3(a,c,p,k)
term(552) = term(552) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,k,j) * wm_interm_95_so_pt3(a,c,p,k)
term(553) = term(553) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,k,j) * wm_interm_96_so_pt3(a,c,p,k)
term(554) = term(554) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,k,j) * wm_interm_103_so_pt3(a,c,p,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(547) = term(547) * (3.9999999999999996d+0) 
term(548) = term(548) * (-1.9999999999999998d+0) 
term(550) = term(550) * (-1.9999999999999998d+0) 
term(551) = term(551) * (16.0d+0) 
term(552) = term(552) * (-8.0d+0) 
term(553) = term(553) * (4.0d+0) 
term(554) = term(554) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(555) = term(555) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_89_so_pt3(a,c,p,k)
term(556) = term(556) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_89_so_pt3(b,c,p,k)
term(557) = term(557) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_95_so_pt3(a,c,p,k)
term(558) = term(558) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_95_so_pt3(b,c,p,k)
term(559) = term(559) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_96_so_pt3(a,c,p,k)
term(560) = term(560) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_96_so_pt3(b,c,p,k)
term(561) = term(561) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_103_so_pt3(a,c,p,k)
term(562) = term(562) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,c,q,j,k,i) * wm_interm_103_so_pt3(b,c,p,k)
term(563) = term(563) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_89_so_pt3(a,c,p,k)
term(564) = term(564) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_95_so_pt3(a,c,p,k)
term(565) = term(565) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_96_so_pt3(a,c,p,k)
term(566) = term(566) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,j,k,i) * wm_interm_103_so_pt3(a,c,p,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(555) = term(555) * (-7.999999999999999d+0) 
term(556) = term(556) * (3.9999999999999996d+0) 
term(557) = term(557) * (3.9999999999999996d+0) 
term(558) = term(558) * (-1.9999999999999998d+0) 
term(559) = term(559) * (-1.9999999999999998d+0) 
term(561) = term(561) * (3.9999999999999996d+0) 
term(562) = term(562) * (-1.9999999999999998d+0) 
term(563) = term(563) * (-15.999999999999998d+0) 
term(564) = term(564) * (7.999999999999999d+0) 
term(565) = term(565) * (-3.9999999999999996d+0) 
term(566) = term(566) * (7.999999999999999d+0) 

do a = nocc + 1, nactive 
term(567) = term(567) + wm_interm_62_so_pt3(a,p) * wm_interm_63_so_pt3(a,q)
term(568) = term(568) + wm_interm_63_so_pt3(a,q) * wm_interm_64_so_pt3(a,p)
term(569) = term(569) + wm_interm_63_so_pt3(a,q) * wm_interm_65_so_pt3(a,p)
term(570) = term(570) + wm_interm_63_so_pt3(a,q) * wm_interm_66_so_pt3(a,p)
term(571) = term(571) + wm_interm_100_so_pt3(q,a) * wm_interm_97_so_pt3(a,p)
term(572) = term(572) + wm_interm_100_so_pt3(q,a) * wm_interm_98_so_pt3(a,p)
term(573) = term(573) + wm_interm_100_so_pt3(q,a) * wm_interm_99_so_pt3(a,p)
term(574) = term(574) + wm_interm_101_so_pt3(q,a) * wm_interm_97_so_pt3(a,p)
term(575) = term(575) + wm_interm_101_so_pt3(q,a) * wm_interm_98_so_pt3(a,p)
term(576) = term(576) + wm_interm_101_so_pt3(q,a) * wm_interm_99_so_pt3(a,p)
term(577) = term(577) + wm_interm_100_so_pt3(q,a) * wm_interm_122_so_pt3(a,p)
term(578) = term(578) + wm_interm_100_so_pt3(q,a) * wm_interm_123_so_pt3(a,p)
term(579) = term(579) + wm_interm_100_so_pt3(q,a) * wm_interm_124_so_pt3(a,p)
term(580) = term(580) + wm_interm_101_so_pt3(q,a) * wm_interm_122_so_pt3(a,p)
term(581) = term(581) + wm_interm_101_so_pt3(q,a) * wm_interm_123_so_pt3(a,p)
term(582) = term(582) + wm_interm_101_so_pt3(q,a) * wm_interm_124_so_pt3(a,p)
end do 

term(567) = term(567) * (3.9999999999999996d+0) 
term(568) = term(568) * (-4.0d+0) 
term(569) = term(569) * (-1.9999999999999998d+0) 
term(570) = term(570) * (2.0d+0) 
term(571) = term(571) * (-1.9999999999999998d+0) 
term(572) = term(572) * (3.9999999999999996d+0) 
term(573) = term(573) * (-1.9999999999999998d+0) 
term(575) = term(575) * (-1.9999999999999998d+0) 
term(577) = term(577) * (-3.9999999999999996d+0) 
term(578) = term(578) * (7.999999999999999d+0) 
term(579) = term(579) * (-3.9999999999999996d+0) 
term(580) = term(580) * (1.9999999999999998d+0) 
term(581) = term(581) * (-3.9999999999999996d+0) 
term(582) = term(582) * (1.9999999999999998d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(583) = term(583) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,j) * wm_interm_92_so_pt3(c,a)
term(584) = term(584) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,j) * wm_interm_93_so_pt3(c,a)
term(585) = term(585) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,j) * wm_interm_94_so_pt3(c,a)
term(586) = term(586) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,j) * wm_interm_120_so_pt3(c,a)
term(587) = term(587) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,j) * wm_interm_121_so_pt3(c,a)
end do 
end do 
end do 
end do 
end do 

term(585) = term(585) * (-2.0d+0) 
term(586) = term(586) * (4.0d+0) 
term(587) = term(587) * (-4.0d+0) 

do i = 1, nocc 
term(588) = term(588) + wm_interm_55_so_pt3(i,p) * wm_interm_62_so_pt3(q,i)
term(589) = term(589) + wm_interm_55_so_pt3(i,p) * wm_interm_64_so_pt3(q,i)
term(590) = term(590) + wm_interm_55_so_pt3(i,p) * wm_interm_65_so_pt3(q,i)
term(591) = term(591) + wm_interm_55_so_pt3(i,p) * wm_interm_66_so_pt3(q,i)
term(592) = term(592) + wm_interm_104_so_pt3(p,i) * wm_interm_97_so_pt3(q,i)
term(593) = term(593) + wm_interm_104_so_pt3(p,i) * wm_interm_98_so_pt3(q,i)
term(594) = term(594) + wm_interm_104_so_pt3(p,i) * wm_interm_99_so_pt3(q,i)
term(595) = term(595) + wm_interm_106_so_pt3(p,i) * wm_interm_97_so_pt3(q,i)
term(596) = term(596) + wm_interm_106_so_pt3(p,i) * wm_interm_98_so_pt3(q,i)
term(597) = term(597) + wm_interm_106_so_pt3(p,i) * wm_interm_99_so_pt3(q,i)
term(598) = term(598) + wm_interm_104_so_pt3(p,i) * wm_interm_122_so_pt3(q,i)
term(599) = term(599) + wm_interm_104_so_pt3(p,i) * wm_interm_123_so_pt3(q,i)
term(600) = term(600) + wm_interm_104_so_pt3(p,i) * wm_interm_124_so_pt3(q,i)
term(601) = term(601) + wm_interm_106_so_pt3(p,i) * wm_interm_122_so_pt3(q,i)
term(602) = term(602) + wm_interm_106_so_pt3(p,i) * wm_interm_123_so_pt3(q,i)
term(603) = term(603) + wm_interm_106_so_pt3(p,i) * wm_interm_124_so_pt3(q,i)
end do 

term(588) = term(588) * (3.9999999999999996d+0) 
term(589) = term(589) * (-4.0d+0) 
term(590) = term(590) * (-1.9999999999999998d+0) 
term(591) = term(591) * (2.0d+0) 
term(593) = term(593) * (-1.9999999999999998d+0) 
term(595) = term(595) * (-1.9999999999999998d+0) 
term(596) = term(596) * (3.9999999999999996d+0) 
term(597) = term(597) * (-1.9999999999999998d+0) 
term(598) = term(598) * (1.9999999999999998d+0) 
term(599) = term(599) * (-3.9999999999999996d+0) 
term(600) = term(600) * (1.9999999999999998d+0) 
term(601) = term(601) * (-3.9999999999999996d+0) 
term(602) = term(602) * (7.999999999999999d+0) 
term(603) = term(603) * (-3.9999999999999996d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(604) = term(604) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,k) * wm_interm_79_so_pt3(c,a,k,j)
term(605) = term(605) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,k) * wm_interm_81_so_pt3(c,a,k,j)
term(606) = term(606) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,k) * wm_interm_83_so_pt3(c,a,k,j)
term(607) = term(607) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,k) * wm_interm_73_so_pt3(c,a,k,j)
term(608) = term(608) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,k) * wm_interm_76_so_pt3(c,a,k,j)
term(609) = term(609) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,k) * wm_interm_75_so_pt3(c,a,k,j)
term(610) = term(610) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,k) * wm_interm_110_so_pt3(c,a,k,j)
term(611) = term(611) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,k) * wm_interm_112_so_pt3(c,a,k,j)
term(612) = term(612) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,k) * wm_interm_107_so_pt3(c,a,k,j)
term(613) = term(613) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,i,k) * wm_interm_108_so_pt3(c,a,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(604) = term(604) * (-2.0d+0) 
term(605) = term(605) * (-2.0d+0) 
term(606) = term(606) * (4.0d+0) 
term(609) = term(609) * (-2.0d+0) 
term(610) = term(610) * (-8.0d+0) 
term(611) = term(611) * (8.0d+0) 
term(612) = term(612) * (4.0d+0) 
term(613) = term(613) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(614) = term(614) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,k) * wm_interm_79_so_pt3(c,a,k,i)
term(615) = term(615) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,k) * wm_interm_81_so_pt3(c,a,k,i)
term(616) = term(616) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,k) * wm_interm_83_so_pt3(c,a,k,i)
term(617) = term(617) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,k) * wm_interm_73_so_pt3(c,a,k,i)
term(618) = term(618) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,k) * wm_interm_76_so_pt3(c,a,k,i)
term(619) = term(619) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,k) * wm_interm_75_so_pt3(c,a,k,i)
term(620) = term(620) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,k) * wm_interm_110_so_pt3(c,a,k,i)
term(621) = term(621) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,k) * wm_interm_112_so_pt3(c,a,k,i)
term(622) = term(622) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,k) * wm_interm_107_so_pt3(c,a,k,i)
term(623) = term(623) + t3(nocc, nactive, a,b,q,p,j,i) * t2(b,c,j,k) * wm_interm_108_so_pt3(c,a,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(614) = term(614) * (3.9999999999999996d+0) 
term(615) = term(615) * (3.9999999999999996d+0) 
term(616) = term(616) * (-7.999999999999999d+0) 
term(617) = term(617) * (-1.9999999999999998d+0) 
term(618) = term(618) * (-1.9999999999999998d+0) 
term(619) = term(619) * (3.9999999999999996d+0) 
term(620) = term(620) * (15.999999999999998d+0) 
term(621) = term(621) * (-15.999999999999998d+0) 
term(622) = term(622) * (-7.999999999999999d+0) 
term(623) = term(623) * (7.999999999999999d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(624) = term(624) + wm_interm_102_so_pt3(q,i,j,p,k,l) * wm_interm_77_so_pt3(k,l,i,j)
term(625) = term(625) + wm_interm_102_so_pt3(q,i,p,j,k,l) * wm_interm_77_so_pt3(k,l,j,i)
term(626) = term(626) + wm_interm_102_so_pt3(q,i,p,j,k,l) * wm_interm_77_so_pt3(k,l,i,j)
term(627) = term(627) + wm_interm_102_so_pt3(q,i,j,p,k,l) * wm_interm_78_so_pt3(k,l,i,j)
term(628) = term(628) + wm_interm_102_so_pt3(q,i,p,j,k,l) * wm_interm_78_so_pt3(k,l,i,j)
term(629) = term(629) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_88_so_pt3(q,i,j,p,k,l)
term(630) = term(630) + wm_interm_102_so_pt3(q,i,j,p,k,l) * wm_interm_109_so_pt3(k,l,i,j)
term(631) = term(631) + wm_interm_102_so_pt3(q,i,p,j,k,l) * wm_interm_109_so_pt3(k,l,j,i)
term(632) = term(632) + wm_interm_102_so_pt3(q,i,p,j,k,l) * wm_interm_109_so_pt3(k,l,i,j)
term(633) = term(633) + wm_interm_105_so_pt3(i,j,k,l) * wm_interm_117_so_pt3(q,i,j,p,k,l)
end do 
end do 
end do 
end do 

term(624) = term(624) * (0.5d+0) 
term(625) = term(625) * (0.5d+0) 
term(626) = term(626) * (-1.0d+0) 
term(627) = term(627) * (-1.0d+0) 
term(628) = term(628) * (0.5d+0) 
term(629) = term(629) * (0.5d+0) 
term(630) = term(630) * (2.0d+0) 
term(631) = term(631) * (2.0d+0) 
term(632) = term(632) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(634) = term(634) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,p,j) * wm_interm_100_so_pt3(a,c)
term(635) = term(635) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,p,j) * wm_interm_101_so_pt3(a,c)
term(636) = term(636) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,p,j) * wm_interm_100_so_pt3(a,c)
term(637) = term(637) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,q,i,p,j) * wm_interm_101_so_pt3(a,c)
end do 
end do 
end do 
end do 
end do 

term(634) = term(634) * (-1.9999999999999998d+0) 
term(636) = term(636) * (-8.0d+0) 
term(637) = term(637) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(638) = term(638) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,l,p,k,j,i)
term(639) = term(639) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,p,l,k,j,i)
term(640) = term(640) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,l,p,k,j,i)
term(641) = term(641) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,l) * wm_interm_102_so_pt3(b,p,l,k,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(638) = term(638) * (-1.0d+0) 
term(639) = term(639) * (0.5d+0) 
term(640) = term(640) * (-2.0d+0) 
term(641) = term(641) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(642) = term(642) + s1(a,i) * s2(b,q,p,j) * wm_interm_23_so_pt3(b,a,j,i)
term(643) = term(643) + s1(a,i) * s2(b,q,p,j) * wm_interm_24_so_pt3(b,a,j,i)
term(644) = term(644) + s1(a,i) * s2(b,q,p,j) * wm_interm_23_so_pt3(b,a,i,j)
term(645) = term(645) + s1(a,i) * s2(b,q,p,j) * wm_interm_24_so_pt3(b,a,i,j)
term(646) = term(646) + s1(a,p) * s2(b,q,j,i) * wm_interm_23_so_pt3(b,a,i,j)
term(647) = term(647) + s1(a,p) * s2(b,q,j,i) * wm_interm_24_so_pt3(b,a,i,j)
end do 
end do 
end do 
end do 

term(642) = term(642) * (-8.0d+0) 
term(643) = term(643) * (4.0d+0) 
term(644) = term(644) * (6.0d+0) 
term(645) = term(645) * (-4.0d+0) 
term(646) = term(646) * (12.0d+0) 
term(647) = term(647) * (-8.0d+0) 


    calc_D_ov_wm_so_cc3_pt3 = zero
    do s = 0, 647
    calc_D_ov_wm_so_cc3_pt3 = calc_D_ov_wm_so_cc3_pt3 + term(s)
    end do

    end function calc_D_ov_wm_so_cc3_pt3
    
    function calc_D_vo_wm_so_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, k, l, b, c 
    real(F64), dimension(0:991) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_23_so_pt3(a,p,j,i)
term(1) = term(1) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_23_so_pt3(p,a,j,i)
term(2) = term(2) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_24_so_pt3(a,p,j,i)
term(3) = term(3) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_24_so_pt3(p,a,j,i)
term(4) = term(4) + wm_interm_60_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,j,i)
term(5) = term(5) + wm_interm_57_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,j,i)
term(6) = term(6) + wm_interm_58_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,j,i)
term(7) = term(7) + wm_interm_59_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,j,i)
term(8) = term(8) + wm_interm_54_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,j,i)
term(9) = term(9) + wm_interm_61_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,j,i)
term(10) = term(10) + wm_interm_54_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(a,p,j,i)
term(11) = term(11) + wm_interm_58_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(a,p,j,i)
term(12) = term(12) + wm_interm_59_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(a,p,j,i)
term(13) = term(13) + wm_interm_31_so_pt3(a,i,j,q) * wm_interm_76_so_pt3(a,p,j,i)
term(14) = term(14) + wm_interm_31_so_pt3(a,i,j,q) * wm_interm_75_so_pt3(a,p,j,i)
term(15) = term(15) + wm_interm_31_so_pt3(a,i,j,q) * wm_interm_81_so_pt3(a,p,j,i)
term(16) = term(16) + wm_interm_31_so_pt3(a,i,j,q) * wm_interm_83_so_pt3(a,p,j,i)
term(17) = term(17) + wm_interm_37_so_pt3(a,i,j,q) * wm_interm_81_so_pt3(a,p,j,i)
term(18) = term(18) + wm_interm_37_so_pt3(a,i,j,q) * wm_interm_83_so_pt3(a,p,j,i)
term(19) = term(19) + wm_interm_37_so_pt3(a,i,j,q) * wm_interm_75_so_pt3(a,p,j,i)
term(20) = term(20) + wm_interm_37_so_pt3(a,i,j,q) * wm_interm_76_so_pt3(a,p,j,i)
term(21) = term(21) + wm_interm_39_so_pt3(a,i,j,q) * wm_interm_81_so_pt3(a,p,j,i)
term(22) = term(22) + wm_interm_39_so_pt3(a,i,j,q) * wm_interm_83_so_pt3(a,p,j,i)
term(23) = term(23) + wm_interm_39_so_pt3(a,i,j,q) * wm_interm_76_so_pt3(a,p,j,i)
term(24) = term(24) + wm_interm_39_so_pt3(a,i,j,q) * wm_interm_75_so_pt3(a,p,j,i)
term(25) = term(25) + wm_interm_35_so_pt3(a,i,j,q) * wm_interm_76_so_pt3(a,p,j,i)
term(26) = term(26) + wm_interm_35_so_pt3(a,i,j,q) * wm_interm_75_so_pt3(a,p,j,i)
term(27) = term(27) + wm_interm_35_so_pt3(a,i,j,q) * wm_interm_81_so_pt3(a,p,j,i)
term(28) = term(28) + wm_interm_35_so_pt3(a,i,j,q) * wm_interm_83_so_pt3(a,p,j,i)
term(29) = term(29) + wm_interm_31_so_pt3(a,i,j,q) * wm_interm_73_so_pt3(a,p,j,i)
term(30) = term(30) + wm_interm_31_so_pt3(a,i,j,q) * wm_interm_79_so_pt3(a,p,j,i)
term(31) = term(31) + wm_interm_37_so_pt3(a,i,j,q) * wm_interm_74_so_pt3(a,p,j,i)
term(32) = term(32) + wm_interm_37_so_pt3(a,i,j,q) * wm_interm_79_so_pt3(a,p,j,i)
term(33) = term(33) + wm_interm_39_so_pt3(a,i,j,q) * wm_interm_79_so_pt3(a,p,j,i)
term(34) = term(34) + wm_interm_39_so_pt3(a,i,j,q) * wm_interm_73_so_pt3(a,p,j,i)
term(35) = term(35) + wm_interm_35_so_pt3(a,i,j,q) * wm_interm_73_so_pt3(a,p,j,i)
term(36) = term(36) + wm_interm_35_so_pt3(a,i,j,q) * wm_interm_79_so_pt3(a,p,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-6.0d+0) 
term(1) = term(1) * (8.0d+0) 
term(2) = term(2) * (4.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-1.0d+0) 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * (4.0d+0) 
term(18) = term(18) * (-2.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (4.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (-8.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(33) = term(33) * (-2.0d+0) 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(37) = term(37) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_24_so_pt3(p,a,i,j)
term(38) = term(38) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_23_so_pt3(p,a,i,j)
term(39) = term(39) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_23_so_pt3(a,p,i,j)
term(40) = term(40) + wm_interm_10_so_pt3(a,i,j,q) * wm_interm_24_so_pt3(a,p,i,j)
term(41) = term(41) + wm_interm_12_so_pt3(a,q,i,j) * wm_interm_30_so_pt3(a,p,i,j)
term(42) = term(42) + wm_interm_12_so_pt3(a,i,q,j) * wm_interm_30_so_pt3(a,p,i,j)
term(43) = term(43) + wm_interm_15_so_pt3(a,i,q,j) * wm_interm_30_so_pt3(a,p,i,j)
term(44) = term(44) + wm_interm_13_so_pt3(a,q,i,j) * wm_interm_30_so_pt3(a,p,i,j)
term(45) = term(45) + wm_interm_14_so_pt3(a,i,q,j) * wm_interm_30_so_pt3(a,p,i,j)
term(46) = term(46) + wm_interm_13_so_pt3(a,i,q,j) * wm_interm_30_so_pt3(a,p,i,j)
term(47) = term(47) + wm_interm_0_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,q,i)
term(48) = term(48) + wm_interm_0_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,i,q)
term(49) = term(49) + wm_interm_0_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,q,i)
term(50) = term(50) + wm_interm_0_so_pt3(a,p,i,j) * wm_interm_37_so_pt3(a,j,i,q)
term(51) = term(51) + wm_interm_0_so_pt3(a,p,i,j) * wm_interm_39_so_pt3(a,j,i,q)
term(52) = term(52) + wm_interm_0_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,i,q)
term(53) = term(53) + wm_interm_40_so_pt3(a,p,i,j) * wm_interm_41_so_pt3(a,q,j,i)
term(54) = term(54) + wm_interm_41_so_pt3(a,q,i,j) * wm_interm_42_so_pt3(a,p,i,j)
term(55) = term(55) + wm_interm_41_so_pt3(a,q,i,j) * wm_interm_43_so_pt3(a,p,i,j)
term(56) = term(56) + wm_interm_40_so_pt3(a,p,i,j) * wm_interm_41_so_pt3(a,q,i,j)
term(57) = term(57) + wm_interm_41_so_pt3(a,q,i,j) * wm_interm_43_so_pt3(a,p,j,i)
term(58) = term(58) + wm_interm_41_so_pt3(a,q,i,j) * wm_interm_42_so_pt3(a,p,j,i)
term(59) = term(59) + wm_interm_57_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,i,j)
term(60) = term(60) + wm_interm_60_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,i,j)
term(61) = term(61) + wm_interm_54_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,i,j)
term(62) = term(62) + wm_interm_61_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,i,j)
term(63) = term(63) + wm_interm_58_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,i,j)
term(64) = term(64) + wm_interm_59_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(p,a,i,j)
term(65) = term(65) + wm_interm_57_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(a,p,i,j)
term(66) = term(66) + wm_interm_60_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(a,p,i,j)
term(67) = term(67) + wm_interm_61_so_pt3(a,i,j,q) * wm_interm_67_so_pt3(a,p,i,j)
term(68) = term(68) + wm_interm_31_so_pt3(a,i,q,j) * wm_interm_81_so_pt3(a,p,j,i)
term(69) = term(69) + wm_interm_31_so_pt3(a,i,q,j) * wm_interm_83_so_pt3(a,p,j,i)
term(70) = term(70) + wm_interm_31_so_pt3(a,i,q,j) * wm_interm_75_so_pt3(a,p,j,i)
term(71) = term(71) + wm_interm_31_so_pt3(a,i,q,j) * wm_interm_76_so_pt3(a,p,j,i)
term(72) = term(72) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_128_so_pt3(p,a,i,j)
term(73) = term(73) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_128_so_pt3(p,a,i,j)
term(74) = term(74) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_129_so_pt3(p,a,i,j)
term(75) = term(75) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_129_so_pt3(p,a,i,j)
term(76) = term(76) + wm_interm_35_so_pt3(a,i,q,j) * wm_interm_81_so_pt3(a,p,j,i)
term(77) = term(77) + wm_interm_35_so_pt3(a,i,q,j) * wm_interm_83_so_pt3(a,p,j,i)
term(78) = term(78) + wm_interm_35_so_pt3(a,i,q,j) * wm_interm_75_so_pt3(a,p,j,i)
term(79) = term(79) + wm_interm_35_so_pt3(a,i,q,j) * wm_interm_76_so_pt3(a,p,j,i)
term(80) = term(80) + wm_interm_129_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,q,j)
term(81) = term(81) + wm_interm_128_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,q,j)
term(82) = term(82) + wm_interm_128_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,j,q)
term(83) = term(83) + wm_interm_129_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,j,q)
term(84) = term(84) + wm_interm_128_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,q,j)
term(85) = term(85) + wm_interm_129_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,q,j)
term(86) = term(86) + wm_interm_129_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,j,q)
term(87) = term(87) + wm_interm_128_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,j,q)
term(88) = term(88) + wm_interm_130_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,j,q)
term(89) = term(89) + wm_interm_130_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,q,j)
term(90) = term(90) + wm_interm_130_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,j,q)
term(91) = term(91) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_130_so_pt3(p,a,i,j)
term(92) = term(92) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_130_so_pt3(p,a,i,j)
term(93) = term(93) + wm_interm_130_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,q,j)
term(94) = term(94) + wm_interm_131_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,j,q)
term(95) = term(95) + wm_interm_131_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,q,j)
term(96) = term(96) + wm_interm_131_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,j,q)
term(97) = term(97) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_131_so_pt3(p,a,i,j)
term(98) = term(98) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_131_so_pt3(p,a,i,j)
term(99) = term(99) + wm_interm_131_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,q,j)
term(100) = term(100) + wm_interm_31_so_pt3(a,i,q,j) * wm_interm_74_so_pt3(a,p,j,i)
term(101) = term(101) + wm_interm_31_so_pt3(a,i,q,j) * wm_interm_79_so_pt3(a,p,j,i)
term(102) = term(102) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_132_so_pt3(p,a,i,j)
term(103) = term(103) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_132_so_pt3(p,a,i,j)
term(104) = term(104) + wm_interm_35_so_pt3(a,i,q,j) * wm_interm_74_so_pt3(a,p,j,i)
term(105) = term(105) + wm_interm_35_so_pt3(a,i,q,j) * wm_interm_79_so_pt3(a,p,j,i)
term(106) = term(106) + wm_interm_132_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,j,q)
term(107) = term(107) + wm_interm_138_so_pt3(a,i,q,j) * wm_interm_147_so_pt3(p,a,i,j)
term(108) = term(108) + wm_interm_135_so_pt3(a,i,j,q) * wm_interm_147_so_pt3(p,a,i,j)
term(109) = term(109) + wm_interm_132_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,q,j)
term(110) = term(110) + wm_interm_133_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,j,q)
term(111) = term(111) + wm_interm_133_so_pt3(p,a,i,j) * wm_interm_138_so_pt3(a,i,q,j)
term(112) = term(112) + wm_interm_133_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,j,q)
term(113) = term(113) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_133_so_pt3(p,a,i,j)
term(114) = term(114) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_133_so_pt3(p,a,i,j)
term(115) = term(115) + wm_interm_133_so_pt3(p,a,i,j) * wm_interm_135_so_pt3(a,i,q,j)
term(116) = term(116) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_130_so_pt3(p,a,i,j)
term(117) = term(117) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_130_so_pt3(p,a,i,j)
term(118) = term(118) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_131_so_pt3(p,a,i,j)
term(119) = term(119) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_131_so_pt3(p,a,i,j)
term(120) = term(120) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_129_so_pt3(p,a,i,j)
term(121) = term(121) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_128_so_pt3(p,a,i,j)
term(122) = term(122) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_129_so_pt3(p,a,i,j)
term(123) = term(123) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_128_so_pt3(p,a,i,j)
term(124) = term(124) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_147_so_pt3(p,a,i,j)
term(125) = term(125) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_147_so_pt3(p,a,i,j)
term(126) = term(126) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_133_so_pt3(p,a,i,j)
term(127) = term(127) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_133_so_pt3(p,a,i,j)
term(128) = term(128) + wm_interm_110_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,q,i)
term(129) = term(129) + wm_interm_112_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,q,i)
term(130) = term(130) + wm_interm_108_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,q,i)
term(131) = term(131) + wm_interm_107_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,q,i)
term(132) = term(132) + wm_interm_107_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,i,q)
term(133) = term(133) + wm_interm_108_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,i,q)
term(134) = term(134) + wm_interm_110_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,i,q)
term(135) = term(135) + wm_interm_112_so_pt3(a,p,i,j) * wm_interm_31_so_pt3(a,j,i,q)
term(136) = term(136) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_139_so_pt3(p,a,i,j)
term(137) = term(137) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_139_so_pt3(p,a,i,j)
term(138) = term(138) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_140_so_pt3(p,a,i,j)
term(139) = term(139) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_140_so_pt3(p,a,i,j)
term(140) = term(140) + wm_interm_110_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,q,i)
term(141) = term(141) + wm_interm_112_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,q,i)
term(142) = term(142) + wm_interm_108_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,q,i)
term(143) = term(143) + wm_interm_107_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,q,i)
term(144) = term(144) + wm_interm_110_so_pt3(a,p,i,j) * wm_interm_37_so_pt3(a,j,i,q)
term(145) = term(145) + wm_interm_112_so_pt3(a,p,i,j) * wm_interm_37_so_pt3(a,j,i,q)
term(146) = term(146) + wm_interm_108_so_pt3(a,p,i,j) * wm_interm_37_so_pt3(a,j,i,q)
term(147) = term(147) + wm_interm_107_so_pt3(a,p,i,j) * wm_interm_37_so_pt3(a,j,i,q)
term(148) = term(148) + wm_interm_138_so_pt3(a,i,q,j) * wm_interm_140_so_pt3(p,a,i,j)
term(149) = term(149) + wm_interm_138_so_pt3(a,i,q,j) * wm_interm_139_so_pt3(p,a,i,j)
term(150) = term(150) + wm_interm_138_so_pt3(a,i,j,q) * wm_interm_139_so_pt3(p,a,i,j)
term(151) = term(151) + wm_interm_138_so_pt3(a,i,j,q) * wm_interm_140_so_pt3(p,a,i,j)
term(152) = term(152) + wm_interm_135_so_pt3(a,i,q,j) * wm_interm_139_so_pt3(p,a,i,j)
term(153) = term(153) + wm_interm_135_so_pt3(a,i,q,j) * wm_interm_140_so_pt3(p,a,i,j)
term(154) = term(154) + wm_interm_135_so_pt3(a,i,j,q) * wm_interm_140_so_pt3(p,a,i,j)
term(155) = term(155) + wm_interm_135_so_pt3(a,i,j,q) * wm_interm_139_so_pt3(p,a,i,j)
term(156) = term(156) + wm_interm_110_so_pt3(a,p,i,j) * wm_interm_39_so_pt3(a,j,i,q)
term(157) = term(157) + wm_interm_112_so_pt3(a,p,i,j) * wm_interm_39_so_pt3(a,j,i,q)
term(158) = term(158) + wm_interm_107_so_pt3(a,p,i,j) * wm_interm_39_so_pt3(a,j,i,q)
term(159) = term(159) + wm_interm_108_so_pt3(a,p,i,j) * wm_interm_39_so_pt3(a,j,i,q)
term(160) = term(160) + wm_interm_107_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,i,q)
term(161) = term(161) + wm_interm_108_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,i,q)
term(162) = term(162) + wm_interm_110_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,i,q)
term(163) = term(163) + wm_interm_112_so_pt3(a,p,i,j) * wm_interm_35_so_pt3(a,j,i,q)
term(164) = term(164) + wm_interm_138_so_pt3(a,i,j,q) * wm_interm_141_so_pt3(p,a,i,j)
term(165) = term(165) + wm_interm_138_so_pt3(a,i,q,j) * wm_interm_141_so_pt3(p,a,i,j)
term(166) = term(166) + wm_interm_135_so_pt3(a,i,j,q) * wm_interm_141_so_pt3(p,a,i,j)
term(167) = term(167) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_141_so_pt3(p,a,i,j)
term(168) = term(168) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_141_so_pt3(p,a,i,j)
term(169) = term(169) + wm_interm_135_so_pt3(a,i,q,j) * wm_interm_141_so_pt3(p,a,i,j)
term(170) = term(170) + wm_interm_138_so_pt3(a,i,j,q) * wm_interm_142_so_pt3(p,a,i,j)
term(171) = term(171) + wm_interm_138_so_pt3(a,i,q,j) * wm_interm_142_so_pt3(p,a,i,j)
term(172) = term(172) + wm_interm_135_so_pt3(a,i,j,q) * wm_interm_142_so_pt3(p,a,i,j)
term(173) = term(173) + wm_interm_125_so_pt3(a,i,q,j) * wm_interm_142_so_pt3(p,a,i,j)
term(174) = term(174) + wm_interm_126_so_pt3(a,i,q,j) * wm_interm_142_so_pt3(p,a,i,j)
term(175) = term(175) + wm_interm_135_so_pt3(a,i,q,j) * wm_interm_142_so_pt3(p,a,i,j)
term(176) = term(176) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_141_so_pt3(p,a,i,j)
term(177) = term(177) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_141_so_pt3(p,a,i,j)
term(178) = term(178) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_142_so_pt3(p,a,i,j)
term(179) = term(179) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_142_so_pt3(p,a,i,j)
term(180) = term(180) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_140_so_pt3(p,a,i,j)
term(181) = term(181) + wm_interm_125_so_pt3(a,i,j,q) * wm_interm_139_so_pt3(p,a,i,j)
term(182) = term(182) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_140_so_pt3(p,a,i,j)
term(183) = term(183) + wm_interm_126_so_pt3(a,i,j,q) * wm_interm_139_so_pt3(p,a,i,j)
end do 
end do 
end do 

term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (-6.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (-6.0d+0) 
term(42) = term(42) * (8.0d+0) 
term(43) = term(43) * (-6.0d+0) 
term(44) = term(44) * (8.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (-8.0d+0) 
term(47) = term(47) * (-2.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (3.9999999999999996d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (-8.0d+0) 
term(53) = term(53) * (-2.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (-2.0d+0) 
term(56) = term(56) * (4.0d+0) 
term(57) = term(57) * (4.0d+0) 
term(58) = term(58) * (-8.0d+0) 
term(59) = term(59) * (2.0d+0) 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * (-1.0d+0) 
term(62) = term(62) * (2.0d+0) 
term(63) = term(63) * (2.0d+0) 
term(64) = term(64) * (-1.0d+0) 
term(65) = term(65) * (-1.0d+0) 
term(66) = term(66) * (2.0d+0) 
term(67) = term(67) * (-1.0d+0) 
term(69) = term(69) * (-2.0d+0) 
term(71) = term(71) * (-2.0d+0) 
term(73) = term(73) * (-2.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (-1.9999999999999998d+0) 
term(77) = term(77) * (3.9999999999999996d+0) 
term(78) = term(78) * (-1.9999999999999998d+0) 
term(79) = term(79) * (3.9999999999999996d+0) 
term(80) = term(80) * (0.5d+0) 
term(81) = term(81) * (-1.0d+0) 
term(82) = term(82) * (0.5d+0) 
term(83) = term(83) * (-1.0d+0) 
term(84) = term(84) * (0.5d+0) 
term(85) = term(85) * (-1.0d+0) 
term(86) = term(86) * (0.5d+0) 
term(87) = term(87) * (-1.0d+0) 
term(88) = term(88) * (-1.0d+0) 
term(89) = term(89) * (0.5d+0) 
term(90) = term(90) * (0.5d+0) 
term(91) = term(91) * (-2.0d+0) 
term(92) = term(92) * (4.0d+0) 
term(93) = term(93) * (-1.0d+0) 
term(94) = term(94) * (2.0d+0) 
term(95) = term(95) * (-1.0d+0) 
term(96) = term(96) * (-1.0d+0) 
term(97) = term(97) * (4.0d+0) 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * (2.0d+0) 
term(103) = term(103) * (-2.0d+0) 
term(104) = term(104) * (-1.9999999999999998d+0) 
term(105) = term(105) * (-1.9999999999999998d+0) 
term(106) = term(106) * (0.5d+0) 
term(107) = term(107) * (0.5d+0) 
term(108) = term(108) * (0.5d+0) 
term(109) = term(109) * (0.5d+0) 
term(110) = term(110) * (-1.0d+0) 
term(111) = term(111) * (0.5d+0) 
term(112) = term(112) * (0.5d+0) 
term(113) = term(113) * (-2.0d+0) 
term(114) = term(114) * (4.0d+0) 
term(115) = term(115) * (-1.0d+0) 
term(117) = term(117) * (-1.9999999999999998d+0) 
term(118) = term(118) * (-1.9999999999999998d+0) 
term(119) = term(119) * (3.9999999999999996d+0) 
term(121) = term(121) * (-2.0d+0) 
term(122) = term(122) * (-2.0d+0) 
term(123) = term(123) * (4.0d+0) 
term(125) = term(125) * (-2.0d+0) 
term(127) = term(127) * (-1.9999999999999998d+0) 
term(128) = term(128) * (4.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (4.0d+0) 
term(131) = term(131) * (-4.0d+0) 
term(132) = term(132) * (4.0d+0) 
term(133) = term(133) * (-4.0d+0) 
term(134) = term(134) * (-8.0d+0) 
term(135) = term(135) * (8.0d+0) 
term(136) = term(136) * (4.0d+0) 
term(137) = term(137) * (-8.0d+0) 
term(138) = term(138) * (-4.0d+0) 
term(139) = term(139) * (8.0d+0) 
term(140) = term(140) * (-7.999999999999999d+0) 
term(141) = term(141) * (7.999999999999999d+0) 
term(142) = term(142) * (-7.999999999999999d+0) 
term(143) = term(143) * (7.999999999999999d+0) 
term(144) = term(144) * (4.0d+0) 
term(145) = term(145) * (-4.0d+0) 
term(146) = term(146) * (4.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (2.0d+0) 
term(149) = term(149) * (-2.0d+0) 
term(150) = term(150) * (2.0d+0) 
term(151) = term(151) * (-2.0d+0) 
term(152) = term(152) * (2.0d+0) 
term(153) = term(153) * (-2.0d+0) 
term(154) = term(154) * (2.0d+0) 
term(155) = term(155) * (-2.0d+0) 
term(156) = term(156) * (-8.0d+0) 
term(157) = term(157) * (8.0d+0) 
term(158) = term(158) * (4.0d+0) 
term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * (-8.0d+0) 
term(161) = term(161) * (8.0d+0) 
term(162) = term(162) * (16.0d+0) 
term(163) = term(163) * (-16.0d+0) 
term(164) = term(164) * (-4.0d+0) 
term(165) = term(165) * (2.0d+0) 
term(166) = term(166) * (2.0d+0) 
term(167) = term(167) * (-8.0d+0) 
term(168) = term(168) * (16.0d+0) 
term(169) = term(169) * (-4.0d+0) 
term(170) = term(170) * (4.0d+0) 
term(171) = term(171) * (-2.0d+0) 
term(172) = term(172) * (-2.0d+0) 
term(173) = term(173) * (8.0d+0) 
term(174) = term(174) * (-16.0d+0) 
term(175) = term(175) * (4.0d+0) 
term(176) = term(176) * (3.9999999999999996d+0) 
term(177) = term(177) * (-7.999999999999999d+0) 
term(178) = term(178) * (-3.9999999999999996d+0) 
term(179) = term(179) * (7.999999999999999d+0) 
term(180) = term(180) * (4.0d+0) 
term(181) = term(181) * (-4.0d+0) 
term(182) = term(182) * (-8.0d+0) 
term(183) = term(183) * (8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(184) = term(184) + r2p(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,k,q,l)
term(185) = term(185) + r2p(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,q,k,l)
term(186) = term(186) + r2p(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,k,l,q)
term(187) = term(187) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,k,q,l)
term(188) = term(188) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,q,k,l)
term(189) = term(189) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,k,l,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(184) = term(184) * (0.5d+0) 
term(185) = term(185) * (0.5d+0) 
term(186) = term(186) * (0.5d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(190) = term(190) + r2p(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,l,q,k)
term(191) = term(191) + r2p(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,q,l,k)
term(192) = term(192) + r2p(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,l,k,q)
term(193) = term(193) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,l,q,k)
term(194) = term(194) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,q,l,k)
term(195) = term(195) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,l,k,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(190) = term(190) * (-1.0d+0) 
term(191) = term(191) * (-1.0d+0) 
term(192) = term(192) * (0.5d+0) 
term(193) = term(193) * (-2.0d+0) 
term(194) = term(194) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(196) = term(196) + wm_interm_44_so_pt3(p,i,j,k) * wm_interm_78_so_pt3(j,k,i,q)
term(197) = term(197) + wm_interm_44_so_pt3(p,i,j,k) * wm_interm_77_so_pt3(k,j,i,q)
term(198) = term(198) + wm_interm_39_so_pt3(p,i,j,k) * wm_interm_77_so_pt3(j,k,i,q)
term(199) = term(199) + wm_interm_37_so_pt3(p,i,j,k) * wm_interm_78_so_pt3(j,k,i,q)
term(200) = term(200) + wm_interm_45_so_pt3(p,i,j,k) * wm_interm_78_so_pt3(j,k,i,q)
term(201) = term(201) + wm_interm_45_so_pt3(p,i,j,k) * wm_interm_77_so_pt3(k,j,i,q)
term(202) = term(202) + wm_interm_138_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(i,q,j,k)
term(203) = term(203) + wm_interm_138_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(q,i,j,k)
term(204) = term(204) + wm_interm_138_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(q,i,k,j)
term(205) = term(205) + wm_interm_135_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(q,i,j,k)
term(206) = term(206) + wm_interm_135_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(i,q,k,j)
term(207) = term(207) + wm_interm_135_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(q,i,k,j)
term(208) = term(208) + wm_interm_125_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(q,i,j,k)
term(209) = term(209) + wm_interm_126_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(q,i,j,k)
term(210) = term(210) + wm_interm_125_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(i,q,k,j)
term(211) = term(211) + wm_interm_125_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(q,i,k,j)
term(212) = term(212) + wm_interm_126_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(i,q,k,j)
term(213) = term(213) + wm_interm_126_so_pt3(p,i,j,k) * wm_interm_149_so_pt3(q,i,k,j)
term(214) = term(214) + wm_interm_109_so_pt3(i,j,q,k) * wm_interm_44_so_pt3(p,k,i,j)
term(215) = term(215) + wm_interm_109_so_pt3(i,j,q,k) * wm_interm_39_so_pt3(p,k,i,j)
term(216) = term(216) + wm_interm_109_so_pt3(i,j,q,k) * wm_interm_37_so_pt3(p,k,i,j)
term(217) = term(217) + wm_interm_109_so_pt3(i,j,q,k) * wm_interm_45_so_pt3(p,k,i,j)
term(218) = term(218) + wm_interm_138_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(i,q,j,k)
term(219) = term(219) + wm_interm_138_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(q,i,j,k)
term(220) = term(220) + wm_interm_138_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(q,i,k,j)
term(221) = term(221) + wm_interm_135_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(q,i,j,k)
term(222) = term(222) + wm_interm_135_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(i,q,k,j)
term(223) = term(223) + wm_interm_135_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(q,i,k,j)
term(224) = term(224) + wm_interm_125_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(q,i,j,k)
term(225) = term(225) + wm_interm_126_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(q,i,j,k)
term(226) = term(226) + wm_interm_125_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(i,q,k,j)
term(227) = term(227) + wm_interm_125_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(q,i,k,j)
term(228) = term(228) + wm_interm_126_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(i,q,k,j)
term(229) = term(229) + wm_interm_126_so_pt3(p,i,j,k) * wm_interm_157_so_pt3(q,i,k,j)
end do 
end do 
end do 

term(196) = term(196) * (0.5d+0) 
term(197) = term(197) * (0.5d+0) 
term(198) = term(198) * (0.5d+0) 
term(199) = term(199) * (0.5d+0) 
term(200) = term(200) * (-0.9999999999999999d+0) 
term(201) = term(201) * (-0.9999999999999999d+0) 
term(202) = term(202) * (0.49999999999999994d+0) 
term(203) = term(203) * (-0.9999999999999999d+0) 
term(204) = term(204) * (0.49999999999999994d+0) 
term(205) = term(205) * (0.49999999999999994d+0) 
term(206) = term(206) * (0.49999999999999994d+0) 
term(207) = term(207) * (-0.9999999999999999d+0) 
term(209) = term(209) * (-2.0d+0) 
term(211) = term(211) * (-2.0d+0) 
term(212) = term(212) * (-2.0d+0) 
term(213) = term(213) * (4.0d+0) 
term(214) = term(214) * (1.9999999999999996d+0) 
term(215) = term(215) * (-2.0d+0) 
term(216) = term(216) * (1.9999999999999996d+0) 
term(217) = term(217) * (-4.0d+0) 
term(219) = term(219) * (-1.9999999999999998d+0) 
term(223) = term(223) * (-1.9999999999999998d+0) 
term(224) = term(224) * (2.0d+0) 
term(225) = term(225) * (-4.0d+0) 
term(226) = term(226) * (2.0d+0) 
term(227) = term(227) * (-4.0d+0) 
term(228) = term(228) * (-4.0d+0) 
term(229) = term(229) * (8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(230) = term(230) + wm_interm_44_so_pt3(p,i,j,k) * wm_interm_77_so_pt3(j,k,q,i)
term(231) = term(231) + wm_interm_44_so_pt3(p,i,j,k) * wm_interm_78_so_pt3(j,k,q,i)
term(232) = term(232) + wm_interm_44_so_pt3(p,i,j,k) * wm_interm_77_so_pt3(k,j,q,i)
term(233) = term(233) + wm_interm_44_so_pt3(p,i,j,k) * wm_interm_78_so_pt3(k,j,q,i)
term(234) = term(234) + wm_interm_39_so_pt3(p,i,j,k) * wm_interm_77_so_pt3(j,k,q,i)
term(235) = term(235) + wm_interm_39_so_pt3(p,i,j,k) * wm_interm_78_so_pt3(j,k,q,i)
term(236) = term(236) + wm_interm_37_so_pt3(p,i,j,k) * wm_interm_77_so_pt3(j,k,q,i)
term(237) = term(237) + wm_interm_45_so_pt3(p,i,j,k) * wm_interm_77_so_pt3(j,k,q,i)
term(238) = term(238) + wm_interm_37_so_pt3(p,i,j,k) * wm_interm_78_so_pt3(j,k,q,i)
term(239) = term(239) + wm_interm_45_so_pt3(p,i,j,k) * wm_interm_78_so_pt3(j,k,q,i)
term(240) = term(240) + wm_interm_45_so_pt3(p,i,j,k) * wm_interm_77_so_pt3(k,j,q,i)
term(241) = term(241) + wm_interm_45_so_pt3(p,i,j,k) * wm_interm_78_so_pt3(k,j,q,i)
term(242) = term(242) + wm_interm_109_so_pt3(i,j,q,k) * wm_interm_44_so_pt3(p,k,j,i)
term(243) = term(243) + wm_interm_109_so_pt3(i,j,q,k) * wm_interm_45_so_pt3(p,k,j,i)
end do 
end do 
end do 

term(230) = term(230) * (0.5d+0) 
term(231) = term(231) * (-1.0d+0) 
term(232) = term(232) * (-1.0d+0) 
term(233) = term(233) * (0.5d+0) 
term(234) = term(234) * (-1.0d+0) 
term(235) = term(235) * (0.5d+0) 
term(236) = term(236) * (0.5d+0) 
term(237) = term(237) * (-0.9999999999999999d+0) 
term(238) = term(238) * (-1.0d+0) 
term(239) = term(239) * (1.9999999999999998d+0) 
term(240) = term(240) * (1.9999999999999998d+0) 
term(241) = term(241) * (-0.9999999999999999d+0) 
term(242) = term(242) * (-2.0d+0) 
term(243) = term(243) * (3.9999999999999996d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(244) = term(244) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_76_so_pt3(c,b,i,j)
term(245) = term(245) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_73_so_pt3(c,b,i,j)
term(246) = term(246) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_74_so_pt3(c,b,i,j)
term(247) = term(247) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_76_so_pt3(c,b,k,j)
term(248) = term(248) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_75_so_pt3(c,b,k,j)
term(249) = term(249) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_81_so_pt3(c,b,k,j)
term(250) = term(250) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_83_so_pt3(c,b,k,j)
term(251) = term(251) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_73_so_pt3(c,b,k,j)
term(252) = term(252) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_79_so_pt3(c,b,k,j)
term(253) = term(253) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_107_so_pt3(c,b,i,j)
term(254) = term(254) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_108_so_pt3(c,b,i,j)
term(255) = term(255) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_107_so_pt3(c,b,k,j)
term(256) = term(256) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_108_so_pt3(c,b,k,j)
term(257) = term(257) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_110_so_pt3(c,b,k,j)
term(258) = term(258) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_112_so_pt3(c,b,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(246) = term(246) * (-2.0d+0) 
term(248) = term(248) * (-2.0d+0) 
term(249) = term(249) * (-2.0d+0) 
term(250) = term(250) * (4.0d+0) 
term(252) = term(252) * (-2.0d+0) 
term(253) = term(253) * (4.0d+0) 
term(254) = term(254) * (-4.0d+0) 
term(255) = term(255) * (4.0d+0) 
term(256) = term(256) * (-4.0d+0) 
term(257) = term(257) * (-8.0d+0) 
term(258) = term(258) * (8.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(259) = term(259) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_144_so_pt3(b,k,l,i,j,q)
term(260) = term(260) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_144_so_pt3(b,l,k,i,j,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(259) = term(259) * (0.5d+0) 
term(260) = term(260) * (-1.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
term(261) = term(261) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,q) * wm_interm_31_so_pt3(b,k,i,j)
term(262) = term(262) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,q) * wm_interm_35_so_pt3(b,k,i,j)
term(263) = term(263) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,q) * wm_interm_39_so_pt3(b,k,i,j)
term(264) = term(264) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,q) * wm_interm_37_so_pt3(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(261) = term(261) * (0.5d+0) 
term(262) = term(262) * (-1.0d+0) 
term(263) = term(263) * (0.5d+0) 
term(264) = term(264) * (-1.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(265) = term(265) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_81_so_pt3(c,a,i,q)
term(266) = term(266) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_83_so_pt3(c,a,i,q)
term(267) = term(267) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_75_so_pt3(c,a,i,q)
term(268) = term(268) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_74_so_pt3(c,a,i,q)
term(269) = term(269) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_73_so_pt3(c,a,i,q)
term(270) = term(270) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_79_so_pt3(c,a,i,q)
term(271) = term(271) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_132_so_pt3(b,c,q,k)
term(272) = term(272) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_132_so_pt3(b,c,q,k)
term(273) = term(273) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_132_so_pt3(a,c,q,k)
term(274) = term(274) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_132_so_pt3(a,c,q,k)
term(275) = term(275) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_128_so_pt3(b,c,q,k)
term(276) = term(276) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_128_so_pt3(b,c,q,k)
term(277) = term(277) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_128_so_pt3(a,c,q,k)
term(278) = term(278) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_128_so_pt3(a,c,q,k)
term(279) = term(279) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_129_so_pt3(b,c,q,k)
term(280) = term(280) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_129_so_pt3(b,c,q,k)
term(281) = term(281) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_129_so_pt3(a,c,q,k)
term(282) = term(282) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_129_so_pt3(a,c,q,k)
term(283) = term(283) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_147_so_pt3(b,c,q,i)
term(284) = term(284) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_132_so_pt3(b,c,q,i)
term(285) = term(285) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_147_so_pt3(a,c,q,i)
term(286) = term(286) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_132_so_pt3(a,c,q,i)
term(287) = term(287) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_129_so_pt3(b,c,q,i)
term(288) = term(288) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_129_so_pt3(a,c,q,i)
term(289) = term(289) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_133_so_pt3(b,c,q,i)
term(290) = term(290) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_133_so_pt3(b,c,q,k)
term(291) = term(291) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_133_so_pt3(b,c,q,k)
term(292) = term(292) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_133_so_pt3(a,c,q,i)
term(293) = term(293) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_133_so_pt3(a,c,q,k)
term(294) = term(294) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_133_so_pt3(a,c,q,k)
term(295) = term(295) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_130_so_pt3(b,c,q,i)
term(296) = term(296) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_130_so_pt3(b,c,q,k)
term(297) = term(297) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_130_so_pt3(b,c,q,k)
term(298) = term(298) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_130_so_pt3(a,c,q,i)
term(299) = term(299) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_130_so_pt3(a,c,q,k)
term(300) = term(300) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_130_so_pt3(a,c,q,k)
term(301) = term(301) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_131_so_pt3(b,c,q,i)
term(302) = term(302) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_131_so_pt3(b,c,q,k)
term(303) = term(303) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_131_so_pt3(b,c,q,k)
term(304) = term(304) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_131_so_pt3(a,c,q,i)
term(305) = term(305) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_131_so_pt3(a,c,q,k)
term(306) = term(306) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_131_so_pt3(a,c,q,k)
term(307) = term(307) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_110_so_pt3(c,a,i,q)
term(308) = term(308) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_112_so_pt3(c,a,i,q)
term(309) = term(309) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_108_so_pt3(c,a,i,q)
term(310) = term(310) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_107_so_pt3(c,a,i,q)
term(311) = term(311) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_139_so_pt3(b,c,q,k)
term(312) = term(312) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_139_so_pt3(b,c,q,k)
term(313) = term(313) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_139_so_pt3(a,c,q,k)
term(314) = term(314) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_139_so_pt3(a,c,q,k)
term(315) = term(315) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_140_so_pt3(b,c,q,k)
term(316) = term(316) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_140_so_pt3(b,c,q,k)
term(317) = term(317) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_140_so_pt3(a,c,q,k)
term(318) = term(318) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_140_so_pt3(a,c,q,k)
term(319) = term(319) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_140_so_pt3(b,c,q,i)
term(320) = term(320) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_139_so_pt3(b,c,q,i)
term(321) = term(321) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_140_so_pt3(a,c,q,i)
term(322) = term(322) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_139_so_pt3(a,c,q,i)
term(323) = term(323) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_141_so_pt3(b,c,q,i)
term(324) = term(324) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_141_so_pt3(b,c,q,k)
term(325) = term(325) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_141_so_pt3(b,c,q,k)
term(326) = term(326) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_141_so_pt3(a,c,q,i)
term(327) = term(327) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_141_so_pt3(a,c,q,k)
term(328) = term(328) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_141_so_pt3(a,c,q,k)
term(329) = term(329) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_142_so_pt3(b,c,q,i)
term(330) = term(330) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_142_so_pt3(b,c,q,k)
term(331) = term(331) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,j,k,i) * wm_interm_142_so_pt3(b,c,q,k)
term(332) = term(332) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_142_so_pt3(a,c,q,i)
term(333) = term(333) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_142_so_pt3(a,c,q,k)
term(334) = term(334) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_142_so_pt3(a,c,q,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(266) = term(266) * (-2.0d+0) 
term(269) = term(269) * (-2.0d+0) 
term(271) = term(271) * (0.5d+0) 
term(272) = term(272) * (-1.0d+0) 
term(273) = term(273) * (0.5d+0) 
term(274) = term(274) * (-1.0d+0) 
term(275) = term(275) * (0.5d+0) 
term(276) = term(276) * (-1.0d+0) 
term(277) = term(277) * (0.5d+0) 
term(278) = term(278) * (-1.0d+0) 
term(279) = term(279) * (-1.0d+0) 
term(280) = term(280) * (2.0d+0) 
term(281) = term(281) * (-1.0d+0) 
term(282) = term(282) * (2.0d+0) 
term(283) = term(283) * (-1.0d+0) 
term(284) = term(284) * (2.0d+0) 
term(285) = term(285) * (0.5d+0) 
term(286) = term(286) * (-1.0d+0) 
term(287) = term(287) * (-1.0d+0) 
term(288) = term(288) * (0.5d+0) 
term(289) = term(289) * (-0.9999999999999999d+0) 
term(290) = term(290) * (-1.0d+0) 
term(291) = term(291) * (2.0d+0) 
term(292) = term(292) * (0.49999999999999994d+0) 
term(293) = term(293) * (-1.0d+0) 
term(294) = term(294) * (2.0d+0) 
term(295) = term(295) * (-0.9999999999999999d+0) 
term(296) = term(296) * (-1.0d+0) 
term(297) = term(297) * (2.0d+0) 
term(298) = term(298) * (0.49999999999999994d+0) 
term(299) = term(299) * (-1.0d+0) 
term(300) = term(300) * (2.0d+0) 
term(301) = term(301) * (1.9999999999999998d+0) 
term(302) = term(302) * (2.0d+0) 
term(303) = term(303) * (-4.0d+0) 
term(304) = term(304) * (-0.9999999999999999d+0) 
term(305) = term(305) * (2.0d+0) 
term(306) = term(306) * (-4.0d+0) 
term(307) = term(307) * (4.0d+0) 
term(308) = term(308) * (-4.0d+0) 
term(309) = term(309) * (4.0d+0) 
term(310) = term(310) * (-4.0d+0) 
term(311) = term(311) * (1.9999999999999991d+0) 
term(312) = term(312) * (-3.9999999999999982d+0) 
term(313) = term(313) * (1.9999999999999991d+0) 
term(314) = term(314) * (-3.9999999999999982d+0) 
term(315) = term(315) * (-2.0d+0) 
term(316) = term(316) * (4.0d+0) 
term(317) = term(317) * (-2.0d+0) 
term(318) = term(318) * (4.0d+0) 
term(319) = term(319) * (-3.9999999999999996d+0) 
term(320) = term(320) * (4.0d+0) 
term(321) = term(321) * (1.9999999999999998d+0) 
term(322) = term(322) * (-2.0d+0) 
term(323) = term(323) * (-4.0d+0) 
term(324) = term(324) * (-3.9999999999999982d+0) 
term(325) = term(325) * (7.9999999999999964d+0) 
term(326) = term(326) * (2.0d+0) 
term(327) = term(327) * (-3.9999999999999982d+0) 
term(328) = term(328) * (7.9999999999999964d+0) 
term(329) = term(329) * (3.9999999999999996d+0) 
term(330) = term(330) * (4.0d+0) 
term(331) = term(331) * (-8.0d+0) 
term(332) = term(332) * (-1.9999999999999998d+0) 
term(333) = term(333) * (4.0d+0) 
term(334) = term(334) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(335) = term(335) + wm_interm_11_so_pt3(i,j) * wm_interm_12_so_pt3(p,i,q,j)
term(336) = term(336) + wm_interm_11_so_pt3(i,j) * wm_interm_13_so_pt3(p,i,q,j)
term(337) = term(337) + wm_interm_11_so_pt3(i,j) * wm_interm_14_so_pt3(p,i,q,j)
term(338) = term(338) + wm_interm_11_so_pt3(i,j) * wm_interm_15_so_pt3(p,i,q,j)
term(339) = term(339) + wm_interm_11_so_pt3(i,j) * wm_interm_16_so_pt3(p,i,q,j)
term(340) = term(340) + wm_interm_11_so_pt3(i,j) * wm_interm_17_so_pt3(p,i,q,j)
term(341) = term(341) + wm_interm_44_so_pt3(p,i,q,j) * wm_interm_4_so_pt3(j,i)
term(342) = term(342) + wm_interm_39_so_pt3(p,i,j,q) * wm_interm_4_so_pt3(j,i)
term(343) = term(343) + wm_interm_45_so_pt3(p,i,q,j) * wm_interm_4_so_pt3(j,i)
term(344) = term(344) + wm_interm_44_so_pt3(p,i,q,j) * wm_interm_82_so_pt3(j,i)
term(345) = term(345) + wm_interm_44_so_pt3(p,i,q,j) * wm_interm_84_so_pt3(j,i)
term(346) = term(346) + wm_interm_44_so_pt3(p,i,q,j) * wm_interm_80_so_pt3(j,i)
term(347) = term(347) + wm_interm_39_so_pt3(p,i,j,q) * wm_interm_82_so_pt3(j,i)
term(348) = term(348) + wm_interm_39_so_pt3(p,i,j,q) * wm_interm_84_so_pt3(j,i)
term(349) = term(349) + wm_interm_39_so_pt3(p,i,j,q) * wm_interm_80_so_pt3(j,i)
term(350) = term(350) + wm_interm_45_so_pt3(p,i,q,j) * wm_interm_82_so_pt3(j,i)
term(351) = term(351) + wm_interm_45_so_pt3(p,i,q,j) * wm_interm_84_so_pt3(j,i)
term(352) = term(352) + wm_interm_45_so_pt3(p,i,q,j) * wm_interm_80_so_pt3(j,i)
term(353) = term(353) + wm_interm_135_so_pt3(p,i,q,j) * wm_interm_150_so_pt3(i,j)
term(354) = term(354) + wm_interm_138_so_pt3(p,i,j,q) * wm_interm_150_so_pt3(i,j)
term(355) = term(355) + wm_interm_135_so_pt3(p,i,q,j) * wm_interm_151_so_pt3(i,j)
term(356) = term(356) + wm_interm_138_so_pt3(p,i,j,q) * wm_interm_151_so_pt3(i,j)
term(357) = term(357) + wm_interm_135_so_pt3(p,i,q,j) * wm_interm_152_so_pt3(i,j)
term(358) = term(358) + wm_interm_138_so_pt3(p,i,j,q) * wm_interm_152_so_pt3(i,j)
term(359) = term(359) + wm_interm_125_so_pt3(p,i,q,j) * wm_interm_150_so_pt3(i,j)
term(360) = term(360) + wm_interm_126_so_pt3(p,i,q,j) * wm_interm_150_so_pt3(i,j)
term(361) = term(361) + wm_interm_125_so_pt3(p,i,q,j) * wm_interm_151_so_pt3(i,j)
term(362) = term(362) + wm_interm_126_so_pt3(p,i,q,j) * wm_interm_151_so_pt3(i,j)
term(363) = term(363) + wm_interm_125_so_pt3(p,i,q,j) * wm_interm_152_so_pt3(i,j)
term(364) = term(364) + wm_interm_126_so_pt3(p,i,q,j) * wm_interm_152_so_pt3(i,j)
term(365) = term(365) + wm_interm_135_so_pt3(p,i,q,j) * wm_interm_158_so_pt3(i,j)
term(366) = term(366) + wm_interm_138_so_pt3(p,i,j,q) * wm_interm_158_so_pt3(i,j)
term(367) = term(367) + wm_interm_135_so_pt3(p,i,q,j) * wm_interm_159_so_pt3(i,j)
term(368) = term(368) + wm_interm_138_so_pt3(p,i,j,q) * wm_interm_159_so_pt3(i,j)
term(369) = term(369) + wm_interm_135_so_pt3(p,i,q,j) * wm_interm_160_so_pt3(i,j)
term(370) = term(370) + wm_interm_138_so_pt3(p,i,j,q) * wm_interm_160_so_pt3(i,j)
term(371) = term(371) + wm_interm_125_so_pt3(p,i,q,j) * wm_interm_158_so_pt3(i,j)
term(372) = term(372) + wm_interm_126_so_pt3(p,i,q,j) * wm_interm_158_so_pt3(i,j)
term(373) = term(373) + wm_interm_125_so_pt3(p,i,q,j) * wm_interm_159_so_pt3(i,j)
term(374) = term(374) + wm_interm_126_so_pt3(p,i,q,j) * wm_interm_159_so_pt3(i,j)
term(375) = term(375) + wm_interm_125_so_pt3(p,i,q,j) * wm_interm_160_so_pt3(i,j)
term(376) = term(376) + wm_interm_126_so_pt3(p,i,q,j) * wm_interm_160_so_pt3(i,j)
end do 
end do 

term(335) = term(335) * (-6.0d+0) 
term(336) = term(336) * (8.0d+0) 
term(337) = term(337) * (-6.0d+0) 
term(338) = term(338) * (8.0d+0) 
term(339) = term(339) * (4.0d+0) 
term(340) = term(340) * (-8.0d+0) 
term(341) = term(341) * (-2.0d+0) 
term(342) = term(342) * (-2.0d+0) 
term(343) = term(343) * (3.9999999999999996d+0) 
term(345) = term(345) * (-2.0d+0) 
term(348) = term(348) * (-2.0d+0) 
term(350) = term(350) * (-1.9999999999999998d+0) 
term(351) = term(351) * (3.9999999999999996d+0) 
term(352) = term(352) * (-1.9999999999999998d+0) 
term(353) = term(353) * (0.5d+0) 
term(354) = term(354) * (0.5d+0) 
term(355) = term(355) * (-1.0d+0) 
term(356) = term(356) * (-1.0d+0) 
term(357) = term(357) * (0.5d+0) 
term(358) = term(358) * (0.5d+0) 
term(360) = term(360) * (-1.9999999999999998d+0) 
term(361) = term(361) * (-1.9999999999999998d+0) 
term(362) = term(362) * (3.9999999999999996d+0) 
term(364) = term(364) * (-1.9999999999999998d+0) 
term(367) = term(367) * (-2.0d+0) 
term(368) = term(368) * (-2.0d+0) 
term(371) = term(371) * (1.9999999999999998d+0) 
term(372) = term(372) * (-3.9999999999999996d+0) 
term(373) = term(373) * (-3.9999999999999996d+0) 
term(374) = term(374) * (7.999999999999999d+0) 
term(375) = term(375) * (1.9999999999999998d+0) 
term(376) = term(376) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(377) = term(377) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_125_so_pt3(b,i,k,j)
term(378) = term(378) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_126_so_pt3(b,i,k,j)
term(379) = term(379) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,q) * wm_interm_31_so_pt3(a,k,j,i)
term(380) = term(380) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,q) * wm_interm_35_so_pt3(a,k,j,i)
term(381) = term(381) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_135_so_pt3(b,i,k,j)
term(382) = term(382) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_138_so_pt3(b,i,k,j)
term(383) = term(383) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_125_so_pt3(b,i,k,j)
term(384) = term(384) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_126_so_pt3(b,i,k,j)
term(385) = term(385) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_31_so_pt3(b,k,j,i)
term(386) = term(386) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_35_so_pt3(b,k,j,i)
term(387) = term(387) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_31_so_pt3(a,k,j,i)
term(388) = term(388) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_35_so_pt3(a,k,j,i)
term(389) = term(389) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_135_so_pt3(b,i,k,j)
term(390) = term(390) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_138_so_pt3(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(377) = term(377) * (-2.0d+0) 
term(378) = term(378) * (4.0d+0) 
term(379) = term(379) * (0.5d+0) 
term(380) = term(380) * (-1.0d+0) 
term(381) = term(381) * (-1.0d+0) 
term(382) = term(382) * (0.5d+0) 
term(383) = term(383) * (-8.0d+0) 
term(384) = term(384) * (16.0d+0) 
term(385) = term(385) * (4.0d+0) 
term(386) = term(386) * (-8.0d+0) 
term(387) = term(387) * (-2.0d+0) 
term(388) = term(388) * (4.0d+0) 
term(389) = term(389) * (-4.0d+0) 
term(390) = term(390) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(391) = term(391) + wm_interm_153_so_pt3(p,i,j,k,q,l) * wm_interm_77_so_pt3(k,l,j,i)
term(392) = term(392) + wm_interm_153_so_pt3(p,i,j,k,q,l) * wm_interm_78_so_pt3(k,l,j,i)
term(393) = term(393) + wm_interm_153_so_pt3(p,i,j,k,q,l) * wm_interm_77_so_pt3(l,k,j,i)
term(394) = term(394) + wm_interm_153_so_pt3(p,i,j,k,q,l) * wm_interm_78_so_pt3(l,k,j,i)
end do 
end do 
end do 
end do 

term(391) = term(391) * (0.5d+0) 
term(392) = term(392) * (-1.0d+0) 
term(393) = term(393) * (-1.0d+0) 
term(394) = term(394) * (0.5d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(395) = term(395) + wm_interm_153_so_pt3(p,i,j,k,q,l) * wm_interm_78_so_pt3(k,l,i,j)
term(396) = term(396) + wm_interm_153_so_pt3(p,i,j,k,q,l) * wm_interm_77_so_pt3(l,k,i,j)
term(397) = term(397) + wm_interm_143_so_pt3(p,i,j,k,q,l) * wm_interm_149_so_pt3(j,i,k,l)
term(398) = term(398) + wm_interm_143_so_pt3(p,i,j,q,k,l) * wm_interm_149_so_pt3(i,j,k,l)
term(399) = term(399) + wm_interm_143_so_pt3(p,i,j,q,k,l) * wm_interm_149_so_pt3(j,i,l,k)
term(400) = term(400) + wm_interm_143_so_pt3(p,i,j,q,k,l) * wm_interm_149_so_pt3(i,j,l,k)
term(401) = term(401) + wm_interm_143_so_pt3(p,i,j,k,q,l) * wm_interm_149_so_pt3(i,j,l,k)
term(402) = term(402) + wm_interm_143_so_pt3(p,i,j,k,q,l) * wm_interm_149_so_pt3(j,i,l,k)
term(403) = term(403) + wm_interm_143_so_pt3(p,i,j,k,q,l) * wm_interm_157_so_pt3(j,i,k,l)
term(404) = term(404) + wm_interm_143_so_pt3(p,i,j,q,k,l) * wm_interm_157_so_pt3(i,j,k,l)
term(405) = term(405) + wm_interm_143_so_pt3(p,i,j,q,k,l) * wm_interm_157_so_pt3(j,i,l,k)
term(406) = term(406) + wm_interm_143_so_pt3(p,i,j,q,k,l) * wm_interm_157_so_pt3(i,j,l,k)
term(407) = term(407) + wm_interm_143_so_pt3(p,i,j,k,q,l) * wm_interm_157_so_pt3(i,j,l,k)
term(408) = term(408) + wm_interm_143_so_pt3(p,i,j,k,q,l) * wm_interm_157_so_pt3(j,i,l,k)
end do 
end do 
end do 
end do 

term(395) = term(395) * (0.5d+0) 
term(396) = term(396) * (0.5d+0) 
term(397) = term(397) * (0.5d+0) 
term(398) = term(398) * (0.5d+0) 
term(399) = term(399) * (0.5d+0) 
term(400) = term(400) * (-1.0d+0) 
term(401) = term(401) * (0.5d+0) 
term(402) = term(402) * (-1.0d+0) 
term(406) = term(406) * (-2.0d+0) 
term(408) = term(408) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(409) = term(409) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_76_so_pt3(c,a,k,q)
term(410) = term(410) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_75_so_pt3(c,a,k,q)
term(411) = term(411) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_81_so_pt3(c,a,k,q)
term(412) = term(412) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_83_so_pt3(c,a,k,q)
term(413) = term(413) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_73_so_pt3(c,a,k,q)
term(414) = term(414) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_79_so_pt3(c,a,k,q)
term(415) = term(415) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_76_so_pt3(c,a,k,q)
term(416) = term(416) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_75_so_pt3(c,a,k,q)
term(417) = term(417) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_81_so_pt3(c,a,k,q)
term(418) = term(418) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_83_so_pt3(c,a,k,q)
term(419) = term(419) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_73_so_pt3(c,a,k,q)
term(420) = term(420) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_79_so_pt3(c,a,k,q)
term(421) = term(421) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_107_so_pt3(c,a,k,q)
term(422) = term(422) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_108_so_pt3(c,a,k,q)
term(423) = term(423) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_110_so_pt3(c,a,k,q)
term(424) = term(424) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_112_so_pt3(c,a,k,q)
term(425) = term(425) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_107_so_pt3(c,a,k,q)
term(426) = term(426) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_108_so_pt3(c,a,k,q)
term(427) = term(427) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_110_so_pt3(c,a,k,q)
term(428) = term(428) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,j,k,i) * wm_interm_112_so_pt3(c,a,k,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(410) = term(410) * (-2.0d+0) 
term(411) = term(411) * (-2.0d+0) 
term(412) = term(412) * (4.0d+0) 
term(414) = term(414) * (-2.0d+0) 
term(415) = term(415) * (-2.0d+0) 
term(416) = term(416) * (4.0d+0) 
term(417) = term(417) * (4.0d+0) 
term(418) = term(418) * (-8.0d+0) 
term(419) = term(419) * (-2.0d+0) 
term(420) = term(420) * (4.0d+0) 
term(421) = term(421) * (4.0d+0) 
term(422) = term(422) * (-4.0d+0) 
term(423) = term(423) * (-8.0d+0) 
term(424) = term(424) * (8.0d+0) 
term(425) = term(425) * (-8.0d+0) 
term(426) = term(426) * (8.0d+0) 
term(427) = term(427) * (16.0d+0) 
term(428) = term(428) * (-16.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(429) = term(429) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,q,k,l)
term(430) = term(430) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,q,k,l)
term(431) = term(431) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,k,q,l)
term(432) = term(432) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,k,q,l)
term(433) = term(433) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,k,l,q)
term(434) = term(434) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,k,l,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(430) = term(430) * (-2.0d+0) 
term(432) = term(432) * (-2.0d+0) 
term(434) = term(434) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(435) = term(435) + wm_interm_43_so_pt3(a,p,q,i) * wm_interm_46_so_pt3(a,i)
term(436) = term(436) + wm_interm_40_so_pt3(a,p,i,q) * wm_interm_46_so_pt3(a,i)
term(437) = term(437) + wm_interm_42_so_pt3(a,p,q,i) * wm_interm_46_so_pt3(a,i)
term(438) = term(438) + wm_interm_62_so_pt3(a,i) * wm_interm_67_so_pt3(p,a,i,q)
term(439) = term(439) + wm_interm_64_so_pt3(a,i) * wm_interm_67_so_pt3(p,a,i,q)
term(440) = term(440) + wm_interm_65_so_pt3(a,i) * wm_interm_67_so_pt3(p,a,i,q)
term(441) = term(441) + wm_interm_66_so_pt3(a,i) * wm_interm_67_so_pt3(p,a,i,q)
term(442) = term(442) + wm_interm_62_so_pt3(a,i) * wm_interm_67_so_pt3(a,p,q,i)
term(443) = term(443) + wm_interm_64_so_pt3(a,i) * wm_interm_67_so_pt3(a,p,q,i)
term(444) = term(444) + wm_interm_65_so_pt3(a,i) * wm_interm_67_so_pt3(a,p,q,i)
term(445) = term(445) + wm_interm_66_so_pt3(a,i) * wm_interm_67_so_pt3(a,p,q,i)
term(446) = term(446) + wm_interm_33_so_pt3(a,i) * wm_interm_81_so_pt3(a,p,i,q)
term(447) = term(447) + wm_interm_36_so_pt3(a,i) * wm_interm_81_so_pt3(a,p,i,q)
term(448) = term(448) + wm_interm_33_so_pt3(a,i) * wm_interm_83_so_pt3(a,p,i,q)
term(449) = term(449) + wm_interm_36_so_pt3(a,i) * wm_interm_83_so_pt3(a,p,i,q)
term(450) = term(450) + wm_interm_33_so_pt3(a,i) * wm_interm_79_so_pt3(a,p,i,q)
term(451) = term(451) + wm_interm_36_so_pt3(a,i) * wm_interm_79_so_pt3(a,p,i,q)
term(452) = term(452) + wm_interm_33_so_pt3(a,i) * wm_interm_76_so_pt3(a,p,i,q)
term(453) = term(453) + wm_interm_36_so_pt3(a,i) * wm_interm_76_so_pt3(a,p,i,q)
term(454) = term(454) + wm_interm_33_so_pt3(a,i) * wm_interm_75_so_pt3(a,p,i,q)
term(455) = term(455) + wm_interm_36_so_pt3(a,i) * wm_interm_75_so_pt3(a,p,i,q)
term(456) = term(456) + wm_interm_33_so_pt3(a,i) * wm_interm_73_so_pt3(a,p,i,q)
term(457) = term(457) + wm_interm_36_so_pt3(a,i) * wm_interm_73_so_pt3(a,p,i,q)
term(458) = term(458) + wm_interm_34_so_pt3(a,i) * wm_interm_76_so_pt3(a,p,i,q)
term(459) = term(459) + wm_interm_34_so_pt3(a,i) * wm_interm_75_so_pt3(a,p,i,q)
term(460) = term(460) + wm_interm_38_so_pt3(a,i) * wm_interm_76_so_pt3(a,p,i,q)
term(461) = term(461) + wm_interm_38_so_pt3(a,i) * wm_interm_75_so_pt3(a,p,i,q)
term(462) = term(462) + wm_interm_34_so_pt3(a,i) * wm_interm_81_so_pt3(a,p,i,q)
term(463) = term(463) + wm_interm_38_so_pt3(a,i) * wm_interm_81_so_pt3(a,p,i,q)
term(464) = term(464) + wm_interm_34_so_pt3(a,i) * wm_interm_83_so_pt3(a,p,i,q)
term(465) = term(465) + wm_interm_38_so_pt3(a,i) * wm_interm_83_so_pt3(a,p,i,q)
term(466) = term(466) + wm_interm_34_so_pt3(a,i) * wm_interm_73_so_pt3(a,p,i,q)
term(467) = term(467) + wm_interm_38_so_pt3(a,i) * wm_interm_73_so_pt3(a,p,i,q)
term(468) = term(468) + wm_interm_34_so_pt3(a,i) * wm_interm_79_so_pt3(a,p,i,q)
term(469) = term(469) + wm_interm_38_so_pt3(a,i) * wm_interm_79_so_pt3(a,p,i,q)
term(470) = term(470) + wm_interm_127_so_pt3(a,i) * wm_interm_128_so_pt3(p,a,q,i)
term(471) = term(471) + wm_interm_127_so_pt3(a,i) * wm_interm_129_so_pt3(p,a,q,i)
term(472) = term(472) + wm_interm_127_so_pt3(a,i) * wm_interm_130_so_pt3(p,a,q,i)
term(473) = term(473) + wm_interm_127_so_pt3(a,i) * wm_interm_131_so_pt3(p,a,q,i)
term(474) = term(474) + wm_interm_127_so_pt3(a,i) * wm_interm_132_so_pt3(p,a,q,i)
term(475) = term(475) + wm_interm_127_so_pt3(a,i) * wm_interm_133_so_pt3(p,a,q,i)
term(476) = term(476) + wm_interm_128_so_pt3(p,a,q,i) * wm_interm_134_so_pt3(a,i)
term(477) = term(477) + wm_interm_129_so_pt3(p,a,q,i) * wm_interm_134_so_pt3(a,i)
term(478) = term(478) + wm_interm_130_so_pt3(p,a,q,i) * wm_interm_134_so_pt3(a,i)
term(479) = term(479) + wm_interm_131_so_pt3(p,a,q,i) * wm_interm_134_so_pt3(a,i)
term(480) = term(480) + wm_interm_132_so_pt3(p,a,q,i) * wm_interm_134_so_pt3(a,i)
term(481) = term(481) + wm_interm_133_so_pt3(p,a,q,i) * wm_interm_134_so_pt3(a,i)
term(482) = term(482) + wm_interm_130_so_pt3(p,a,q,i) * wm_interm_136_so_pt3(a,i)
term(483) = term(483) + wm_interm_131_so_pt3(p,a,q,i) * wm_interm_136_so_pt3(a,i)
term(484) = term(484) + wm_interm_128_so_pt3(p,a,q,i) * wm_interm_136_so_pt3(a,i)
term(485) = term(485) + wm_interm_129_so_pt3(p,a,q,i) * wm_interm_136_so_pt3(a,i)
term(486) = term(486) + wm_interm_128_so_pt3(p,a,q,i) * wm_interm_137_so_pt3(a,i)
term(487) = term(487) + wm_interm_129_so_pt3(p,a,q,i) * wm_interm_137_so_pt3(a,i)
term(488) = term(488) + wm_interm_130_so_pt3(p,a,q,i) * wm_interm_137_so_pt3(a,i)
term(489) = term(489) + wm_interm_131_so_pt3(p,a,q,i) * wm_interm_137_so_pt3(a,i)
term(490) = term(490) + wm_interm_133_so_pt3(p,a,q,i) * wm_interm_136_so_pt3(a,i)
term(491) = term(491) + wm_interm_132_so_pt3(p,a,q,i) * wm_interm_136_so_pt3(a,i)
term(492) = term(492) + wm_interm_132_so_pt3(p,a,q,i) * wm_interm_137_so_pt3(a,i)
term(493) = term(493) + wm_interm_133_so_pt3(p,a,q,i) * wm_interm_137_so_pt3(a,i)
term(494) = term(494) + wm_interm_110_so_pt3(a,p,i,q) * wm_interm_33_so_pt3(a,i)
term(495) = term(495) + wm_interm_110_so_pt3(a,p,i,q) * wm_interm_36_so_pt3(a,i)
term(496) = term(496) + wm_interm_112_so_pt3(a,p,i,q) * wm_interm_33_so_pt3(a,i)
term(497) = term(497) + wm_interm_112_so_pt3(a,p,i,q) * wm_interm_36_so_pt3(a,i)
term(498) = term(498) + wm_interm_107_so_pt3(a,p,i,q) * wm_interm_33_so_pt3(a,i)
term(499) = term(499) + wm_interm_107_so_pt3(a,p,i,q) * wm_interm_36_so_pt3(a,i)
term(500) = term(500) + wm_interm_108_so_pt3(a,p,i,q) * wm_interm_33_so_pt3(a,i)
term(501) = term(501) + wm_interm_108_so_pt3(a,p,i,q) * wm_interm_36_so_pt3(a,i)
term(502) = term(502) + wm_interm_107_so_pt3(a,p,i,q) * wm_interm_34_so_pt3(a,i)
term(503) = term(503) + wm_interm_108_so_pt3(a,p,i,q) * wm_interm_34_so_pt3(a,i)
term(504) = term(504) + wm_interm_107_so_pt3(a,p,i,q) * wm_interm_38_so_pt3(a,i)
term(505) = term(505) + wm_interm_108_so_pt3(a,p,i,q) * wm_interm_38_so_pt3(a,i)
term(506) = term(506) + wm_interm_110_so_pt3(a,p,i,q) * wm_interm_34_so_pt3(a,i)
term(507) = term(507) + wm_interm_110_so_pt3(a,p,i,q) * wm_interm_38_so_pt3(a,i)
term(508) = term(508) + wm_interm_112_so_pt3(a,p,i,q) * wm_interm_34_so_pt3(a,i)
term(509) = term(509) + wm_interm_112_so_pt3(a,p,i,q) * wm_interm_38_so_pt3(a,i)
term(510) = term(510) + wm_interm_127_so_pt3(a,i) * wm_interm_139_so_pt3(p,a,q,i)
term(511) = term(511) + wm_interm_127_so_pt3(a,i) * wm_interm_140_so_pt3(p,a,q,i)
term(512) = term(512) + wm_interm_127_so_pt3(a,i) * wm_interm_141_so_pt3(p,a,q,i)
term(513) = term(513) + wm_interm_127_so_pt3(a,i) * wm_interm_142_so_pt3(p,a,q,i)
term(514) = term(514) + wm_interm_134_so_pt3(a,i) * wm_interm_139_so_pt3(p,a,q,i)
term(515) = term(515) + wm_interm_134_so_pt3(a,i) * wm_interm_140_so_pt3(p,a,q,i)
term(516) = term(516) + wm_interm_134_so_pt3(a,i) * wm_interm_141_so_pt3(p,a,q,i)
term(517) = term(517) + wm_interm_134_so_pt3(a,i) * wm_interm_142_so_pt3(p,a,q,i)
term(518) = term(518) + wm_interm_136_so_pt3(a,i) * wm_interm_141_so_pt3(p,a,q,i)
term(519) = term(519) + wm_interm_136_so_pt3(a,i) * wm_interm_142_so_pt3(p,a,q,i)
term(520) = term(520) + wm_interm_136_so_pt3(a,i) * wm_interm_139_so_pt3(p,a,q,i)
term(521) = term(521) + wm_interm_136_so_pt3(a,i) * wm_interm_140_so_pt3(p,a,q,i)
term(522) = term(522) + wm_interm_137_so_pt3(a,i) * wm_interm_139_so_pt3(p,a,q,i)
term(523) = term(523) + wm_interm_137_so_pt3(a,i) * wm_interm_140_so_pt3(p,a,q,i)
term(524) = term(524) + wm_interm_137_so_pt3(a,i) * wm_interm_141_so_pt3(p,a,q,i)
term(525) = term(525) + wm_interm_137_so_pt3(a,i) * wm_interm_142_so_pt3(p,a,q,i)
end do 
end do 

term(435) = term(435) * (-2.0d+0) 
term(436) = term(436) * (-2.0000000000000004d+0) 
term(437) = term(437) * (4.0d+0) 
term(438) = term(438) * (3.9999999999999996d+0) 
term(439) = term(439) * (-4.0d+0) 
term(440) = term(440) * (-1.9999999999999998d+0) 
term(441) = term(441) * (2.0d+0) 
term(442) = term(442) * (3.9999999999999996d+0) 
term(443) = term(443) * (-4.0d+0) 
term(444) = term(444) * (-1.9999999999999998d+0) 
term(445) = term(445) * (2.0d+0) 
term(446) = term(446) * (1.9999999999999998d+0) 
term(447) = term(447) * (-3.9999999999999996d+0) 
term(448) = term(448) * (-3.9999999999999996d+0) 
term(449) = term(449) * (7.999999999999999d+0) 
term(450) = term(450) * (1.9999999999999998d+0) 
term(451) = term(451) * (-3.9999999999999996d+0) 
term(452) = term(452) * (-0.9999999999999999d+0) 
term(453) = term(453) * (1.9999999999999998d+0) 
term(454) = term(454) * (1.9999999999999998d+0) 
term(455) = term(455) * (-3.9999999999999996d+0) 
term(456) = term(456) * (-0.9999999999999999d+0) 
term(457) = term(457) * (1.9999999999999998d+0) 
term(459) = term(459) * (-2.0d+0) 
term(460) = term(460) * (-2.0d+0) 
term(461) = term(461) * (4.0d+0) 
term(462) = term(462) * (-2.0d+0) 
term(463) = term(463) * (4.0d+0) 
term(464) = term(464) * (4.0d+0) 
term(465) = term(465) * (-8.0d+0) 
term(467) = term(467) * (-2.0d+0) 
term(468) = term(468) * (-2.0d+0) 
term(469) = term(469) * (4.0d+0) 
term(470) = term(470) * (2.0d+0) 
term(471) = term(471) * (-4.0d+0) 
term(472) = term(472) * (-4.0d+0) 
term(473) = term(473) * (8.0d+0) 
term(474) = term(474) * (2.0d+0) 
term(475) = term(475) * (-4.0d+0) 
term(476) = term(476) * (-1.0d+0) 
term(477) = term(477) * (2.0d+0) 
term(478) = term(478) * (2.0d+0) 
term(479) = term(479) * (-4.0d+0) 
term(480) = term(480) * (-1.0d+0) 
term(481) = term(481) * (2.0d+0) 
term(482) = term(482) * (-2.0d+0) 
term(483) = term(483) * (4.0d+0) 
term(485) = term(485) * (-2.0d+0) 
term(486) = term(486) * (-2.0d+0) 
term(487) = term(487) * (4.0d+0) 
term(488) = term(488) * (4.0d+0) 
term(489) = term(489) * (-8.0d+0) 
term(490) = term(490) * (-2.0d+0) 
term(492) = term(492) * (-2.0d+0) 
term(493) = term(493) * (4.0d+0) 
term(494) = term(494) * (7.999999999999999d+0) 
term(495) = term(495) * (-15.999999999999998d+0) 
term(496) = term(496) * (-7.999999999999999d+0) 
term(497) = term(497) * (15.999999999999998d+0) 
term(498) = term(498) * (-3.9999999999999996d+0) 
term(499) = term(499) * (7.999999999999999d+0) 
term(500) = term(500) * (3.9999999999999996d+0) 
term(501) = term(501) * (-7.999999999999999d+0) 
term(502) = term(502) * (4.0d+0) 
term(503) = term(503) * (-4.0d+0) 
term(504) = term(504) * (-8.0d+0) 
term(505) = term(505) * (8.0d+0) 
term(506) = term(506) * (-8.0d+0) 
term(507) = term(507) * (16.0d+0) 
term(508) = term(508) * (8.0d+0) 
term(509) = term(509) * (-16.0d+0) 
term(510) = term(510) * (8.0d+0) 
term(511) = term(511) * (-8.0d+0) 
term(512) = term(512) * (-16.0d+0) 
term(513) = term(513) * (16.0d+0) 
term(514) = term(514) * (-4.0d+0) 
term(515) = term(515) * (4.0d+0) 
term(516) = term(516) * (8.0d+0) 
term(517) = term(517) * (-8.0d+0) 
term(518) = term(518) * (-8.0d+0) 
term(519) = term(519) * (8.0d+0) 
term(520) = term(520) * (4.0d+0) 
term(521) = term(521) * (-4.0d+0) 
term(522) = term(522) * (-8.0d+0) 
term(523) = term(523) * (8.0d+0) 
term(524) = term(524) * (16.0d+0) 
term(525) = term(525) * (-16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(526) = term(526) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_68_so_pt3(b,a,i,j)
term(527) = term(527) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_69_so_pt3(b,a,i,j)
term(528) = term(528) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_70_so_pt3(b,a,i,j)
term(529) = term(529) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_71_so_pt3(b,a,i,j)
term(530) = term(530) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_72_so_pt3(b,a,i,j)
term(531) = term(531) + t1(b,j) * t2(a,p,q,i) * wm_interm_47_so_pt3(b,a,i,j)
term(532) = term(532) + t1(b,j) * t2(a,p,q,i) * wm_interm_49_so_pt3(b,a,i,j)
term(533) = term(533) + t1(b,j) * t2(a,p,q,i) * wm_interm_51_so_pt3(b,a,i,j)
term(534) = term(534) + t1(a,q) * t2(b,p,j,i) * wm_interm_51_so_pt3(b,a,i,j)
term(535) = term(535) + t1(a,q) * t2(b,p,j,i) * wm_interm_47_so_pt3(b,a,i,j)
term(536) = term(536) + t1(a,q) * t2(b,p,j,i) * wm_interm_49_so_pt3(b,a,i,j)
end do 
end do 
end do 
end do 

term(526) = term(526) * (2.0d+0) 
term(527) = term(527) * (-0.6666666666666666d+0) 
term(528) = term(528) * (-0.6666666666666666d+0) 
term(529) = term(529) * (0.6666666666666666d+0) 
term(530) = term(530) * (0.6666666666666666d+0) 
term(531) = term(531) * (-0.6666666666666666d+0) 
term(532) = term(532) * (0.6666666666666666d+0) 
term(533) = term(533) * (-0.6666666666666666d+0) 
term(534) = term(534) * (-0.6666666666666666d+0) 
term(535) = term(535) * (-0.6666666666666666d+0) 
term(536) = term(536) * (0.6666666666666666d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(537) = term(537) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_68_so_pt3(b,a,j,i)
term(538) = term(538) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_69_so_pt3(b,a,j,i)
term(539) = term(539) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_70_so_pt3(b,a,j,i)
term(540) = term(540) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_71_so_pt3(b,a,j,i)
term(541) = term(541) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_72_so_pt3(b,a,j,i)
term(542) = term(542) + t1(b,j) * t2(a,p,q,i) * wm_interm_49_so_pt3(b,a,j,i)
term(543) = term(543) + t1(b,j) * t2(a,p,q,i) * wm_interm_50_so_pt3(b,a,j,i)
term(544) = term(544) + t1(b,j) * t2(a,p,q,i) * wm_interm_51_so_pt3(b,a,j,i)
term(545) = term(545) + t1(a,q) * t2(b,p,j,i) * wm_interm_50_so_pt3(b,a,j,i)
term(546) = term(546) + t1(a,q) * t2(b,p,j,i) * wm_interm_51_so_pt3(b,a,j,i)
term(547) = term(547) + t1(a,q) * t2(b,p,j,i) * wm_interm_49_so_pt3(b,a,j,i)
end do 
end do 
end do 
end do 

term(537) = term(537) * (-4.0d+0) 
term(538) = term(538) * (1.3333333333333333d+0) 
term(539) = term(539) * (1.3333333333333333d+0) 
term(540) = term(540) * (-0.3333333333333333d+0) 
term(541) = term(541) * (-0.3333333333333333d+0) 
term(542) = term(542) * (-0.3333333333333333d+0) 
term(543) = term(543) * (-0.3333333333333333d+0) 
term(544) = term(544) * (1.3333333333333333d+0) 
term(545) = term(545) * (-0.3333333333333333d+0) 
term(546) = term(546) * (1.3333333333333333d+0) 
term(547) = term(547) * (-0.3333333333333333d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(548) = term(548) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_147_so_pt3(b,c,q,k)
term(549) = term(549) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_132_so_pt3(b,c,q,k)
term(550) = term(550) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_132_so_pt3(a,c,q,k)
term(551) = term(551) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_128_so_pt3(a,c,q,k)
term(552) = term(552) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_129_so_pt3(a,c,q,k)
term(553) = term(553) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_129_so_pt3(b,c,q,k)
term(554) = term(554) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_81_so_pt3(c,a,k,q)
term(555) = term(555) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_83_so_pt3(c,a,k,q)
term(556) = term(556) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_76_so_pt3(c,a,k,q)
term(557) = term(557) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_75_so_pt3(c,a,k,q)
term(558) = term(558) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_79_so_pt3(c,a,k,q)
term(559) = term(559) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_73_so_pt3(c,a,k,q)
term(560) = term(560) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_133_so_pt3(b,c,q,k)
term(561) = term(561) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_133_so_pt3(a,c,q,k)
term(562) = term(562) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_130_so_pt3(b,c,q,k)
term(563) = term(563) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_130_so_pt3(a,c,q,k)
term(564) = term(564) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_131_so_pt3(b,c,q,k)
term(565) = term(565) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_131_so_pt3(a,c,q,k)
term(566) = term(566) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_140_so_pt3(b,c,q,k)
term(567) = term(567) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_139_so_pt3(b,c,q,k)
term(568) = term(568) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_139_so_pt3(a,c,q,k)
term(569) = term(569) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_140_so_pt3(a,c,q,k)
term(570) = term(570) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_110_so_pt3(c,a,k,q)
term(571) = term(571) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_112_so_pt3(c,a,k,q)
term(572) = term(572) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_107_so_pt3(c,a,k,q)
term(573) = term(573) + s2(a,b,i,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_108_so_pt3(c,a,k,q)
term(574) = term(574) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_141_so_pt3(b,c,q,k)
term(575) = term(575) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_141_so_pt3(a,c,q,k)
term(576) = term(576) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_142_so_pt3(b,c,q,k)
term(577) = term(577) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_142_so_pt3(a,c,q,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(548) = term(548) * (0.5d+0) 
term(549) = term(549) * (-1.0d+0) 
term(550) = term(550) * (0.5d+0) 
term(551) = term(551) * (0.5d+0) 
term(552) = term(552) * (-1.0d+0) 
term(553) = term(553) * (0.5d+0) 
term(554) = term(554) * (-2.0d+0) 
term(555) = term(555) * (4.0d+0) 
term(557) = term(557) * (-2.0d+0) 
term(558) = term(558) * (-2.0d+0) 
term(560) = term(560) * (0.5d+0) 
term(561) = term(561) * (-1.0d+0) 
term(562) = term(562) * (0.5d+0) 
term(563) = term(563) * (-1.0d+0) 
term(564) = term(564) * (-1.0d+0) 
term(565) = term(565) * (2.0d+0) 
term(566) = term(566) * (1.9999999999999991d+0) 
term(567) = term(567) * (-2.0d+0) 
term(568) = term(568) * (1.9999999999999991d+0) 
term(569) = term(569) * (-2.0d+0) 
term(570) = term(570) * (-8.0d+0) 
term(571) = term(571) * (8.0d+0) 
term(572) = term(572) * (4.0d+0) 
term(573) = term(573) * (-4.0d+0) 
term(574) = term(574) * (1.9999999999999991d+0) 
term(575) = term(575) * (-3.9999999999999982d+0) 
term(576) = term(576) * (-2.0d+0) 
term(577) = term(577) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(578) = term(578) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_68_so_pt3(a,b,i,j)
term(579) = term(579) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_69_so_pt3(a,b,i,j)
term(580) = term(580) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_71_so_pt3(a,b,i,j)
term(581) = term(581) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_70_so_pt3(a,b,i,j)
term(582) = term(582) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_72_so_pt3(a,b,i,j)
term(583) = term(583) + t1(b,j) * t2(a,p,q,i) * wm_interm_47_so_pt3(a,b,i,j)
term(584) = term(584) + t1(b,j) * t2(a,p,q,i) * wm_interm_49_so_pt3(a,b,i,j)
term(585) = term(585) + t1(b,j) * t2(a,p,q,i) * wm_interm_50_so_pt3(a,b,i,j)
term(586) = term(586) + t1(a,q) * t2(b,p,j,i) * wm_interm_50_so_pt3(a,b,i,j)
term(587) = term(587) + t1(a,q) * t2(b,p,j,i) * wm_interm_47_so_pt3(a,b,i,j)
term(588) = term(588) + t1(a,q) * t2(b,p,j,i) * wm_interm_49_so_pt3(a,b,i,j)
end do 
end do 
end do 
end do 

term(578) = term(578) * (-4.0d+0) 
term(579) = term(579) * (1.3333333333333333d+0) 
term(580) = term(580) * (-0.3333333333333333d+0) 
term(581) = term(581) * (1.3333333333333333d+0) 
term(582) = term(582) * (-0.3333333333333333d+0) 
term(583) = term(583) * (1.3333333333333333d+0) 
term(584) = term(584) * (-0.3333333333333333d+0) 
term(585) = term(585) * (-0.3333333333333333d+0) 
term(586) = term(586) * (-0.3333333333333333d+0) 
term(587) = term(587) * (1.3333333333333333d+0) 
term(588) = term(588) * (-0.3333333333333333d+0) 

do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(589) = term(589) + wm_interm_109_so_pt3(i,j,k,l) * wm_interm_153_so_pt3(p,k,l,j,q,i)
term(590) = term(590) + wm_interm_109_so_pt3(i,j,k,l) * wm_interm_153_so_pt3(p,l,k,j,q,i)
end do 
end do 
end do 
end do 

term(589) = term(589) * (2.0d+0) 
term(590) = term(590) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(591) = term(591) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_81_so_pt3(c,a,k,q)
term(592) = term(592) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_83_so_pt3(c,a,k,q)
term(593) = term(593) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_75_so_pt3(c,a,k,q)
term(594) = term(594) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_132_so_pt3(b,c,q,k)
term(595) = term(595) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_147_so_pt3(a,c,q,k)
term(596) = term(596) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_132_so_pt3(a,c,q,k)
term(597) = term(597) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_81_so_pt3(c,a,i,q)
term(598) = term(598) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_83_so_pt3(c,a,i,q)
term(599) = term(599) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_75_so_pt3(c,a,i,q)
term(600) = term(600) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_74_so_pt3(c,a,k,q)
term(601) = term(601) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_73_so_pt3(c,a,k,q)
term(602) = term(602) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_79_so_pt3(c,a,k,q)
term(603) = term(603) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_129_so_pt3(a,c,q,k)
term(604) = term(604) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_128_so_pt3(b,c,q,k)
term(605) = term(605) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_129_so_pt3(b,c,q,k)
term(606) = term(606) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_74_so_pt3(c,a,i,q)
term(607) = term(607) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_73_so_pt3(c,a,i,q)
term(608) = term(608) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_79_so_pt3(c,a,i,q)
term(609) = term(609) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_147_so_pt3(b,c,q,i)
term(610) = term(610) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_132_so_pt3(b,c,q,i)
term(611) = term(611) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_147_so_pt3(a,c,q,i)
term(612) = term(612) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_132_so_pt3(a,c,q,i)
term(613) = term(613) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_129_so_pt3(b,c,q,i)
term(614) = term(614) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_129_so_pt3(a,c,q,i)
term(615) = term(615) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_133_so_pt3(b,c,q,i)
term(616) = term(616) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_133_so_pt3(b,c,q,k)
term(617) = term(617) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_133_so_pt3(a,c,q,k)
term(618) = term(618) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_133_so_pt3(a,c,q,i)
term(619) = term(619) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_130_so_pt3(b,c,q,i)
term(620) = term(620) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_130_so_pt3(b,c,q,k)
term(621) = term(621) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_130_so_pt3(a,c,q,k)
term(622) = term(622) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_130_so_pt3(a,c,q,i)
term(623) = term(623) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_131_so_pt3(b,c,q,i)
term(624) = term(624) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_131_so_pt3(b,c,q,k)
term(625) = term(625) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_131_so_pt3(a,c,q,k)
term(626) = term(626) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_131_so_pt3(a,c,q,i)
term(627) = term(627) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_110_so_pt3(c,a,k,q)
term(628) = term(628) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_112_so_pt3(c,a,k,q)
term(629) = term(629) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_108_so_pt3(c,a,k,q)
term(630) = term(630) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_139_so_pt3(b,c,q,k)
term(631) = term(631) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_140_so_pt3(a,c,q,k)
term(632) = term(632) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_139_so_pt3(a,c,q,k)
term(633) = term(633) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_110_so_pt3(c,a,i,q)
term(634) = term(634) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_112_so_pt3(c,a,i,q)
term(635) = term(635) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_108_so_pt3(c,a,i,q)
term(636) = term(636) + s2(a,b,j,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_107_so_pt3(c,a,k,q)
term(637) = term(637) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_140_so_pt3(b,c,q,k)
term(638) = term(638) + s2(a,b,j,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_107_so_pt3(c,a,i,q)
term(639) = term(639) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_140_so_pt3(b,c,q,i)
term(640) = term(640) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_139_so_pt3(b,c,q,i)
term(641) = term(641) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_140_so_pt3(a,c,q,i)
term(642) = term(642) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_139_so_pt3(a,c,q,i)
term(643) = term(643) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_141_so_pt3(b,c,q,i)
term(644) = term(644) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_141_so_pt3(b,c,q,k)
term(645) = term(645) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_141_so_pt3(a,c,q,k)
term(646) = term(646) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_141_so_pt3(a,c,q,i)
term(647) = term(647) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_142_so_pt3(b,c,q,i)
term(648) = term(648) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,k,j,i) * wm_interm_142_so_pt3(b,c,q,k)
term(649) = term(649) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_142_so_pt3(a,c,q,k)
term(650) = term(650) + r2(vrdav_Rl, a,j,b,k) * t3(nocc, nactive, b,c,p,k,j,i) * wm_interm_142_so_pt3(a,c,q,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(592) = term(592) * (-2.0d+0) 
term(594) = term(594) * (0.5d+0) 
term(595) = term(595) * (0.5d+0) 
term(596) = term(596) * (-1.0d+0) 
term(597) = term(597) * (-2.0d+0) 
term(598) = term(598) * (4.0d+0) 
term(599) = term(599) * (-2.0d+0) 
term(601) = term(601) * (-2.0d+0) 
term(603) = term(603) * (0.5d+0) 
term(604) = term(604) * (0.5d+0) 
term(605) = term(605) * (-1.0d+0) 
term(606) = term(606) * (-2.0d+0) 
term(607) = term(607) * (4.0d+0) 
term(608) = term(608) * (-2.0d+0) 
term(609) = term(609) * (0.5d+0) 
term(610) = term(610) * (-1.0d+0) 
term(611) = term(611) * (-1.0d+0) 
term(612) = term(612) * (2.0d+0) 
term(613) = term(613) * (0.5d+0) 
term(614) = term(614) * (-1.0d+0) 
term(615) = term(615) * (0.49999999999999994d+0) 
term(616) = term(616) * (-1.0d+0) 
term(617) = term(617) * (0.5d+0) 
term(618) = term(618) * (-0.9999999999999999d+0) 
term(619) = term(619) * (0.49999999999999994d+0) 
term(620) = term(620) * (-1.0d+0) 
term(621) = term(621) * (0.5d+0) 
term(622) = term(622) * (-0.9999999999999999d+0) 
term(623) = term(623) * (-0.9999999999999999d+0) 
term(624) = term(624) * (2.0d+0) 
term(625) = term(625) * (-1.0d+0) 
term(626) = term(626) * (1.9999999999999998d+0) 
term(627) = term(627) * (4.0d+0) 
term(628) = term(628) * (-4.0d+0) 
term(629) = term(629) * (4.0d+0) 
term(630) = term(630) * (1.9999999999999991d+0) 
term(631) = term(631) * (1.9999999999999991d+0) 
term(632) = term(632) * (-2.0d+0) 
term(633) = term(633) * (-8.0d+0) 
term(634) = term(634) * (8.0d+0) 
term(635) = term(635) * (-8.0d+0) 
term(636) = term(636) * (-4.0d+0) 
term(637) = term(637) * (-2.0d+0) 
term(638) = term(638) * (8.0d+0) 
term(639) = term(639) * (1.9999999999999998d+0) 
term(640) = term(640) * (-2.0d+0) 
term(641) = term(641) * (-3.9999999999999996d+0) 
term(642) = term(642) * (4.0d+0) 
term(643) = term(643) * (2.0d+0) 
term(644) = term(644) * (-3.9999999999999982d+0) 
term(645) = term(645) * (1.9999999999999991d+0) 
term(646) = term(646) * (-4.0d+0) 
term(647) = term(647) * (-1.9999999999999998d+0) 
term(648) = term(648) * (4.0d+0) 
term(649) = term(649) * (-2.0d+0) 
term(650) = term(650) * (3.9999999999999996d+0) 

do i = 1, nocc 
term(651) = term(651) + wm_interm_11_so_pt3(i,q) * wm_interm_19_so_pt3(p,i)
term(652) = term(652) + wm_interm_11_so_pt3(i,q) * wm_interm_20_so_pt3(p,i)
term(653) = term(653) + wm_interm_11_so_pt3(i,q) * wm_interm_21_so_pt3(p,i)
term(654) = term(654) + wm_interm_11_so_pt3(i,q) * wm_interm_22_so_pt3(p,i)
term(655) = term(655) + wm_interm_34_so_pt3(p,i) * wm_interm_4_so_pt3(i,q)
term(656) = term(656) + wm_interm_33_so_pt3(p,i) * wm_interm_4_so_pt3(i,q)
term(657) = term(657) + wm_interm_38_so_pt3(p,i) * wm_interm_4_so_pt3(i,q)
term(658) = term(658) + wm_interm_36_so_pt3(p,i) * wm_interm_4_so_pt3(i,q)
term(659) = term(659) + wm_interm_34_so_pt3(p,i) * wm_interm_82_so_pt3(i,q)
term(660) = term(660) + wm_interm_34_so_pt3(p,i) * wm_interm_84_so_pt3(i,q)
term(661) = term(661) + wm_interm_34_so_pt3(p,i) * wm_interm_80_so_pt3(i,q)
term(662) = term(662) + wm_interm_33_so_pt3(p,i) * wm_interm_82_so_pt3(i,q)
term(663) = term(663) + wm_interm_33_so_pt3(p,i) * wm_interm_84_so_pt3(i,q)
term(664) = term(664) + wm_interm_33_so_pt3(p,i) * wm_interm_80_so_pt3(i,q)
term(665) = term(665) + wm_interm_38_so_pt3(p,i) * wm_interm_82_so_pt3(i,q)
term(666) = term(666) + wm_interm_38_so_pt3(p,i) * wm_interm_84_so_pt3(i,q)
term(667) = term(667) + wm_interm_38_so_pt3(p,i) * wm_interm_80_so_pt3(i,q)
term(668) = term(668) + wm_interm_36_so_pt3(p,i) * wm_interm_82_so_pt3(i,q)
term(669) = term(669) + wm_interm_36_so_pt3(p,i) * wm_interm_84_so_pt3(i,q)
term(670) = term(670) + wm_interm_36_so_pt3(p,i) * wm_interm_80_so_pt3(i,q)
term(671) = term(671) + wm_interm_136_so_pt3(p,i) * wm_interm_150_so_pt3(q,i)
term(672) = term(672) + wm_interm_137_so_pt3(p,i) * wm_interm_150_so_pt3(q,i)
term(673) = term(673) + wm_interm_136_so_pt3(p,i) * wm_interm_151_so_pt3(q,i)
term(674) = term(674) + wm_interm_137_so_pt3(p,i) * wm_interm_151_so_pt3(q,i)
term(675) = term(675) + wm_interm_136_so_pt3(p,i) * wm_interm_152_so_pt3(q,i)
term(676) = term(676) + wm_interm_137_so_pt3(p,i) * wm_interm_152_so_pt3(q,i)
term(677) = term(677) + wm_interm_134_so_pt3(p,i) * wm_interm_150_so_pt3(q,i)
term(678) = term(678) + wm_interm_127_so_pt3(p,i) * wm_interm_150_so_pt3(q,i)
term(679) = term(679) + wm_interm_134_so_pt3(p,i) * wm_interm_151_so_pt3(q,i)
term(680) = term(680) + wm_interm_127_so_pt3(p,i) * wm_interm_151_so_pt3(q,i)
term(681) = term(681) + wm_interm_134_so_pt3(p,i) * wm_interm_152_so_pt3(q,i)
term(682) = term(682) + wm_interm_127_so_pt3(p,i) * wm_interm_152_so_pt3(q,i)
term(683) = term(683) + wm_interm_111_so_pt3(i,q) * wm_interm_34_so_pt3(p,i)
term(684) = term(684) + wm_interm_113_so_pt3(i,q) * wm_interm_34_so_pt3(p,i)
term(685) = term(685) + wm_interm_111_so_pt3(i,q) * wm_interm_33_so_pt3(p,i)
term(686) = term(686) + wm_interm_113_so_pt3(i,q) * wm_interm_33_so_pt3(p,i)
term(687) = term(687) + wm_interm_111_so_pt3(i,q) * wm_interm_38_so_pt3(p,i)
term(688) = term(688) + wm_interm_113_so_pt3(i,q) * wm_interm_38_so_pt3(p,i)
term(689) = term(689) + wm_interm_111_so_pt3(i,q) * wm_interm_36_so_pt3(p,i)
term(690) = term(690) + wm_interm_113_so_pt3(i,q) * wm_interm_36_so_pt3(p,i)
term(691) = term(691) + wm_interm_136_so_pt3(p,i) * wm_interm_158_so_pt3(q,i)
term(692) = term(692) + wm_interm_137_so_pt3(p,i) * wm_interm_158_so_pt3(q,i)
term(693) = term(693) + wm_interm_136_so_pt3(p,i) * wm_interm_159_so_pt3(q,i)
term(694) = term(694) + wm_interm_137_so_pt3(p,i) * wm_interm_159_so_pt3(q,i)
term(695) = term(695) + wm_interm_136_so_pt3(p,i) * wm_interm_160_so_pt3(q,i)
term(696) = term(696) + wm_interm_137_so_pt3(p,i) * wm_interm_160_so_pt3(q,i)
term(697) = term(697) + wm_interm_134_so_pt3(p,i) * wm_interm_158_so_pt3(q,i)
term(698) = term(698) + wm_interm_127_so_pt3(p,i) * wm_interm_158_so_pt3(q,i)
term(699) = term(699) + wm_interm_134_so_pt3(p,i) * wm_interm_159_so_pt3(q,i)
term(700) = term(700) + wm_interm_127_so_pt3(p,i) * wm_interm_159_so_pt3(q,i)
term(701) = term(701) + wm_interm_134_so_pt3(p,i) * wm_interm_160_so_pt3(q,i)
term(702) = term(702) + wm_interm_127_so_pt3(p,i) * wm_interm_160_so_pt3(q,i)
end do 

term(651) = term(651) * (-8.0d+0) 
term(652) = term(652) * (4.0d+0) 
term(653) = term(653) * (6.0d+0) 
term(654) = term(654) * (-2.0d+0) 
term(655) = term(655) * (-2.0d+0) 
term(656) = term(656) * (1.9999999999999998d+0) 
term(657) = term(657) * (4.0d+0) 
term(658) = term(658) * (-3.9999999999999996d+0) 
term(660) = term(660) * (-2.0d+0) 
term(662) = term(662) * (-0.9999999999999999d+0) 
term(663) = term(663) * (1.9999999999999998d+0) 
term(664) = term(664) * (-0.9999999999999999d+0) 
term(665) = term(665) * (-2.0d+0) 
term(666) = term(666) * (4.0d+0) 
term(667) = term(667) * (-2.0d+0) 
term(668) = term(668) * (1.9999999999999998d+0) 
term(669) = term(669) * (-3.9999999999999996d+0) 
term(670) = term(670) * (1.9999999999999998d+0) 
term(672) = term(672) * (-2.0d+0) 
term(673) = term(673) * (-2.0d+0) 
term(674) = term(674) * (4.0d+0) 
term(676) = term(676) * (-2.0d+0) 
term(677) = term(677) * (-0.9999999999999999d+0) 
term(678) = term(678) * (1.9999999999999998d+0) 
term(679) = term(679) * (1.9999999999999998d+0) 
term(680) = term(680) * (-3.9999999999999996d+0) 
term(681) = term(681) * (-0.9999999999999999d+0) 
term(682) = term(682) * (1.9999999999999998d+0) 
term(683) = term(683) * (4.0d+0) 
term(684) = term(684) * (-4.0d+0) 
term(685) = term(685) * (-3.9999999999999996d+0) 
term(686) = term(686) * (3.9999999999999996d+0) 
term(687) = term(687) * (-8.0d+0) 
term(688) = term(688) * (8.0d+0) 
term(689) = term(689) * (7.999999999999999d+0) 
term(690) = term(690) * (-7.999999999999999d+0) 
term(691) = term(691) * (2.0d+0) 
term(692) = term(692) * (-4.0d+0) 
term(693) = term(693) * (-4.0d+0) 
term(694) = term(694) * (8.0d+0) 
term(695) = term(695) * (2.0d+0) 
term(696) = term(696) * (-4.0d+0) 
term(697) = term(697) * (-1.9999999999999998d+0) 
term(698) = term(698) * (3.9999999999999996d+0) 
term(699) = term(699) * (3.9999999999999996d+0) 
term(700) = term(700) * (-7.999999999999999d+0) 
term(701) = term(701) * (-1.9999999999999998d+0) 
term(702) = term(702) * (3.9999999999999996d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(703) = term(703) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_125_so_pt3(b,i,k,j)
term(704) = term(704) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_126_so_pt3(b,i,k,j)
term(705) = term(705) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_135_so_pt3(b,i,k,j)
term(706) = term(706) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_138_so_pt3(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(703) = term(703) * (8.0d+0) 
term(704) = term(704) * (-16.0d+0) 
term(705) = term(705) * (4.0d+0) 
term(706) = term(706) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(707) = term(707) + wm_interm_19_so_pt3(a,q) * wm_interm_29_so_pt3(a,p)
term(708) = term(708) + wm_interm_20_so_pt3(a,q) * wm_interm_29_so_pt3(a,p)
term(709) = term(709) + wm_interm_21_so_pt3(a,q) * wm_interm_29_so_pt3(a,p)
term(710) = term(710) + wm_interm_22_so_pt3(a,q) * wm_interm_29_so_pt3(a,p)
term(711) = term(711) + wm_interm_32_so_pt3(a,p) * wm_interm_33_so_pt3(a,q)
term(712) = term(712) + wm_interm_32_so_pt3(a,p) * wm_interm_34_so_pt3(a,q)
term(713) = term(713) + wm_interm_32_so_pt3(a,p) * wm_interm_36_so_pt3(a,q)
term(714) = term(714) + wm_interm_32_so_pt3(a,p) * wm_interm_38_so_pt3(a,q)
term(715) = term(715) + wm_interm_33_so_pt3(a,q) * wm_interm_93_so_pt3(a,p)
term(716) = term(716) + wm_interm_33_so_pt3(a,q) * wm_interm_94_so_pt3(a,p)
term(717) = term(717) + wm_interm_34_so_pt3(a,q) * wm_interm_93_so_pt3(a,p)
term(718) = term(718) + wm_interm_34_so_pt3(a,q) * wm_interm_94_so_pt3(a,p)
term(719) = term(719) + wm_interm_36_so_pt3(a,q) * wm_interm_93_so_pt3(a,p)
term(720) = term(720) + wm_interm_36_so_pt3(a,q) * wm_interm_94_so_pt3(a,p)
term(721) = term(721) + wm_interm_38_so_pt3(a,q) * wm_interm_93_so_pt3(a,p)
term(722) = term(722) + wm_interm_38_so_pt3(a,q) * wm_interm_94_so_pt3(a,p)
term(723) = term(723) + wm_interm_136_so_pt3(a,q) * wm_interm_145_so_pt3(p,a)
term(724) = term(724) + wm_interm_137_so_pt3(a,q) * wm_interm_145_so_pt3(p,a)
term(725) = term(725) + wm_interm_136_so_pt3(a,q) * wm_interm_146_so_pt3(p,a)
term(726) = term(726) + wm_interm_137_so_pt3(a,q) * wm_interm_146_so_pt3(p,a)
term(727) = term(727) + wm_interm_33_so_pt3(a,q) * wm_interm_92_so_pt3(a,p)
term(728) = term(728) + wm_interm_34_so_pt3(a,q) * wm_interm_92_so_pt3(a,p)
term(729) = term(729) + wm_interm_36_so_pt3(a,q) * wm_interm_92_so_pt3(a,p)
term(730) = term(730) + wm_interm_38_so_pt3(a,q) * wm_interm_92_so_pt3(a,p)
term(731) = term(731) + wm_interm_136_so_pt3(a,q) * wm_interm_148_so_pt3(p,a)
term(732) = term(732) + wm_interm_137_so_pt3(a,q) * wm_interm_148_so_pt3(p,a)
term(733) = term(733) + wm_interm_134_so_pt3(a,q) * wm_interm_145_so_pt3(p,a)
term(734) = term(734) + wm_interm_127_so_pt3(a,q) * wm_interm_145_so_pt3(p,a)
term(735) = term(735) + wm_interm_134_so_pt3(a,q) * wm_interm_146_so_pt3(p,a)
term(736) = term(736) + wm_interm_127_so_pt3(a,q) * wm_interm_146_so_pt3(p,a)
term(737) = term(737) + wm_interm_134_so_pt3(a,q) * wm_interm_148_so_pt3(p,a)
term(738) = term(738) + wm_interm_127_so_pt3(a,q) * wm_interm_148_so_pt3(p,a)
term(739) = term(739) + wm_interm_120_so_pt3(a,p) * wm_interm_33_so_pt3(a,q)
term(740) = term(740) + wm_interm_121_so_pt3(a,p) * wm_interm_33_so_pt3(a,q)
term(741) = term(741) + wm_interm_120_so_pt3(a,p) * wm_interm_34_so_pt3(a,q)
term(742) = term(742) + wm_interm_121_so_pt3(a,p) * wm_interm_34_so_pt3(a,q)
term(743) = term(743) + wm_interm_120_so_pt3(a,p) * wm_interm_36_so_pt3(a,q)
term(744) = term(744) + wm_interm_121_so_pt3(a,p) * wm_interm_36_so_pt3(a,q)
term(745) = term(745) + wm_interm_120_so_pt3(a,p) * wm_interm_38_so_pt3(a,q)
term(746) = term(746) + wm_interm_121_so_pt3(a,p) * wm_interm_38_so_pt3(a,q)
term(747) = term(747) + wm_interm_136_so_pt3(a,q) * wm_interm_154_so_pt3(p,a)
term(748) = term(748) + wm_interm_137_so_pt3(a,q) * wm_interm_154_so_pt3(p,a)
term(749) = term(749) + wm_interm_136_so_pt3(a,q) * wm_interm_155_so_pt3(p,a)
term(750) = term(750) + wm_interm_137_so_pt3(a,q) * wm_interm_155_so_pt3(p,a)
term(751) = term(751) + wm_interm_136_so_pt3(a,q) * wm_interm_156_so_pt3(p,a)
term(752) = term(752) + wm_interm_137_so_pt3(a,q) * wm_interm_156_so_pt3(p,a)
term(753) = term(753) + wm_interm_134_so_pt3(a,q) * wm_interm_154_so_pt3(p,a)
term(754) = term(754) + wm_interm_127_so_pt3(a,q) * wm_interm_154_so_pt3(p,a)
term(755) = term(755) + wm_interm_134_so_pt3(a,q) * wm_interm_155_so_pt3(p,a)
term(756) = term(756) + wm_interm_127_so_pt3(a,q) * wm_interm_155_so_pt3(p,a)
term(757) = term(757) + wm_interm_134_so_pt3(a,q) * wm_interm_156_so_pt3(p,a)
term(758) = term(758) + wm_interm_127_so_pt3(a,q) * wm_interm_156_so_pt3(p,a)
end do 

term(707) = term(707) * (-8.0d+0) 
term(708) = term(708) * (4.0d+0) 
term(709) = term(709) * (6.0d+0) 
term(710) = term(710) * (-2.0d+0) 
term(711) = term(711) * (1.9999999999999998d+0) 
term(712) = term(712) * (-1.9999999999999998d+0) 
term(713) = term(713) * (-3.9999999999999996d+0) 
term(714) = term(714) * (4.0d+0) 
term(715) = term(715) * (-0.9999999999999999d+0) 
term(716) = term(716) * (1.9999999999999998d+0) 
term(718) = term(718) * (-1.9999999999999996d+0) 
term(719) = term(719) * (1.9999999999999998d+0) 
term(720) = term(720) * (-3.9999999999999996d+0) 
term(721) = term(721) * (-1.9999999999999998d+0) 
term(722) = term(722) * (3.9999999999999996d+0) 
term(724) = term(724) * (-2.0d+0) 
term(725) = term(725) * (-2.0d+0) 
term(726) = term(726) * (4.0d+0) 
term(727) = term(727) * (-0.9999999999999999d+0) 
term(729) = term(729) * (1.9999999999999998d+0) 
term(730) = term(730) * (-1.9999999999999998d+0) 
term(732) = term(732) * (-2.0d+0) 
term(733) = term(733) * (-0.9999999999999999d+0) 
term(734) = term(734) * (1.9999999999999998d+0) 
term(735) = term(735) * (1.9999999999999998d+0) 
term(736) = term(736) * (-3.9999999999999996d+0) 
term(737) = term(737) * (-0.9999999999999999d+0) 
term(738) = term(738) * (1.9999999999999998d+0) 
term(739) = term(739) * (-3.9999999999999996d+0) 
term(740) = term(740) * (3.9999999999999996d+0) 
term(741) = term(741) * (3.999999999999999d+0) 
term(742) = term(742) * (-3.999999999999999d+0) 
term(743) = term(743) * (7.999999999999999d+0) 
term(744) = term(744) * (-7.999999999999999d+0) 
term(745) = term(745) * (-7.999999999999999d+0) 
term(746) = term(746) * (7.999999999999999d+0) 
term(747) = term(747) * (2.0d+0) 
term(748) = term(748) * (-4.0d+0) 
term(749) = term(749) * (-4.0d+0) 
term(750) = term(750) * (8.0d+0) 
term(751) = term(751) * (2.0d+0) 
term(752) = term(752) * (-4.0d+0) 
term(753) = term(753) * (-1.9999999999999998d+0) 
term(754) = term(754) * (3.9999999999999996d+0) 
term(755) = term(755) * (3.9999999999999996d+0) 
term(756) = term(756) * (-7.999999999999999d+0) 
term(757) = term(757) * (-1.9999999999999998d+0) 
term(758) = term(758) * (3.9999999999999996d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(759) = term(759) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_93_so_pt3(a,b)
term(760) = term(760) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_94_so_pt3(a,b)
term(761) = term(761) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_92_so_pt3(a,b)
term(762) = term(762) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_93_so_pt3(a,b)
term(763) = term(763) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_94_so_pt3(a,b)
term(764) = term(764) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_92_so_pt3(a,b)
term(765) = term(765) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_120_so_pt3(a,b)
term(766) = term(766) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_121_so_pt3(a,b)
term(767) = term(767) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_120_so_pt3(a,b)
term(768) = term(768) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_121_so_pt3(a,b)
end do 
end do 
end do 
end do 
end do 

term(760) = term(760) * (-2.0d+0) 
term(762) = term(762) * (-2.0d+0) 
term(763) = term(763) * (4.0d+0) 
term(764) = term(764) * (-2.0d+0) 
term(765) = term(765) * (4.0d+0) 
term(766) = term(766) * (-4.0d+0) 
term(767) = term(767) * (-8.0d+0) 
term(768) = term(768) * (8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(769) = term(769) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,k,j) * wm_interm_125_so_pt3(b,i,k,j)
term(770) = term(770) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,k,j) * wm_interm_126_so_pt3(b,i,k,j)
term(771) = term(771) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,k,j) * wm_interm_135_so_pt3(b,i,k,j)
term(772) = term(772) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,k,j) * wm_interm_138_so_pt3(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(769) = term(769) * (-2.0d+0) 
term(770) = term(770) * (4.0d+0) 
term(771) = term(771) * (-1.0d+0) 
term(772) = term(772) * (0.5d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(773) = term(773) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_148_so_pt3(c,a)
term(774) = term(774) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_145_so_pt3(c,a)
term(775) = term(775) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_146_so_pt3(c,a)
term(776) = term(776) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_154_so_pt3(c,a)
term(777) = term(777) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_155_so_pt3(c,a)
end do 
end do 
end do 
end do 
end do 

term(773) = term(773) * (0.5d+0) 
term(774) = term(774) * (0.5d+0) 
term(775) = term(775) * (-1.0d+0) 
term(776) = term(776) * (1.9999999999999993d+0) 
term(777) = term(777) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(778) = term(778) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_148_so_pt3(c,a)
term(779) = term(779) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_145_so_pt3(c,a)
term(780) = term(780) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_146_so_pt3(c,a)
term(781) = term(781) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_154_so_pt3(c,a)
term(782) = term(782) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_155_so_pt3(c,a)
end do 
end do 
end do 
end do 
end do 

term(778) = term(778) * (-1.0d+0) 
term(779) = term(779) * (-1.0d+0) 
term(780) = term(780) * (2.0d+0) 
term(781) = term(781) * (-3.9999999999999987d+0) 
term(782) = term(782) * (4.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(783) = term(783) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_148_so_pt3(b,a)
term(784) = term(784) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_148_so_pt3(b,a)
term(785) = term(785) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_145_so_pt3(b,a)
term(786) = term(786) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_145_so_pt3(b,a)
term(787) = term(787) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_146_so_pt3(b,a)
term(788) = term(788) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_146_so_pt3(b,a)
term(789) = term(789) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_154_so_pt3(b,a)
term(790) = term(790) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_154_so_pt3(b,a)
term(791) = term(791) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_155_so_pt3(b,a)
term(792) = term(792) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_155_so_pt3(b,a)
end do 
end do 
end do 
end do 
end do 

term(783) = term(783) * (0.5d+0) 
term(784) = term(784) * (-1.0d+0) 
term(785) = term(785) * (0.5d+0) 
term(786) = term(786) * (-1.0d+0) 
term(787) = term(787) * (-1.0d+0) 
term(788) = term(788) * (2.0d+0) 
term(789) = term(789) * (1.9999999999999993d+0) 
term(790) = term(790) * (-3.9999999999999987d+0) 
term(791) = term(791) * (-2.0d+0) 
term(792) = term(792) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(793) = term(793) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_68_so_pt3(a,b,j,i)
term(794) = term(794) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_69_so_pt3(a,b,j,i)
term(795) = term(795) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_71_so_pt3(a,b,j,i)
term(796) = term(796) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_70_so_pt3(a,b,j,i)
term(797) = term(797) + r1(vrdav_Rr, a,q) * t2(b,p,j,i) * wm_interm_72_so_pt3(a,b,j,i)
term(798) = term(798) + t1(b,j) * t2(a,p,q,i) * wm_interm_47_so_pt3(a,b,j,i)
term(799) = term(799) + t1(b,j) * t2(a,p,q,i) * wm_interm_50_so_pt3(a,b,j,i)
term(800) = term(800) + t1(b,j) * t2(a,p,q,i) * wm_interm_51_so_pt3(a,b,j,i)
term(801) = term(801) + t1(a,q) * t2(b,p,j,i) * wm_interm_51_so_pt3(a,b,j,i)
term(802) = term(802) + t1(a,q) * t2(b,p,j,i) * wm_interm_47_so_pt3(a,b,j,i)
term(803) = term(803) + t1(a,q) * t2(b,p,j,i) * wm_interm_50_so_pt3(a,b,j,i)
end do 
end do 
end do 
end do 

term(793) = term(793) * (2.0d+0) 
term(794) = term(794) * (-0.6666666666666666d+0) 
term(795) = term(795) * (0.6666666666666666d+0) 
term(796) = term(796) * (-0.6666666666666666d+0) 
term(797) = term(797) * (0.6666666666666666d+0) 
term(798) = term(798) * (-0.6666666666666666d+0) 
term(799) = term(799) * (0.6666666666666666d+0) 
term(800) = term(800) * (-0.6666666666666666d+0) 
term(801) = term(801) * (-0.6666666666666666d+0) 
term(802) = term(802) * (-0.6666666666666666d+0) 
term(803) = term(803) * (0.6666666666666666d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(804) = term(804) + r1(vrdav_Rl, b,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_10_so_pt3(a,k,i,j)
term(805) = term(805) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_10_so_pt3(b,k,i,j)
term(806) = term(806) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_10_so_pt3(b,i,k,j)
term(807) = term(807) + r1(vrdav_Rl, b,j) * r3(vrdav_Rr, a,q,b,k,p,i) * wm_interm_10_so_pt3(a,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(804) = term(804) * (-6.0d+0) 
term(805) = term(805) * (8.0d+0) 
term(806) = term(806) * (-6.0d+0) 
term(807) = term(807) * (8.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(808) = term(808) + wm_interm_109_so_pt3(i,j,k,l) * wm_interm_153_so_pt3(p,l,k,i,q,j)
term(809) = term(809) + wm_interm_109_so_pt3(i,j,k,l) * wm_interm_153_so_pt3(p,k,l,i,q,j)
end do 
end do 
end do 
end do 

term(808) = term(808) * (2.0d+0) 
term(809) = term(809) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(810) = term(810) + wm_interm_111_so_pt3(i,j) * wm_interm_44_so_pt3(p,j,q,i)
term(811) = term(811) + wm_interm_113_so_pt3(i,j) * wm_interm_44_so_pt3(p,j,q,i)
term(812) = term(812) + wm_interm_111_so_pt3(i,j) * wm_interm_39_so_pt3(p,j,i,q)
term(813) = term(813) + wm_interm_113_so_pt3(i,j) * wm_interm_39_so_pt3(p,j,i,q)
term(814) = term(814) + wm_interm_111_so_pt3(i,j) * wm_interm_45_so_pt3(p,j,q,i)
term(815) = term(815) + wm_interm_113_so_pt3(i,j) * wm_interm_45_so_pt3(p,j,q,i)
end do 
end do 

term(810) = term(810) * (4.0d+0) 
term(811) = term(811) * (-4.0d+0) 
term(812) = term(812) * (4.0d+0) 
term(813) = term(813) * (-4.0d+0) 
term(814) = term(814) * (-7.999999999999999d+0) 
term(815) = term(815) * (7.999999999999999d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(816) = term(816) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_41_so_pt3(b,j,k,i)
term(817) = term(817) + r1(vrdav_Rl, a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_41_so_pt3(b,j,i,k)
term(818) = term(818) + r1(vrdav_Rl, b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_41_so_pt3(a,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(816) = term(816) * (-2.0d+0) 
term(817) = term(817) * (4.0d+0) 
term(818) = term(818) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(819) = term(819) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,q) * wm_interm_31_so_pt3(a,k,i,j)
term(820) = term(820) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,q) * wm_interm_35_so_pt3(a,k,i,j)
term(821) = term(821) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,q) * wm_interm_37_so_pt3(a,k,i,j)
term(822) = term(822) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,q) * wm_interm_39_so_pt3(a,k,i,j)
term(823) = term(823) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_31_so_pt3(a,k,i,j)
term(824) = term(824) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_35_so_pt3(a,k,i,j)
term(825) = term(825) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_31_so_pt3(b,k,i,j)
term(826) = term(826) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_35_so_pt3(b,k,i,j)
term(827) = term(827) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_37_so_pt3(a,k,i,j)
term(828) = term(828) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_39_so_pt3(a,k,i,j)
term(829) = term(829) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_39_so_pt3(b,k,i,j)
term(830) = term(830) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_37_so_pt3(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(819) = term(819) * (-1.0d+0) 
term(820) = term(820) * (2.0d+0) 
term(821) = term(821) * (0.5d+0) 
term(822) = term(822) * (-1.0d+0) 
term(823) = term(823) * (4.0d+0) 
term(824) = term(824) * (-8.0d+0) 
term(825) = term(825) * (-2.0d+0) 
term(826) = term(826) * (4.0d+0) 
term(827) = term(827) * (-2.0d+0) 
term(828) = term(828) * (4.0d+0) 
term(829) = term(829) * (-2.0d+0) 
term(830) = term(830) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(831) = term(831) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_31_so_pt3(a,k,i,j)
term(832) = term(832) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_35_so_pt3(a,k,i,j)
term(833) = term(833) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,k) * wm_interm_31_so_pt3(a,k,i,j)
term(834) = term(834) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,k) * wm_interm_35_so_pt3(a,k,i,j)
term(835) = term(835) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,k) * wm_interm_31_so_pt3(b,k,i,j)
term(836) = term(836) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,k) * wm_interm_35_so_pt3(b,k,i,j)
term(837) = term(837) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_37_so_pt3(a,k,i,j)
term(838) = term(838) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_39_so_pt3(a,k,i,j)
term(839) = term(839) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,k) * wm_interm_37_so_pt3(a,k,i,j)
term(840) = term(840) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,k) * wm_interm_39_so_pt3(a,k,i,j)
term(841) = term(841) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,k) * wm_interm_39_so_pt3(b,k,i,j)
term(842) = term(842) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,k) * wm_interm_37_so_pt3(b,k,i,j)
term(843) = term(843) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_31_so_pt3(a,k,i,j)
term(844) = term(844) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_35_so_pt3(a,k,i,j)
term(845) = term(845) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_31_so_pt3(b,k,i,j)
term(846) = term(846) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_35_so_pt3(b,k,i,j)
term(847) = term(847) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_125_so_pt3(b,i,j,k)
term(848) = term(848) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_126_so_pt3(b,i,j,k)
term(849) = term(849) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_135_so_pt3(b,i,j,k)
term(850) = term(850) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_37_so_pt3(a,k,i,j)
term(851) = term(851) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_39_so_pt3(a,k,i,j)
term(852) = term(852) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_39_so_pt3(b,k,i,j)
term(853) = term(853) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_37_so_pt3(b,k,i,j)
term(854) = term(854) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,k,j) * wm_interm_138_so_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(831) = term(831) * (2.0d+0) 
term(832) = term(832) * (-4.0d+0) 
term(833) = term(833) * (-1.0d+0) 
term(834) = term(834) * (2.0d+0) 
term(835) = term(835) * (0.5d+0) 
term(836) = term(836) * (-1.0d+0) 
term(837) = term(837) * (-1.0d+0) 
term(838) = term(838) * (2.0d+0) 
term(839) = term(839) * (0.5d+0) 
term(840) = term(840) * (-1.0d+0) 
term(841) = term(841) * (0.5d+0) 
term(842) = term(842) * (-1.0d+0) 
term(843) = term(843) * (-3.999999999999999d+0) 
term(844) = term(844) * (8.0d+0) 
term(845) = term(845) * (1.9999999999999996d+0) 
term(846) = term(846) * (-4.0d+0) 
term(847) = term(847) * (-4.0d+0) 
term(848) = term(848) * (8.0d+0) 
term(849) = term(849) * (-2.0d+0) 
term(850) = term(850) * (1.9999999999999996d+0) 
term(851) = term(851) * (-3.999999999999999d+0) 
term(852) = term(852) * (1.9999999999999996d+0) 
term(853) = term(853) * (-3.999999999999999d+0) 
term(854) = term(854) * (4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(855) = term(855) + wm_interm_109_so_pt3(i,j,k,q) * wm_interm_44_so_pt3(p,k,i,j)
term(856) = term(856) + wm_interm_109_so_pt3(i,j,k,q) * wm_interm_39_so_pt3(p,k,i,j)
term(857) = term(857) + wm_interm_109_so_pt3(i,j,k,q) * wm_interm_37_so_pt3(p,k,i,j)
term(858) = term(858) + wm_interm_109_so_pt3(i,j,k,q) * wm_interm_45_so_pt3(p,k,i,j)
end do 
end do 
end do 

term(855) = term(855) * (-2.0d+0) 
term(856) = term(856) * (1.9999999999999996d+0) 
term(857) = term(857) * (-2.0d+0) 
term(858) = term(858) * (3.9999999999999996d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(859) = term(859) + wm_interm_109_so_pt3(i,j,k,q) * wm_interm_44_so_pt3(p,k,j,i)
term(860) = term(860) + wm_interm_109_so_pt3(i,j,k,q) * wm_interm_45_so_pt3(p,k,j,i)
end do 
end do 
end do 

term(859) = term(859) * (1.9999999999999996d+0) 
term(860) = term(860) * (-4.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(861) = term(861) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,q,l,k)
term(862) = term(862) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,q,l,k)
term(863) = term(863) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,l,q,k)
term(864) = term(864) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,l,q,k)
term(865) = term(865) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,l,k,q)
term(866) = term(866) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,l,k,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(861) = term(861) * (-2.0d+0) 
term(862) = term(862) * (4.0d+0) 
term(863) = term(863) * (-2.0d+0) 
term(864) = term(864) * (4.0d+0) 
term(866) = term(866) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(867) = term(867) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,k,p,q) * wm_interm_10_so_pt3(b,j,k,i)
term(868) = term(868) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,k,p,q) * wm_interm_10_so_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(867) = term(867) * (4.0d+0) 
term(868) = term(868) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(869) = term(869) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,k,j) * wm_interm_125_so_pt3(b,i,j,k)
term(870) = term(870) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,k,j) * wm_interm_126_so_pt3(b,i,j,k)
term(871) = term(871) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_31_so_pt3(b,k,j,i)
term(872) = term(872) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_35_so_pt3(b,k,j,i)
term(873) = term(873) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_125_so_pt3(b,i,j,k)
term(874) = term(874) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_126_so_pt3(b,i,j,k)
term(875) = term(875) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,k) * wm_interm_31_so_pt3(b,k,j,i)
term(876) = term(876) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,k) * wm_interm_35_so_pt3(b,k,j,i)
term(877) = term(877) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_31_so_pt3(a,k,j,i)
term(878) = term(878) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_35_so_pt3(a,k,j,i)
term(879) = term(879) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,k) * wm_interm_31_so_pt3(a,k,j,i)
term(880) = term(880) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,k) * wm_interm_35_so_pt3(a,k,j,i)
term(881) = term(881) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,k,j) * wm_interm_135_so_pt3(b,i,j,k)
term(882) = term(882) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_135_so_pt3(b,i,j,k)
term(883) = term(883) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,k,j) * wm_interm_138_so_pt3(b,i,j,k)
term(884) = term(884) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_138_so_pt3(b,i,j,k)
term(885) = term(885) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_125_so_pt3(b,i,j,k)
term(886) = term(886) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_126_so_pt3(b,i,j,k)
term(887) = term(887) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_31_so_pt3(b,k,j,i)
term(888) = term(888) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_35_so_pt3(b,k,j,i)
term(889) = term(889) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_31_so_pt3(a,k,j,i)
term(890) = term(890) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_35_so_pt3(a,k,j,i)
term(891) = term(891) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_135_so_pt3(b,i,j,k)
term(892) = term(892) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,k,j) * wm_interm_138_so_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(869) = term(869) * (-2.0d+0) 
term(870) = term(870) * (4.0d+0) 
term(871) = term(871) * (2.0d+0) 
term(872) = term(872) * (-4.0d+0) 
term(874) = term(874) * (-2.0d+0) 
term(875) = term(875) * (-1.0d+0) 
term(876) = term(876) * (2.0d+0) 
term(877) = term(877) * (-1.0d+0) 
term(878) = term(878) * (2.0d+0) 
term(879) = term(879) * (0.5d+0) 
term(880) = term(880) * (-1.0d+0) 
term(881) = term(881) * (-1.0d+0) 
term(882) = term(882) * (0.5d+0) 
term(883) = term(883) * (2.0d+0) 
term(884) = term(884) * (-1.0d+0) 
term(885) = term(885) * (4.0d+0) 
term(886) = term(886) * (-8.0d+0) 
term(887) = term(887) * (-3.999999999999999d+0) 
term(888) = term(888) * (7.999999999999999d+0) 
term(889) = term(889) * (1.9999999999999996d+0) 
term(890) = term(890) * (-4.0d+0) 
term(891) = term(891) * (2.0d+0) 
term(892) = term(892) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(893) = term(893) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_31_so_pt3(b,k,i,j)
term(894) = term(894) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_35_so_pt3(b,k,i,j)
term(895) = term(895) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,k,j) * wm_interm_125_so_pt3(b,i,j,k)
term(896) = term(896) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,k,j) * wm_interm_126_so_pt3(b,i,j,k)
term(897) = term(897) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,k,j) * wm_interm_135_so_pt3(b,i,j,k)
term(898) = term(898) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_39_so_pt3(b,k,i,j)
term(899) = term(899) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_37_so_pt3(b,k,i,j)
term(900) = term(900) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,k,j) * wm_interm_138_so_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(893) = term(893) * (-1.0d+0) 
term(894) = term(894) * (2.0d+0) 
term(896) = term(896) * (-2.0d+0) 
term(897) = term(897) * (0.5d+0) 
term(898) = term(898) * (-1.0d+0) 
term(899) = term(899) * (2.0d+0) 
term(900) = term(900) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(901) = term(901) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,k,j) * wm_interm_125_so_pt3(b,i,k,j)
term(902) = term(902) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,k,j) * wm_interm_126_so_pt3(b,i,k,j)
term(903) = term(903) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,q) * wm_interm_31_so_pt3(b,k,j,i)
term(904) = term(904) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,q) * wm_interm_35_so_pt3(b,k,j,i)
term(905) = term(905) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,k,j) * wm_interm_135_so_pt3(b,i,k,j)
term(906) = term(906) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,k,j) * wm_interm_138_so_pt3(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(901) = term(901) * (4.0d+0) 
term(902) = term(902) * (-8.0d+0) 
term(903) = term(903) * (-1.0d+0) 
term(904) = term(904) * (2.0d+0) 
term(905) = term(905) * (2.0d+0) 
term(906) = term(906) * (-1.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(907) = term(907) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_148_so_pt3(b,c)
term(908) = term(908) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_145_so_pt3(b,c)
term(909) = term(909) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_146_so_pt3(b,c)
term(910) = term(910) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_93_so_pt3(c,b)
term(911) = term(911) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_94_so_pt3(c,b)
term(912) = term(912) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_92_so_pt3(c,b)
term(913) = term(913) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_154_so_pt3(b,c)
term(914) = term(914) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_155_so_pt3(b,c)
term(915) = term(915) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_120_so_pt3(c,b)
term(916) = term(916) + s2(a,b,j,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_121_so_pt3(c,b)
end do 
end do 
end do 
end do 
end do 

term(909) = term(909) * (-2.0d+0) 
term(911) = term(911) * (-2.0d+0) 
term(913) = term(913) * (3.9999999999999982d+0) 
term(914) = term(914) * (-4.0d+0) 
term(915) = term(915) * (4.0d+0) 
term(916) = term(916) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(917) = term(917) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,q,k,l)
term(918) = term(918) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,q,k,l)
term(919) = term(919) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,k,q,l)
term(920) = term(920) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,k,q,l)
term(921) = term(921) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,k,l,q)
term(922) = term(922) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,k,l,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(917) = term(917) * (0.5d+0) 
term(918) = term(918) * (-1.0d+0) 
term(919) = term(919) * (0.5d+0) 
term(920) = term(920) * (-1.0d+0) 
term(921) = term(921) * (0.5d+0) 
term(922) = term(922) * (-1.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(923) = term(923) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_81_so_pt3(a,b,k,j)
term(924) = term(924) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_83_so_pt3(a,b,k,j)
term(925) = term(925) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_75_so_pt3(a,b,k,j)
term(926) = term(926) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_76_so_pt3(a,b,k,j)
term(927) = term(927) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_74_so_pt3(a,b,k,j)
term(928) = term(928) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_79_so_pt3(a,b,k,j)
term(929) = term(929) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_76_so_pt3(a,b,k,j)
term(930) = term(930) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_73_so_pt3(a,b,k,j)
term(931) = term(931) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_74_so_pt3(a,b,k,j)
term(932) = term(932) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_110_so_pt3(a,b,k,j)
term(933) = term(933) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_112_so_pt3(a,b,k,j)
term(934) = term(934) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_108_so_pt3(a,b,k,j)
term(935) = term(935) + s2(b,c,i,j) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_107_so_pt3(a,b,k,j)
term(936) = term(936) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_107_so_pt3(a,b,k,j)
term(937) = term(937) + s2(b,c,j,i) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_108_so_pt3(a,b,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(924) = term(924) * (-2.0d+0) 
term(926) = term(926) * (-2.0d+0) 
term(931) = term(931) * (-1.9999999999999998d+0) 
term(932) = term(932) * (4.0d+0) 
term(933) = term(933) * (-4.0d+0) 
term(934) = term(934) * (4.0d+0) 
term(935) = term(935) * (-4.0d+0) 
term(936) = term(936) * (3.9999999999999996d+0) 
term(937) = term(937) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(938) = term(938) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,l) * wm_interm_144_so_pt3(a,l,k,q,i,j)
term(939) = term(939) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,l) * wm_interm_144_so_pt3(a,k,l,q,i,j)
term(940) = term(940) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_144_so_pt3(b,l,k,q,i,j)
term(941) = term(941) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,p,l) * wm_interm_144_so_pt3(a,k,l,q,i,j)
term(942) = term(942) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,l) * wm_interm_144_so_pt3(a,k,l,q,i,j)
term(943) = term(943) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,l) * wm_interm_144_so_pt3(a,l,k,q,i,j)
term(944) = term(944) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_144_so_pt3(b,l,k,q,i,j)
term(945) = term(945) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_144_so_pt3(b,k,l,q,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(938) = term(938) * (0.49999999999999994d+0) 
term(939) = term(939) * (-0.9999999999999999d+0) 
term(940) = term(940) * (0.49999999999999994d+0) 
term(941) = term(941) * (0.49999999999999994d+0) 
term(942) = term(942) * (2.0d+0) 
term(943) = term(943) * (-1.9999999999999998d+0) 
term(944) = term(944) * (2.0d+0) 
term(945) = term(945) * (-1.9999999999999998d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(946) = term(946) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_144_so_pt3(b,k,l,q,i,j)
term(947) = term(947) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_144_so_pt3(b,l,k,q,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(946) = term(946) * (0.49999999999999994d+0) 
term(947) = term(947) * (-0.9999999999999999d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(948) = term(948) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_76_so_pt3(a,b,i,k)
term(949) = term(949) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_73_so_pt3(a,b,i,k)
term(950) = term(950) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_74_so_pt3(a,b,i,k)
term(951) = term(951) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_76_so_pt3(a,b,i,j)
term(952) = term(952) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_73_so_pt3(a,b,i,j)
term(953) = term(953) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_74_so_pt3(a,b,i,j)
term(954) = term(954) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_81_so_pt3(c,b,i,k)
term(955) = term(955) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_83_so_pt3(c,b,i,k)
term(956) = term(956) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_75_so_pt3(c,b,i,k)
term(957) = term(957) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_74_so_pt3(c,b,i,k)
term(958) = term(958) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_73_so_pt3(c,b,i,k)
term(959) = term(959) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_79_so_pt3(c,b,i,k)
term(960) = term(960) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_107_so_pt3(a,b,i,k)
term(961) = term(961) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_108_so_pt3(a,b,i,k)
term(962) = term(962) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_107_so_pt3(a,b,i,j)
term(963) = term(963) + s2(b,c,j,k) * t3(nocc, nactive, a,c,p,q,k,i) * wm_interm_108_so_pt3(a,b,i,j)
term(964) = term(964) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_110_so_pt3(c,b,i,k)
term(965) = term(965) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_112_so_pt3(c,b,i,k)
term(966) = term(966) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_108_so_pt3(c,b,i,k)
term(967) = term(967) + s2(a,b,k,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_107_so_pt3(c,b,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(950) = term(950) * (-2.0d+0) 
term(951) = term(951) * (-2.0d+0) 
term(952) = term(952) * (-2.0d+0) 
term(953) = term(953) * (4.0d+0) 
term(955) = term(955) * (-2.0000000000000004d+0) 
term(958) = term(958) * (-2.0000000000000004d+0) 
term(960) = term(960) * (4.0d+0) 
term(961) = term(961) * (-4.0d+0) 
term(962) = term(962) * (-8.0d+0) 
term(963) = term(963) * (8.0d+0) 
term(964) = term(964) * (4.000000000000001d+0) 
term(965) = term(965) * (-4.000000000000001d+0) 
term(966) = term(966) * (4.000000000000001d+0) 
term(967) = term(967) * (-4.000000000000001d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(968) = term(968) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,q,l,k)
term(969) = term(969) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,q,l,k)
term(970) = term(970) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,l,q,k)
term(971) = term(971) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,l,q,k)
term(972) = term(972) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,i,j,l,k,q)
term(973) = term(973) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,l,k) * wm_interm_143_so_pt3(b,j,i,l,k,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(968) = term(968) * (-1.0d+0) 
term(969) = term(969) * (2.0d+0) 
term(970) = term(970) * (-1.0d+0) 
term(971) = term(971) * (2.0d+0) 
term(972) = term(972) * (0.5d+0) 
term(973) = term(973) * (-1.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(974) = term(974) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,l) * wm_interm_144_so_pt3(a,k,l,j,i,q)
term(975) = term(975) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,l) * wm_interm_144_so_pt3(a,l,k,j,i,q)
term(976) = term(976) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,p,l) * wm_interm_144_so_pt3(a,l,k,j,i,q)
term(977) = term(977) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_144_so_pt3(b,l,k,j,i,q)
term(978) = term(978) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,l) * wm_interm_144_so_pt3(a,l,k,j,i,q)
term(979) = term(979) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,l) * wm_interm_144_so_pt3(a,k,l,j,i,q)
term(980) = term(980) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_144_so_pt3(b,l,k,j,i,q)
term(981) = term(981) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_144_so_pt3(b,k,l,j,i,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(974) = term(974) * (0.5d+0) 
term(975) = term(975) * (-1.0d+0) 
term(976) = term(976) * (0.5d+0) 
term(977) = term(977) * (-1.0d+0) 
term(978) = term(978) * (1.9999999999999998d+0) 
term(979) = term(979) * (-2.0d+0) 
term(980) = term(980) * (-3.9999999999999996d+0) 
term(981) = term(981) * (4.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(982) = term(982) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_144_so_pt3(b,k,l,j,i,q)
term(983) = term(983) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_144_so_pt3(b,l,k,j,i,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(982) = term(982) * (-1.0d+0) 
term(983) = term(983) * (2.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(984) = term(984) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,l) * wm_interm_144_so_pt3(a,k,l,i,j,q)
term(985) = term(985) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,l) * wm_interm_144_so_pt3(a,l,k,i,j,q)
term(986) = term(986) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,p,l) * wm_interm_144_so_pt3(a,l,k,i,j,q)
term(987) = term(987) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_144_so_pt3(b,l,k,i,j,q)
term(988) = term(988) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,l) * wm_interm_144_so_pt3(a,l,k,i,j,q)
term(989) = term(989) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,l) * wm_interm_144_so_pt3(a,k,l,i,j,q)
term(990) = term(990) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_144_so_pt3(b,l,k,i,j,q)
term(991) = term(991) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_144_so_pt3(b,k,l,i,j,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(984) = term(984) * (-1.0d+0) 
term(985) = term(985) * (2.0d+0) 
term(986) = term(986) * (-1.0d+0) 
term(987) = term(987) * (0.5d+0) 
term(988) = term(988) * (-3.9999999999999996d+0) 
term(989) = term(989) * (4.0d+0) 
term(990) = term(990) * (1.9999999999999998d+0) 
term(991) = term(991) * (-2.0d+0) 


    calc_D_vo_wm_so_cc3_pt3 = zero
    do s = 0, 991
    calc_D_vo_wm_so_cc3_pt3 = calc_D_vo_wm_so_cc3_pt3 + term(s)
    end do

    end function calc_D_vo_wm_so_cc3_pt3
    
    function calc_D_vv_wm_so_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_cc3_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, b, a, l 
    real(F64), dimension(0:737) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_162_so_pt3(p,i,j,k) * wm_interm_2_so_pt3(q,j,k,i)
term(1) = term(1) + wm_interm_162_so_pt3(p,i,j,k) * wm_interm_2_so_pt3(q,k,j,i)
term(2) = term(2) + wm_interm_162_so_pt3(p,i,j,k) * wm_interm_1_so_pt3(q,k,j,i)
term(3) = term(3) + wm_interm_162_so_pt3(p,i,j,k) * wm_interm_1_so_pt3(q,j,k,i)
term(4) = term(4) + wm_interm_162_so_pt3(p,i,j,k) * wm_interm_3_so_pt3(q,k,j,i)
term(5) = term(5) + wm_interm_162_so_pt3(p,i,j,k) * wm_interm_3_so_pt3(q,j,k,i)
term(6) = term(6) + wm_interm_161_so_pt3(p,i,j,k) * wm_interm_1_so_pt3(q,j,k,i)
term(7) = term(7) + wm_interm_161_so_pt3(p,i,j,k) * wm_interm_2_so_pt3(q,k,j,i)
term(8) = term(8) + wm_interm_161_so_pt3(p,i,j,k) * wm_interm_3_so_pt3(q,j,k,i)
term(9) = term(9) + wm_interm_163_so_pt3(p,i,j,k) * wm_interm_2_so_pt3(q,k,j,i)
term(10) = term(10) + wm_interm_163_so_pt3(p,i,j,k) * wm_interm_2_so_pt3(q,j,k,i)
term(11) = term(11) + wm_interm_163_so_pt3(p,i,j,k) * wm_interm_1_so_pt3(q,j,k,i)
term(12) = term(12) + wm_interm_163_so_pt3(p,i,j,k) * wm_interm_1_so_pt3(q,k,j,i)
term(13) = term(13) + wm_interm_163_so_pt3(p,i,j,k) * wm_interm_3_so_pt3(q,j,k,i)
term(14) = term(14) + wm_interm_163_so_pt3(p,i,j,k) * wm_interm_3_so_pt3(q,k,j,i)
term(15) = term(15) + wm_interm_10_so_pt3(q,i,j,k) * wm_interm_173_so_pt3(p,k,j,i)
term(16) = term(16) + wm_interm_10_so_pt3(q,i,j,k) * wm_interm_174_so_pt3(p,k,j,i)
term(17) = term(17) + wm_interm_10_so_pt3(q,i,j,k) * wm_interm_175_so_pt3(p,k,j,i)
term(18) = term(18) + wm_interm_10_so_pt3(q,i,j,k) * wm_interm_178_so_pt3(p,k,j,i)
term(19) = term(19) + wm_interm_180_so_pt3(q,i,j,k) * wm_interm_44_so_pt3(p,k,j,i)
term(20) = term(20) + wm_interm_180_so_pt3(q,i,j,k) * wm_interm_45_so_pt3(p,k,j,i)
term(21) = term(21) + wm_interm_196_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,j,i)
term(22) = term(22) + wm_interm_195_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,j,i)
term(23) = term(23) + wm_interm_201_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,j,i)
term(24) = term(24) + wm_interm_192_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,j,i)
term(25) = term(25) + wm_interm_193_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,j,i)
term(26) = term(26) + wm_interm_194_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,j,i)
term(27) = term(27) + wm_interm_213_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,j,i)
term(28) = term(28) + wm_interm_212_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,j,i)
term(29) = term(29) + wm_interm_210_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,j,i)
term(30) = term(30) + wm_interm_211_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (1.9999999999999998d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (-0.9999999999999999d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (3.9999999999999996d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (8.0d+0) 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * (-3.0d+0) 
term(16) = term(16) * (4.0d+0) 
term(17) = term(17) * (-8.0d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-1.0d+0) 
term(20) = term(20) * (1.9999999999999998d+0) 
term(21) = term(21) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * (2.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(31) = term(31) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,j,k) * wm_interm_5_so_pt3(b,i,l,k)
term(32) = term(32) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,i,k) * wm_interm_5_so_pt3(b,j,l,k)
term(33) = term(33) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,i,k) * wm_interm_5_so_pt3(b,j,k,l)
term(34) = term(34) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,j,k) * wm_interm_5_so_pt3(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(31) = term(31) * (-1.9999999999999998d+0) 
term(32) = term(32) * (3.9999999999999996d+0) 
term(33) = term(33) * (-1.9999999999999998d+0) 
term(34) = term(34) * (3.9999999999999996d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(35) = term(35) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,j) * wm_interm_170_so_pt3(a,k)
term(36) = term(36) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,j) * wm_interm_170_so_pt3(a,i)
term(37) = term(37) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,j) * wm_interm_171_so_pt3(a,k)
term(38) = term(38) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,j) * wm_interm_171_so_pt3(a,i)
term(39) = term(39) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,a,j) * wm_interm_48_so_pt3(b,k)
term(40) = term(40) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,p,j) * wm_interm_48_so_pt3(b,k)
term(41) = term(41) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_48_so_pt3(b,i)
term(42) = term(42) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,p,j) * wm_interm_48_so_pt3(b,i)
term(43) = term(43) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_48_so_pt3(a,k)
term(44) = term(44) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_48_so_pt3(a,k)
term(45) = term(45) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_48_so_pt3(a,i)
term(46) = term(46) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_53_so_pt3(a,k)
term(47) = term(47) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_53_so_pt3(a,i)
term(48) = term(48) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,a,j) * wm_interm_53_so_pt3(b,k)
term(49) = term(49) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_53_so_pt3(b,i)
term(50) = term(50) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_53_so_pt3(a,k)
term(51) = term(51) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,p,j) * wm_interm_53_so_pt3(b,i)
term(52) = term(52) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,p,j) * wm_interm_53_so_pt3(b,k)
term(53) = term(53) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,p,j) * wm_interm_48_so_pt3(b,k)
term(54) = term(54) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_48_so_pt3(b,i)
term(55) = term(55) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_48_so_pt3(a,k)
term(56) = term(56) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_48_so_pt3(a,i)
term(57) = term(57) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_53_so_pt3(a,k)
term(58) = term(58) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_53_so_pt3(a,i)
term(59) = term(59) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,p,j) * wm_interm_53_so_pt3(b,k)
term(60) = term(60) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_53_so_pt3(b,i)
end do 
end do 
end do 
end do 
end do 

term(35) = term(35) * (12.0d+0) 
term(36) = term(36) * (-16.0d+0) 
term(37) = term(37) * (-6.0d+0) 
term(38) = term(38) * (8.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(47) = term(47) * (-0.5d+0) 
term(48) = term(48) * (-2.0d+0) 
term(50) = term(50) * (-0.5d+0) 
term(51) = term(51) * (-0.5d+0) 
term(53) = term(53) * (-8.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (4.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (-2.0d+0) 
term(58) = term(58) * (2.0d+0) 
term(59) = term(59) * (4.0d+0) 
term(60) = term(60) * (-2.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(61) = term(61) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,j) * wm_interm_5_so_pt3(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(61) = term(61) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(62) = term(62) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_8_so_pt3(b,k)
term(63) = term(63) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_8_so_pt3(b,k)
term(64) = term(64) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_9_so_pt3(b,k)
term(65) = term(65) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_9_so_pt3(b,k)
term(66) = term(66) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_8_so_pt3(b,k)
term(67) = term(67) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_8_so_pt3(b,k)
term(68) = term(68) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_9_so_pt3(b,k)
term(69) = term(69) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_9_so_pt3(b,k)
term(70) = term(70) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,k) * wm_interm_170_so_pt3(a,j)
term(71) = term(71) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,k) * wm_interm_170_so_pt3(a,i)
term(72) = term(72) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,i) * wm_interm_170_so_pt3(a,k)
term(73) = term(73) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,i) * wm_interm_170_so_pt3(a,j)
term(74) = term(74) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,k) * wm_interm_171_so_pt3(a,j)
term(75) = term(75) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,k) * wm_interm_171_so_pt3(a,i)
term(76) = term(76) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,i) * wm_interm_171_so_pt3(a,k)
term(77) = term(77) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,i) * wm_interm_171_so_pt3(a,j)
term(78) = term(78) + s2(a,q,i,j) * t3(nocc, nactive, a,b,p,k,j,i) * wm_interm_186_so_pt3(b,k)
term(79) = term(79) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,k,j,i) * wm_interm_186_so_pt3(b,i)
term(80) = term(80) + r2(vrdav_Rl, a,i,q,j) * t3(nocc, nactive, a,b,p,k,j,i) * wm_interm_46_so_pt3(b,k)
term(81) = term(81) + r2(vrdav_Rl, a,k,q,j) * t3(nocc, nactive, a,b,p,k,j,i) * wm_interm_46_so_pt3(b,i)
term(82) = term(82) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,p,k) * wm_interm_48_so_pt3(b,j)
term(83) = term(83) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,b,i) * wm_interm_48_so_pt3(a,k)
term(84) = term(84) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,b,i) * wm_interm_48_so_pt3(a,j)
term(85) = term(85) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,b,k) * wm_interm_48_so_pt3(a,j)
term(86) = term(86) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,p,k) * wm_interm_48_so_pt3(a,j)
term(87) = term(87) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,b,k) * wm_interm_48_so_pt3(a,i)
term(88) = term(88) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,p,k) * wm_interm_48_so_pt3(a,i)
term(89) = term(89) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,b,i) * wm_interm_53_so_pt3(a,k)
term(90) = term(90) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,b,k) * wm_interm_53_so_pt3(a,i)
term(91) = term(91) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,p,k) * wm_interm_53_so_pt3(a,i)
term(92) = term(92) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,p,k) * wm_interm_53_so_pt3(b,j)
term(93) = term(93) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,b,i) * wm_interm_53_so_pt3(a,j)
term(94) = term(94) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,b,k) * wm_interm_53_so_pt3(a,j)
term(95) = term(95) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,p,k) * wm_interm_53_so_pt3(a,j)
term(96) = term(96) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,p,i) * wm_interm_48_so_pt3(b,k)
term(97) = term(97) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_48_so_pt3(b,j)
term(98) = term(98) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_48_so_pt3(b,j)
term(99) = term(99) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_48_so_pt3(b,i)
term(100) = term(100) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_48_so_pt3(a,k)
term(101) = term(101) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,p,k) * wm_interm_48_so_pt3(a,j)
term(102) = term(102) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,p,i) * wm_interm_48_so_pt3(a,j)
term(103) = term(103) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_48_so_pt3(a,i)
term(104) = term(104) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_53_so_pt3(a,k)
term(105) = term(105) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_53_so_pt3(a,i)
term(106) = term(106) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,p,i) * wm_interm_53_so_pt3(b,k)
term(107) = term(107) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_53_so_pt3(b,i)
term(108) = term(108) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_53_so_pt3(b,j)
term(109) = term(109) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_53_so_pt3(b,j)
term(110) = term(110) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,p,k) * wm_interm_53_so_pt3(a,j)
term(111) = term(111) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,p,i) * wm_interm_53_so_pt3(a,j)
end do 
end do 
end do 
end do 
end do 

term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * (2.0d+0) 
term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (-1.0d+0) 
term(66) = term(66) * (-8.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(68) = term(68) * (4.0d+0) 
term(69) = term(69) * (-2.0d+0) 
term(70) = term(70) * (-16.0d+0) 
term(71) = term(71) * (12.0d+0) 
term(72) = term(72) * (-8.0d+0) 
term(73) = term(73) * (16.0d+0) 
term(74) = term(74) * (8.0d+0) 
term(75) = term(75) * (-6.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (-8.0d+0) 
term(78) = term(78) * (-1.9999999999999998d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (-2.0d+0) 
term(81) = term(81) * (4.0d+0) 
term(84) = term(84) * (-2.0d+0) 
term(85) = term(85) * (4.0d+0) 
term(86) = term(86) * (-2.0d+0) 
term(87) = term(87) * (-2.0d+0) 
term(89) = term(89) * (-0.5d+0) 
term(91) = term(91) * (-0.5d+0) 
term(92) = term(92) * (-0.5d+0) 
term(94) = term(94) * (-2.0d+0) 
term(96) = term(96) * (8.0d+0) 
term(97) = term(97) * (4.0d+0) 
term(98) = term(98) * (-4.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (-4.0d+0) 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * (8.0d+0) 
term(103) = term(103) * (4.0d+0) 
term(104) = term(104) * (2.0d+0) 
term(105) = term(105) * (-2.0d+0) 
term(106) = term(106) * (-4.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (-2.0d+0) 
term(109) = term(109) * (2.0d+0) 
term(110) = term(110) * (4.0d+0) 
term(111) = term(111) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(112) = term(112) + wm_interm_5_so_pt3(p,i,j,k) * wm_interm_85_so_pt3(q,i,k,j)
term(113) = term(113) + wm_interm_5_so_pt3(p,i,j,k) * wm_interm_86_so_pt3(q,i,k,j)
term(114) = term(114) + wm_interm_5_so_pt3(p,i,j,k) * wm_interm_87_so_pt3(q,i,k,j)
term(115) = term(115) + wm_interm_5_so_pt3(p,i,j,k) * wm_interm_91_so_pt3(q,i,j,k)
term(116) = term(116) + wm_interm_5_so_pt3(p,i,j,k) * wm_interm_90_so_pt3(q,i,k,j)
term(117) = term(117) + wm_interm_5_so_pt3(p,i,j,k) * wm_interm_91_so_pt3(q,i,k,j)
term(118) = term(118) + wm_interm_114_so_pt3(q,i,j,k) * wm_interm_5_so_pt3(p,i,k,j)
term(119) = term(119) + wm_interm_115_so_pt3(q,i,j,k) * wm_interm_5_so_pt3(p,i,k,j)
term(120) = term(120) + wm_interm_116_so_pt3(q,i,j,k) * wm_interm_5_so_pt3(p,i,k,j)
term(121) = term(121) + wm_interm_119_so_pt3(q,i,j,k) * wm_interm_5_so_pt3(p,i,j,k)
term(122) = term(122) + wm_interm_118_so_pt3(q,i,j,k) * wm_interm_5_so_pt3(p,i,k,j)
term(123) = term(123) + wm_interm_119_so_pt3(q,i,j,k) * wm_interm_5_so_pt3(p,i,k,j)
term(124) = term(124) + wm_interm_10_so_pt3(q,i,j,k) * wm_interm_173_so_pt3(p,k,i,j)
term(125) = term(125) + wm_interm_10_so_pt3(q,i,j,k) * wm_interm_174_so_pt3(p,k,i,j)
term(126) = term(126) + wm_interm_10_so_pt3(q,i,j,k) * wm_interm_175_so_pt3(p,k,i,j)
term(127) = term(127) + wm_interm_12_so_pt3(p,i,j,k) * wm_interm_176_so_pt3(q,j,i,k)
term(128) = term(128) + wm_interm_13_so_pt3(p,i,j,k) * wm_interm_176_so_pt3(q,j,i,k)
term(129) = term(129) + wm_interm_15_so_pt3(p,i,j,k) * wm_interm_176_so_pt3(q,i,j,k)
term(130) = term(130) + wm_interm_14_so_pt3(p,i,j,k) * wm_interm_176_so_pt3(q,i,j,k)
term(131) = term(131) + wm_interm_12_so_pt3(p,i,j,k) * wm_interm_176_so_pt3(q,i,j,k)
term(132) = term(132) + wm_interm_13_so_pt3(p,i,j,k) * wm_interm_176_so_pt3(q,i,j,k)
term(133) = term(133) + wm_interm_10_so_pt3(q,i,j,k) * wm_interm_178_so_pt3(p,k,i,j)
term(134) = term(134) + wm_interm_12_so_pt3(p,i,j,k) * wm_interm_179_so_pt3(q,i,j,k)
term(135) = term(135) + wm_interm_13_so_pt3(p,i,j,k) * wm_interm_179_so_pt3(q,i,j,k)
term(136) = term(136) + wm_interm_14_so_pt3(p,i,j,k) * wm_interm_179_so_pt3(q,i,j,k)
term(137) = term(137) + wm_interm_15_so_pt3(p,i,j,k) * wm_interm_179_so_pt3(q,i,j,k)
term(138) = term(138) + wm_interm_12_so_pt3(p,i,j,k) * wm_interm_179_so_pt3(q,j,i,k)
term(139) = term(139) + wm_interm_13_so_pt3(p,i,j,k) * wm_interm_179_so_pt3(q,j,i,k)
term(140) = term(140) + wm_interm_181_so_pt3(q,i,j,k) * wm_interm_44_so_pt3(p,k,i,j)
term(141) = term(141) + wm_interm_181_so_pt3(q,i,j,k) * wm_interm_37_so_pt3(p,k,i,j)
term(142) = term(142) + wm_interm_181_so_pt3(q,i,j,k) * wm_interm_45_so_pt3(p,k,i,j)
term(143) = term(143) + wm_interm_180_so_pt3(q,i,j,k) * wm_interm_39_so_pt3(p,k,i,j)
term(144) = term(144) + wm_interm_135_so_pt3(p,i,j,k) * wm_interm_41_so_pt3(q,i,k,j)
term(145) = term(145) + wm_interm_138_so_pt3(p,i,j,k) * wm_interm_41_so_pt3(q,i,j,k)
term(146) = term(146) + wm_interm_125_so_pt3(p,i,j,k) * wm_interm_41_so_pt3(q,i,k,j)
term(147) = term(147) + wm_interm_126_so_pt3(p,i,j,k) * wm_interm_41_so_pt3(q,i,k,j)
term(148) = term(148) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_54_so_pt3(q,j,i,k)
term(149) = term(149) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_54_so_pt3(q,i,j,k)
term(150) = term(150) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_57_so_pt3(q,i,j,k)
term(151) = term(151) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_57_so_pt3(q,j,i,k)
term(152) = term(152) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_58_so_pt3(q,j,i,k)
term(153) = term(153) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_58_so_pt3(q,i,j,k)
term(154) = term(154) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_59_so_pt3(q,j,i,k)
term(155) = term(155) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_59_so_pt3(q,i,j,k)
term(156) = term(156) + wm_interm_209_so_pt3(p,i,j,k) * wm_interm_54_so_pt3(q,i,j,k)
term(157) = term(157) + wm_interm_209_so_pt3(p,i,j,k) * wm_interm_57_so_pt3(q,j,i,k)
term(158) = term(158) + wm_interm_209_so_pt3(p,i,j,k) * wm_interm_58_so_pt3(q,i,j,k)
term(159) = term(159) + wm_interm_209_so_pt3(p,i,j,k) * wm_interm_59_so_pt3(q,i,j,k)
term(160) = term(160) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_60_so_pt3(q,i,j,k)
term(161) = term(161) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_60_so_pt3(q,j,i,k)
term(162) = term(162) + wm_interm_209_so_pt3(p,i,j,k) * wm_interm_60_so_pt3(q,j,i,k)
term(163) = term(163) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_61_so_pt3(q,i,j,k)
term(164) = term(164) + wm_interm_208_so_pt3(p,i,j,k) * wm_interm_61_so_pt3(q,j,i,k)
term(165) = term(165) + wm_interm_209_so_pt3(p,i,j,k) * wm_interm_61_so_pt3(q,j,i,k)
term(166) = term(166) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_54_so_pt3(q,j,i,k)
term(167) = term(167) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_54_so_pt3(q,i,j,k)
term(168) = term(168) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_57_so_pt3(q,i,j,k)
term(169) = term(169) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_57_so_pt3(q,j,i,k)
term(170) = term(170) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_58_so_pt3(q,j,i,k)
term(171) = term(171) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_58_so_pt3(q,i,j,k)
term(172) = term(172) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_59_so_pt3(q,j,i,k)
term(173) = term(173) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_59_so_pt3(q,i,j,k)
term(174) = term(174) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_60_so_pt3(q,i,j,k)
term(175) = term(175) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_60_so_pt3(q,j,i,k)
term(176) = term(176) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_61_so_pt3(q,i,j,k)
term(177) = term(177) + wm_interm_219_so_pt3(p,i,j,k) * wm_interm_61_so_pt3(q,j,i,k)
term(178) = term(178) + wm_interm_199_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,i,j)
term(179) = term(179) + wm_interm_200_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,i,j)
term(180) = term(180) + wm_interm_198_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,i,j)
term(181) = term(181) + wm_interm_195_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,i,j)
term(182) = term(182) + wm_interm_197_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,i,j)
term(183) = term(183) + wm_interm_201_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,i,j)
term(184) = term(184) + wm_interm_214_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,i,j)
term(185) = term(185) + wm_interm_215_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,i,j)
term(186) = term(186) + wm_interm_212_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,i,j)
term(187) = term(187) + wm_interm_213_so_pt3(q,i,j,k) * wm_interm_226_so_pt3(p,k,i,j)
end do 
end do 
end do 

term(112) = term(112) * (-1.0d+0) 
term(113) = term(113) * (2.0d+0) 
term(114) = term(114) * (-1.0d+0) 
term(115) = term(115) * (-0.9999999999999999d+0) 
term(116) = term(116) * (-0.9999999999999999d+0) 
term(117) = term(117) * (1.9999999999999998d+0) 
term(118) = term(118) * (-2.0d+0) 
term(119) = term(119) * (4.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * (-1.9999999999999998d+0) 
term(122) = term(122) * (-1.9999999999999998d+0) 
term(123) = term(123) * (3.9999999999999996d+0) 
term(124) = term(124) * (4.0d+0) 
term(125) = term(125) * (-6.0d+0) 
term(126) = term(126) * (8.0d+0) 
term(127) = term(127) * (-3.0d+0) 
term(128) = term(128) * (4.0d+0) 
term(129) = term(129) * (-3.0d+0) 
term(130) = term(130) * (4.0d+0) 
term(131) = term(131) * (2.0d+0) 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * (-3.0d+0) 
term(134) = term(134) * (-3.0d+0) 
term(135) = term(135) * (4.0d+0) 
term(136) = term(136) * (-3.0d+0) 
term(137) = term(137) * (4.0d+0) 
term(138) = term(138) * (2.0d+0) 
term(139) = term(139) * (-4.0d+0) 
term(140) = term(140) * (-1.0d+0) 
term(141) = term(141) * (-1.0d+0) 
term(142) = term(142) * (1.9999999999999998d+0) 
term(143) = term(143) * (-1.0d+0) 
term(144) = term(144) * (-1.0d+0) 
term(145) = term(145) * (-1.0d+0) 
term(146) = term(146) * (-1.9999999999999998d+0) 
term(147) = term(147) * (3.9999999999999996d+0) 
term(148) = term(148) * (-0.5d+0) 
term(150) = term(150) * (-0.5d+0) 
term(153) = term(153) * (-2.0d+0) 
term(154) = term(154) * (-0.5d+0) 
term(156) = term(156) * (-0.5d+0) 
term(157) = term(157) * (-0.5d+0) 
term(159) = term(159) * (-0.5d+0) 
term(161) = term(161) * (-2.0d+0) 
term(163) = term(163) * (-0.5d+0) 
term(165) = term(165) * (-0.5d+0) 
term(166) = term(166) * (-2.0d+0) 
term(167) = term(167) * (2.0d+0) 
term(168) = term(168) * (-2.0d+0) 
term(169) = term(169) * (2.0d+0) 
term(170) = term(170) * (4.0d+0) 
term(171) = term(171) * (-4.0d+0) 
term(172) = term(172) * (-2.0d+0) 
term(173) = term(173) * (2.0d+0) 
term(174) = term(174) * (4.0d+0) 
term(175) = term(175) * (-4.0d+0) 
term(176) = term(176) * (-2.0d+0) 
term(177) = term(177) * (2.0d+0) 
term(178) = term(178) * (-0.5d+0) 
term(180) = term(180) * (-0.5d+0) 
term(181) = term(181) * (-0.5d+0) 
term(182) = term(182) * (-0.5d+0) 
term(184) = term(184) * (-2.0d+0) 
term(185) = term(185) * (2.0d+0) 
term(186) = term(186) * (-2.0d+0) 
term(187) = term(187) * (2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(188) = term(188) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,j,i) * wm_interm_161_so_pt3(a,i,k,l)
term(189) = term(189) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,k,i) * wm_interm_162_so_pt3(a,i,j,l)
term(190) = term(190) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,k,i) * wm_interm_161_so_pt3(a,i,j,l)
term(191) = term(191) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,j,i) * wm_interm_163_so_pt3(a,i,k,l)
term(192) = term(192) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,k,i) * wm_interm_163_so_pt3(a,i,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(188) = term(188) * (-1.0d+0) 
term(189) = term(189) * (-0.9999999999999999d+0) 
term(190) = term(190) * (1.9999999999999998d+0) 
term(191) = term(191) * (-4.0d+0) 
term(192) = term(192) * (3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(193) = term(193) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,l,i) * wm_interm_162_so_pt3(a,l,k,j)
term(194) = term(194) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,l,i) * wm_interm_162_so_pt3(a,l,j,k)
term(195) = term(195) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,l,i) * wm_interm_161_so_pt3(a,l,j,k)
term(196) = term(196) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,l,i) * wm_interm_163_so_pt3(a,l,j,k)
term(197) = term(197) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,l,i) * wm_interm_163_so_pt3(a,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(193) = term(193) * (-0.9999999999999999d+0) 
term(194) = term(194) * (1.9999999999999998d+0) 
term(195) = term(195) * (-0.9999999999999999d+0) 
term(196) = term(196) * (-4.0d+0) 
term(197) = term(197) * (3.9999999999999996d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(198) = term(198) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,a,i) * wm_interm_48_so_pt3(b,k)
term(199) = term(199) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_48_so_pt3(b,j)
term(200) = term(200) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,a,k) * wm_interm_48_so_pt3(b,j)
term(201) = term(201) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,a,k) * wm_interm_48_so_pt3(b,i)
term(202) = term(202) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,a,i) * wm_interm_53_so_pt3(b,k)
term(203) = term(203) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,a,k) * wm_interm_53_so_pt3(b,i)
term(204) = term(204) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_53_so_pt3(b,j)
term(205) = term(205) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,a,k) * wm_interm_53_so_pt3(b,j)
end do 
end do 
end do 
end do 
end do 

term(198) = term(198) * (-2.0d+0) 
term(200) = term(200) * (-2.0d+0) 
term(203) = term(203) * (-0.5d+0) 
term(204) = term(204) * (-0.5d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(206) = term(206) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,l,k) * wm_interm_5_so_pt3(b,i,l,k)
term(207) = term(207) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,l,k) * wm_interm_5_so_pt3(b,i,k,l)
term(208) = term(208) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,l,k) * wm_interm_5_so_pt3(b,i,k,l)
term(209) = term(209) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,l,k) * wm_interm_5_so_pt3(b,i,l,k)
term(210) = term(210) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,l,k) * wm_interm_5_so_pt3(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(206) = term(206) * (-1.0d+0) 
term(207) = term(207) * (2.0d+0) 
term(208) = term(208) * (-8.0d+0) 
term(209) = term(209) * (-2.0d+0) 
term(210) = term(210) * (4.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(211) = term(211) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,l,j,k) * wm_interm_5_so_pt3(b,i,l,k)
term(212) = term(212) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,l,i,k) * wm_interm_5_so_pt3(b,j,l,k)
term(213) = term(213) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,l,i,k) * wm_interm_5_so_pt3(b,j,k,l)
term(214) = term(214) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,l,j,k) * wm_interm_5_so_pt3(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(211) = term(211) * (-0.9999999999999999d+0) 
term(212) = term(212) * (1.9999999999999998d+0) 
term(213) = term(213) * (-0.9999999999999999d+0) 
term(214) = term(214) * (1.9999999999999998d+0) 

do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(215) = term(215) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(a,p,l,i) * wm_interm_209_so_pt3(b,k,j,l)
term(216) = term(216) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(a,p,l,i) * wm_interm_209_so_pt3(b,j,k,l)
term(217) = term(217) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,l,i) * wm_interm_209_so_pt3(a,j,k,l)
term(218) = term(218) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,l,i) * wm_interm_208_so_pt3(a,k,j,l)
term(219) = term(219) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,l,i) * wm_interm_208_so_pt3(a,j,k,l)
term(220) = term(220) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(a,p,l,i) * wm_interm_208_so_pt3(b,j,k,l)
term(221) = term(221) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(a,p,l,i) * wm_interm_219_so_pt3(b,j,k,l)
term(222) = term(222) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(a,p,l,i) * wm_interm_219_so_pt3(b,k,j,l)
term(223) = term(223) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,l,i) * wm_interm_219_so_pt3(a,k,j,l)
term(224) = term(224) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,l,i) * wm_interm_219_so_pt3(a,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(215) = term(215) * (-0.5d+0) 
term(217) = term(217) * (-0.5d+0) 
term(218) = term(218) * (-0.5d+0) 
term(220) = term(220) * (-0.5d+0) 
term(221) = term(221) * (-2.0d+0) 
term(222) = term(222) * (2.0d+0) 
term(223) = term(223) * (-2.0d+0) 
term(224) = term(224) * (2.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(225) = term(225) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,l,i) * wm_interm_209_so_pt3(b,j,k,l)
term(226) = term(226) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(b,p,l,i) * wm_interm_209_so_pt3(a,k,j,l)
term(227) = term(227) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(b,p,l,i) * wm_interm_209_so_pt3(a,j,k,l)
term(228) = term(228) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(b,p,l,i) * wm_interm_208_so_pt3(a,j,k,l)
term(229) = term(229) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,l,i) * wm_interm_208_so_pt3(b,k,j,l)
term(230) = term(230) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,l,i) * wm_interm_208_so_pt3(b,j,k,l)
term(231) = term(231) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,j,i) * wm_interm_209_so_pt3(b,l,k,i)
term(232) = term(232) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,j,i) * wm_interm_209_so_pt3(b,k,l,i)
term(233) = term(233) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,l,i) * wm_interm_209_so_pt3(b,k,j,i)
term(234) = term(234) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,l,i) * wm_interm_209_so_pt3(b,j,k,i)
term(235) = term(235) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,k,i) * wm_interm_209_so_pt3(b,l,j,i)
term(236) = term(236) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,k,i) * wm_interm_209_so_pt3(b,j,l,i)
term(237) = term(237) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,j,i) * wm_interm_209_so_pt3(a,k,l,i)
term(238) = term(238) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,j,i) * wm_interm_209_so_pt3(a,l,k,i)
term(239) = term(239) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,k,i) * wm_interm_209_so_pt3(a,l,j,i)
term(240) = term(240) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,k,i) * wm_interm_209_so_pt3(a,j,l,i)
term(241) = term(241) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,l,i) * wm_interm_209_so_pt3(a,k,j,i)
term(242) = term(242) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,l,i) * wm_interm_209_so_pt3(a,j,k,i)
term(243) = term(243) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,l,i) * wm_interm_208_so_pt3(b,j,k,i)
term(244) = term(244) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,j,i) * wm_interm_208_so_pt3(b,k,l,i)
term(245) = term(245) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,k,i) * wm_interm_208_so_pt3(b,j,l,i)
term(246) = term(246) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,k,i) * wm_interm_208_so_pt3(a,j,l,i)
term(247) = term(247) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,j,i) * wm_interm_208_so_pt3(a,l,k,i)
term(248) = term(248) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,l,i) * wm_interm_208_so_pt3(a,j,k,i)
term(249) = term(249) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,l,i) * wm_interm_219_so_pt3(b,k,j,l)
term(250) = term(250) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(b,p,l,i) * wm_interm_219_so_pt3(a,j,k,l)
term(251) = term(251) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(b,p,l,i) * wm_interm_219_so_pt3(a,k,j,l)
term(252) = term(252) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,l,i) * wm_interm_219_so_pt3(b,j,k,l)
term(253) = term(253) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,j,i) * wm_interm_219_so_pt3(b,k,l,i)
term(254) = term(254) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,j,i) * wm_interm_219_so_pt3(b,l,k,i)
term(255) = term(255) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,l,i) * wm_interm_219_so_pt3(b,j,k,i)
term(256) = term(256) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,l,i) * wm_interm_219_so_pt3(b,k,j,i)
term(257) = term(257) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,k,i) * wm_interm_219_so_pt3(b,j,l,i)
term(258) = term(258) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(a,p,k,i) * wm_interm_219_so_pt3(b,l,j,i)
term(259) = term(259) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,j,i) * wm_interm_219_so_pt3(a,l,k,i)
term(260) = term(260) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,j,i) * wm_interm_219_so_pt3(a,k,l,i)
term(261) = term(261) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,k,i) * wm_interm_219_so_pt3(a,j,l,i)
term(262) = term(262) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,k,i) * wm_interm_219_so_pt3(a,l,j,i)
term(263) = term(263) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,l,i) * wm_interm_219_so_pt3(a,j,k,i)
term(264) = term(264) + r3(vrdav_Rl, a,k,b,l,q,j) * t2(b,p,l,i) * wm_interm_219_so_pt3(a,k,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(225) = term(225) * (-0.5d+0) 
term(226) = term(226) * (-0.5d+0) 
term(228) = term(228) * (-0.5d+0) 
term(229) = term(229) * (-0.5d+0) 
term(231) = term(231) * (-0.5d+0) 
term(233) = term(233) * (-0.5d+0) 
term(236) = term(236) * (-2.0d+0) 
term(237) = term(237) * (-0.5d+0) 
term(239) = term(239) * (-0.5d+0) 
term(242) = term(242) * (-2.0d+0) 
term(243) = term(243) * (-0.5d+0) 
term(244) = term(244) * (-0.5d+0) 
term(246) = term(246) * (-0.5d+0) 
term(247) = term(247) * (-0.5d+0) 
term(249) = term(249) * (-2.0d+0) 
term(250) = term(250) * (-2.0d+0) 
term(251) = term(251) * (2.0d+0) 
term(252) = term(252) * (2.0d+0) 
term(253) = term(253) * (-2.0d+0) 
term(254) = term(254) * (2.0d+0) 
term(255) = term(255) * (-2.0d+0) 
term(256) = term(256) * (2.0d+0) 
term(257) = term(257) * (4.0d+0) 
term(258) = term(258) * (-4.0d+0) 
term(259) = term(259) * (-2.0d+0) 
term(260) = term(260) * (2.0d+0) 
term(261) = term(261) * (-2.0d+0) 
term(262) = term(262) * (2.0d+0) 
term(263) = term(263) * (4.0d+0) 
term(264) = term(264) * (-4.0d+0) 

do i = 1, nocc 
term(265) = term(265) + wm_interm_97_so_pt3(q,i) * wm_interm_9_so_pt3(p,i)
term(266) = term(266) + wm_interm_98_so_pt3(q,i) * wm_interm_9_so_pt3(p,i)
term(267) = term(267) + wm_interm_99_so_pt3(q,i) * wm_interm_9_so_pt3(p,i)
term(268) = term(268) + wm_interm_8_so_pt3(p,i) * wm_interm_97_so_pt3(q,i)
term(269) = term(269) + wm_interm_8_so_pt3(p,i) * wm_interm_98_so_pt3(q,i)
term(270) = term(270) + wm_interm_8_so_pt3(p,i) * wm_interm_99_so_pt3(q,i)
term(271) = term(271) + wm_interm_122_so_pt3(q,i) * wm_interm_9_so_pt3(p,i)
term(272) = term(272) + wm_interm_123_so_pt3(q,i) * wm_interm_9_so_pt3(p,i)
term(273) = term(273) + wm_interm_124_so_pt3(q,i) * wm_interm_9_so_pt3(p,i)
term(274) = term(274) + wm_interm_122_so_pt3(q,i) * wm_interm_8_so_pt3(p,i)
term(275) = term(275) + wm_interm_123_so_pt3(q,i) * wm_interm_8_so_pt3(p,i)
term(276) = term(276) + wm_interm_124_so_pt3(q,i) * wm_interm_8_so_pt3(p,i)
term(277) = term(277) + wm_interm_170_so_pt3(q,i) * wm_interm_19_so_pt3(p,i)
term(278) = term(278) + wm_interm_170_so_pt3(q,i) * wm_interm_20_so_pt3(p,i)
term(279) = term(279) + wm_interm_170_so_pt3(q,i) * wm_interm_21_so_pt3(p,i)
term(280) = term(280) + wm_interm_170_so_pt3(q,i) * wm_interm_22_so_pt3(p,i)
term(281) = term(281) + wm_interm_171_so_pt3(q,i) * wm_interm_19_so_pt3(p,i)
term(282) = term(282) + wm_interm_171_so_pt3(q,i) * wm_interm_20_so_pt3(p,i)
term(283) = term(283) + wm_interm_171_so_pt3(q,i) * wm_interm_21_so_pt3(p,i)
term(284) = term(284) + wm_interm_171_so_pt3(q,i) * wm_interm_22_so_pt3(p,i)
term(285) = term(285) + wm_interm_186_so_pt3(q,i) * wm_interm_34_so_pt3(p,i)
term(286) = term(286) + wm_interm_186_so_pt3(q,i) * wm_interm_33_so_pt3(p,i)
term(287) = term(287) + wm_interm_186_so_pt3(q,i) * wm_interm_38_so_pt3(p,i)
term(288) = term(288) + wm_interm_186_so_pt3(q,i) * wm_interm_36_so_pt3(p,i)
term(289) = term(289) + wm_interm_136_so_pt3(p,i) * wm_interm_46_so_pt3(q,i)
term(290) = term(290) + wm_interm_137_so_pt3(p,i) * wm_interm_46_so_pt3(q,i)
term(291) = term(291) + wm_interm_134_so_pt3(p,i) * wm_interm_46_so_pt3(q,i)
term(292) = term(292) + wm_interm_127_so_pt3(p,i) * wm_interm_46_so_pt3(q,i)
term(293) = term(293) + wm_interm_187_so_pt3(p,i) * wm_interm_62_so_pt3(q,i)
term(294) = term(294) + wm_interm_187_so_pt3(p,i) * wm_interm_64_so_pt3(q,i)
term(295) = term(295) + wm_interm_188_so_pt3(p,i) * wm_interm_62_so_pt3(q,i)
term(296) = term(296) + wm_interm_188_so_pt3(p,i) * wm_interm_64_so_pt3(q,i)
term(297) = term(297) + wm_interm_189_so_pt3(p,i) * wm_interm_62_so_pt3(q,i)
term(298) = term(298) + wm_interm_189_so_pt3(p,i) * wm_interm_64_so_pt3(q,i)
term(299) = term(299) + wm_interm_187_so_pt3(p,i) * wm_interm_65_so_pt3(q,i)
term(300) = term(300) + wm_interm_187_so_pt3(p,i) * wm_interm_66_so_pt3(q,i)
term(301) = term(301) + wm_interm_188_so_pt3(p,i) * wm_interm_65_so_pt3(q,i)
term(302) = term(302) + wm_interm_188_so_pt3(p,i) * wm_interm_66_so_pt3(q,i)
term(303) = term(303) + wm_interm_189_so_pt3(p,i) * wm_interm_65_so_pt3(q,i)
term(304) = term(304) + wm_interm_189_so_pt3(p,i) * wm_interm_66_so_pt3(q,i)
term(305) = term(305) + wm_interm_190_so_pt3(p,i) * wm_interm_62_so_pt3(q,i)
term(306) = term(306) + wm_interm_190_so_pt3(p,i) * wm_interm_64_so_pt3(q,i)
term(307) = term(307) + wm_interm_191_so_pt3(p,i) * wm_interm_62_so_pt3(q,i)
term(308) = term(308) + wm_interm_191_so_pt3(p,i) * wm_interm_64_so_pt3(q,i)
term(309) = term(309) + wm_interm_190_so_pt3(p,i) * wm_interm_65_so_pt3(q,i)
term(310) = term(310) + wm_interm_190_so_pt3(p,i) * wm_interm_66_so_pt3(q,i)
term(311) = term(311) + wm_interm_191_so_pt3(p,i) * wm_interm_65_so_pt3(q,i)
term(312) = term(312) + wm_interm_191_so_pt3(p,i) * wm_interm_66_so_pt3(q,i)
term(313) = term(313) + wm_interm_220_so_pt3(q,i) * wm_interm_53_so_pt3(p,i)
term(314) = term(314) + wm_interm_221_so_pt3(q,i) * wm_interm_53_so_pt3(p,i)
term(315) = term(315) + wm_interm_222_so_pt3(q,i) * wm_interm_53_so_pt3(p,i)
term(316) = term(316) + wm_interm_220_so_pt3(q,i) * wm_interm_48_so_pt3(p,i)
term(317) = term(317) + wm_interm_221_so_pt3(q,i) * wm_interm_48_so_pt3(p,i)
term(318) = term(318) + wm_interm_222_so_pt3(q,i) * wm_interm_48_so_pt3(p,i)
term(319) = term(319) + wm_interm_227_so_pt3(q,i) * wm_interm_53_so_pt3(p,i)
term(320) = term(320) + wm_interm_228_so_pt3(q,i) * wm_interm_53_so_pt3(p,i)
term(321) = term(321) + wm_interm_227_so_pt3(q,i) * wm_interm_48_so_pt3(p,i)
term(322) = term(322) + wm_interm_228_so_pt3(q,i) * wm_interm_48_so_pt3(p,i)
end do 

term(265) = term(265) * (-1.0d+0) 
term(266) = term(266) * (2.0d+0) 
term(267) = term(267) * (-1.0d+0) 
term(268) = term(268) * (2.0d+0) 
term(269) = term(269) * (-4.0d+0) 
term(270) = term(270) * (2.0d+0) 
term(271) = term(271) * (-2.0d+0) 
term(272) = term(272) * (4.0d+0) 
term(273) = term(273) * (-2.0d+0) 
term(274) = term(274) * (4.0d+0) 
term(275) = term(275) * (-8.0d+0) 
term(276) = term(276) * (4.0d+0) 
term(277) = term(277) * (16.0d+0) 
term(278) = term(278) * (-8.0d+0) 
term(279) = term(279) * (-12.0d+0) 
term(280) = term(280) * (4.0d+0) 
term(281) = term(281) * (-8.0d+0) 
term(282) = term(282) * (4.0d+0) 
term(283) = term(283) * (6.0d+0) 
term(284) = term(284) * (-2.0d+0) 
term(285) = term(285) * (-2.0d+0) 
term(286) = term(286) * (1.9999999999999998d+0) 
term(287) = term(287) * (4.0d+0) 
term(288) = term(288) * (-3.9999999999999996d+0) 
term(289) = term(289) * (-2.0d+0) 
term(290) = term(290) * (4.0d+0) 
term(291) = term(291) * (1.9999999999999998d+0) 
term(292) = term(292) * (-3.9999999999999996d+0) 
term(293) = term(293) * (3.9999999999999996d+0) 
term(294) = term(294) * (-4.0d+0) 
term(295) = term(295) * (-1.9999999999999998d+0) 
term(296) = term(296) * (2.0d+0) 
term(297) = term(297) * (-1.9999999999999998d+0) 
term(298) = term(298) * (2.0d+0) 
term(299) = term(299) * (-1.9999999999999998d+0) 
term(300) = term(300) * (2.0d+0) 
term(302) = term(302) * (-1.0d+0) 
term(304) = term(304) * (-1.0d+0) 
term(305) = term(305) * (7.999999999999999d+0) 
term(306) = term(306) * (-8.0d+0) 
term(307) = term(307) * (-7.999999999999999d+0) 
term(308) = term(308) * (8.0d+0) 
term(309) = term(309) * (-3.9999999999999996d+0) 
term(310) = term(310) * (4.0d+0) 
term(311) = term(311) * (3.9999999999999996d+0) 
term(312) = term(312) * (-4.0d+0) 
term(313) = term(313) * (-0.9999999999999999d+0) 
term(314) = term(314) * (1.9999999999999998d+0) 
term(315) = term(315) * (-0.9999999999999999d+0) 
term(316) = term(316) * (1.9999999999999998d+0) 
term(317) = term(317) * (-3.9999999999999996d+0) 
term(318) = term(318) * (1.9999999999999998d+0) 
term(319) = term(319) * (-3.9999999999999996d+0) 
term(320) = term(320) * (3.9999999999999996d+0) 
term(321) = term(321) * (7.999999999999999d+0) 
term(322) = term(322) * (-7.999999999999999d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(323) = term(323) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,l,b,j,p,i) * wm_interm_10_so_pt3(b,l,i,k)
term(324) = term(324) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,l,b,j,p,i) * wm_interm_10_so_pt3(b,i,l,k)
term(325) = term(325) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,l,b,k,p,i) * wm_interm_10_so_pt3(b,i,l,j)
term(326) = term(326) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,l,b,k,p,i) * wm_interm_10_so_pt3(b,l,i,j)
term(327) = term(327) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,b,j) * wm_interm_52_so_pt3(a,l,i,k)
term(328) = term(328) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,b,j) * wm_interm_52_so_pt3(a,l,k,i)
term(329) = term(329) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,l,p,j) * wm_interm_52_so_pt3(a,l,k,i)
term(330) = term(330) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,l,p,j) * wm_interm_52_so_pt3(b,l,i,k)
term(331) = term(331) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,l,p,j) * wm_interm_52_so_pt3(b,l,k,i)
term(332) = term(332) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,b,k) * wm_interm_52_so_pt3(a,l,i,j)
term(333) = term(333) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,b,i) * wm_interm_52_so_pt3(a,l,k,j)
term(334) = term(334) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,l,p,k) * wm_interm_52_so_pt3(a,l,i,j)
term(335) = term(335) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,l,p,j) * wm_interm_52_so_pt3(a,l,i,k)
term(336) = term(336) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,l,p,j) * wm_interm_52_so_pt3(a,l,k,i)
term(337) = term(337) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,l,p,j) * wm_interm_52_so_pt3(b,l,i,k)
term(338) = term(338) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,l,p,j) * wm_interm_52_so_pt3(b,l,k,i)
term(339) = term(339) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,l,p,i) * wm_interm_52_so_pt3(b,l,k,j)
term(340) = term(340) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,l,p,k) * wm_interm_52_so_pt3(a,l,i,j)
term(341) = term(341) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,l,p,k) * wm_interm_52_so_pt3(b,l,i,j)
term(342) = term(342) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,l,p,i) * wm_interm_52_so_pt3(a,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(323) = term(323) * (-6.0d+0) 
term(324) = term(324) * (8.0d+0) 
term(325) = term(325) * (-6.0d+0) 
term(326) = term(326) * (4.0d+0) 
term(327) = term(327) * (-0.5d+0) 
term(329) = term(329) * (-0.5d+0) 
term(330) = term(330) * (-0.5d+0) 
term(333) = term(333) * (-0.5d+0) 
term(334) = term(334) * (-0.5d+0) 
term(335) = term(335) * (2.0d+0) 
term(336) = term(336) * (-2.0d+0) 
term(337) = term(337) * (-2.0d+0) 
term(338) = term(338) * (4.0d+0) 
term(339) = term(339) * (-4.0d+0) 
term(340) = term(340) * (-2.0d+0) 
term(341) = term(341) * (2.0d+0) 
term(342) = term(342) * (2.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(343) = term(343) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,j,b,l,p,i) * wm_interm_10_so_pt3(b,l,i,k)
term(344) = term(344) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,j,b,l,p,i) * wm_interm_10_so_pt3(b,i,l,k)
term(345) = term(345) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,j) * wm_interm_176_so_pt3(a,i,k,l)
term(346) = term(346) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,j) * wm_interm_176_so_pt3(a,k,i,l)
term(347) = term(347) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,j) * wm_interm_179_so_pt3(a,i,k,l)
term(348) = term(348) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,j) * wm_interm_179_so_pt3(a,k,i,l)
term(349) = term(349) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,a,l) * wm_interm_52_so_pt3(b,l,k,j)
term(350) = term(350) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,p,l) * wm_interm_52_so_pt3(b,l,k,j)
term(351) = term(351) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,b,l) * wm_interm_52_so_pt3(a,l,i,j)
term(352) = term(352) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_52_so_pt3(b,l,i,j)
term(353) = term(353) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,b,l) * wm_interm_52_so_pt3(a,l,k,j)
term(354) = term(354) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_52_so_pt3(b,l,i,j)
term(355) = term(355) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,p,l) * wm_interm_52_so_pt3(a,l,k,j)
term(356) = term(356) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,p,l) * wm_interm_52_so_pt3(b,l,k,j)
term(357) = term(357) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,p,l) * wm_interm_52_so_pt3(a,l,i,j)
term(358) = term(358) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_52_so_pt3(b,l,i,j)
term(359) = term(359) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,p,l) * wm_interm_52_so_pt3(a,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(343) = term(343) * (8.0d+0) 
term(344) = term(344) * (-6.0d+0) 
term(345) = term(345) * (4.0d+0) 
term(346) = term(346) * (-3.0d+0) 
term(347) = term(347) * (-3.0d+0) 
term(348) = term(348) * (4.0d+0) 
term(349) = term(349) * (-2.0d+0) 
term(351) = term(351) * (-0.5d+0) 
term(354) = term(354) * (-0.5d+0) 
term(355) = term(355) * (-0.5d+0) 
term(356) = term(356) * (4.0d+0) 
term(357) = term(357) * (2.0d+0) 
term(358) = term(358) * (-2.0d+0) 
term(359) = term(359) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(360) = term(360) + r1(vrdav_Rl, q,j) * t2(a,p,j,i) * wm_interm_97_so_pt3(a,i)
term(361) = term(361) + r1(vrdav_Rl, q,j) * t2(a,p,j,i) * wm_interm_98_so_pt3(a,i)
term(362) = term(362) + r1(vrdav_Rl, q,j) * t2(a,p,j,i) * wm_interm_99_so_pt3(a,i)
term(363) = term(363) + r1(vrdav_Rl, q,j) * t2(a,p,j,i) * wm_interm_122_so_pt3(a,i)
term(364) = term(364) + r1(vrdav_Rl, q,j) * t2(a,p,j,i) * wm_interm_123_so_pt3(a,i)
term(365) = term(365) + r1(vrdav_Rl, q,j) * t2(a,p,j,i) * wm_interm_124_so_pt3(a,i)
term(366) = term(366) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_33_so_pt3(a,j)
term(367) = term(367) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_36_so_pt3(a,j)
term(368) = term(368) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_34_so_pt3(a,j)
term(369) = term(369) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_38_so_pt3(a,j)
term(370) = term(370) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_127_so_pt3(a,j)
term(371) = term(371) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_134_so_pt3(a,j)
term(372) = term(372) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_136_so_pt3(a,j)
term(373) = term(373) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_137_so_pt3(a,j)
term(374) = term(374) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_62_so_pt3(a,j)
term(375) = term(375) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_64_so_pt3(a,j)
term(376) = term(376) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_65_so_pt3(a,j)
term(377) = term(377) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_66_so_pt3(a,j)
term(378) = term(378) + s1(q,j) * t2(a,p,j,i) * wm_interm_220_so_pt3(a,i)
term(379) = term(379) + s1(q,j) * t2(a,p,j,i) * wm_interm_221_so_pt3(a,i)
term(380) = term(380) + s1(q,j) * t2(a,p,j,i) * wm_interm_222_so_pt3(a,i)
term(381) = term(381) + s1(q,j) * t2(a,p,j,i) * wm_interm_227_so_pt3(a,i)
term(382) = term(382) + s1(q,j) * t2(a,p,j,i) * wm_interm_228_so_pt3(a,i)
end do 
end do 
end do 

term(360) = term(360) * (-1.0d+0) 
term(361) = term(361) * (2.0d+0) 
term(362) = term(362) * (-1.0d+0) 
term(363) = term(363) * (-2.0d+0) 
term(364) = term(364) * (4.0d+0) 
term(365) = term(365) * (-2.0d+0) 
term(366) = term(366) * (-3.9999999999999996d+0) 
term(367) = term(367) * (7.999999999999999d+0) 
term(368) = term(368) * (4.0d+0) 
term(369) = term(369) * (-8.0d+0) 
term(370) = term(370) * (8.0d+0) 
term(371) = term(371) * (-4.0d+0) 
term(372) = term(372) * (4.0d+0) 
term(373) = term(373) * (-8.0d+0) 
term(374) = term(374) * (-7.999999999999999d+0) 
term(375) = term(375) * (8.0d+0) 
term(376) = term(376) * (3.9999999999999996d+0) 
term(377) = term(377) * (-4.0d+0) 
term(378) = term(378) * (-0.9999999999999999d+0) 
term(379) = term(379) * (1.9999999999999998d+0) 
term(380) = term(380) * (-0.9999999999999999d+0) 
term(381) = term(381) * (-3.9999999999999996d+0) 
term(382) = term(382) * (3.9999999999999996d+0) 

do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(383) = term(383) + r2(vrdav_Rl, a,i,q,j) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_10_so_pt3(b,k,l,j)
term(384) = term(384) + r2(vrdav_Rl, a,i,q,j) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_10_so_pt3(b,l,k,j)
term(385) = term(385) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_10_so_pt3(b,i,l,j)
term(386) = term(386) + r2(vrdav_Rl, a,k,q,j) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_10_so_pt3(b,l,i,j)
term(387) = term(387) + r2(vrdav_Rl, a,j,q,i) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_10_so_pt3(b,k,l,j)
term(388) = term(388) + r2(vrdav_Rl, a,j,q,i) * r3(vrdav_Rr, a,k,b,l,p,i) * wm_interm_10_so_pt3(b,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(383) = term(383) * (-6.0d+0) 
term(384) = term(384) * (8.0d+0) 
term(385) = term(385) * (8.0d+0) 
term(386) = term(386) * (-8.0d+0) 
term(387) = term(387) * (4.0d+0) 
term(388) = term(388) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(389) = term(389) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,i,j,k) * wm_interm_8_so_pt3(b,k)
term(390) = term(390) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,i,j,k) * wm_interm_9_so_pt3(b,k)
term(391) = term(391) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,j,k) * wm_interm_8_so_pt3(b,k)
term(392) = term(392) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,j,k) * wm_interm_9_so_pt3(b,k)
end do 
end do 
end do 
end do 
end do 

term(389) = term(389) * (2.0d+0) 
term(390) = term(390) * (-1.0d+0) 
term(391) = term(391) * (4.0d+0) 
term(392) = term(392) * (-2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(393) = term(393) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(a,p,k,i) * wm_interm_187_so_pt3(b,j)
term(394) = term(394) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,j,i) * wm_interm_187_so_pt3(a,k)
term(395) = term(395) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,k,i) * wm_interm_187_so_pt3(a,j)
term(396) = term(396) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(a,p,k,i) * wm_interm_188_so_pt3(b,j)
term(397) = term(397) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,j,i) * wm_interm_188_so_pt3(a,k)
term(398) = term(398) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,k,i) * wm_interm_188_so_pt3(a,j)
term(399) = term(399) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(a,p,k,i) * wm_interm_189_so_pt3(b,j)
term(400) = term(400) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,j,i) * wm_interm_189_so_pt3(a,k)
term(401) = term(401) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,k,i) * wm_interm_189_so_pt3(a,j)
term(402) = term(402) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(a,p,k,i) * wm_interm_190_so_pt3(b,j)
term(403) = term(403) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,j,i) * wm_interm_190_so_pt3(a,k)
term(404) = term(404) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,k,i) * wm_interm_190_so_pt3(a,j)
term(405) = term(405) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(a,p,k,i) * wm_interm_191_so_pt3(b,j)
term(406) = term(406) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,j,i) * wm_interm_191_so_pt3(a,k)
term(407) = term(407) + r3(vrdav_Rl, a,i,b,k,q,j) * t2(b,p,k,i) * wm_interm_191_so_pt3(a,j)
end do 
end do 
end do 
end do 
end do 

term(393) = term(393) * (-1.0d+0) 
term(394) = term(394) * (-1.0d+0) 
term(395) = term(395) * (2.0d+0) 
term(396) = term(396) * (0.5d+0) 
term(397) = term(397) * (0.5d+0) 
term(398) = term(398) * (-1.0d+0) 
term(399) = term(399) * (0.5d+0) 
term(400) = term(400) * (0.5d+0) 
term(401) = term(401) * (-1.0d+0) 
term(402) = term(402) * (-2.0d+0) 
term(403) = term(403) * (-2.0d+0) 
term(404) = term(404) * (4.0d+0) 
term(405) = term(405) * (2.0d+0) 
term(406) = term(406) * (2.0d+0) 
term(407) = term(407) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(408) = term(408) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_33_so_pt3(a,j)
term(409) = term(409) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_36_so_pt3(a,j)
term(410) = term(410) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_34_so_pt3(a,j)
term(411) = term(411) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_38_so_pt3(a,j)
term(412) = term(412) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_127_so_pt3(a,j)
term(413) = term(413) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_134_so_pt3(a,j)
term(414) = term(414) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_136_so_pt3(a,j)
term(415) = term(415) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_137_so_pt3(a,j)
term(416) = term(416) + r2(vrdav_Rl, a,i,q,j) * t1(p,i) * wm_interm_19_so_pt3(a,j)
term(417) = term(417) + r2(vrdav_Rl, a,i,q,j) * t1(p,i) * wm_interm_20_so_pt3(a,j)
term(418) = term(418) + r2(vrdav_Rl, a,i,q,j) * t1(p,i) * wm_interm_21_so_pt3(a,j)
term(419) = term(419) + r2(vrdav_Rl, a,i,q,j) * t1(p,i) * wm_interm_22_so_pt3(a,j)
term(420) = term(420) + s2(a,q,i,j) * t1(p,i) * wm_interm_182_so_pt3(a,j)
term(421) = term(421) + s2(a,q,i,j) * t1(p,i) * wm_interm_183_so_pt3(a,j)
term(422) = term(422) + s2(a,q,i,j) * t1(p,i) * wm_interm_184_so_pt3(a,j)
term(423) = term(423) + s2(a,q,i,j) * t1(p,i) * wm_interm_185_so_pt3(a,j)
term(424) = term(424) + r2p(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_62_so_pt3(a,j)
term(425) = term(425) + r2p(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_64_so_pt3(a,j)
term(426) = term(426) + r2p(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_65_so_pt3(a,j)
term(427) = term(427) + r2p(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_66_so_pt3(a,j)
term(428) = term(428) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_62_so_pt3(a,j)
term(429) = term(429) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_64_so_pt3(a,j)
term(430) = term(430) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_65_so_pt3(a,j)
term(431) = term(431) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_66_so_pt3(a,j)
end do 
end do 
end do 

term(408) = term(408) * (1.9999999999999998d+0) 
term(409) = term(409) * (-3.9999999999999996d+0) 
term(410) = term(410) * (-2.0d+0) 
term(411) = term(411) * (4.0d+0) 
term(412) = term(412) * (-4.0d+0) 
term(413) = term(413) * (2.0d+0) 
term(414) = term(414) * (-2.0d+0) 
term(415) = term(415) * (4.0d+0) 
term(416) = term(416) * (-8.0d+0) 
term(417) = term(417) * (4.0d+0) 
term(418) = term(418) * (6.0d+0) 
term(419) = term(419) * (-2.0d+0) 
term(420) = term(420) * (-6.0d+0) 
term(421) = term(421) * (8.0d+0) 
term(422) = term(422) * (2.0d+0) 
term(423) = term(423) * (-4.0d+0) 
term(424) = term(424) * (1.9999999999999998d+0) 
term(425) = term(425) * (-2.0d+0) 
term(426) = term(426) * (-0.9999999999999999d+0) 
term(428) = term(428) * (7.999999999999999d+0) 
term(429) = term(429) * (-8.0d+0) 
term(430) = term(430) * (-3.9999999999999996d+0) 
term(431) = term(431) * (4.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(432) = term(432) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,l) * wm_interm_176_so_pt3(a,i,k,l)
term(433) = term(433) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,l) * wm_interm_176_so_pt3(a,i,j,l)
term(434) = term(434) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,k) * wm_interm_176_so_pt3(a,i,j,l)
term(435) = term(435) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,l) * wm_interm_176_so_pt3(a,k,j,l)
term(436) = term(436) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,l) * wm_interm_176_so_pt3(a,j,k,l)
term(437) = term(437) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,k) * wm_interm_176_so_pt3(a,j,i,l)
term(438) = term(438) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,i) * wm_interm_176_so_pt3(a,k,j,l)
term(439) = term(439) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,l) * wm_interm_176_so_pt3(a,k,i,l)
term(440) = term(440) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,l) * wm_interm_176_so_pt3(a,j,i,l)
term(441) = term(441) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,i) * wm_interm_176_so_pt3(a,j,k,l)
term(442) = term(442) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,l) * wm_interm_179_so_pt3(a,k,i,l)
term(443) = term(443) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,l) * wm_interm_179_so_pt3(a,j,i,l)
term(444) = term(444) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,l) * wm_interm_179_so_pt3(a,j,k,l)
term(445) = term(445) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,i,l) * wm_interm_179_so_pt3(a,k,j,l)
term(446) = term(446) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,k) * wm_interm_179_so_pt3(a,i,j,l)
term(447) = term(447) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,k) * wm_interm_179_so_pt3(a,j,i,l)
term(448) = term(448) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,i) * wm_interm_179_so_pt3(a,j,k,l)
term(449) = term(449) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,l,i) * wm_interm_179_so_pt3(a,k,j,l)
term(450) = term(450) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,j,l) * wm_interm_179_so_pt3(a,i,k,l)
term(451) = term(451) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,q,k,l) * wm_interm_179_so_pt3(a,i,j,l)
term(452) = term(452) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,b,l) * wm_interm_52_so_pt3(a,l,i,k)
term(453) = term(453) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,b,l) * wm_interm_52_so_pt3(a,l,k,i)
term(454) = term(454) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,b,l) * wm_interm_52_so_pt3(a,l,j,k)
term(455) = term(455) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,b,l) * wm_interm_52_so_pt3(a,l,j,i)
term(456) = term(456) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,j,p,l) * wm_interm_52_so_pt3(a,l,i,k)
term(457) = term(457) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,i,p,l) * wm_interm_52_so_pt3(a,l,j,k)
term(458) = term(458) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,i,p,l) * wm_interm_52_so_pt3(b,l,j,k)
term(459) = term(459) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,p,l) * wm_interm_52_so_pt3(a,l,i,k)
term(460) = term(460) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,j,p,l) * wm_interm_52_so_pt3(a,l,k,i)
term(461) = term(461) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,i,p,l) * wm_interm_52_so_pt3(a,l,j,k)
term(462) = term(462) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,k,p,l) * wm_interm_52_so_pt3(a,l,j,i)
term(463) = term(463) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,p,l) * wm_interm_52_so_pt3(b,l,i,k)
term(464) = term(464) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,i,p,l) * wm_interm_52_so_pt3(b,l,j,k)
term(465) = term(465) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_52_so_pt3(b,l,j,i)
term(466) = term(466) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,j,p,l) * wm_interm_52_so_pt3(b,l,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(432) = term(432) * (-3.0d+0) 
term(433) = term(433) * (4.0d+0) 
term(434) = term(434) * (-3.0d+0) 
term(435) = term(435) * (-3.0d+0) 
term(436) = term(436) * (4.0d+0) 
term(437) = term(437) * (4.0d+0) 
term(438) = term(438) * (2.0d+0) 
term(439) = term(439) * (2.0d+0) 
term(440) = term(440) * (-4.0d+0) 
term(441) = term(441) * (-4.0d+0) 
term(442) = term(442) * (-3.0d+0) 
term(443) = term(443) * (4.0d+0) 
term(444) = term(444) * (-3.0d+0) 
term(445) = term(445) * (4.0d+0) 
term(446) = term(446) * (4.0d+0) 
term(447) = term(447) * (-3.0d+0) 
term(448) = term(448) * (2.0d+0) 
term(449) = term(449) * (-4.0d+0) 
term(450) = term(450) * (2.0d+0) 
term(451) = term(451) * (-4.0d+0) 
term(453) = term(453) * (-0.5d+0) 
term(454) = term(454) * (-2.0d+0) 
term(456) = term(456) * (-0.5d+0) 
term(458) = term(458) * (-0.5d+0) 
term(459) = term(459) * (-2.0d+0) 
term(460) = term(460) * (2.0d+0) 
term(461) = term(461) * (4.0d+0) 
term(462) = term(462) * (-4.0d+0) 
term(463) = term(463) * (2.0d+0) 
term(464) = term(464) * (-2.0d+0) 
term(465) = term(465) * (2.0d+0) 
term(466) = term(466) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(467) = term(467) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, a,j,p,k) * wm_interm_2_so_pt3(a,j,k,i)
term(468) = term(468) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, a,j,p,k) * wm_interm_1_so_pt3(a,k,j,i)
term(469) = term(469) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, a,j,p,k) * wm_interm_3_so_pt3(a,k,j,i)
term(470) = term(470) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_91_so_pt3(a,j,i,k)
term(471) = term(471) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_90_so_pt3(a,j,i,k)
term(472) = term(472) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_85_so_pt3(a,j,i,k)
term(473) = term(473) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_86_so_pt3(a,j,i,k)
term(474) = term(474) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_87_so_pt3(a,j,i,k)
term(475) = term(475) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_119_so_pt3(a,j,i,k)
term(476) = term(476) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_118_so_pt3(a,j,i,k)
term(477) = term(477) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_114_so_pt3(a,j,i,k)
term(478) = term(478) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_115_so_pt3(a,j,i,k)
term(479) = term(479) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_116_so_pt3(a,j,i,k)
term(480) = term(480) + r2p(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_54_so_pt3(a,k,j,i)
term(481) = term(481) + r2p(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_57_so_pt3(a,j,k,i)
term(482) = term(482) + r2p(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_58_so_pt3(a,k,j,i)
term(483) = term(483) + r2p(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_59_so_pt3(a,k,j,i)
term(484) = term(484) + r2p(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_60_so_pt3(a,j,k,i)
term(485) = term(485) + r2p(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_61_so_pt3(a,j,k,i)
term(486) = term(486) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_54_so_pt3(a,k,j,i)
term(487) = term(487) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_54_so_pt3(a,j,k,i)
term(488) = term(488) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_57_so_pt3(a,j,k,i)
term(489) = term(489) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_58_so_pt3(a,k,j,i)
term(490) = term(490) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_57_so_pt3(a,k,j,i)
term(491) = term(491) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_58_so_pt3(a,j,k,i)
term(492) = term(492) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_59_so_pt3(a,k,j,i)
term(493) = term(493) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_59_so_pt3(a,j,k,i)
term(494) = term(494) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_60_so_pt3(a,j,k,i)
term(495) = term(495) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_60_so_pt3(a,k,j,i)
term(496) = term(496) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_61_so_pt3(a,j,k,i)
term(497) = term(497) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * wm_interm_61_so_pt3(a,k,j,i)
end do 
end do 
end do 
end do 

term(467) = term(467) * (-0.9999999999999999d+0) 
term(468) = term(468) * (-1.0d+0) 
term(469) = term(469) * (2.0d+0) 
term(470) = term(470) * (-0.9999999999999999d+0) 
term(471) = term(471) * (1.9999999999999998d+0) 
term(472) = term(472) * (2.0d+0) 
term(473) = term(473) * (-4.0d+0) 
term(474) = term(474) * (2.0d+0) 
term(475) = term(475) * (-1.9999999999999998d+0) 
term(476) = term(476) * (3.9999999999999996d+0) 
term(477) = term(477) * (4.0d+0) 
term(478) = term(478) * (-8.0d+0) 
term(479) = term(479) * (4.0d+0) 
term(480) = term(480) * (-0.5d+0) 
term(481) = term(481) * (-0.5d+0) 
term(483) = term(483) * (-0.5d+0) 
term(485) = term(485) * (-0.5d+0) 
term(486) = term(486) * (-2.0d+0) 
term(487) = term(487) * (2.0d+0) 
term(488) = term(488) * (-2.0d+0) 
term(489) = term(489) * (4.0d+0) 
term(490) = term(490) * (2.0d+0) 
term(491) = term(491) * (-4.0d+0) 
term(492) = term(492) * (-2.0d+0) 
term(493) = term(493) * (2.0d+0) 
term(494) = term(494) * (4.0d+0) 
term(495) = term(495) * (-4.0d+0) 
term(496) = term(496) * (-2.0d+0) 
term(497) = term(497) * (2.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(498) = term(498) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_5_so_pt3(b,j,l,k)
term(499) = term(499) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_5_so_pt3(b,j,l,k)
term(500) = term(500) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_5_so_pt3(b,j,k,l)
term(501) = term(501) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_5_so_pt3(b,j,l,k)
term(502) = term(502) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_5_so_pt3(b,j,l,k)
term(503) = term(503) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,l,j,i) * wm_interm_181_so_pt3(b,l,i,k)
term(504) = term(504) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,l,j,i) * wm_interm_180_so_pt3(b,i,l,k)
term(505) = term(505) + r2(vrdav_Rl, a,i,q,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_41_so_pt3(b,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(498) = term(498) * (-4.0d+0) 
term(499) = term(499) * (2.0d+0) 
term(500) = term(500) * (4.0d+0) 
term(501) = term(501) * (-8.0d+0) 
term(502) = term(502) * (4.0d+0) 
term(503) = term(503) * (-1.0d+0) 
term(504) = term(504) * (-1.0d+0) 
term(505) = term(505) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(506) = term(506) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * wm_interm_62_so_pt3(a,j)
term(507) = term(507) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * wm_interm_64_so_pt3(a,j)
term(508) = term(508) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * wm_interm_65_so_pt3(a,j)
term(509) = term(509) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * wm_interm_66_so_pt3(a,j)
end do 
end do 
end do 

term(506) = term(506) * (1.9999999999999998d+0) 
term(507) = term(507) * (-2.0d+0) 
term(508) = term(508) * (-0.9999999999999999d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(510) = term(510) + r2p(vrdav_Rr, p,i,a,j) * s1(q,i) * wm_interm_62_so_pt3(a,j)
term(511) = term(511) + r2p(vrdav_Rr, p,i,a,j) * s1(q,i) * wm_interm_64_so_pt3(a,j)
term(512) = term(512) + r2p(vrdav_Rr, p,i,a,j) * s1(q,i) * wm_interm_65_so_pt3(a,j)
term(513) = term(513) + r2p(vrdav_Rr, p,i,a,j) * s1(q,i) * wm_interm_66_so_pt3(a,j)
end do 
end do 
end do 

term(510) = term(510) * (-3.9999999999999996d+0) 
term(511) = term(511) * (4.0d+0) 
term(512) = term(512) * (1.9999999999999998d+0) 
term(513) = term(513) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(514) = term(514) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,j,l,i) * wm_interm_181_so_pt3(b,i,l,k)
term(515) = term(515) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,j,l,i) * wm_interm_181_so_pt3(b,l,i,k)
term(516) = term(516) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,j,l,i) * wm_interm_180_so_pt3(b,l,i,k)
term(517) = term(517) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,j,l,i) * wm_interm_180_so_pt3(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(514) = term(514) * (-1.0d+0) 
term(515) = term(515) * (2.0d+0) 
term(516) = term(516) * (-1.0d+0) 
term(517) = term(517) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(518) = term(518) + r2(vrdav_Rl, a,k,q,j) * t3(nocc, nactive, a,b,p,j,l,i) * wm_interm_41_so_pt3(b,k,l,i)
term(519) = term(519) + r2(vrdav_Rl, a,k,q,j) * t3(nocc, nactive, a,b,p,j,l,i) * wm_interm_41_so_pt3(b,k,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(518) = term(518) * (-2.0d+0) 
term(519) = term(519) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(520) = term(520) + r2(vrdav_Rl, a,k,q,j) * t3(nocc, nactive, a,b,p,k,l,i) * wm_interm_41_so_pt3(b,j,l,i)
term(521) = term(521) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,k,l,i) * wm_interm_181_so_pt3(b,i,l,j)
term(522) = term(522) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,k,l,i) * wm_interm_181_so_pt3(b,l,i,j)
term(523) = term(523) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,k,l,i) * wm_interm_180_so_pt3(b,l,i,j)
term(524) = term(524) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,k,l,i) * wm_interm_180_so_pt3(b,i,l,j)
term(525) = term(525) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,b,i) * wm_interm_52_so_pt3(a,l,j,k)
term(526) = term(526) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,b,k) * wm_interm_52_so_pt3(a,l,j,i)
term(527) = term(527) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, b,l,p,k) * wm_interm_52_so_pt3(a,l,j,i)
term(528) = term(528) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, a,l,p,k) * wm_interm_52_so_pt3(b,l,j,i)
term(529) = term(529) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,l,p,i) * wm_interm_52_so_pt3(a,l,j,k)
term(530) = term(530) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, b,l,p,k) * wm_interm_52_so_pt3(a,l,j,i)
term(531) = term(531) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,l,p,i) * wm_interm_52_so_pt3(b,l,j,k)
term(532) = term(532) + r3(vrdav_Rl, a,j,b,k,q,i) * r2m(vrdav_Rr, a,l,p,k) * wm_interm_52_so_pt3(b,l,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(520) = term(520) * (4.0d+0) 
term(521) = term(521) * (2.0d+0) 
term(522) = term(522) * (-4.0d+0) 
term(523) = term(523) * (2.0d+0) 
term(524) = term(524) * (-4.0d+0) 
term(526) = term(526) * (-2.0d+0) 
term(528) = term(528) * (-0.5d+0) 
term(529) = term(529) * (-4.0d+0) 
term(530) = term(530) * (4.0d+0) 
term(531) = term(531) * (2.0d+0) 
term(532) = term(532) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(533) = term(533) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,j,i,k) * wm_interm_8_so_pt3(b,k)
term(534) = term(534) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,j,i,k) * wm_interm_9_so_pt3(b,k)
end do 
end do 
end do 
end do 
end do 

term(533) = term(533) * (-4.0d+0) 
term(534) = term(534) * (2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(535) = term(535) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,k,i,j) * wm_interm_8_so_pt3(b,k)
term(536) = term(536) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,k,i,j) * wm_interm_9_so_pt3(b,k)
end do 
end do 
end do 
end do 
end do 

term(535) = term(535) * (2.0d+0) 
term(536) = term(536) * (-1.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(537) = term(537) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, p,j,a,k) * wm_interm_1_so_pt3(a,j,k,i)
term(538) = term(538) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, p,j,a,k) * wm_interm_1_so_pt3(a,k,j,i)
term(539) = term(539) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, p,j,a,k) * wm_interm_2_so_pt3(a,k,j,i)
term(540) = term(540) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, p,j,a,k) * wm_interm_2_so_pt3(a,j,k,i)
term(541) = term(541) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, p,j,a,k) * wm_interm_3_so_pt3(a,j,k,i)
term(542) = term(542) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, p,j,a,k) * wm_interm_3_so_pt3(a,k,j,i)
term(543) = term(543) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_54_so_pt3(a,j,k,i)
term(544) = term(544) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_54_so_pt3(a,k,j,i)
term(545) = term(545) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_57_so_pt3(a,k,j,i)
term(546) = term(546) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_58_so_pt3(a,j,k,i)
term(547) = term(547) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_57_so_pt3(a,j,k,i)
term(548) = term(548) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_58_so_pt3(a,k,j,i)
term(549) = term(549) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_59_so_pt3(a,j,k,i)
term(550) = term(550) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_59_so_pt3(a,k,j,i)
term(551) = term(551) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_60_so_pt3(a,k,j,i)
term(552) = term(552) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_60_so_pt3(a,j,k,i)
term(553) = term(553) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_61_so_pt3(a,k,j,i)
term(554) = term(554) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * wm_interm_61_so_pt3(a,j,k,i)
end do 
end do 
end do 
end do 

term(537) = term(537) * (-1.0d+0) 
term(538) = term(538) * (2.0d+0) 
term(539) = term(539) * (-0.9999999999999999d+0) 
term(540) = term(540) * (1.9999999999999998d+0) 
term(541) = term(541) * (2.0d+0) 
term(542) = term(542) * (-4.0d+0) 
term(543) = term(543) * (-0.5d+0) 
term(545) = term(545) * (-0.5d+0) 
term(548) = term(548) * (-2.0d+0) 
term(549) = term(549) * (-0.5d+0) 
term(552) = term(552) * (-2.0d+0) 
term(553) = term(553) * (-0.5d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(555) = term(555) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,i,k,j) * wm_interm_8_so_pt3(b,k)
term(556) = term(556) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,i,k,j) * wm_interm_9_so_pt3(b,k)
end do 
end do 
end do 
end do 
end do 

term(555) = term(555) * (-3.9999999999999996d+0) 
term(556) = term(556) * (1.9999999999999998d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(557) = term(557) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_8_so_pt3(b,k)
term(558) = term(558) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_9_so_pt3(b,k)
end do 
end do 
end do 
end do 
end do 

term(557) = term(557) * (7.999999999999999d+0) 
term(558) = term(558) * (-3.9999999999999996d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(559) = term(559) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,i,k) * wm_interm_8_so_pt3(b,k)
term(560) = term(560) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,i,k) * wm_interm_9_so_pt3(b,k)
term(561) = term(561) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,i,k) * wm_interm_8_so_pt3(b,k)
term(562) = term(562) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,i,k) * wm_interm_8_so_pt3(b,k)
term(563) = term(563) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,i,k) * wm_interm_9_so_pt3(b,k)
term(564) = term(564) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,i,k) * wm_interm_9_so_pt3(b,k)
end do 
end do 
end do 
end do 
end do 

term(559) = term(559) * (2.0d+0) 
term(560) = term(560) * (-1.0d+0) 
term(561) = term(561) * (-8.0d+0) 
term(562) = term(562) * (4.0d+0) 
term(563) = term(563) * (4.0d+0) 
term(564) = term(564) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(565) = term(565) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_8_so_pt3(b,k)
term(566) = term(566) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_9_so_pt3(b,k)
term(567) = term(567) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_8_so_pt3(b,k)
term(568) = term(568) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_8_so_pt3(b,k)
term(569) = term(569) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_9_so_pt3(b,k)
term(570) = term(570) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_9_so_pt3(b,k)
term(571) = term(571) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,j,k,i) * wm_interm_186_so_pt3(b,i)
term(572) = term(572) + r2(vrdav_Rl, a,k,q,j) * t3(nocc, nactive, a,b,p,j,k,i) * wm_interm_46_so_pt3(b,i)
end do 
end do 
end do 
end do 
end do 

term(565) = term(565) * (-3.9999999999999996d+0) 
term(566) = term(566) * (1.9999999999999998d+0) 
term(567) = term(567) * (15.999999999999998d+0) 
term(568) = term(568) * (-7.999999999999999d+0) 
term(569) = term(569) * (-7.999999999999999d+0) 
term(570) = term(570) * (3.9999999999999996d+0) 
term(571) = term(571) * (-2.0d+0) 
term(572) = term(572) * (-2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(573) = term(573) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,j,i) * wm_interm_162_so_pt3(a,i,l,k)
term(574) = term(574) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,j,i) * wm_interm_161_so_pt3(a,i,l,k)
term(575) = term(575) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,k,i) * wm_interm_161_so_pt3(a,i,l,j)
term(576) = term(576) + t3(nocc, nactive, a,b,q,k,i,j) * t2(b,p,l,i) * wm_interm_162_so_pt3(a,l,j,k)
term(577) = term(577) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,l,i) * wm_interm_162_so_pt3(a,i,j,k)
term(578) = term(578) + t3(nocc, nactive, a,b,q,k,i,j) * t2(b,p,l,i) * wm_interm_161_so_pt3(a,l,k,j)
term(579) = term(579) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,l,i) * wm_interm_161_so_pt3(a,i,k,j)
term(580) = term(580) + t3(nocc, nactive, a,b,q,k,i,j) * t2(b,p,l,i) * wm_interm_161_so_pt3(a,l,j,k)
term(581) = term(581) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,l,i) * wm_interm_161_so_pt3(a,i,j,k)
term(582) = term(582) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,j,i) * wm_interm_163_so_pt3(a,i,l,k)
term(583) = term(583) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,k,i) * wm_interm_163_so_pt3(a,i,l,j)
term(584) = term(584) + t3(nocc, nactive, a,b,q,k,i,j) * t2(b,p,l,i) * wm_interm_163_so_pt3(a,l,k,j)
term(585) = term(585) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,l,i) * wm_interm_163_so_pt3(a,i,k,j)
term(586) = term(586) + t3(nocc, nactive, a,b,q,k,i,j) * t2(b,p,l,i) * wm_interm_163_so_pt3(a,l,j,k)
term(587) = term(587) + t3(nocc, nactive, a,b,q,k,l,j) * t2(b,p,l,i) * wm_interm_163_so_pt3(a,i,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(573) = term(573) * (-1.0d+0) 
term(574) = term(574) * (2.0d+0) 
term(575) = term(575) * (-0.9999999999999999d+0) 
term(576) = term(576) * (-1.0d+0) 
term(577) = term(577) * (2.0d+0) 
term(578) = term(578) * (-1.0d+0) 
term(579) = term(579) * (2.0d+0) 
term(580) = term(580) * (2.0d+0) 
term(581) = term(581) * (-4.0d+0) 
term(582) = term(582) * (4.0d+0) 
term(583) = term(583) * (-4.0d+0) 
term(584) = term(584) * (-4.0d+0) 
term(585) = term(585) * (8.0d+0) 
term(586) = term(586) * (4.0d+0) 
term(587) = term(587) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(588) = term(588) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_1_so_pt3(a,j,k,i)
term(589) = term(589) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_1_so_pt3(a,k,j,i)
term(590) = term(590) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_2_so_pt3(a,k,j,i)
term(591) = term(591) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_2_so_pt3(a,j,k,i)
term(592) = term(592) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_3_so_pt3(a,j,k,i)
term(593) = term(593) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_3_so_pt3(a,k,j,i)
term(594) = term(594) + r2(vrdav_Rl, a,k,q,j) * t1(p,i) * wm_interm_12_so_pt3(a,j,k,i)
term(595) = term(595) + r2(vrdav_Rl, a,k,q,j) * t1(p,i) * wm_interm_12_so_pt3(a,k,j,i)
term(596) = term(596) + r2(vrdav_Rl, a,k,q,j) * t1(p,i) * wm_interm_14_so_pt3(a,j,k,i)
term(597) = term(597) + r2(vrdav_Rl, a,k,q,j) * t1(p,i) * wm_interm_15_so_pt3(a,k,j,i)
term(598) = term(598) + r2(vrdav_Rl, a,k,q,j) * t1(p,i) * wm_interm_13_so_pt3(a,j,k,i)
term(599) = term(599) + r2(vrdav_Rl, a,k,q,j) * t1(p,i) * wm_interm_14_so_pt3(a,k,j,i)
term(600) = term(600) + r2(vrdav_Rl, a,k,q,j) * t1(p,i) * wm_interm_15_so_pt3(a,j,k,i)
term(601) = term(601) + r2(vrdav_Rl, a,k,q,j) * t1(p,i) * wm_interm_13_so_pt3(a,k,j,i)
term(602) = term(602) + s1(q,j) * t2(a,p,k,i) * wm_interm_199_so_pt3(a,i,k,j)
term(603) = term(603) + s1(q,j) * t2(a,p,k,i) * wm_interm_200_so_pt3(a,i,k,j)
term(604) = term(604) + s1(q,j) * t2(a,p,k,i) * wm_interm_198_so_pt3(a,i,k,j)
term(605) = term(605) + s1(q,j) * t2(a,p,k,i) * wm_interm_196_so_pt3(a,k,i,j)
term(606) = term(606) + s1(q,j) * t2(a,p,k,i) * wm_interm_201_so_pt3(a,k,i,j)
term(607) = term(607) + s1(q,j) * t2(a,p,k,i) * wm_interm_197_so_pt3(a,k,i,j)
term(608) = term(608) + s1(q,j) * t2(a,p,k,i) * wm_interm_195_so_pt3(a,i,k,j)
term(609) = term(609) + s1(q,j) * t2(a,p,k,i) * wm_interm_196_so_pt3(a,i,k,j)
term(610) = term(610) + s1(q,j) * t2(a,p,k,i) * wm_interm_199_so_pt3(a,k,i,j)
term(611) = term(611) + s1(q,j) * t2(a,p,k,i) * wm_interm_200_so_pt3(a,k,i,j)
term(612) = term(612) + s1(q,j) * t2(a,p,k,i) * wm_interm_197_so_pt3(a,i,k,j)
term(613) = term(613) + s1(q,j) * t2(a,p,k,i) * wm_interm_198_so_pt3(a,k,i,j)
term(614) = term(614) + s1(q,j) * t2(a,p,k,i) * wm_interm_192_so_pt3(a,k,i,j)
term(615) = term(615) + s1(q,j) * t2(a,p,k,i) * wm_interm_193_so_pt3(a,k,i,j)
term(616) = term(616) + s1(q,j) * t2(a,p,k,i) * wm_interm_194_so_pt3(a,k,i,j)
term(617) = term(617) + s1(q,j) * t2(a,p,k,i) * wm_interm_192_so_pt3(a,i,k,j)
term(618) = term(618) + s1(q,j) * t2(a,p,k,i) * wm_interm_193_so_pt3(a,i,k,j)
term(619) = term(619) + s1(q,j) * t2(a,p,k,i) * wm_interm_194_so_pt3(a,i,k,j)
term(620) = term(620) + s1(q,j) * t2(a,p,k,i) * wm_interm_214_so_pt3(a,i,k,j)
term(621) = term(621) + s1(q,j) * t2(a,p,k,i) * wm_interm_215_so_pt3(a,i,k,j)
term(622) = term(622) + s1(q,j) * t2(a,p,k,i) * wm_interm_213_so_pt3(a,k,i,j)
term(623) = term(623) + s1(q,j) * t2(a,p,k,i) * wm_interm_212_so_pt3(a,k,i,j)
term(624) = term(624) + s1(q,j) * t2(a,p,k,i) * wm_interm_212_so_pt3(a,i,k,j)
term(625) = term(625) + s1(q,j) * t2(a,p,k,i) * wm_interm_213_so_pt3(a,i,k,j)
term(626) = term(626) + s1(q,j) * t2(a,p,k,i) * wm_interm_214_so_pt3(a,k,i,j)
term(627) = term(627) + s1(q,j) * t2(a,p,k,i) * wm_interm_215_so_pt3(a,k,i,j)
term(628) = term(628) + s1(q,j) * t2(a,p,k,i) * wm_interm_210_so_pt3(a,k,i,j)
term(629) = term(629) + s1(q,j) * t2(a,p,k,i) * wm_interm_211_so_pt3(a,k,i,j)
term(630) = term(630) + s1(q,j) * t2(a,p,k,i) * wm_interm_210_so_pt3(a,i,k,j)
term(631) = term(631) + s1(q,j) * t2(a,p,k,i) * wm_interm_211_so_pt3(a,i,k,j)
end do 
end do 
end do 
end do 

term(588) = term(588) * (-4.0d+0) 
term(589) = term(589) * (4.0d+0) 
term(590) = term(590) * (-4.0d+0) 
term(591) = term(591) * (3.9999999999999996d+0) 
term(592) = term(592) * (8.0d+0) 
term(593) = term(593) * (-8.0d+0) 
term(594) = term(594) * (-6.0d+0) 
term(595) = term(595) * (8.0d+0) 
term(596) = term(596) * (-3.0d+0) 
term(597) = term(597) * (-3.0d+0) 
term(598) = term(598) * (8.0d+0) 
term(599) = term(599) * (2.0d+0) 
term(600) = term(600) * (2.0d+0) 
term(601) = term(601) * (-8.0d+0) 
term(602) = term(602) * (-0.5d+0) 
term(604) = term(604) * (-0.5d+0) 
term(605) = term(605) * (-0.5d+0) 
term(606) = term(606) * (-0.5d+0) 
term(608) = term(608) * (-0.5d+0) 
term(611) = term(611) * (-2.0d+0) 
term(612) = term(612) * (-0.5d+0) 
term(614) = term(614) * (-0.5d+0) 
term(616) = term(616) * (-0.5d+0) 
term(618) = term(618) * (-2.0d+0) 
term(620) = term(620) * (-2.0d+0) 
term(621) = term(621) * (2.0d+0) 
term(622) = term(622) * (-2.0d+0) 
term(623) = term(623) * (2.0d+0) 
term(624) = term(624) * (-2.0d+0) 
term(625) = term(625) * (2.0d+0) 
term(626) = term(626) * (4.0d+0) 
term(627) = term(627) * (-4.0d+0) 
term(628) = term(628) * (-2.0d+0) 
term(629) = term(629) * (2.0d+0) 
term(630) = term(630) * (4.0d+0) 
term(631) = term(631) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(632) = term(632) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_85_so_pt3(a,j,k,i)
term(633) = term(633) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_86_so_pt3(a,j,k,i)
term(634) = term(634) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_87_so_pt3(a,j,k,i)
term(635) = term(635) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_90_so_pt3(a,j,k,i)
term(636) = term(636) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_114_so_pt3(a,j,k,i)
term(637) = term(637) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_115_so_pt3(a,j,k,i)
term(638) = term(638) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_116_so_pt3(a,j,k,i)
term(639) = term(639) + r1(vrdav_Rl, q,j) * t2(a,p,k,i) * wm_interm_118_so_pt3(a,j,k,i)
end do 
end do 
end do 
end do 

term(632) = term(632) * (-1.0d+0) 
term(633) = term(633) * (2.0d+0) 
term(634) = term(634) * (-1.0d+0) 
term(635) = term(635) * (-0.9999999999999999d+0) 
term(636) = term(636) * (-2.0d+0) 
term(637) = term(637) * (4.0d+0) 
term(638) = term(638) * (-2.0d+0) 
term(639) = term(639) * (-1.9999999999999998d+0) 

do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(640) = term(640) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,k,i,j) * wm_interm_8_so_pt3(b,k)
term(641) = term(641) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,k,i,j) * wm_interm_9_so_pt3(b,k)
term(642) = term(642) + t3(nocc, nactive, a,b,q,k,i,j) * t2(b,p,k,i) * wm_interm_164_so_pt3(a,j)
term(643) = term(643) + t3(nocc, nactive, a,b,q,k,i,j) * t2(b,p,k,i) * wm_interm_165_so_pt3(a,j)
term(644) = term(644) + t3(nocc, nactive, a,b,q,k,i,j) * t2(b,p,k,i) * wm_interm_166_so_pt3(a,j)
term(645) = term(645) + t3(nocc, nactive, a,b,q,k,i,j) * t2(b,p,k,i) * wm_interm_168_so_pt3(a,j)
term(646) = term(646) + t3(nocc, nactive, a,b,q,k,i,j) * t2(b,p,k,i) * wm_interm_169_so_pt3(a,j)
end do 
end do 
end do 
end do 
end do 

term(640) = term(640) * (4.0d+0) 
term(641) = term(641) * (-2.0d+0) 
term(642) = term(642) * (-0.9999999999999999d+0) 
term(643) = term(643) * (1.9999999999999998d+0) 
term(644) = term(644) * (-0.9999999999999999d+0) 
term(645) = term(645) * (-3.9999999999999996d+0) 
term(646) = term(646) * (3.9999999999999996d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(647) = term(647) + r2(vrdav_Rl, a,k,q,j) * t3(nocc, nactive, a,b,p,k,l,i) * wm_interm_41_so_pt3(b,j,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(647) = term(647) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(648) = term(648) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,k,j) * wm_interm_8_so_pt3(b,k)
term(649) = term(649) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,k,j) * wm_interm_9_so_pt3(b,k)
term(650) = term(650) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,j,i) * wm_interm_164_so_pt3(a,k)
term(651) = term(651) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,j,i) * wm_interm_165_so_pt3(a,k)
term(652) = term(652) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,j,i) * wm_interm_166_so_pt3(a,k)
term(653) = term(653) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,k,i) * wm_interm_164_so_pt3(a,j)
term(654) = term(654) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,k,i) * wm_interm_165_so_pt3(a,j)
term(655) = term(655) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,k,i) * wm_interm_166_so_pt3(a,j)
term(656) = term(656) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,j,i) * wm_interm_168_so_pt3(a,k)
term(657) = term(657) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,j,i) * wm_interm_169_so_pt3(a,k)
term(658) = term(658) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,k,i) * wm_interm_168_so_pt3(a,j)
term(659) = term(659) + t3(nocc, nactive, a,b,q,i,k,j) * t2(b,p,k,i) * wm_interm_169_so_pt3(a,j)
term(660) = term(660) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,j,i) * wm_interm_187_so_pt3(b,k)
term(661) = term(661) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,k,i) * wm_interm_187_so_pt3(b,j)
term(662) = term(662) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(b,p,k,i) * wm_interm_187_so_pt3(a,j)
term(663) = term(663) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,j,i) * wm_interm_188_so_pt3(b,k)
term(664) = term(664) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,k,i) * wm_interm_188_so_pt3(b,j)
term(665) = term(665) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(b,p,k,i) * wm_interm_188_so_pt3(a,j)
term(666) = term(666) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,j,i) * wm_interm_189_so_pt3(b,k)
term(667) = term(667) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,k,i) * wm_interm_189_so_pt3(b,j)
term(668) = term(668) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(b,p,k,i) * wm_interm_189_so_pt3(a,j)
term(669) = term(669) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,j,i) * wm_interm_190_so_pt3(b,k)
term(670) = term(670) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,k,i) * wm_interm_190_so_pt3(b,j)
term(671) = term(671) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(b,p,k,i) * wm_interm_190_so_pt3(a,j)
term(672) = term(672) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,j,i) * wm_interm_191_so_pt3(b,k)
term(673) = term(673) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(a,p,k,i) * wm_interm_191_so_pt3(b,j)
term(674) = term(674) + r3(vrdav_Rl, a,k,b,i,q,j) * t2(b,p,k,i) * wm_interm_191_so_pt3(a,j)
end do 
end do 
end do 
end do 
end do 

term(648) = term(648) * (-7.999999999999999d+0) 
term(649) = term(649) * (3.9999999999999996d+0) 
term(650) = term(650) * (-0.9999999999999999d+0) 
term(651) = term(651) * (1.9999999999999998d+0) 
term(652) = term(652) * (-0.9999999999999999d+0) 
term(653) = term(653) * (1.9999999999999998d+0) 
term(654) = term(654) * (-3.9999999999999996d+0) 
term(655) = term(655) * (1.9999999999999998d+0) 
term(656) = term(656) * (-3.9999999999999996d+0) 
term(657) = term(657) * (3.9999999999999996d+0) 
term(658) = term(658) * (7.999999999999999d+0) 
term(659) = term(659) * (-7.999999999999999d+0) 
term(660) = term(660) * (-1.0d+0) 
term(661) = term(661) * (2.0d+0) 
term(662) = term(662) * (-1.0d+0) 
term(663) = term(663) * (0.5d+0) 
term(664) = term(664) * (-1.0d+0) 
term(665) = term(665) * (0.5d+0) 
term(666) = term(666) * (0.5d+0) 
term(667) = term(667) * (-1.0d+0) 
term(668) = term(668) * (0.5d+0) 
term(669) = term(669) * (-2.0d+0) 
term(670) = term(670) * (4.0d+0) 
term(671) = term(671) * (-2.0d+0) 
term(672) = term(672) * (2.0d+0) 
term(673) = term(673) * (-4.0d+0) 
term(674) = term(674) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
term(675) = term(675) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,i,l,k) * wm_interm_5_so_pt3(b,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(675) = term(675) * (-1.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
term(676) = term(676) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,j,l,k) * wm_interm_5_so_pt3(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(676) = term(676) * (2.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(677) = term(677) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,l,k,j) * wm_interm_5_so_pt3(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(677) = term(677) * (-1.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(678) = term(678) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_5_so_pt3(b,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(678) = term(678) * (2.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(679) = term(679) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,a,l) * wm_interm_52_so_pt3(b,l,i,k)
term(680) = term(680) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,i,a,l) * wm_interm_52_so_pt3(b,l,j,k)
term(681) = term(681) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_52_so_pt3(b,l,j,i)
term(682) = term(682) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,j,a,l) * wm_interm_52_so_pt3(b,l,k,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(679) = term(679) * (-0.5d+0) 
term(681) = term(681) * (-0.5d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do l = 1, nocc 
term(683) = term(683) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,a,j) * wm_interm_52_so_pt3(b,l,i,k)
term(684) = term(684) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,a,j) * wm_interm_52_so_pt3(b,l,k,i)
term(685) = term(685) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,a,i) * wm_interm_52_so_pt3(b,l,k,j)
term(686) = term(686) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,a,k) * wm_interm_52_so_pt3(b,l,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(684) = term(684) * (-2.0d+0) 
term(686) = term(686) * (-0.5d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
do l = 1, nocc 
term(687) = term(687) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,a,i) * wm_interm_52_so_pt3(b,l,j,k)
term(688) = term(688) + r3(vrdav_Rl, a,j,b,k,q,i) * r2p(vrdav_Rr, p,l,a,k) * wm_interm_52_so_pt3(b,l,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(687) = term(687) * (-0.5d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(689) = term(689) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,i,l,k) * wm_interm_5_so_pt3(b,j,k,l)
term(690) = term(690) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,l,k) * wm_interm_5_so_pt3(b,j,l,k)
term(691) = term(691) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,i,l,k) * wm_interm_5_so_pt3(b,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(689) = term(689) * (2.0d+0) 
term(690) = term(690) * (-2.0d+0) 
term(691) = term(691) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(692) = term(692) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,j,l,k) * wm_interm_5_so_pt3(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(692) = term(692) * (-4.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(693) = term(693) + r2p(vrdav_Rr, p,i,a,j) * t3(nocc, nactive, a,b,q,l,k,j) * wm_interm_5_so_pt3(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(693) = term(693) * (2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(694) = term(694) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_31_so_pt3(a,i,k,j)
term(695) = term(695) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_35_so_pt3(a,i,k,j)
term(696) = term(696) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_125_so_pt3(a,i,k,j)
term(697) = term(697) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_126_so_pt3(a,i,k,j)
term(698) = term(698) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_135_so_pt3(a,i,k,j)
term(699) = term(699) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_37_so_pt3(a,i,k,j)
term(700) = term(700) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_39_so_pt3(a,i,k,j)
term(701) = term(701) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_138_so_pt3(a,i,k,j)
term(702) = term(702) + s2(a,q,k,j) * t1(p,i) * wm_interm_174_so_pt3(a,i,k,j)
term(703) = term(703) + s2(a,q,k,j) * t1(p,i) * wm_interm_175_so_pt3(a,i,k,j)
term(704) = term(704) + s2(a,q,k,j) * t1(p,i) * wm_interm_178_so_pt3(a,i,k,j)
term(705) = term(705) + s2(a,q,k,j) * t1(p,i) * wm_interm_173_so_pt3(a,i,k,j)
end do 
end do 
end do 
end do 

term(694) = term(694) * (4.0d+0) 
term(695) = term(695) * (-8.0d+0) 
term(696) = term(696) * (-2.0d+0) 
term(697) = term(697) * (4.0d+0) 
term(698) = term(698) * (-1.0d+0) 
term(699) = term(699) * (-1.0d+0) 
term(700) = term(700) * (2.0d+0) 
term(701) = term(701) * (2.0d+0) 
term(702) = term(702) * (-6.0d+0) 
term(703) = term(703) * (8.0d+0) 
term(704) = term(704) * (-3.0d+0) 
term(705) = term(705) * (2.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(706) = term(706) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_5_so_pt3(b,j,k,l)
term(707) = term(707) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,l,k,i) * wm_interm_5_so_pt3(b,j,k,l)
term(708) = term(708) + r2(vrdav_Rl, a,i,q,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_41_so_pt3(b,j,k,l)
term(709) = term(709) + r2(vrdav_Rl, a,k,q,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_41_so_pt3(b,j,l,i)
term(710) = term(710) + s2(a,q,i,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_181_so_pt3(b,l,k,j)
term(711) = term(711) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_181_so_pt3(b,l,i,j)
term(712) = term(712) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_180_so_pt3(b,l,i,j)
term(713) = term(713) + s2(a,q,i,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_180_so_pt3(b,l,k,j)
term(714) = term(714) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_181_so_pt3(b,i,l,j)
term(715) = term(715) + s2(a,q,i,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_181_so_pt3(b,k,l,j)
term(716) = term(716) + s2(a,q,i,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_180_so_pt3(b,k,l,j)
term(717) = term(717) + s2(a,q,k,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_180_so_pt3(b,i,l,j)
term(718) = term(718) + r2(vrdav_Rl, a,k,q,j) * t3(nocc, nactive, a,b,p,l,k,i) * wm_interm_41_so_pt3(b,j,i,l)
term(719) = term(719) + r2(vrdav_Rl, a,k,q,j) * t3(nocc, nactive, a,b,p,l,j,i) * wm_interm_41_so_pt3(b,k,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(706) = term(706) * (-1.0d+0) 
term(707) = term(707) * (-2.0d+0) 
term(708) = term(708) * (-2.0d+0) 
term(709) = term(709) * (-2.0d+0) 
term(710) = term(710) * (-0.9999999999999999d+0) 
term(711) = term(711) * (2.0d+0) 
term(712) = term(712) * (-1.0d+0) 
term(713) = term(713) * (1.9999999999999998d+0) 
term(714) = term(714) * (-1.0d+0) 
term(715) = term(715) * (1.9999999999999998d+0) 
term(716) = term(716) * (-0.9999999999999999d+0) 
term(717) = term(717) * (2.0d+0) 
term(718) = term(718) * (4.0d+0) 
term(719) = term(719) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(720) = term(720) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_31_so_pt3(a,i,j,k)
term(721) = term(721) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_35_so_pt3(a,i,j,k)
term(722) = term(722) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_125_so_pt3(a,i,j,k)
term(723) = term(723) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_126_so_pt3(a,i,j,k)
term(724) = term(724) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_135_so_pt3(a,i,j,k)
term(725) = term(725) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_39_so_pt3(a,i,j,k)
term(726) = term(726) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * wm_interm_37_so_pt3(a,i,j,k)
term(727) = term(727) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * wm_interm_138_so_pt3(a,i,j,k)
term(728) = term(728) + s2(a,q,k,j) * t1(p,i) * wm_interm_178_so_pt3(a,i,j,k)
term(729) = term(729) + s2(a,q,k,j) * t1(p,i) * wm_interm_174_so_pt3(a,i,j,k)
term(730) = term(730) + s2(a,q,k,j) * t1(p,i) * wm_interm_175_so_pt3(a,i,j,k)
term(731) = term(731) + s2(a,q,k,j) * t1(p,i) * wm_interm_173_so_pt3(a,i,j,k)
end do 
end do 
end do 
end do 

term(720) = term(720) * (-2.0d+0) 
term(721) = term(721) * (4.0d+0) 
term(722) = term(722) * (4.0d+0) 
term(723) = term(723) * (-8.0d+0) 
term(724) = term(724) * (2.0d+0) 
term(725) = term(725) * (-1.0d+0) 
term(726) = term(726) * (2.0d+0) 
term(727) = term(727) * (-1.0d+0) 
term(728) = term(728) * (2.0d+0) 
term(729) = term(729) * (8.0d+0) 
term(730) = term(730) * (-8.0d+0) 
term(731) = term(731) * (-3.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(732) = term(732) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,l,j,k) * wm_interm_5_so_pt3(b,i,k,l)
term(733) = term(733) + r2p(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,l,i,k) * wm_interm_5_so_pt3(b,j,l,k)
term(734) = term(734) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,l,j,k) * wm_interm_5_so_pt3(b,i,k,l)
term(735) = term(735) + r2m(vrdav_Rr, a,i,p,j) * t3(nocc, nactive, a,b,q,l,i,k) * wm_interm_5_so_pt3(b,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(732) = term(732) * (-0.9999999999999999d+0) 
term(733) = term(733) * (-0.9999999999999999d+0) 
term(734) = term(734) * (-1.9999999999999998d+0) 
term(735) = term(735) * (-1.9999999999999998d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(736) = term(736) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,j,l,k) * wm_interm_5_so_pt3(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(736) = term(736) * (4.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(737) = term(737) + r2m(vrdav_Rr, a,j,p,i) * t3(nocc, nactive, a,b,q,l,k,j) * wm_interm_5_so_pt3(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(737) = term(737) * (-2.0d+0) 


    calc_D_vv_wm_so_cc3_pt3 = zero
    do s = 0, 737
    calc_D_vv_wm_so_cc3_pt3 = calc_D_vv_wm_so_cc3_pt3 + term(s)
    end do

    end function calc_D_vv_wm_so_cc3_pt3
    

  end module so_cc3_pt3b
