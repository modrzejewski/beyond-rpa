module auto2e_KetTransform_9_5
use arithmetic
use math_constants
implicit none

integer, dimension(55, 21), parameter :: RCopyIdx_9_5 = reshape([1, 16, 30, 43, 55, &
    66, 76, 85, 93, 100, 121, 135, 148, 160, 171, 181, 190, 198, 205, 226, 239, 251, &
    262, 272, 281, 289, 296, 317, 329, 340, 350, 359, 367, 374, 395, 406, 416, 425, &
    433, 440, 461, 471, 480, 488, 495, 516, 525, 533, 540, 561, 569, 576, 597, 604, &
    625, 16, 30, 43, 55, 66, 76, 85, 93, 100, 106, 135, 148, 160, 171, 181, 190, &
    198, 205, 211, 239, 251, 262, 272, 281, 289, 296, 302, 329, 340, 350, 359, 367, &
    374, 380, 406, 416, 425, 433, 440, 446, 471, 480, 488, 495, 501, 525, 533, 540, &
    546, 569, 576, 582, 604, 610, 631, 30, 43, 55, 66, 76, 85, 93, 100, 106, 111, &
    148, 160, 171, 181, 190, 198, 205, 211, 216, 251, 262, 272, 281, 289, 296, 302, &
    307, 340, 350, 359, 367, 374, 380, 385, 416, 425, 433, 440, 446, 451, 480, 488, &
    495, 501, 506, 533, 540, 546, 551, 576, 582, 587, 610, 615, 636, 43, 55, 66, &
    76, 85, 93, 100, 106, 111, 115, 160, 171, 181, 190, 198, 205, 211, 216, 220, &
    262, 272, 281, 289, 296, 302, 307, 311, 350, 359, 367, 374, 380, 385, 389, 425, &
    433, 440, 446, 451, 455, 488, 495, 501, 506, 510, 540, 546, 551, 555, 582, 587, &
    591, 615, 619, 640, 55, 66, 76, 85, 93, 100, 106, 111, 115, 118, 171, 181, 190, &
    198, 205, 211, 216, 220, 223, 272, 281, 289, 296, 302, 307, 311, 314, 359, 367, &
    374, 380, 385, 389, 392, 433, 440, 446, 451, 455, 458, 495, 501, 506, 510, 513, &
    546, 551, 555, 558, 587, 591, 594, 619, 622, 643, 66, 76, 85, 93, 100, 106, 111, &
    115, 118, 120, 181, 190, 198, 205, 211, 216, 220, 223, 225, 281, 289, 296, 302, &
    307, 311, 314, 316, 367, 374, 380, 385, 389, 392, 394, 440, 446, 451, 455, 458, &
    460, 501, 506, 510, 513, 515, 551, 555, 558, 560, 591, 594, 596, 622, 624, 645, &
    121, 135, 148, 160, 171, 181, 190, 198, 205, 211, 226, 239, 251, 262, 272, 281, &
    289, 296, 302, 317, 329, 340, 350, 359, 367, 374, 380, 395, 406, 416, 425, 433, &
    440, 446, 461, 471, 480, 488, 495, 501, 516, 525, 533, 540, 546, 561, 569, 576, &
    582, 597, 604, 610, 625, 631, 646, 135, 148, 160, 171, 181, 190, 198, 205, 211, &
    216, 239, 251, 262, 272, 281, 289, 296, 302, 307, 329, 340, 350, 359, 367, 374, &
    380, 385, 406, 416, 425, 433, 440, 446, 451, 471, 480, 488, 495, 501, 506, 525, &
    533, 540, 546, 551, 569, 576, 582, 587, 604, 610, 615, 631, 636, 651, 148, 160, &
    171, 181, 190, 198, 205, 211, 216, 220, 251, 262, 272, 281, 289, 296, 302, 307, &
    311, 340, 350, 359, 367, 374, 380, 385, 389, 416, 425, 433, 440, 446, 451, 455, &
    480, 488, 495, 501, 506, 510, 533, 540, 546, 551, 555, 576, 582, 587, 591, 610, &
    615, 619, 636, 640, 655, 160, 171, 181, 190, 198, 205, 211, 216, 220, 223, 262, &
    272, 281, 289, 296, 302, 307, 311, 314, 350, 359, 367, 374, 380, 385, 389, 392, &
    425, 433, 440, 446, 451, 455, 458, 488, 495, 501, 506, 510, 513, 540, 546, 551, &
    555, 558, 582, 587, 591, 594, 615, 619, 622, 640, 643, 658, 171, 181, 190, 198, &
    205, 211, 216, 220, 223, 225, 272, 281, 289, 296, 302, 307, 311, 314, 316, 359, &
    367, 374, 380, 385, 389, 392, 394, 433, 440, 446, 451, 455, 458, 460, 495, 501, &
    506, 510, 513, 515, 546, 551, 555, 558, 560, 587, 591, 594, 596, 619, 622, 624, &
    643, 645, 660, 226, 239, 251, 262, 272, 281, 289, 296, 302, 307, 317, 329, 340, &
    350, 359, 367, 374, 380, 385, 395, 406, 416, 425, 433, 440, 446, 451, 461, 471, &
    480, 488, 495, 501, 506, 516, 525, 533, 540, 546, 551, 561, 569, 576, 582, 587, &
    597, 604, 610, 615, 625, 631, 636, 646, 651, 661, 239, 251, 262, 272, 281, 289, &
    296, 302, 307, 311, 329, 340, 350, 359, 367, 374, 380, 385, 389, 406, 416, 425, &
    433, 440, 446, 451, 455, 471, 480, 488, 495, 501, 506, 510, 525, 533, 540, 546, &
    551, 555, 569, 576, 582, 587, 591, 604, 610, 615, 619, 631, 636, 640, 651, 655, &
    665, 251, 262, 272, 281, 289, 296, 302, 307, 311, 314, 340, 350, 359, 367, 374, &
    380, 385, 389, 392, 416, 425, 433, 440, 446, 451, 455, 458, 480, 488, 495, 501, &
    506, 510, 513, 533, 540, 546, 551, 555, 558, 576, 582, 587, 591, 594, 610, 615, &
    619, 622, 636, 640, 643, 655, 658, 668, 262, 272, 281, 289, 296, 302, 307, 311, &
    314, 316, 350, 359, 367, 374, 380, 385, 389, 392, 394, 425, 433, 440, 446, 451, &
    455, 458, 460, 488, 495, 501, 506, 510, 513, 515, 540, 546, 551, 555, 558, 560, &
    582, 587, 591, 594, 596, 615, 619, 622, 624, 640, 643, 645, 658, 660, 670, 317, &
    329, 340, 350, 359, 367, 374, 380, 385, 389, 395, 406, 416, 425, 433, 440, 446, &
    451, 455, 461, 471, 480, 488, 495, 501, 506, 510, 516, 525, 533, 540, 546, 551, &
    555, 561, 569, 576, 582, 587, 591, 597, 604, 610, 615, 619, 625, 631, 636, 640, &
    646, 651, 655, 661, 665, 671, 329, 340, 350, 359, 367, 374, 380, 385, 389, 392, &
    406, 416, 425, 433, 440, 446, 451, 455, 458, 471, 480, 488, 495, 501, 506, 510, &
    513, 525, 533, 540, 546, 551, 555, 558, 569, 576, 582, 587, 591, 594, 604, 610, &
    615, 619, 622, 631, 636, 640, 643, 651, 655, 658, 665, 668, 674, 340, 350, 359, &
    367, 374, 380, 385, 389, 392, 394, 416, 425, 433, 440, 446, 451, 455, 458, 460, &
    480, 488, 495, 501, 506, 510, 513, 515, 533, 540, 546, 551, 555, 558, 560, 576, &
    582, 587, 591, 594, 596, 610, 615, 619, 622, 624, 636, 640, 643, 645, 655, 658, &
    660, 668, 670, 676, 395, 406, 416, 425, 433, 440, 446, 451, 455, 458, 461, 471, &
    480, 488, 495, 501, 506, 510, 513, 516, 525, 533, 540, 546, 551, 555, 558, 561, &
    569, 576, 582, 587, 591, 594, 597, 604, 610, 615, 619, 622, 625, 631, 636, 640, &
    643, 646, 651, 655, 658, 661, 665, 668, 671, 674, 677, 406, 416, 425, 433, 440, &
    446, 451, 455, 458, 460, 471, 480, 488, 495, 501, 506, 510, 513, 515, 525, 533, &
    540, 546, 551, 555, 558, 560, 569, 576, 582, 587, 591, 594, 596, 604, 610, 615, &
    619, 622, 624, 631, 636, 640, 643, 645, 651, 655, 658, 660, 665, 668, 670, 674, &
    676, 679, 461, 471, 480, 488, 495, 501, 506, 510, 513, 515, 516, 525, 533, 540, &
    546, 551, 555, 558, 560, 561, 569, 576, 582, 587, 591, 594, 596, 597, 604, 610, &
    615, 619, 622, 624, 625, 631, 636, 640, 643, 645, 646, 651, 655, 658, 660, 661, &
    665, 668, 670, 671, 674, 676, 677, 679, 680], [55, 21])

contains

subroutine auto2e_RCopy_9_5(T, R, idx)
real(F64), dimension(:), intent(out) :: T
real(F64), dimension(:), intent(in) :: R
integer, dimension(:), intent(in) :: idx
T(1:10) = R(idx(1):idx(1)+9)
T(11:19) = R(idx(2):idx(2)+8)
T(20:27) = R(idx(3):idx(3)+7)
T(28:34) = R(idx(4):idx(4)+6)
T(35:40) = R(idx(5):idx(5)+5)
T(41:45) = R(idx(6):idx(6)+4)
T(46:49) = R(idx(7):idx(7)+3)
T(50:52) = R(idx(8):idx(8)+2)
T(53:54) = R(idx(9):idx(9)+1)
T(55) = R(idx(10))
T(56:64) = R(idx(11):idx(11)+8)
T(65:72) = R(idx(12):idx(12)+7)
T(73:79) = R(idx(13):idx(13)+6)
T(80:85) = R(idx(14):idx(14)+5)
T(86:90) = R(idx(15):idx(15)+4)
T(91:94) = R(idx(16):idx(16)+3)
T(95:97) = R(idx(17):idx(17)+2)
T(98:99) = R(idx(18):idx(18)+1)
T(100) = R(idx(19))
T(101:108) = R(idx(20):idx(20)+7)
T(109:115) = R(idx(21):idx(21)+6)
T(116:121) = R(idx(22):idx(22)+5)
T(122:126) = R(idx(23):idx(23)+4)
T(127:130) = R(idx(24):idx(24)+3)
T(131:133) = R(idx(25):idx(25)+2)
T(134:135) = R(idx(26):idx(26)+1)
T(136) = R(idx(27))
T(137:143) = R(idx(28):idx(28)+6)
T(144:149) = R(idx(29):idx(29)+5)
T(150:154) = R(idx(30):idx(30)+4)
T(155:158) = R(idx(31):idx(31)+3)
T(159:161) = R(idx(32):idx(32)+2)
T(162:163) = R(idx(33):idx(33)+1)
T(164) = R(idx(34))
T(165:170) = R(idx(35):idx(35)+5)
T(171:175) = R(idx(36):idx(36)+4)
T(176:179) = R(idx(37):idx(37)+3)
T(180:182) = R(idx(38):idx(38)+2)
T(183:184) = R(idx(39):idx(39)+1)
T(185) = R(idx(40))
T(186:190) = R(idx(41):idx(41)+4)
T(191:194) = R(idx(42):idx(42)+3)
T(195:197) = R(idx(43):idx(43)+2)
T(198:199) = R(idx(44):idx(44)+1)
T(200) = R(idx(45))
T(201:204) = R(idx(46):idx(46)+3)
T(205:207) = R(idx(47):idx(47)+2)
T(208:209) = R(idx(48):idx(48)+1)
T(210) = R(idx(49))
T(211:213) = R(idx(50):idx(50)+2)
T(214:215) = R(idx(51):idx(51)+1)
T(216) = R(idx(52))
T(217:218) = R(idx(53):idx(53)+1)
T(219) = R(idx(54))
T(220) = R(idx(55))
end subroutine auto2e_RCopy_9_5

subroutine auto2e_RCopy_KetTransform_9_5(S, T, Ex, Ey, Ez, R, lx, ly, lz)
!
! Transform the ket shell pair from Hermite to Cartesian Gaussian basis.
! This variant of the transformation algorithm starts by copying the Rtuv
! matrix elements into contiguous memory locations.
!
real(F64), dimension(:), intent(out) :: S
real(F64), dimension(:), intent(out) :: T
real(F64), dimension(:), intent(in) :: Ex, Ey, Ez
real(F64), dimension(:), intent(in) :: R
integer, intent(in) :: lx, ly, lz
real(F64) :: c
integer :: tau, nu, phi, i
integer :: x0, y0, z0, x, y, z
S = ZERO
x0 = ((lx + 1) * lx) / 2 + 1
y0 = ((ly + 1) * ly) / 2 + 1
z0 = ((lz + 1) * lz) / 2 + 1
do phi = 0, lz
do nu = 0, ly
do tau = 0, lx
i = ((2*5+3-phi)*phi)/2+nu+1
call auto2e_Rcopy_9_5(T, R(tau+1:), RCopyIdx_9_5(:, i))
x = x0 + tau
y = y0 + nu
z = z0 + phi
c = (-1)**modulo(tau+nu+phi, 2)*Ex(x)*Ey(y)*Ez(z)
S = S + c * T
end do
end do
end do
end subroutine auto2e_RCopy_KetTransform_9_5
end module auto2e_KetTransform_9_5