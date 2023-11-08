module auto2e_KetTransform_7_3
use arithmetic
use math_constants
implicit none

integer, dimension(36, 10), parameter :: RCopyIdx_7_3 = reshape([1, 12, 22, 31, 39, &
    46, 52, 57, 67, 77, 86, 94, 101, 107, 112, 122, 131, 139, 146, 152, 157, 167, &
    175, 182, 188, 193, 203, 210, 216, 221, 231, 237, 242, 252, 257, 267, 12, 22, &
    31, 39, 46, 52, 57, 61, 77, 86, 94, 101, 107, 112, 116, 131, 139, 146, 152, 157, &
    161, 175, 182, 188, 193, 197, 210, 216, 221, 225, 237, 242, 246, 257, 261, 271, &
    22, 31, 39, 46, 52, 57, 61, 64, 86, 94, 101, 107, 112, 116, 119, 139, 146, 152, &
    157, 161, 164, 182, 188, 193, 197, 200, 216, 221, 225, 228, 242, 246, 249, 261, &
    264, 274, 31, 39, 46, 52, 57, 61, 64, 66, 94, 101, 107, 112, 116, 119, 121, 146, &
    152, 157, 161, 164, 166, 188, 193, 197, 200, 202, 221, 225, 228, 230, 246, 249, &
    251, 264, 266, 276, 67, 77, 86, 94, 101, 107, 112, 116, 122, 131, 139, 146, 152, &
    157, 161, 167, 175, 182, 188, 193, 197, 203, 210, 216, 221, 225, 231, 237, 242, &
    246, 252, 257, 261, 267, 271, 277, 77, 86, 94, 101, 107, 112, 116, 119, 131, &
    139, 146, 152, 157, 161, 164, 175, 182, 188, 193, 197, 200, 210, 216, 221, 225, &
    228, 237, 242, 246, 249, 257, 261, 264, 271, 274, 280, 86, 94, 101, 107, 112, &
    116, 119, 121, 139, 146, 152, 157, 161, 164, 166, 182, 188, 193, 197, 200, 202, &
    216, 221, 225, 228, 230, 242, 246, 249, 251, 261, 264, 266, 274, 276, 282, 122, &
    131, 139, 146, 152, 157, 161, 164, 167, 175, 182, 188, 193, 197, 200, 203, 210, &
    216, 221, 225, 228, 231, 237, 242, 246, 249, 252, 257, 261, 264, 267, 271, 274, &
    277, 280, 283, 131, 139, 146, 152, 157, 161, 164, 166, 175, 182, 188, 193, 197, &
    200, 202, 210, 216, 221, 225, 228, 230, 237, 242, 246, 249, 251, 257, 261, 264, &
    266, 271, 274, 276, 280, 282, 285, 167, 175, 182, 188, 193, 197, 200, 202, 203, &
    210, 216, 221, 225, 228, 230, 231, 237, 242, 246, 249, 251, 252, 257, 261, 264, &
    266, 267, 271, 274, 276, 277, 280, 282, 283, 285, 286], [36, 10])

contains

subroutine auto2e_RCopy_7_3(T, R, idx)
real(F64), dimension(:), intent(out) :: T
real(F64), dimension(:), intent(in) :: R
integer, dimension(:), intent(in) :: idx
T(1:8) = R(idx(1):idx(1)+7)
T(9:15) = R(idx(2):idx(2)+6)
T(16:21) = R(idx(3):idx(3)+5)
T(22:26) = R(idx(4):idx(4)+4)
T(27:30) = R(idx(5):idx(5)+3)
T(31:33) = R(idx(6):idx(6)+2)
T(34:35) = R(idx(7):idx(7)+1)
T(36) = R(idx(8))
T(37:43) = R(idx(9):idx(9)+6)
T(44:49) = R(idx(10):idx(10)+5)
T(50:54) = R(idx(11):idx(11)+4)
T(55:58) = R(idx(12):idx(12)+3)
T(59:61) = R(idx(13):idx(13)+2)
T(62:63) = R(idx(14):idx(14)+1)
T(64) = R(idx(15))
T(65:70) = R(idx(16):idx(16)+5)
T(71:75) = R(idx(17):idx(17)+4)
T(76:79) = R(idx(18):idx(18)+3)
T(80:82) = R(idx(19):idx(19)+2)
T(83:84) = R(idx(20):idx(20)+1)
T(85) = R(idx(21))
T(86:90) = R(idx(22):idx(22)+4)
T(91:94) = R(idx(23):idx(23)+3)
T(95:97) = R(idx(24):idx(24)+2)
T(98:99) = R(idx(25):idx(25)+1)
T(100) = R(idx(26))
T(101:104) = R(idx(27):idx(27)+3)
T(105:107) = R(idx(28):idx(28)+2)
T(108:109) = R(idx(29):idx(29)+1)
T(110) = R(idx(30))
T(111:113) = R(idx(31):idx(31)+2)
T(114:115) = R(idx(32):idx(32)+1)
T(116) = R(idx(33))
T(117:118) = R(idx(34):idx(34)+1)
T(119) = R(idx(35))
T(120) = R(idx(36))
end subroutine auto2e_RCopy_7_3

subroutine auto2e_RCopy_KetTransform_7_3(S, T, Ex, Ey, Ez, R, lx, ly, lz)
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
i = ((2*3+3-phi)*phi)/2+nu+1
call auto2e_Rcopy_7_3(T, R(tau+1:), RCopyIdx_7_3(:, i))
x = x0 + tau
y = y0 + nu
z = z0 + phi
c = (-1)**modulo(tau+nu+phi, 2)*Ex(x)*Ey(y)*Ez(z)
S = S + c * T
end do
end do
end do
end subroutine auto2e_RCopy_KetTransform_7_3
end module auto2e_KetTransform_7_3