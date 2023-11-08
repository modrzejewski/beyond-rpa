module math_constants_F128
      use arithmetic

      real(F128), parameter :: ZERO = 0.0_F128
      real(F128), parameter :: ONE = 1.0_F128
      real(F128), parameter :: TWO = 2.0_F128
      real(F128), parameter :: FOUR = 4.0_F128
      real(F128), parameter :: PI = FOUR * atan(ONE)
end module math_constants_F128
