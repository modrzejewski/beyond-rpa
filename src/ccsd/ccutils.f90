module ccutils
      use gparam

      implicit none

contains

      function ai_idx(b, j, c, k)
            !
            ! While making double excitations
            ! the inequality (ai) >= (bj) must be fulfilled.
            ! To obtain this in code it was derived that if 'a' and 'i' are fixed then
            ! 'b' and 'j', must satisfy:
            ! A * b + j < A * a + i  and constant A = nocc
            ! 
            ! Derivation:  1,2,..N, N+1, ...Norb
            ! 1-N : occupied orbitals
            ! N+1 - Norb : virtual orbitals
            ! 
            ! In the limiting case 
            ! A * (N+2) + 1 > A(N+1) + N
            ! A > N - 1
            ! A = nocc
            !
            logical :: ai_idx
            integer, intent(in) :: b, j, c, k
            integer :: A 
            integer :: lf, rt
            
            A = ne / 2
            ai_idx = .false.

            lf = A * b + j
            rt = A * c + k
            
            if(lf.ge.rt) ai_idx = .true.

      end function ai_idx


end module ccutils
