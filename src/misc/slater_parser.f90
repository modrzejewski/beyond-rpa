module slater_parser

      use parser
      use gparam

      implicit none

contains


      subroutine read_whole_matrix(un, l2, r, a)
            !
            ! Read lower triangle of a matrix from binary
            ! unit u, to the matrix a. 
            ! The length of the line = l2.
            ! Number of record = r
            ! 
            !
            integer, intent(in) :: un
            integer, intent(in) :: l2
            integer, intent(in) :: r
            real(F64), dimension(:,:), intent(out) :: a

            integer :: i, j, k, iend
            real(F64) :: val, rtemp

            integer :: n

            a = zero

            do i = 1, r*l2
                  read(un) rtemp
            end do

            iend = 1
            k = 1
            do i = 1, CC_NORB

                  do j = 1, CC_NORB

                        read(un) val
                        a(i, j) = val
!                        print*, i, j, a(i, j)
                  
                  ! if (k .le. iend)then 
                  !       print*, 'A wpisuje na miejsce', iend, k
                  !       a(iend, k) = val
                  !       k = k + 1
                  ! else
                  !       k = 1
                  !       iend = iend + 1
                  !       print*, 'B wpisuje na mijsce', iend, k
                  !       a(iend, k) = val
                  !       k = k + 1
                  ! end if
            end do
            end do

            ! do i = 1, CC_NORB
            !       print*, a(i, 97)
            ! end do

            ! print*,'bb',  a(7,97)

            n = size(a, dim=1)


      end subroutine read_whole_matrix

      subroutine read_lower_triangle(un, l2, r, a)
            !                                                                                                  
            ! Read upper traingle and writhe to 
            ! lower triangle of a matrix from binar
            ! unit u, to the matrix a.
            ! The length of the line = l2.
            ! Number of record = r
            !
            integer, intent(in) :: un
            integer, intent(in) :: l2
            integer, intent(in) :: r
            real(F64), dimension(:,:), intent(out) :: a
            
            integer :: i, k, iend
            real(F64) :: val, rtemp
            
            integer :: n
            
            a = zero

            do i = 1, r*l2
                  read(un) rtemp
            end do

            iend = 1
            k = 1
            do i = 1, l2
                  read(un) val
                  
                  if (k .le. iend)then
                        a(iend, k) = val
!                        if (abs(val).gt.1.d-10)then
!                  end if
                        k = k + 1
                  else
                        k = 1
                        iend = iend + 1
                        a(iend, k) = val
 !                       if (abs(val).gt.1.d-10)then
  !                end if


                        k = k + 1
                  end if
            end do

            n = size(a, dim=1)

      end subroutine read_lower_triangle

      subroutine read_lower_triangle_asym(un, l2, r, a)
            !                                                                                                                                                
            ! Read upper triangle and write to
            ! lower triangle of a matrix from binar                                                                      
            ! unit u, to the matrix a.                                                                                            
            ! The length of the line = l2.                                                                                       
            ! Number of record = r                                                                                            
            !                    
            integer, intent(in) :: un
            integer, intent(in) :: l2
            integer, intent(in) :: r
            real(F64), dimension(:,:), intent(out) :: a

            integer :: i, k, iend
            real(F64) :: val, rtemp

            integer :: n

            a = zero

            do i = 1, r*l2
                  read(un) rtemp
            end do

            iend = 1
            k = 1
            do i = 1, l2
                  read(un) val

                  if (k .le. iend)then
                        if (iend == k) then
                              a(iend, k) = val
                        else
                              a(iend, k) = -val
                        end if
                        k = k + 1
                  else
                        k = 1
                        iend = iend + 1
                        if (iend == k) then
                              a(iend, k) = val
                        else
                              a(iend, k) = -val
                        end if
                        k = k + 1
                  end if
            end do

            n = size(a, dim=1)

      end subroutine read_lower_triangle_asym

      subroutine read_lower_lower_triangle(un, l2, r, a)
            !                                                                                                                                                                           
            ! Read lower triangle and write to lower triangle
            ! of a matrix from binar                                                                                                 
            ! unit u, to the matrix a.                                                                                                     
            ! The length of the line = l2.                                                                                   
            ! Number of record = r                                                
            !                                                                        
            integer, intent(in) :: un
            integer, intent(in) :: l2
            integer, intent(in) :: r
            real(F64), dimension(:,:), intent(out) :: a

            integer :: i, k, iend
            real(F64) :: val, rtemp

            integer :: n

            a = zero

            do i = 1, r*l2
                  read(un) rtemp
            end do

            iend = 1
            k = 1
            do i = 1, l2
                  read(un) val

                  if (k .le. iend)then
                        a(iend, k) = val
                        k = k + 1
                  else
                        k = 1
                        iend = iend + 1
                        a(iend, k) = val
                        k = k + 1
                  end if
            end do

            n = size(a, dim=1)

      end subroutine read_lower_lower_triangle



      subroutine read_lower_triangle_norec(fn, i, l2, m, a)
            !
            ! Read lower triangle of a matrix from binary
            ! unit u, from file fn, to the matrix a. 
            ! The length of the line = l2.
            ! The dimension of matrix a = m            
            ! Number of skipped blank lines = i
            !
            character(*), intent(in) :: fn
            integer, intent(in) :: i
            integer, intent(in) :: l2
            integer, intent(in) :: m
            real(F64), dimension(:,:), intent(out) :: a

            real(F64), dimension(:), allocatable :: work

            integer :: j, k, v
            integer :: un

            allocate(work(l2))

            open(newunit=un, file=fn, access='sequential', form='unformatted', status='old')

            do j = 1, i-1
                  read(un) 
            end do

            read(un) work


            a = zero

            v = 1            
            do j = 1, m
                  do k = 1, j
                        a(j, k) = work(v)
                        v = v + 1
                  end do
            end do


            deallocate(work)
            close(un)


      end subroutine read_lower_triangle_norec

      subroutine read_irrep2(mocoeff, eorb, order, irrep0, irrep1, nr_irrep, nocc)

            real(F64), dimension(:, :), intent(inout) :: mocoeff
            real(F64), dimension(:), intent(inout) :: eorb
            integer, dimension(:,:), intent(inout) :: irrep0, irrep1
            integer, dimension(:), intent(in) :: nr_irrep
            integer, intent(in) :: order
            integer, intent(in) :: nocc

            integer :: i, j, k, offset
            real(F64), dimension(:,:), allocatable :: work
            real(F64), dimension(:), allocatable :: weorb
            integer, dimension(:), allocatable :: iocc, ivirt
            integer, dimension(:,:), allocatable :: wirrep0, wirrep1            
            real(F64), dimension(:), allocatable :: eorb_temp
            integer, dimension(:), allocatable :: idx_temp

            allocate(work(CC_NORB, CC_NORB))
            allocate(weorb(CC_NORB))
            
            allocate(eorb_temp(size(eorb)))
            allocate(idx_temp(size(eorb)))

            work = mocoeff
            weorb = eorb

            offset = 0
            irrep0 = 0
            irrep1 = 0
            allocate(iocc(order))
            allocate(ivirt(order))
            allocate(wirrep0(2, order))
            allocate(wirrep1(2, order))

            

            ! Find which orbitals are occupied
            do i = 1, size(eorb)
                  idx_temp(i) = i
            end do
            
            eorb_temp = eorb
            call dsort(eorb_temp, idx_temp, size(eorb))
            print*, 'idx_temp'
            print*, idx_temp(1:12)


            iocc = 0
            ivirt = 0
            do i = 1, order
                  
                  if (nr_irrep(i) .gt. 0) then
                        do j = offset + 1, offset + nr_irrep(i)
                              do k = 1, nocc
                                    if (j .eq. idx_temp(k)) then
                                          iocc(i) = iocc(i) + 1
                                    end if
                              end do
                        end do
                  end if

                  ivirt(i) = nr_irrep(i) - iocc(i)

                  if (iocc(i) .ne. 0) then
                        irrep0(1, i) = offset + 1
                        irrep1(1, i) = offset + iocc(i)
                        
                        if (nr_irrep(i) .ne. 0 .and. nr_irrep(i) .gt. iocc(i)) then
                              irrep0(2, i) = offset + iocc(i) + 1
                              irrep1(2, i) = offset + nr_irrep(i)
                        end if
                        
                  else if (iocc(i) .eq. 0) then
                        
                        if (nr_irrep(i) .ne. 0) then
                              irrep0(2, i) = offset  + 1
                              irrep1(2, i) = offset + nr_irrep(i)
                        end if
                  end if
                              
                              
                  offset = offset + nr_irrep(i)
            end do

            offset = 0
            k = 1
            
            wirrep0 = 0
            wirrep1 = 0
            do i = 1, order
                  if (iocc(i) .ne.0)then
                        wirrep0(1, i) = k
                        do j = irrep0(1, i), irrep1(1, i)
                              eorb(k) = weorb(j)
                              mocoeff(:, k) = work(:, j)
                              k = k + 1
                        end do
                        wirrep1(1, i) = k-1
                  end if
            end do

           do i = 1, order
                  if (ivirt(i) .ne.0)then
                        wirrep0(2, i) = k
                        do j = irrep0(2, i), irrep1(2, i)
                              eorb(k) = weorb(j)
                              mocoeff(:, k) = work(:, j)
                              k = k + 1
                        end do
                        wirrep1(2, i) = k-1
                  end if
            end do

            irrep0 = wirrep0
            irrep1 = wirrep1

            do i = 1, size(eorb)
                  print*, eorb(i)
            end do

            deallocate(eorb_temp)
            deallocate(idx_temp)

      end subroutine read_irrep2


      subroutine read_irrep(fn, r, rl, order, eorb, irrep0, irrep1, nr_irrep)
            character(*), intent(in) ::fn
            integer, intent(in) :: r
            integer, intent(in) :: rl
            integer, intent(in) :: order
            real(F64), dimension(:), intent(in) :: eorb
            integer, dimension(:,:), intent(out) :: irrep0
            integer, dimension(:,:), intent(out) :: irrep1
            integer, dimension(:), intent(out) :: nr_irrep

            integer :: un
            integer :: i, j
            integer :: val
            integer :: offset
            integer :: iocc


            open(newunit=un, file=fn, access='direct', form='unformatted', status='old', recl = rl)


            irrep0 = 0
            irrep1 = 0

            offset = 0

            ! number of occupied orbitals in each symmetry


            iocc = 0
            nr_irrep = 0

            do i = 1, order

                  read(un, rec=r+i) val
                  iocc = 0
                  nr_irrep(i) = val

                  if (val .gt. 0) then
                        do j = offset + 1, offset + val
                              if (eorb(j) .lt. zero) then
                                    iocc = iocc + 1
                              end if
                        end do
                  end if

                  if (iocc .ne. 0) then
                        irrep0(1, i) = offset + 1
                        irrep1(1, i) = offset + iocc

                        if (val .ne. 0 .and. val .gt. iocc) then
                              irrep0(2, i) = offset + iocc + 1
                              irrep1(2, i) = offset + val
                        end if

                  else if (iocc .eq. 0) then

                        if (val .ne. 0) then
                              irrep0(2, i) = offset  + 1
                              irrep1(2, i) = offset + val
                        end if
                  end if

                  offset = offset + val
            end do


            close(un)

      end subroutine read_irrep



      subroutine smfill_lower(a)
            real(F64), dimension(:,:), intent(inout) :: a
            integer :: i, k, n

            n = size(a, dim=1)

            do i = 1, n
                  do k = 1, i-1                        
                        a(i, k) = a(k, i)
                  end do
            end do

      end subroutine smfill_lower


end module slater_parser
