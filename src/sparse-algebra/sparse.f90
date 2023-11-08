
module sparse
      !
      ! **********************************************
      ! DESCRIPTION:
      ! **********************************************
      !
      ! Sparse matrix algebra with strict error
      ! control
      !
      ! **********************************************
      ! REFERENCES:
      ! **********************************************
      !
      ! 1. Systematic Sparse Matrix Error Control
      !    for Linear Scaling Electronic Structure
      !    Calculations, Rubensson, E.H., Salek,
      !    P., J. Comput. Chem. 26, 1628-1637 (2005)
      !
      !
      !
      use math_constants
      use sort
      use gparam
      
      implicit none
      !
      ! ***********************************************************************
      ! PARAMETERS:
      ! ***********************************************************************
      !
      double precision, parameter             :: threshold = 1.d-6
      !
      !
      !
      ! ***********************************************************************
      ! GLOBAL VARIABLES:
      ! ***********************************************************************
      !
      integer                                 :: ngroups
      integer, dimension(:), pointer          :: partition
      integer, dimension(:), pointer          :: cumulative_sum
      integer, dimension(:), pointer          :: complement, complement_k0
      integer, dimension(:), pointer          :: complementa, complement_k0a
      integer, dimension(:), pointer          :: ncomplement, ncomplementa
      integer, dimension(:), pointer          :: iblocks
      integer, dimension(:), pointer          :: bblocks
      double precision, dimension(:), pointer :: denserow
      double precision, dimension(:), pointer :: norm
      double precision, dimension(:), pointer :: work
      !
      !
      !
      save    :: ngroups, partition, cumulative_sum, &
                 complement, complementa, complement_k0, complement_k0a, &
                 ncomplement, ncomplementa, iblocks, bblocks, denserow, &
                 norm, work
      private :: threshold, ngroups, &
                 complement, complementa, complement_k0, complement_k0a, &
                 ncomplement, ncomplementa, iblocks, bblocks, denserow, &
                 norm, work
                 
      !
      ! Sparse matrix definition
      !
      type spmatrix
            !!!!!!!!!!!!!!!
            ! Change values so that indices begin at 0
            ! Describe structure of following arrays
            ! Change spcpy when zero index implemented
            !
            double precision, dimension(:), pointer :: values
            integer, dimension(:), pointer :: rows
            integer, dimension(:), pointer :: pointer_b
            integer, dimension(:), pointer :: k0
            integer :: next_element = 1
            integer :: size = 0
      end type spmatrix

contains

      subroutine sparse_init()
            integer :: i, sum
            integer :: s, p

            !
            ! Block of sparse matrix is defined
            ! by AO's belonging to the same atom
            !
            ngroups = natom

            allocate(partition(ngroups))

            do i = 1, natom
                  p = 0
                  do s = sh0(i), sh0(i + 1) - 1
                       p = p + nfunc(shtype(sh(s))) 
                  end do
                  partition(i) = p
            end do

            allocate(cumulative_sum(ngroups))
            allocate(complement(ngroups * (ngroups - 1) / 2 ))
            allocate(complementa(ngroups * (ngroups - 1) / 2 ))
            allocate(complement_k0(ngroups * (ngroups - 1) / 2 ))
            allocate(complement_k0a(ngroups * (ngroups - 1) / 2 ))
            allocate(ncomplement(ngroups))
            allocate(ncomplementa(ngroups))
            allocate(denserow(norb))
            allocate(norm(ngroups))
            allocate(work(maxval(partition) * norb))
            allocate(iblocks(0:ngroups))
            allocate(bblocks(ngroups))

            sum = 0
            do i = 1, ngroups
                  cumulative_sum(i) = sum
                  sum = sum + partition(i)
            end do
      end subroutine sparse_init


      subroutine sparse_free()
            deallocate(partition)
            deallocate(cumulative_sum)
            deallocate(complement)
            deallocate(complementa)
            deallocate(complement_k0)
            deallocate(complement_k0a)
            deallocate(ncomplement)
            deallocate(ncomplementa)
            deallocate(denserow)
            deallocate(norm)
            deallocate(work)
            deallocate(iblocks)
            deallocate(bblocks)
      end subroutine sparse_free

      
      subroutine spgetthr(t)
            double precision, intent(out) :: t

            t = threshold
      end subroutine spgetthr


      function spwquery()
            integer :: spwquery

            spwquery = maxval(partition(1:ngroups)) * norb
      end function spwquery

      
      subroutine spmatrix_init(a, nonzero_elements)
            integer, intent(in) :: nonzero_elements
            type(spmatrix) :: a

            integer :: nblocks

            nblocks = ngroups**2

            allocate(a%values(nonzero_elements))
            allocate(a%rows(nblocks))
            allocate(a%k0(nblocks))
            allocate(a%pointer_b(ngroups + 1))

            a%pointer_b(1) = 1
            a%size = nonzero_elements
      end subroutine spmatrix_init


      subroutine spmatrix_free(a)
            type(spmatrix) :: a

            deallocate(a%values)
            deallocate(a%rows)
            deallocate(a%k0)
            deallocate(a%pointer_b)
      end subroutine spmatrix_free


      subroutine gctcontribd(m, a, val)
            integer, intent(in) :: m
            double precision, dimension(m, m), intent(in) :: a
            double precision, dimension(:), intent(inout) :: val

            integer :: i, j

            do j = 1, m
                  do i = 1, j - 1
                        val(j) = val(j) + abs(a(i, j))
                  end do
                  
                  val(j) = val(j) + a(j, j)

                  do i = j + 1, m
                        val(j) = val(j) + abs(a(i, j))
                  end do
            end do
      end subroutine gctcontribd

      
      subroutine gctcontrib(m, n, a, val)
            integer, intent(in) :: m, n
            double precision, dimension(m, n), intent(in) :: a
            double precision, dimension(:), intent(inout) :: val

            integer :: i, j

            do j = 1, n
                  do i = 1, m
                        val(j) = val(j) + abs(a(i, j))
                  end do
            end do
      end subroutine gctcontrib


      subroutine gctcontribt(m, n, a, val)
            integer, intent(in) :: m, n
            double precision, dimension(m, n), intent(in) :: a
            double precision, dimension(:), intent(inout) :: val

            integer :: i, j

            do j = 1, n
                  do i = 1, m
                        val(i) = val(i) + abs(a(i, j))
                  end do
            end do
      end subroutine gctcontribt

      
      function spsygct(a)
            !
            ! *********************************************
            ! DESCRIPTION:
            ! *********************************************
            !
            ! Calculate approximate |A|_2 norm of sparse
            ! matrix A using the Gershgorin circular
            ! theorem. It is assumed that values on the
            ! diagonal are always non-negative
            !
            ! *********************************************
            ! REFERENCES:
            ! *********************************************
            !
            ! 1. Jansik, B., J. Chem. Phys. 126,
            !    124104 (2007), eq. 35
            !
            ! *********************************************
            ! INPUTS:
            ! *********************************************
            ! 
            ! A - Sprase matrix structure for which
            !     aproximate norm is computed. Only values
            !     below diagonal are used
            !
            !
            !
            double precision :: spsygct

            type(spmatrix) :: a

            integer :: j
            integer :: rowindex
            integer :: m, m0, m1
            integer :: k0
            integer :: idim, jdim
            integer :: joffset
            integer :: pos, nc

            denserow = zero
            ncomplement = 0

            do j = 1, ngroups
                  jdim = partition(j)
                  m0 = a%pointer_b(j)
                  m1 = a%pointer_b(j + 1) - 1
                  joffset = cumulative_sum(j) + 1
                  !
                  ! Diagonal block
                  !
                  rowindex = a%rows(m0)
                  idim = partition(rowindex)
                  k0 = a%k0(m0)
                  !
                  ! gctcontib for diagonal block
                  ! 
                  call gctcontribd(idim, a%values(k0:), denserow(joffset:))
                  !
                  ! Blocks below diagonal, a_{ij}, i > j
                  !
                  do m = m0 + 1, m1
                        rowindex = a%rows(m)
                        idim = partition(rowindex)
                        k0 = a%k0(m)
                        ! 
                        ! Storing information about counterpart
                        ! symmetric block
                        !
                        nc = ncomplement(rowindex) + 1
                        ncomplement(rowindex) = nc
                        pos = sypos(nc, rowindex)
                        complement(pos) = j
                        complement_k0(pos) = k0
                        !
                        !
                        !
                        call gctcontrib(idim, jdim, a%values(k0:), denserow(joffset:))
                  end do
                  !
                  ! Blocks above diagonal a_{ij}, j > i
                  !
                  pos = sypos(1, j)
                  do m = 1, ncomplement(j)
                        rowindex = complement(pos)
                        idim = partition(rowindex)
                        k0 = complement_k0(pos)
                        !
                        ! gctcontrib for transposed block
                        !
                        call gctcontribt(jdim, idim, a%values(k0:), denserow(joffset:))
                        pos = pos + 1
                  end do
            end do
            
            spsygct = maxval(denserow)
      end function spsygct


      function spsyl1norm(a)
            !
            ! *********************************************
            ! DESCRIPTION:
            ! *********************************************
            !
            ! Calculate ||A||_1 norm of sparse matrix A
            !
            ! *********************************************
            ! REFERENCES:
            ! *********************************************
            !
            ! 1. Jansik, B., J. Chem. Phys. 126,
            !    124104 (2007)
            !
            ! *********************************************
            ! INPUTS:
            ! *********************************************
            ! 
            ! A - Sprase matrix structure for which
            !     norm is computed. Only values
            !     below diagonal are used
            !
            !
            !
            double precision :: spsyl1norm

            type(spmatrix) :: a

            integer :: j
            integer :: rowindex
            integer :: m, m0, m1
            integer :: k0
            integer :: idim, jdim
            integer :: joffset
            integer :: pos, nc

            denserow = zero
            ncomplement = 0

            do j = 1, ngroups
                  jdim = partition(j)
                  m0 = a%pointer_b(j)
                  m1 = a%pointer_b(j + 1) - 1
                  joffset = cumulative_sum(j) + 1
                  !
                  ! Diagonal block
                  !
                  rowindex = a%rows(m0)
                  idim = partition(rowindex)
                  k0 = a%k0(m0)
                  call gctcontrib(idim, jdim, a%values(k0:), denserow(joffset:))
                  !
                  ! Blocks below diagonal, a_{ij}, i > j
                  !
                  do m = m0 + 1, m1
                        rowindex = a%rows(m)
                        idim = partition(rowindex)
                        k0 = a%k0(m)
                        ! 
                        ! Storing information about counterpart
                        ! symmetric block
                        !
                        nc = ncomplement(rowindex) + 1
                        ncomplement(rowindex) = nc
                        pos = sypos(nc, rowindex)
                        complement(pos) = j
                        complement_k0(pos) = k0
                        !
                        !
                        !
                        call gctcontrib(idim, jdim, a%values(k0:), denserow(joffset:))
                  end do
                  !
                  ! Blocks above diagonal a_{ij}, j > i
                  !
                  pos = sypos(1, j)
                  do m = 1, ncomplement(j)
                        rowindex = complement(pos)
                        idim = partition(rowindex)
                        k0 = complement_k0(pos)
                        !
                        ! gctcontrib for transposed block
                        !
                        call gctcontribt(jdim, idim, a%values(k0:), denserow(joffset:))
                        pos = pos + 1
                  end do
            end do
            
            spsyl1norm = maxval(denserow)
      end function spsyl1norm


      function l1norm(a, m, n)
            !
            ! *********************************************
            ! DESCRIPTION:
            ! *********************************************
            !
            ! Calculate matrix l1-norm (submultiplicative
            ! norm). It is defined as
            ! ||A||_1 = max_j \sum_i |a_{ij}|
            !
            ! *********************************************
            ! REFERENCES:
            ! *********************************************
            !
            ! 1. Rubensson, E.H., Salek, P.,
            !    J. Comput. Chem. 26, 1628 (2005)
            !
            ! *********************************************
            ! INPUTS:
            ! *********************************************
            !
            ! A - Rectangular matrix for which norm is
            !     calculated
            ! M - Number of rows of matrix A
            ! N - Number of columns of matrix A.
            !
            !
            !
            double precision :: l1norm
            double precision, dimension(:), intent(in) :: a
            integer, intent(in) :: m, n

            double precision :: column_sum
            integer :: i, j, k

            l1norm = zero
            k = 1
            do j= 1, n
                  column_sum = zero
                  do i = 1, m
                        column_sum = column_sum + abs(a(k))
                        k = k + 1
                  end do
                  
                  if (column_sum .gt. l1norm) then
                        l1norm = column_sum
                  end if
            end do
      end function l1norm


      ! subroutine add_column(col, a, info)
      !       !
      !       ! ************************************************
      !       ! DESCRIPTION:
      !       ! ************************************************
      !       !
      !       ! Add a assumed-dense column of data to matrix A.
      !       ! Select nonneglible contributions
      !       !
      !       ! ************************************************
      !       ! REFERENCES:
      !       ! ************************************************
      !       !
      !       ! 1. Rubensson, E.H., Salek, P.,
      !       !    J. Comput. Chem. 26, 1628 (2005)
      !       !
      !       ! ************************************************
      !       ! INPUTS:
      !       ! ************************************************
      !       ! 
      !       ! COL       - Array containing data which is to 
      !       !             be added to sparse matrix A. This
      !       !             array is assumed dense
      !       ! 
      !       ! A         - Sparse matrix structure
      !       !
      !       ! INFO      - Output value set to -1 if whole
      !       !             column is to be neglected. Otherwise
      !       !             set to 0
      !       !
      !       !
      !       !
      !       double precision, dimension(:), intent(in) :: col
      !       type(spmatrix), intent(inout) :: a
      !       integer, intent(out) :: info

      !       double precision, dimension(ngroups) :: norm
      !       double precision :: normsum
      !       integer, dimension(ngroups) :: blocks
      !       integer :: sum
      !       integer, dimension(ngroups) :: cumulative_sum0, cumulative_sum1
      !       integer :: k0, k1, k00, k11, i, n, l, rowindex, columnindex, blocksize, idim, jdim
      !       integer :: nelements

      !       info = 0
      !       columnindex = a%next_column
      !       jdim = partition(columnindex)

      !       sum = 0
      !       do i = 1, ngroups
      !             cumulative_sum0(i) = sum
      !             sum = sum + partition(i)
      !             cumulative_sum1(i) = sum
      !       end do            

      !       k0 = 1
      !       do i = 1, ngroups
      !             idim = partition(i)
      !             blocksize = idim * jdim
      !             k1 = k0 + blocksize - 1
      !             norm(i) = l1norm(col(k0:k1), idim, jdim)
      !             blocks(i) = i
      !             k0 = k0 + blocksize
      !       end do
      !       !
      !       ! Sort blocks according to l1 matrix norm in ascending order
      !       !
      !       call dsort(norm, blocks, ngroups)
      !       !
      !       ! Neglect blocks according to eq. 4 in  J. Comput. Chem. 26, 1628-1637 (2005) 
      !       !
      !       n = 1
      !       normsum = zero

      !       do i = 1, ngroups
      !             normsum = normsum + norm(i)
      !             if (normsum .lt. threshold) then
      !                   n = n + 1
      !             else
      !                   exit
      !             end if
      !       end do
      !       !
      !       ! Generate exception in case matrix is singular
      !       !
      !       if (n .gt. ngroups) then
      !             info = -1
      !       else
      !             l = a%next_block
      !             k00 = a%next_element
      !             nelements = 0

      !             do i = n, ngroups
      !                   rowindex = blocks(i)
      !                   k0 = cumulative_sum0(rowindex) * jdim + 1
      !                   k1 = cumulative_sum1(rowindex) * jdim
      !                   blocksize = k1 - k0 + 1
      !                   !
      !                   ! Store row index
      !                   !
      !                   a%rows(l) = rowindex
      !                   !
      !                   ! Values matrix
      !                   !
      !                   k11 = k00 + blocksize - 1
      !                   a%values(k00:k11) = col(k0:k1)
      !                   !
      !                   ! Store position of written block
      !                   !
      !                   a%k0(l) = k00
      !                   a%k1(l) = k11

      !                   nelements = nelements + blocksize
      !                   l = l + 1
      !                   k00 = k00 + blocksize
      !             end do
      !             !
      !             ! Calculate pointer_b and pointer_e
      !             !
      !             k0 = a%next_block
      !             k1 = l - 1
      !             a%pointer_b(columnindex) = k0
      !             a%pointer_e(columnindex) = k1
      !             !
      !             ! Index of next entry in values array, next block, next column
      !             !
      !             a%next_element = a%next_element + nelements
      !             a%next_block = l
      !             a%next_column = columnindex + 1
      !       end if
      ! end subroutine add_column


      ! subroutine add_spcolumn(col, a, blocks, nblocks, info)
      !       !
      !       ! ************************************************
      !       ! DESCRIPTION:
      !       ! ************************************************
      !       !
      !       ! Add a sparse column of data to matrix A.
      !       ! Select nonneglible contributions
      !       !
      !       ! ************************************************
      !       ! REFERENCES:
      !       ! ************************************************
      !       !
      !       ! 1. Rubensson, E.H., Salek, P.,
      !       !    J. Comput. Chem. 26, 1628 (2005)
      !       !
      !       ! ************************************************
      !       ! INPUTS:
      !       ! ************************************************
      !       ! 
      !       ! COL       - Array containing data which is to 
      !       !             be added to sparse matrix A. 
      !       !             Addresses of nonzero blocks have to
      !       !             be provided. Data should be placed
      !       !             adjacently. Example:
      !       !             COLUMNINDEX = 5
      !       !             BLOCKS = (/3, 1/)
      !       !             COL adjacently stores blocks (3, 5)
      !       !             and and (1, 5) (Note that row order
      !       !             is irrelevant)
      !       ! 
      !       ! A         - Sparse matrix structure
      !       ! 
      !       ! BLOCKS    - Aray containing indices of rows
      !       !             where nonnegligible blocks are
      !       !             expected. It is assumed that blocks
      !       !             are placed adjacently in COL array
      !       !
      !       ! NBLOCKS   - Number of blocks which addresses
      !       !             are provided in BLOCKS array
      !       ! 
      !       ! INFO      - Output value set to -1 if whole
      !       !             column is to be neglected. Otherwise
      !       !             set to 0
      !       !
      !       !
      !       !
      !       double precision, dimension(:), intent(in) :: col
      !       type(spmatrix), intent(inout) :: a
      !       integer, dimension(:), intent(in) :: blocks
      !       integer, intent(in) :: nblocks
      !       integer, intent(out) :: info

      !       double precision, dimension(ngroups) :: norm
      !       double precision :: normsum
      !       integer :: sum, bi
      !       integer, dimension(ngroups) :: cumulative_sum0, cumulative_sum1
      !       integer :: k0, k1, k00, k11, i, n, l, rowindex, columnindex, blocksize, idim, jdim
      !       integer :: nelements

      !       info = 0
      !       columnindex = a%next_column
      !       jdim = partition(columnindex)

      !       sum = 0
      !       do i = 1, nblocks
      !             bi = blocks(i)
      !             cumulative_sum0(bi) = sum
      !             sum = sum + partition(bi)
      !             cumulative_sum1(bi) = sum
      !       end do 

      !       k0 = 1
      !       do i = 1, nblocks
      !             idim = partition(blocks(i))
      !             blocksize = idim * jdim
      !             k1 = k0 + blocksize - 1
      !             norm(i) = l1norm(col(k0:k1), idim, jdim)
      !             k0 = k0 + blocksize
      !       end do
      !       !
      !       ! Sort blocks according to l1 matrix norm in ascending order
      !       !
      !       call dsort(norm, blocks, nblocks)
      !       !
      !       !Neglect blocks according to eq. 4 in  J. Comput. Chem. 26, 1628-1637 (2005) 
      !       !
      !       n = 1
      !       normsum = zero

      !       do i = 1, nblocks
      !             normsum = normsum + norm(i)
      !             if (normsum .lt. threshold) then
      !                   n = n + 1
      !             else
      !                   exit
      !             end if
      !       end do
      !       !
      !       ! Generate exception in case matrix is singular
      !       !
      !       if (n .gt. nblocks) then
      !             info = -1
      !       else
      !             l = a%next_block
      !             k00 = a%next_element
      !             nelements = 0
      !             do i = n, nblocks
      !                   rowindex = blocks(i)
      !                   k0 = cumulative_sum0(rowindex) * jdim + 1
      !                   k1 = cumulative_sum1(rowindex) * jdim
      !                   blocksize = k1 - k0 + 1
      !                   !
      !                   ! Store row index
      !                   !
      !                   a%rows(l) = rowindex
      !                   !
      !                   ! Values matrix
      !                   !
      !                   k11 = k00 + blocksize - 1
      !                   a%values(k00:k11) = col(k0:k1)
      !                   !
      !                   ! Store position of written block
      !                   !
      !                   a%k0(l) = k00
      !                   a%k1(l) = k11

      !                   nelements = nelements + blocksize
      !                   l = l + 1
      !                   k00 = k00 + blocksize
      !             end do
      !             !
      !             ! Calculate pointer_b and pointer_e
      !             !
      !             k0 = a%next_block
      !             k1 = l - 1
      !             a%pointer_b(columnindex) = k0
      !             a%pointer_e(columnindex) = k1
      !             !
      !             ! Index of next entry in values array, next block, next column
      !             !
      !             a%next_element = a%next_element + nelements
      !             a%next_block = l
      !             a%next_column = columnindex + 1
      !       end if
      ! end subroutine add_spcolumn


      subroutine spcol(col, columnindex, a, blocks, nblocks, neglect)
            !
            ! ************************************************
            ! DESCRIPTION:
            ! ************************************************
            !
            ! Add a sparse column of data to symmetric 
            ! sparse matrix A. Select nonneglible
            ! contributions if NEGLECT .EQ. .FALSE.
            !
            ! ************************************************
            ! REFERENCES:
            ! ************************************************
            !
            ! 1. Rubensson, E.H., Salek, P.,
            !    J. Comput. Chem. 26, 1628 (2005)
            !
            ! ************************************************
            ! INPUTS:
            ! ************************************************
            ! 
            ! COL       - Array containing data which is to 
            !             be added to sparse matrix A. 
            !             Addresses of nonzero blocks have to
            !             be provided. Example:
            !             COLUMNINDEX = 5
            !             BLOCKS = (/3, 1/)
            !             COL stores blocks (3, 5)
            !             and and (1, 5) as if it was a dense 
            !             column
            !
            ! C..INDEX  - Index of column to add. Columns
            !             should be added in ordered manner
            ! 
            ! A         - Sparse matrix structure
            ! 
            ! BLOCKS    - Aray containing indices of rows
            !             where nonnegligible blocks are
            !             expected. Index of diagonal block
            !             SHOULD NOT be contained in BLOCKS
            !             because it is assumed to be always
            !             nonneglible block
            !
            ! NBLOCKS   - Number of blocks which addresses
            !             are provided in BLOCKS array
            !             (diagonal block doesn't count)
            !
            ! NEGLECT   - Set to 1 if selection of negligible
            !             blocks is to be performed
            !
            !
            double precision, dimension(:), intent(in) :: col
            integer, intent(in) :: columnindex
            type(spmatrix), intent(inout) :: a
            integer, dimension(0:), intent(inout) :: blocks
            integer, intent(in) :: nblocks
            integer, intent(in) :: neglect

            double precision :: normsum
            integer :: k0, k1
            integer :: k00, k11
            integer :: i, n, l
            integer :: rowindex, blocksize
            integer :: idim, jdim
            integer :: nelements

            jdim = partition(columnindex)
            
            if (nblocks .gt. 0) then 
                  if (neglect .eq. 1) then
                        do i = 1, nblocks
                              rowindex = blocks(i)
                              idim = partition(rowindex)
                              blocksize = idim * jdim
                              k0 = cumulative_sum(rowindex) * jdim + 1
                              norm(i) = l1norm(col(k0:), idim, jdim)
                        end do
                        !
                        ! Sort blocks according to l1 matrix norm in ascending order
                        !
                        call dsort(norm, blocks(1:), nblocks)
                        !
                        ! Neglect blocks according to eq. 4 in  J. Comput. Chem. 26, 1628-1637 (2005) 
                        !
                        n = 1
                        normsum = zero
                        do i = 1, nblocks
                              normsum = normsum + norm(i)
                              if (normsum .lt. threshold) then
                                    n = n + 1
                              else
                                    exit
                              end if
                        end do
                  else
                        do i = 1, nblocks
                              rowindex = blocks(i)
                              idim = partition(rowindex)
                              blocksize = idim * jdim
                              k0 = cumulative_sum(rowindex) * jdim + 1
                        end do

                        n = nblocks
                  end if
                  !
                  ! Always include diagonal block as first element in column.
                  ! Diagonal block index is always stored in blocks(n - 1)
                  !
                  n = n - 1
                  blocks(n) = columnindex
            else
                  n = nblocks
                  blocks(n) = columnindex
            end if

            l = a%pointer_b(columnindex)
            k00 = a%next_element
            nelements = 0
            do i = n, nblocks
                  rowindex = blocks(i)
                  idim = partition(rowindex)
                  blocksize = idim * jdim
                  k0 = cumulative_sum(rowindex) * jdim + 1
                  k1 = k0 + blocksize - 1
                  !
                  ! Store row index
                  !
                  a%rows(l) = rowindex
                  !
                  ! Values matrix
                  !
                  k11 = k00 + blocksize - 1
                  a%values(k00:k11) = col(k0:k1)
                  !
                  ! Store position of written block
                  !
                  a%k0(l) = k00
                  nelements = nelements + blocksize
                  l = l + 1
                  k00 = k00 + blocksize
            end do
            !
            ! Calculate pointer_b
            !
            a%pointer_b(columnindex + 1) = l
            !
            ! Index of next entry in values array
            !
            a%next_element = a%next_element + nelements
      end subroutine spcol


      ! subroutine spdgemm(a, b, c)
      !       !
      !       ! *********************************************************
      !       ! DESCRIPTION:
      !       ! *********************************************************
      !       !
      !       ! Sparse-sparse DGEMM - sparse matrix multiplication:
      !       ! C = A * B,    A, B, C - sparse matrices
      !       !
      !       ! *********************************************************
      !       ! REFERENCES:
      !       ! *********************************************************
      !       !
      !       ! 1. Rubensson, E.H., Salek, P.,
      !       !    J. Comput. Chem. 26, 1628 (2005)
      !       !
      !       ! *********************************************************
      !       ! INPUTS:
      !       ! *********************************************************
      !       !
      !       ! A, B, C   - Sparse matrix structures
      !       !
      !       !
      !       !
      !       type(spmatrix) :: a, b, c

      !       integer :: info
      !       integer :: nblocks
      !       integer, dimension(ngroups) :: blocks
      !       integer :: j, k0, k1, k, l,i ,i0, i1, blocksize, column_end
      !       integer, dimension(ngroups) :: pos0, pos1
      !       integer :: next_element
      !       integer :: idim, ldim, jdim
      !       integer :: m0, m1, m
      !       double precision, dimension(:), pointer :: ail, blj, cij
      !       double precision, dimension(maxval(partition) * norbitals), target :: work
      !       logical, dimension(ngroups) :: lblocks

      !       external :: dgemm

      !       c%next_element = 1
      !       c%next_block = 1
      !       c%next_column = 1
      !       !
      !       ! Calculate j-th dense column of matrix c, j = 1, ..., ngroups
      !       !
      !       do j = 1, ngroups
      !             jdim = partition(j)
      !             !
      !             ! Calculate ail * blj, l = 1, ..., ngroups products
      !             !
      !             m0 = b%pointer_b(j)
      !             m1 = b%pointer_e(j)
      !             lblocks = .false.
      !             nblocks = 0
      !             work = zero
      !             next_element = 1

      !             do m = m0, m1
      !                   l = b%rows(m)
      !                   ldim = partition(l)
      !                   k0 = b%k0(m)
      !                   k1 = b%k1(m)
      !                   blj => b%values(k0:k1)

      !                   k0 = a%pointer_b(l)
      !                   k1 = a%pointer_e(l)

      !                   do k = k0, k1
      !                         i = a%rows(k)
      !                         idim = partition(i)

      !                         if (.not. lblocks(i)) then
      !                               lblocks(i) = .true.
      !                               nblocks = nblocks + 1
      !                               blocks(nblocks) = i
      !                               pos0(i) = next_element
      !                               next_element = next_element + idim * jdim
      !                               pos1(i) = next_element - 1
      !                         end if

      !                         i0 = pos0(i)
      !                         i1 = pos1(i)
                              
      !                         cij => work(i0:i1)

      !                         i0 = a%k0(k)
      !                         i1 = a%k1(k)

      !                         ail => a%values(i0:i1)

      !                         call dgemm("N", "N", idim, jdim, ldim, &
      !                               one, ail, idim, blj, ldim, one, cij, idim)
      !                   end do
      !             end do

      !             call add_spcolumn(work, c, blocks, nblocks, info)

      !       end do
      ! end subroutine spdgemm


      function sypos(i, j)
            integer :: sypos
            
            integer, intent(in) :: i, j

            sypos = ((j - 1) * (j - 2)) / 2 + i
      end function sypos


      subroutine spsymm(a, b, c, neglect)
            !
            ! *********************************************************
            ! DESCRIPTION:
            ! *********************************************************
            !
            ! Sparse-sparse DSYMM - sparse symmetric
            ! matrix multiplication:
            ! C = A * B,    A, B, C - sparse symmetric matrices (only
            !                         values below diagonal stored
            !
            ! *********************************************************
            ! REFERENCES:
            ! *********************************************************
            !
            ! 1. Rubensson, E.H., Salek, P.,
            !    J. Comput. Chem. 26, 1628 (2005)
            !
            ! *********************************************************
            ! INPUTS:
            ! *********************************************************
            !
            ! A, B, C   - Sparse matrix structures, only valuses below
            !             diagonal are considered
            !
            ! NEGLECT   - Set to 1 if selection of negligible blocks
            !             in sparse matrix C is to be performed
            !
            !
            type(spmatrix) :: a, b, c
            integer, intent(in) :: neglect

            integer :: i, j, l, v
            integer :: bb
            integer :: idim, ldim, jdim
            integer :: i0
            integer :: k0, k1, k
            integer :: m0, m1, m
            double precision, dimension(:), pointer :: ail, blj, cij
            integer :: nblocks
            integer :: pos, nc

            external :: dgemm

            c%next_element = 1
            c%pointer_b(1) = 1

            !
            ! Calculate positions of a_{ij} above diagonal
            !
            ncomplementa = 0

            do v = 1, ngroups
                  m0 = a%pointer_b(v) + 1
                  m1 = a%pointer_b(v + 1) - 1

                  do k = m0, m1
                        i = a%rows(k)
                        k0 = a%k0(k)
                        nc = ncomplementa(i) + 1
                        pos = sypos(nc, i)
                        ncomplementa(i) = nc
                        complementa(pos) = v
                        complement_k0a(pos) = k0
                  end do
            end do

            ncomplement = 0
            !
            ! Calculate j-th sparse column of matrix c, j = 1, ..., ngroups,
            ! calculate only c_{ij}, i >= j
            !
            do j = 1, ngroups
                  jdim = partition(j)
                  !
                  ! Calculate ail * blj, l = 1, ..., ngroups products
                  !
                  m0 = b%pointer_b(j)
                  m1 = b%pointer_b(j + 1) - 1
                  bblocks = 0
                  work = zero
                  !
                  ! Diagonal block of j-th column of B
                  !
                  m = m0
                  l = b%rows(m)
                  ldim = partition(l)
                  k0 = b%k0(m)
                  blj => b%values(k0:)

                  k0 = a%pointer_b(l)
                  k1 = a%pointer_b(l + 1) - 1

                  do k = k0, k1
                        i = a%rows(k)
                        idim = partition(i)
                        bblocks(i) = ior(bblocks(i), 1)

                        i0 = cumulative_sum(i) * jdim + 1
                        cij => work(i0:)

                        i0 = a%k0(k)
                        ail => a%values(i0:)

                        call dgemm("N", "N", idim, jdim, ldim, &
                              one, ail, idim, blj, ldim, one, cij, idim)
                  end do
                  !
                  ! Blocks below diagonal of j-th column of B
                  !
                  do m = m0 + 1, m1
                        l = b%rows(m)
                        ldim = partition(l)
                        k0 = b%k0(m)
                        ! 
                        ! Storing information about counterpart
                        ! symmetric block
                        !
                        nc = ncomplement(l) + 1
                        ncomplement(l) = nc
                        pos = sypos(nc, l)
                        complement(pos) = j
                        complement_k0(pos) = k0

                        blj => b%values(k0:)

                        k0 = a%pointer_b(l)
                        k1 = a%pointer_b(l + 1) - 1

                        do k = k0, k1
                              i = a%rows(k)
                              idim = partition(i)
                              bblocks(i) = ior(bblocks(i), 1)

                              i0 = cumulative_sum(i) * jdim + 1
                              cij => work(i0:)

                              i0 = a%k0(k)
                              ail => a%values(i0:)

                             call dgemm("N", "N", idim, jdim, ldim, &
                                   one, ail, idim, blj, ldim, one, cij, idim)
                        end do

                        pos = sypos(ncomplementa(l), l)
                        do k = ncomplementa(l), 1, -1
                              i = complementa(pos)
                              !
                              ! Calculate only c_{ij}, i >= j
                              !
                              if (i .ge. j) then
                                    idim = partition(i)
                                    bblocks(i) = ior(bblocks(i), 1)

                                    i0 = cumulative_sum(i) * jdim + 1
                                    cij => work(i0:)

                                    i0 = complement_k0a(pos)
                                    ail => a%values(i0:)

                                    call dgemm("T", "N", idim, jdim, ldim, &
                                          one, ail, ldim, blj, ldim, one, cij, idim)                              
                                    pos = pos - 1
                              else
                                    exit
                              end if
                        end do
                  end do
                  !
                  ! b_{lj}, j > l
                  !
                  pos = sypos(1, j)
                  do m = 1, ncomplement(j)
                        l = complement(pos)
                        ldim = partition(l)
                        k0 = complement_k0(pos)

                        blj => b%values(k0:)

                        k0 = a%pointer_b(l)
                        k1 = a%pointer_b(l + 1) - 1

                        do k = k0, k1
                              i = a%rows(k)
                              !
                              ! Calculate only c_{ij}, i >= j
                              !
                              if (i .ge. j) then
                                    idim = partition(i)
                                    bblocks(i) = ior(bblocks(i), 1)

                                    i0 = cumulative_sum(i) * jdim + 1
                                    cij => work(i0:)

                                    i0 = a%k0(k)
                                    ail => a%values(i0:)

                                    call dgemm("N", "T", idim, jdim, ldim, &
                                          one, ail, idim, blj, jdim, one, cij, idim)
                              end if
                        end do

                        pos = pos + 1
                  end do
                  !
                  ! Always include diagonal block for performance reasons 
                  ! (faster evaluation of trace and other diagonal operations)
                  !
                  nblocks = 0
                  iblocks = 0
                  do i = j + 1, ngroups
                        bb = bblocks(i)
                        nblocks = nblocks + bb
                        iblocks(nblocks) = iblocks(nblocks) + i * bb
                  end do
                  !
                  ! Add column to matrix c
                  !
                  call spcol(work, j, c, iblocks, nblocks, neglect)
            end do
      end subroutine spsymm


      subroutine spsyapb(a, b, c, neglect)
            !
            ! *********************************************************
            ! DESCRIPTION:
            ! *********************************************************
            !
            ! Sparse-sparse matrix addition:
            ! C = A + B,    A, B, C - symmetric sparse matrices
            !               storing only x_{ij}, i >= j
            !
            ! *********************************************************
            ! REFERENCES:
            ! *********************************************************
            !
            ! 1. Rubensson, E.H., Salek, P.,
            !    J. Comput. Chem. 26, 1628 (2005)
            !
            ! *********************************************************
            ! INPUTS:
            ! *********************************************************
            !
            ! A, B, C   - Sparse matrix structures
            !
            ! NEGLECT   - If set to 1, SPCOL subroutine will look for
            !             negligible blocks in C
            !             
            !      
            type(spmatrix) :: a, b, c
            integer, intent(in) :: neglect

            integer :: i, j, k
            integer :: idim, jdim
            integer :: i0, i1
            integer :: k0, k1, k00, k11
            integer :: blocksize
            integer :: nblocks
            integer :: bb
            
            c%next_element = 1

            do j = 1, ngroups
                  jdim = partition(j)
                  bblocks = 0
                  work = zero

                  i0 = a%pointer_b(j)
                  i1 = a%pointer_b(j + 1) - 1
                  
                  do i = i0, i1
                        k = a%rows(i)
                        bblocks(k) = 1
                        idim = partition(k)
                        blocksize = idim * jdim
                        k0 = cumulative_sum(k) * jdim + 1
                        k1 = k0 + blocksize - 1
                        k00 = a%k0(i)
                        k11 = k00 + blocksize - 1
                        work(k0:k1) = work(k0:k1) + a%values(k00:k11)
                  end do

                  i0 = b%pointer_b(j)
                  i1 = b%pointer_b(j + 1) - 1
                  
                  do i = i0, i1
                        k = b%rows(i)
                        idim = partition(k)
                        bblocks(k) = ior(bblocks(k), 1)
                        blocksize = idim * jdim
                        k0 = cumulative_sum(k) * jdim + 1
                        k1 = k0 + blocksize - 1
                        k00 = b%k0(i)
                        k11 = k00 + blocksize - 1
                        work(k0:k1) = work(k0:k1) + b%values(k00:k11)
                  end do
             
                  !
                  ! Always include diagonal block for performance reasons 
                  ! (faster evaluation of trace and other diagonal operations)
                  !
                  nblocks = 0
                  iblocks = 0
                  !
                  ! Diagonal block skipped
                  !
                  do i = j + 1, ngroups
                        bb = bblocks(i)
                        nblocks = nblocks + bb
                        iblocks(nblocks) = iblocks(nblocks) + i * bb
                  end do
                  !
                  ! Add j-th column to sparse matrix C
                  !
                  call spcol(work, j, c, iblocks, nblocks, neglect)

            end do
      end subroutine spsyapb

      
      function sptrace(a)
            double precision :: sptrace

            type(spmatrix) :: a

            double precision :: sum
            integer :: i, j, k, jdim
            integer :: rowindex

            sum = zero
            do i = 1, ngroups
                  k = a%pointer_b(i)
                  rowindex = a%rows(k)
                  k = a%k0(k)
                  jdim = partition(rowindex)
                  
                  do j = 1, jdim
                        sum = sum + a%values(k)
                        k = k + jdim + 1
                  end do
            end do

            sptrace = sum
      end function sptrace

      
      subroutine spscale(a, alpha)
            type(spmatrix) :: a
            double precision, intent(in) :: alpha

            integer :: i

            do i = 1, a%next_element - 1
                  a%values(i) = a%values(i) * alpha
            end do
      end subroutine spscale


      subroutine spcpy(a, b)
            !
            ! B = A
            !
            type(spmatrix) :: a, b

            b%pointer_b = a%pointer_b
            b%rows = a%rows
            b%k0 = a%k0
            b%values(1:a%next_element - 1) = a%values(1:a%next_element - 1)
            b%next_element = a%next_element
      end subroutine spcpy


      subroutine spgeape(a, alpha, beta)
            !
            ! A = alpha * A + beta * I
            !
            type(spmatrix) :: a
            double precision, intent(in) :: alpha, beta

            integer :: i, j, k, jdim
            integer :: rowindex
            
            !
            ! Scale matrix A by scalar alpha
            !
            call spscale(a, alpha)
            
            do i = 1, ngroups
                  k = a%pointer_b(i)
                  rowindex = a%rows(k)
                  k = a%k0(k)
                  jdim = partition(rowindex)

                  do j = 1, jdim
                        a%values(k) = a%values(k) + beta
                        k = k + jdim + 1
                  end do
            end do
      end subroutine spgeape
      
      
      subroutine spgedrep(a, b)
            type(spmatrix) :: a
            double precision, dimension(:, :), intent(out) :: b
            
            integer :: i, j, k, u
            integer :: m0, m1
            integer :: k0
            integer :: i0, i1, j0, i00, i11
            integer :: idim, jdim
            
            b = zero

            do j = 1, ngroups
                  m0 = a%pointer_b(j)
                  m1 = a%pointer_b(j + 1) - 1
                  jdim = partition(j)
                  j0 = cumulative_sum(j)

                  do i = m0, m1
                        k = a%rows(i)
                        k0 = a%k0(i)
                        idim = partition(k)
                        
                        i0 = cumulative_sum(k) + 1
                        i1 = i0 + idim - 1
                        i00 = k0
                        i11 = k0 + idim - 1
                        do u = 1, jdim
                             b(i0:i1, j0 + u) = a%values(i00:i11)
                             i00 = i00 + idim
                             i11 = i11 + idim
                        end do
                  end do
            end do
      end subroutine spgedrep


      subroutine spsydrep(a, b)
            type(spmatrix) :: a
            double precision, dimension(:, :), intent(out) :: b
            
            integer :: i, j
            
            call spgedrep(a, b)

            do j = 1, norb - 1
                  do i = j + 1, norb
                        b(j, i) = b(i, j)
                  end do
            end do
      end subroutine spsydrep
end module sparse



