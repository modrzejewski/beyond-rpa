! -----------------------------------------------------------------
!                       JOURNAL MODULE
! -----------------------------------------------------------------
! A set of subroutines for keeping track of records stored in
! logical containers. A logical container can be for example an
! array in the memory, or a space reserved on a hard disk. Use this
! module for easy transfer of data between different types of
! memory.
!
module journal
      use display

      implicit none
      
      integer, parameter, private :: UNDEFINED = -1
      
      type titem
            !
            ! Container where the item is stored
            !
            integer :: cont = UNDEFINED
            !
            ! Position within the container
            !
            integer :: pos = UNDEFINED
      end type titem

      type tcontainer
            integer :: nstored
            integer, dimension(:), allocatable :: items
            integer, dimension(:), allocatable :: freepos
            integer :: nmax
      end type tcontainer

      type tjournal
            integer :: nitems
            integer :: max_nitems
            integer :: ncontainers
            type(tcontainer), dimension(:), allocatable :: containers
            type(titem), dimension(:), allocatable :: items
      contains
            procedure, pass :: init => journal_init
            procedure, pass :: write => journal_write
            procedure, pass :: read => journal_read
            procedure, pass :: delete => journal_delete
            procedure, pass :: move => journal_move
            procedure, pass :: nfree => journal_nfree
            procedure, pass :: isfree => journal_isfree
            procedure, pass :: destroy => journal_destroy
      end type tjournal

contains
      
      subroutine journal_init(this, nmax)
            ! --------------------------------------------
            ! Initialize a TJOURNAL instance.
            ! --------------------------------------------
            ! NMAX
            !       SIZE(NMAX) is read to set the number
            !       of logical containers. NMAX(K) is
            !       the maximum number of records in K-th
            !       container.
            !
            class(tjournal), intent(out)      :: this
            integer, dimension(:), intent(in) :: nmax

            integer :: n, k, l, ncontainers

            ncontainers = size(nmax)
            n = sum(nmax)

            this%ncontainers = ncontainers
            this%nitems = 0
            this%max_nitems = 0
            allocate(this%items(n))
            allocate(this%containers(ncontainers))
            do k = 1, ncontainers
                  associate (t => this%containers(k))
                        t%nstored = 0
                        t%nmax = nmax(k)
                        allocate(t%items(nmax(k)))
                        allocate(t%freepos(nmax(k)))
                        !
                        ! Free indices are stored in descending order
                        !
                        t%freepos = [(l, l = nmax(k), 1, -1)]
                        t%items = UNDEFINED
                        this%max_nitems = this%max_nitems + t%nmax
                  end associate
            end do
      end subroutine journal_init

      
      subroutine journal_destroy(this)
            class(tjournal), intent(out) :: this
            
            if (allocated(this%items)) deallocate(this%items)
            if (allocated(this%containers)) deallocate(this%containers)
            this%nitems = 0
            this%max_nitems = 0
            this%ncontainers = 0
      end subroutine journal_destroy
      

      function journal_write(this, item, c)
            ! --------------------------------------------------
            ! Store a new item in a specified container. The
            ! UNDEFINED value is returned if no space is left
            ! within the specified container. The position of
            ! the item is returned if writing attempt succeeds.
            ! The position of ITEM within C is locked until one
            ! calls JOURNAL_DELETE.
            ! --------------------------------------------------
            ! ITEM
            !        Identifier of the new item.
            ! C   
            !        Container in which the new item is to be
            !        stored.
            ! 
            ! JOURNAL_WRITE
            !        If the container has free spece left,
            !        JOURNAL_WRITE equals the position of the
            !        new item within C. Otherwise, UNDEFINED
            !        is returned.
            !        
            !
            integer                        :: journal_write
            class(tjournal), intent(inout) :: this
            integer, intent(in)            :: item
            integer, intent(in)            :: c

            integer :: nfree
            integer :: pos

            associate (t => this%containers(c), i => this%items(item))
                  nfree = t%nmax - t%nstored
                  if (nfree > 0) then
                        call this%delete(item)
                        pos = t%freepos(nfree)
                        t%nstored = t%nstored + 1
                        t%items(pos) = item
                        i%cont = c
                        i%pos = pos
                        this%nitems = this%nitems + 1
                  else
                        pos = UNDEFINED
                  end if
                  journal_write = pos
            end associate
      end function journal_write

      
      subroutine journal_delete(this, item)
            ! ------------------------------------------
            ! Clear the journal entry for ITEM. Unlock
            ! free space in the container where ITEM is
            ! stored.
            ! ------------------------------------------
            class(tjournal), intent(inout) :: this
            integer, intent(in)            :: item

            integer :: c, pos, nfree
            integer :: k
            integer :: idx

            associate (i => this%items(item))
                  if (i%cont .ne. UNDEFINED) then
                        c = i%cont
                        pos = i%pos
                        i%cont = UNDEFINED
                        i%pos = UNDEFINED
                        !
                        ! Insert POS into FREEPOS. The indices in
                        ! FREEPOS are stored in decreasing order.
                        !
                        associate (t => this%containers(c))
                              nfree = t%nmax - t%nstored
                              if (nfree > 0) then
                                    !
                                    ! Find the largest index IDX for which
                                    ! FREEPOS(IDX) > POS. The binary search
                                    ! returns IDX=0 if all values in FREEPOS
                                    ! are smaller than POS.
                                    !
                                    call binary_search(t%freepos, nfree, pos, idx)
                                    do k = nfree+1, idx+2, -1
                                          t%freepos(k) = t%freepos(k-1)
                                    end do
                                    t%freepos(idx+1) = pos
                              else
                                    t%freepos(1) = pos
                              end if
                              t%nstored = t%nstored - 1
                              t%items(pos) = UNDEFINED
                              this%nitems = this%nitems - 1
                        end associate
                  end if
            end associate
      end subroutine journal_delete


      subroutine journal_read(this, item, pos, c)
            ! -----------------------------------------
            ! Read the position of ITEM.
            ! -----------------------------------------
            ! ITEM
            !        Identifier of the requested item
            ! POS
            !        Position of ITEM within the
            !        container C
            ! C 
            !        Identifier of the container where
            !        ITEM is stored.
            !
            class(tjournal), intent(in) :: this
            integer, intent(in)         :: item
            integer, intent(out)        :: pos
            integer, intent(out)        :: c

            pos = this%items(item)%pos
            c = this%items(item)%cont
      end subroutine journal_read

      
      function journal_isfree(this, pos, cont)
            !
            ! Test if the specified position in CONT is occupied.
            !
            logical                     :: journal_isfree
            class(tjournal), intent(in) :: this
            integer, intent(in)         :: pos
            integer, intent(in)         :: cont

            if (cont <= 0 .or. cont > this%ncontainers) then
                  call msg("ISFREE: THE VALUE OF CONT BEYOND ACCEPTABLE RANGE", MSG_ERROR)
                  call imsg("CONT", cont, MSG_ERROR)
                  stop
            end if

            if (pos <= 0 .or. pos > this%containers(cont)%nmax) then
                  print*, 'pos', this%containers(cont)%nmax
                  call msg("ISFREE: THE VALUE OF POS BEYOND ACCEPTABLE RANGE", MSG_ERROR)
                  call imsg("POS", pos, MSG_ERROR)
                  stop
            end if

            if (this%containers(cont)%items(pos) == UNDEFINED) then
                  journal_isfree = .true.
            else
                  journal_isfree = .false.
            end if
      end function journal_isfree
      

      subroutine journal_move(this, source_pos, source_cont, target_pos, target_cont)
            ! ------------------------------------------------------------------------
            ! Move an item from position SOURCE_POS in container SOURCE_CONT to
            ! the first container which has a free position. UNDEFINED value is 
            ! returned if SOURCE_POS corresponds to an ulocked (unoccupied) position
            ! in SOURCE_CONT. UNDEFINED is also returned if no further container has
            ! space left.
            ! ------------------------------------------------------------------------
            class(tjournal), intent(inout) :: this
            integer, intent(in)            :: source_pos
            integer, intent(in)            :: source_cont
            integer, intent(out)           :: target_pos
            integer, intent(out)           :: target_cont

            integer :: item, nfree, k

            target_pos = UNDEFINED
            target_cont = UNDEFINED
            item = this%containers(source_cont)%items(source_pos)
            if (item == UNDEFINED) return
                  
            do k = 1, this%ncontainers
                  if (k == source_cont) then
                        associate (t => this%containers(source_cont), &
                              i => this%items(item))
                              nfree = t%nmax - t%nstored                        
                              if (nfree > 0) then
                                    target_pos = t%freepos(nfree)
                                    if (target_pos < source_pos) then
                                          call this%delete(item)
                                          target_pos = this%write(item, source_cont)
                                    else
                                          t%freepos(nfree) = source_pos
                                          t%items(source_pos) = UNDEFINED
                                          t%items(target_pos) = item
                                          i%pos = target_pos
                                    end if
                              else
                                    target_pos = UNDEFINED
                              end if
                        end associate
                  else
                        target_pos = this%write(item, k)
                  end if

                  if (target_pos .ne. UNDEFINED) then
                        target_cont = k
                        exit
                  end if
            end do
      end subroutine journal_move

      
      function journal_nfree(this)
            integer :: journal_nfree
            class(tjournal), intent(in) :: this

            journal_nfree = this%max_nitems - this%nitems
      end function journal_nfree


      pure subroutine binary_search(list, n, thresh, idx)
            ! ----------------------------------------------------------------
            ! Perform binary search in a list of numbers sorted in
            ! decreasing order. Find the largest IDX for which
            ! LIST(IDX) >= THRESH.
            !
            ! Four possible cases
            ! --------------------
            !
            ! THRESH = 4
            ! LIST = [9, 8, 5, 4, 1, 0]
            ! IDX <- 4
            !
            ! THRESH = 1
            ! LIST = [2, 1, 1, 0]
            ! IDX <- 3
            !
            ! THRESH = 4
            ! LIST = [4, 3, 2, 2]
            ! IDX <- 4 (NO ELEMENTS BELOW THRESHOLD)
            !
            ! THRESH = 4
            ! LIST    = [2, 1, 1, 0]
            ! IDX <- 0 (ALL ELEMENTS BELOW THRESHOLD)
            ! ----------------------------------------------------------------
            ! LIST 
            !        List of integer numbers sorted in decreasing order
            ! N    
            !        Number of elements in LIST. N > 0.
            ! THRESH
            !        Threshold value
            ! IDX  
            !        Output, LIST(IDX) >= THRESH,
            !        LIST(IDX + 1) < THRESH OR
            !        IDX + 1 > N.
            !        IDX = 0 if LIST(1) < THRESH, that is, all elements
            !        are below threshold.
            !
            integer, dimension(:), intent(in) :: list
            integer, intent(in)               :: n
            integer, intent(in)               :: thresh
            integer, intent(out)              :: idx

            integer :: j, k

            if (list(n) >= thresh) then
                  idx = n
            else if (list(1) < thresh) then
                  idx = 0
            else
                  idx = 1
                  j = n
                  bisection: do
                        k = (idx + j) / 2
                        if (thresh .le. list(k)) then
                              idx = k
                        else
                              j = k
                        end if
                        if (idx + 1 .ge. j) then
                              exit bisection
                        end if
                  end do bisection
            end if
      end subroutine binary_search
end module journal
