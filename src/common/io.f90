! ------------------------------------------------------------------------
!                          INPUT/OUTPUT MODULE
! ------------------------------------------------------------------------
! Subprograms for writing i) unformatted stream (binary) files,
! ii) formatted sequential (text) files, iii) record files
! ------------------------------------------------------------------------
!                               GLOSSARY
! ------------------------------------------------------------------------
! TEXT FILE
!          A sequential formatted file defined by the Fortran Standard.
!          It is a portable choice for storing data. Recommended if
!          the file is expected to be used on different machines.
! BINARY FILE
!          An unformatted stream file defined by the Fortran Standard.
!          Reading and writing operations on binary files are extremely
!          fast. Binary files also are more space-efficient than text
!          files. However, it is not recommended to move binary files
!          between different machines.
! RECORD
!          A storage record is a set of files containing a binary file
!          (where the actual data are stored) and text files which
!          contain meta-data. The meta-data enable easy verification and
!          re-use of the actual data contained in a record when
!          recovering from a power outage or system crash. It is a
!          fail-safe choice for writing binary files.
! LINK
!          A text file pointing to a binary file. This is a part of the
!          fail-safe infrastructure of the storage record.
!
module io
      use arithmetic
      use string
      use display
      use gparam
      use iso_fortran_env
      use clock

      implicit none
      save

      interface io_size_byte
            module procedure :: io_size_byte_rank1_F64
            module procedure :: io_size_byte_rank2_F64
            module procedure :: io_size_byte_rank3_F64
            module procedure :: io_size_byte_rank4_F64
            module procedure :: io_size_byte_rank1_F32
            module procedure :: io_size_byte_rank2_F32
            module procedure :: io_size_byte_rank3_F32
            module procedure :: io_size_byte_rank4_F32
      end interface io_size_byte

      interface io_binary_read
            module procedure :: io_binary_read_rank1
            module procedure :: io_binary_read_rank2
      end interface io_binary_read

      interface io_binary_write
            module procedure :: io_binary_write_rank1
            module procedure :: io_binary_write_rank2
      end interface io_binary_write

      interface io_record_write
            module procedure :: io_record_write_rank1
            module procedure :: io_record_write_rank2
      end interface io_record_write

      interface io_record_read
            module procedure :: io_record_read_rank1
            module procedure :: io_record_read_rank2
      end interface io_record_read

      interface io_record_axpy
            module procedure :: io_record_axpy_rank1
      end interface io_record_axpy
      !
      ! The maximum length of an I/O error message
      !
      integer, parameter :: IO_MAX_MSGLEN = 256
      !
      ! A scratch directory for record files. A directory
      ! separator is the last character of IO_SCRATCHDIR.
      !
      character(:), allocatable :: IO_SCRATCHDIR
      !
      ! Maximum number of records in a record group
      !
      integer, parameter :: IO_MAX_RECORDS = 1000
      type trecord
            !
            ! Identifier for the record data.
            !
            character(len=DEFLEN) :: basename = ""
            !
            ! The current version of the record file,
            ! VERSION = 1, 2, 3, ...
            ! It is incremented as soon the record file
            ! corresponding to BASENAME is successfully
            ! stored. Only the record file corresponding
            ! to the newest version is kept on disk.
            !
            integer :: version = 0
            !
            ! Size of the record file in bytes.
            !
            integer(I64) :: size = 0
      end type trecord

      type trecgroup
            type(trecord), dimension(IO_MAX_RECORDS) :: c
            !
            ! Number of record files. Note that only a single version
            ! of a record file is stored.
            !
            integer :: n = 0
            !
            ! Total size of all record files in bytes.
            !
            integer(I64) :: size = 0
            !
            ! Maximum total size of the record files belonging
            ! to this group in bytes.
            ! Ignored if MAX_SIZE==0.
            !
            integer(I64) :: max_size = 0
            logical :: init = .false.
            !
            ! Prefix for record filenames
            !
            character(len=DEFLEN) :: prefix = ""
            ! ------------------------------------------------
            ! READ/WRITE STATISTICS
            ! ------------------------------------------------
            !
            ! Total number of bytes read
            !
            integer(I64) :: bytes_read = I64_ZERO
            !
            ! Total number of bytes written
            !
            integer(I64) :: bytes_write = I64_ZERO
            !
            ! Total number of wallclock seconds spent reading
            !
            real(F64) :: seconds_read = ZERO
            !
            ! Total number of wallclock seconds spent writing
            !
            real(F64) :: seconds_write = ZERO
      end type trecgroup

      type TState
            integer, dimension(:, :), allocatable :: tlexci                  
            integer, dimension(:, :), allocatable :: tuexci
            integer, dimension(:), allocatable :: tiexci
            integer :: tmaxexc
            type(trecgroup), dimension(:), allocatable :: trecs
            real(F64), dimension(:, :), allocatable :: twr_exc                  
            integer :: tnidx
      end type TState


      integer, parameter :: IO_SUCCESS = 0
      integer, parameter :: IO_SPACE_EXCEEDED = 1
      integer, parameter :: IO_INTEGRITY_FAILURE = 2
      integer, parameter :: IO_NONEXISTENT = 3

contains

      subroutine io_set_scratchdir(s)
            !
            ! Set a scratch directory for writing record files.
            !
            character(*), intent(in) :: s
            
            integer :: k
            
            k = len(s)
            
            if (s(k:k) == DIRSEP) then
                  IO_SCRATCHDIR = s
            else
                  IO_SCRATCHDIR = s // DIRSEP
            end if
      end subroutine io_set_scratchdir


      function io_size_byte_rank1_F64(a)
            !
            ! Size of an array A in bytes. The size in bytes
            ! is stored in an integer of I64 kind to prevent
            ! an overflow for large arrays.
            !
            integer(I64)                        :: io_size_byte_rank1_F64
            real(F64), dimension(:), intent(in) :: a

            io_size_byte_rank1_F64 = storage_size(a, kind=I64) &
                  * size(a, kind=I64) / 8_I64
      end function io_size_byte_rank1_F64


      function io_size_byte_rank2_F64(a)
            integer(I64)                           :: io_size_byte_rank2_F64
            real(F64), dimension(:, :), intent(in) :: a

            io_size_byte_rank2_F64 = storage_size(a, kind=I64) &
                  * size(a, kind=I64) / 8_I64
      end function io_size_byte_rank2_F64


      function io_size_byte_rank3_F64(a)
            integer(I64)                              :: io_size_byte_rank3_F64
            real(F64), dimension(:, :, :), intent(in) :: a

            io_size_byte_rank3_F64 = storage_size(a, kind=I64) &
                  * size(a, kind=I64) / 8_I64
      end function io_size_byte_rank3_F64


      function io_size_byte_rank4_F64(a)
            integer(I64)                                 :: io_size_byte_rank4_F64
            real(F64), dimension(:, :, :, :), intent(in) :: a

            io_size_byte_rank4_F64 = storage_size(a, kind=I64) &
                  * size(a, kind=I64) / 8_I64
      end function io_size_byte_rank4_F64


      function io_size_byte_rank1_F32(a)
            !
            ! Size of an array A in bytes. The size in bytes
            ! is stored in an integer of I64 kind to prevent
            ! an overflow for large arrays.
            !
            integer(I64)                        :: io_size_byte_rank1_F32
            real(F32), dimension(:), intent(in) :: a

            io_size_byte_rank1_F32 = storage_size(a, kind=I64) &
                  * size(a, kind=I64) / 8_I64
      end function io_size_byte_rank1_F32


      function io_size_byte_rank2_F32(a)
            integer(I64)                           :: io_size_byte_rank2_F32
            real(F32), dimension(:, :), intent(in) :: a

            io_size_byte_rank2_F32 = storage_size(a, kind=I64) &
                  * size(a, kind=I64) / 8_I64
      end function io_size_byte_rank2_F32


      function io_size_byte_rank3_F32(a)
            integer(I64)                              :: io_size_byte_rank3_F32
            real(F32), dimension(:, :, :), intent(in) :: a

            io_size_byte_rank3_F32 = storage_size(a, kind=I64) &
                  * size(a, kind=I64) / 8_I64
      end function io_size_byte_rank3_F32


      function io_size_byte_rank4_F32(a)
            integer(I64)                                 :: io_size_byte_rank4_F32
            real(F32), dimension(:, :, :, :), intent(in) :: a

            io_size_byte_rank4_F32 = storage_size(a, kind=I64) &
                  * size(a, kind=I64) / 8_I64
      end function io_size_byte_rank4_F32
      
      
      function io_exists(s)
            !
            ! Check if the file referenced by S exists.
            !
            logical :: io_exists
            character(*), intent(in) :: s
            
            inquire(file=s, exist=io_exists)
      end function io_exists


      function io_argv(k)
            !
            ! Return a K-th command line argument of the command
            ! that invoked the main program.
            !
            character(:), allocatable :: io_argv
            integer, intent(in) :: k

            integer :: l

            call get_command_argument(k, length=l)
            allocate(character(l) :: io_argv)
            call get_command_argument(k, io_argv)
      end function io_argv


      subroutine io_remove(f)
            !
            ! Remove a file. Return if file does not exist.
            !
            character(*), intent(in) :: f
            
            integer :: u, ios
            character(len=IO_MAX_MSGLEN) :: errmsg
            
            open(newunit=u, file=f, status="OLD", iostat=ios)
            if (ios == 0) then
                  close(u, status="DELETE", iostat=ios, iomsg=errmsg)
                  if (ios .ne. 0) then
                        call msg("COULD NOT DELETE FILE", MSG_ERROR)
                        call msg(trim(adjustl(f)), MSG_ERROR)
                        call msg(trim(errmsg), MSG_ERROR)
                        stop
                  end if
            end if
      end subroutine io_remove


      function io_text_open(filename, s)
            !
            ! Open a text file.
            !
            integer                            :: io_text_open
            character(*), intent(in)           :: filename
            character(*), intent(in)           :: s

            integer :: open_stat
            character(len=IO_MAX_MSGLEN) :: errmsg

            open(newunit=io_text_open, file=filename, status=s, &
                  access="SEQUENTIAL", iostat=open_stat, iomsg=errmsg)
            
            if (open_stat .ne. 0) then
                  call msg("Could not open file", MSG_ERROR)
                  call msg(trim(adjustl(filename)), MSG_ERROR)
                  call msg(trim(errmsg), MSG_ERROR)
                  error stop
            end if
      end function io_text_open


      subroutine io_text_readline(line, u, eof)
            !
            ! Read a line from a text file. The limit for the line
            ! size is MAXCHUNKS * DEFLEN characters (see the code).
            !
            character(:), allocatable, intent(out) :: line
            integer, intent(in)                    :: u
            logical, optional, intent(out)         :: eof

            character(len=80) :: chunk
            character(len=IO_MAX_MSGLEN) :: errmsg
            integer :: s, ios
            integer :: n
            integer, parameter :: maxchunks = 2**10

            line = ""
            if (present(eof)) eof = .false.
            
            lineloop: do n = 1, maxchunks
                  read(u, "(A)", advance="NO", size=s, &
                        iostat=ios, iomsg=errmsg) chunk

                  if (s > 0) then
                        line = line // chunk(1:s)
                  end if

                  if (ios == iostat_end) then
                        if (present(eof)) eof = .true.
                        exit lineloop
                  else if (ios == iostat_eor) then
                        exit lineloop
                  else if (ios .ne. 0) then
                        call msg("COULD NOT READ LINE", MSG_ERROR)
                        call msg(trim(errmsg), MSG_ERROR)
                        error stop
                  end if
            end do lineloop
      end subroutine io_text_readline


      subroutine io_text_write(a, filename)
            !
            ! Save a matrix of floating-point numbers into a text file.
            ! An appropriate format is chosen to retain the full precision
            ! of the input. Use this subroutine to write portable, human-
            ! readable files. Use IO_BINARY_WRITE if the data need not be
            ! read on other machines.
            !
            real(F64), dimension(:, :), intent(in) :: a
            character(*), intent(in)           :: filename

            integer :: u
            integer :: m, n
            integer :: i, j
            character(:), allocatable :: fmt

            u = io_text_open(filename, "REPLACE")
            m = size(a, dim=1)
            n = size(a, dim=2)
            !
            ! ({N}(ES{F64_ES_W}.{F64_ES_D}E{F64_ES_E},:,1X))
            !
            fmt = "(" // str(n) // "(ES" // str(F64_ES_W) // "." // &
                  str(F64_ES_D) // "E" // str(F64_ES_E) // ",:,1X))"

            do i = 1, m
                  write(u, fmt) (a(i, j), j = 1, n)
            end do

            close(u)
      end subroutine io_text_write


      subroutine io_text_read(a, filename)
            !
            ! Read a matrix of floating-point numbers from a text file.
            ! See the comments for IO_TEXT_WRITE.
            !
            real(F64), dimension(:, :), intent(out) :: a
            character(*), intent(in)            :: filename

            integer :: u
            integer :: m, n
            integer :: i, j

            u = io_text_open(filename, "OLD")
            m = size(a, dim=1)
            n = size(a, dim=2)

            do i = 1, m
                  read(u, *) (a(i, j), j = 1, n)
            end do

            close(u)
      end subroutine io_text_read


      function io_binary_open(filename, s)
            !
            ! Open a binary file.
            !
            integer                  :: io_binary_open
            character(*), intent(in) :: filename
            character(*), intent(in) :: s

            integer :: open_stat
            character(len=IO_MAX_MSGLEN) :: errmsg

            open(newunit=io_binary_open, access="STREAM", form="UNFORMATTED", &
                  file=filename, status=s, iostat=open_stat, iomsg=errmsg)
            
            if (open_stat .ne. 0) then
                  call msg("COULD NOT OPEN FILE", MSG_ERROR)
                  call msg(trim(adjustl(filename)), MSG_ERROR)
                  call msg(trim(errmsg), MSG_ERROR)
                  stop
            end if
      end function io_binary_open


      subroutine io_binary_write_rankn(a, n, filename)
            real(F64), dimension(*), intent(in) :: a
            integer, intent(in)                 :: n
            character(*), intent(in)            :: filename
            
            call io_binary_write_rank1(a(1:n), filename)
      end subroutine io_binary_write_rankn
      

      subroutine io_binary_write_rank1(a, filename)
            !
            ! Save a matrix of floating-point numbers into a binary file.
            ! The binary file is not guaranteed to be portable to other
            ! machines. For example, one of the reasons for the non-portability
            ! is the endianness of a machine. Use this subroutine to efficiently
            ! write scratch files and records. See also IO_TEXT_WRITE.
            ! A can be a noncontiguous array.
            !
            real(F64), dimension(:), intent(in) :: a
            character(*), intent(in)            :: filename

            integer :: k0, k1, k, n, nblocks
            integer :: u, write_stat
            integer, parameter :: blocksize = 1024**2
            character(len=IO_MAX_MSGLEN) :: errmsg

            u = io_binary_open(filename, "REPLACE")
            n = size(a, dim=1)
            nblocks = n / blocksize
            if (modulo(n, blocksize) > 0) then
                  nblocks = nblocks + 1
            end if
            k0 = 1
            do k = 1, nblocks
                  k1 = min(n, k0+blocksize-1)
                  write(u, iostat=write_stat, iomsg=errmsg) a(k0:k1)
                  k0 = k1
                  if (write_stat .ne. 0) then
                        call msg("COULD NOT WRITE TO FILE")
                        call msg(trim(filename))
                        call msg(trim(errmsg))
                        stop
                  end if
            end do

            close(u)
      end subroutine io_binary_write_rank1


      subroutine io_binary_write_rank2(a, filename)
            real(F64), dimension(:, :), contiguous, intent(in) :: a
            character(*), intent(in)                           :: filename

            integer :: n
            
            n = size(a)
            call io_binary_write_rankn(a, n, filename)
      end subroutine io_binary_write_rank2

      
      subroutine io_binary_read_rankn(a, n, filename)
            real(F64), dimension(*), intent(out) :: a
            integer, intent(in)                  :: n
            character(*), intent(in)             :: filename
            
            call io_binary_read_rank1(a(1:n), filename)
      end subroutine io_binary_read_rankn


      subroutine io_binary_read_rank1(a, filename)
            !
            ! Read a matrix of floating-point numbers from a binary file.
            ! See the comments for IO_BINARY_WRITE_RANK1.
            !
            real(F64), dimension(:), intent(out) :: a
            character(*), intent(in)         :: filename

            integer :: k0, k1, k, n
            integer :: u, read_stat
            integer :: nblocks
            integer :: blocksize = 1024**2
            character(len=IO_MAX_MSGLEN) :: errmsg

            u = io_binary_open(filename, "OLD")
            n = size(a)
            nblocks = n / blocksize
            if (modulo(n, blocksize) > 0) then
                  nblocks = nblocks + 1
            end if
            k0 = 1
            do k = 1, nblocks
                  k1 = min(n, k0+blocksize-1)
                  read(u, iostat=read_stat, iomsg=errmsg) a(k0:k1)
                  k0 = k1
                  if (read_stat .ne. 0) then
                        call msg("COULD NOT READ FROM FILE", MSG_ERROR)
                        call msg(trim(filename), MSG_ERROR)
                        call msg(trim(errmsg), MSG_ERROR)
                        stop
                  end if
            end do

            close(u)
      end subroutine io_binary_read_rank1


      subroutine io_binary_read_rank2(a, filename)
            real(F64), dimension(:, :), contiguous, intent(out) :: a
            character(*), intent(in)                            :: filename

            integer :: n

            n = size(a)
            call io_binary_read_rankn(a, n, filename)
      end subroutine io_binary_read_rank2


      subroutine io_binary_axpy_rank1(y, alpha, filename)
            !
            ! Perform AXPY operation, where X is read from
            ! a binary file:
            ! Y <- ALPHA * X + Y.
            ! See the comments for IO_BINARY_WRITE_RANK1.
            !
            real(F64), dimension(:), intent(inout) :: y
            real(F64), intent(in)                  :: alpha
            character(*), intent(in)               :: filename

            integer :: k0, k1, k, n
            integer :: u, read_stat
            integer :: nblocks
            integer :: blocksize = 1024**2
            real(F64), dimension(:), allocatable :: buffer
            character(len=IO_MAX_MSGLEN) :: errmsg

            allocate(buffer(blocksize))
            
            u = io_binary_open(filename, "OLD")
            n = size(y)
            nblocks = n / blocksize
            if (modulo(n, blocksize) > 0) then
                  nblocks = nblocks + 1
            end if
            k0 = 1
            do k = 1, nblocks
                  k1 = min(n, k0+blocksize-1)
                  read(u, iostat=read_stat, iomsg=errmsg) buffer(1:k1-k0+1)

                  if (read_stat .ne. 0) then
                        call msg("COULD NOT READ FROM FILE", MSG_ERROR)
                        call msg(trim(filename), MSG_ERROR)
                        call msg(trim(errmsg), MSG_ERROR)
                        stop
                  end if

                  y(k0:k1) = y(k0:k1) + alpha * buffer(1:k1-k0+1)
                  k0 = k1
            end do

            close(u)
            deallocate(buffer)
      end subroutine io_binary_axpy_rank1


      subroutine io_link_write(target_file, link_file)
            !
            ! Create a link file. The link file contains a single line
            ! with a path to the target file.
            !
            character(*), intent(in) :: target_file
            character(*), intent(in) :: link_file
            
            integer :: u
            
            u = io_text_open(link_file, "REPLACE")
            write(u, "(A)") target_file
            close(u)
      end subroutine io_link_write


      function io_link_read(link_file)
            character(:), allocatable :: io_link_read
            character(*), intent(in)  :: link_file

            integer :: u

            u = io_text_open(link_file, "OLD")
            call io_text_readline(io_link_read, u)
            close(u)
      end function io_link_read
      
      
      function record_filename(basename, version)
            !
            ! Generate a name of a record file corresponding
            ! to BASENAME. The file name is decorated with the job
            ! title and a version number.
            !
            character(:), allocatable :: record_filename
            character(*), intent(in)  :: basename
            integer, intent(in)       :: version
            
            record_filename = JOBTITLE // "_" // trim(basename) // "_" // str(version)
      end function record_filename
      
      
      function record_linkname(basename)
            !
            ! Generate the name of a link file corresponding
            ! to BASENAME. The link name is decorated with
            ! the job title only.
            !
            character(:), allocatable :: record_linkname
            character(*), intent(in)  :: basename
            
            record_linkname = JOBTITLE // "_" // trim(basename)
      end function record_linkname
      
      
      function record_find(g, basename)
            integer                           :: record_find
            type(trecgroup), intent(in)       :: g
            character(*), intent(in)          :: basename

            integer :: k

            record_find = -1
            kloop: do k = 1, g%n
                  if (g%c(k)%basename == basename) then
                        record_find = k
                        exit kloop
                  end if
            end do kloop
      end function record_find


      subroutine io_record_initgroup(g, max_size, prefix)
            !
            ! G has intent INOUT in order to preserve
            ! the disk usage statistics from previous
            ! records.
            !
            type(trecgroup), intent(inout) :: g
            integer(I64), intent(in)       :: max_size
            character(*), intent(in)       :: prefix

            g%max_size = max_size
            g%init = .true.
            g%prefix = prefix
      end subroutine io_record_initgroup


      subroutine tstate_initgroup(tst, order, lexci, uexci, nidx_ccsd, nidx_cc3, method)
            type(TState), intent(inout) :: tst
            integer, intent(in) :: order
            integer, dimension(:, :), intent(in) :: lexci, uexci
            integer :: i
            integer :: twr_exc_dim
            integer, intent(in) :: nidx_ccsd
            integer, intent(in) :: nidx_cc3
            integer, intent(in) :: method


            allocate(tst%tiexci(order))

            do i = 1, order
!                  print*, 'lexxxci', lexci(2, i), uexci(2, i)
                  tst%tiexci(i) = max(lexci(2, i), uexci(2, i))
            end do

            twr_exc_dim = sum(tst%tiexci)

            allocate(tst%tlexci(2, order))
            allocate(tst%tuexci(2, order))
            allocate(tst%trecs(order))
            allocate(tst%twr_exc(twr_exc_dim, order))

            select case(method)
            case(THEORY_CCSD)
                  tst%tnidx = nidx_ccsd
            case(THEORY_CC3)
                  tst%tnidx = nidx_cc3
            end select

      end subroutine tstate_initgroup



      subroutine io_record_deletegroup(g)
            type(trecgroup), intent(inout) :: g

            character(:), allocatable :: f, l
            integer :: i

            do i = 1, g%n
                  f = IO_SCRATCHDIR // &
                        record_filename(g%c(i)%basename, g%c(i)%version)
                  l = IO_SCRATCHDIR // record_linkname(g%c(i)%basename)
                  call io_remove(l)
                  call io_remove(f)
                  g%size = g%size - g%c(i)%size
            end do

            g%n = 0
            g%init = .false.
            g%prefix = ""
      end subroutine io_record_deletegroup


      subroutine io_record_write_rank1(g, idx, a, info)
            !
            ! Create a record file for a rank-1 array.
            ! Any future reference to this record must
            ! employ the identifier IDX.
            !
            type(trecgroup), intent(inout)      :: g
            integer, intent(in)                 :: idx
            real(F64), dimension(:), intent(in) :: a
            integer, intent(out)                :: info
            
            character(:), allocatable :: b
            character(:), allocatable :: filename
            character(:), allocatable :: linkname
            character(:), allocatable :: linktarget
            integer :: i, version
            logical :: newlink
            integer(I64) :: size
            type(tclock) :: clock

            info = IO_SUCCESS

            if (.not. g%init) then
                  call msg("TRECORDGROUP INSTANCE IS NOT INITIALIZED", MSG_ERROR)
                  stop
            end if

            b = trim(adjustl(g%prefix)) // str(idx)
            if (len(b) > DEFLEN) then
                  call msg("TOO LONG IDENTIFIER FOR A RECORD", MSG_ERROR)
                  stop
            end if
            
            i = record_find(g, b)
            if (i < 0) then
                  i = g%n + 1
                  version = 1
                  newlink = .true.
            else
                version = g%c(i)%version + 1
                newlink = .false.
            end if

            size = io_size_byte(a)

            linktarget = record_filename(b, version)
            filename = IO_SCRATCHDIR // linktarget
            linkname = IO_SCRATCHDIR // record_linkname(b)
            call clock_start(clock)

            call io_binary_write(a, filename)
            call io_link_write(target_file=linktarget, link_file=linkname)

            g%bytes_write = g%bytes_write + io_size_byte(a)
            g%seconds_write = g%seconds_write + clock_readwall(clock)            

            if (newlink) then
                  g%c(i)%basename = b
                  g%c(i)%version = 1
                  g%c(i)%size = size
                  g%size = g%size + size
                  g%n = g%n + 1
            else
                  g%c(i)%version = version
                  g%size = g%size + (size - g%c(i)%size)
                  g%c(i)%size = size
                  !
                  ! Delete old version
                  !
                  filename= IO_SCRATCHDIR // &
                        record_filename(b, version-1)
                  call io_remove(filename)
            end if
      end subroutine io_record_write_rank1

      
      subroutine io_record_write_rank2(g, idx, a, info)
            type(trecgroup), intent(inout)                     :: g
            integer, intent(in)                                :: idx
            real(F64), dimension(:, :), contiguous, intent(in) :: a
            integer, intent(out)                               :: info
            
            integer :: n

            n = size(a)
            call io_record_write_rankn(g, idx, a, n, info)
      end subroutine io_record_write_rank2
      

      subroutine io_record_write_rankn(g, idx, a, n, info)
            type(trecgroup), intent(inout)      :: g
            integer, intent(in)                 :: idx
            real(F64), dimension(*), intent(in) :: a
            integer, intent(in)                 :: n
            integer, intent(out)                :: info

            call io_record_write_rank1(g, idx, a(1:n), info)
      end subroutine io_record_write_rankn


      function io_record_path_to_binary(g, idx, info)

            character(:), allocatable :: io_record_path_to_binary
            type(trecgroup), intent(inout)       :: g
            integer, intent(in)                  :: idx
            integer, intent(out)                 :: info

            character(:), allocatable :: b
            character(:), allocatable :: linkname

            info = IO_SUCCESS
            b = trim(adjustl(g%prefix)) // str(idx)
            linkname = IO_SCRATCHDIR // record_linkname(b)
            io_record_path_to_binary = IO_SCRATCHDIR // io_link_read(linkname)

      end function io_record_path_to_binary

      
      function io_record_path_to_link(title, info)

            character(:), allocatable :: io_record_path_to_link
            character(:), allocatable, intent(in) :: title
            integer, intent(out)                 :: info

            info = IO_SUCCESS
!            io_record_path_to_link = IO_SCRATCHDIR // DIRSEP // title

            io_record_path_to_link = ROOTDIR //'saved-vectors' // DIRSEP // title

      end function io_record_path_to_link

      
      subroutine io_record_read_rank1(g, idx, a, info, name)
            !
            ! Read a rank-1 array from a record file.
            !
            type(trecgroup), intent(inout)       :: g
            integer, intent(in)                  :: idx
            real(F64), dimension(:), intent(out) :: a
            integer, intent(out)                 :: info
            character(*), optional, intent(in)             :: name

            character(:), allocatable :: b
            character(:), allocatable :: linkname
            character(:), allocatable :: filename
            type(tclock) :: clock
            
            call clock_start(clock)
            
            info = IO_SUCCESS
            if (present(name)) then
                  linkname = name
                  filename = io_link_read(linkname)
            else
                  b = trim(adjustl(g%prefix)) // str(idx)
                  linkname = IO_SCRATCHDIR // record_linkname(b)
                  filename = IO_SCRATCHDIR // io_link_read(linkname)
            end if
            call io_binary_read_rank1(a, filename)
            g%bytes_read = g%bytes_read + io_size_byte(a)
            g%seconds_read = g%seconds_read + clock_readwall(clock)
      end subroutine io_record_read_rank1


      subroutine io_record_axpy_rank1(g, idx, y, alpha, info)
            !
            ! Perform AXPY operation, where X is read
            ! from a record file:
            ! Y <- ALPHA * X + Y.
            !
            type(trecgroup), intent(inout)         :: g
            integer, intent(in)                    :: idx
            real(F64), dimension(:), intent(inout) :: y
            real(F64), intent(in)                  :: alpha
            integer, intent(out)                   :: info

            character(:), allocatable :: b
            character(:), allocatable :: linkname
            character(:), allocatable :: filename
            type(tclock) :: clock
            
            call clock_start(clock)

            info = IO_SUCCESS
            b = trim(adjustl(g%prefix)) // str(idx)
            linkname = IO_SCRATCHDIR // record_linkname(b)
            filename = IO_SCRATCHDIR // io_link_read(linkname)
            call io_binary_axpy_rank1(y, alpha, filename)

            g%bytes_read = g%bytes_read + io_size_byte(y)
            g%seconds_read = g%seconds_read + clock_readwall(clock)
      end subroutine io_record_axpy_rank1


      subroutine io_record_read_rank2(g, idx, a, info, name)
            type(trecgroup), intent(inout)                      :: g
            integer, intent(in)                                 :: idx
            real(F64), dimension(:, :), contiguous, intent(out) :: a
            integer, intent(out)                                :: info
            character(*), optional, intent(in)             :: name

            integer :: n
            character(:), allocatable :: b
            character(:), allocatable :: linkname
            character(:), allocatable :: filename
            type(tclock) :: clock

            call clock_start(clock)

            info = IO_SUCCESS
            n = size(a)
            if (present(name)) then
                  filename = name
            else
                  b = trim(adjustl(g%prefix)) // str(idx)
                  linkname = IO_SCRATCHDIR // record_linkname(b)
                  filename = IO_SCRATCHDIR // io_link_read(linkname)
            end if
            call io_binary_read_rankn(a, n, filename)

            g%bytes_read = g%bytes_read + io_size_byte(a)
            g%seconds_read = g%seconds_read + clock_readwall(clock)
      end subroutine io_record_read_rank2


      subroutine io_record_read_rankn(g, idx, a, n, info)
            type(trecgroup), intent(inout)       :: g
            integer, intent(in)                  :: idx
            real(F64), dimension(*), intent(out) :: a
            integer, intent(in)                  :: n
            integer, intent(out)                 :: info
            
            call io_record_read_rank1(g, idx, a(1:n), info)
      end subroutine io_record_read_rankn
end module io
