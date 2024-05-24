program T2P_CoLoCor
  implicit none
  
  character(100)              :: orig_file, new_file, name, temp, fmt
  character(100),allocatable  :: hsus(:)
  character(3000)             :: line
  character(:),allocatable    :: iomsg
  integer                     :: i, nclasses, nhsus, ioerr, iline, maxlogs, n, nlogs, log_id, prev_id, nmoves, nmoved
  real*8                      :: zland, dist
  real*8                      :: coords(2), new_coords(2)
  real*8, allocatable         :: values(:), coords_prev(:,:)
  integer, parameter          :: uin=10, uout=11, ulog=9
  
  ! Initialize
  maxlogs     = 10000
  iline       = 1
  nmoved      = 0
  nmoves      = 0
  nlogs       = 0
  prev_id     = 0
  
  ! Start
  write(*,'(a)') '*--- T2P Co-Located Log Corrector (CoLoCor) ---*'
  write(*,'(a)') '      Copyright S.S. Papadopulos & Assoc.'
  write(*,*)
  
  if (COMMAND_ARGUMENT_COUNT() > 3) then
    call GET_COMMAND_ARGUMENT(1, orig_file)
    call GET_COMMAND_ARGUMENT(2, new_file)
    call GET_COMMAND_ARGUMENT(3, temp)
    read(temp, *) nclasses
    call GET_COMMAND_ARGUMENT(4, temp)
    read(temp, *) nhsus
  else
    write(*,*) "ERROR - Must pass orig_file new_file nclasses nhsus"
  end if
  
  ! Open files
  open(ulog, file="CoLoCor_Moved.log", action='WRITE', STATUS='REPLACE', iostat=ioerr, iomsg=iomsg)
  call iomsg_handler(ioerr, iomsg)
  open(uin, file=orig_file, action='READ', STATUS='OLD', iostat=ioerr, iomsg=iomsg)
  call iomsg_handler(ioerr, iomsg)
  open(uout, file=new_file, action='WRITE', STATUS='REPLACE', iostat=ioerr, iomsg=iomsg)
  call iomsg_handler(ioerr, iomsg)
  
  ! Write to log
  write(ulog,'(a)') "       ___        __         ___           "
  write(ulog,'(a)') "      / __\___   / /  ___   / __\___  _ __ "
  write(ulog,'(a)') "     / /  / _ \ / /  / _ \ / /  / _ \| '__|"
  write(ulog,'(a)') "    / /__| (_) / /__| (_) / /__| (_) | |   "
  write(ulog,'(a)') "    \____/\___/\____/\___/\____/\___/|_|   "
  write(ulog,'(a)') "                                           "
  write(ulog,'(a)') '*--- T2P Co-Located Log Corrector (CoLoCor) ---*'
  write(ulog,'(a)') '      Copyright S.S. Papadopulos & Assoc.'
  write(ulog,*)
  
  ! Setup overwrite line
  open(6, carriagecontrol='fortran')
  
  ! Allocate
  allocate(values(nclasses+1))  ! Storing depths here, too
  allocate(hsus(nhsus))
  allocate(coords_prev(2,maxlogs))
  values = 0.0
  hsus   = ''
  write(fmt,'(a,2(i2,a))') '(a20,2i6,2f16.6,2f10.4,',nclasses,'(1x,g12.6),',nhsus,'(2x,a))'
  
  ! Copy over file header
  read(uin,   '(a)') line
  write(uout, '(a)') trim(line)
  
  ! Start read-writing
  do while (.true.)
    ! Read in whole log (all intervals)
    read(uin,*,iostat=ioerr) name, log_id, n, coords(1:2), zland, values(1:nclasses+1), hsus(1:nhsus)
    if (ioerr /= 0) then
      exit  ! Exit on read error or end of file
    endif
    
    ! Report line
    write(6, '("+",a,2(i7,a))') ' - Status: Line', iline, ',  Log # ',log_id
    
    ! Is this a new log?
    if (prev_id /= log_id) then
      ! Check the coordinates
      new_coords = coords
      call colocate_corrector(new_coords, coords_prev, nlogs, nmoves, dist)
      ! If moves
      if (nmoves > 0) then
        write(ulog,'(2x,a,i4,3a,2f16.6,a,2f16.6,a,f6.3)') 'Moved Well: ',log_id, ' (',trim(name),') from ', coords(1:2), ' to ', new_coords(1:2), ' - Distance: ', dist
        nmoved = nmoved + 1
      end if
      ! Add to existing coordinates
      nlogs = nlogs + 1
      if (nlogs > maxlogs) then
        call resize_coords_prev(coords_prev, maxlogs*2)
        maxlogs = size(coords_prev, 2)
      end if
      coords_prev(1:2, nlogs) = new_coords
      prev_id = log_id
    end if
    
    ! Write
    write(uout,trim(fmt)) trim(name), log_id, n, new_coords(1:2), zland, values(1:nclasses+1), (trim(hsus(i)), i=1,nhsus)
    ! Increment
    iline = iline + 1
    
  end do
  
  close(uin)
  close(uout)
  write(*,'(/,a)') 'EOF reached - Done!'
  write(*,'(a,i8)') 'Total Logs moved: ', nmoved
  write(*,'(/,a)') 'Review CoLoCor_Moved.log for moved logs and compare'
  write(*,'(a)')   'in/out files to ensure program copied logs correctly.'
  
  write(ulog,'(/,a)') 'EOF reached - Done!'
  write(ulog,'(a,i8)') 'Total Logs moved: ', nmoved
  write(ulog,'(/,a)') 'Review CoLoCor_Moved.log for moved logs and compare'
  write(ulog,'(a)')   'in/out files to ensure program copied logs correctly.'
  
contains

!-------------------------------------------------------------------------------------------------!

subroutine iomsg_handler(ioerr, iomsg)
  implicit none
  integer,intent(in)                     :: ioerr
  character(*),intent(in)                :: iomsg
    
  if (ioerr /= 0) then
    write(*,'(a)') trim(iomsg)
    error stop
  end if
    
end subroutine iomsg_handler

!-------------------------------------------------------------------------------------------------!

subroutine colocate_corrector(new_coords, coords, ncoords, nmoves, dist)
  implicit none
! Checks for duplicate X,Y values and moves them away from each other.
!
! Added by Leland Scantlebury July 2019
  integer, intent(in)     :: ncoords
  integer, intent(inout)  :: nmoves
  real*8, intent(inout)   :: new_coords(2), coords(2,ncoords), dist
  integer                 :: i, j, k
  real*8, parameter       :: move=0.01

  ! Setup
  nmoves = 0
  dist   = 0.0

  ! Outer Loop over coordinates
  do i=1, ncoords
    ! Check for x-y match
    if (new_coords(1) == coords(1,i).and.new_coords(2) == coords(2,i)) then
      ! Move
      nmoves = nmoves + 1
      new_coords(1) = new_coords(1) + move * nmoves
      new_coords(2) = new_coords(2) + move * nmoves
    end if
  end do
  
  dist = sqrt(2*(nmoves*move)**2)

end subroutine colocate_corrector

!-------------------------------------------------------------------------------------------------!

subroutine resize_coords_prev(coords_prev, new_size)
    real*8, allocatable, intent(inout) :: coords_prev(:,:)
    integer, intent(in) :: new_size
    real*8, allocatable :: temp(:,:)

    ! Allocate a temporary array with the new size
    allocate(temp(2, new_size))
    temp = 0.0
    temp(:, 1:size(coords_prev, 2)) = coords_prev  ! Copy existing data

    ! Reallocate coords_prev with the new size
    call move_alloc(temp, coords_prev)
    
end subroutine resize_coords_prev

!-------------------------------------------------------------------------------------------------!
end program T2P_CoLoCor