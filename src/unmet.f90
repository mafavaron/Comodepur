! Created by  on 2019-05-22.

program urmet

    use calendar

    implicit none

    ! Locals
    integer             :: iRetCode
    integer             :: iTimeFrom
    integer             :: iTimeTo
    integer             :: iCurTime
    integer             :: iBaseTime
    character(len=256)  :: sInPath
    character(len=256)  :: sInFile
    character(len=256)  :: sOutPrefix
    character(len=256)  :: sOutFile
    character(len=19)   :: sIsoDateTime
    character(len=10)   :: sBuffer
    integer             :: iYear, iMonth, iDay, iHour, iMinute, iSecond
    integer, dimension(:), allocatable  :: ivTimeStamp
    real, dimension(:), allocatable     :: rvU, rvV, rvW, rvT
    integer, dimension(:), allocatable  :: ivCounterIndex
    integer, dimension(:), allocatable  :: ivNumData
    real, dimension(:), allocatable     :: rvSumU
    real, dimension(:), allocatable     :: rvSumV
    real, dimension(:), allocatable     :: rvSumW
    real, dimension(:), allocatable     :: rvSumT

    ! Get parameters
    if(command_argument_count() /= 4) then
        print *, "urmet - Urban (micro)Meteorological Checker"
        print *
        print *, "Usage:"
        print *
        print *, "  ./urmet <Path> <DateFrom> <DateTo> <ResultPrefix>"
        print *
        print *, "Copyright by Mauri Favaron"
        print *, "             This is open-source software, covered by the MIT license"
        print *
        stop
    end if
    call get_command_argument(1, sInPath)
    call get_command_argument(2, sBuffer)
    read(sBuffer, "(i4,1x,i2,1x,i2)") iYear, iMonth, iDay
    call packtime(iTimeFrom, iYear, iMonth, iDay)
    call get_command_argument(3, sBuffer)
    read(sBuffer, "(i4,1x,i2,1x,i2)") iYear, iMonth, iDay
    call packtime(iTimeTo, iYear, iMonth, iDay)
    iTimeTo = iTimeTo + 24*3600
    call get_command_argument(4, sOutPrefix)
    
    ! Main loop: Iterate over files
    do iCurTime = iTimeFrom, iTimeTo-1, 3600
    
        ! Form current date and time
        call unpacktime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
        write(sInFile, "(a, '/', i4.4, i2.2, i2.2, '.', i2.2, '.csv')") iYear, iMonth, iDay, iHour
        
        ! Gather file contents
        iRetCode = readSoniclibFile(10, sInFile, ivTimeStamp, rvU, rvV, rvW, rvT)
        if(iRetCode /= 0) cycle
        print *, "File ", trim(sInFile), " read"
        
        ! Compute base time stamp for current file, and use it to shift time stamps
        ! for sub-hours averages
        call packtime(iBaseTime, iYear, iMonth, iDay, iHour, 0, 0)
        
    end do
    
contains

    function readSoniclibFile(iLUN, sFileName, ivTimeStamp, rvU, rvV, rvW, rvT) result(iRetCode)
    
        ! Routine arguments
        integer, intent(in)                             :: iLUN
        character(len=*), intent(in)                    :: sFileName
        integer, dimension(:), allocatable, intent(out) :: ivTimeStamp
        real, dimension(:), allocatable, intent(out)    :: rvU
        real, dimension(:), allocatable, intent(out)    :: rvV
        real, dimension(:), allocatable, intent(out)    :: rvW
        real, dimension(:), allocatable, intent(out)    :: rvT
        integer                                         :: iRetCode
        
        ! Locals
        integer             :: iErrCode
        integer             :: iNumData
        integer             :: iData
        character(len=256)  :: sBuffer
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! First step: count lines in file, and use this information to reserve workspace
        open(iLUN, file=sFileName, status='old', action='read', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        iNumData = -1   ! Starting from -1 in order to out-count the header
        do
            read(iLUN, "(a)", iostat=iErrCode) sBuffer
            if(iErrCode /= 0) exit
            iNumData = iNumData + 1
        end do
        if(iNumData <= 0) then
            iRetCode = 2
            close(iLUN)
            return
        end if
        if(allocated(ivTimeStamp)) deallocate(ivTimeStamp)
        if(allocated(rvU))         deallocate(rvU)
        if(allocated(rvV))         deallocate(rvV)
        if(allocated(rvW))         deallocate(rvW)
        if(allocated(rvT))         deallocate(rvT)
        allocate(ivTimeStamp(iNumData))
        allocate(rvU(iNumData))
        allocate(rvV(iNumData))
        allocate(rvW(iNumData))
        allocate(rvT(iNumData))
        
        ! Second pass: read actual data
        rewind(iLUN)
        read(iLUN, "(a)") sBuffer   ! Skip header
        do iData = 1, iNumData
            read(iLUN, *, iostat=iErrCode) ivTimeStamp(iData), rvU(iData), rvV(iData), rvW(iData), rvT(iData)
            if(iErrCode /= 0) then
                iErrCode = 3
                close(iLUN)
                return
            end if
        end do
        
        ! Leave
        close(iLUN)
        
    end function readSoniclibFile
    
end program urmet
