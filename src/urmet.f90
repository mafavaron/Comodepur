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
    integer             :: iAveraging
    integer             :: iHold
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
    real, dimension(:), allocatable     :: rvSumUU
    real, dimension(:), allocatable     :: rvSumVV
    real, dimension(:), allocatable     :: rvSumWW
    real, dimension(:), allocatable     :: rvSumTT
    real, dimension(:), allocatable     :: rvSumUV
    real, dimension(:), allocatable     :: rvSumUW
    real, dimension(:), allocatable     :: rvSumVW
    real, dimension(:), allocatable     :: rvSumUT
    real, dimension(:), allocatable     :: rvSumVT
    real, dimension(:), allocatable     :: rvSumWT

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
    if(iTimeFrom > iTimeTo) then
        iHold = iTimeFrom
        iTimeFrom = iTimeTo
        iTimeTo = iHold
    end if
    call get_command_argument(4, sOutPrefix)
    
    ! Main loop: Iterate over files
    do iCurTime = iTimeFrom, iTimeTo-1, 3600
    
        ! Form current date and time
        call unpacktime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
        write(sInFile, "(a, '/', i4.4, i2.2, i2.2, '.', i2.2, '.csv')") trim(sInPath), iYear, iMonth, iDay, iHour
        
        ! Gather file contents
        print *, "File ", trim(sInFile), " read"
        iRetCode = readSoniclibFile(10, sInFile, ivTimeStamp, rvU, rvV, rvW, rvT)
        if(iRetCode /= 0) cycle
        
        ! Aggregate data on 5 minutes basis
        iAveraging = 300
        iRetCode = aggregate(ivTimeStamp, rvU, rvV, rvW, rvT, iAveraging)
        
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
            read(iLUN, "(a)", iostat=iErrCode) sBuffer
            if(iErrCode /= 0) then
                iErrCode = 3
                close(iLUN)
                return
            end if
            read(sBuffer, *, iostat=iErrCode) ivTimeStamp(iData), rvU(iData), rvV(iData), rvW(iData), rvT(iData)
            if(iErrCode /= 0) then
                iErrCode = 4
                close(iLUN)
                return
            end if
        end do
        
        ! Leave
        close(iLUN)
        
    end function readSoniclibFile
    
    
    function aggregate(ivTimeStamp, rvU, rvV, rvW, rvT, iAveraging) result(iRetCode)
    
        ! Routine arguments
        integer, dimension(:), intent(in)   :: ivTimeStamp
        real, dimension(:), intent(in)      :: rvU
        real, dimension(:), intent(in)      :: rvV
        real, dimension(:), intent(in)      :: rvW
        real, dimension(:), intent(in)      :: rvT
        integer, intent(in)                 :: iAveraging
        integer                             :: iRetCode
        
        ! Locals
        integer                             :: iNumBlocks
        integer                             :: i
        integer                             :: j
        integer, dimension(:), allocatable  :: ivAccIndex
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Define the block number, and use it to reserve workspace
        iNumBlocks = 3600 / iAveraging
        allocate(ivAccIndex(size(ivTimeStamp)))
        
        ! Generate the aggregation index
        ivAccIndex = ivTimeStamp / iAveraging + 1
        
        ! Generate sums
        do i = 1, iNumBlocks
            j            = ivAccIndex(i)
            ivNumData(j) = ivNumData(j) + 1
            rvSumU(j)    = rvSumU(j)  + rvU(i)
            rvSumV(j)    = rvSumV(j)  + rvV(i)
            rvSumW(j)    = rvSumW(j)  + rvW(i)
            rvSumT(j)    = rvSumT(j)  + rvT(i)
            rvSumUU(j)   = rvSumUU(j) + rvU(i)**2
            rvSumVV(j)   = rvSumVV(j) + rvV(i)**2
            rvSumWW(j)   = rvSumWW(j) + rvW(i)**2
            rvSumUV(j)   = rvSumUV(j) + rvU(i)*rvV(i)
            rvSumUW(j)   = rvSumUW(j) + rvU(i)*rvW(i)
            rvSumVW(j)   = rvSumVW(j) + rvV(i)*rvW(i)
            rvSumUT(j)   = rvSumUT(j) + rvU(i)*rvT(i)
            rvSumVT(j)   = rvSumVT(j) + rvV(i)*rvT(i)
            rvSumWT(j)   = rvSumWT(j) + rvW(i)*rvT(i)
        end do
        
        ! Leave
        deallocate(ivAccIndex)
        
        
    end function aggregate
    
end program urmet
