program ConvertToSonicLib

    use calendar

    implicit none
    
    ! Locals
    integer             :: iRetCode
    character(len=256)  :: sPathFrom
    character(len=256)  :: sPathTo
    character(len=256)  :: sFileName
    character(len=256)  :: sOutFile
    character(len=20)   :: sFirstDate
    character(len=20)   :: sLastDate
    character(len=16)   :: sBuffer
    integer             :: iNumDays
    integer             :: iFirstDate
    integer             :: iLastDate
    integer             :: iCurDate
    integer             :: iYear
    integer             :: iMonth
    integer             :: iDay
    integer             :: iHour
    integer             :: iMinute
    integer             :: iSecond
    integer             :: iHold
    integer             :: i
    integer, dimension(:), allocatable  :: ivTimeStamp
    real, dimension(:), allocatable     :: rvU
    real, dimension(:), allocatable     :: rvV
    real, dimension(:), allocatable     :: rvW
    real, dimension(:), allocatable     :: rvT
    
    ! Get input parameters
    if(command_argument_count() /= 4) then
        print *, "convert - Utility to convert USA1 data to SonicLib form"
        print *
        print *, "Usage:"
        print *
        print *, "   ./convert <PathFrom> <PathTo> <StartDate> <EndDate>"
        print *
        print *, "Copyright 2019 by Mauri Favaron"
        print *, "                  This is open source software, covered by the MIT license"
        print *
        stop
    end if
    call get_command_argument(1, sPathFrom)
    call get_command_argument(2, sPathTo)
    call get_command_argument(3, sFirstDate)
    call get_command_argument(4, sLastDate)
    
    ! Get dates, and convert them to epoch form
    read(sFirstDate, "(i4,1x,i2,1x,i2)", iostat=iRetCode) iYear, iMonth, iDay
    if(iRetCode /= 0) then
        print *, "Error: Invalid start date"
        stop
    end if
    call PackTime(iFirstDate, iYear, iMonth, iDay)
    read(sLastDate, "(i4,1x,i2,1x,i2)", iostat=iRetCode) iYear, iMonth, iDay
    if(iRetCode /= 0) then
        print *, "Error: Invalid end date"
        stop
    end if
    call PackTime(iLastDate, iYear, iMonth, iDay)
    if(iLastDate < iFirstDate) then
        iHold = iLastDate
        iLastDate = iFirstDate
        iFirstDate = iHold
    end if
    iLastDate = iLastDate + 86400   ! Increment day, so tha last date can be included
    
    ! Main loop: iterate over expected files
    do iCurDate = iFirstDate, iLastDate-1, 3600
    
        ! Generate file names
        call UnpackTime(iCurDate, iYear, iMonth, iDay, iHour, iMinute, iSecond)
        write(sFileName, "(a, '/', i4.4, i2.2, '\', i4.4, i2.2, i2.2, '.', i2.2)") &
            trim(sPathFrom), iYear, iMonth, iYear, iMonth, iDay, iHour
        write(sOutFile, "(a, '/', i4.4, i2.2, i2.2, '.', i2.2, '.csv')") trim(sPathTo), iYear, iMonth, iDay, iHour
        
        ! Read file
        iRetCode = getFile(10, sFileName, ivTimeStamp, rvU, rvV, rvW, rvT)
        if(iRetCode /= 0) cycle
        
        ! Write file in SonicLib form
        open(10, file=sOutFile, status='unknown', action='write')
        write(10, "('time.stamp, u, v, w, t')")
        do i = 1, size(ivTimeStamp)
            write(10, "(i4, 4(',',f8.2))") ivTimeStamp(i), rvU(i), rvV(i), rvW(i), rvT(i)
        end do
        close(10)
        print *, "File: ", trim(sFileName)
        
    end do
    
contains

    function getFile(iLUN, sFileName, ivTimeStamp, rvU, rvV, rvW, rvT) result(iRetCode)
    
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
        integer             :: iNumLines
        integer             :: i
        integer             :: iErrCode
        character(len=64)   :: sBuffer
        integer             :: iU, iV, iW, iT
        real                :: rDeltaT
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Count lines in file
        open(iLUN, file=sFileName, action='read', status='old', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        iNumLines = 0
        do
            read(iLUN, "(a)", iostat = iErrCode) sBuffer
            if(iErrCode /= 0) exit
            iNumLines = iNumLines + 1
        end do
        if(iNumLines <= 0) then
            iRetCode = 2
            close(iLUN)
            return
        end if
        
        ! Reserve workspace and populate it
        if(allocated(ivTimeStamp)) deallocate(ivTimeStamp)
        if(allocated(rvU))         deallocate(rvU)
        if(allocated(rvV))         deallocate(rvV)
        if(allocated(rvW))         deallocate(rvW)
        if(allocated(rvT))         deallocate(rvT)
        allocate(ivTimeStamp(iNumLines))
        allocate(rvU(iNumLines))
        allocate(rvV(iNumLines))
        allocate(rvW(iNumLines))
        allocate(rvT(iNumLines))
        rewind(iLUN)
        do i = 1, iNumLines
            read(iLUN, "(a)") sBuffer
            if(sBuffer(6:12) /= '       ') then
                read(sBuffer, "(6x,i5,3(5x,i5))", iostat=iErrCode) iV, iU, iW, iT
                if(iErrCode == 0) then
                    rvU(i) = iU / 100.0
                    rvV(i) = iV / 100.0
                    rvW(i) = iW / 100.0
                    rvT(i) = iT / 100.0
                else
                    rvU(i) = -9999.9
                    rvV(i) = -9999.9
                    rvW(i) = -9999.9
                    rvT(i) = -9999.9
                end if
            else
                rvU(i) = -9999.9
                rvV(i) = -9999.9
                rvW(i) = -9999.9
                rvT(i) = -9999.9
            end if
        end do
        close(iLUN)
        
        ! Generate and assign time stamp, assuming a smooth time progression
        rDeltaT = 3600.0 / iNumLines
        ivTimeStamp = [(floor((i-1)*rDeltaT), i = 1, iNumLines)]
        
    end function getFile

end program ConvertToSonicLib

