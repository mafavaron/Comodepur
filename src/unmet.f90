! Created by  on 2019-05-22.

program chk_wind.f90

    implicit none

    use calendar

    ! Locals
    integer             :: iRetCode
    integer             :: iTimeFrom
    integer             :: iTimeTo
    integer             :: iCurTime
    character(len=256)  :: sInPath
    character(len=256)  :: sInFile
    character(len=256)  :: sOutPrefix
    character(len=256)  :: sOutFile
    character(len=19)   :: sIsoDateTime
    character(len=10)   :: sBuffer
    integer             :: iYear, iMonth, iDay, iHour, iMinute, iSecond

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
    read(sBuffer, iYear, iMonth, iDay)
    call packtime(iTimeFrom, iYear, iMonth, iDay)
    call get_command_argument(3, sBuffer)
    read(sBuffer, iYear, iMonth, iDay)
    call packtime(iTimeTo, iYear, iMonth, iDay)
    iTimeTo = iTimeTo + 24*3600
    call get_command_argument(4, sOutPrefix)
    
    ! Main loop: Iterate over files
    do iCurTime = iTimeFrom, iTimeTo-1, 3600
    
        ! Form current date and time
        call unpacktime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
        write(sInFile, "(a, '/', i4.4, i2.2, i2.2, '.', i2.2, '.csv')") iYear, iMonth, iDay, iHour
        
        ! Gather file contents
        iRetCode = readSoniclibFile(10, ivTimeStamp, rvU, rvV, rvW, rvT)
        if(iRetCode /= 0) cycle
        
    end do
    
contains

    function readSoniclibFile(iLUN, ivTimeStamp, rvU, rvV, rvW, rvT) result(iRetCode)
    end function readSoniclibFile
    
end program chk_wind.f90
