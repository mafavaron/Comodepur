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
    integer, dimension(:), allocatable  :: ivBlockTime
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
    integer, dimension(:), allocatable  :: ivOutStamp
    real, dimension(:), allocatable     :: rvOutU
    real, dimension(:), allocatable     :: rvOutV
    real, dimension(:), allocatable     :: rvOutW
    real, dimension(:), allocatable     :: rvOutT
    real, dimension(:,:,:), allocatable :: raOutCovWind
    real, dimension(:), allocatable     :: rvOutTT
    real, dimension(:,:), allocatable   :: rmOutCovWindTemp

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
        iRetCode = readSoniclibFile(10, sInFile, ivTimeStamp, rvU, rvV, rvW, rvT)
        if(iRetCode /= 0) cycle
        
        ! Aggregate data on 5 minutes basis
        iAveraging = 300
        iRetCode = aggregate( &
            ivTimeStamp, rvU, rvV, rvW, rvT, iAveraging, &
            ivOutStamp, &
            rvOutU, rvOutV, rvOutW, rvOutT, &
            raOutCovWind, rvOutTT, rmOutCovWindTemp &
        )
        if(iRetCode /= 0) cycle
        
        ! Compute base time stamp for current file, and use it to shift time stamps
        ! for sub-hours averages
        call packtime(iBaseTime, iYear, iMonth, iDay, iHour, 0, 0)
        ivOutStamp = ivOutStamp + iBaseTime
        
        
        print *, "File ", trim(sInFile), " processed"
        
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
    
    
    function aggregate( &
        ivTimeStamp, &
        rvU, rvV, rvW, rvT, &
        iAveraging, &
        ivOutStamp, &
        rvOutU, rvOutV, rvOutW, rvOutT, &
        raOutCovWind, rvOutTT, rmOutCovWindTemp &
    ) result(iRetCode)
    
        ! Routine arguments
        integer, dimension(:), allocatable, intent(in)      :: ivTimeStamp
        real, dimension(:), allocatable, intent(in)         :: rvU
        real, dimension(:), allocatable, intent(in)         :: rvV
        real, dimension(:), allocatable, intent(in)         :: rvW
        real, dimension(:), allocatable, intent(in)         :: rvT
        integer, intent(in)                                 :: iAveraging
        integer, dimension(:), allocatable, intent(out)     :: ivOutStamp
        real, dimension(:), allocatable, intent(out)        :: rvOutU
        real, dimension(:), allocatable, intent(out)        :: rvOutV
        real, dimension(:), allocatable, intent(out)        :: rvOutW
        real, dimension(:), allocatable, intent(out)        :: rvOutT
        real, dimension(:,:,:), allocatable, intent(out)    :: raOutCovWind
        real, dimension(:), allocatable, intent(out)        :: rvOutTT
        real, dimension(:,:), allocatable, intent(out)      :: rmOutCovWindTemp
        integer                                             :: iRetCode
        
        ! Locals
        integer                             :: iNumBlocks
        integer                             :: i
        integer                             :: j
        integer, dimension(:), allocatable  :: ivAccIndex
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Define the block number, and use it to reserve workspace
        iNumBlocks = 3600 / iAveraging
        call reallocate_int(ivNumData, iNumBlocks)
        call reallocate_int(ivBlockTime, iNumBlocks)
        call reallocate(rvSumU, iNumBlocks)
        call reallocate(rvSumV, iNumBlocks)
        call reallocate(rvSumW, iNumBlocks)
        call reallocate(rvSumT, iNumBlocks)
        call reallocate(rvSumUU, iNumBlocks)
        call reallocate(rvSumVV, iNumBlocks)
        call reallocate(rvSumWW, iNumBlocks)
        call reallocate(rvSumTT, iNumBlocks)
        call reallocate(rvSumUV, iNumBlocks)
        call reallocate(rvSumUW, iNumBlocks)
        call reallocate(rvSumVW, iNumBlocks)
        call reallocate(rvSumUT, iNumBlocks)
        call reallocate(rvSumVT, iNumBlocks)
        call reallocate(rvSumWT, iNumBlocks)
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
            rvSumTT(j)   = rvSumTT(j) + rvT(i)**2
            rvSumUV(j)   = rvSumUV(j) + rvU(i)*rvV(i)
            rvSumUW(j)   = rvSumUW(j) + rvU(i)*rvW(i)
            rvSumVW(j)   = rvSumVW(j) + rvV(i)*rvW(i)
            rvSumUT(j)   = rvSumUT(j) + rvU(i)*rvT(i)
            rvSumVT(j)   = rvSumVT(j) + rvV(i)*rvT(i)
            rvSumWT(j)   = rvSumWT(j) + rvW(i)*rvT(i)
        end do
        
        ! Render to means and (co)variances
        call reallocate_int(ivOutStamp, iNumBlocks)
        call reallocate(rvOutU, iNumBlocks)
        call reallocate(rvOutV, iNumBlocks)
        call reallocate(rvOutW, iNumBlocks)
        call reallocate(rvOutT, iNumBlocks)
        call reallocate(rvOutTT, iNumBlocks)
        if(allocated(raOutCovWind)) deallocate(raOutCovWind)
        allocate(raOutCovWind(iNumBlocks, 3, 3))
        if(allocated(rmOutCovWindTemp)) deallocate(rmOutCovWindTemp)
        allocate(rmOutCovWindTemp(iNumBlocks, 3))
        do i = 1, iNumBlocks
            if(ivNumData(i) > 0) then
                rvOutU(i) = rvSumU(i) / ivNumData(i)
                rvOutV(i) = rvSumV(i) / ivNumData(i)
                rvOutW(i) = rvSumW(i) / ivNumData(i)
                rvOutT(i) = rvSumT(i) / ivNumData(i)
                raOutCovWind(i,1,1) = rvSumUU(i) / ivNumData(i) - rvOutU(i)**2
                raOutCovWind(i,2,2) = rvSumVV(i) / ivNumData(i) - rvOutV(i)**2
                raOutCovWind(i,3,3) = rvSumWW(i) / ivNumData(i) - rvOutW(i)**2
                rvOutTT(i) = rvSumTT(i) / ivNumData(i) - rvOutT(i)**2
                raOutCovWind(i,1,2) = rvSumUV(i) / ivNumData(i) - rvOutU(i)*rvOutV(i)
                raOutCovWind(i,1,3) = rvSumUW(i) / ivNumData(i) - rvOutU(i)*rvOutW(i)
                raOutCovWind(i,2,3) = rvSumVW(i) / ivNumData(i) - rvOutV(i)*rvOutW(i)
                rmOutCovWindTemp(i,1) = rvSumUT(i) / ivNumData(i) - rvOutU(i)*rvOutT(i)
                rmOutCovWindTemp(i,2) = rvSumVT(i) / ivNumData(i) - rvOutV(i)*rvOutT(i)
                rmOutCovWindTemp(i,3) = rvSumWT(i) / ivNumData(i) - rvOutW(i)*rvOutT(i)
            else
                rvOutU(i)  = -9999.9
                rvOutV(i)  = -9999.9
                rvOutW(i)  = -9999.9
                rvOutT(i)  = -9999.9
                raOutCovWind(i,1,1) = -9999.9
                raOutCovWind(i,2,2) = -9999.9
                raOutCovWind(i,3,3) = -9999.9
                rvOutTT(i) = -9999.9
                raOutCovWind(i,1,2) = -9999.9
                raOutCovWind(i,1,3) = -9999.9
                raOutCovWind(i,2,3) = -9999.9
                rmOutCovWindTemp(i,1) = -9999.9
                rmOutCovWindTemp(i,2) = -9999.9
                rmOutCovWindTemp(i,3) = -9999.9
            end if
            raOutCovWind(i,2,1) = raOutCovWind(i,1,2)
            raOutCovWind(i,3,1) = raOutCovWind(i,1,3)
            raOutCovWind(i,3,2) = raOutCovWind(i,2,3)
        end do
        
        ! Generation of second time stamps
        ivOutStamp = [(i*iAveraging, i = 0, iNumBlocks-1)]
        
        ! Leave
        deallocate(ivAccIndex)
        
    end function aggregate
    
    
    subroutine reallocate(x, iSize)
    
        ! Routine arguments
        real, dimension(:), allocatable, intent(inout)  :: x
        integer, intent(in)                             :: iSize
        
        ! Locals
        ! --none--
        
        ! Re-allocate vector
        if(allocated(x)) deallocate(x)
        allocate(x(iSize))
        
    end subroutine reallocate
    
    
    subroutine reallocate_int(x, iSize)
    
        ! Routine arguments
        integer, dimension(:), allocatable, intent(inout)  :: x
        integer, intent(in)                                :: iSize
        
        ! Locals
        ! --none--
        
        ! Re-allocate vector
        if(allocated(x)) deallocate(x)
        allocate(x(iSize))
        
    end subroutine reallocate_int
    
end program urmet
