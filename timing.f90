subroutine timing(time, funct)
    !time - time being recorded
    !funct - function which time belongs to, numbered as:
    !   1 - CNVconv
    !   2 - conv2dbin
    !   3 - maxpool2x23d
    !   4 - densebin
    !   5 - thresholdlayer
    !   6 - infer
    !   7 - load images
    !   8 - load weights
    !   9 - elemwise xnor (not used)
    !  10 - msum3d (not used)
    real time
    integer funct

    integer, parameter :: n = 10
    real avtimings(n)
    integer counts(n)

    common /ts/ avtimings, counts

    !Keep track of how many times a function is called/timed
    counts(funct) = counts(funct) + 1
    !Keep track of the average time for each function
    avtimings(funct) = (avtimings(funct) * (counts(funct) -1) + time)/counts(funct)

    !(NOTE - cpu_time has minimum resultion of 10 milliseconds)
end subroutine timing

subroutine timingresults()
    !prints the results of the timings
    integer, parameter :: n = 10
    real avtimings(n)
    integer counts(n)

    common /ts/ avtimings, counts
    print *, ''
    print *, '- Timings -'
    print '(10X, 10A12)', 'CNVconv', 'conv2dbin', 'maxpool',&
                          'densebin', 'thresh Lay', 'infer',&
                          'ld imgs', 'ld wts'!, 'el xnor', 'msum'
    print '(A10, 10ES12.2)', 'Averages: ', avtimings(1:8)
    print '(A10, 10I12)', 'Counts  : ', counts(1:8)
    print *, ''

end subroutine timingresults

subroutine timingstarts(funct)
    !starts the timing for a particular function
    !(NOTE - cpu_time has minimum resultion of 10 milliseconds)
    !funct - id of the function being timed (for CNV these are detailed above)
    integer, parameter :: n = 10
    real timstart(n), timend(n)
    integer funct
    logical started(n)

    common /tm/ timstart, timend, started

    !Show message if timer for this function is already running
    if (started(funct)) then
        print *, 'Must call timingend before timingstart'
    end if

    !Set the state of the timer for this function to ON, and start timer
    started(funct) = .true.
    call cpu_time(timstart(funct))
end subroutine timingstarts

subroutine timingend(funct)
    !stops the timing for a particular function
    !(NOTE - cpu_time has minimum resultion of 10 milliseconds)
    !funct - id of the function being timed (for CNV these are detailed above)
    integer, parameter :: n = 10
    real timstart(n), timend(n)
    integer funct
    logical started(n)

    common /tm/ timstart, timend, started

    !Show message if timer for this function has not been started
    if (.not.started(funct)) then
        print *, 'Must call timingstart before timingend'
    end if

    !Set the state of the timer for this function to OFF, and stop timer
    started(funct) = .false.
    call cpu_time(timend(funct))

    !Save the timing result
    call timing(timend(funct)-timstart(funct), funct)
end subroutine timingend
