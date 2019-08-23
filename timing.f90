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
    !   9 - elemwise xnor
    !  10 - msum3d
    real time
    integer funct

    integer, parameter :: n = 10
    real avtimings(n)
    integer counts(n)

    common /ts/ avtimings, counts

    counts(funct) = counts(funct) + 1
    avtimings(funct) = (avtimings(funct) * (counts(funct) -1) + time)/counts(funct)


end subroutine timing

subroutine timingresults()
    integer, parameter :: n = 10
    real avtimings(n)
    integer counts(n)

    common /ts/ avtimings, counts
    print *, ''
    print *, '- Timings -'
    print '(10X, 10A12)', 'CNVconv', 'conv2dbin', 'maxpool',&
                          'densebin', 'thresh Lay', 'infer',&
                          'ld imgs', 'ld wts', 'el xnor', 'msum'
    print '(A10, 10ES12.2)', 'Averages: ', avtimings
    print '(A10, 10I12)', 'Counts  : ', counts
    print *, ''

end subroutine timingresults

subroutine timingstarts(funct)
    integer, parameter :: n = 10
    real timstart(n), timend(n)
    integer funct
    logical started(n)

    common /tm/ timstart, timend, started

    if (started(funct)) then
        print *, 'Must call timingend before timingstart'
    end if

    started(funct) = .true.
    call cpu_time(timstart(funct))
end subroutine timingstarts

subroutine timingend(funct)
    integer, parameter :: n = 10
    real timstart(n), timend(n)
    integer funct
    logical started(n)

    common /tm/ timstart, timend, started

    if (.not.started(funct)) then
        print *, 'Must call timingstart before timingend'
    end if

    started(funct) = .false.
    call cpu_time(timend(funct))
    call timing(timend(funct)-timstart(funct), funct)
end subroutine timingend
