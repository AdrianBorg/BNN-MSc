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
    real time
    integer funct

    integer, parameter :: n = 8
    real avtimings(n)
    integer counts(n)

    common /ts/ avtimings, counts

    counts(funct) = counts(funct) + 1
    avtimings(funct) = (avtimings(funct) * (counts(funct) -1) + time)/counts(funct)


end subroutine timing

subroutine timingresults()
    integer, parameter :: n = 8
    real avtimings(n)
    integer counts(n)

    common /ts/ avtimings, counts

    print *, 'Averages: ', avtimings
    print *, 'Counts  : ', counts

end subroutine timingresults

subroutine timingstarts(funct)
!    real timstart, timend
!    integer funct, f0
!    logical started
!
!    common /tm/ timstart, timend, started, f0
!
!    if (started) then
!        print *, 'Must call timingend before timingstart'
!    end if
!    f0 = funct
!
!    started = .true.
!    call cpu_time(timstart)
end subroutine timingstarts

subroutine timingend(funct)
!    real timstart, timend
!    integer funct, f0
!    logical started
!
!    common /tm/ timstart, timend, started, f0
!    if (.not.started) then
!        print *, 'Must call timingstart before timingend'
!    else if (funct /= f0) then
!        print *, 'Function timing mismatch'
!    end if
!
!    started = .false.
!    call cpu_time(timend)
!    call timing(timstart-timend, funct)
end subroutine timingend
