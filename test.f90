program test

    implicit none
    call runtests()

end program test

subroutine runtests()
    print *, ' - Starting Tests -'
    print *, ''
    call testmsum3d()
    call testmmulargmax()
    call testmaxpool()
    call testhardtanh2d()
    call testhardtanh3d()
    call testhardtanh4d()
    call testbatchnorm2d()
    call testbatchnorm3d()
    call testconv2d()
    call testthresholdLayer()
    call testdense()
    call testsumpopcount()
    call testxnor()
    call testmmulbin()
    call testdensebin()
    call testconv2dbin()
!    call testload() !something with this crashed at runtime
!    call testReader()
    print *, ''
    print *, ' - Tests Done -'
    print *, ''
end subroutine runtests