!program testfunctions
!    implicit none
!
!    !picture to test
!!    integer, parameter :: pic=5030
!
!    !testing batch variables
!    integer, parameter :: npics = 10000, dims = 32, colours = 3
!    integer imgs(npics, colours, dims, dims)
!    integer(1) labels(npics)
!
!    !weights variables
!    integer, parameter :: f = 3, chOut(0:9) = [3, 64, 64, 128, 128, 256, 256, 512, 512, 10]
!    integer :: w1(chOut(1), chOut(0), f ,f), w2(chOut(2), chOut(1), f ,f)
!    integer :: w3(chOut(3), chOut(2), f ,f), w4(chOut(4), chOut(3), f ,f)
!    integer :: w5(chOut(5), chOut(4), f ,f), w6(chOut(6), chOut(5), f ,f)
!    integer :: w7(chOut(6), chOut(7)), w8(chOut(7), chOut(8)), w9(chOut(8), chOut(9))
!    integer :: t1(chOut(1)), t2(chOut(2)), t3(chOut(3)), t4(chOut(4)), t5(chOut(5))
!    integer :: t6(chOut(6)), t7(chOut(7)), t8(chOut(8))
!
!!    !process variables
!!    integer, parameter :: odim(6) = [30, 28, 12, 10, 3, 1], pdim(2) = [14, 5]
!!    integer o1(chOut(1), odim(1), odim(1)), o2(chOut(2), odim(2), odim(2))
!!    integer o3(chOut(3), odim(3), odim(3)), o4(chOut(4), odim(4), odim(4))
!!    integer o5(chOut(5), odim(5), odim(5)), o6(chOut(6), odim(6), odim(6))
!!    integer o7(chOut(7)), o8(chOut(8)), o9(chOut(9))
!!    integer p1(chOut(2), pdim(1), pdim(1)), p2(chOut(4), pdim(2), pdim(2))
!
!    integer result, stats(0:9), i, correct
!!    integer, parameter :: dd = 1, cc = 0
!!
!!    print *, dd .EQV. cc, dd .EQV. dd, cc .EQV. cc
!    !put this in conv and dense
!
!    call testmsum3d()
!    call testmmulargmax()
!    call testmaxpool()
!    call testhardtanh2d()
!    call testhardtanh3d()
!    call testhardtanh4d()
!    call testbatchnorm2d()
!    call testbatchnorm3d()
!    call testconv2d()
!    call testthresholdLayer()
!    call testdense()
!    call testsumpopcount()
!    call testxnor()
!    call testmmulbin()
!    call testdensebin()
!    call testconv2dbin()
!!    call testload() !something with this crashed at runtime
!!    call testReader()
!
!    !begin program
!    print *, '----------------'
!    print *, ''
!    print *, '- Tests done! Starting program -'
!    print *, ''
!    call cifarFileReader1(imgs, labels)
!    print *, '- Images   : loaded'
!    call loadData(chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
!    print *, '- Weights  : loaded'
!
!    do i = 0, 9
!        stats(i) = 0
!    end do
!
!    correct = 0
!
!    do i = 1, npics
!        call infer(result, imgs(i, :, :, :), dims, f, chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
!        if (result == labels(i)) then
!            stats(result) = stats(result) + 1
!            correct = correct + 1
!            print *, stats
!        end if
!    end do
!
!    call timingresults()
!
!    print *, 'stats: ', stats
!    print *, 'correct: ', correct
!
!
!!
!!    call loadData(chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
!!    print *, '- Weights  : loaded'
!!
!!    call cconvT(o1, imgs(pic, :, :, :), chOut(1), chOut(0), dims, w1, t1, .false.)
!!    print *, '- 1st Layer: done'
!!
!!    call cconvT(o2, o1, chOut(2), chOut(1), odim(1), w2, t2, .true.)
!!    print *, '- 2nd Layer: done'
!!
!!    call maxpool2x23d(p1, o2, chOut(2), odim(2))
!!    print *, '- maxpool  : done'
!!
!!    call cconvT(o3, p1, chOut(3), chOut(2), pdim(1), w3, t3, .true.)
!!    print *, '- 3rd Layer: done'
!!
!!    call cconvT(o4, o3, chOut(4), chOut(3), odim(3), w4, t4, .true.)
!!    print *, '- 4th Layer: done'
!!
!!    call maxpool2x23d(p2, o4, chOut(4), odim(4))
!!    print *, '- maxpool  : done'
!!
!!    call cconvT(o5, p2, chOut(5), chOut(4), pdim(2), w5, t5, .true.)
!!    print *, '- 5th Layer: done'
!!
!!    call cconvT(o6, o5, chOut(6), chOut(5), odim(5), w6, t6, .true.)
!!    print *, '- 6th Layer: done'
!!
!!    call cdensebinT(o7, o6(:, 1, 1), chOut(7), chOut(6), w7, t7)
!!    print *, '- 7th Layer: done'
!!
!!    call cdensebinT(o8, o7, chOut(8), chOut(7), w8, t8)
!!    print *, '- 8th Layer: done'
!!
!!    call densebin(o9, o8, chOut(9), chOut(8), w9)
!!    print *, '- 9th Layer: done'
!!
!!    print *, o9
!!
!!    call argmax(pos, o9, 10, 1)
!!    result = pos-1
!!    print *, 'expected: ', labels(pic)
!!    print *, 'result  : ', result
!
!
!end program testfunctions

subroutine runtests()
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

end subroutine runtests

subroutine testmsum3d()
    integer, parameter :: n = 2, m = 2, k=2
    integer a(n,m,k), res, expected

    a = reshape([ 1, 2, 3, 4, 5, 6, 7, 8], [n,m,k])
    expected = 36
    call msum3d(res, a, shape(a))
    call testequalintegermat(expected, res, [1], [1], 1, 'msum2d')

end subroutine testmsum3d

subroutine testmmulargmax()
    integer, parameter :: n = 3, m = 3
    integer, dimension(n ,m) :: a, b, res1, res2, res3
    integer :: mx(2)

    a = transpose(reshape([ 1, 2, 3, 4, 5, 6, 7, 8, 9], [n,m]))
    !transpose because fortran is column major, therefore without it the matrix would be 1,4,7/2,5,9/3,6,9
    b = reshape([ 1, 2, 3, 4, 5, 6, 7, 8, 9], [n,m])
    res1 = reshape([ 0, 0, 0, 0, 0, 0, 0, 0, 0], [n,m])
    res2 = reshape([ 0, 0, 0, 0, 0, 0, 0, 0, 0], [n,m])

!    print *, '---------------'
!    print *, 'Testing mmul, sqmmul and argmax'
!    print *, ' '
!    print *, 'a = ', a
!    print *, 'b = ', b
!    print *, 'res1 = ', res1
!    print *, 'res2 = ', res2

    res1 = matmul(a, b)

!    print *, 'matmul| res1 = ', res1

    call mmul(res2, a, b, n, m, n, m)

!    print *, 'mmul  | res2 = ', res2

    call sqmmul(res3, a, b, n, m)

!    print *, 'sqmmul| res3 = ', res3

    call argmax(mx, a, n, m)

!    print *, 'argmax| mx = ', mx, shape(mx)
!    print *, ' '
    call testequalintegermat(res1, res2, shape(res1), shape(res2), 2, 'mmul')
    call testequalintegermat(res1, res3, shape(res1), shape(res3), 2, 'sqmul')
    call testequalintegermat(mx, [3, 3], shape(mx), shape(mx), 1, 'argmax')
!    print *, mx
end subroutine testmmulargmax

subroutine testmaxpool()
    integer, parameter :: n = 6, m = 6
    integer :: a(n, m), res1(n/2, m/2), expected(n/2, m/2)
    a = transpose(reshape([ 1, 7, 3, 4, 5, 6, &
                            7, 8, 9, 10, 16, 12, &
                            13, 14, 15, 11, 10, 4, &
                            -1, 4, 66, -10, 43, 5, &
                            3, 6, 7, 20, -7, -23, &
                            -19, 0, 4, 68, -3, -2], [n,m]))

!    print *, '---------------'
!    print *, 'Testing maxpool'
!    print *, ' '

    call maxpool(res1, a, [2 ,2], n, m)
!    print *, 'input = '
!    call printmat(a, shape(a))
!    print *, 'res1 = '
!    call printmat(res1, shape(res1))
    expected = reshape([8, 14, 6, 10, 66, 68, 16, 43, -2], shape(expected))
    call testequalintegermat(res1, expected, shape(res1), shape(res1), 2, 'maxpool')
end subroutine testmaxpool

subroutine testhardtanh2d()
    integer, parameter :: n = 4, m = 4
    integer :: a(n,m), res(n,m), expected(n,m)
    a = reshape([10, -2, 8, -9, -9,  0, -1,-2, 1, 4, 5, -1, 4, -19, -22, 2], shape(a))
    expected = reshape([1, -1, 1, -1, -1, 1, -1, -1, 1, 1, 1, -1, 1, -1, -1, 1], shape(expected))
    call hardtanh2d(res, a, n, m)
    call testequalintegermat(res, expected, shape(res), shape(expected), 2, 'hardtanh2d')
!    call printmat(res, shape(res))
end subroutine testhardtanh2d

subroutine testhardtanh3d()
    integer, parameter :: n = 2, m = 2, l = 2
    integer :: a(n, m, l), res(n, m, l), expected(n, m, l)
    a = reshape([3, -2, 4, 3, -2, 4, -2, 4], shape(a))
    expected = reshape([1, -1, 1, 1, -1, 1, -1, 1], shape(expected))
    call hardtanh3d(res, a, n, m, l)
    call testequalintegermat(res, expected, shape(res), shape(expected), 3, 'hardtanh3d')
!    print *, expected
!    print *, res
end subroutine testhardtanh3d

subroutine testhardtanh4d()
    integer, parameter :: n = 2, m = 2, l = 2, k = 2
    integer :: a(n, m, l, k), res(n, m, l, k), expected(n, m, l, k)
    a = reshape([3, -2, 4, 3, -2, 4, -2, 4, 5, 2, -9, -6, 9, 10, -4, -3], shape(a))
    expected = reshape([1, -1, 1, 1, -1, 1, -1, 1, 1, 1, -1, -1, 1, 1, -1, -1], shape(expected))
    call hardtanh4d(res, a, n, m, l, k)
    call testequalintegermat(res, expected, shape(res), shape(expected), 4, 'hardtanh4d')
!    print *, expected
!    print *, res
end subroutine testhardtanh4d

subroutine testbatchnorm2d()
    integer, parameter :: n = 2, m = 2
    real :: res(n,m), a(n,m), expected(n,m), beta, gam, mean, invstd
    a = reshape([0.5, -2.5, 4.2, -1.8], shape(a))
    beta = 0.4
    gam = 0.8
    mean = 1.2
    invstd = 1.5
    expected = reshape([-.44, -4.04, 4.0, -3.2], shape(expected))
    call batchnorm2d(res, a, shape(a), beta, gam, mean, invstd)
    call testequalrealmat(expected, res, [n, m, 1], [n, m, 1], 'batchnorm2d')
end subroutine testbatchnorm2d

subroutine testbatchnorm3d()
    integer, parameter :: n = 2, m = 2, k = 2
    real :: beta(k), gam(k), mean(k), invstd(k)
    real :: res1(k,n,m), res2(n,k,m), res3(n,m,k)
    real :: a1(k,n,m), a2(n,k,m), a3(n,m,k)
    real :: exp1(k,n,m), exp2(n,k,m), exp3(n,m,k)
    a1 = reshape([0.5, -2.5, 4.2, -1.8, 1.0, 2.0, 3.0, -2.0], shape(a1))
    a2 = reshape([0.5, -2.5, 4.2, -1.8, 1.0, 2.0, 3.0, -2.0], shape(a2))
    a3 = reshape([0.5, -2.5, 4.2, -1.8, 1.0, 2.0, 3.0, -2.0], shape(a3))

!    print *, a1(1, 1, 1), a1(1, 1, 2)
!    print *, a1(1, 2, 1), a1(1, 2, 2)
!    print *, a1(2, 1, 1), a1(2, 1, 2)
!    print *, a1(2, 2, 1), a1(2, 2, 2)

    beta = reshape([0.4, -0.5], shape(beta))
    gam = reshape([0.8, 0.6], shape(gam))
    mean = reshape([1.2, 0.5], shape(mean))
    invstd = reshape([1.5, -0.9], shape(invstd))

    exp1 = reshape([-.44, 1.12, 4.0, 0.742, 0.16, -1.31, 2.56, 0.85], shape(exp1))
    exp2 = reshape([-.44, -4.04, -2.498, 0.742, 0.16, 1.36, -1.85, 0.85], shape(exp2))
    exp3 = reshape([-.44, -4.04, 4.0, -3.2, -.77, -1.31, -1.85, 0.85], shape(exp3))
    call batchnorm3d(res1, a1, shape(a1), beta, gam, mean, invstd, 1)
    call batchnorm3d(res2, a2, shape(a2), beta, gam, mean, invstd, 2)
    call batchnorm3d(res3, a3, shape(a3), beta, gam, mean, invstd, 3)
    call testequalrealmat(exp1, res1, [k, n, m], [k, n, m], 'batchnorm3d piv=1')
    call testequalrealmat(exp2, res2, [n, k, m], [n, k, m], 'batchnorm3d piv=2')
    call testequalrealmat(exp3, res3, [n, m, k], [n, m, k], 'batchnorm3d piv=3')

end subroutine testbatchnorm3d

subroutine testconv2d()
    integer, parameter :: chOut = 2, chIn = 3, W = 7, H = 7, fn = 3, fm = 3, stride = 2
    integer a(chIn, H, W), bias(chOut), fil(chOut, chIn, fn, fm), res(chOut, (H-fn)/stride+1, (W-fm)/stride+1)
    integer expected(chOut, (H-fn)/stride+1, (W-fm)/stride+1)

    a(1, :, :) = reshape([0, 0, 0, 0, 0, 0, 0, &
                          0, 1, 0, 1, 1, 2, 0, &
                          0, 0, 0, 0, 2, 0, 0, &
                          0, 0, 1, 1, 1, 1, 0, &
                          0, 2, 0, 1, 1, 1, 0, &
                          0, 2, 0, 1, 0, 2, 0, &
                          0, 0, 0, 0, 0, 0, 0], &
                          shape(a(1, :, :)))

    a(2, :, :) = reshape([0, 0, 0, 0, 0, 0, 0, &
                          0, 1, 0, 0, 2, 2, 0, &
                          0, 1, 0, 2, 0, 1, 0, &
                          0, 1, 2, 1, 1, 0, 0, &
                          0, 1, 2, 0, 2, 2, 0, &
                          0, 1, 2, 1, 0, 0, 0, &
                          0, 0, 0, 0, 0, 0, 0], &
                          shape(a(1, :, :)))

    a(3, :, :) = reshape([0, 0, 0, 0, 0, 0, 0, &
                          0, 0, 2, 2, 1, 2, 0, &
                          0, 1, 1, 0, 2, 2, 0, &
                          0, 1, 2, 2, 2, 0, 0, &
                          0, 1, 1, 1, 1, 2, 0, &
                          0, 0, 0, 2, 1, 1, 0, &
                          0, 0, 0, 0, 0, 0, 0], &
                          shape(a(1, :, :)))

    fil(1, 1, :, :) = reshape([-1, 0, -1, &
                               0, -1, -1, &
                               -1, 0, 0], &
                               [3,3])

    fil(1, 2, :, :) = reshape([0, 0, 0, &
                               1, 1, 0, &
                               0, 0, -1], &
                               [3,3])

    fil(1, 3, :, :) = reshape([1, 1, 0, &
                               -1, 0, 0, &
                               0, -1, -1], &
                               [3,3])

    fil(2, 1, :, :) = reshape([0, -1, 1, &
                               0, 1, -1, &
                               -1, 0, 0], &
                               [3,3])

    fil(2, 2, :, :) = reshape([-1, 0, -1, &
                               0, 1, 1, &
                               0, 0, 1], &
                               [3,3])

    fil(2, 3, :, :) = reshape([-1, 1, -1, &
                               1, 1, 0, &
                               -1, 1, 0], &
                               [3,3])

    bias = reshape([1, 0], shape(bias))

    expected(1, :, :) = reshape([-1, -5, -2, &
                                 -2, -5, -2, &
                                 1, 4, 0], &
                                 [3,3])

    expected(2, :, :) = reshape([3, 5, 5, &
                                 6, 7, 3, &
                                 1, -1, 2], &
                                 [3,3])

    call conv2d(res, a, chOut, chIn, W, H, bias, fil, fn, fm, stride)
!    print *, res
!    print *, expected
    call testequalintegermat(res, expected, shape(res), shape(expected), size(shape(res)), 'conv2d')
end subroutine testconv2d

subroutine testthresholdLayer()
    integer, parameter :: n = 2, m = 2, l = 2
    integer :: tresh(n), a(n, m, l), res(n, m, l), expected(n, m, l)
    a = reshape([1, -2, -1, 3, -2, 4, 2, -1], shape(a))
    tresh = reshape([1, -1], shape(tresh))
!    expected = reshape([1, -1, -1, 1, -1, 1, 1, 1], shape(expected))
    expected = reshape([1, 0, 0, 1, 0, 1, 1, 1], shape(expected))
    call thresholdLayer(res, a, n, m, l, tresh)
    call testequalintegermat(res, expected, shape(res), shape(expected), size(shape(res)), 'thresholdLayer')

!    print *, a
!    print *, res
!    print *, expected
end subroutine testthresholdLayer

subroutine testdense()
    integer, parameter :: nIn = 3, nOut = 2
    integer :: res(nOut), expected(nOut), a(nIn), weights(nIn, nOut)

    a = [1, 0, 1]
    weights = reshape([1, 1, 0, 1, 0, 1], shape(weights))
    expected = [1, 2]
!    print *, '################write test func - dense'
    call dense(res, a, nOut, nIn, weights)
    call testequalintegermat(res, expected, shape(res), shape(expected), size(shape(res)), 'dense')
end subroutine testdense

subroutine testload()
    integer, parameter :: f = 3, chOut(0:9) = [3, 64, 64, 128, 128, 256, 256, 512, 512, 10]
    integer(1) :: w1(chOut(1), chOut(0), f ,f), w2(chOut(2), chOut(1), f ,f)
    integer(1) :: w3(chOut(3), chOut(2), f ,f), w4(chOut(4), chOut(3), f ,f)
    integer(1) :: w5(chOut(5), chOut(4), f ,f), w6(chOut(6), chOut(5), f ,f)
    integer(1) :: w7(chOut(6), chOut(7)), w8(chOut(7), chOut(8)), w9(chOut(8), chOut(9))
    integer(4) :: t1(chOut(1)), t2(chOut(2)), t3(chOut(3)), t4(chOut(4)), t5(chOut(5))
    integer(4) :: t6(chOut(6)), t7(chOut(7)), t8(chOut(8))

    call loadData(chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)

    !call printmat(w1(1,1,:,:), [3, 3])
    print *, '###############write test func - load'

end subroutine testload

subroutine testReader()
    integer imgs(10000,3,32,32), labels(10000)
    call cifarFileReader1(imgs, labels)
    print *, '###############write test func - read'
end subroutine testReader

subroutine testsumpopcount()
    integer, parameter :: n = 2, m = 2, l = 2
    integer :: a(n, m, l), res, expected
    a = reshape([1, 0, 0, 1, 1, 1, 0, 1], shape(a))
    expected = 2
    call sumPopcount(res, a, [n, m, l])
    call testequalintegermat([res], [expected], [1], [1], 1, 'sumpopcount1')
    a = reshape([1, 1, 1, 1, 1, 1, 1, 1], shape(a))
    expected = 8
    call sumPopcount(res, a, [n, m, l])
    call testequalintegermat([res], [expected], [1], [1], 1, 'sumpopcount2')
    a = reshape([0, 0, 0, 0, 0, 0, 0, 0], shape(a))
    expected = -8
    call sumPopcount(res, a, [n, m, l])
    call testequalintegermat([res], [expected], [1], [1], 1, 'sumpopcount3')

end subroutine testsumpopcount

subroutine testxnor()
    integer res
    call xnor(res, 1, 0)
    call testequalintegermat([res], [0], [1], [1], 1, 'xnor1')
    call xnor(res, 0, 1)
    call testequalintegermat([res], [0], [1], [1], 1, 'xnor2')
    call xnor(res, 1, 1)
    call testequalintegermat([res], [1], [1], [1], 1, 'xnor3')
    call xnor(res, 0, 0)
    call testequalintegermat([res], [1], [1], [1], 1, 'xnor4')
end subroutine testxnor

subroutine testmmulbin()
    integer, parameter :: n = 3, m = 2
    integer :: a(m, n), b(n, m), res(m, m), expected(m,m)
    a = transpose(reshape([ 1, 1, 0, 1, 0, 1], [n,m]))
    !transpose because fortran is column major, therefore without it the matrix would be 1,4,7/2,5,9/3,6,9
    b = reshape([ 1, 1, 1, 0, 0, 0], [n,m])
    expected = reshape([ 2, 2, 1, 1], [m,m])
    call mmulbin(res, a, b, m, n, n, m)
    call testequalintegermat(res, expected, shape(res), shape(expected), 2, 'mmulbin')
end subroutine testmmulbin

subroutine testdensebin()
    integer, parameter :: nIn = 3, nOut = 2
    integer :: res(nOut), expected(nOut), a(nIn), weights(nIn, nOut)

    a = [1, 0, 1]
    weights = reshape([1, 1, 0, 1, 0, 1], shape(weights))
    expected = [1, 3]
!    print *, '################write test func - dense'
    call densebin(res, a, nOut, nIn, weights)
    call testequalintegermat(res, expected, shape(res), shape(expected), size(shape(res)), 'densebin')
end subroutine testdensebin

subroutine testconv2dbin()
    integer, parameter :: chOut = 1, chIn = 1, W = 3, H = 3, fn = 2, fm = 2, stride = 1
    integer a(chIn, H, W), bias(chOut), fil(chOut, chIn, fn, fm), res(chOut, (H-fn)/stride+1, (W-fm)/stride+1)
    integer expected(chOut, (H-fn)/stride+1, (W-fm)/stride+1)
    integer t(2,2)

    a(1, :, :) = reshape([1, 1, 0, 0, 1, 0, 1, 0, 1], shape(a(1, :, :)))
    t = reshape([ 0, 1, 1, 1], [fn,fm])
    fil(1, 1, :, :) = t
    expected = reshape([ 2, 1, 3, 1], shape(expected))
    bias = [0]

    call conv2dbin(res, a, chOut, chIn, W, H, bias, fil, fn, fm, stride)
    call testequalintegermat(res, expected, shape(res), shape(expected), 3, 'conv2dbin')

end subroutine testconv2dbin
