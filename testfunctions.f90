program testfunctions
    implicit none

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
    call testload()
    !call testload1()
    call testReader()

end program testfunctions

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
    expected = reshape([1, -1, -1, 1, -1, 1, 1, 1], shape(expected))
    call thresholdLayer(res, a, n, m, l, tresh)
    call testequalintegermat(res, expected, shape(res), shape(expected), size(shape(res)), 'thresholdLayer')

!    print *, a
!    print *, res
!    print *, expected
end subroutine testthresholdLayer

subroutine testdense()
    integer, parameter :: nIn = 2, nOut = 2
    integer :: res(nOut), expected(nOut), a(nIn), weights(nIn, nOut)
    print *, '################write test func'
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

end subroutine testload

subroutine testReader()
    integer(2) imgs(10000,3,32,32), labels(10000)
    call cifarFileReader(imgs, labels)
end subroutine testReader
