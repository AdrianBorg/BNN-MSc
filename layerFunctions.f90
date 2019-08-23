subroutine maxpool(output, input, ps, n, m)
    implicit none
    !ps (poolsize) parameters must perfectly divide the relevant n or m
    !output - output matrix
    !input - input matrix
    !ps - pool size (2D)
    !n - num of rows in input
    !m - num of columns in input
    integer, intent(in) :: n, m, input(n,m), ps(2)
    integer, intent(out) :: output(n/ps(1), m/ps(2))
    integer row, col, loc(2), t1, t2

    do row = 1, n, ps(1)
        do col = 1, m, ps(2)
            call argmax(loc, input(row:row+ps(1)-1, col:col+ps(2)-1), ps(1), ps(2))
            t1 = row/ps(1) + 1
            t2 = col/ps(2) + 1
!            print *, loc
            output(t1, t2) = input(row+loc(1)-1, col+loc(2)-1)
        end do
    end do

end subroutine maxpool

subroutine hardtanh2d(res, a, n, m)
    !converts input 2d matrix to one with values of either 1 or -1.
    !converts values <0 to -1, otherwise to 1
    !res - output matrix
    !a - input matrix
    !n - num of rows in input
    !m - num of columns in input
    integer, intent(in) :: n, m, a(n,m)
    integer, intent(out) :: res(n,m)
    integer i, j

    do i = 1, n
        do j= 1, m
            if (a(i,j) < 0) then
                res(i,j) = -1
            else
                res(i,j) = 1
            end if
        end do
    end do
end subroutine hardtanh2D

subroutine hardtanh3d(res, a, n, m, l)
    !converts input 3d matrix to one with values of either 1 or -1.
    !converts values <0 to -1, otherwise to 1
    !res - output matrix
    !a - input matrix
    !n - num of rows in input
    !m - num of columns in input
    !l - num of channels in input
    integer, intent(in) :: n, m, l, a(n, m, l)
    integer, intent(out) :: res(n, m, l)
    integer k
    do k = 1, l
!        print*, res(k, :, :)
        call hardtanh2d(res(k, :, :), a(k, :, :), n, m)
    end do
end subroutine hardtanh3d

subroutine hardtanh4d(res, a, n, m, l, k)
    !converts input 4d matrix to one with values of either 1 or -1.
    !converts values <0 to -1, otherwise to 1
    !res - output matrix
    !a - input matrix
    !n, m, l, k - num of indicies in each of the 4 dimensions
    integer, intent(in) :: n, m, l, k, a(n, m, l, k)
    integer, intent(out) :: res(n, m, l, k)
    integer j
    do j = 1, k
        call hardtanh3d(res(j, : , :, :), a(j, :, :, :), n, m, l)
    end do
end subroutine hardtanh4d

subroutine batchnorm2d(res, a, sh, beta, gam, mean, invstd)
    !applies batch normalisation to one channel
    !res - output matrix
    !a - input matrix
    !sh - shape of a
    !beta - addition parameter
    !gam - multiplication param
    !mean - mean of the channel
    !invstd - inverse of the standard deviation
    integer, intent(in) :: sh(2)
    real, intent(in) ::  a(sh(1), sh(2)), beta, gam, mean, invstd
    real, intent(out) :: res(sh(1), sh(2))
    integer i, j

    do i = 1, sh(1)
        do j = 1, sh(2)
            res(i,j) = (a(i,j) - mean)*invstd*gam + beta
        end do
    end do
end subroutine batchnorm2d

subroutine batchnorm3d(res, a, sh, beta, gam, mean, invstd, pivot)
    !applies batch normalisation to all channels
    !res - output matrix
    !a - input matrix
    !sh - shape of a
    !beta - addition parameter
    !gam - multiplication param
    !mean - mean of the channel
    !invstd - inverse of the standard deviation
    !pivot - the axis along which to apply the 2d batch norms
    integer, intent(in) :: sh(3), pivot
    real, intent(in) ::  a(sh(1), sh(2), sh(3)), beta(sh(pivot)), gam(sh(pivot)), mean(sh(pivot)), invstd(sh(pivot))
    real, intent(out) :: res(sh(1), sh(2), sh(3))
    integer i

    if ( pivot < 1 .OR. pivot > 3) then
        print *, 'Pivot must be 1, 2 or 3, nothing else'
    end if

    if (pivot == 1) then
        do i = 1, sh(pivot)
            call batchnorm2d(res(i, :, :), a(i, :, :), [sh(2), sh(3)], beta(i), gam(i), mean(i), invstd(i))
        end do
    else if (pivot == 2) then
        do i = 1, sh(pivot)
            call batchnorm2d(res(:, i, :), a(:, i, :), [sh(1), sh(3)], beta(i), gam(i), mean(i), invstd(i))
        end do
    else if (pivot == 3) then
        do i = 1, sh(pivot)
            call batchnorm2d(res(:, :, i), a(:, :, i), [sh(1), sh(2)], beta(i), gam(i), mean(i), invstd(i))
        end do
    end if
end subroutine batchnorm3d

subroutine conv2d(res, a, chOut, chIn, W, H, bias, fil, fn, fm, stride)
    !performs a 2d convolution
    !res - output matrix
    !a - input matrix
    !chOut - number of output channels
    !chIn - number of input channels
    !W - width of each channel (num of cols)
    !H - height of each channel (num of rows)
    !bias - vector of biases for each output channel
    !fil - matrix of filters
    !fn - number of
    !stride - stride
    integer, intent(in) :: chOut, chIn, W, H, a(chIn, H, W), bias(chOut), fn, fm, fil(chOut, chIn, fn, fm), stride
    integer, intent(out) :: res(chOut, (H-fn)/stride+1, (W-fm)/stride+1)
    integer i, j, l, tempsum, filmult(chIn, fn, fm)

    res = res * 0

    do l = 1, chOut
!        do k = 1, chIn
            do j = 1, H-fn+1, stride
                do i = 1, W-fm+1, stride
!                    call elemMultBin (filmult, fil(l, :, :, :), a(:, j:j+fn-1, i:i+fm-1), chIn, fn, fm) !filter * receptive field
                    filmult = fil(l, :, :, :) * a(:, j:j+fn-1, i:i+fm-1)
                    call msum3d(tempsum, filmult, shape(filmult)) !sum of resulting matrix
!                    call sumPopcount(tempsum, filmult, shape(filmult))
                    res(l, (j-1)/stride+1, (i-1)/stride+1) = tempsum + bias(l) !output element value = sum + bias
                end do
            end do
!        end do
    end do

end subroutine conv2d

subroutine thresholdLayer(res, a, ch, W, H, thres)
    !makes the bits be -1 if below the threshold or 1 otherwise
    !res - output matrix
    !a - input matrix
    !ch - number of channels
    !W - width of each channel (num of cols)
    !H - height of each channel (num of rows)
    !thres - thresholds for each channel
    integer, intent(in) ::  ch, W, H, thres(ch), a(ch, H, W)
    integer, intent(out) :: res(ch, H, W)
    integer i, j, k

    call timingstarts(5)

    do k = 1, ch
        do j = 1, H
            do i = 1, W
                if (a(k, j, i) < thres(k)) then
!                    res(k, j, i) = -1
                    res(k, j, i) = 0
                else
                    res(k, j, i) = 1
                end if
!                print *, k, j, i, a(k, j, i), thres(k)
            end do
        end do
    end do

    call timingend(5)

end subroutine thresholdLayer

subroutine dense(res, a, nOut, nIn, weights)
    !emulates a fullly connected layer
    !res - output matrix
    !a - input matrix
    !nOut - num of outputs
    !nIn - num of inputs
    !weights - weights matrix
    integer, intent(in) :: nIn, nOut, a(nIn), weights(nIn, nOut)
    integer, intent(out) :: res(nOut)
    integer tempIn(1, nIn), tempOut(nOut, 1)

    tempIn(1, :) = a(:)
    call mmul(tempOut, tempIn, weights, 1, nIn, nIn, nOut)
    res = tempOut(:, 1)
end subroutine dense

!binary versions

subroutine densebin(res, a, nOut, nIn, weights)
    !emulates a fullly connected layer
    !res - output matrix
    !a - input matrix
    !nOut - num of outputs
    !nIn - num of inputs
    !weights - weights matrix
    integer, intent(in) :: nIn, nOut, a(nIn), weights(nIn, nOut)
    integer, intent(out) :: res(nOut)
    integer tempIn(1, nIn), tempOut(nOut, 1)

    call timingstarts(4)

    tempIn(1, :) = a(:)
    call mmulbin(tempOut, tempIn, weights, 1, nIn, nIn, nOut)
    res = tempOut(:, 1)

    call timingend(4)

end subroutine densebin

subroutine conv2dbin(res, a, chOut, chIn, W, H, fil, fn, fm, stride)
    !performs a 2d convolution, assumes filter is binary
    !res - output matrix
    !a - input matrix
    !chOut - number of output channels
    !chIn - number of input channels
    !W - width of each channel (num of cols)
    !H - height of each channel (num of rows)
    !fil - matrix of filters
    !fn - number of
    !stride - stride
    integer, intent(in) :: chOut, chIn, W, H, a(chIn, H, W), fn, fm, fil(chOut, chIn, fn, fm), stride
    integer, intent(out) :: res(chOut, (H-fn)/stride+1, (W-fm)/stride+1)
    integer i, j, l, tempsum, filmult(chIn, fn, fm)

    integer i2, j2, k2

    call timingstarts(2)

    res = res * 0

    do l = 1, chOut
        do j = 1, H-fn+1, stride
            do i = 1, W-fm+1, stride
!                    call elemwisexnor3d(filmult, fil(l, :, :, :), a(:, j:j+fn-1, i:i+fm-1), chIn, fn, fm) !filter * receptive field
                do i2 = 1, fm
                    do j2 = 1, fn
                        do k2 = 1, chIn
                            if (fil(l, k2, j2, i2) == a(k2, j+j2-1, i+i2-1)) then
                                filmult(k2, j2, i2) = 1
                            else
                                filmult(k2, j2, i2) = 0
                            end if
                        end do
                    end do
                end do
!                    call msum3d(tempsum, filmult, shape(filmult)) !sum of resulting matrix
                tempsum = 0

                do i2 = 1, fm
                    do j2 = 1, fn
                        do k2 = 1, chIn
                            tempsum = tempsum + filmult(k2, j2, i2)
                        end do
                    end do
                end do

                res(l, (j-1)/stride+1, (i-1)/stride+1) = tempsum !output element value = sum
            end do
        end do
    end do

    call timingend(2)

end subroutine conv2dbin

subroutine conv2dbinT(res, a, chOut, chIn, W, H, fil, fn, fm, stride, thres)
    !performs a 2d convolution, assumes filter is binary
    !res - output matrix
    !a - input matrix
    !chOut - number of output channels
    !chIn - number of input channels
    !W - width of each channel (num of cols)
    !H - height of each channel (num of rows)
    !fil - matrix of filters
    !fn - number of
    !stride - stride
    !thres - thresholds
    integer, intent(in) :: chOut, chIn, W, H, a(chIn, H, W), fn, fm, fil(chOut, chIn, fn, fm), stride
    integer, intent(in) :: thres(chOut)
    integer, intent(out) :: res(chOut, (H-fn)/stride+1, (W-fm)/stride+1)
    integer i, j, l, tempsum, filmult(chIn, fn, fm)

    integer i2, j2, k2

    call timingstarts(2)

    res = res * 0

    do l = 1, chOut
        do j = 1, H-fn+1, stride
            do i = 1, W-fm+1, stride
!                    call elemwisexnor3d(filmult, fil(l, :, :, :), a(:, j:j+fn-1, i:i+fm-1), chIn, fn, fm) !filter * receptive field
                do i2 = 1, fm
                    do j2 = 1, fn
                        do k2 = 1, chIn
                            if (fil(l, k2, j2, i2) == a(k2, j+j2-1, i+i2-1)) then
                                filmult(k2, j2, i2) = 1
                            else
                                filmult(k2, j2, i2) = 0
                            end if
                        end do
                    end do
                end do
!                    call msum3d(tempsum, filmult, shape(filmult)) !sum of resulting matrix
                tempsum = 0

                do i2 = 1, fm
                    do j2 = 1, fn
                        do k2 = 1, chIn
                            tempsum = tempsum + filmult(k2, j2, i2)
                        end do
                    end do
                end do

!                   call  threshold layer
                if (tempsum < thres(l)) then
                    res(l, (j-1)/stride+1, (i-1)/stride+1) = 0
                else
                    res(l, (j-1)/stride+1, (i-1)/stride+1) = 1
                end if
!                    res(l, (j-1)/stride+1, (i-1)/stride+1) = tempsum !output element value = sum
            end do
        end do
    end do

    call timingend(2)

end subroutine conv2dbinT

subroutine CNVconvT(res, a, chOut, chIn, W, H, fil, fn, fm, stride, thres)
    !performs a 2d convolution
    !res - output matrix
    !a - input matrix
    !chOut - number of output channels
    !chIn - number of input channels
    !W - width of each channel (num of cols)
    !H - height of each channel (num of rows)
    !bias - vector of biases for each output channel
    !fil - matrix of filters
    !fn - number of
    !stride - stride
    integer, intent(in) :: chOut, chIn, W, H, a(chIn, H, W), fn, fm, fil(chOut, chIn, fn, fm), stride
    integer, intent(in) :: thres(chOut)
    integer, intent(out) :: res(chOut, (H-fn)/stride+1, (W-fm)/stride+1)
    integer i, j, l, tempsum, filmult(chIn, fn, fm), b(chIn, fn, fm)
    integer i2, j2, k2
    call timingstarts(1)

    res = res * 0

    do l = 1, chOut
        do j = 1, H-fn+1, stride
            do i = 1, W-fm+1, stride
                b = 2 * fil(l, :, :, :) - 1 ! in order to get values to be -1 or +1 from 0 and 1
                filmult = b * a(:, j:j+fn-1, i:i+fm-1)
!                    call msum3d(tempsum, filmult, shape(filmult)) !sum of resulting matrix
                tempsum = 0

                do i2 = 1, fm
                    do j2 = 1, fn
                        do k2 = 1, chIn
                            tempsum = tempsum + filmult(k2, j2, i2)
                        end do
                    end do
                end do
                !                   call  threshold layer
                if (tempsum < thres(l)) then
                    res(l, (j-1)/stride+1, (i-1)/stride+1) = 0
                else
                    res(l, (j-1)/stride+1, (i-1)/stride+1) = 1
                end if
!                    res(l, (j-1)/stride+1, (i-1)/stride+1) = tempsum !output element value = sum + bias
            end do
        end do
    end do

    call timingend(1)

end subroutine CNVconvT
!#####convenience functions

subroutine CNVconv(res, a, chOut, chIn, W, H, fil, fn, fm, stride)
    !performs a 2d convolution
    !res - output matrix
    !a - input matrix
    !chOut - number of output channels
    !chIn - number of input channels
    !W - width of each channel (num of cols)
    !H - height of each channel (num of rows)
    !bias - vector of biases for each output channel
    !fil - matrix of filters
    !fn - number of
    !stride - stride
    integer, intent(in) :: chOut, chIn, W, H, a(chIn, H, W), fn, fm, fil(chOut, chIn, fn, fm), stride
    integer, intent(out) :: res(chOut, (H-fn)/stride+1, (W-fm)/stride+1)
    integer i, j, l, tempsum, filmult(chIn, fn, fm), b(chIn, fn, fm)
    integer i2, j2, k2
    call timingstarts(1)

    res = res * 0

    do l = 1, chOut
!        do k = 1, chIn
            do j = 1, H-fn+1, stride
                do i = 1, W-fm+1, stride
                    b = 2 * fil(l, :, :, :) - 1 ! in order to get values to be -1 or +1 from 0 and 1
                    filmult = b * a(:, j:j+fn-1, i:i+fm-1)
!                    call msum3d(tempsum, filmult, shape(filmult)) !sum of resulting matrix
                    tempsum = 0

                    do i2 = 1, fm
                        do j2 = 1, fn
                            do k2 = 1, chIn
                                tempsum = tempsum + filmult(k2, j2, i2)
                            end do
                        end do
                    end do
                    res(l, (j-1)/stride+1, (i-1)/stride+1) = tempsum !output element value = sum + bias
                end do
            end do
!        end do
    end do

    call timingend(1)

end subroutine CNVconv

!subroutine conv2dCNV(res, a, chOut, chIn, dims, weights)
!    !convenience subroutine to call con2v with 3x3 filter, stride = 1 and biases = 0
!    integer, intent(in) :: chOut, chIn, dims, a(chIn, dims, dims)
!    integer, intent(in) :: weights(chOut, chIn, dims, dims)
!    integer, intent(out) :: res(chOut, dims-2, dims-2)
!
!    call CNVconv(res, a, chOut, chIn, dims, dims, weights, 3, 3, 1)
!end subroutine conv2dCNV

!subroutine conv2dbinCNV(res, a, chOut, chIn, dims, weights)
!    !convenience subroutine to call con2v with 3x3 filter, stride = 1 and biases = 0
!    integer, intent(in) :: chOut, chIn, dims, a(chIn, dims, dims)
!    integer, intent(in) :: weights(chOut, chIn, dims, dims)
!    integer, intent(out) :: res(chOut, dims-2, dims-2)
!
!    call conv2dbin(res, a, chOut, chIn, dims, dims, weights, 3, 3, 1)
!end subroutine conv2dbinCNV

subroutine cconvT(res, a, chOut, chIn, dims, weights, thres, bin)
    !convenience subroutine to call con2v with 3x3 filter, stride = 1 and biases = 0
    !and apply threshold layer after
    integer, intent(in) :: chOut, chIn, dims, a(chIn, dims, dims), thres(chOut)
    integer, intent(in) :: weights(chOut, chIn, dims, dims)
    integer, intent(out) :: res(chOut, dims-2, dims-2)
    integer outConv(chOut, dims-2, dims-2)
    logical bin

    if (bin) then
        call conv2dbin(outConv, a, chOut, chIn, dims, dims, weights, 3, 3, 1)
    else
        call CNVconv(outConv, a, chOut, chIn, dims, dims, weights, 3, 3, 1)
    end if
    call thresholdLayer(res, outConv, chOut, dims-2, dims-2, thres)

end subroutine cconvT

subroutine cdensebinT(res, a, nOut, nIn, weights, thres)
    !convenience subroutine to call dense and threshold layer
    !res - output matrix
    !a - input matrix
    !nOut - num of outputs
    !nIn - num of inputs
    !weights - weights matrix
    !thres - thresholds for each channel
    integer, intent(in) :: nIn, nOut, a(nIn), thres(nOut)
    integer, intent(in) :: weights(nIn, nOut)
    integer, intent(out) :: res(nOut)
    integer outDense(nOut)

    call densebin(outDense, a, nOut, nIn, weights)
    call thresholdLayer(res, outDense, nOut, 1, 1, thres)

end subroutine cdensebinT

subroutine maxpool2x23d(output, input, ch, n)
    !perform 3d max pool with pool size 2x2
    !output - output matrix
    !input - input matrix
    !ch - num of channels in input
    !n - num of rows/columns in input
    integer, intent(in) :: ch, n, input(ch, n, n)
    integer, intent(out) :: output(ch, n/2, n/2)
    integer i

    call timingstarts(3)

    do i = 1, ch
        call maxpool(output(i, :, :), input(i, :, :), [2, 2], n, n)
    end do

    call timingend(3)

end subroutine maxpool2x23d
