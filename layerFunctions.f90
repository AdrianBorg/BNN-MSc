subroutine maxpool(output, input, ps, n, m)
    implicit none
    !NOTE ps (poolsize) parameters must perfectly divide the relevant n or m
    !output - output matrix
    !input - input matrix
    !ps - pool size (2D)
    !n - num of rows in input
    !m - num of columns in input
    integer, intent(in) :: n, m, input(n,m), ps(2)
    integer, intent(out) :: output(n/ps(1), m/ps(2))
    integer row, col, loc(2), t1, t2

    do row = 1, n, ps(1) !cycle through the array
        do col = 1, m, ps(2)
            !get the index of the maximum value
            call argmax(loc, input(row:row+ps(1)-1, col:col+ps(2)-1), ps(1), ps(2))
            t1 = row/ps(1) + 1
            t2 = col/ps(2) + 1
            !save the value in the output array
            output(t1, t2) = input(row+loc(1)-1, col+loc(2)-1)
        end do
    end do

end subroutine maxpool

subroutine thresholdLayer(res, a, ch, W, H, thres)
    !acts in place of a batchnorm layer in a binarized neural network
    !makes the bits in an array 0 if below the threshold or 1 otherwise
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

    do k = 1, ch !cycle through each channel and index
        do j = 1, H
            do i = 1, W
                if (a(k, j, i) < thres(k)) then
                    res(k, j, i) = 0
                else
                    res(k, j, i) = 1
                end if
            end do
        end do
    end do

    call timingend(5)

end subroutine thresholdLayer

!Subroutines for binary versions of neural network layers

subroutine densebin(res, a, nOut, nIn, weights)
    !emulates a binarized, fully connected layer
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
    !perform binarized matrix multiplication of the input with the weights matrix
    call mmulbin(tempOut, tempIn, weights, 1, nIn, nIn, nOut)
    res = tempOut(:, 1)

    call timingend(4)

end subroutine densebin

subroutine conv2dbinT(res, a, chOut, chIn, W, H, fil, fn, fm, stride, thres)
    !performs a 2d convolution, filter is binary, performs thresholding. Expects input array to be binary values
    !res - output matrix
    !a - input matrix (BINARY INPUT)
    !chOut - number of output channels
    !chIn - number of input channels
    !W - width of each channel (num of cols)
    !H - height of each channel (num of rows)
    !fil - matrix of filters (BINARY)
    !fn - number of
    !stride - stride
    !thres - thresholds
    integer, intent(in) :: chOut, chIn, W, H, a(chIn, H, W), fn, fm, fil(chOut, chIn, fn, fm), stride
    integer, intent(in) :: thres(chOut)
    integer, intent(out) :: res(chOut, (H-fn)/stride+1, (W-fm)/stride+1)
    integer i, j, l, tempsum, filmult(chIn, fn, fm)
    integer i2, j2, k2

    call timingstarts(2)

    res = res * 0 !initialise result array

    do l = 1, chOut !cycle through each channel
        do j = 1, H-fn+1, stride !slide the filter over the input
            do i = 1, W-fm+1, stride

                !perform an elementwise xnor operation between the filter and receptive field
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

                !sum all values from the result of the xnor operation above
                tempsum = 0

                do i2 = 1, fm
                    do j2 = 1, fn
                        do k2 = 1, chIn
                            tempsum = tempsum + filmult(k2, j2, i2)
                        end do
                    end do
                end do

                !check the result against the threshold value, set as 0 if below or 1 otherwise
                if (tempsum < thres(l)) then
                    res(l, (j-1)/stride+1, (i-1)/stride+1) = 0
                else
                    res(l, (j-1)/stride+1, (i-1)/stride+1) = 1
                end if
            end do
        end do
    end do

    call timingend(2)

end subroutine conv2dbinT

subroutine CNVconvT(res, a, chOut, chIn, W, H, fil, fn, fm, stride, thres)
    !performs a 2d convolution, filter is binary, performs thresholding
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
    !thres - thresholds
    integer, intent(in) :: chOut, chIn, W, H, a(chIn, H, W), fn, fm, fil(chOut, chIn, fn, fm), stride
    integer, intent(in) :: thres(chOut)
    integer, intent(out) :: res(chOut, (H-fn)/stride+1, (W-fm)/stride+1)
    integer i, j, l, tempsum, filmult(chIn, fn, fm), b(chIn, fn, fm)
    integer i2, j2, k2
    call timingstarts(1)

    res = res * 0 !initialise result array

    do l = 1, chOut !cycle through each channel
        do j = 1, H-fn+1, stride !slide the filter over the input
            do i = 1, W-fm+1, stride

                b = 2 * fil(l, :, :, :) - 1 !convert values to -1 or +1 from 0 and 1
                filmult = b * a(:, j:j+fn-1, i:i+fm-1)

                !sum all values from the result of the elementwaise multiplication operation above
                tempsum = 0

                do i2 = 1, fm
                    do j2 = 1, fn
                        do k2 = 1, chIn
                            tempsum = tempsum + filmult(k2, j2, i2)
                        end do
                    end do
                end do

                !check the result against the threshold value, set as 0 if below or 1 otherwise
                if (tempsum < thres(l)) then
                    res(l, (j-1)/stride+1, (i-1)/stride+1) = 0
                else
                    res(l, (j-1)/stride+1, (i-1)/stride+1) = 1
                end if
            end do
        end do
    end do

    call timingend(1)

end subroutine CNVconvT


!convenience functions

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
