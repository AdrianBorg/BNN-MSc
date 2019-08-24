subroutine infer(result, img, dims, f, chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
    !method to classify an image using the cifar10 CNV network
    !result - resulting classification
    !img - the input image to be classified
    !dims - the dimensions of the image (32x32 for CNV)
    !f - the square dimensions of the filter used in convolution layers (3 for CNV)
    !chOut - the number channels in the arrays between each layers
    !w1, ..., w9 - arrays for the binary wieghts, numbered for each layer
    !t1, ..., t8 - arrays fro the threshold values, numbered for each layer
    implicit none

    !result variables
    integer result, pos

    !image variables
    integer dims, img(3, dims, dims)

    !weights variables
    integer :: f, chOut(0:9)
    integer :: w1(chOut(1), chOut(0), f ,f), w2(chOut(2), chOut(1), f ,f)
    integer :: w3(chOut(3), chOut(2), f ,f), w4(chOut(4), chOut(3), f ,f)
    integer :: w5(chOut(5), chOut(4), f ,f), w6(chOut(6), chOut(5), f ,f)
    integer :: w7(chOut(6), chOut(7)), w8(chOut(7), chOut(8)), w9(chOut(8), chOut(9))
    integer :: t1(chOut(1)), t2(chOut(2)), t3(chOut(3)), t4(chOut(4)), t5(chOut(5))
    integer :: t6(chOut(6)), t7(chOut(7)), t8(chOut(8))

    !process variables
    integer, parameter :: odim(6) = [30, 28, 12, 10, 3, 1], pdim(2) = [14, 5]
    integer o1(chOut(1), odim(1), odim(1)), o2(chOut(2), odim(2), odim(2))
    integer o3(chOut(3), odim(3), odim(3)), o4(chOut(4), odim(4), odim(4))
    integer o5(chOut(5), odim(5), odim(5)), o6(chOut(6), odim(6), odim(6))
    integer o7(chOut(7)), o8(chOut(8)), o9(chOut(9))
    integer r7(chOut(7)), r8(chOut(8))
    integer p1(chOut(2), pdim(1), pdim(1)), p2(chOut(4), pdim(2), pdim(2))

    call timingstarts(6)

    !begin network

    !Conv layers
    call CNVconvT(o1, img, chOut(1), chOut(0), 32, 32,  w1, 3, 3, 1, t1)
    call conv2dbinT(o2, o1, chOut(2), chOut(1), 30, 30, w2, 3, 3, 1, t2)
    call maxpool2x23d(p1, o2, chOut(2), odim(2))
    call conv2dbinT(o3, p1, chOut(3), chOut(2), 14, 14, w3, 3, 3, 1, t3)
    call conv2dbinT(o4, o3, chOut(4), chOut(3), 12, 12, w4, 3, 3, 1, t4)
    call maxpool2x23d(p2, o4, chOut(4), odim(4))
    call conv2dbinT(o5, p2, chOut(5), chOut(4), 5, 5, w5, 3, 3, 1, t5)
    call conv2dbinT(o6, o5, chOut(6), chOut(5), 3, 3, w6, 3, 3, 1, t6)

    !Dense layers
    call densebin(r7, o6(:, 1, 1), chOut(7), chOut(6), w7)
    call thresholdLayer(o7, r7, chOut(7), 1, 1, t7)

    call densebin(r8, o7, chOut(8), chOut(7), w8)
    call thresholdLayer(o8, r8, chOut(8), 1, 1, t8)

    call densebin(o9, o8, chOut(9), chOut(8), w9)

    !extract result
    call maxpos(pos, o9, 10)
    result = pos-1

    call timingend(6)

end subroutine infer
