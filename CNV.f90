program CNV

    !testing batch variables
    integer, parameter :: npics = 100, dims = 32, colours = 3
    integer imgs(npics, colours, dims, dims)
    integer(1) labels(npics)

    !weights variables
    integer, parameter :: f = 3, chOut(0:9) = [3, 64, 64, 128, 128, 256, 256, 512, 512, 10]
    integer :: w1(chOut(1), chOut(0), f ,f), w2(chOut(2), chOut(1), f ,f)
    integer :: w3(chOut(3), chOut(2), f ,f), w4(chOut(4), chOut(3), f ,f)
    integer :: w5(chOut(5), chOut(4), f ,f), w6(chOut(6), chOut(5), f ,f)
    integer :: w7(chOut(6), chOut(7)), w8(chOut(7), chOut(8)), w9(chOut(8), chOut(9))
    integer :: t1(chOut(1)), t2(chOut(2)), t3(chOut(3)), t4(chOut(4)), t5(chOut(5))
    integer :: t6(chOut(6)), t7(chOut(7)), t8(chOut(8))

    integer result, stats(0:9), i, correct

    !begin program
    print *, '--------------------'
    print *, ''
    print *, '- Starting program -'
    print *, ''
    call cifarFileReader(imgs, labels)
    print *, '- Images   : loaded'
    call loadData(chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
    print *, '- Weights  : loaded'

    do i = 0, 9
        stats(i) = 0
    end do

    correct = 0

    do i = 1, npics
        call infer(result, imgs(i, :, :, :), dims, f, chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
        if (result == labels(i)) then
            stats(result) = stats(result) + 1
            correct = correct + 1
        end if
        if (mod(i, 1000) == 0) then
            print *, 'i = ', i, '| stats = ', stats
        end if
    end do

    call timingresults()

    print *, 'stats: ', stats
    print *, 'correct: ', correct

end program CNV

subroutine infer(result, img, dims, f, chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)

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
    integer p1(chOut(2), pdim(1), pdim(1)), p2(chOut(4), pdim(2), pdim(2))

    call timingstarts(6)

    !begin program

    !begin network
    call cconvT(o1, img, chOut(1), chOut(0), dims, w1, t1, .false.)
    call cconvT(o2, o1, chOut(2), chOut(1), odim(1), w2, t2, .true.)
    call maxpool2x23d(p1, o2, chOut(2), odim(2))
    call cconvT(o3, p1, chOut(3), chOut(2), pdim(1), w3, t3, .true.)
    call cconvT(o4, o3, chOut(4), chOut(3), odim(3), w4, t4, .true.)
    call maxpool2x23d(p2, o4, chOut(4), odim(4))
    call cconvT(o5, p2, chOut(5), chOut(4), pdim(2), w5, t5, .true.)
    call cconvT(o6, o5, chOut(6), chOut(5), odim(5), w6, t6, .true.)
    call cdensebinT(o7, o6(:, 1, 1), chOut(7), chOut(6), w7, t7)
    call cdensebinT(o8, o7, chOut(8), chOut(7), w8, t8)
    call densebin(o9, o8, chOut(9), chOut(8), w9)

    !extract result
    call maxpos(pos, o9, 10)
    result = pos-1

    call timingend(6)

end subroutine infer
