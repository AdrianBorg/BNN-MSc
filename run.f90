program run
    implicit none

    integer, parameter :: pics2run = 10000

    !testing batch variables
    integer, parameter :: npics = 10000, dims = 32, colours = 3
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

    ! call runtests()

    !begin program
    print *, '--------------------'
    print *, ''
    print *, '- Starting program -'
    print *, ''
    call cifarFileReader1(imgs, labels)
    print *, '- Images   : loaded'
    call loadData(chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
    print *, '- Weights  : loaded'

    do i = 0, 9
        stats(i) = 0
    end do

    correct = 0

    do i = 1, pics2run
        call infer(result, imgs(i, :, :, :), dims, f, chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
        if (result == labels(i)) then
            stats(result) = stats(result) + 1
            correct = correct + 1
        end if
        if (i == 100) then
            print *, 'i = ', i, '| stats = ', stats
            call timingresults()
        end if
        if (mod(i, 1000) == 0) then
            print *, 'i = ', i, '| stats = ', stats
            call timingresults()
        end if
    end do

    call timingresults()

    print *, '- Results -'
    print '(10X, 10A12)', 'airplane', 'car', 'bird', 'cat', 'deer', 'dog', 'frog', 'horse', 'ship', 'truck'
    print '(A10, 10I12)', 'stats: ', stats
    print *, 'correct: ', correct
end program run
