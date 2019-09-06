program run
    !This program classifies images using the CNV binarized network
    !Modify the pics2run variable to change the amount of images to calssify during the run
    implicit none

    integer, parameter :: pics2run = 10000

    !testing batch variables
    integer, parameter :: npics = 10000, dims = 32, colours = 3
    integer imgs(npics, colours, dims, dims)
    integer(1) labels(npics)

    !weights variables
    integer, parameter :: f = 3, chOut(0:9) = [3, 64, 64, 128, 128, 256, 256, 512, 512, 10]
    integer :: w1(chOut(0), f ,f, chOut(1)), w2(chOut(1), f ,f, chOut(2))
    integer :: w3(chOut(2), f ,f, chOut(3)), w4(chOut(3), f ,f, chOut(4))
    integer :: w5(chOut(4), f ,f, chOut(5)), w6(chOut(5), f ,f, chOut(6))

    integer :: w7(chOut(6), chOut(7)), w8(chOut(7), chOut(8)), w9(chOut(8), chOut(9))
    integer :: t1(chOut(1)), t2(chOut(2)), t3(chOut(3)), t4(chOut(4)), t5(chOut(5))
    integer :: t6(chOut(6)), t7(chOut(7)), t8(chOut(8))

    !results variables
    integer result, stats(0:9), i, correct, distribution(0:9, 0:9)

    !begin program
    print *, '--------------------'
    print *, ''
    print *, '- Starting program -'
    print *, ''
    !load the images
    call cifarFileReader1(imgs, labels)
    print *, '- Images   : loaded'
    !load the wieghts
!    call loadData(chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
    call loadData(chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
    print *, '- Weights  : loaded'

    !initialise variables to 0
    do i = 0, 9
        stats(i) = 0
    end do
    distribution = distribution*0
    correct = 0

    !cycle through the pictures
    do i = 1, pics2run
        call infer(result, imgs(i, :, :, :), dims, f, chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)

        distribution(labels(i), result) = distribution(labels(i), result) + 1
        if (result == labels(i)) then !if a correct match is made save the result
            stats(result) = stats(result) + 1
            correct = correct + 1
        end if

        if (i == 100) then !print progress at 100 pics, then at every multiple of 1000
            print *, 'i = ', i, '| stats = ', stats
#ifdef DO_TIMING
            call timingresults()
#endif
        else if (mod(i, 1000) == 0) then
            print *, 'i = ', i, '| stats = ', stats
#ifdef DO_TIMING
            call timingresults()
#endif
        end if
    end do

    !print results of the timings
#ifdef DO_TIMING
    call timingresults()
#endif

    !print results of the cassifications
    print *, '- Results -'
    print '(10X, 10A12)', 'airplane', 'car', 'bird', 'cat', 'deer', 'dog', 'frog', 'horse', 'ship', 'truck'
    print '(A10, 10I12)', 'stats: ', stats
    print *, 'correct: ', correct
    print *, ''
    print *, '- Distribution -'
    print '(10X, 10A12)', 'airplane', 'car', 'bird', 'cat', 'deer', 'dog', 'frog', 'horse', 'ship', 'truck'
    print '(A10, 10I12)', 'airplane: ', distribution(0, :)
    print '(A10, 10I12)', 'car: ', distribution(1, :)
    print '(A10, 10I12)', 'bird: ', distribution(2, :)
    print '(A10, 10I12)', 'cat: ', distribution(3, :)
    print '(A10, 10I12)', 'deer: ', distribution(4, :)
    print '(A10, 10I12)', 'dog: ', distribution(5, :)
    print '(A10, 10I12)', 'frog: ', distribution(6, :)
    print '(A10, 10I12)', 'horse: ', distribution(7, :)
    print '(A10, 10I12)', 'ship: ', distribution(8, :)
    print '(A10, 10I12)', 'truck: ', distribution(9, :)
end program run
