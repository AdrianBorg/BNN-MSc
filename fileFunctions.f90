subroutine loadData(chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
    integer, parameter :: f = 3
    !integer, parameter :: chOut(0:9) = [3, 64, 64, 128, 128, 256, 256, 512, 512, 10]
    integer, intent(in) :: chOut(0:9)
    integer(1) :: rw1(chOut(0), f ,f, chOut(1)), rw2(chOut(1), f ,f, chOut(2))
    integer(1) :: rw3(chOut(2), f ,f, chOut(3)), rw4(chOut(3), f ,f, chOut(4))
    integer(1) :: rw5(chOut(4), f ,f, chOut(5)), rw6(chOut(5), f ,f, chOut(6))
    integer(1) :: rw7(chOut(6), chOut(7)), rw8(chOut(7), chOut(8)), rw9(chOut(8), chOut(9))
    integer :: t1(chOut(1)), t2(chOut(2)), t3(chOut(3)), t4(chOut(4)), t5(chOut(5))
    integer :: t6(chOut(6)), t7(chOut(7)), t8(chOut(8))

    integer, intent(out) :: w1(chOut(0), f ,f, chOut(1)), w2(chOut(1), f ,f, chOut(2))
    integer, intent(out) :: w3(chOut(2), f ,f, chOut(3)), w4(chOut(3), f ,f, chOut(4))
    integer, intent(out) :: w5(chOut(4), f ,f, chOut(5)), w6(chOut(5), f ,f, chOut(6))
    integer, intent(out) :: w7(chOut(6), chOut(7)), w8(chOut(7), chOut(8)), w9(chOut(8), chOut(9))

    ! check values in the line above
    integer i, o, k, h, w
    character*20 filenameW
    character*19 filenameT
    character*1 fileNum

#ifdef DO_TIMING
    call timingstarts(8)
#endif

    do k = 1, 9
        write(fileNum, '(I1)') k-1
        filenameW = 'params/weightsLayer'//fileNum
        filenameT = 'params/treshsLayer'//fileNum
        open(unit=10, file=filenameW, access='STREAM', status='old', form='unformatted')
        open(unit=25, file=filenameT, access='STREAM', status='old', form='unformatted')
        do o = 1, chOut(k)
            !read weights
            if (k < 7) then     !for each conv layer
                do w = 1, f
                    do h = 1, f
                        do i = 1, chOut(k-1)
                            if (k==1) then
                                read(10) rw1(i, h, w, o)
                                w1(i, h, w, o) = rw1(i, h, w, o)
                            else if (k==2) then
                                read(10) rw2(i, h, w, o)
                                w2(i, h, w, o) = rw2(i, h, w, o)
                            else if (k==3) then
                                read(10) rw3(i, h, w, o)
                                w3(i, h, w, o) = rw3(i, h, w, o)
                            else if (k==4) then
                                read(10) rw4(i, h, w, o)
                                w4(i, h, w, o) = rw4(i, h, w, o)
                            else if (k==5) then
                                read(10) rw5(i, h, w, o)
                                w5(i, h, w, o) = rw5(i, h, w, o)
                            else if (k==6) then
                                read(10) rw6(i, h, w, o)
                                w6(i, h, w, o) = rw6(i, h, w, o)
                            end if
                        end do
                    end do
                end do

            else    !for each full layer
                do i = 1, chOut(k-1)
                    if (k==7) then
                        read(10) rw7(i, o)
                        w7(i, o) = rw7(i, o)
                    else if (k==8) then
                        read(10) rw8(i, o)
                        w8(i, o) = rw8(i, o)
                    else if (k==9) then
                        read(10) rw9(i, o)
                        w9(i, o) = rw9(i, o)
                    end if
                end do
            end if

            !read thresholds
            if (k==1) then
                read(25) t1(o)
            else if (k==2) then
                read(25) t2(o)
            else if (k==3) then
                read(25) t3(o)
            else if (k==4) then
                read(25) t4(o)
            else if (k==5) then
                read(25) t5(o)
            else if (k==6) then
                read(25) t6(o)
            else if (k==7) then
                read(25) t7(o)
            else if (k==8) then
                read(25) t8(o)
            end if
        end do
        close(10)
        close(25)
    end do
#ifdef DO_TIMING
    call timingend(8)
#endif
end subroutine loadData

subroutine cifarFileReader1(imgs_unsigned, label)
    !method to read images from the cifar10 dataset
    !imgs_unsigned - array of images read, with pixel colour values form 0 to 255
    !              - images are of 32x32 size with 3 colour channels
    !label - array of labels for each image, corresponding to what their classification should be

    !NOTE - modify the "file" property below to change the file to read the images from

    integer, parameter :: npics = 10000, dims = 32
    integer(1) label(npics)
    integer nr, py, px, colour
    ! These are signed bytes, so between -128 and 127
    integer(1) imgs(npics,3,dims,dims)
    ! imgs_unsigned(i,j,k) = imgs(i,j,k) +128
    integer imgs_unsigned(npics,3,dims,dims)

#ifdef DO_TIMING
    call timingstarts(7)
#endif

    !modify "file" argument here to switch the file being read
    open(unit=42,file="imgBins/test_batch.bin",access='STREAM', status='old', form='unformatted')

    do nr=1,npics !for each image
        read(42) label(nr)
        do colour=1,3 !for each channel cycle thorugh each pixel
            do py=1,dims
                do px=1,dims
                    read(42) imgs(nr,colour,py,px) !read the pixel value
                    if (imgs(nr,colour,py,px) < 0) then !pixels are read as 2s complement, so convert to unsigned
                        imgs_unsigned(nr,colour,py,px) = imgs(nr,colour,py,px) + 256
                    else
                        imgs_unsigned(nr,colour,py,px) = imgs(nr,colour,py,px)
                    end if
                end do
            end do
        end do
    end do

    close(42)

#ifdef DO_TIMING
    call timingend(7)
#endif

end subroutine cifarFileReader1
