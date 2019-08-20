subroutine testfileloading()
    integer, parameter :: f = 3, in = 3, out = 64
    character*13, parameter :: filename = 'weightsLayer0'
    integer i, iostatus
    integer(1) j

    open(unit=10, file=filename, access='stream', status='old')

    do i = 1, f*f
        read(10, IOSTAT=iostatus) j
        print *, iostatus, j
    end do

end subroutine testfileloading

subroutine loadData(chOut, w1, w2, w3, w4, w5, w6, w7, w8, w9, t1, t2, t3, t4, t5, t6, t7, t8)
    integer, parameter :: f = 3
    !integer, parameter :: chOut(0:9) = [3, 64, 64, 128, 128, 256, 256, 512, 512, 10]
    integer, intent(in) :: chOut(0:9)
    integer(1), intent(out) :: w1(chOut(1), chOut(0), f ,f), w2(chOut(2), chOut(1), f ,f)
    integer(1), intent(out) :: w3(chOut(3), chOut(2), f ,f), w4(chOut(4), chOut(3), f ,f)
    integer(1), intent(out) :: w5(chOut(5), chOut(4), f ,f), w6(chOut(6), chOut(5), f ,f)
    integer(1), intent(out) :: w7(chOut(6), chOut(7)), w8(chOut(7), chOut(8)), w9(chOut(8), chOut(9))
    integer(4), intent(out) :: t1(chOut(1)), t2(chOut(2)), t3(chOut(3)), t4(chOut(4)), t5(chOut(5))
    integer(4), intent(out) :: t6(chOut(6)), t7(chOut(7)), t8(chOut(8))
    ! check values in the line above
    integer i, o, k, h, w
    character*13 filenameW
    character*12 filenameT
    character*1 fileNum

    do k = 1, 9
        write(fileNum, '(I1)') k-1
        filenameW = 'weightsLayer'//fileNum
        filenameT = 'treshsLayer'//fileNum
        open(unit=10, file=filenameW, access='STREAM', form='unformatted')
        open(unit=25, file=filenameT, access='STREAM', form='unformatted')
        do o = 1, chOut(k)
            !read weights
            if (k < 7) then     !for each conv layer
                do w = 1, f
                    do h = 1, f
                        do i = 1, chOut(k-1)
                            if (k==1) then
                                read(10) w1(o, i, h, w)
                            else if (k==2) then
                                read(10) w2(o, i, h, w)
                            else if (k==3) then
                                read(10) w3(o, i, h, w)
                            else if (k==4) then
                                read(10) w4(o, i, h, w)
                            else if (k==5) then
                                read(10) w5(o, i, h, w)
                            else if (k==6) then
                                read(10) w6(o, i, h, w)
                            end if
                        end do
                    end do
                end do

            else    !for each full layer
                do i = 1, chOut(k-1)
                    if (k==7) then
                        read(10) w7(i, o)
                    else if (k==8) then
                        read(10) w8(i, o)
                    else if (k==9) then
                        read(10) w9(i, o)
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

end subroutine loadData

subroutine cifarReader(imgs)
    integer(1) label, zero2, dtype, ndims
    integer nr, pixel, colour, row, col
    ! These are signed bytes, so between -128 and 127
    integer(1) imgs(1024,3,10000)
    ! imgs_unsigned(i,j,k) = imgs(i,j,k) +128
    integer imgs_unsigned(1024,3,10000)

    label=42 ! should 0-9

    open(unit=42,file="train-images-idx3-ubyte",access='STREAM')

    do nr=1,10000
        read(42) label
        do colour=1,3
            do pixel=1,1024
                read(42) imgs(pixel,colour,nr)
            end do
        end do
    end do

    close(42)


    do nr=1,10
        do row=1,32
            print *, (imgs((row-1)*32+col,1,nr),col=1,32) ! colour channel 1
        end do
    end do

end subroutine cifarReader
