subroutine sqmmul(res, a, b, n, m)
    !performs the marix multiplication of 2 square matricies
    !res - ouput
    !a, b - input matricies
    !n - num of rows for a and b
    !m - num of columns for a and b
    integer :: n, m, row, column, digit, total
    integer, intent(in) :: a(n,m), b(n,m)
    integer, intent(out) :: res(n, m)

    do row = 1, n
        do column = 1, m
            total = 0
            do digit = 1, m
                total = total + a(row, digit) * b(digit, column)
            end do
            res(row, column) = total
        end do
    end do

end subroutine sqmmul

subroutine printmat(a, sh)
    !prints an array in a nice format
    !res - ouput
    !a - input matricies
    !sh - shape of a
    integer :: sh(2), n, m, a(sh(1),sh(2)), i, j
    n = sh(1)
    m = sh(2)
    do, i=1,m
        print *, ( a(i,j), j=1,n )
    enddo
end subroutine printmat

subroutine elemMultBin(res, fil, a, l, n, m)
    !calculates the dot product assuming fil is binary
    !res - output
    !fil - binary weights
    !a - input
    !l, n, m - num of channels, rows, columns respectively
    integer, intent(in) :: l, n, m, a(l, n, m), fil(l, n, m)
    integer, intent(out) :: res(l, n, m)
    integer i, j, k

    do k = 1, l
        do j = 1, n
            do i = 1, m
                if (fil(k, j, i) == 0) then
                    res(k, j, i) = -a(k, j, i)
                else
                    res(k, j, i) = a(k, j, i)
                end if
            end do
        end do
    end do

end subroutine elemMultBin

subroutine sumPopcount(res, a, s)
    integer, intent(in) :: s(3), a(s(1), s(2), s(3))
    integer, intent(out) :: res
    integer popcount, i, j, k

    popcount = 0

    do k = 1, s(1)
        do j = 1, s(2)
            do i = 1, s(3)
                popcount = popcount + a(k, j, i)
            end do
        end do
    end do

    !since count of the number of 0s and number of 1s is the size of input
    !and the sum of all 0s and 1s is the result, knowing the size and num of 1s:
    res = -size(a) + popcount + popcount

! reasoning:
!    size = y0 + y1
!    sum = y1 - y0
!
!    y0 = size - y1
!    sum = y1 - size + y1


end subroutine sumPopcount

subroutine elemwisexnor3d(res, a, b, l, n, m)
    integer, intent(in) :: n, m, a(l, n, m), b(l, n, m)
    integer, intent(out) :: res(l, n, m)
    integer i, j, k

!    call timingstarts(9)
    do i = 1, m
        do j = 1, n
            do k = 1, l
                !call xnor(res(k, j, i), a(k, j, i), b(k, j, i))
                if (a(k, j, i) == b(k, j, i)) then
                    res(k, j, i) = 1
                else
                    res(k, j, i) = 0
                end if
            end do
        end do
    end do
!    call timingend(9)
end subroutine elemwisexnor3d

subroutine mmul(res, a, b, an, am, bn, bm)
    !performs the matrix multiplication of 2 matricies
    !res - ouput
    !a, b - input matricies
    !an, bn - num of rows for a and b respectively
    !am, bm - num of columns for a and b respectively
    integer :: an, am, bn, bm, row, column, digit, total
    integer, intent(in) :: a(an,am), b(bn,bm)
    integer, intent(out) :: res(an, bm)

    if (am /= bn) then
        print *, 'Warning, matricies cannot be multiplied'
        print *, 'a = ', a, 'shape =', shape(a)
        print *, 'b = ', b, 'shape =', shape(b)
    end if

    do row = 1, an !iterate through each row
        do column = 1, bm !iterate through each column
            total = 0
            do digit = 1, am !iterate through the number in the row and columns
                total = total + a(row, digit) * b(digit, column)
            end do
            res(row, column) = total
        end do
    end do

end subroutine mmul
