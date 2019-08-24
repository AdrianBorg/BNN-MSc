subroutine maxpos(res, a, n)
    !find the index of the max value in a 1d array
    !res - index of max value
    !a - array to be searched
    !n - size of the array
    integer, intent(in) :: n, a(n)
    integer, intent(out) :: res
    integer i, mx

    mx = -huge(res)

    do i = 1, n
        if (a(i) .gt. mx) then
            mx = a(i)
            res = i
        end if
    end do

end subroutine maxpos

subroutine argmax(res, a, n, m)
    !finds the indicies of the element with the maximum value in the array
    !res - ouput
    !a - input matrix
    !n - num of rows for a
    !m - num of columns for a
    integer, intent(in) :: n, m, a(n,m)
    integer :: row, col, mx
    integer, intent(out) :: res(2)

    mx = -huge(a(1,1))

    do row = 1, n
        do col = 1,n
            if (a(row, col) .gt. mx) then
                mx = a(row, col)
                res = [row, col]
            end if
        end do
    end do
end subroutine argmax

subroutine xnor(res, a, b)
    !perform xnor between 2 integers
    !res - result
    !a, b - inputs
    integer :: a, b, res

    if (a == b) then
        res = 1
    else
        res = 0
    end if
end subroutine xnor

subroutine msum3d(res, a, s)
    !sums all values in a 3D matrix
    !res - result of the usm
    !a - input 2d matrix
    !s - shape of a
    integer, intent(in) :: s(3), a(s(1), s(2), s(3))
    integer, intent(out) :: res
    integer i, j, k

    res = 0

    do k = 1, s(3)
        do j = 1, s(2)
            do i = 1, s(1)
                res = res + a(i, j, k)
            end do
        end do
    end do
end subroutine msum3d

subroutine mmulbin(res, a, b, an, am, bn, bm)
    !performs the matrix multiplication of 2 binarized matricies
    !res - ouput
    !a, b - input matricies
    !an, bn - num of rows for a and b respectively
    !am, bm - num of columns for a and b respectively
    integer :: an, am, bn, bm, row, column, digit, total, xn
    integer, intent(in) :: a(an,am), b(bn,bm)
    integer, intent(out) :: res(an, bm)

    do row = 1, an !iterate through each row
        do column = 1, bm !iterate through each column
            total = 0
            do digit = 1, am !iterate through the number in the row and columns
                call xnor(xn, a(row, digit), b(digit, column))
                total = total + xn
            end do
            !since count of the number of 0s and number of 1s is the size (number) of input
            !and the sum of all 0s and 1s is the result, knowing the size and num of 1s:
            res(row, column) = total
        end do
    end do

end subroutine mmulbin
