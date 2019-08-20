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
        do column = 1, am !iterate through each column
            total = 0
            do digit = 1, am !iterate through the number in the row and columns
                total = total + a(row, digit) * b(digit, column)
            end do
            res(row, column) = total
        end do
    end do

end subroutine mmul

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
            if (a(row, col) > mx) then
                mx = a(row, col)
                res = [row, col]
            end if
        end do
    end do
end subroutine argmax

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

subroutine equalmat(res, a1, a2, sh1, sh2, ndim)
    !checks if 2 matricies are equal
    !res - ouput
    !a1, a2 - input matricies
    !sh1, sh2 - shape of a1 and a2 respectively
    !ndim - num of dimensions in the matricies
    integer, intent(in) :: ndim, sh1(ndim), sh2(ndim), a1(sh1(1), sh1(2), sh1(3), sh1(4)), a2(sh2(1), sh2(2), sh2(3), sh1(4))
    logical res

    if (ndim == 1) then
        call equalmat1(res, a1, a2, sh1(1), sh2(1))
    else if (ndim == 2) then
        call equalmat2(res, a1, a2, sh1, sh2)
    else if (ndim == 3) then
        call equalmat3(res, a1, a2, sh1, sh2)
    else if (ndim == 4) then
        call equalmat4(res, a1, a2, sh1, sh2)
    end if
end subroutine

subroutine testequalintegermat(a1, a2, sh1, sh2, ndim, msg)
    !asserts that 2 matricies are equal, prints T of F if true or false respectively
    !a1, a2 - input matricies
    !sh1, sh2 - shape of a1 and a2 respectively
    !ndim - num of dimensions in the matricies
    !msg - msg to be output
    integer, intent(in) :: ndim, sh1(ndim), sh2(ndim), a1(sh1(1), sh1(2), sh1(3), sh1(4)), a2(sh2(1), sh2(2), sh2(3), sh1(4))
    logical res
    character(len=*) msg

!    print *, sh1
    call equalmat(res, a1, a2, sh1, sh2, ndim)
    print *, res, ' : ', msg
end subroutine

subroutine equalmat1(res, a1, a2, sh1, sh2)
    !checks if 2 matricies of shape (X, 1) are equal
    !res - ouput
    !a1, a2 - input matricies
    !sh1, sh2 - shape of a1 and a2 respectively
    integer, intent(in) :: sh1, sh2, a1(sh1), a2(sh2)
    logical res

    res = (size(a1) == size(a2))
    call checkequal(res, a1, a2, size(a1))
end subroutine equalmat1

subroutine equalmat2(res, a1, a2, sh1, sh2)
    !checks if 2 matricies of shape (X, Y) are equal
    !res - ouput
    !a1, a2 - input matricies
    !sh1, sh2 - shape of a1 and a2 respectively
    integer, intent(in) :: sh1(2), sh2(2), a1(sh1(1), sh1(2)), a2(sh2(1), sh2(2))
    integer b1(size(a1)), b2(size(a2))
    logical res

    b1 = reshape(a1, shape(b1))
    b2 = reshape(a2, shape(b2))
    res = (size(a1) == size(a2))
    call checkequal(res, b1, b2, size(a1))
end subroutine equalmat2

subroutine equalmat3(res, a1, a2, sh1, sh2)
    !checks if 2 matricies of shape (X, Y, Z) are equal
    !res - ouput
    !a1, a2 - input matricies
    !sh1, sh2 - shape of a1 and a2 respectively
    integer, intent(in) :: sh1(3), sh2(3), a1(sh1(1), sh1(2), sh1(3)), a2(sh2(1), sh2(2), sh2(3))
    integer b1(size(a1)), b2(size(a2))
    logical res

    b1 = reshape(a1, shape(b1))
    b2 = reshape(a2, shape(b2))

!    print *, b1
!    print *, b2
    res = (size(a1) == size(a2))
    call checkequal(res, b1, b2, size(a1))
end subroutine equalmat3

subroutine equalmat4(res, a1, a2, sh1, sh2)
    !checks if 2 matricies of shape (W, X, Y, Z) are equal
    !res - ouput
    !a1, a2 - input matricies
    !sh1, sh2 - shape of a1 and a2 respectively
    integer, intent(in) :: sh1(4), sh2(4), a1(sh1(1), sh1(2), sh1(3), sh1(4)), a2(sh2(1), sh2(2), sh2(3), sh1(4))
    integer b1(size(a1)), b2(size(a2))
    logical res

    b1 = reshape(a1, shape(b1))
    b2 = reshape(a2, shape(b2))

!    print *, b1
!    print *, b2
    res = (size(a1) == size(a2))
    call checkequal(res, b1, b2, size(a1))
end subroutine equalmat4

subroutine checkequal(res, b1, b2, s)
    !checks if 2 1d arrays are equal
    !res - output
    !b1, b2 - input matricies
    !s - size of arrays
    integer :: i, s, b1(s), b2(s)
    logical res

    if ( res ) then
        do i = 1, s
            res = b1(i) == b2(i)

            if ( .not. res ) exit
        enddo
    endif
end subroutine checkequal

subroutine checkequalreal(res, b1, b2, s)
    !checks if 2 1d arrays are equal
    !res - output
    !b1, b2 - input matricies
    !s - size of arrays
    integer :: i, s
    real :: b1(s), b2(s)
    logical res

    if ( res ) then
        do i = 1, s
            res = abs(b1(i) - b2(i)) < 0.00001
!            print *, abs(b1(i) - b2(i))
            if ( .not. res ) exit
        enddo
    endif
end subroutine checkequalreal

subroutine testequalrealmat(a1, a2, sh1, sh2, msg)
    !asserts that 2 3d matricies are equal, prints T of F if true or false respectively
    !a1, a2 - input matricies
    !sh1, sh2 - shape of a1 and a2 respectively in 3 dimensions!!!!!
    !ndim - num of dimensions in the matricies
    !msg - msg to be output
    integer, intent(in) :: sh1(3), sh2(3)
    real, intent(in) :: a1(sh1(1), sh1(2), sh1(3)), a2(sh2(1), sh2(2), sh2(3))
    real b1(size(a1)), b2(size(a2))
    logical res
    character(len=*) msg

    b1 = reshape(a1, shape(b1))
    b2 = reshape(a2, shape(b2))

    res = (size(a1) == size(a2))

    call checkequalreal(res, b1, b2, size(a1))
    print *, res, ' : ', msg
end subroutine testequalrealmat
