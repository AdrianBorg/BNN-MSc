subroutine testequalintegermat(a1, a2, sh1, sh2, ndim, msg)
    !asserts that 2 matricies are equal, prints T of F if true or false respectively
    !a1, a2 - input matricies
    !sh1, sh2 - shape of a1 and a2 respectively
    !ndim - num of dimensions in the matricies
    !msg - msg to be output
    integer, intent(in) :: ndim, sh1(ndim), sh2(ndim), a1(sh1(1), sh1(2), sh1(3), sh1(4)), a2(sh2(1), sh2(2), sh2(3), sh1(4))
    logical res
    character(len=*) msg

    call equalmat(res, a1, a2, sh1, sh2, ndim)
    print *, res, ' : ', msg
end subroutine

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
            if ( .not. res ) exit
        enddo
    endif
end subroutine checkequalreal
