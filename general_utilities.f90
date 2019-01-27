MODULE general_utilities
! 
IMPLICIT NONE
!
private
!
interface sort
  module procedure quick_sort
end interface
!
interface maximum
  module procedure  maximum_only,maximum_position
end interface 
!
public :: sort,minimum,maximum,mean,variance,std
public :: coefficient_of_variation
!
 CONTAINS
!=========================================================
RECURSIVE SUBROUTINE quick_sort(list, order)
!==========================================================
! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which gives
! the positions of the elements in the original order.
!===========================================================
IMPLICIT NONE
REAL(KIND=8), DIMENSION (:), INTENT(INOUT)  :: list
INTEGER, DIMENSION (:), INTENT(OUT)  :: order

! Local variable
INTEGER :: i
!write(*,*) SIZE(list)
DO i = 1, SIZE(list)
  order(i) = i
END DO

CALL quick_sort_1(1, SIZE(list))

CONTAINS

RECURSIVE SUBROUTINE quick_sort_1(left_end, right_end)

INTEGER, INTENT(IN) :: left_end, right_end

!     Local variables
INTEGER             :: i, j, itemp
REAL(KIND=8)        :: reference, temp
INTEGER, PARAMETER  :: max_simple_sort_size = 6

IF (right_end < left_end + max_simple_sort_size) THEN
  ! Use interchange sort for small lists
  CALL interchange_sort(left_end, right_end)

ELSE
  ! Use partition ("quick") sort
  reference = list((left_end + right_end)/2)
  i = left_end - 1; j = right_end + 1

  DO
    ! Scan list from left end until element >= reference is found
    DO
      i = i + 1
      IF (list(i) >= reference) EXIT
    END DO
    ! Scan list from right end until element <= reference is found
    DO
      j = j - 1
      IF (list(j) <= reference) EXIT
    END DO


    IF (i < j) THEN
      ! Swap two out-of-order elements
      temp = list(i); list(i) = list(j); list(j) = temp
      itemp = order(i); order(i) = order(j); order(j) = itemp
    ELSE IF (i == j) THEN
      i = i + 1
      EXIT
    ELSE
      EXIT
    END IF
  END DO

  IF (left_end < j) CALL quick_sort_1(left_end, j)
  IF (i < right_end) CALL quick_sort_1(i, right_end)
END IF

END SUBROUTINE quick_sort_1


SUBROUTINE interchange_sort(left_end, right_end)

INTEGER, INTENT(IN) :: left_end, right_end

!     Local variables
INTEGER             :: i, j, itemp
REAL(KIND=8)        :: temp

DO i = left_end, right_end - 1
  DO j = i+1, right_end
    IF (list(i) > list(j)) THEN
      temp = list(i); list(i) = list(j); list(j) = temp
      itemp = order(i); order(i) = order(j); order(j) = itemp
    END IF
  END DO
END DO

END SUBROUTINE interchange_sort
!
END SUBROUTINE quick_sort
!===================================================================================================
FUNCTION minimum(a) RESULT(mn)
!===================================================================================================
REAL(KIND=8),DIMENSION(:) :: a
REAL(KIND=8) :: mn
!
INTEGER :: i,number_elements
mn=a(1)
number_elements=size(a)
do i=2,number_elements
   if(a(i) .lt. mn) then
     mn=a(i)
   endif
enddo
!
END FUNCTION minimum
!===================================================================================================
SUBROUTINE maximum_only(a,mx)
!===================================================================================================
REAL(KIND=8),DIMENSION(:) :: a
REAL(KIND=8) :: mx
!
INTEGER :: i,number_elements
mx=a(1)
number_elements=size(a)
do i=2,number_elements
   if(a(i) .gt. mx) then
     mx=a(i)
   endif
enddo
!
END SUBROUTINE maximum_only
!=============================================================================================
 subroutine maximum_position(a,mx,p)
!=============================================================================================
 REAL(KIND=8),DIMENSION(:) :: a
 REAL(KIND=8) :: mx
 integer :: p
!
 INTEGER :: i,number_elements
 mx=a(1)
 number_elements=size(a)
 do i=2,number_elements
    if(a(i) .gt. mx) then
      mx=a(i);p=i;
    endif
 enddo
!
 end subroutine maximum_position
!=============================================================================================
function mean(a) result(mn)
!=============================================================================================
real(kind=8),dimension(:),intent(inout) :: a
real(kind=8) :: mn
!
mn=sum(a)/float(size(a));
!
end function mean
!=============================================================================================
function variance(a) result(v)
!=============================================================================================
real(kind=8),dimension(:),intent(inout) :: a
real(kind=8) :: v
!
real(kind=8),dimension(size(a)) :: a2
!
a2=a**2;
v=mean(a2)-(mean(a))**2;
!
end function variance
!=============================================================================================
function std(a) result(s)
!=============================================================================================
real(kind=8),dimension(:),intent(inout) :: a
real(kind=8) :: s
!
real(kind=8) :: v
!
v=variance(a);
s=sqrt(v);
!
end function std
!============================================================================================
function coefficient_of_variation(a) result(cov)
!============================================================================================
real(kind=8),dimension(:),intent(inout) :: a
real(kind=8) :: cov
!
 cov=std(a)/mean(a);

end function coefficient_of_variation
!
END MODULE general_utilities
