!****h* Kohonen/distance_base_utilities
!
! NAME
!  MODULE distance_base_utilities
!
! PURPOSE
!  This module defines an abstract class for distance 
!
! AUTHOR
! Oscar Garcia-Cabrejo
!$Author$
! NOTES 
!$Rev$
!$HeadURL$
! MODIFICATION HISTORY
!$LastChangedDate$
!$LastChangedRevision$
!$LastChangedBy$
!*****
module distance_base_utilities
!
implicit none
!****c* distance_base_utilities/distance_base
! NAME
!   distance_base
! PURPOSE
!   Abstract Class to represent an abstract function to calculate distance
type,abstract :: distance_base
  contains
!  
!  METHODS
!
    procedure(distance_function1),deferred :: calculate
end type distance_base
!*****
abstract interface
!****f* distance_base_utilities/distance_function1
! NAME
!   distance_function1
! PURPOSE
!   Template for Function to calculate distance 
! SYNOPSIS
!========================================================================================
  function distance_function1(distance,vector1,vector2) result(d)
!========================================================================================
    import :: distance_base
    class(distance_base) :: distance
    real(kind=8),dimension(:,:),intent(inout) :: vector1,vector2
    real(kind=8) :: d
!*****
  end function distance_function1
!
end interface

end module distance_base_utilities