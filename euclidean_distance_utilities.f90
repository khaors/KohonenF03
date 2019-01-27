!****h* Kohonen/euclidean_distance_utilities
!
! NAME
!  MODULE euclidean_distance_utilities
!
! PURPOSE
!  This module defines a class to calculate the Euclidean distance between kohonen prototypes 
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
module euclidean_distance_utilities
!
use distance_base_utilities;
!
implicit none
!****c* euclidean_distance_utilities/euclidean_distance
! NAME
!   euclidean_distance
! PURPOSE
!   Class to calculate the euclidean distance 
type,extends(distance_base) :: euclidean_distance 
 contains
!
! METHODS
!
   procedure,public :: calculate => calculate_euclidean_distance
!*****
end type euclidean_distance
!
 contains
!****f* euclidean_distance_utilities/calculate_euclidean_distance
! NAME
!   calculate_euclidean_distance
! PURPOSE
!   Function to calculate euclidean distance between vectors 
! SYNOPSIS
!========================================================================================
 function calculate_euclidean_distance(distance,vector1,vector2) result(d)
!========================================================================================
   class(euclidean_distance) :: distance
   real(kind=8),dimension(:,:),intent(inout) :: vector1,vector2
   real(kind=8) :: d
!*****
   d=sum((vector1-vector2)**2);
!
 end function calculate_euclidean_distance
!
end module euclidean_distance_utilities