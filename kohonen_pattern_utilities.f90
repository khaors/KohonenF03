!****h* Kohonen/kohonen_pattern_utilities
!
! NAME
!  MODULE kohonen_pattern_utilities
!
! PURPOSE
!  This module defines a class for kohonen patterns (input data) 
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
module kohonen_pattern_utilities
!
use kohonen_prototype_utilities;
!
implicit none
!****c* kohonen_pattern_utilities/kohonen_pattern
! NAME
!   kohonen_pattern
! PURPOSE
!   Class to represent a container for input data to  a kohonen map
type kohonen_pattern
!
!  ATTRIBUTES
!
  private
    type(kohonen_prototype) :: pattern
    character(len=50) :: pattern_name 
  contains
!  
!  METHODS
!   
    procedure,public :: create => kohonen_pattern_create
    procedure,public :: destroy => kohonen_pattern_destroy
    procedure,public :: get => kohonen_pattern_accessor
    procedure,public :: set => kohonen_pattern_mutator
    procedure,public :: print => kohonen_pattern_print
    procedure,public :: get_nrow => kohonen_pattern_nrow
    procedure,public :: get_ncol => kohonen_pattern_ncol
!
end type kohonen_pattern
!*****
 contains
!****f* kohonen_pattern_utilities/kohonen_pattern_create
! NAME
!   kohonen_pattern_create
! PURPOSE
!   Kohonen pattern constructor
! SYNOPSIS
!========================================================================================
 subroutine kohonen_pattern_create(current_pattern,input,name)
!========================================================================================
   class(kohonen_pattern) :: current_pattern
   real(kind=8),dimension(:,:),intent(inout) :: input
   character(len=*),optional :: name
!*****   
   call current_pattern%pattern%create(input);
   if(present(name)) then
     current_pattern%pattern_name=trim(name);
   else
     current_pattern%pattern_name="";
   endif
!   
 end subroutine kohonen_pattern_create
!****f* kohonen_pattern_utilities/kohonen_pattern_destroy
! NAME
!   kohonen_pattern_destroy
! PURPOSE
!   Kohonen pattern destructor
! SYNOPSIS
!========================================================================================
 subroutine kohonen_pattern_destroy(current_pattern)
!========================================================================================
   class(kohonen_pattern) :: current_pattern
!*****
   call current_pattern%pattern%destroy();
!   
 end subroutine kohonen_pattern_destroy
!****f* kohonen_pattern_utilities/kohonen_pattern_accessor
! NAME
!   kohonen_pattern_accessor
! PURPOSE
!   Kohonen pattern accessor
! SYNOPSIS
!========================================================================================
 subroutine kohonen_pattern_accessor(current_pattern,pattern_value)
!========================================================================================
   class(kohonen_pattern) :: current_pattern
   type(kohonen_prototype),intent(inout) :: pattern_value
!*****
   pattern_value=current_pattern%pattern;
!
 end subroutine kohonen_pattern_accessor
!****f* kohonen_pattern_utilities/kohonen_pattern_mutator
! NAME
!   kohonen_pattern_mutator
! PURPOSE
!   Kohonen pattern mutator
! SYNOPSIS
!========================================================================================
 subroutine kohonen_pattern_mutator(current_pattern,pattern_value)
!========================================================================================
   class(kohonen_pattern) :: current_pattern
   type(kohonen_prototype),intent(inout) :: pattern_value
!*****
   current_pattern%pattern=pattern_value;
!
 end subroutine kohonen_pattern_mutator
!****f* kohonen_pattern_utilities/kohonen_pattern_print
! NAME
!   kohonen_pattern_print
! PURPOSE
!   Function to print a Kohonen pattern 
! SYNOPSIS
!========================================================================================
 subroutine kohonen_pattern_print(current_pattern,unit_)
!========================================================================================
   class(kohonen_pattern) :: current_pattern
   integer,intent(inout),optional :: unit_
!*****
   if(present(unit_)) then
     write(unit_,*)
     write(unit_,*) 'PATTERN: ',trim(current_pattern%pattern_name);
     write(unit_,*)
     call current_pattern%pattern%print(unit_);
   else
     write(*,*)
     write(*,*) 'PATTERN: ',trim(current_pattern%pattern_name);
     write(*,*) 
     call current_pattern%pattern%print();
   endif
 !  
 end subroutine kohonen_pattern_print
!
 function kohonen_pattern_nrow(current_pattern) result(nr)
   class(kohonen_pattern) :: current_pattern
   integer :: nr
!
   nr=current_pattern%pattern%get_nrow();
!
 end function kohonen_pattern_nrow
!
 function kohonen_pattern_ncol(current_pattern) result(nc)
   class(kohonen_pattern) :: current_pattern
   integer :: nc
!
   nc=current_pattern%pattern%get_ncol();
!
 end function kohonen_pattern_ncol

end module kohonen_pattern_utilities