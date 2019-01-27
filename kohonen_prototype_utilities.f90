!****h* Kohonen/kohonen_prototype_utilities
!
! NAME
!  MODULE kohonen_pattern_utilities
!
! PURPOSE
!  This module defines a class for kohonen prototype (units inside kohonen layers) 
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
module kohonen_prototype_utilities
!
use distance_base_utilities;
!
implicit none
!****c* kohonen_prototype_utilities/kohonen_prototype
! NAME
!   kohonen_prototype
! PURPOSE
!   Class to store a prototype inside a Kohonen map
type kohonen_prototype
  private
  real(kind=8),allocatable :: data_(:,:)
  integer :: number_rows,number_columns
!  
!  METHODS
!
  contains
!
       procedure :: create => kohonen_prototype_constructor
       procedure :: destroy => kohonen_prototype_destructor
       procedure :: get_prototype => kohonen_prototype_accessor
       procedure :: set_prototype => kohonen_prototype_mutator
       procedure :: print => kohonen_prototype_print
       procedure :: distance => kohonen_prototype_distance
       procedure :: get_nrow => kohonen_prototype_nrow
       procedure :: get_ncol => kohonen_prototype_ncol
!
end type kohonen_prototype
!*****
 contains
!****f* kohonen_prototype_utilities/kohonen_prototype_constructor
!  NAME
!    kohonen_prototype_constructor
!  PURPOSE
!    Constructor
!  SYNOPSIS
!========================================================================================
 subroutine kohonen_prototype_constructor(prototype,input_data)
!========================================================================================
   class(kohonen_prototype) :: prototype
   real(kind=8),dimension(:,:) :: input_data
!*****
   integer :: ierr
!
   prototype%number_rows=size(input_data,1);
   prototype%number_columns=size(input_data,2);
   allocate(prototype%data_(prototype%number_rows,prototype%number_columns),stat=ierr)
   prototype%data_=input_data;
!
 end subroutine kohonen_prototype_constructor
!****f* kohonen_prototype_utilities/kohonen_prototype_destructor
!  NAME
!    kohonen_prototype_destructor
!  PURPOSE
!    Destructor
!  SYNOPSIS
!========================================================================================
 subroutine kohonen_prototype_destructor(prototype)
!========================================================================================
   class(kohonen_prototype),intent(inout) :: prototype
!*****
   if(allocated(prototype%data_)) then
!     write(*,*) 'Prototype, ',allocated(prototype%data_),size(prototype%data_,1),size(prototype%data_,2)
     deallocate(prototype%data_);
!     write(*,*) 'Prototype release'
   endif
 end subroutine kohonen_prototype_destructor
!****f* kohonen_prototype_utilities/kohonen_prototype_accessor
!  NAME
!    kohonen_prototype_accessor
!  PURPOSE
!    Acccessor
!  SYNOPSIS
!========================================================================================
 subroutine kohonen_prototype_accessor(prototype,d)
!========================================================================================
   class(kohonen_prototype) :: prototype
   real(kind=8),dimension(prototype%number_rows,prototype%number_columns) :: d
!*****
   if(allocated(prototype%data_)) then
!     write(*,*) 'ACCESOR'
     d=prototype%data_;
   else 
     stop
   endif
!   
 end subroutine kohonen_prototype_accessor
!****f* kohonen_prototype_utilities/kohonen_prototype_mutator
!  NAME
!    kohonen_prototype_mutator
!  PURPOSE
!    Mutator
!  SYNOPSIS
!========================================================================================
 subroutine kohonen_prototype_mutator(prototype,new_data)
!========================================================================================
   class(kohonen_prototype) :: prototype
   real(kind=8),dimension(:,:),intent(inout) :: new_data
!*****
   if( (size(new_data,1) .eq. prototype%number_rows) .and. (size(new_data,2) .eq. prototype%number_columns) ) then
     prototype%data_=new_data
   endif
!
 end subroutine kohonen_prototype_mutator
 !****f* kohonen_prototype_utilities/kohonen_prototype_print
!  NAME
!    kohonen_prototype_print
!  PURPOSE
!    Function to print a kohonen prototype
!  SYNOPSIS
!========================================================================================
 subroutine kohonen_prototype_print(prototype,unit_)
!========================================================================================
 class(kohonen_prototype) :: prototype
 integer,intent(inout),optional :: unit_
!*****
 integer :: ix,iy
!
 if(present(unit_)) then
   write(unit_,*) 'Prototype'
   if(size(prototype%data_,2) .ne. 1) then
   do ix=1,size(prototype%data_,1)
      write(unit_,*) (prototype%data_(ix,iy),iy=1,size(prototype%data_,2))
   enddo!ix
   else
      write(unit_,*) (prototype%data_(ix,1),ix=1,size(prototype%data_,1))
   endif
 else
   write(*,*) 'Prototype',size(prototype%data_,1),size(prototype%data_,2)
   do ix=1,size(prototype%data_,1)
      write(*,*) (prototype%data_(ix,iy),iy=1,size(prototype%data_,2))
   enddo!ix
 endif
!
 end subroutine kohonen_prototype_print
!========================================================================================
 function kohonen_prototype_distance(prototype,prototype1,f) result(d)
!========================================================================================
   class(kohonen_prototype) :: prototype
   type(kohonen_prototype) :: prototype1
   class(distance_base),allocatable :: f
   real(kind=8)  :: d
!*****
   real(kind=8),dimension(prototype%number_rows,prototype%number_columns) :: prot1,prot2
   d=0.0d0
   call prototype%get_prototype(prot1);
   call prototype1%get_prototype(prot2);
   d=f%calculate(prot1,prot2);
!
 end function kohonen_prototype_distance
!========================================================================================
 function kohonen_prototype_nrow(prototype) result(nr)
!========================================================================================
   class(kohonen_prototype) :: prototype
   integer :: nr
!
   nr=size(prototype%data_,1);
!
 end function kohonen_prototype_nrow
!========================================================================================
 function kohonen_prototype_ncol(prototype) result(nc)
!========================================================================================
   class(kohonen_prototype) :: prototype
   integer :: nc
!
   nc=size(prototype%data_,2);
!
 end function kohonen_prototype_ncol

end module kohonen_prototype_utilities