!****h* Kohonen/influence_function_utilities
!
! NAME
!  MODULE influence_function_utilities
!
! PURPOSE
!  This module defines a class to calculate the influence functions required in Robust SOM 
!
! AUTHOR
! Oscar Garcia-Cabrejo
module influence_function_utilities
!
implicit none
!*****
!
!****c* influence_function_utilities/influence_function
! NAME
!   influence_function
! PURPOSE
!   Class that represents an influence function
type :: influence_function
!
! METHODS
!
  contains
    procedure,public :: calculate => calculate_influence_function
!*****    
end type influence_function
!
contains
!****f* influence_function_utilities/calculate_influence_function
! NAME
!   calculate_influence_function
! PURPOSE
!   Calculates the influence function
! SYNOPSIS
!========================================================================================
  function calculate_influence_function(my_function,type_,r) result(v)
!========================================================================================
  class(influence_function) :: my_function
  character(len=*) :: type_ 
  real(kind=8),intent(inout) :: r 
  real(kind=8) :: v
!*****
  real(kind=8) :: c,den
!
  select case(trim(type_))
    case('L2')
        v=r;
    case('L1')
        v=sgn(r);
    case('L1-L2')
        den=dsqrt(1.0d0+0.5d0*r**2);
        v=r/den;
    case('Lp')
        c=1.2d0;
        v=sgn(r)*dabs(r)**(c-1)
    case('Fair')
        c=1.3998d0;
        den=1.0d0+dabs(r)/c;
        v=r/den;
    case('Huber')
        c=1.345d0;
    	if(dabs(r) .le. c) then 
           v=r;
        else
            v=c*sgn(r);
    	endif
    case('Cauchy')
        c=2.3849;
        v=r/(1.0d0+(r/c)**2);
    case('German-McClure')
        v=r/(1.0d0+r**2)**2;
    case('Welsch')
        c=2.9846d0;
        v=r*dexp(-(r/c)**2);
    case('Tukey')
        c=4.6851d0;
    	if(dabs(r) .le. c) then 
            v=r*(1.0d0-(r/c)**2)**2
        else
            v=0.0d0;
    	endif
  end select
!  
  end function calculate_influence_function
!****f* influence_function_utilities/sgn
! NAME
!   sgn
! PURPOSE
!   Sign function
! SYNOPSIS 
!========================================================================================
  function sgn(x) result(v)
!======================================================================================== 
  real(kind=8),intent(inout) :: x
  real(kind=8) :: v
!*****
  if(x < 0.0d0) then 
     v=-1.0d0;
  else
     v=1.0d0;
  endif
!  
  end function sgn
!  
end module influence_function_utilities