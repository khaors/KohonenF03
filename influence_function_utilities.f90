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
use mtmod;
use general_utilities;
use quicksort_utilities;
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
    procedure,public :: calculate_sigma1
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
    case('')
         v=r;
    case default
         v=r;
!
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
!========================================================================================
    function calculate_sigma1(my_function,input_data,M,seed) result(sigma)
!========================================================================================
      class(influence_function) :: my_function
      real(kind=8),dimension(:,:),intent(inout) :: input_data
      integer,intent(inout) :: M
      integer,intent(inout),optional :: seed 
      real(kind=8) :: sigma
!
      real(kind=8),dimension(size(input_data,1)-M,size(input_data,2)) :: input_data1
      real(kind=8),dimension(M,size(input_data,2)) :: p_vector
      real(kind=8),dimension(size(input_data,1)) :: sample_pos,current_min
      real(kind=8),dimension(size(input_data,1)-M,M) :: sigma_table
      real(kind=8),dimension(M) :: current_sigma
      integer,dimension(size(input_data,1)) :: sample_index
      type(quicksort) :: qsort
      integer ::ndat,nvar,seed1,i,j,median_pos
!
      if(.not. present(seed)) then 
        seed1=12345;
      else 
        seed1=seed;
      endif
   !
      ndat=size(input_data,1);
      nvar=size(input_data,2);
    ! 
      call sgrnd(seed1);
      call grnd_array(sample_pos);
      do i=1,size(sample_index);
         sample_index(i)=i;
      enddo
      call qsort%sort(sample_pos,sample_index);
      p_vector(1:M,1:nvar)=input_data(sample_index(1:M),1:nvar);
      write(*,*) 'p_vector= ',size(p_vector,1),size(p_vector,2);
      input_data1(1:(ndat-M),1:nvar)=input_data(sample_index((M+1):ndat),1:nvar)
      write(*,*) 'input_data1= ',size(input_data1,1),size(input_data1,2);
      write(*,*) 'sigma_table= ',size(sigma_table,1),size(sigma_table,2);
!
      do i=1,size(sigma_table,1);
          do j=1,size(sigma_table,2);
              sigma_table(i,j)=sum((input_data1(i,:)-p_vector(j,:))**2); 
          enddo
      enddo
      do i=1,size(sigma_table,1);
          current_sigma=sigma_table(i,:);
          !write(*,*) i,current_sigma(1:M);
          current_min(i)=minimum(current_sigma);
          write(*,*) i,current_min(i);
      enddo
      do i=1,size(sample_index);
          sample_index(i)=i;
      enddo
      call qsort%sort(current_min,sample_index);
      median_pos=int(M/2);
      sigma=current_min(median_pos);
      write(*,*) 'sigma= ',sigma;
!
    end function calculate_sigma1
!  
end module influence_function_utilities