!****h* Kohonen/factory_distance_utilities
!
! NAME
!  MODULE factory_distance_utilities
!
! PURPOSE
!  This module defines a factory to create distance objects 
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

module factory_distance_utilities
!
use distance_base_utilities;
use euclidean_distance_utilities;
! use manhattan_distance_utilities;
! use correlation_distance_utilities;
! use correlation2_distance_utilities;
! use itakura_saito_distance_utilities;
! use prediction_distance_utilities;
! use log_likelihood_distance_utilities;
! use wavelet_distance_utilities;
! use dtw_distance_utilities;
!
implicit none;
!****c* factory_distance_utilities/factory_distance
! NAME
!   factory_distance
! PURPOSE
!  Class to represent a distance factory 

type factory_distance 
!  
!  METHODS
!
  contains
    procedure,public :: create_distance
end type factory_distance
!*****
 contains
 
  subroutine create_distance(factory,type_,dist)
    class(factory_distance) :: factory
    character(len=*) :: type_
    class(distance_base),allocatable :: dist
!
    select case(trim(type_))
      case('euclidean')
        !write(*,*) 'Euclidean distance';
        if(allocated(dist)) deallocate(dist);
        allocate(euclidean_distance :: dist);
!       case('manhattan')
!         !write(*,*) 'Manhattan distance allocated';
!         if(allocated(dist)) deallocate(dist);
!         allocate(manhattan_distance :: dist);
!       case('correlation')
!         !write(*,*) 'Correlation distance allocated';
!         if(allocated(dist)) deallocate(dist);
!         allocate(correlation_distance :: dist);
!       case('correlation2')
!         !write(*,*) 'Correlation2 distance allocated';
!         if(allocated(dist)) deallocate(dist);
!         allocate(correlation2_distance :: dist);
!       case('itakura_saito') 
!         !write(*,*) 'Itakura-Saito distance allocated';
!         if(allocated(dist)) deallocate(dist);
!         allocate(itakura_saito_distance :: dist);
!       case('prediction')
! !        write(*,*) 'Prediction distance allocated';      
!         if(allocated(dist)) deallocate(dist);
!         allocate(prediction_distance :: dist);
!       case('log_likelihood')
!         if(allocated(dist)) deallocate(dist);
!         allocate(log_likelihood_distance :: dist);
!       case('wavelet')
!         if(allocated(dist)) deallocate(dist);
!         allocate(wavelet_distance :: dist);
!       case('dtw')
!         if(allocated(dist)) deallocate(dist);
!         allocate(dtw_distance :: dist);
      case default
        write(*,*) 'ERROR: the requested distance is not defined'
        stop
    end select
!
  end subroutine create_distance
!  
end module factory_distance_utilities