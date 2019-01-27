program som_train
!
use som_train_variables;
use self_organized_map_utilities;
!
implicit none;
!
real(kind=8),parameter :: version=0.2d0
character(len=*),parameter :: name='SOM_TRAIN';
character(len=40) :: arg
!real(kind=8),dimension(4,1) :: check
!integer,allocatable :: sample_index(:)
!
write(*,*)
write(*,'(A,A,f10.5)') trim(name),' version: ',version
write(*,*)
write(*,*) 'Developed by: Oscar Garcia-Cabrejo'
write(*,*)
write(*,*) ' === This guy has Kojones ===  '
write(*,*)
!
call getarg(1,arg)
if(arg(1:1) .ne. ' ') then
  parfl=trim(adjustl(arg))
else
write(*,*) 'Parameter file?'
read(*,'(A30)') parfl
if(parfl(1:1) .eq. ' ') parfl='som_train.par';
endif
write(*,*) 'Parameter file= ',trim(parfl)
!
call initialize_variables(parfl);
!
call my_som%create(som_parameters);
!
call my_som%train(input_patterns);
!
!call my_som%predict(input_patterns)
!
call my_som%destroy();
!
call release_variables();
!
write(*,*)
write(*,'(A,A,f10.5,2X,A)') name,' version: ',version,'Finished'
write(*,*)
!
end program som_train