program two_level_som_train
use two_level_som_train_variables;
use two_level_self_organized_map_utilities;
!
implicit none;
!
real(kind=8),parameter :: version=0.1d0
character(len=*),parameter :: name='TWO_LEVEL_SOM_TRAIN'
character(len=40) :: arg

!
write(*,*)
write(*,'(A,A,f10.5)') name,' version: ',version
write(*,*)
write(*,*) 'Developed by: Oscar Garcia-Cabrejo'
write(*,*) '              Giovanni Quiroga    '
write(*,*) '              Erika Rodriguez     '
write(*,*)
write(*,*) ' === These guys have cojones ===  '
write(*,*)


call getarg(1,arg)
if(arg(1:1) .ne. ' ') then
  parfl=trim(adjustl(arg))
else
write(*,*) 'Parameter file?'
read(*,'(A30)') parfl
if(parfl(1:1) .eq. ' ') parfl='two_level_som_train.par';
endif
write(*,*) 'Parameter file= ',trim(parfl)
!
call initialize_variables(parfl);
!
call my_som%create(som_parameters);
!
call my_som%train(input_patterns);
!
if(som_parameters(1)%train_option == 0) then
  call my_som%calculate_sum2_clusters_grid();
endif
!
call my_som%destroy();
!
call release_variables();
!
write(*,*)
write(*,'(A,A,f10.5,2X,A)') name,' version: ',version,'Finished'
write(*,*)
!
end program two_level_som_train