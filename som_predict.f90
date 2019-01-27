program som_predict
!
use som_predict_variables;
use self_organized_map_utilities;
!
implicit none;
!
real(kind=8),parameter :: version=0.1d0
character(len=40) :: arg
!
write(*,*)
write(*,'(A,f10.5)') 'SOM_PREDICT version: ',version
write(*,*)
write(*,*) 'Developed by: Oscar Garcia-Cabrejo'
write(*,*)
write(*,*) ' === This guy has Kojones ===  '
write(*,*)

call getarg(1,arg)
if(arg(1:1) .ne. ' ') then
  parfl=trim(adjustl(arg))
else
  write(*,*) 'Parameter file?'
  read(*,'(A30)') parfl
  if(parfl(1:1) .eq. ' ') parfl='som_predict.par';
endif
write(*,*) 'Parameter file= ',trim(parfl)
!
call initialize_variables(parfl);
!
call my_som%create(som_parameters);
!
call my_som%read(prototype_file); !create(som_parameters);
!
call my_som%predict(input_patterns,map_output);
!
call my_som%destroy();
!
call release_variables();
!
end program som_predict