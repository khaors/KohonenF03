module sort_base_utilities
!
implicit none;
!
  type, abstract :: sort_base
   contains
      procedure (sort_procedure), deferred, pass :: sort
   end type sort_base
!
   abstract interface
      subroutine sort_procedure(my_sort,list,order)
         import  :: sort_base
         class(sort_base), intent(inout) :: my_sort
         real(kind=8),dimension(:),intent(inout) :: list
         integer, dimension(:), intent(out) :: order
      end subroutine sort_procedure
   end interface
!

 
end module sort_base_utilities