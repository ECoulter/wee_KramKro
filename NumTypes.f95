module NumTypes

integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = selected_real_kind(2*precision(1.0_sp))
integer, parameter :: qp = selected_real_kind(4*precision(1.0_sp))

integer, parameter :: i4b = selected_int_kind(8)
integer, parameter :: i2b = selected_int_kind(4)
integer, parameter :: i1b = selected_int_kind(2)

end module NumTypes
