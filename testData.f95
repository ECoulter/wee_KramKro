program main
IMPLICIT NONE
!to make some test for the KK relations
!use k(w) = 1/(R + i w L) 
! so that Re[k(w)] = R / (R**2 + w**2 * L**2)
! and     Im[k(w)] = -w*L / (R**2 + w**2 * L**2)
! (this is the transfer function for a simple RL circuit)
!from w = -50 to 50?
!just pick R = 1 ohm, L = 1 (unit of inductance)

!if you change this, kindly make the increment something
! that divides evenly into limit...
real :: limit = 20.0, increment = 0.02, w = 0
real :: imag_part = 0, real_part = 0
real :: R = 1.0, L = 4.0, shift = 0
character*32 :: outfile = "testdat.dat"
integer ::  numsteps = 0, i = 0

open(unit=1,file=outfile,action="write")

!this is why the increment should divide evenly into limit
! since numsteps must be an INT
numsteps = limit / increment

do i = -numsteps, numsteps
 w = limit * real(i)/numsteps + shift
 imag_part = R / (R**2 + (w - shift)**2 * L**2)
 real_part = -1*(w - shift)*L / (R**2 + (w - shift)**2 * L**2)
 write(1,*) w,real_part,imag_part
end do

close(1)

end program main
