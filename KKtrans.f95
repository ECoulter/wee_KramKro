module KK_selfEn
!FINISH WRITE OUTPUT, AND ERROR CHECK THE CODE
!ALSO, FINISH THE CONVERSION TO IMDIEL
use NumTypes
IMPLICIT NONE


real(kind=dp), parameter :: pi = 3.141592653589793238462

contains

subroutine ReadSelfEn(inputfile,outputarray)
!read Refl in from file, place in Array
character*32, intent(in) :: inputfile
character*32 :: dummy !just for reading past comments
!output array index (1,:) holds energies
!             index (2,:) holds reflectance
real(kind=dp), intent(inout), dimension(:,:) :: outputarray
integer :: i = 0, numdata = 0

numdata = size(outputarray,2)

open(unit=10,file=inputfile,action="read")
!read past comment chars
do 
  read(10,*,end=20) dummy
  if (isnum(dummy)) exit
  cycle
  20 print *, "END OF FILE BEFORE COMMENTS WERE DONE! Refl set to 0"
     outputarray = 0
enddo

backspace(10)

do i = 1, numdata
  read(10,*,END=50) outputarray(1,i),dummy,outputarray(2,i)

  cycle

  50 print *, "Unexpected end of file! at line: ",i
     print *, "Expected ",numdata," records."
     exit
end do

100 close(10)

end subroutine ReadSelfEn

subroutine CalcRealSquaredVersion(im_array,re_array)
!based on def'n of KK relation
!between real and im part of an analytic
!imaginary function
!calculate Phase of Reflectance, place in Array
!arrays index (1,:) holds energies
!       index (2,:) holds quantity
real(kind=dp), intent(inout), dimension(:,:) :: im_array
real(kind=dp),intent(inout), dimension(:,:) :: re_array
integer :: numdata = 0, i = 0, j = 0, avg_step = 0, start = 0
real(kind=dp) :: omega = 0, omega_next = 0, omega_zero = 0
real(kind=dp) :: delta_omega = 0, avg_delta = 0
real(kind=dp) :: mac_sum_i = 0, shift = 0, err_corr = 1000000
logical :: shifted = .FALSE.

numdata = size(im_array,2)
re_array = 0

if(im_array(1,1) .lt. 0) then
  print *, "Setting lowest frequency to 0!!!"
  shifted = .TRUE.
  shift  = im_array(1,1)
!why the hell does this help?
!just a constant shift to very large frequency...
!so, this Squared version works better for the Lorenztian
! given in the paper
! but for the one from Saff & Snider, I have to apply
! this shift away from 0 to get a good result???
! when I add a shift to the test function itself, 
! results in errors as well?!?! ARGH
  im_array(1,:) = im_array(1,:) - err_corr*shift
  re_array(1,:) = re_array(1,:) - err_corr*shift
endif

do j = 1, numdata
!see Appl. Spectroscopy Vol 42, Issue 6, 952-957
!MacLaurin Approx - supposedly better than Trapeziod approx?
!this is actually the trapezoid approx but only over
! half of the data points? wtf?
!"Maclaurin's formula" uses every Other data point 
! where the parity of the odd summation is chosen 
! to avoid the point i = j
! havent' found this in a book yet...
  avg_step = 0
  start = 0
  avg_delta = 0
  mac_sum_i = 0
  omega_zero = im_array(1,j)
  re_array(1,j) = omega_zero

  if(mod(j,2).eq.0) then
    start = 1
  else if (mod(j,2).eq.1) then
    start = 2
  else
    print *, "j is neither even nor odd!!! WHAT THE HELL is this?",j
    exit
  endif

  do i = start, numdata, 2
    avg_step = avg_step + 1
    omega = im_array(1,i)
    if(i.ne.numdata) then
      omega_next = im_array(1,i+1)
    else
      omega_next = im_array(1,i-1)
    endif
    delta_omega = abs(omega_next - omega)
    avg_delta = (avg_delta * (avg_step - 1) + delta_omega) / avg_step
    mac_sum_i = 0.5 * (im_array(2,i) / (omega - omega_zero) + &
                      im_array(2,i) / (omega + omega_zero))
    re_array(2,j) = re_array(2,j) + mac_sum_i
  end do

  re_array(2,j) = 1 * re_array(2,j) * (2.0/pi) * 2 * avg_delta

end do

if(shifted) then
  im_array(1,:) = im_array(1,:) + err_corr*shift
  re_array(1,:) = re_array(1,:) + err_corr*shift
endif

end subroutine CalcRealSquaredVersion

subroutine CalcReal(im_array,re_array)
!based on def'n of KK relation
!between real and im part of an analytic
!imaginary function
!calculate Phase of Reflectance, place in Array
!arrays index (1,:) holds energies
!       index (2,:) holds quantity
real(kind=dp), intent(inout), dimension(:,:) :: im_array
real(kind=dp),intent(inout), dimension(:,:) :: re_array
integer :: numdata = 0, i = 0, j = 0, avg_step = 0, start = 0
real(kind=dp) :: omega = 0, omega_next = 0, omega_zero = 0
real(kind=dp) :: delta_omega = 0, avg_delta = 0
real(kind=dp) :: mac_sum_i = 0, shift = 0
logical :: shifted = .false.
!real(kind=dp) :: omegafrac = 0
!real(kind=dp) :: eps_frac = 0.01, epsilon_omega = 0
!real(kind=dp) :: f_of_omega = 0, f_of_omega_next = 0
!real(kind=dp) :: slope = 0, im_of_omega_eps = 0
!real(kind=dp) :: trap_area = 0

numdata = size(im_array,2)
re_array = 0

if(im_array(1,1) .ge. 0) then
  print *, "Setting center frequency = 0"
  shifted = .TRUE.
  shift  = im_array(1,numdata/2)
  im_array(1,:) = im_array(1,:) - shift
  re_array(1,:) = re_array(1,:) - shift
endif

do j = 1, numdata
!MacLaurin Approx - supposedly better than Trapeziod approx?
!this is actually the trapezoid approx but only over
! half of the data points? wtf?
! also I may have been doing the Trap approx wrong...
! should go back and look at that at some point...
!Maclaurin's formula uses every Other data point 
! where the parity of the odd summation is chosen 
! to avoid the point i = j
  avg_step = 0
  start = 0
  avg_delta = 0
  mac_sum_i = 0
  omega_zero = im_array(1,j)
  re_array(1,j) = omega_zero

! if(j.eq.490) then
!  print *, "omega0 = ",omega_zero
! endif

  if(mod(j,2).eq.0) then
    start = 1
  else if (mod(j,2).eq.1) then
    start = 2
  else
    print *, "j is neither even nor odd!!! WHAT THE HELL is this?",j
    exit
  endif

  do i = start, numdata, 2
    avg_step = avg_step + 1
    omega = im_array(1,i)
    if(i.ne.numdata) then
      omega_next = im_array(1,i+1)
    else
      omega_next = im_array(1,i-1)
    endif
    delta_omega = abs(omega_next - omega)
    avg_delta = (avg_delta * (avg_step - 1) + delta_omega) / avg_step
    mac_sum_i = im_array(2,i) / (omega - omega_zero)
    re_array(2,j) = re_array(2,j) + mac_sum_i
  end do

  re_array(2,j) = 1 * re_array(2,j) * (1.0/pi) * 2 * avg_delta

end do

if(shifted) then
  im_array(1,:) = im_array(1,:) + shift
  re_array(1,:) = re_array(1,:) + shift
endif

end subroutine CalcReal

subroutine WriteOutput(out_file_name,out_array)
character(len=*), intent(in) :: out_file_name
real(kind=dp), intent(in), dimension(:,:) :: out_array
integer :: i = 0, j = 0, len1 = 0, len2 = 0

len1 = size(out_array,1)
len2 = size(out_array,2)

open(unit=10,file=trim(out_file_name),action="write")

do j = 1,len2
 write(10,*) (out_array(i,j),i=1,len1)
end do

close(10)

end subroutine WriteOutput

real(kind=dp) function Bofx(x)
!eq'n (A10) of PRB.2.2182
real(kind=dp),intent(in) :: x
real(kind=dp) :: current_B
real(kind=dp) :: tol = 0.00000000000000000000001, next_term
integer :: m ! used in a sum... 
integer, parameter :: maxsteps = 100000

current_B = 0
next_term = 0

do m = 1, maxsteps

 next_term = (x**(2.0*m+1.0))/(dble(2.0*m - 1.0)**2)
!if(m.eq.1) print *, "current_B : ",current_B
 current_B = current_B + next_term

 if(abs(next_term) .lt. tol) then
!    print *, "B converged at ", m,"steps, for x = ", x
!    print *, "current_B : ",current_B
!    print *, ""
   exit
 endif

 if(m.eq.maxsteps) then
   print *, "Bofx may be unconverged at value: ",x
 endif
 
end do

Bofx = current_B * (2.0/pi)

end function Bofx

real(kind=dp) function Phiofx(x,omega_lt_omega_zero)
!eq'n (A7) of PRB2.2182
real(kind=dp),intent(in) :: x
logical, intent(in) :: omega_lt_omega_zero

if (x .eq. 1.0) then
  Phiofx = 0.25*pi
else if (x .gt. 1.0) then
  print *, "x GREATER THAN 1.0 SENT TO PHI!!",x
  Phiofx = 0.25*pi
else if(x .lt. 1.0) then
  if(omega_lt_omega_zero) then
    Phiofx = 0.5*pi - Bofx(x)
  else
    Phiofx = Bofx(x)
  endif
endif

end function Phiofx

real(kind=dp) function x_of_omega(omega_zero,omega)
real(kind=dp),intent(in) :: omega_zero,omega

if(omega_zero.lt.omega) then
  x_of_omega = omega_zero/omega
else if(omega.lt.omega_zero) then
  x_of_omega = omega/omega_zero
else
  x_of_omega = 1.0
endif

end function x_of_omega

subroutine Test_x_of_omega(dotest)
logical, intent(in) :: dotest
real(kind=dp) :: num1 = 4.123312317
real(kind=dp) :: num2 = 1.498759817
if(dotest) then
 print *, "x_of ",num1,"and",num2," is: ",x_of_omega(num1,num2)
 print *, "x_of ",num2,"and",num1," is: ",x_of_omega(num2,num1)
 print *, "x_of ",1,"and",2," is: ",x_of_omega(dble(1),dble(2))
 print *, "x_of ",num2,"and",num2," is: ",x_of_omega(num2,num2)
endif 
end subroutine Test_x_of_omega

subroutine Test_Phi(dotest)
logical, intent(in) :: dotest
integer :: i = 0, j = 0
if(.not.dotest) return

do i = 1,10
 print *, "Phi of ",dble(i/10.0)," is: ",Phiofx(dble(i/10.0),.true.)
 print *, "Diff between true/false is: ",Phiofx(dble(i/10.0),.true.) + &
                                         Phiofx(dble(i/10.0),.false.)
 print *, ""
end do

end subroutine Test_Phi

subroutine Test_B(dotest)
logical, intent(in) :: dotest
integer :: i = 0, j = 0

if(.not.dotest) return

do i = 1,10
 print *, "B of ",dble(i/10.0), "is: ", Bofx(dble(i/10.0))
 print *, ""
end do

end subroutine Test_B

logical function isnum(c)
!note: this assumes ASCII data?
! also, could be done simpler by trying to read c into a real variable
! and checking if failure?
character(len=*), intent(in) :: c
real(kind=dp) :: test = 0
integer :: i = 0,dotcount = 0

do i = 1,1
  read(c,*,ERR=10,END=10) test
  cycle
10  isnum = .false.
    return
end do

isnum = .true.

end function isnum

subroutine CalcRealTrap(im_array,re_array)
!based on def'n of KK relation
!between real and im part of an analytic
!imaginary function
!calculate Phase of Reflectance, place in Array
!arrays index (1,:) holds energies
!       index (2,:) holds quantity
real(kind=dp), intent(inout), dimension(:,:) :: im_array
real(kind=dp),intent(inout), dimension(:,:) :: re_array
integer :: numdata = 0, i = 0, j = 0, avg_step = 0, start = 0
real(kind=dp) :: omega = 0, omega_next = 0, omega_zero = 0
real(kind=dp) :: delta_omega = 0, avg_delta = 0
!real(kind=dp) :: mac_sum_i = 0, shift = 0
!logical :: shifted = .false.
real(kind=dp) :: omegafrac = 0
real(kind=dp) :: eps_frac = 0.01, epsilon_omega = 0
real(kind=dp) :: f_of_omega = 0, f_of_omega_next = 0
real(kind=dp) :: f_of_omega_1 = 0, f_of_omega_n = 0
real(kind=dp) :: slope = 0, im_of_omega_eps = 0
real(kind=dp) :: trap_term = 0

numdata = size(im_array,2)
re_array = 0

do j = 1, numdata

 omega_zero = im_array(1,j)
 re_array(1,j) = omega_zero

 do i = 1, numdata - 1

!trapezoid approx
   omega = im_array(1,i)
   omega_next = im_array(1,i+1)
   delta_omega = abs(omega - omega_next)
   if(i.eq.j) then
!we need an interpolated value of the im_array at omega plus epsilon
!shift omega to avoid the pole
     cycle
!AH-HAH! THIS WAS CAUSING THE CALCULATED REAL PART
! TO LOOK EXACTLY LIKE THE IM PART BECAUSE THIS TERM
! IS JUST A HUGE NUMBER TIME THE IM PART!
! SO, THIS IS INCORRECT!!!
     epsilon_omega = delta_omega * eps_frac
     omega = omega - epsilon_omega
     delta_omega = abs(omega - omega_next)
     slope = (im_array(2,i+1) - im_array(2,i)) / &
             (im_array(1,i+1) - im_array(1,i))
     im_of_omega_eps = epsilon_omega * slope + im_array(2,i)
     f_of_omega = im_of_omega_eps / (omega - omega_zero)
   else
!!no interpolation needed, suckas!
     f_of_omega = im_array(2,i) / (omega - omega_zero)
   endif

   trap_term = f_of_omega * delta_omega
  

   re_array(2,j) = re_array(2,j) + trap_term
 end do

 if(j.eq.1) then
   delta_omega = abs(im_array(1,1) - im_array(1,2))
   epsilon_omega = delta_omega * eps_frac
   omega = im_array(1,1) + epsilon_omega
   slope = (im_array(2,2) - im_array(2,1)) / &
           (im_array(1,2) - im_array(1,1))
   im_of_omega_eps = epsilon_omega * slope + im_array(2,i)
   f_of_omega_1 = im_of_omega_eps / (omega - omega_zero)
   f_of_omega_n = im_array(2,numdata) / (im_array(1,numdata) - omega_zero)
   re_array(2,j) = re_array(2,j) -  & 
                   0.5*(f_of_omega_1 + f_of_omega_n)*delta_omega
 else if (j.eq.numdata) then
   delta_omega = abs(im_array(1,numdata) - im_array(1,numdata-1))
   epsilon_omega = delta_omega * eps_frac
   omega = im_array(1,numdata) - epsilon_omega
   slope = (im_array(2,2) - im_array(2,1)) / &
           (im_array(1,2) - im_array(1,1))
   im_of_omega_eps = im_array(2,numdata) - epsilon_omega * slope 
   f_of_omega_1 = im_array(2,1) / (im_array(1,1) - omega_zero)
   f_of_omega_n = im_of_omega_eps / (omega - omega_zero)
   re_array(2,j) = re_array(2,j) -  & 
                   0.5*(f_of_omega_1 + f_of_omega_n)*delta_omega
 else
   f_of_omega_1 = im_array(2,1) / (im_array(1,1) - omega_zero)
   f_of_omega_n = im_array(2,numdata) / (im_array(1,numdata) - omega_zero)
   re_array(2,j) = re_array(2,j) -  & 
                   0.5*(f_of_omega_1 + f_of_omega_n)*delta_omega

 endif

   re_array(2,j) = re_array(2,j) * (1/pi)

end do

end subroutine CalcRealTrap

subroutine KramKro(known_array,trans_array,sign_of_transform)
!based on def'n of KK relation
!between real and im part of an analytic
!imaginary function
!calculate Phase of Reflectance, place in Array
!arrays index (1,:) holds energies
!       index (2,:) holds quantity
real(kind=dp), intent(inout), dimension(:,:) :: known_array
real(kind=dp),intent(inout), dimension(:,:) :: trans_array
!if from real to imag, this should be 1
! otherwise, should be -1
integer, intent(in) :: sign_of_transform
integer :: numdata = 0, i = 0, j = 0, avg_step = 0, start = 0
real(kind=dp) :: omega = 0, omega_next = 0, omega_zero = 0
real(kind=dp) :: delta_omega = 0, avg_delta = 0
real(kind=dp) :: mac_sum_i = 0, shift = 0
logical :: shifted = .false.

numdata = size(known_array,2)
trans_array = 0

if(sign_of_transform.ne.1 .or. sign_of_transform .ne. -1) then
  print *, "Sign of transform VERY STRANGE:",sign_of_transform
  return
endif

if(known_array(1,1) .ge. 0) then
  print *, "Setting center frequency = 0"
  shifted = .TRUE.
  shift  = known_array(1,numdata/2)
  known_array(1,:) = known_array(1,:) - shift
endif

trans_array(1,:) = known_array(1,:)

do j = 1, numdata
!MacLaurin Approx - supposedly better than Trapeziod approx?
!this is actually the trapezoid approx but only over
! half of the data points? wtf?
! also I may have been doing the Trap approx wrong...
! should go back and look at that at some point...
!Maclaurin's formula uses every Other data point 
! where the parity of the odd summation is chosen 
! to avoid the point i = j
  avg_step = 0
  start = 0
  avg_delta = 0
  mac_sum_i = 0
  omega_zero = known_array(1,j)

! if(j.eq.490) then
!  print *, "omega0 = ",omega_zero
! endif

  if(mod(j,2).eq.0) then
    start = 1
  else if (mod(j,2).eq.1) then
    start = 2
  else
    print *, "j is neither even nor odd!!! WHAT THE HELL is this?",j
    exit
  endif

  do i = start, numdata, 2
    avg_step = avg_step + 1
    omega = known_array(1,i)
    if(i.ne.numdata) then
      omega_next = known_array(1,i+1)
    else
      omega_next = known_array(1,i-1)
    endif
    delta_omega = abs(omega_next - omega)
    avg_delta = (avg_delta * (avg_step - 1) + delta_omega) / avg_step
    mac_sum_i = known_array(2,i) / (omega - omega_zero)
    trans_array(2,j) = trans_array(2,j) + mac_sum_i
  end do

  trans_array(2,j) = sign_of_transform * trans_array(2,j) *  &
                     (1.0/pi) * 2 * avg_delta

end do

if(shifted) then
  known_array(1,:) = known_array(1,:) + shift
  trans_array(1,:) = re_array(1,:) + shift
endif

end subroutine CalcReal

end module KK_selfEn
