program main
use KK_selfEn
use POSParser
use Numtypes
IMPLICIT NONE

type(ParamList) :: params

character*32 :: selfEn_file = "Default"
integer :: numdata = 0

real(kind=dp),dimension(:,:), allocatable :: re_SelfEn, im_SelfEn

integer :: i = 0, j = 0

call BuildIntList("numdata",defint=numdata,list=Params)
call BuildCharList("File",defchar=selfEn_file,list=Params)
if(NeedHelp(params)) then
 stop
end if

allocate(re_SelfEn(1:2,1:numdata))
allocate(im_SelfEn(1:2,1:numdata))

call ReadSelfEn(selfEn_file,im_SelfEn)
!call WriteOutput("refl-"//trim(refl_file),reflectance)

call CalcReal(im_SelfEn,re_SelfEn)
call WriteOutput("real-"//trim(selfEn_file),re_SelfEn)
call WriteOutput("im-"//trim(selfEn_file),im_SelfEn)

!call CalcRealSquaredVersion(im_SelfEn,re_SelfEn)
!call WriteOutput("real2-"//trim(selfEn_file),re_SelfEn)
!call WriteOutput("im2-"//trim(selfEn_file),im_SelfEn)

call KramKro(re_SelfEn,im_SelfEn,1.0)
call WriteOutput("realback-"//trim(selfEn_file),re_SelfEn)
call WriteOutput("imback-"//trim(selfEn_file),im_SelfEn)

!call CalcRealTrap(im_SelfEn,re_SelfEn)
!call WriteOutput("real3-"//trim(selfEn_file),re_SelfEn)
!call WriteOutput("im3-"//trim(selfEn_file),im_SelfEn)

end program main
