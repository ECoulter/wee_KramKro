      module POSParser


      type intparam
       character :: cmdname*32 = ""
       integer :: defint = 0
      end type intparam

      integer,parameter,private :: dp = selected_real_kind(8)

      type dbleparam
       character :: cmdname*32 = ""
       real(dp) :: defdble = 0.0
      end type dbleparam

      type charparam
       character :: cmdname*32 = ""
       character :: defchar*32 = ""
      endtype charparam

      type ParamList
       type(intparam), dimension(50) :: ints
       type(dbleparam), dimension(50) :: dbles
       type(charparam), dimension(50) :: words
       integer :: clength=0,ilength=0,dble_length=0
      end type ParamList
 
      contains

      subroutine BuildCharList(cmd,defchar,List)
       implicit none
       character(len=*), intent(in) :: cmd
       character(len=*), intent(inout) :: defchar
       integer :: stat
       type(ParamList), intent(inout) :: List

       call GetCharParam(cmd,defchar,stat)

       List%clength = List%clength + 1
       List%words(List%clength)%cmdname = cmd  
       List%words(List%clength)%defchar = trim(defchar)

      end subroutine BuildCharList

      subroutine BuildIntList(cmd,defint,List)
       implicit none
       character(len=*), intent(in) :: cmd
       integer, intent(inout) :: defint
       integer :: stat = 0
       type(ParamList), intent(inout) :: List

       call GetIntParam(cmd,defint,stat)

       List%ilength = List%ilength + 1
       List%ints(List%ilength)%cmdname = cmd  
       List%ints(List%ilength)%defint = defint

      end subroutine BuildIntList

      subroutine BuildDbleList(cmd,defdble,List)
       implicit none
       character(len=*), intent(in) :: cmd
       real(kind=dp), intent(inout) :: defdble
       type(ParamList), intent(inout) :: List
       integer :: stat = 0

       call GetDbleParam(cmd,defdble,stat)

       List%dble_length = List%dble_length + 1
       List%dbles(List%dble_length)%cmdname = cmd  
       List%dbles(List%dble_length)%defdble = defdble

      end subroutine BuildDbleList

      subroutine PrintList(List)
      implicit none
      type(ParamList), intent(in) :: List
      integer :: i = 1

      print *, "Parameter List:"
      if(list%ilength .NE. 0) then
       do
        print *, List%ints(i)%cmdname,List%ints(i)%defint
        if(i.eq.List%ilength) exit
        i=i+1
       enddo
      endif
  
      i = 1
    
      if(list%dble_length .NE. 0) then
       do
        print *, List%dbles(i)%cmdname,List%dbles(i)%defdble
        if(i.eq.List%dble_length) exit
        i=i+1
       enddo
      endif

      i = 1

      if(list%clength .NE. 0) then
       do
        print *, List%words(i)%cmdname,"   ",List%words(i)%defchar
        if(i.eq.List%clength) exit
        i=i+1
       enddo
      endif

      endsubroutine PrintList

      subroutine GetIntParam(cmdname,cmdvalue,outstat)
      implicit none
      character :: cmdline*511,tmparg*32
      character(len=*), intent(in):: cmdname
      character(len=32) :: charint
      integer, intent(inout) :: cmdvalue
      integer :: cmdlen, stat,i,eqlen
      integer, intent(out) :: outstat
      
      outstat = 0
      i=1
      do
       call get_command_argument(i,tmparg,cmdlen,outstat)
       if(len_trim(tmparg).eq.0) exit
       if(tmparg(1:2).ne."--") then
        print *, "Bad command format!",tmparg
        stop 'Use format --PARAMNAME=VALUE'
        stop 0001
       endif
       if(tmparg(3:len_trim(cmdname)+2).eq.trim(cmdname)) then
        if(tmparg(len_trim(cmdname)+3:len_trim(cmdname)+3).eq."=") then
         write(charint,*) tmparg(len_trim(cmdname)+4:)
         read(charint,*) cmdvalue
        else
         print *, i, "Bad Format for command",tmparg,"!"
         stop 'Either include an = sign or stop hitting &
              &spacebar so much!'
         stop 0002
        endif
       endif
       i=i+1
      enddo

      end subroutine GetIntParam

      subroutine GetDbleParam(cmdname,cmdvalue,outstat)
      implicit none
      character :: cmdline*511,tmparg*32
      character(len=*), intent(in):: cmdname
      character(len=32) :: chardble
      integer,parameter :: dp = selected_real_kind(8)
      real(kind=dp), intent(inout) :: cmdvalue
      integer :: cmdlen, stat,i,eqlen
      integer, intent(out) :: outstat
      
      outstat = 0
      i=1
      do
       call get_command_argument(i,tmparg,cmdlen,outstat)
       if(len_trim(tmparg).eq.0) exit
       if(tmparg(1:2).ne."--") then
        print *, "Bad command format!",tmparg
        stop 'Use format --PARAMNAME=VALUE'
        stop 0001
       endif
       if(tmparg(3:len_trim(cmdname)+2).eq.trim(cmdname)) then
        if(tmparg(len_trim(cmdname)+3:len_trim(cmdname)+3).eq."=") then
         write(chardble,*) tmparg(len_trim(cmdname)+4:)
         read(chardble,*) cmdvalue
        else
         print *, i, "Bad Format for command",tmparg,"!"
         stop 'Either include an = sign or stop hitting &
              &spacebar so much!'
         stop 0002
        endif
       endif
       i=i+1
      enddo

      end subroutine GetDbleParam

      subroutine GetCharParam(cmdname,cmdvalue,outstat)
      implicit none
      character :: cmdline*511,tmparg*32
      character(len=*), intent(in):: cmdname
!     character(len=32),intent(inout) :: cmdvalue
      character(len=*),intent(inout) :: cmdvalue
      integer :: cmdlen, stat,i,eqlen
      integer, intent(out) :: outstat
      
      outstat = 0
      i=1
      do
       call get_command_argument(i,tmparg,cmdlen,outstat)
       if(len_trim(tmparg).eq.0) exit
       if(tmparg(1:2).ne."--") then
        print *, i,"Bad command format!",tmparg
        stop 'Use format --PARAMNAME=VALUE'
        stop 0003
        return
       endif
       if(tmparg(3:len_trim(cmdname)+2).eq.trim(cmdname)) then
        if(tmparg(len_trim(cmdname)+3:len_trim(cmdname)+3).eq."=") then
         write(cmdvalue,'(A)') trim(tmparg(len_trim(cmdname)+4:))
        else
         print *, "Bad Format in command:",tmparg
         stop 'Either include an = sign or stop hitting &
              &spacebar so much!'
         stop 0004
        endif
       endif
!      write(*,*) i,":",trim(tmparg),len(tmparg),len_trim(tmparg),stat
       i=i+1
      enddo

      end subroutine GetCharParam

      logical function NeedHelp(Params)
      implicit none
      character :: cmdline*511,tmparg*32
      type(ParamList) :: Params
      integer :: i

      i = 0  
      do
       call get_command_argument(i,tmparg)
       if(len_trim(tmparg).eq.0) exit
       if(trim(tmparg(3:)).eq."help") then
        print *, "You are using the POSParser. Available options",&
              &new_line('x'),&
              &" are:",&
              &new_line('x'),&
              &" integer",&
              &new_line('x'),&
              &" double(8 bit)",&
              &new_line('x'),&
              &" character ",&
              &new_line('x'),&
              &" parameters. Character",&
              &" parameter variables MAY NOT BE LARGER OR SMALLER ",&
              &new_line('x'),&
              &" THAN 32 Characters! (The variable size, at least;",&
              &new_line('x'),&
              &" variables may contain trailing blanks).",&
              &new_line('x')
        print *, "Available options are:"
        call PrintList(Params)
        NeedHelp = .TRUE.
        return 
!       call PrintList()
       endif
       i=i+1
      enddo
      NeedHelp = .FALSE.
     
      end function NeedHelp
      
      end module POSParser
