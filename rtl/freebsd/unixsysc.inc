{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 2000 by Marco van de Voort
     member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{
function clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;
{NOT IMPLEMENTED YET UNDER BSD}
begin // perhaps it is better to implement the hack from solaris then this msg
 HALT;
END;

  if (pointer(func)=nil) or (sp=nil) then
   begin
     LinuxError:=EsysEInval;
     exit(-1);
   end;
  asm
        { Insert the argument onto the new stack. }
        movl    sp,%ecx
        subl    $8,%ecx
        movl    args,%eax
        movl    %eax,4(%ecx)

        { Save the function pointer as the zeroth argument.
          It will be popped off in the child in the ebx frobbing below. }
        movl    func,%eax
        movl    %eax,0(%ecx)

        { Do the system call }
        pushl   %ebx
        pushl   %ebx
      //  movl    flags,%ebx
        movl    $251,%eax
        int     $0x80
        popl    %ebx
        popl    %ebx
        test    %eax,%eax
        jnz     .Lclone_end

        { We're in the new thread }
        subl    %ebp,%ebp       { terminate the stack frame }
        call    *%ebx
        { exit process }
        movl    %eax,%ebx
        movl    $1,%eax
        int     $0x80

.Lclone_end:
        movl    %eax,__RESULT
  end;
end;
}

{
Procedure GetTimeOfDay(var tv:timeval);
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}

var  tz : timezone;

begin
 do_syscall(syscall_nr_gettimeofday,longint(@tv),longint(@tz));
 LinuxError:=FpGetErrno;
end;
}
Function  fdFlush (fd : Longint) : Boolean;

begin
  fdflush:=do_syscall(syscall_nr_fsync,fd)=0;
  LinuxError:=FpGetErrno;
end;

Function  Flock (fd,mode : longint) : boolean;

begin
 Flock:=do_syscall(syscall_nr_flock,fd,mode)=0;
 LinuxError:=FpGetErrno;
end;

Function StatFS(Path:Pathstr;Var Info:Tstatfs):Boolean;

{
  Get all information on a fileSystem, and return it in Info.
  Path is the name of a file/directory on the fileSystem you wish to
  investigate.
}

begin
  path:=path+#0;
  StatFS:=Do_Syscall(syscall_nr_statfs,longint(@path[1]),longint(@info))=0;
  LinuxError:=FpGetErrno;
end;

Function StatFS(Fd:Longint;Var Info:tstatfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Fd is the file descriptor of a file/directory on the fileSystem
  you wish to investigate.
}

begin
 StatFS:=do_syscall(syscall_nr_fstatfs,fd,longint(@info))=0;
 LinuxError:=FpGetErrno;
end;

Function AssignPipe(var pipe_in,pipe_out:longint):boolean;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  pip  : tpipe;

begin
 do_syscall(syscall_nr_pipe,longint(@pip));
 LinuxError:=FpGetErrno;
 pipe_in:=pip[1];
 pipe_out:=pip[2];
 AssignPipe:=(LinuxError=0);
end;


Function PClose(Var F:text) :longint;
var
  pl  : ^longint;
  res : longint;

begin
  do_syscall(syscall_nr_close,Textrec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(textrec(f).userdata[2]);
  fpwaitpid(pl^,res,0);
  pclose:=res shr 8;
end;

Function PClose(Var F:file) : longint;
var
  pl : ^longint;
  res : longint;

begin
  do_syscall(syscall_nr_close,filerec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  fpwaitpid(pl^,res,0);
  pclose:=res shr 8;
end;

function MUnMap (P : Pointer; Size : Longint) : Boolean;

begin
  MUnMap:=do_syscall(syscall_nr_munmap,longint(P),Size)=0;
  LinuxError:=FpGetErrno;
end;

function  Clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint; 

var lerrno : Longint;
    errset : Boolean;
    Res    : Longint;	
begin
  errset:=false;
  Res:=0;
asm
        pushl   %esi
        movl    12(%ebp), %esi  // get stack addr
        subl    $4, %esi
        movl    20(%ebp), %eax  // get __arg
        movl    %eax, (%esi)
        subl    $4, %esi
        movl    8(%ebp), %eax   // get __fn
        movl    %eax, (%esi)
        pushl   16(%ebp)
        pushl   %esi
        mov     syscall_nr_rfork, %eax
        int     $0x80                  // call actualsyscall
        jb      .L2
        test    %edx, %edx
        jz      .L1
        movl    %esi,%esp
        popl    %eax
        call    %eax
        addl    $8, %esp
        call    halt            // Does not return
.L2:
        mov     %eax,LErrNo
        mov     $true,Errset
	mov	$-1,%eax
//        jmp     .L1
.L1:
        addl    $8, %esp
        popl    %esi
	mov	%eax,Res
end;
  If ErrSet Then
   fpSetErrno(LErrno);
  Clone:=Res; 
end;

{
  $Log$
  Revision 1.9  2003-09-15 07:09:58  marco
   * small fixes, round 1

  Revision 1.8  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.7  2003/01/05 19:02:29  marco
   * Should now work with baseunx. (gmake all works)

  Revision 1.6  2002/10/18 12:19:59  marco
   * Fixes to get the generic *BSD RTL compiling again + fixes for thread
     support. Still problems left in fexpand. (inoutres?) Therefore fixed
     sysposix not yet commited

  Revision 1.5  2002/09/07 16:01:18  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/05/06 09:35:09  marco
   * Some stuff from 1.0.x ported

}
