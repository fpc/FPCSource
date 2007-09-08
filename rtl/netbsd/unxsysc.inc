{
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
     Lfpseterrno(EsysEInval);
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

{$ifndef FPC_USE_LIBC}
Function  fpFlock (fd,mode : longint) : cint;

begin
 fpFlock:=do_syscall(syscall_nr_flock,fd,mode);
end;

Function  fpfStatFS (Fd: cint; Info:pstatfs):cint;
begin
  fpfstatfs:=do_SysCall(SysCall_nr_fstatfs,fd,TSysParam(info))
end;

Function  fpStatFS  (Path:pchar; Info:pstatfs):cint;

begin
  fpstatfs:=do_SysCall(SysCall_nr_statfs,TSysParam(path),TSysParam(Info))
end;

Function  fpfsync (fd : cint) : cint;

begin
  fpfsync:=do_SysCall(syscall_nr_fsync, fd);
end;

// needs oldfpccall;
Function intAssignPipe(var pipe_in,pipe_out:longint;var errn:cint):cint; oldfpccall;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}

begin
{$ifdef cpui386}
 asm
   mov $42,%eax
   int $0x80
   jb .Lerror
   mov pipe_in,%ebx
   mov %eax,(%ebx)
   mov pipe_out,%ebx
   mov $0,%eax
   mov %edx,(%ebx)
   mov %eax,%ebx
   jmp .Lexit
.Lerror:
   mov %eax,%ebx
   mov $-1,%eax
.Lexit:
   mov Errn,%edx
   mov %ebx,(%edx)
 end;
{$endif}
end;


{
Function PClose(Var F:text) :cint;
var
  pl  : ^longint;
  res : longint;

begin
  do_syscall(syscall_nr_close,Textrec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(textrec(f).userdata[2]);
  fpwaitpid(pl^,@res,0);
  pclose:=res shr 8;
end;

Function PClose(Var F:file) : cint;
var
  pl : ^cint;
  res : cint;

begin
  do_syscall(syscall_nr_close,filerec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  fpwaitpid(pl^,@res,0);
  pclose:=res shr 8;
end;
}

function MUnMap (P : Pointer; Size : size_t) : cint;

begin
  MUnMap:=do_syscall(syscall_nr_munmap,longint(P),Size);
end;
{$else}

{

}

{$endif}


{
function  intClone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;  oldfpccall;


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
        mov     $-1,%eax
//        jmp     .L1
.L1:
        addl    $8, %esp
        popl    %esi
        mov     %eax,Res
end;
  If ErrSet Then
   fpSetErrno(LErrno);
  intClone:=Res;
end;



function  Clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;

begin
  Clone:=
        intclone(tclonefunc(func),sp,flags,args);
end;
}


