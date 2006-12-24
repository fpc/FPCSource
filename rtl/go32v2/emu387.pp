{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Pierre Muller

    FPU Emulator support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit emu387;
interface

procedure npxsetup(prog_name : string);


implementation

{$asmmode ATT}

uses
  dxeload,dpmiexcp,strings;

type
  emu_entry_type = function(exc : pexception_state) : longint;

var
  _emu_entry : emu_entry_type;


procedure _control87(mask1,mask2 : longint);
begin
{ Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details }
{ from file cntrl87.s in src/libc/pc_hw/fpu }
  asm
        { make room on stack }
        pushl   %eax
        fstcw   (%esp)
        fwait
        popl    %eax
        andl    $0xffff, %eax
        { OK;  we have the old value ready }

        movl    mask2, %ecx
        notl    %ecx
        andl    %eax, %ecx      { the bits we want to keep }

        movl    mask2, %edx
        andl    mask1, %edx      { the bits we want to change }

        orl     %ecx, %edx      { the new value }
        pushl   %edx
        fldcw   (%esp)
        popl    %edx
  end;
end;


{ the problem with the stack that is not cleared }
function emu_entry(exc : pexception_state) : longint;
begin
  emu_entry:=_emu_entry(exc);
end;


function nofpsig( sig : longint) : longint;cdecl;
const
  last_eip : longint = 0;
var
  res : longint;
begin
  {if last_eip=djgpp_exception_state^.__eip then
    begin
       writeln('emu call two times at same address');
       dpmi_set_coprocessor_emulation(1);
       _raise(SIGFPE);
       exit(0);
    end; }
  last_eip:=djgpp_exception_state^.__eip;
  res:=emu_entry(djgpp_exception_state);
  if res<>0 then
    begin
       writeln('emu call failed. res = ',res);
       dpmi_set_coprocessor_emulation(1);
       _raise(SIGFPE);
       exit(0);
    end;
  dpmi_longjmp(pdpmi_jmp_buf(djgpp_exception_state)^, djgpp_exception_state^.__eax);
  nofpsig:=0;
end;


var
  prev_exit : pointer;

procedure restore_DPMI_fpu_state;
begin
  exitproc:=prev_exit;
  { Enable Coprocessor, no exceptions }
  dpmi_set_coprocessor_emulation(1);
{$ifdef SYSTEMDEBUG}
  writeln(stderr,'Coprocessor restored ');
{$endif}
end;

{ function _detect_80387 : boolean;
  not used because of the underscore problem }

{$L fpu.o }


function getenv(const envvar:string):string;
{ Copied here, preserves uses Dos (PFV) }
var
  hp      : ppchar;
  hs,
  _envvar : string;
  eqpos   : longint;
begin
  _envvar:=upcase(envvar);
  hp:=envp;
  getenv:='';
  while assigned(hp^) do
   begin
     hs:=strpas(hp^);
     eqpos:=pos('=',hs);
     if copy(hs,1,eqpos-1)=_envvar then
      begin
        getenv:=copy(hs,eqpos+1,255);
        exit;
      end;
     inc(hp);
   end;
end;


function __detect_80387:byte;external name '__detect_80387';

procedure npxsetup(prog_name : string);
var
  cp : string;
  i : byte;
  have_80387 : boolean;
  emu_p : pointer;
const
  veryfirst : boolean = True;
begin
  cp:=getenv('387');
  if (length(cp)>0) and (upcase(cp[1])='N') then
    have_80387:=False
  else
    begin
       dpmi_set_coprocessor_emulation(1);
       asm
          call __detect_80387
          movb %al,have_80387
       end;
    end;
  if (length(cp)>0) and (upcase(cp[1])='Q') then
    begin
       if not have_80387 then
         write(stderr,'No ');
       writeln(stderr,'80387 detected.');
    end;

  if have_80387 then
   begin
     { mask all exceptions, except invalid operation }
     { change to same value as in v2prt0.as (PM)     }
     _control87($0332, $ffff)
   end
  else
    begin
       { Flags value 3 means coprocessor emulation, exceptions to us }
       if (dpmi_set_coprocessor_emulation(3)<>0) then
         begin
            writeln(stderr,'Warning: Coprocessor not present and DPMI setup failed!');
            writeln(stderr,'         If application attempts floating operations system may hang!');
         end
       else
         begin
            cp:=getenv('EMU387');
            if length(cp)=0 then
              begin
                 for i:=length(prog_name) downto 1 do
                   if (prog_name[i]='\') or (prog_name[i]='/') then
                     break;
                 if i>1 then
                   cp:=copy(prog_name,1,i);
                 cp:=cp+'wmemu387.dxe';
              end;
            emu_p:=dxe_load(cp);
            _emu_entry:=emu_entry_type(emu_p);
            if (emu_p=nil) then
              begin
                 writeln(cp+' load failed !');
                 halt;
              end;
            if veryfirst then
              begin
                 veryfirst:=false;
                 prev_exit:=exitproc;
                 exitproc:=@restore_DPMI_fpu_state;
              end;
            signal(SIGNOFP,@nofpsig);
         end;
    end;
end;

begin
   npxsetup(paramstr(0));
end.
