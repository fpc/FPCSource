{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Pierre Muller,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ Translated to FPK pascal by Pierre Muller,
without changing the fpu.s file }
{
/* Copyright (C) 1994, 1995 Charles Sandmann (sandmann@clio.rice.edu)
 * FPU setup and emulation hooks for DJGPP V2.0
 * This file maybe freely distributed, no warranty. */
this file has been translated from
  npxsetup.c  }

unit emu387;

  interface

    procedure npxsetup(prog_name : string);

  implementation

    uses dxeload, dpmiexcp, strings;

  type
     emu_entry_type = function(exc : pexception_state) : longint;

  var
     _emu_entry : emu_entry_type;


  procedure _control87(mask1,mask2 : word);

    begin
{/* Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details */}
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
           andl    %eax, %ecx      /* the bits we want to keep */

           movl    mask2, %edx
           andl    mask1, %edx      /* the bits we want to change */

           orl     %ecx, %edx      /* the new value */
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

  function nofpsig( sig : longint) : longint;
    var res : longint;
    const
       last_eip : longint = 0;

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
       longjmp(pjmprec(djgpp_exception_state)^, djgpp_exception_state^.__eax);
       nofpsig:=0;
    end;

  var
     prev_exit : pointer;

  procedure restore_DPMI_fpu_state;
    begin
       exitproc:=prev_exit;
       dpmi_set_coprocessor_emulation(1);
       writeln('Coprocessor restored ');
       {/* Enable Coprocessor, no exceptions */}
    end;

 { function _detect_80387 : boolean;[C];
  not used because of the underscore problem }

{$L fpu.o }


  function getenv(const envvar:string):string;
  { Copied here, preserves uses Dos (PFV) }
    var
      hp      : ppchar;
      hs,
      _envvar : string;
      eqpos,i : longint;
    begin
      _envvar:=upcase(envvar);
      hp:=environ;
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
         hp:=hp+4;
       end;
    end;

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
      {/* mask all exceptions, except invalid operation */}
        _control87($033e, $ffff)
      else
        begin
           {/* Flags value 3 means coprocessor emulation, exceptions to us */}
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

{
  $Log$
  Revision 1.2  1998-03-26 12:23:17  peter
    * emu387 doesn't uses dos anymore (getenv copied local)
    * makefile compilation order changed

  Revision 1.1.1.1  1998/03/25 11:18:42  root
  * Restored version

  Revision 1.6  1998/03/18 15:34:46  pierre
    + fpu state is restaured in excep_exit
      less risk of problems

  Revision 1.5  1998/02/05 17:24:09  pierre
    * bug in assembler code
    * changed default name to wmemu387.dxe

  Revision 1.4  1998/02/05 17:04:59  pierre
    * emulation is working with wmemu387.dxe

  Revision 1.3  1998/01/26 11:57:34  michael
  + Added log at the end

  Revision 1.2  1998/01/19 17:04:40  pierre
    * bug in dxe loading corrected, emu still does not work !!

  Revision 1.1  1998/01/16 16:53:15  pierre
      emu387 is a program based on npxset from DJGPP
      that loads the emu387.dxe if no FPU is present
      or if the env var 387 is set to N

}


{
  $Log$
  Revision 1.2  1998-03-26 12:23:17  peter
    * emu387 doesn't uses dos anymore (getenv copied local)
    * makefile compilation order changed

  Revision 1.1.1.1  1998/03/25 11:18:42  root
  * Restored version

  Revision 1.6  1998/03/18 15:34:46  pierre
    + fpu state is restaured in excep_exit
      less risk of problems

  Revision 1.5  1998/02/05 17:24:09  pierre
    * bug in assembler code
    * changed default name to wmemu387.dxe

  Revision 1.4  1998/02/05 17:04:59  pierre
    * emulation is working with wmemu387.dxe

  Revision 1.3  1998/01/26 11:57:34  michael
  + Added log at the end



  Working file: rtl/dos/go32v2/emu387.pp
  description:
  ----------------------------
  revision 1.2
  date: 1998/01/19 17:04:40;  author: pierre;  state: Exp;  lines: +11 -2
    * bug in dxe loading corrected, emu still does not work !!
  ----------------------------
  revision 1.1
  date: 1998/01/16 16:53:15;  author: pierre;  state: Exp;
      emu387 is a program based on npxset from DJGPP
      that loads the emu387.dxe if no FPU is present
      or if the env var 387 is set to N
  =============================================================================
}
