{
    $Id$
    Copyright (c) 1998 by Peter Vreman

    This unit handles the default verbose routines

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit verb_def;
interface
uses verbose;

procedure SetRedirectFile(const fn:string);

procedure _stop;
Function  _comment(Level:Longint;const s:string):boolean;
function  _internalerror(i : longint) : boolean;

implementation
uses
  strings,dos,globals,files;

const
  { RHIDE expect gcc like error output }
  rh_errorstr='error: ';
  rh_warningstr='warning: ';
  fatalstr='Fatal: ';
  errorstr='Error: ';
  warningstr='Warning: ';
  notestr='Note: ';
  hintstr='Hint: ';

var
  redirexitsave : pointer;
  redirtext : boolean;
  redirfile : text;

{****************************************************************************
                       Extra Handlers for default compiler
****************************************************************************}

procedure DoneRedirectFile;{$ifndef FPC}far;{$ENDIF}
begin
  exitproc:=redirexitsave;
  if redirtext then
   close(redirfile);
end;


procedure SetRedirectFile(const fn:string);
begin
  assign(redirfile,fn);
  {$I-}
   rewrite(redirfile);
  {$I+}
  redirtext:=(ioresult=0);
  if redirtext then
   begin
     redirexitsave:=exitproc;
     exitproc:=@DoneRedirectFile;
   end;
end;


{****************************************************************************
                         Predefined default Handlers
****************************************************************************}


{ predefined handler to stop the compiler }
procedure _stop;
begin
  halt(1);
end;


Function _comment(Level:Longint;const s:string):boolean;
var
  hs : string;
begin
  _comment:=false; { never stop }
  if (verbosity and Level)=Level then
   begin
   { Status info?, Called every line }
     if ((Level and V_Status)<>0) and (s='') then
      begin
        if (abslines=1) then
          WriteLn(memavail shr 10,' Kb Free');
        if (status.currentline mod 100=0) then
          Write(status.currentline,' ',memavail shr 10,' Kb Free'#13);
      end
     else
   { Message }
      begin
        hs:='';
        if not(use_rhide) then
          begin
            if (verbosity and Level)=V_Hint then
              hs:=hintstr;
            if (verbosity and Level)=V_Note then
              hs:=notestr;
            if (verbosity and Level)=V_Warning then
              hs:=warningstr;
            if (verbosity and Level)=V_Error then
              hs:=errorstr;
            if (verbosity and Level)=V_Fatal then
              hs:=fatalstr;
          end
        else
          begin
            if (verbosity and Level)=V_Hint then
              hs:=rh_warningstr;
            if (verbosity and Level)=V_Note then
              hs:=rh_warningstr;
            if (verbosity and Level)=V_Warning then
              hs:=rh_warningstr;
            if (verbosity and Level)=V_Error then
              hs:=rh_errorstr;
            if (verbosity and Level)=V_Fatal then
              hs:=rh_errorstr;
          end;
        if (Level<$100) and Assigned(current_module) and Assigned(current_module^.current_inputfile) then
          hs:=current_module^.current_inputfile^.get_file_line+' '+hs;
      { add the message to the text }
        hs:=hs+s;
{$ifdef FPC}
        if UseStdErr and (Level<$100) then
         begin
           writeln(stderr,hs);
           flush(stderr);
         end
        else
{$endif}
         begin
           if redirtext then
            writeln(redirfile,hs)
           else
            writeln(hs);
         end;
      end;
   end;
end;


function _internalerror(i : longint) : boolean;
begin
  _comment(V_Fatal,'Internal error '+tostr(i));
  _internalerror:=true;
end;


begin
{$ifdef FPC}
  do_stop:=@_stop;
  do_comment:=@_comment;
  do_internalerror:=@_internalerror;
{$else}
  do_stop:=_stop;
  do_comment:=_comment;
  do_internalerror:=_internalerror;
{$endif}
end.
{
  $Log$
  Revision 1.8  1998-05-21 19:33:38  peter
    + better procedure directive handling and only one table

  Revision 1.7  1998/05/12 10:47:01  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.6  1998/05/11 13:07:58  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.5  1998/04/30 15:59:43  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.4  1998/04/29 10:34:09  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions
}
