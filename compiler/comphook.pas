{
    $Id$
    Copyright (c) 1998 by Peter Vreman

    This unit handles the compilerhooks for output to external programs

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
unit comphook;
interface

Const
{ <$10000 will show file and line }
  V_Fatal       = $0;
  V_Error       = $1;
  V_Normal      = $2; { doesn't show a text like Error: }
  V_Warning     = $4;
  V_Note        = $8;
  V_Hint        = $10;
  V_Macro       = $100;
  V_Procedure   = $200;
  V_Conditional = $400;
  V_Info        = $10000;
  V_Status      = $20000;
  V_Used        = $40000;
  V_Tried       = $80000;
  V_Debug       = $100000;

  V_ShowFile    = $ffff;
  V_All         = $ffffffff;
  V_Default     = V_Fatal + V_Error + V_Normal;

type
  PCompilerStatus = ^TCompilerStatus;
  TCompilerStatus = record
  { Current status }
    currentmodule,
    currentsource : string;   { filename }
    currentline,
    currentcolumn : longint;  { current line and column }
  { Total Status }
    compiledlines : longint;  { the number of lines which are compiled }
    errorcount    : longint;  { number of generated errors }
  { Settings for the output }
    verbosity     : longint;
    maxerrorcount : longint;
    skip_error,
    use_stderr,
    use_redir,
    use_gccoutput : boolean;
  { Redirection support }
    redirfile : text;
  end;
var
  status : tcompilerstatus;

{ Default Functions }
procedure def_stop;
Function  def_status:boolean;
Function  def_comment(Level:Longint;const s:string):boolean;
function  def_internalerror(i:longint):boolean;

{ Function redirecting for IDE support }
type
  tstopprocedure         = procedure;
  tstatusfunction        = function:boolean;
  tcommentfunction       = function(Level:Longint;const s:string):boolean;
  tinternalerrorfunction = function(i:longint):boolean;
const
  do_stop          : tstopprocedure   = def_stop;
  do_status        : tstatusfunction  = def_status;
  do_comment       : tcommentfunction = def_comment;
  do_internalerror : tinternalerrorfunction = def_internalerror;



implementation

{$ifdef USEEXCEPT}
  uses tpexcept;
{$endif USEEXCEPT}

{****************************************************************************
                          Helper Routines
****************************************************************************}

function gccfilename(const s : string) : string;
var
  i : longint;
begin
  for i:=1to length(s) do
   begin
     case s[i] of
      '\' : gccfilename[i]:='/';
 'A'..'Z' : gccfilename[i]:=chr(ord(s[i])+32);
     else
      gccfilename[i]:=s[i];
     end;
   end;
  {$ifndef TP}
    {$ifopt H+}
      setlength(gccfilename,length(s));
    {$else}
      gccfilename[0]:=s[0];
    {$endif}
  {$else}
    gccfilename[0]:=s[0];
  {$endif}
end;


function tostr(i : longint) : string;
var
  hs : string;
begin
  str(i,hs);
  tostr:=hs;
end;


{****************************************************************************
                         Predefined default Handlers
****************************************************************************}

{ predefined handler when then compiler stops }
procedure def_stop;
begin
{$ifndef USEEXCEPT}
  Halt(1);
{$else USEEXCEPT}
  Halt(1);
{$endif USEEXCEPT}
end;


function def_status:boolean;
begin
  def_status:=false; { never stop }
{ Status info?, Called every line }
  if ((status.verbosity and V_Status)<>0) then
   begin
     if (status.compiledlines=1) then
       WriteLn(memavail shr 10,' Kb Free');
     if (status.currentline>0) and (status.currentline mod 100=0) then
{$ifdef FPC}
       WriteLn(status.currentline,' ',memavail shr 10,'/',system.heapsize shr 10,' Kb Free');
{$else}
       WriteLn(status.currentline,' ',memavail shr 10,' Kb Free');
{$endif}
   end
end;


Function def_comment(Level:Longint;const s:string):boolean;
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
  hs : string;
begin
  def_comment:=false; { never stop }
  if (status.verbosity and Level)=Level then
   begin
     hs:='';
     if not(status.use_gccoutput) then
       begin
         if (status.verbosity and Level)=V_Hint then
           hs:=hintstr;
         if (status.verbosity and Level)=V_Note then
           hs:=notestr;
         if (status.verbosity and Level)=V_Warning then
           hs:=warningstr;
         if (status.verbosity and Level)=V_Error then
           hs:=errorstr;
         if (status.verbosity and Level)=V_Fatal then
           hs:=fatalstr;
       end
     else
       begin
         if (status.verbosity and Level)=V_Hint then
           hs:=rh_warningstr;
         if (status.verbosity and Level)=V_Note then
           hs:=rh_warningstr;
         if (status.verbosity and Level)=V_Warning then
           hs:=rh_warningstr;
         if (status.verbosity and Level)=V_Error then
           hs:=rh_errorstr;
         if (status.verbosity and Level)=V_Fatal then
           hs:=rh_errorstr;
       end;
     if (Level<=V_ShowFile) and (status.currentsource<>'') and (status.currentline>0) then
      begin
        { Adding the column should not confuse RHIDE,
        even if it does not yet use it PM
        but only if it is after error or warning !! PM }
        if status.currentcolumn>0 then
         begin
           if status.use_gccoutput then
             hs:=gccfilename(status.currentsource)+':'+tostr(status.currentline)+': '+hs
                 +tostr(status.currentcolumn)+': '
           else
             hs:=status.currentsource+'('+tostr(status.currentline)
                 +','+tostr(status.currentcolumn)+') '+hs;
         end
        else
         begin
           if status.use_gccoutput then
             hs:=gccfilename(status.currentsource)+': '+hs+tostr(status.currentline)+': '
           else
             hs:=status.currentsource+'('+tostr(status.currentline)+') '+hs;
         end;
      end;
   { add the message to the text }
     hs:=hs+s;
{$ifdef FPC}
     if status.use_stderr then
      begin
        writeln(stderr,hs);
        flush(stderr);
      end
     else
{$endif}
      begin
        if status.use_redir then
         writeln(status.redirfile,hs)
        else
         writeln(hs);
      end;
   end;
end;


function def_internalerror(i : longint) : boolean;
begin
  do_comment(V_Fatal,'Internal error '+tostr(i));
  def_internalerror:=true;
end;




end.
{
  $Log$
  Revision 1.12  1998-11-16 15:41:39  peter
    * tp7 didn't like my ifopt H+ :(

  Revision 1.11  1998/11/16 12:17:59  peter
    * H+ fixes

  Revision 1.10  1998/10/27 13:45:25  pierre
    * classes get a vmt allways
    * better error info (tried to remove
      several error strings introduced by the tpexcept handling)

  Revision 1.9  1998/10/26 17:15:16  pierre
    + added two level of longjump to
      allow clean freeing of used memory on errors

  Revision 1.8  1998/09/15 10:49:32  pierre
  merged from fixes branch

  Revision 1.7.2.1  1998/09/15 10:30:17  pierre
  RHIDE output corrected

  Revision 1.7  1998/09/04 17:34:21  pierre
    * bug with datalabel corrected
    + assembler errors better commented
    * one nested record crash removed

  Revision 1.6  1998/09/01 17:37:59  peter
    * nicer output when column=0

  Revision 1.5  1998/08/18 15:11:51  peter
    * recompiles again

  Revision 1.4  1998/08/18 14:17:08  pierre
    * bug about assigning the return value of a function to
      a procvar fixed : warning
      assigning a proc to a procvar need @ in FPC mode !!
    * missing file/line info restored

  Revision 1.3  1998/08/18 09:24:40  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.2  1998/08/11 14:02:45  peter
    * don't write line if no sourcefile is set

  Revision 1.1  1998/08/10 10:18:24  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

  Revision 1.14  1998/08/04 13:22:48  pierre
    * weird bug fixed :
      a pchar ' ' (simple space or any other letter) was found to
      be equal to a string of length zero !!!
      thus printing out non sense
      found that out while checking Control-C !!
    + added column info also in RHIDE format as
      it might be usefull later

  Revision 1.13  1998/07/14 14:47:12  peter
    * released NEWINPUT

  Revision 1.12  1998/07/07 11:20:19  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.11  1998/06/19 15:40:00  peter
    * bp7 fix

  Revision 1.10  1998/06/16 11:32:19  peter
    * small cosmetic fixes

  Revision 1.9  1998/05/23 01:21:33  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.8  1998/05/21 19:33:38  peter
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
