{
    $Id$
    Copyright (c) 1998 by the FPC development team

    This unit handles the verbose management

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
unit verbose;
interface

uses messages;

{$IFNDEF EXTERN_MSG}
  {$i msgtxt.inc}
{$ENDIF}

{$i msgidx.inc}

Const
  MaxErrorCount : longint = 50;
{ <$100 can include file and linenr info }
  V_Fatal       = $0;
  V_Error       = $1;
  V_Normal      = $2;
  V_Warning     = $4;
  V_Note        = $8;
  V_Hint        = $10;
  V_Info        = $100;
  V_Status      = $200;
  V_Used        = $400;
  V_Tried       = $800;
  V_Macro       = $1000;
  V_Procedure   = $2000;
  V_Conditional = $4000;
  V_Debug       = $8000;

  V_All         = $ffffffff;
  V_Default     = V_Fatal + V_Error + V_Normal;

  Verbosity     : longint=V_Default;

type
  TCompileStatus = record
    currentmodule,
    currentsource : string;   { filename }
    currentline,
    currentcolumn : longint;  { current line and column }
    compiledlines : longint;  { the number of lines which are compiled }
    errorcount    : longint;  { number of generated errors }
  end;


var
  status      : tcompilestatus;
  msg         : pmessage;
  UseStdErr,
  Use_Rhide   : boolean;


procedure LoadMsgFile(const fn:string);
function  SetVerbosity(const s:string):boolean;

procedure stop;
procedure comment(l:longint;const s:string);
procedure internalerror(i:longint);
procedure Message(w:tmsgconst);
procedure Message1(w:tmsgconst;const s1:string);
procedure Message2(w:tmsgconst;const s1,s2:string);
procedure Message3(w:tmsgconst;const s1,s2,s3:string);

{ Function redirecting for IDE support }
type
  tstopprocedure         = procedure;
  tcommentfunction       = function(Level:Longint;const s:string):boolean;
  tinternalerrorfunction = function(i:longint):boolean;
var
  do_stop          : tstopprocedure;
  do_comment       : tcommentfunction;
  do_internalerror : tinternalerrorfunction;


implementation
uses
  globals;

procedure LoadMsgFile(const fn:string);
begin
  if not (msg=nil) then
   dispose(msg,Done);
  msg:=new(pmessage,InitExtern(fn,ord(endmsgconst)));
end;


function SetVerbosity(const s:string):boolean;
var
  m : Longint;
  i : Word;
  inverse : boolean;
  c : char;
begin
  setverbosity:=false;
  val(s,m,i);
  if (i=0) and (s<>'') then
   verbosity:=m
  else
   begin
     for i:=1 to length(s) do
       begin
          c:=s[i];
          if (i<length(s)) and (s[i+1]='-') then
            begin
               inc(i);
               inverse:=true;
            end
          else
            inverse:=false;
          case upcase(s[i]) of
          { Special cases }
           'A' : Verbosity:=V_All;
           '0' : Verbosity:=V_Default;
           'R' : begin
                    if inverse then
                      begin
                         Use_rhide:=false;
                         UseStdErr:=false;
                      end
                    else
                      begin
                         Use_rhide:=true;
                         UseStdErr:=true;
                      end;
                 end;
          { Normal cases - do an or }
           'E' : if inverse then
                   Verbosity:=Verbosity and (not V_Error)
                 else
                   Verbosity:=Verbosity or V_Error;
           'I' : if inverse then
                   Verbosity:=Verbosity and (not V_Info)
                 else
                   Verbosity:=Verbosity or V_Info;
           'W' : if inverse then
                   Verbosity:=Verbosity and (not V_Warning)
                 else
                   Verbosity:=Verbosity or V_Warning;
           'N' : if inverse then
                   Verbosity:=Verbosity and (not V_Note)
                 else
                   Verbosity:=Verbosity or V_Note;
           'H' : if inverse then
                   Verbosity:=Verbosity and (not V_Hint)
                 else
                   Verbosity:=Verbosity or V_Hint;
           'L' : if inverse then
                   Verbosity:=Verbosity and (not V_Status)
                 else
                   Verbosity:=Verbosity or V_Status;
           'U' : if inverse then
                   Verbosity:=Verbosity and (not V_Used)
                 else
                   Verbosity:=Verbosity or V_Used;
           'T' : if inverse then
                   Verbosity:=Verbosity and (not V_Tried)
                 else
                   Verbosity:=Verbosity or V_Tried;
           'M' : if inverse then
                   Verbosity:=Verbosity and (not V_Macro)
                 else
                   Verbosity:=Verbosity or V_Macro;
           'P' : if inverse then
                   Verbosity:=Verbosity and (not V_Procedure)
                 else
                   Verbosity:=Verbosity or V_Procedure;
           'C' : if inverse then
                   Verbosity:=Verbosity and (not V_Conditional)
                 else
                   Verbosity:=Verbosity or V_Conditional;
           'D' : if inverse then
                   Verbosity:=Verbosity and (not V_Debug)
                 else
                   Verbosity:=Verbosity or V_Debug;
           end;
       end;
     end;
  if Verbosity=0 then
   Verbosity:=V_Default;
  setverbosity:=true;
end;


procedure stop;
begin
{$ifndef TP}
  do_stop();
{$else}
  do_stop;
{$endif}
end;


procedure internalerror(i : longint);
begin
  do_internalerror(i);
  stop;
end;


procedure Comment(l:longint;const s:string);
var
  dostop : boolean;
begin
  dostop:=((l and V_Fatal)<>0);
  if (l and V_Error)<>0 then
   inc(status.errorcount);
{ fix status }
{$ifdef NEWINPUT}
  status.currentline:=aktfilepos.line;
  status.currentcolumn:=aktfilepos.column;
{$endif}
{ show comment }
  if do_comment(l,s) or dostop or (status.errorcount>=maxerrorcount) then
   stop
end;


Procedure Msg2Comment(s:string);
var
  idx,i,v : longint;
  dostop  : boolean;
begin
{Reset}
  dostop:=false;
  v:=0;
{Parse options}
  idx:=pos('_',s);
  if idx=0 then
   v:=V_Default
  else
   if (idx in [1..5]) then
    begin
      for i:=1 to idx do
       begin
         case upcase(s[i]) of
          'F' : begin
                  v:=v or V_Fatal;
                  dostop:=true;
                end;
          'E' : begin
                  v:=v or V_Error;
                  inc(status.errorcount);
                end;
          'O' : v:=v or V_Normal;
          'W' : v:=v or V_Warning;
          'N' : v:=v or V_Note;
          'H' : v:=v or V_Hint;
          'I' : v:=v or V_Info;
          'L' : v:=v or V_Status;
          'U' : v:=v or V_Used;
          'T' : v:=v or V_Tried;
          'M' : v:=v or V_Macro;
          'P' : v:=v or V_Procedure;
          'C' : v:=v or V_Conditional;
          'D' : v:=v or V_Debug;
          'S' : dostop:=true;
          '_' : ;
         end;
       end;
    end;
  Delete(s,1,idx);
  Replace(s,'$VER',version_string);
  Replace(s,'$TARGET',target_string);
{ fix status }
{$ifdef NEWINPUT}
  status.currentline:=aktfilepos.line;
  status.currentcolumn:=aktfilepos.column;
{$endif}
{ show comment }
  if do_comment(v,s) or dostop or (status.errorcount>=maxerrorcount) then
   stop;
end;


procedure Message(w:tmsgconst);
begin
  Msg2Comment(msg^.Get(ord(w)));
end;


procedure Message1(w:tmsgconst;const s1:string);
begin
  Msg2Comment(msg^.Get1(ord(w),s1));
end;


procedure Message2(w:tmsgconst;const s1,s2:string);
begin
  Msg2Comment(msg^.Get2(ord(w),s1,s2));
end;


procedure Message3(w:tmsgconst;const s1,s2,s3:string);
begin
  Msg2Comment(msg^.Get3(ord(w),s1,s2,s3));
end;


begin
{$IFNDEF EXTERN_MSG}
  msg:=new(pmessage,Init(@msgtxt,ord(endmsgconst)));
{$ENDIF}
end.

{
  $Log$
  Revision 1.9  1998-07-07 11:20:20  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.8  1998/05/23 01:21:35  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.7  1998/05/21 19:33:40  peter
    + better procedure directive handling and only one table

  Revision 1.6  1998/05/12 10:47:01  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.5  1998/04/30 15:59:43  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.4  1998/04/23 12:11:22  peter
    * fixed -v0 to displayV_Default (=errors+fatals)

  Revision 1.3  1998/04/13 21:15:42  florian
    * error handling of pass_1 and cgi386 fixed
    * the following bugs fixed: 0117, 0118, 0119 and 0129, 0122 was already
      fixed, verified
}
