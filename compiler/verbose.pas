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

uses
  messages;

{$ifdef TP}
  {$define EXTERN_MSG}
{$endif}

{$ifndef EXTERN_MSG}
  {$i msgtxt.inc}
{$endif}

{$i msgidx.inc}


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

var
  msg : pmessage;

procedure SetRedirectFile(const fn:string);
function  SetVerbosity(const s:string):boolean;

procedure LoadMsgFile(const fn:string);
procedure UpdateReplacement(var s:string);

procedure Stop;
procedure ShowStatus;
procedure Internalerror(i:longint);
procedure Comment(l:longint;s:string);
procedure Message(w:tmsgconst);
procedure Message1(w:tmsgconst;const s1:string);
procedure Message2(w:tmsgconst;const s1,s2:string);
procedure Message3(w:tmsgconst;const s1,s2,s3:string);

procedure InitVerbose;


implementation
uses
  files,comphook,
  globals;

var
  redirexitsave : pointer;

{****************************************************************************
                       Extra Handlers for default compiler
****************************************************************************}

procedure DoneRedirectFile;{$ifndef FPC}far;{$ENDIF}
begin
  exitproc:=redirexitsave;
  if status.use_redir then
   close(status.redirfile);
end;


procedure SetRedirectFile(const fn:string);
begin
  assign(status.redirfile,fn);
  {$I-}
   rewrite(status.redirfile);
  {$I+}
  status.use_redir:=(ioresult=0);
  if status.use_redir then
   begin
     redirexitsave:=exitproc;
     exitproc:=@DoneRedirectFile;
   end;
end;


function SetVerbosity(const s:string):boolean;
var
  m : Longint;
  i : Word;
  inverse : boolean;
  c : char;
begin
  Setverbosity:=false;
  val(s,m,i);
  if (i=0) and (s<>'') then
   status.verbosity:=m
  else
   begin
     for i:=1 to length(s) do
       begin
          c:=upcase(s[i]);
          inverse:=false;
        { on/off ? }
          if (i<length(s)) then
           case s[i+1] of
            '-' : begin
                    inc(i);
                    inverse:=true;
                  end;
            '+' : inc(i);
           end;
         { handle switch }
          case c of
          { Special cases }
           'A' : status.verbosity:=V_All;
           '0' : status.verbosity:=V_Default;
           'R' : begin
                    if inverse then
                      begin
                         status.use_gccoutput:=false;
                         status.use_stderr:=false;
                      end
                    else
                      begin
                         status.use_gccoutput:=true;
                         status.use_stderr:=true;
                      end;
                 end;
          { Normal cases - do an or }
           'E' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Error)
                 else
                   status.verbosity:=status.verbosity or V_Error;
           'I' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Info)
                 else
                   status.verbosity:=status.verbosity or V_Info;
           'W' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Warning)
                 else
                   status.verbosity:=status.verbosity or V_Warning;
           'N' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Note)
                 else
                   status.verbosity:=status.verbosity or V_Note;
           'H' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Hint)
                 else
                   status.verbosity:=status.verbosity or V_Hint;
           'L' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Status)
                 else
                   status.verbosity:=status.verbosity or V_Status;
           'U' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Used)
                 else
                   status.verbosity:=status.verbosity or V_Used;
           'T' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Tried)
                 else
                   status.verbosity:=status.verbosity or V_Tried;
           'M' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Macro)
                 else
                   status.verbosity:=status.verbosity or V_Macro;
           'P' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Procedure)
                 else
                   status.verbosity:=status.verbosity or V_Procedure;
           'C' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Conditional)
                 else
                   status.verbosity:=status.verbosity or V_Conditional;
           'D' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Debug)
                 else
                   status.verbosity:=status.verbosity or V_Debug;
           end;
       end;
     end;
  if status.verbosity=0 then
   status.verbosity:=V_Default;
  setverbosity:=true;
end;


procedure LoadMsgFile(const fn:string);
begin
  if not(msg=nil) then
   dispose(msg,Done);
  msg:=new(pmessage,InitExtern(fn,ord(endmsgconst)));
end;


procedure UpdateReplacement(var s:string);
begin
  Replace(s,'$FPCVER',version_string);
  Replace(s,'$FPCDATE',date_string);
  Replace(s,'$FPCTARGET',target_string);
end;


var
  lastfileidx,
  lastmoduleidx : longint;
Procedure UpdateStatus;
begin
{ fix status }
  status.currentline:=aktfilepos.line;
  status.currentcolumn:=aktfilepos.column;
  if assigned(current_module) and
     ((current_module^.unit_index<>lastmoduleidx) or
      (aktfilepos.fileindex<>lastfileidx)) then
   begin
     status.currentsource:=current_module^.sourcefiles.get_file_name(aktfilepos.fileindex);
     lastmoduleidx:=current_module^.unit_index;
     { update lastfileidx only if name known PM }
     if status.currentsource<>'' then
       lastfileidx:=aktfilepos.fileindex
     else
       lastfileidx:=0;
   end;
end;


procedure stop;
begin
{$ifndef TP}
  do_stop();
{$else}
  do_stop;
{$endif}
end;


procedure ShowStatus;
begin
  UpdateStatus;
{$ifndef TP}
  if do_status() then
   stop;
{$else}
  if do_status then
   stop;
{$endif}
end;


procedure internalerror(i : longint);
begin
  do_internalerror(i);
  stop;
end;


procedure Comment(l:longint;s:string);
var
  dostop : boolean;
begin
  dostop:=((l and V_Fatal)<>0);
  if (l and V_Error)<>0 then
   inc(status.errorcount);
{ Create status info }
  UpdateStatus;
{ Fix replacements }
  UpdateReplacement(s);
{ show comment }
  if do_comment(l,s) or dostop or (status.errorcount>=status.maxerrorcount) then
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
   v:=V_Normal
  else
   if (idx in [1..5]) then
    begin
      for i:=1 to idx do
       begin
         case upcase(s[i]) of
          'F' : begin
                  v:=v or V_Fatal;
                  inc(status.errorcount);
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
{ fix status }
  UpdateStatus;
{ Fix replacements }
  UpdateReplacement(s);
{ show comment }
  if do_comment(v,s) or dostop or (status.errorcount>=status.maxerrorcount) then
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


procedure InitVerbose;
begin
{ Init }
  FillChar(Status,sizeof(TCompilerStatus),0);
  status.verbosity:=V_Default;
  Status.MaxErrorCount:=50;
end;

begin
{$ifndef EXTERN_MSG}
  msg:=new(pmessage,Init(@msgtxt,ord(endmsgconst)));
{$else}
  LoadMsgFile(exepath+'errore.msg');
{$endif}
end.

{
  $Log$
  Revision 1.19  1998-09-01 12:49:52  peter
    * better setverbosity to support W+/W- etc.

  Revision 1.18  1998/08/29 13:52:40  peter
    + new messagefile
    * merged optione.msg into errore.msg

  Revision 1.17  1998/08/19 14:57:52  peter
    * small fix for aktfilepos

  Revision 1.16  1998/08/18 14:17:15  pierre
    * bug about assigning the return value of a function to
      a procvar fixed : warning
      assigning a proc to a procvar need @ in FPC mode !!
    * missing file/line info restored

  Revision 1.15  1998/08/18 09:24:49  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.14  1998/08/11 14:09:15  peter
    * fixed some messages and smaller msgtxt.inc

  Revision 1.13  1998/08/10 14:50:37  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.12  1998/08/10 10:18:37  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

  Revision 1.11  1998/07/14 14:47:13  peter
    * released NEWINPUT

  Revision 1.10  1998/07/07 12:32:56  peter
    * status.currentsource is now calculated in verbose (more accurated)

  Revision 1.9  1998/07/07 11:20:20  peter
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
