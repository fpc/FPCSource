{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

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
  messages,cobjects;

{$ifdef TP}
  {$define EXTERN_MSG}
{$endif}

{$ifndef EXTERN_MSG}
  {$i msgtxt.inc}
{$endif}

{$i msgidx.inc}


Const
{ <$10000 will show file and line }
  V_None         = $0;
  V_Fatal        = $1;
  V_Error        = $2;
  V_Normal       = $4; { doesn't show a text like Error: }
  V_Warning      = $8;
  V_Note         = $10;
  V_Hint         = $20;
  V_Macro        = $100;
  V_Procedure    = $200;
  V_Conditional  = $400;
  V_Assem        = $800;
  V_Info         = $10000;
  V_Status       = $20000;
  V_Used         = $40000;
  V_Tried        = $80000;
  V_Debug        = $100000;
  V_Declarations = $200000;
  V_Executable   = $400000;
  V_ShowFile     = $ffff;
  V_All          = $ffffffff;
  V_Default      = V_Fatal + V_Error + V_Normal;

var
  msg : pmessage;

procedure SetRedirectFile(const fn:string);
function  SetVerbosity(const s:string):boolean;

procedure LoadMsgFile(const fn:string);

procedure Stop;
procedure ShowStatus;
function  ErrorCount:longint;
procedure SetMaxErrorCount(count:longint);
procedure GenerateError;
procedure Internalerror(i:longint);
procedure Comment(l:longint;s:string);
procedure Message(w:tmsgconst);
procedure Message1(w:tmsgconst;const s1:string);
procedure Message2(w:tmsgconst;const s1,s2:string);
procedure Message3(w:tmsgconst;const s1,s2,s3:string);
procedure MessagePos(const pos:tfileposinfo;w:tmsgconst);
procedure MessagePos1(const pos:tfileposinfo;w:tmsgconst;const s1:string);
procedure MessagePos2(const pos:tfileposinfo;w:tmsgconst;const s1,s2:string);
procedure MessagePos3(const pos:tfileposinfo;w:tmsgconst;const s1,s2,s3:string);

procedure InitVerbose;
procedure DoneVerbose;


implementation
uses
  files,comphook,
  version,globals;

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
  i : Integer;
  inverse : boolean;
  c : char;
begin
  Setverbosity:=false;
  val(s,m,i);
  if (i=0) and (s<>'') then
   status.verbosity:=m
  else
   begin
     i:=1;
     while i<=length(s) do
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
           'B' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Declarations)
                 else
                   status.verbosity:=status.verbosity or V_Declarations;
           'X' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Executable)
                 else
                   status.verbosity:=status.verbosity or V_Executable;
           'Z' : if inverse then
                   status.verbosity:=status.verbosity and (not V_Assem)
                 else
                   status.verbosity:=status.verbosity or V_Assem;
           end;
          inc(i);
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


var
  lastfileidx,
  lastmoduleidx : longint;
Procedure UpdateStatus;
begin
{ fix status }
  status.currentline:=aktfilepos.line;
  status.currentcolumn:=aktfilepos.column;
  if assigned(current_module) and assigned(current_module^.sourcefiles) and
     ((current_module^.unit_index<>lastmoduleidx) or
      (aktfilepos.fileindex<>lastfileidx)) then
   begin
     { update status record }
     status.currentmodule:=current_module^.modulename^;
     status.currentsource:=current_module^.sourcefiles^.get_file_name(aktfilepos.fileindex);
     status.currentsourcepath:=current_module^.sourcefiles^.get_file_path(aktfilepos.fileindex);
     { update lastfileidx only if name known PM }
     if status.currentsource<>'' then
       lastfileidx:=aktfilepos.fileindex
     else
       lastfileidx:=0;
     lastmoduleidx:=current_module^.unit_index;
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


function ErrorCount:longint;
begin
  ErrorCount:=status.errorcount;
end;


procedure SetMaxErrorCount(count:longint);
begin
  status.maxerrorcount:=count;
end;


procedure GenerateError;
begin
  inc(status.errorcount);
end;


procedure internalerror(i : longint);
begin
  UpdateStatus;
  do_internalerror(i);
  inc(status.errorcount);
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
  DefaultReplacements(s);
{ show comment }
  if do_comment(l,s) or dostop then
   stop;
  if (status.errorcount>=status.maxerrorcount) and not status.skip_error then
   begin
     Message1(unit_f_errors_in_unit,tostr(status.errorcount));
     status.skip_error:=true;
     stop;
   end;
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
   if (idx >= 1) And (idx <= 5) then
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

          'W':
            v:=v or V_Warning;

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
          'B' : v:=v or V_Declarations;
          'X' : v:=v or V_Executable;
          'Z' : v:=v or V_Assem;
          'S' : dostop:=true;
          '_' : ;
         end;
       end;
    end;
  Delete(s,1,idx);
{ fix status }
  UpdateStatus;
{ Fix replacements }
  DefaultReplacements(s);
{ show comment }
  if do_comment(v,s) or dostop then
   stop;
  if (status.errorcount>=status.maxerrorcount) and not status.skip_error then
   begin
     Message1(unit_f_errors_in_unit,tostr(status.errorcount));
     status.skip_error:=true;
     stop;
   end;
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


procedure MessagePos(const pos:tfileposinfo;w:tmsgconst);
var
  oldpos : tfileposinfo;
begin
  oldpos:=aktfilepos;
  aktfilepos:=pos;
  Msg2Comment(msg^.Get(ord(w)));
  aktfilepos:=oldpos;
end;


procedure MessagePos1(const pos:tfileposinfo;w:tmsgconst;const s1:string);
var
  oldpos : tfileposinfo;
begin
  oldpos:=aktfilepos;
  aktfilepos:=pos;
  Msg2Comment(msg^.Get1(ord(w),s1));
  aktfilepos:=oldpos;
end;


procedure MessagePos2(const pos:tfileposinfo;w:tmsgconst;const s1,s2:string);
var
  oldpos : tfileposinfo;
begin
  oldpos:=aktfilepos;
  aktfilepos:=pos;
  Msg2Comment(msg^.Get2(ord(w),s1,s2));
  aktfilepos:=oldpos;
end;


procedure MessagePos3(const pos:tfileposinfo;w:tmsgconst;const s1,s2,s3:string);
var
  oldpos : tfileposinfo;
begin
  oldpos:=aktfilepos;
  aktfilepos:=pos;
  Msg2Comment(msg^.Get3(ord(w),s1,s2,s3));
  aktfilepos:=oldpos;
end;


procedure InitVerbose;
begin
{ Init }
{$ifndef EXTERN_MSG}
  msg:=new(pmessage,Init(@msgtxt,ord(endmsgconst)));
{$else}
  LoadMsgFile(exepath+'errore.msg');
{$endif}
  FillChar(Status,sizeof(TCompilerStatus),0);
  status.verbosity:=V_Default;
  Status.MaxErrorCount:=50;
end;

procedure DoneVerbose;
begin
  if assigned(msg) then
   begin
     dispose(msg,Done);
     msg:=nil;
   end;
end;

end.

{
  $Log$
  Revision 1.45  2000-02-09 13:23:09  peter
    * log truncated

  Revision 1.44  2000/01/07 01:14:49  peter
    * updated copyright to 2000

  Revision 1.43  1999/11/06 14:34:32  peter
    * truncated log to 20 revs

  Revision 1.42  1999/08/05 16:53:28  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

}
