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

{$define allow_oldstyle}

{$IFNDEF EXTERN_MSG}
  {$i msgtxt.inc}
{$ENDIF}

{$i msgidx.inc}

Const
  MaxErrorCount : longint = 50;
{ <$100 can include file and linenr info }
  V_Fatal       = $0;
  V_Error       = $1;
  V_Warning     = $2;
  V_Note        = $4;
  V_Hint        = $8;
  V_Info        = $100;
  V_Linenrs     = $200;
  V_Used        = $400;
  V_Tried       = $800;
  V_Macro       = $1000;
  V_Procedure   = $2000;
  V_Conditional = $4000;
  V_Debug       = $8000;

  V_All         = $ffffffff;
  V_Default     = V_Error;

  Verbosity     : longint=V_Default;

var
  errorcount    : longint;  { number of generated errors }
  msg           : pmessage;

procedure LoadMsgFile(const fn:string);
function  SetVerbosity(const s:string):boolean;

procedure stop;
procedure comment(l:longint;const s:string);
procedure internalerror(i:longint);
procedure Message(w:tmsgconst);
procedure Message1(w:tmsgconst;const s1:string);
procedure Message2(w:tmsgconst;const s1,s2:string);
procedure Message3(w:tmsgconst;const s1,s2,s3:string);

{ old calling style }
{$ifdef allow_oldstyle}
var
  exterror      : pchar;
procedure note(w:tmsgconst);
procedure warning(w:tmsgconst);
procedure error(w:tmsgconst);
procedure fatalerror(w:tmsgconst);
{$endif}

{ Function redirecting for IDE support }
type
  tstopprocedure = procedure;
  tcommentprocedure = procedure(Level:Longint;const s:string);
{old handlers }
  terrorfunction = function(w:tmsgconst) : boolean;
  tinternalerrorfunction = function(i : longint) : boolean;
var
{ this procedure is called to stop the compiler                 }
{ e.g. this procedure has to restore the state before compiling }
  do_stop : tstopprocedure;

{ called when writing something to the screen, called with the level }
{ of the comment }
  do_comment : tcommentprocedure;

{ only for compatibility }
  do_note,do_warning,do_error,do_fatalerror : terrorfunction;
  do_internalerror : tinternalerrorfunction;


implementation
uses globals;


procedure LoadMsgFile(const fn:string);
begin
  if not (msg=nil) then
   dispose(msg,Done);
  msg:=new(pmessage,InitExtern(fn,ord(endmsgconst)));
end;


function SetVerbosity(const s:string):boolean;
var
  m : Longint;
  c : Word;
begin
  setverbosity:=false;
  val(s,m,c);
  if (c=0) and (s<>'') then
   verbosity:=m
  else
   begin
     for c:=1 to length(s) do
      case upcase(s[c]) of
      { Special cases }
       'A' : Verbosity:=V_All;
       '0' : Verbosity:=V_Default;
      { Normal cases - do an or }
       'E' : Verbosity:=Verbosity or V_Error;
       'I' : Verbosity:=Verbosity or V_Info;
       'W' : Verbosity:=Verbosity or V_Warning;
       'N' : Verbosity:=Verbosity or V_Note;
       'H' : Verbosity:=Verbosity or V_Hint;
       'L' : Verbosity:=Verbosity or V_Linenrs;
       'U' : Verbosity:=Verbosity or V_Used;
       'T' : Verbosity:=Verbosity or V_Tried;
       'M' : Verbosity:=Verbosity or V_Macro;
       'P' : Verbosity:=Verbosity or V_Procedure;
       'C' : Verbosity:=Verbosity or V_Conditional;
       'D' : Verbosity:=Verbosity or V_Debug;
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
begin
  do_comment(l,s);
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
                  inc(errorcount);
                  dostop:=(errorcount>=maxerrorcount);
                end;
          'W' : v:=v or V_Warning;
          'N' : v:=v or V_Note;
          'H' : v:=v or V_Hint;
          'I' : v:=v or V_Info;
          'L' : v:=v or V_Linenrs;
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
  Comment(v,s);
  if dostop then
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


{*****************************************************************************
                                   Old Style
*****************************************************************************}

{$ifdef allow_oldstyle}

  procedure warning(w:tmsgconst);
  begin
    if do_warning(w) then
     stop;
  end;


  procedure note(w:tmsgconst);
  begin
    if do_note(w) then
     stop;
  end;


  procedure error(w:tmsgconst);
  begin
    inc(errorcount);
    if do_error(w) then
     stop;
  end;


  procedure fatalerror(w:tmsgconst);
  begin
    do_fatalerror(w);
    stop;
  end;

{$endif}

begin
{$IFNDEF EXTERN_MSG}
  msg:=new(pmessage,Init(@msgtxt,ord(endmsgconst)));
{$ENDIF}
end.

{
  $Log$
  Revision 1.4  1998-04-23 12:11:22  peter
    * fixed -v0 to displayV_Default (=errors+fatals)

  Revision 1.3  1998/04/13 21:15:42  florian
    * error handling of pass_1 and cgi386 fixed
    * the following bugs fixed: 0117, 0118, 0119 and 0129, 0122 was already
      fixed, verified

  Revision 1.2  1998/03/28 23:09:57  florian
    * secondin bugfix (m68k and i386)
    * overflow checking bugfix (m68k and i386) -- pretty useless in
      secondadd, since everything is done using 32-bit
    * loading pointer to routines hopefully fixed (m68k)
    * flags problem with calls to RTL internal routines fixed (still strcmp
      to fix) (m68k)
    * #ELSE was still incorrect (didn't take care of the previous level)
    * problem with filenames in the command line solved
    * problem with mangledname solved
    * linking name problem solved (was case insensitive)
    * double id problem and potential crash solved
    * stop after first error
    * and=>test problem removed
    * correct read for all float types
    * 2 sigsegv fixes and a cosmetic fix for Internal Error
    * push/pop is now correct optimized (=> mov (%esp),reg)

  Revision 1.1.1.1  1998/03/25 11:18:15  root
  * Restored version

  Revision 1.17  1998/03/10 16:43:34  peter
    * fixed Fatal error writting

  Revision 1.16  1998/03/10 01:17:30  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.15  1998/03/06 00:53:02  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.14  1998/03/04 01:35:15  peter
    * messages for unit-handling and assembler/linker
    * the compiler compiles without -dGDB, but doesn't work yet
    + -vh for Hint

  Revision 1.13  1998/03/03 16:45:25  peter
    + message support for assembler parsers

  Revision 1.12  1998/03/02 16:02:05  peter
    * new style messages for pp.pas
    * cleanup of pp.pas

  Revision 1.11  1998/03/02 01:49:40  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

}
