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

{$define allow_oldstyle}

var
  UseStdErr : boolean;
procedure SetRedirectFile(const fn:string);

procedure _stop;
procedure _comment(Level:Longint;const s:string);
{$ifdef allow_oldstyle}
function _warning(w : tmsgconst) : boolean;
function _note(w : tmsgconst) : boolean;
function _error(w : tmsgconst) : boolean;
function _fatalerror(w : tmsgconst) : boolean;
function _internalerror(i : longint) : boolean;
{$endif}

implementation
uses
  strings,dos,cobjects,systems,globals,files;

const
{$ifdef USE_RHIDE}
  { RHIDE expect gcc like error output }
  fatalstr='fatal: ';
  errorstr='error: ';
  warningstr='warning: ';
  notestr='warning: ';
  hintstr='warning: ';
{$else}
  fatalstr='Fatal Error: ';
  errorstr='Error: ';
  warningstr='Warning: ';
  notestr='Note: ';
  hintstr='Hint: ';
{$endif USE_RHIDE}

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


Procedure _comment(Level:Longint;const s:string);
var
  hs : string;
{$ifdef USE_RHIDE}
  i  : longint;
{$endif}
begin
  if (verbosity and Level)=Level then
   begin
   {Create hs}
     hs:='';
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
     if (Level<$100) and Assigned(current_module) and
        Assigned(current_module^.current_inputfile) then
      hs:=current_module^.current_inputfile^.get_file_line+' '+hs;
{$ifdef USE_RHIDE}
     if (Level<$100) then
      begin
        i:=length(hs)+1;
        hs:=hs+lowercase(Copy(s,1,5))+Copy(s,6,255);
      end
     else
{$endif USE_RHIDE}
      hs:=hs+s;
{$ifdef FPC}
     if UseStdErr and (Level<$100) then
      begin
        writeln(stderr,hs);
        flush(stderr);
      end
     else
{$ENDIF}
      begin
        if redirtext then
         writeln(redirfile,hs)
        else
         writeln(hs);
      end;
   end;
end;


function _internalerror(i : longint) : boolean;
var
  temp : string;
begin
  if assigned(current_module^.current_inputfile) then
   temp:=current_module^.current_inputfile^.get_file_line+': '
  else
   temp:='';
  comment(V_Error,temp+'Internal error '+tostr(i));
  _internalerror:=true;
end;

{****************************************************************************
                                 Old Style
****************************************************************************}


{$ifdef allow_oldstyle}

procedure ShowExtError(l:longint;w:tmsgconst);
var
  s : string;
begin
{fix the string to be written }
  s:=msg^.get(ord(w));
  if assigned(exterror) then
   begin
     s:=s+strpas(exterror);
     strdispose(exterror);
     exterror:=nil;
   end;
  _comment(l,s);
end;


{ predefined handler for warnings }
function _warning(w : tmsgconst) : boolean;
begin
  ShowExtError(V_Warning,w);
  _warning:=false;
end;


function _note(w : tmsgconst) : boolean;
begin
  ShowExtError(V_Note,w);
  _note:=false;
end;


function _error(w : tmsgconst) : boolean;
begin
  ShowExtError(V_Error,w);
  _error:=(errorcount>50);
end;


function _fatalerror(w : tmsgconst) : boolean;
begin
  ShowExtError(V_Error,w);
  _fatalerror:=true;
end;

{$endif}

begin
{$ifdef FPC}
  do_stop:=@_stop;
  do_comment:=@_comment;
  {$ifdef allow_oldstyle}
     do_note:=@_note;
     do_warning:=@_warning;
     do_error:=@_error;
     do_fatalerror:=@_fatalerror;
     do_internalerror:=@_internalerror;
  {$endif}
{$else}
  do_stop:=_stop;
  do_comment:=_comment;
  {$ifdef allow_oldstyle}
     do_note:=_note;
     do_warning:=_warning;
     do_error:=_error;
     do_fatalerror:=_fatalerror;
     do_internalerror:=_internalerror;
  {$endif}
{$endif}
end.
{
  $Log$
  Revision 1.1  1998-03-25 11:18:15  root
  Initial revision

  Revision 1.6  1998/03/10 16:43:34  peter
    * fixed Fatal error writting

  Revision 1.5  1998/03/10 01:17:30  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.4  1998/03/06 00:53:02  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.3  1998/03/04 17:34:15  michael
  + Changed ifdef FPK to ifdef FPC

  Revision 1.2  1998/03/03 16:45:25  peter
    + message support for assembler parsers

}
