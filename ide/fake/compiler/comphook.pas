{
    $Id$

    Fake comphook unit
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
    currentsourcepath,
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
  Revision 1.1  2000-07-13 09:48:33  michael
  + Initial import

  Revision 1.1  1999/01/28 19:56:12  peter
    * moved to include compiler/gdb independent of each other

  Revision 1.2  1999/01/14 21:43:08  peter
    + status.currentsourcepath

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.1  1998/12/10 23:54:28  peter
    * initial version of the FV IDE
    * initial version of a fake compiler

}
