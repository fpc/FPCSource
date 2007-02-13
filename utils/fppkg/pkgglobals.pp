{$mode objfpc}
{$h+}
unit pkgglobals;

interface

uses
  SysUtils,
  Classes;

Const
{$ifdef unix}
  ExeExt = '';
{$else unix}
  ExeExt = '.exe';
{$endif unix}

Type
  TVerbosity = (vError,vInfo,vCommands,vDebug);
  TVerbosities = Set of TVerbosity;

  EPackagerError = class(Exception);
  
// Logging
Function StringToVerbosity (S : String) : TVerbosity;
Function VerbosityToString (V : TVerbosity): String;
Procedure Log(Level: TVerbosity;Msg : String);
Procedure Log(Level: TVerbosity;Fmt : String; const Args : array of const);
Procedure Error(Msg : String);
Procedure Error(Fmt : String; const Args : array of const);

// Utils
function maybequoted(const s:string):string;
Function FixPath(const S : String) : string;

var
  Verbosity : TVerbosities;


Implementation

uses
  typinfo,
  contnrs,
  uriparser,
  pkgmessages;

function StringToVerbosity(S: String): TVerbosity;
Var
  I : integer;
begin
  I:=GetEnumValue(TypeInfo(TVerbosity),'v'+S);
  If (I<>-1) then
    Result:=TVerbosity(I)
  else
    Raise EPackagerError.CreateFmt(SErrInvalidVerbosity,[S]);
end;

Function VerbosityToString (V : TVerbosity): String;
begin
  Result:=GetEnumName(TypeInfo(TVerbosity),Integer(V));
  Delete(Result,1,1);// Delete 'v'
end;


procedure Log(Level:TVerbosity;Msg: String);
begin
  if Level in Verbosity then
    Writeln(stdErr,Msg);
end;


Procedure Log(Level:TVerbosity; Fmt:String; const Args:array of const);
begin
  Log(Level,Format(Fmt,Args));
end;


procedure Error(Msg: String);
begin
  Raise EPackagerError.Create(Msg);
end;


procedure Error(Fmt: String; const Args: array of const);
begin
  Raise EPackagerError.CreateFmt(Fmt,Args);
end;


function maybequoted(const s:string):string;
const
  {$IFDEF MSWINDOWS}
    FORBIDDEN_CHARS = ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
                       '{', '}', '''', '`', '~'];
  {$ELSE}
    FORBIDDEN_CHARS = ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
                       '{', '}', '''', ':', '\', '`', '~'];
  {$ENDIF}
var
  s1 : string;
  i  : integer;
  quoted : boolean;
begin
  quoted:=false;
  s1:='"';
  for i:=1 to length(s) do
   begin
     case s[i] of
       '"' :
         begin
           quoted:=true;
           s1:=s1+'\"';
         end;
       ' ',
       #128..#255 :
         begin
           quoted:=true;
           s1:=s1+s[i];
         end;
       else begin
         if s[i] in FORBIDDEN_CHARS then
           quoted:=True;
         s1:=s1+s[i];
       end;
     end;
   end;
  if quoted then
    maybequoted:=s1+'"'
  else
    maybequoted:=s;
end;


Function FixPath(const S : String) : string;
begin
  If (S<>'') then
    Result:=IncludeTrailingPathDelimiter(S)
  else
    Result:='';
end;



end.
