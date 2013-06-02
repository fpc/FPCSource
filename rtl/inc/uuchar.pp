{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2011 by the Free Pascal development team

    This unit redefines the Char type from ansichar into widechar

    See the file COPYING.FPC, included in this distribution,
    For details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit uuchar;

interface

  type
    char = widechar;
    pchar = pwidechar;


Function ParamStr(Param : Integer) : UnicodeString;

implementation

{$if defined(FPC_HAS_FEATURE_COMMANDARGS) }
Function ParamStr(Param : Integer) : unicodestring;

Var Len : longint;
    s   : ansistring;
begin
{
  Paramstr(0) should return the name of the binary.
  Since this functionality is included in the system unit,
  we fetch it from there.
  Normally, pathnames are less than 255 chars anyway,
  so this will work correct in 99% of all cases.
  In time, the system unit should get a GetExeName call.
}
  if (Param=0) then
    Paramstr:=System.Paramstr(0)
  else if (Param>0) and (Param<argc) then
    begin
    Len:=0;
    While Argv[Param][Len]<>#0 do
      Inc(len);
    SetLength(s,Len);
    If Len>0 then
      Move(Argv[Param][0],s[1],Len);
     paramstr:=s;
    end
  else
    paramstr:='';
end;
{$endif FPC_HAS_FEATURE_COMMANDARGS}

end.
