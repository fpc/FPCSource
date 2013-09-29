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


{$ifdef FPC_HAS_FEATURE_COMMANDARGS}
Function ParamStr(Param: Longint): UnicodeString;
{$endif FPC_HAS_FEATURE_COMMANDARGS}

implementation

{$ifdef FPC_HAS_FEATURE_COMMANDARGS}
Function ParamStr(Param: Longint): UnicodeString;
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
      paramstr:=UnicodeString(Argv[Param])
    else
      paramstr:='';
  end;
{$endif FPC_HAS_FEATURE_COMMANDARGS}

end.
