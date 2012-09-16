{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements basic platform independent types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_types;

interface
uses
  Types;

{$IFDEF DELPHI}
  type
    QWord = type Int64;
    DWORD = LongWord;
    PtrInt = Integer;
    PtrUInt = Cardinal;
    SizeInt = Longint;
    PPtrInt  = ^PtrInt;
{$ENDIF}


{$IF NOT Defined(PPtrUInt)}
type
  PPtrUInt = ^PtrUInt;
{$IFEND}

{$IFDEF FPC}
type
  TStringBufferType = AnsiString;
{$ENDIF}

{$IFDEF DELPHI}
  {$IFDEF HAS_UNICODE}
    UnicodeChar = WideChar;
    TStringBufferType = RawByteString;
  {$ELSE HAS_UNICODE}
    TStringBufferType = AnsiString;
  {$ENDIF HAS_UNICODE}
{$ENDIF}
  TBinaryString = TStringBufferType;

  TByteDynArray = Types.TByteDynArray;
  
implementation

end.
