{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe
    member of the Free Pascal development team.

    This file implements the helper routines for TObject

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

type
  TObject = class(JLObject)
   strict private
    DestructorCalled: Boolean;
   public
    procedure Free;
    destructor Destroy; virtual;
    procedure finalize; override;
  end;
  TClass = class of TObject;

   {$ifndef nounsupported}
   const
      vtInteger       = 0;
      vtBoolean       = 1;
      vtChar          = 2;
   {$ifndef FPUNONE}
      vtExtended      = 3;
   {$endif}
      vtString        = 4;
      vtPointer       = 5;
      vtPChar         = 6;
      vtObject        = 7;
      vtClass         = 8;
      vtWideChar      = 9;
      vtPWideChar     = 10;
      vtAnsiString    = 11;
      vtCurrency      = 12;
      vtVariant       = 13;
      vtInterface     = 14;
      vtWideString    = 15;
      vtInt64         = 16;
      vtQWord         = 17;
      vtUnicodeString = 18;

   type
     TVarRec = record
        case VType : sizeint of
   {$ifdef ENDIAN_BIG}
          vtInteger       : ({$IFDEF CPU64}integerdummy1 : Longint;{$ENDIF CPU64}VInteger: Longint);
          vtBoolean       : ({$IFDEF CPU64}booldummy : Longint;{$ENDIF CPU64}booldummy1,booldummy2,booldummy3: byte; VBoolean: Boolean);
          vtChar          : ({$IFDEF CPU64}chardummy : Longint;{$ENDIF CPU64}chardummy1,chardummy2,chardummy3: byte; VChar: Char);
          vtWideChar      : ({$IFDEF CPU64}widechardummy : Longint;{$ENDIF CPU64}wchardummy1,VWideChar: WideChar);
   {$else ENDIAN_BIG}
          vtInteger       : (VInteger: Longint);
          vtBoolean       : (VBoolean: Boolean);
          vtChar          : (VChar: Char);
          vtWideChar      : (VWideChar: WideChar);
   {$endif ENDIAN_BIG}
   //       vtString        : (VString: PShortString);
          vtPointer       : (VPointer: JLObject);
          vtPChar         : (VPChar: JLObject);
          vtObject        : (VObject: TObject);
          vtClass         : (VClass: TClass);
          vtPWideChar     : (VPWideChar: JLObject);
          vtAnsiString    : (VAnsiString: AnsiStringClass);
          vtCurrency      : (VCurrency: Currency);
   //       vtVariant       : (VVariant: PVariant);
          vtInterface     : (VInterface: JLObject);
          vtWideString    : (VWideString: JLString);
          vtInt64         : (VInt64: Int64);
          vtUnicodeString : (VUnicodeString: JLString);
          vtQWord         : (VQWord: QWord);
      end;
{$endif}

