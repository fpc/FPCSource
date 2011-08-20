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

  TJClass = class of jlobject;


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
       VType: sizeint;
       Value: JLObject;
       procedure init(l: longint);
       procedure init(b: boolean);
       procedure init(c: ansichar);
       procedure init(w: widechar);
       procedure init(d: extended);
       procedure init(const s: shortstring);
       // pointer = object -> use constref to get different signature
       procedure init(constref p: pointer);
       procedure init(p: pchar);
       procedure init(p: JLObject);
       procedure init(c: TJClass);
       procedure init(p: pwidechar);
       procedure init(const a: ansistring);
       // currency = int64 -> use constref to get different signature
       procedure init(constref c: currency);
       // procedure init(const v: variant);
       // interface = object
       procedure init(const w: widestring);
       procedure init(i: int64);
       // unicodestring = widestring

       // qword = int64 -> extra parameter to solve signature problem
       procedure init(q: qword; unsigned: boolean = true);

       function VInteger: longint;
       function VBoolean: boolean;
       function VChar: ansichar;
       function VWideChar: widechar;
       function VExtended: PExtended;
       function VDouble: double;
       function VString: PShortString;
       function VPointer: pointer;
       function VPChar: PChar;
       function VObject: JLObject;
       function VClass: TJClass;
       function VPWideChar: PWideChar;
       function VAnsiString: Pointer;
       function VCurrency: PCurrency;
       // function VVariant: PVariant;
       function VInterface: JLObject;
       function VWideString: Pointer;
       function VInt64: PInt64;
       function VUnicodeString: Pointer;
       function VQWord: PQWord;
      end;

