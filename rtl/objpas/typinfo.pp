{
    $Id$
    This file is part of the Free Pascal run time library.

    Copyright (c) 1998 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This unit provides the same functionality as the TypInfo Unit }
{ of Delphi                                                     }

unit typinfo;

  interface

    uses
       sysutils;

    type
      // FPC uses a Byte because a enumeration type takes always
      // 4 bytes which is too much 
      TTypeKind = Byte;
      TOrdType = Byte;
      TMethodKind = Byte;
      TFloatType = Byte;
      TParamFlags;

      TTypeKinds = set of TTypeKind;

      TTypeInfo = record
         Kind : TTypeKind;
         Name : ShortString;
      end;
      PTypeInfo = ^TTypeInfo;
      PPTypeInfo = ^PTypeInfo;

      PTypeData = ^TTypeData;
      TTypeData = packed record
         case TTypeKind of
            tkUnKnown,tkLString,tkWString,tkAString,tkVariant:
              ();
            tkInteger,tkChar,tkEnumeration,tkWChar:
              ( //!!!!! To be added );
            tkFloat:
              (FloatType : TFloatType);
            tkSString:
              (MaxLength : Byte);
            tkClass:
              (ClassType : TClass);
              (ParentInfo : PPTypeInfo);
              (PropCount : SmallInt);
              (UnitName : ShortString);
      end;

   const
      // if you change one of the following constants,
      // you have also to change the compiler in an appropriate way !
      tkUnknown       = 0;
      tkInteger       = 1;
      tkChar          = 2;
      tkEnumeration   = 3;
      tkFloat         = 4;
      tkSet           = 6;
      tkMethod        = 7;
      tkSString       = 8;
      tkString        = tkSString;
      tkLString       = 9;
      tkAString       = 10;
      tkWString       = 11;
      tkVariant       = 12;
      tkArray         = 13;
      tkRecord        = 14;
      tkInterface     = 15;
      tkClass         = 16;
      tkObject        = 17;
      tkWChar         = 18;

      otSByte         = 0;
      otUByte         = 1;
      otSWord         = 2;
      otUWord         = 3;
      otSLong         = 4;
      otULong         = 5;

      ftSingle        = 0;
      ftDouble        = 1;
      ftExtended      = 2;
      ftComp          = 3;
      ftCurr          = 4;
      ftFixed16       = 5;
      ftFixed32       = 6;

    // just skips the id and the name
    function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;

  implementation

    function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;

      begin
         GetTypeData:=PTypeData(TypeInfo)+2+PByte(TypeInfo+1)^;
      end;

end.

{
  $Log$
  Revision 1.1  1998-08-25 22:30:00  florian
    + initial revision:
       o constants
       o basic type data record

}
