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

    uses objpas;
{
       sysutils;
}
// temporary types:

    type
       ShortString=String;
       PByte      =^Byte;
       PBoolean   =^Boolean;

{$MINENUMSIZE 1   this saves a lot of memory }
       // if you change one of the following enumeration types
       // you have also to change the compiler in an appropriate way !
       TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,
                   tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool);

       TTOrdType = (otSByte,otUByte,otSWord,otUWord,otSLong,otULong);

       TFloatType = (ftSingle,ftDouble,ftExtended,ftComp,ftCurr,
                     ftFixed16,ftFixed32);
       TMethodKind = (mkProcedure,mkFunction,mkSafeProcedure,mkSafeFunction);
       TParamFlags = set of (pfVar,pfConst,pfArray,pfAddress,pfReference,pfOut);
       TIntfFlags = set of (ifHasGuid,ifDispInterface,ifDispatch);

{$MINENUMSIZE DEFAULT}

   const
      ptField = 0;
      ptStatic = 1;
      ptVirtual = 2;
      ptConst = 3;

      tkString        = tkSString;

   type
      TTypeKinds = set of TTypeKind;

      TTypeInfo = record
         Kind : TTypeKind;
         Name : ShortString;
         // here the type data follows as TTypeData record
      end;

      PTypeInfo = ^TTypeInfo;
      PPTypeInfo = ^PTypeInfo;

      PTypeData = ^TTypeData;
      TTypeData = packed record
         case TTypeKind of
            tkUnKnown,tkLString,tkWString,tkAString,tkVariant:
              ();
            tkInteger,tkChar,tkEnumeration,tkWChar:
              (OrdType : TTOrdType;
               case TTypeKind of
                  tkInteger,tkChar,tkEnumeration,tkBool,tkWChar : (
                    MinValue,MaxValue : Longint;
                    case TTypeKind of
                      tkEnumeration:
                        (
                        BaseType : PTypeInfo;
                        NameList : ShortString)
                    );
                  tkSet:
                    (CompType : PTypeInfo)
              );
            tkFloat:
              (FloatType : TFloatType);
            tkSString:
              (MaxLength : Byte);
            tkClass:
              (ClassType : TClass;
               ParentInfo : PTypeInfo;
               PropCount : SmallInt;
               UnitName : ShortString
               // here the properties follow as array of TPropInfo
              );
            tkMethod:
              ({!!!!!!!}
              );
            tkInterface:
              ({!!!!!!!}
              );
      end;

      // unsed, just for completeness
      TPropData = packed record
        PropCount : Word;
        PropList : record end;
      end;

      PPropInfo = ^TPropInfo;
      TPropInfo = packed record
        PropType : PTypeInfo;
        GetProc : Pointer;
        SetProc : Pointer;
        StoredProc : Pointer;
        Index : Integer;
        Default : Longint;
        NameIndex : SmallInt;

        // contains the type of the Get/Set/Storedproc, see also ptxxx
        // bit 0..1 GetProc
        //     2..3 SetProc
        //     4..5 StoredProc
        PropProcs : Byte;

        Name : ShortString;
      end;

      TProcInfoProc = procedure(PropInfo : PPropInfo) of object;

      PPropList = ^TPropList;
      TPropList = array[0..65535] of PPropInfo;

   const
      tkAny = [Low(TTypeKind)..High(TTypeKind)];
      tkMethods = [tkMethod];
      tkProperties = tkAny-tkMethods-[tkUnknown];

    { general property handling }
    // just skips the id and the name
    function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;

    // searches in the property PropName
    function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;
    procedure GetPropInfos(TypeInfo : PTypeInfo;PropList : PPropList);
    function GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds;
      PropList : PPropList) : Integer;

    // returns true, if PropInfo is a stored property
    function IsStoredProp(Instance : TObject;PropInfo : PPropInfo) : Boolean;

    { subroutines to read/write properties }
    function GetOrdProp(Instance : TObject;PropInfo : PPropInfo) : Longint;
    procedure SetOrdProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Longint);

    function GetStrProp(Instance : TObject;PropInfo : PPropInfo) : string;
    procedure SetStrProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : string);

    function GetFloatProp(Instance : TObject;PropInfo : PPropInfo) : Extended;
    procedure SetFloatProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Extended);

    function GetVariantProp(Instance : TObject;PropInfo : PPropInfo): Variant;
    procedure SetVariantProp(Instance : TObject;PropInfo : PPropInfo;
      const Value: Variant);

    function GetMethodProp(Instance : TObject;PropInfo : PPropInfo) : TMethod;
    procedure SetMethodProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : TMethod);

    { misc. stuff }
    function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;
    function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;

  implementation

{$ASMMODE INTEL}

    function CallMethod_Integer(s : Pointer;Address : Pointer) : Integer;assembler;

      asm
         mov ESI,s
         mov EDI,Address
         call [EDI]
         // now the result should be in EAX, untested yet (FK)
      end;

    function CallMethod_Boolean(s : Pointer;Address : Pointer) : Boolean;assembler;

      asm
         mov ESI,s
         mov EDI,Address
         call [EDI]
         // now the result should be in EAX, untested yet (FK)
      end;

    function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;

      begin
         GetTypeData:=PTypeData(TypeInfo)+2+PByte(TypeInfo+1)^;
      end;

    function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;

      var
         hp : PTypeData;
         i : longint;

      begin
         Result:=Nil;
         while Assigned(TypeInfo) do
           begin
              // skip the name
              hp:=GetTypeData(Typeinfo);

              // the class info rtti the property rtti follows
              // immediatly
              Result:=PPropInfo(@hp^.UnitName)+byte(hp^.UnitName[0])+1;
              for i:=1 to hp^.PropCount do
                begin
                   // found a property of that name ?
                   if Result^.Name=PropName then
                     exit;

                   // skip to next property
                   Result:=PPropInfo(@Result^.Name)+byte(Result^.Name[0])+1;
                end;
              // parent class
              Typeinfo:=hp^.ParentInfo;
           end;
      end;

    function IsStoredProp(Instance : TObject;PropInfo : PPropInfo) : Boolean;

      begin
         case (PropInfo^.PropProcs shr 4) and 3 of
            0:
              IsStoredProp:=PBoolean(Pointer(Instance)+Longint(PropInfo^.StoredProc))^;
            1:
              IsStoredProp:=CallMethod(Instance,PropInfo^.StoredProc);
            2:
              IsStoredProp:=CallMethod(Instance,(PPointer(Instance.ClassType)+Longint(PropInfo^.StoredProc)^);
            3:
              IsStoredProp:=LongBool(PropInfo^.StoredProc);
         end;
      end;

    procedure GetPropInfos(TypeInfo : PTypeInfo;PropList : PPropList);

      begin
         {!!!!!!!!!!!}
      end;

    function GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds;
      PropList : PPropList) : Integer;

      begin
         {!!!!!!!!!!!}
      end;

    function GetOrdProp(Instance : TObject;PropInfo : PPropInfo) : Longint;

      var
         value : longint;

      begin
         case (PropInfo^.PropProcs) and 3 of
            0:
              Value:=PLongint(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
            1:
              Value:=CallMethod(Instance,PropInfo^.GetProc);
            2:
              Value:=CallMethod(Instance,(PPointer(Instance.ClassType)+Longint(PropInfo^.GetProc)^);
         end;
         { cut off unnecessary stuff }
         case GetTypeData(PropInfo^.PropType)^.OrdType of
            otSWord,otUWord:
              Value:=Value and $ffff;
            otSByte,otUByte:
              Value:=Value and $ff;
         end;
         GetOrdProp:=Value;
      end;

    procedure SetOrdProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Longint);

      begin
         {!!!!!!!!!!!}
      end;

    function GetStrProp(Instance : TObject;PropInfo : PPropInfo) : string;

      begin
         {!!!!!!!!!!!}
      end;

    procedure SetStrProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : string);

      begin
         {!!!!!!!!!!!}
      end;

    function GetFloatProp(Instance : TObject;PropInfo : PPropInfo) : Extended;

      begin
         {!!!!!!!!!!!}
      end;

    procedure SetFloatProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Extended);

      begin
         {!!!!!!!!!!!}
      end;

    function GetVariantProp(Instance : TObject;PropInfo : PPropInfo): Variant;

      begin
         {!!!!!!!!!!!}
      end;

    procedure SetVariantProp(Instance : TObject;PropInfo : PPropInfo;
      const Value: Variant);

      begin
         {!!!!!!!!!!!}
      end;

    function GetMethodProp(Instance : TObject;PropInfo : PPropInfo) : TMethod;

      begin
         {!!!!!!!!!!!}
      end;

    procedure SetMethodProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : TMethod);

      begin
         {!!!!!!!!!!!}
      end;

    function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;

      begin
         {!!!!!!!!!!!}
      end;

    function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;

      begin
         {!!!!!!!!!!!}
      end;

end.

{
  $Log$
  Revision 1.9  1998-09-19 15:25:45  florian
    * procedure GetOrdProp added

  Revision 1.8  1998/09/19 08:33:53  florian
    + some procedures added

  Revision 1.7  1998/09/08 09:52:31  florian
    * small problems fixed

  Revision 1.6  1998/09/08 00:08:36  michael
  Made it compilable

  Revision 1.5  1998/09/07 23:11:43  florian
    + more fields to TTypeInfo added

  Revision 1.4  1998/09/07 19:34:47  florian
    * constant value is now supported as stored condition

  Revision 1.3  1998/09/07 08:32:59  florian
    + procedure IsStoredProc added

  Revision 1.2  1998/09/06 21:27:05  florian
    + some methods and declarations added

  Revision 1.1  1998/08/25 22:30:00  florian
    + initial revision:
       o constants
       o basic type data record
}
