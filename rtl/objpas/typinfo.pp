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
{$MINENUMSIZE 1   this saves a lot of memory }
       // if you change one of the following enumeration types
       // you have also to change the compiler in an appropriate way !
       TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,
                   tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar);

       TTOrdType = (otSByte,otUByte,otSWord,otUWord,otSLong,otULong);

       TFloatType = (ftSingle,ftDouble,ftExtended,ftComp,ftCurr,
                     ftFixed16,ftFixed32);
{$MINENUMSIZE DEFAULT}

   const
      ptField = 0;
      ptStatic = 1;
      ptVirtual = 2;

   const
      tkString        = tkSString;

   type
      TMethodKind = Byte;

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
              (ClassType : TClass;
               ParentInfo : PTypeInfo;
               PropCount : SmallInt;
               UnitName : ShortString;
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
        ProcProcs : Byte;

        Name : ShortString;
      end;

      TProcInfoProc = procedure(PropInfo : PPropInfo) of object;

      PPropList = ^TPropList;
      TPropList = array[0..65535] of PPropInfo;

   const
      tkAny = [Low(TTypeKind)..High(TTypeKind)];
      tkMethods = [tkMethod];
      tkProperties = tkAny-tkMethods-[tkUnknown];

    // just skips the id and the name
    function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;

    // searches in the property PropName
    function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;

    // returns true, if PropInfo is a stored property
    function IsStoredProp(Instance: TObject; PropInfo: PPropInfo): Boolean;
    {
    procedure GetPropInfos(TypeInfo : PTypeInfo;PropList : PPropList);
    function GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds;
      PropList : PPropList) : Integer;
    }
  implementation

    function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;

      begin
         GetTypeData:=PTypeData(TypeInfo)+2+PByte(TypeInfo+1)^;
      end;

    function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;

      var
         hp : PTypeData;

      begin
         Result:=Nil;
         while Assigned(hp) do
           begin
              // skip the name
              hp:=GetTypeData;

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
              hp:=hp^.ParentInfo;
           end;
      end;

    function IsStoredProp(Instance: TObject; PropInfo: PPropInfo): Boolean;

      type
         tbfunction = function : boolean of object;

      var
         caller : packed record
            Instance : Pointer;
            Address : Pointer;
         end;

      begin
         caller.Instance:=Instance;
         case (PropInfo^.PropProcs shr 4) and 3 of
            0:
              IsStoredProp:=
                PBoolean(Pointer(Instance)+Longint(PropInfo^.StoredProc))^;
            1:
              begin
                 caller.Address:=PropInfo^.StoredProc;
                 IsStoredProc:=tbfunction(caller);
              end;
            2:
              begin
                 caller.Address:=PPointer(PPointer(Instance.ClassType)+Longint(PropInfo^.StoredProc))^;
                 IsStoredProc:=tbfunction(caller);
              end;
         end;
      end;

end.

{
  $Log$
  Revision 1.3  1998-09-07 08:32:59  florian
    + procedure IsStoredProc added

  Revision 1.2  1998/09/06 21:27:05  florian
    + some methods and declarations added

  Revision 1.1  1998/08/25 22:30:00  florian
    + initial revision:
       o constants
       o basic type data record
}
