{
    $Id$
    This file is part of the Free Pascal run time library.

    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This unit provides the same Functionality as the TypInfo Unit }
{ of Delphi                                                     }

unit typinfo;

  interface

{$MODE objfpc}

  uses SysUtils;


// temporary types:

    type
{$ifndef HASVARIANT}
       Variant      = Pointer;
{$endif}

{$MINENUMSIZE 1   this saves a lot of memory }
       // if you change one of the following enumeration types
       // you have also to change the compiler in an appropriate way !
       TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,
                   tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord,
                   tkDynArray,tkInterfaceRaw);

       TTOrdType  = (otSByte,otUByte,otSWord,otUWord,otSLong,otULong);

       TFloatType = (ftSingle,ftDouble,ftExtended,ftComp,ftCurr);
       TMethodKind = (mkProcedure,mkFunction,mkConstructor,mkDestructor,
                      mkClassProcedure, mkClassFunction);
       TParamFlags    = set of (pfVar,pfConst,pfArray,pfAddress,pfReference,pfOut);
       TIntfFlag      = (ifHasGuid,ifDispInterface,ifDispatch);
       TIntfFlags     = set of TIntfFlag;
       TIntfFlagsBase = set of TIntfFlag;

{$MINENUMSIZE DEFAULT}

   const
      ptField = 0;
      ptStatic = 1;
      ptVirtual = 2;
      ptConst = 3;

      tkString        = tkSString;

   type
      TTypeKinds = set of TTypeKind;

{$PACKRECORDS 1}
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
              (MethodKind : TMethodKind;
               ParamCount : Byte;
               ParamList : array[0..1023] of Char
             {in reality ParamList is a array[1..ParamCount] of:
                  record
                    Flags : TParamFlags;
                    ParamName : ShortString;
                    TypeName : ShortString;
                  end;
              followed by
                  ResultType : ShortString}
              );
            tkInt64:
              (MinInt64Value, MaxInt64Value: Int64);
            tkQWord:
              (MinQWordValue, MaxQWordValue: QWord);
            tkInterface,
            tkInterfaceRaw:
              (
               IntfParent: PPTypeInfo;
               IID: PGUID;
               IIDStr: ShortString;
               IntfUnit: ShortString;
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
        //     6 : true, constant index property
        PropProcs : Byte;

        Name : ShortString;
      end;

      TProcInfoProc = Procedure(PropInfo : PPropInfo) of object;

      PPropList = ^TPropList;
      TPropList = array[0..65535] of PPropInfo;

   const
      tkAny = [Low(TTypeKind)..High(TTypeKind)];
      tkMethods = [tkMethod];
      tkProperties = tkAny-tkMethods-[tkUnknown];

// general property handling
Function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;

Function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;
Function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string; AKinds : TTypeKinds) : PPropInfo;
Function GetPropInfo(Instance: TObject; const PropName: string; AKinds: TTypeKinds) : PPropInfo;
Function GetPropInfo(Instance: TObject; const PropName: string): PPropInfo;
Function GetPropInfo(AClass: TClass; const PropName: string; AKinds: TTypeKinds) : PPropInfo;
Function GetPropInfo(AClass: TClass; const PropName: string): PPropInfo;
Function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo;
Function FindPropInfo(AClass:TClass;const PropName: string): PPropInfo;
Procedure GetPropInfos(TypeInfo : PTypeInfo;PropList : PPropList);
Function  GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds; PropList : PPropList) : Integer;

// Property information routines.
Function IsStoredProp(Instance: TObject;PropInfo : PPropInfo) : Boolean;
Function IsStoredProp(Instance: TObject; const PropName: string): Boolean;
Function IsPublishedProp(Instance: TObject; const PropName: string): Boolean;
Function IsPublishedProp(AClass: TClass; const PropName: string): Boolean;
Function PropType(Instance: TObject; const PropName: string): TTypeKind;
Function PropType(AClass: TClass; const PropName: string): TTypeKind;
Function PropIsType(Instance: TObject; const PropName: string; TypeKind: TTypeKind): Boolean;
Function PropIsType(AClass: TClass; const PropName: string; TypeKind: TTypeKind): Boolean;

// subroutines to read/write properties
Function  GetOrdProp(Instance: TObject; PropInfo : PPropInfo) : Longint;
Function  GetOrdProp(Instance: TObject; const PropName: string): Longint;
Procedure SetOrdProp(Instance: TObject; PropInfo : PPropInfo;  Value : Longint);
Procedure SetOrdProp(Instance: TObject; const PropName: string; Value: Longint);

Function  GetEnumProp(Instance: TObject; const PropName: string): string;
Function  GetEnumProp(Instance: TObject; const PropInfo: PPropInfo): string;
Procedure SetEnumProp(Instance: TObject; const PropName: string;const Value: string);
Procedure SetEnumProp(Instance: TObject; const PropInfo: PPropInfo;const Value: string);

Function  GetSetProp(Instance: TObject; const PropName: string): string;
Function  GetSetProp(Instance: TObject; const PropName: string; Brackets: Boolean): string;
Function  GetSetProp(Instance: TObject; const PropInfo: PPropInfo; Brackets: Boolean): string;
Procedure SetSetProp(Instance: TObject; const PropName: string; const Value: string);
Procedure SetSetProp(Instance: TObject; const PropInfo: PPropInfo; const Value: string);

Function  GetStrProp(Instance: TObject; PropInfo : PPropInfo) : Ansistring;
Function  GetStrProp(Instance: TObject; const PropName: string): string;
Procedure SetStrProp(Instance: TObject; const PropName: string; const Value: AnsiString);
Procedure SetStrProp(Instance: TObject; PropInfo : PPropInfo;  const Value : Ansistring);

Function  GetFloatProp(Instance: TObject; PropInfo : PPropInfo) : Extended;
Function  GetFloatProp(Instance: TObject; const PropName: string): Extended;
Procedure SetFloatProp(Instance: TObject; const PropName: string; Value: Extended);
Procedure SetFloatProp(Instance: TObject; PropInfo : PPropInfo;  Value : Extended);

Function  GetVariantProp(Instance: TObject; PropInfo : PPropInfo): Variant;
Function  GetVariantProp(Instance: TObject; const PropName: string): Variant;
Procedure SetVariantProp(Instance: TObject; const PropName: string; const Value: Variant);
Procedure SetVariantProp(Instance: TObject; PropInfo : PPropInfo; const Value: Variant);

Function  GetObjectProp(Instance: TObject; const PropName: string): TObject;
Function  GetObjectProp(Instance: TObject; const PropName: string; MinClass: TClass): TObject;
Function  GetObjectProp(Instance: TObject; PropInfo: PPropInfo; MinClass: TClass): TObject;
Procedure SetObjectProp(Instance: TObject; const PropName: string; Value: TObject);
Procedure SetObjectProp(Instance: TObject; PropInfo: PPropInfo; Value: TObject);

Function  GetObjectPropClass(Instance: TObject; const PropName: string): TClass;

Function  GetMethodProp(Instance: TObject; PropInfo: PPropInfo) : TMethod;
Function  GetMethodProp(Instance: TObject; const PropName: string): TMethod;
Procedure SetMethodProp(Instance: TObject; PropInfo: PPropInfo;  const Value : TMethod);
Procedure SetMethodProp(Instance: TObject; const PropName: string; const Value: TMethod);

Function  GetInt64Prop(Instance: TObject; PropInfo: PPropInfo): Int64;
Function  GetInt64Prop(Instance: TObject; const PropName: string): Int64;
Procedure SetInt64Prop(Instance: TObject; PropInfo: PPropInfo;  const Value: Int64);
Procedure SetInt64Prop(Instance: TObject; const PropName: string;  const Value: Int64);

Function GetPropValue(Instance: TObject; const PropName: string): Variant;
Function GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;
Procedure SetPropValue(Instance: TObject; const PropName: string; const Value: Variant);

// Auxiliary routines, which may be useful
Function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;
Function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;
function SetToString(PropInfo: PPropInfo; Value: Integer; Brackets: Boolean) : String;
function SetToString(PropInfo: PPropInfo; Value: Integer) : String;
function StringToSet(PropInfo: PPropInfo; const Value: string): Integer;

const
    BooleanIdents: array[Boolean] of String = ('False', 'True');
    DotSep: String = '.';

Type
  EPropertyError = Class(Exception);

Implementation

ResourceString
  SErrPropertyNotFound = 'Unknown property: "%s"';
  SErrUnknownEnumValue = 'Unknown enumeration value: "%s"';

type
  PMethod = ^TMethod;

{ ---------------------------------------------------------------------
  Auxiliary methods
  ---------------------------------------------------------------------}

Function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;

  Var PS : PShortString;
      PT : PTypeData;

begin
 PT:=GetTypeData(TypeInfo);
 // ^.BaseType);
 //      If PT^.MinValue<0 then Value:=Ord(Value<>0); {map to 0/1}
 PS:=@PT^.NameList;
 While Value>0 Do
  begin
    PS:=PShortString(pointer(PS)+PByte(PS)^+1);
    Dec(Value);
  end;
 Result:=PS^;
end;


Function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;

  Var PS : PShortString;
      PT : PTypeData;
      Count : longint;

begin
  If Length(Name)=0 then
    exit(-1);
  PT:=GetTypeData(TypeInfo);
  Count:=0;
  Result:=-1;
  PS:=@PT^.NameList;
  While (Result=-1) and (PByte(PS)^<>0) do
    begin
      If CompareText(PS^, Name) = 0 then
        Result:=Count;
      PS:=PShortString(pointer(PS)+PByte(PS)^+1);
      Inc(Count);
    end;
end;


Function SetToString(PropInfo: PPropInfo; Value: Integer; Brackets: Boolean) : String;

Var
  I : Integer;
  PTI : PTypeInfo;

begin
  PTI:=GetTypeData(PropInfo^.PropType)^.CompType;
  Result:='';
  For I:=0 to SizeOf(Integer)*8-1 do
    begin
      if ((Value and 1)<>0) then
        begin
          If Result='' then
            Result:=GetEnumName(PTI,i)
          else
            Result:=Result+','+GetEnumName(PTI,I);
        end;
      Value:=Value shr 1;
    end;
  if Brackets then
    Result:='['+Result+']';
end;


Function SetToString(PropInfo: PPropInfo; Value: Integer) : String;

begin
  Result:=SetToString(PropInfo,Value,False);
end;


Const
  SetDelim = ['[',']',',',' '];

Function GetNextElement(Var S : String) : String;
Var
  J : Integer;
begin
  J:=1;
  Result:='';
  If Length(S)>0 then
    begin
      While (J<=Length(S)) and Not (S[j] in SetDelim) do
        Inc(j);
      Result:=Copy(S,1,j-1);
      Delete(S,1,j);
    end;
end;


Function StringToSet(PropInfo: PPropInfo; const Value: string): Integer;
Var
  S,T : String;
  I : Integer;
  PTI : PTypeInfo;

begin
  Result:=0;
  PTI:=GetTypeData(PropInfo^.PropType)^.Comptype;
  S:=Value;
  I:=1;
  If Length(S)>0 then
    begin
      While (I<=Length(S)) and (S[i] in SetDelim) do
        Inc(I);
      Delete(S,1,i-1);
    end;
  While (S<>'') do
    begin
      T:=GetNextElement(S);
      if T<>'' then
        begin
          I:=GetEnumValue(PTI,T);
          if (I<0) then
            raise EPropertyError.CreateFmt(SErrUnknownEnumValue, [T]);
          Result:=Result or (1 shl i);
        end;
    end;
end;


Function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;
begin
  GetTypeData:=PTypeData(pointer(TypeInfo)+2+PByte(pointer(TypeInfo)+1)^);
end;


{ ---------------------------------------------------------------------
  Basic Type information functions.
  ---------------------------------------------------------------------}

Function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;
var
  hp : PTypeData;
  i : longint;
  p : string;
  pd : ^TPropData;
begin
  P:=UpCase(PropName);
  while Assigned(TypeInfo) do
    begin
      // skip the name
      hp:=GetTypeData(Typeinfo);
      // the class info rtti the property rtti follows immediatly
      pd:=pointer(pointer(@hp^.UnitName)+Length(hp^.UnitName)+1);
      Result:=@pd^.PropList;
      for i:=1 to pd^.PropCount do
        begin
          // found a property of that name ?
          if Upcase(Result^.Name)=P then
            exit;
          // skip to next property
          Result:=PPropInfo(pointer(@Result^.Name)+byte(Result^.Name[0])+1);
        end;
      // parent class
      Typeinfo:=hp^.ParentInfo;
    end;
  Result:=Nil;
end;


Function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string; Akinds : TTypeKinds) : PPropInfo;
begin
  Result:=GetPropInfo(TypeInfo,PropName);
  If (Akinds<>[]) then
    If (Result<>Nil) then
      If Not (Result^.PropType^.Kind in AKinds) then
        Result:=Nil;
end;


Function GetPropInfo(AClass: TClass; const PropName: string; AKinds: TTypeKinds) : PPropInfo;
begin
  Result:=GetPropInfo(PTypeInfo(AClass.ClassInfo),PropName,AKinds);
end;


Function GetPropInfo(Instance: TObject; const PropName: string; AKinds: TTypeKinds) : PPropInfo;
begin
  Result:=GetPropInfo(Instance.ClassType,PropName,AKinds);
end;


Function GetPropInfo(Instance: TObject; const PropName: string): PPropInfo;
begin
  Result:=GetPropInfo(Instance,PropName,[]);
end;


Function GetPropInfo(AClass: TClass; const PropName: string): PPropInfo;
begin
  Result:=GetPropInfo(AClass,PropName,[]);
end;


Function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo;
begin
  result:=GetPropInfo(Instance, PropName);
  if Result=nil then
    Raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;


Function FindPropInfo(AClass:TClass;const PropName: string): PPropInfo;
begin
  result:=GetPropInfo(AClass,PropName);
  if result=nil then
    Raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;


Function IsStoredProp(Instance : TObject;PropInfo : PPropInfo) : Boolean;
type
  TBooleanFunc=function:boolean of object;
var
  AMethod : TMethod;
begin
  case (PropInfo^.PropProcs shr 4) and 3 of
    ptfield:
      Result:=PBoolean(Pointer(Instance)+Longint(PropInfo^.StoredProc))^;
    ptconst:
      Result:=LongBool(PropInfo^.StoredProc);
    ptstatic,
    ptvirtual:
      begin
        if (PropInfo^.PropProcs shr 4) and 3=ptstatic then
          AMethod.Code:=PropInfo^.StoredProc
        else
          AMethod.Code:=ppointer(Pointer(Instance.ClassType)+Longint(PropInfo^.StoredProc))^;
        AMethod.Data:=Instance;
        Result:=TBooleanFunc(AMethod)();
      end;
  end;
end;


Procedure GetPropInfos(TypeInfo : PTypeInfo;PropList : PPropList);
{
        Store Pointers to property information in the list pointed
        to by proplist. PRopList must contain enough space to hold ALL
        properties.
}
Var
  TD : PTypeData;
  TP : PPropInfo;
  Count : Longint;
begin
  TD:=GetTypeData(TypeInfo);
  // Get this objects TOTAL published properties count
  TP:=(@TD^.UnitName+Length(TD^.UnitName)+1);
  Count:=PWord(TP)^;
  // Now point TP to first propinfo record.
  Inc(Longint(TP),SizeOF(Word));
  While Count>0 do
    begin
      PropList^[0]:=TP;
      Inc(Longint(PropList),SizeOf(Pointer));
      // Point to TP next propinfo record.
      // Located at Name[Length(Name)+1] !
      TP:=PPropInfo(pointer(@TP^.Name)+PByte(@TP^.Name)^+1);
      Dec(Count);
    end;
  // recursive call for parent info.
  If TD^.Parentinfo<>Nil then
    GetPropInfos (TD^.ParentInfo,PropList);
end;


Procedure InsertProp (PL : PProplist;PI : PPropInfo; Count : longint);
Var
  I : Longint;
begin
  I:=0;
  While (I<Count) and (PI^.Name>PL^[I]^.Name) do
    Inc(I);
  If I<Count then
    Move(PL^[I], PL^[I+1], (Count - I) * SizeOf(Pointer));
  PL^[I]:=PI;
end;


Function GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds; PropList : PPropList) : Integer;
{
  Store Pointers to property information OF A CERTAIN KIND in the list pointed
  to by proplist. PRopList must contain enough space to hold ALL
  properties.
}
Var
  TempList : PPropList;
  PropInfo : PPropinfo;
  I,Count : longint;
begin
  Result:=0;
  Count:=GetTypeData(TypeInfo)^.Propcount;
  If Count>0 then
    begin
      GetMem(TempList,Count*SizeOf(Pointer));
      Try
        GetPropInfos(TypeInfo,TempList);
        For I:=0 to Count-1 do
          begin
            PropInfo:=TempList^[i];
            If PropInfo^.PropType^.Kind in TypeKinds then
              begin
                InsertProp(PropList,PropInfo,Result);
                Inc(Result);
              end;
          end;
      finally
        FreeMem(TempList,Count*SizeOf(Pointer));
      end;
    end;
end;


{ ---------------------------------------------------------------------
  Property access functions
  ---------------------------------------------------------------------}

{ ---------------------------------------------------------------------
  Ordinal properties
  ---------------------------------------------------------------------}

Function GetOrdProp(Instance : TObject;PropInfo : PPropInfo) : Longint;
type
  TGetIntegerProcIndex=function(index:longint):longint of object;
  TGetIntegerProc=function:longint of object;
var
  TypeInfo: PTypeInfo;
  AMethod : TMethod;
begin
  Result:=0;
  case (PropInfo^.PropProcs) and 3 of
    ptfield:
      Result:=PLongint(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
    ptstatic,
    ptvirtual :
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          Result:=TGetIntegerProcIndex(AMethod)(PropInfo^.Index)
        else
          Result:=TGetIntegerProc(AMethod)();
      end;
  end;
  { cut off unnecessary stuff }
  TypeInfo := PropInfo^.PropType;
  case TypeInfo^.Kind of
    tkChar, tkBool:
      Result:=Result and $ff;
    tkWChar:
      Result:=Result and $ffff;
    tkEnumeration,
    tkInteger:
      case GetTypeData(TypeInfo)^.OrdType of
        otSWord,otUWord:
          Result:=Result and $ffff;
        otSByte,otUByte:
          Result:=Result and $ff;
      end;
  end;
end;


Procedure SetOrdProp(Instance : TObject;PropInfo : PPropInfo;Value : Longint);
type
  TSetIntegerProcIndex=procedure(index,i:longint) of object;
  TSetIntegerProc=procedure(i:longint) of object;
var
  DataSize: Integer;
  AMethod : TMethod;
begin
  DataSize := 4;
  if PropInfo^.PropType^.Kind <> tkClass then
    begin
      { cut off unnecessary stuff }
      case GetTypeData(PropInfo^.PropType)^.OrdType of
        otSWord,otUWord:
          begin
            Value:=Value and $ffff;
            DataSize := 2;
          end;
        otSByte,otUByte:
          begin
            Value:=Value and $ff;
            DataSize := 1;
          end;
       end;
    end;
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptfield:
      case DataSize of
        1: PByte(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Byte(Value);
        2: PWord(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Word(Value);
        4: PLongint(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
      end;
    ptstatic,
    ptvirtual :
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          TSetIntegerProcIndex(AMethod)(PropInfo^.Index,Value)
        else
          TSetIntegerProc(AMethod)(Value);
      end;
  end;
end;


Function GetOrdProp(Instance: TObject; const PropName: string): Longint;
begin
  Result:=GetOrdProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetOrdProp(Instance: TObject; const PropName: string;  Value: Longint);
begin
  SetOrdProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Function GetEnumProp(Instance: TObject; Const PropInfo: PPropInfo): string;
begin
  Result:=GetEnumName(PropInfo^.PropType, GetOrdProp(Instance, PropInfo));
end;


Function GetEnumProp(Instance: TObject; const PropName: string): string;
begin
  Result:=GetEnumProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetEnumProp(Instance: TObject; const PropName: string;  const Value: string);
begin
  SetEnumProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Procedure SetEnumProp(Instance: TObject; Const PropInfo : PPropInfo; const Value: string);
Var
  PV : Longint;
begin
  If PropInfo<>Nil then
    begin
      PV:=GetEnumValue(PropInfo^.PropType, Value);
      if (PV<0) then
        raise EPropertyError.CreateFmt(SErrUnknownEnumValue, [Value]);
      SetOrdProp(Instance, PropInfo,PV);
    end;
end;


{ ---------------------------------------------------------------------
  Set properties
  ---------------------------------------------------------------------}

Function GetSetProp(Instance: TObject; const PropName: string): string;
begin
  Result:=GetSetProp(Instance,PropName,False);
end;


Function GetSetProp(Instance: TObject; const PropName: string; Brackets: Boolean): string;
begin
  Result:=GetSetProp(Instance,FindPropInfo(Instance,PropName),Brackets);
end;


Function GetSetProp(Instance: TObject; const PropInfo: PPropInfo; Brackets: Boolean): string;
begin
  Result:=SetToString(PropInfo,GetOrdProp(Instance,PropInfo),Brackets);
end;


Procedure SetSetProp(Instance: TObject; const PropName: string; const Value: string);
begin
  SetSetProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Procedure SetSetProp(Instance: TObject; const PropInfo: PPropInfo; const Value: string);
begin
  SetOrdProp(Instance,PropInfo,StringToSet(PropInfo,Value));
end;

{ ---------------------------------------------------------------------
  Object properties
  ---------------------------------------------------------------------}

Function GetObjectProp(Instance: TObject; const PropName: string): TObject;
begin
  Result:=GetObjectProp(Instance,PropName,Nil);
end;


Function GetObjectProp(Instance: TObject; const PropName: string; MinClass: TClass): TObject;
begin
  Result:=GetObjectProp(Instance,FindPropInfo(Instance,PropName),MinClass);
end;


Function GetObjectProp(Instance: TObject; PropInfo : PPropInfo; MinClass: TClass): TObject;
begin
  Result:=TObject(GetOrdProp(Instance,PropInfo));
  If (MinClass<>Nil) and (Result<>Nil) Then
    If Not Result.InheritsFrom(MinClass) then
      Result:=Nil;
end;


Procedure SetObjectProp(Instance: TObject; const PropName: string;  Value: TObject);
begin
  SetObjectProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Procedure SetObjectProp(Instance: TObject; PropInfo : PPropInfo;  Value: TObject);
begin
  SetOrdProp(Instance,PropInfo,Integer(Value));
end;


Function GetObjectPropClass(Instance: TObject; const PropName: string): TClass;
begin
  Result:=GetTypeData(FindPropInfo(Instance,PropName)^.PropType)^.ClassType;
end;


{ ---------------------------------------------------------------------
  String properties
  ---------------------------------------------------------------------}

Function GetStrProp(Instance: TObject; PropInfo: PPropInfo): AnsiString;
type
  TGetShortStrProcIndex=function(index:longint):ShortString of object;
  TGetShortStrProc=function():ShortString of object;
  TGetAnsiStrProcIndex=function(index:longint):AnsiString of object;
  TGetAnsiStrProc=function():AnsiString of object;
var
  AMethod : TMethod;
begin
  Result:='';
  case Propinfo^.PropType^.Kind of
    tkSString:
      begin
        case (PropInfo^.PropProcs) and 3 of
          ptField:
            Result := PShortString(Pointer(Instance) + LongWord(PropInfo^.GetProc))^;
          ptstatic,
          ptvirtual :
            begin
              if (PropInfo^.PropProcs and 3)=ptStatic then
                AMethod.Code:=PropInfo^.GetProc
              else
                AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                Result:=TGetShortStrProcIndex(AMethod)(PropInfo^.Index)
              else
                Result:=TGetShortStrProc(AMethod)();
            end;
        end;
      end;
    tkAString:
      begin
        case (PropInfo^.PropProcs) and 3 of
          ptField:
            Result := PAnsiString(Pointer(Instance) + LongWord(PropInfo^.GetProc))^;
          ptstatic,
          ptvirtual :
            begin
              if (PropInfo^.PropProcs and 3)=ptStatic then
                AMethod.Code:=PropInfo^.GetProc
              else
                AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                Result:=TGetAnsiStrProcIndex(AMethod)(PropInfo^.Index)
              else
                Result:=TGetAnsiStrProc(AMethod)();
            end;
        end;
      end;
  end;
end;


Procedure SetStrProp(Instance : TObject;PropInfo : PPropInfo; const Value : AnsiString);
type
  TSetShortStrProcIndex=procedure(index:longint;const s:ShortString) of object;
  TSetShortStrProc=procedure(const s:ShortString) of object;
  TSetAnsiStrProcIndex=procedure(index:longint;s:AnsiString) of object;
  TSetAnsiStrProc=procedure(s:AnsiString) of object;
var
  AMethod : TMethod;
begin
  case Propinfo^.PropType^.Kind of
    tkSString:
      begin
        case (PropInfo^.PropProcs shr 2) and 3 of
          ptField:
            PShortString(Pointer(Instance) + LongWord(PropInfo^.SetProc))^:=Value;
          ptstatic,
          ptvirtual :
            begin
              if (PropInfo^.PropProcs and 3)=ptStatic then
                AMethod.Code:=PropInfo^.SetProc
              else
                AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                TSetShortStrProcIndex(AMethod)(PropInfo^.Index,Value)
              else
                TSetShortStrProc(AMethod)(Value);
            end;
        end;
      end;
    tkAString:
      begin
        case (PropInfo^.PropProcs shr 2) and 3 of
          ptField:
            PAnsiString(Pointer(Instance) + LongWord(PropInfo^.SetProc))^:=Value;
          ptstatic,
          ptvirtual :
            begin
              if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
                AMethod.Code:=PropInfo^.SetProc
              else
                AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                TSetAnsiStrProcIndex(AMethod)(PropInfo^.Index,Value)
              else
                TSetAnsiStrProc(AMethod)(Value);
            end;
        end;
      end;
  end;
end;


Function GetStrProp(Instance: TObject; const PropName: string): string;
begin
  Result:=GetStrProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetStrProp(Instance: TObject; const PropName: string; const Value: AnsiString);
begin
  SetStrProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


{ ---------------------------------------------------------------------
  Float properties
  ---------------------------------------------------------------------}

function GetFloatProp(Instance : TObject;PropInfo : PPropInfo) : Extended;
type
  TGetExtendedProc = function:Extended of object;
  TGetExtendedProcIndex = function(Index: integer): Extended of object;
  TGetDoubleProc = function:Double of object;
  TGetDoubleProcIndex = function(Index: integer): Double of object;
  TGetSingleProc = function:Single of object;
  TGetSingleProcIndex = function(Index: integer):Single of object;
var
  AMethod : TMethod;
begin
  Result:=0.0;
  case PropInfo^.PropProcs and 3 of
    ptField:
      Case GetTypeData(PropInfo^.PropType)^.FloatType of
       ftSingle:
         Result:=PSingle(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
       ftDouble:
         Result:=PDouble(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
       ftExtended:
         Result:=PExtended(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
{$ifndef cpum68k}
       ftcomp:
         Result:=PComp(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
{$endif cpum68k}
       end;
    ptStatic,
    ptVirtual:
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        Case GetTypeData(PropInfo^.PropType)^.FloatType of
          ftSingle:
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              Result:=TGetSingleProc(AMethod)()
            else
              Result:=TGetSingleProcIndex(AMethod)(PropInfo^.Index);
          ftDouble:
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              Result:=TGetDoubleProc(AMethod)()
            else
              Result:=TGetDoubleProcIndex(AMethod)(PropInfo^.Index);
          ftExtended:
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              Result:=TGetExtendedProc(AMethod)()
            else
              Result:=TGetExtendedProcIndex(AMethod)(PropInfo^.Index);
        end;
      end;
  end;
end;


Procedure SetFloatProp(Instance : TObject;PropInfo : PPropInfo; Value : Extended);
type
  TSetExtendedProc = procedure(const AValue: Extended) of object;
  TSetExtendedProcIndex = procedure(Index: integer; const AValue: Extended) of object;
  TSetDoubleProc = procedure(const AValue: Double) of object;
  TSetDoubleProcIndex = procedure(Index: integer; const AValue: Double) of object;
  TSetSingleProc = procedure(const AValue: Single) of object;
  TSetSingleProcIndex = procedure(Index: integer; const AValue: Single) of object;
Var
  AMethod : TMethod;
begin
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptfield:
      Case GetTypeData(PropInfo^.PropType)^.FloatType of
        ftSingle:
          PSingle(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
        ftDouble:
          PDouble(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
        ftExtended:
          PExtended(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
       end;
    ptStatic,
    ptVirtual:
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        Case GetTypeData(PropInfo^.PropType)^.FloatType of
          ftSingle:
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              TSetSingleProc(AMethod)(Value)
            else
              TSetSingleProcIndex(AMethod)(PropInfo^.Index,Value);
          ftDouble:
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              TSetDoubleProc(AMethod)(Value)
            else
              TSetDoubleProcIndex(AMethod)(PropInfo^.Index,Value);
          ftExtended:
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              TSetExtendedProc(AMethod)(Value)
            else
              TSetExtendedProcIndex(AMethod)(PropInfo^.Index,Value);
        end;
      end;
  end;
end;


function GetFloatProp(Instance: TObject; const PropName: string): Extended;
begin
  Result:=GetFloatProp(Instance,FindPropInfo(Instance,PropName))
end;


Procedure SetFloatProp(Instance: TObject; const PropName: string;  Value: Extended);
begin
  SetFloatProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


{ ---------------------------------------------------------------------
  Variant properties
  ---------------------------------------------------------------------}

Function GetVariantProp(Instance : TObject;PropInfo : PPropInfo): Variant;
begin
{$warning GetVariantProp not implemented}
{$ifdef HASVARIANT}
  Result:=Null;
{$else}
  Result:=nil;
{$endif}
end;


Procedure SetVariantProp(Instance : TObject;PropInfo : PPropInfo; const Value: Variant);
begin
{$warning SetVariantProp not implemented}
end;


Function GetVariantProp(Instance: TObject; const PropName: string): Variant;
begin
  Result:=GetVariantProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetVariantProp(Instance: TObject; const PropName: string;  const Value: Variant);
begin
  SetVariantprop(instance,FindpropInfo(Instance,PropName),Value);
end;


{ ---------------------------------------------------------------------
  Method properties
  ---------------------------------------------------------------------}


Function GetMethodProp(Instance : TObject;PropInfo : PPropInfo) : TMethod;
type
  TGetMethodProcIndex=function(index:longint):PMethod of object;
  TGetMethodProc=function():PMethod of object;
var
  value: PMethod;
  AMethod : TMethod;
begin
  Value:=nil;
  case (PropInfo^.PropProcs) and 3 of
    ptfield:
      Value:=PMethod(Pointer(Instance)+Longint(PropInfo^.GetProc));
    ptstatic,
    ptvirtual :
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          Value:=TGetMethodProcIndex(AMethod)(PropInfo^.Index)
        else
          Value:=TGetMethodProc(AMethod)();
      end;
  end;
  if Value=nil then
    begin
      Result.Code:=nil;
      Result.Data:=nil;
    end
  else
    Result:=Value^;
end;


Procedure SetMethodProp(Instance : TObject;PropInfo : PPropInfo; const Value : TMethod);
type
  TSetMethodProcIndex=procedure(index:longint;p:PMethod) of object;
  TSetMethodProc=procedure(p:PMethod) of object;
var
  AMethod : TMethod;
begin
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptfield:
      PMethod(Pointer(Instance)+Longint(PropInfo^.SetProc))^ := Value;
    ptstatic,
    ptvirtual :
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          TSetMethodProcIndex(AMethod)(PropInfo^.Index,@Value)
        else
          TSetMethodProc(AMethod)(@Value);
      end;
  end;
end;


Function GetMethodProp(Instance: TObject; const PropName: string): TMethod;
begin
  Result:=GetMethodProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetMethodProp(Instance: TObject; const PropName: string;  const Value: TMethod);
begin
  SetMethodProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


{ ---------------------------------------------------------------------
  Int64 properties
  ---------------------------------------------------------------------}

Function GetInt64Prop(Instance: TObject; PropInfo: PPropInfo): Int64;
type
  TGetInt64ProcIndex=function(index:longint):Int64 of object;
  TGetInt64Proc=function():Int64 of object;
var
  AMethod : TMethod;
begin
  Result:=0;
  case (PropInfo^.PropProcs) and 3 of
    ptfield:
      Result:=PInt64(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
    ptstatic,
    ptvirtual :
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          result:=TGetInt64ProcIndex(AMethod)(PropInfo^.Index)
        else
          result:=TGetInt64Proc(AMethod)();
      end;
  end;
end;


procedure SetInt64Prop(Instance: TObject; PropInfo: PPropInfo; const Value: Int64);
type
  TSetInt64ProcIndex=procedure(index:longint;i:Int64) of object;
  TSetInt64Proc=procedure(i:Int64) of object;
var
  AMethod : TMethod;
begin
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptfield:
      PInt64(Pointer(Instance)+Longint(PropInfo^.SetProc))^ := Value;
    ptstatic,
    ptvirtual :
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          TSetInt64ProcIndex(AMethod)(PropInfo^.Index,Value)
        else
          TSetInt64Proc(AMethod)(Value);
      end;
  end;
end;


Function GetInt64Prop(Instance: TObject; const PropName: string): Int64;
begin
  Result:=GetInt64Prop(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetInt64Prop(Instance: TObject; const PropName: string; const Value: Int64);
begin
  SetInt64Prop(Instance,FindPropInfo(Instance,PropName),Value);
end;


{ ---------------------------------------------------------------------
  All properties through variant.
  ---------------------------------------------------------------------}

Function GetPropValue(Instance: TObject; const PropName: string): Variant;
begin
  Result:=GetPropValue(Instance,PropName,True);
end;


Function GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;
begin
end;


Procedure SetPropValue(Instance: TObject; const PropName: string;  const Value: Variant);
begin
end;


{ ---------------------------------------------------------------------
  Easy access methods that appeared in Delphi 5
  ---------------------------------------------------------------------}

Function IsPublishedProp(Instance: TObject; const PropName: string): Boolean;
begin
  Result:=GetPropInfo(Instance,PropName)<>Nil;
end;

Function IsPublishedProp(AClass: TClass; const PropName: string): Boolean;
begin
  Result:=GetPropInfo(AClass,PropName)<>Nil;
end;

Function PropIsType(Instance: TObject; const PropName: string; TypeKind: TTypeKind): Boolean;
begin
  Result:=FindPropInfo(Instance,PropName)^.PropType^.Kind=TypeKind
end;

Function PropIsType(AClass: TClass; const PropName: string; TypeKind: TTypeKind): Boolean;
begin
  Result:=PropType(AClass,PropName)=TypeKind
end;

Function PropType(Instance: TObject; const PropName: string): TTypeKind;
begin
  Result:=FindPropInfo(Instance,PropName)^.PropType^.Kind;
end;

Function PropType(AClass: TClass; const PropName: string): TTypeKind;
begin
  Result:=FindPropInfo(AClass,PropName)^.PropType^.Kind;
end;

Function IsStoredProp(Instance: TObject; const PropName: string): Boolean;
begin
  Result:=IsStoredProp(instance,FindPropInfo(Instance,PropName));
end;

end.
{
  $Log$
  Revision 1.21  2004-02-20 15:55:26  peter
    * enable variant again

  Revision 1.20  2003/12/24 22:27:13  peter
    * removed assembler
    * cleanup

  Revision 1.19  2003/12/22 11:32:04  marco
   * splitted up tintfflags into several components

  Revision 1.18  2003/10/24 08:37:20  marco
   * Fix from Peter

  Revision 1.17  2003/10/17 20:58:27  olle
    * Changed m68k to cpum68k, i386 to cpui386

  Revision 1.16  2003/04/24 11:46:25  florian
    * fixed wrong newlines

  Revision 1.15  2003/03/29 16:55:56  michael
  + Patch from Mattias Gaertner for single typeinfo

  Revision 1.14  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed

  Revision 1.13  2002/04/04 18:32:59  peter
    * merged getpropinfo fix
}
