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
//{$ifndef HASVARIANT}
       Variant      = Pointer;
//{$endif}

{$MINENUMSIZE 1   this saves a lot of memory }
       // if you change one of the following enumeration types
       // you have also to change the compiler in an appropriate way !
       TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,
                   tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord,
                   tkDynArray,tkInterfaceRaw);

       TTOrdType = (otSByte,otUByte,otSWord,otUWord,otSLong,otULong);

       TFloatType = (ftSingle,ftDouble,ftExtended,ftComp,ftCurr);
       TMethodKind = (mkProcedure,mkFunction,mkConstructor,mkDestructor,
                      mkClassProcedure, mkClassFunction);
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
  If Length(Name)=0 then exit(-1);
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
  Low-level calling of methods.
  ---------------------------------------------------------------------}

{$I typinfo.inc}

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

begin
         case (PropInfo^.PropProcs shr 4) and 3 of
            ptfield:
              IsStoredProp:=PBoolean(Pointer(Instance)+Longint(PropInfo^.StoredProc))^;
            ptstatic:
              IsStoredProp:=CallBooleanFunc(Instance,PropInfo^.StoredProc,0,0);
            ptvirtual:
              IsStoredProp:=CallBooleanFunc(Instance,ppointer(Pointer(Instance.ClassType)+Longint(PropInfo^.StoredProc))^,0,0);
            ptconst:
              IsStoredProp:=LongBool(PropInfo^.StoredProc);
         end;
end;



Procedure GetPropInfos(TypeInfo : PTypeInfo;PropList : PPropList);
{
        Store Pointers to property information in the list pointed
        to by proplist. PRopList must contain enough space to hold ALL
        properties.
}
Type PWord = ^Word;

Var TD : PTypeData;
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

Var I : Longint;

begin
     I:=0;
     While (I<Count) and (PI^.Name>PL^[I]^.Name) do Inc(I);
     If I<Count then
       Move(PL^[I], PL^[I+1], (Count - I) * SizeOf(Pointer));
     PL^[I]:=PI;
end;

Function GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds;
      PropList : PPropList) : Integer;

      {
        Store Pointers to property information OF A CERTAIN KIND in the list pointed
        to by proplist. PRopList must contain enough space to hold ALL
        properties.
      }
Var TempList : PPropList;
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

Procedure SetIndexValues (P: PPRopInfo; Var Index,IValue : Longint);

begin
    Index:=((P^.PropProcs shr 6) and 1);
    If Index<>0 then
      IValue:=P^.Index
    else
      IValue:=0;
end;

{ ---------------------------------------------------------------------
  Property access functions
  ---------------------------------------------------------------------}

{ ---------------------------------------------------------------------
  Ordinal properties
  ---------------------------------------------------------------------}

Function GetOrdProp(Instance : TObject;PropInfo : PPropInfo) : Longint;

var
         value,Index,Ivalue : longint;
         TypeInfo: PTypeInfo;

begin
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              Value:=PLongint(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
            ptstatic:
              Value:=CallIntegerFunc(Instance,PropInfo^.GetProc,Index,IValue);
            ptvirtual:
              Value:=CallIntegerFunc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,Index,IValue);
         end;
         { cut off unnecessary stuff }
         TypeInfo := PropInfo^.PropType;
         case TypeInfo^.Kind of
           tkChar, tkBool:
             Value:=Value and $ff;
           tkWChar:
             Value:=Value and $ffff;
           tkInteger:
             case GetTypeData(TypeInfo)^.OrdType of
                otSWord,otUWord:
                  Value:=Value and $ffff;
                otSByte,otUByte:
                  Value:=Value and $ff;
             end;
         end;
         GetOrdProp:=Value;
end;

Procedure SetOrdProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Longint);

var
        Index,IValue : Longint;
        DataSize: Integer;

begin
         if PropInfo^.PropType^.Kind <> tkClass then
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
    else
        DataSize := 4;
           end
         else
           DataSize := 4;
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs shr 2) and 3 of
            ptfield:
              case DataSize of
                1: PByte(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Byte(Value);
                2: PWord(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Word(Value);
                4: PLongint(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
              end;
            ptstatic:
              CallIntegerProc(Instance,PropInfo^.SetProc,Value,Index,IValue);
            ptvirtual:
              CallIntegerProc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^,Value,Index,IValue);
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
var
      Index, IValue: LongInt;
      ShortResult: ShortString;
begin
      SetIndexValues(PropInfo, Index, IValue);
      case Propinfo^.PropType^.Kind of
        tkSString:
          case (PropInfo^.PropProcs) and 3 of
            ptField:
              Result := PShortString(Pointer(Instance) + LongWord(PropInfo^.GetProc))^;
            ptStatic:
              begin
                CallSStringFunc(Instance, PropInfo^.GetProc, Index, IValue, ShortResult);
                Result := ShortResult;
              end;
            ptVirtual:
              begin
                CallSStringFunc(Instance, PPointer(Pointer(Instance.ClassType) +
                  LongWord(PropInfo^.GetProc))^, Index, IValue, ShortResult);
              Result := ShortResult;
            end;
          end;
        tkAString:
          case (PropInfo^.PropProcs) and 3 of
            ptField:
              Result := PAnsiString(Pointer(Instance) + LongWord(PropInfo^.GetProc))^;
            ptStatic:
              Pointer(Result) := Pointer(LongWord(CallIntegerFunc(Instance, PropInfo^.GetProc, Index, IValue)));
            ptVirtual:
              Pointer(Result) := Pointer(LongWord(CallIntegerFunc(Instance,
                PPointer(Pointer(Instance.ClassType) + LongWord(PropInfo^.GetProc))^, Index, IValue)));
          end;
        else
          // Property is neither of type AnsiString nor of type ShortString
          SetLength(Result, 0);
      end;
end;

Procedure SetAStrProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : AnsiString);

{
Dirty trick based on fact that AnsiString is just a pointer,
hence can be treated like an integer type.
}
var
         Index,Ivalue : Longint;

begin
         SetIndexValues(PropInfo,Index,IValue);
         case (PropInfo^.PropProcs shr 2) and 3 of
            ptfield:
              PAnsiString(Pointer(Instance) + Longint(PropInfo^.SetProc))^ := Value;
            ptstatic:
              CallIntegerProc(Instance,PropInfo^.SetProc,Longint(Pointer(Value)),Index,IValue);
            ptvirtual:
              CallIntegerProc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^,Longint(Pointer(Value)),Index,IValue);
         end;
end;

Procedure SetSStrProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : ShortString);

Var Index,IValue: longint;

begin
      SetIndexValues(PRopInfo,Index,IValue);
         case (PropInfo^.PropProcs shr 2) and 3 of
            ptfield:
              PShortString(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
            ptstatic:
              CallSStringProc(Instance,PropInfo^.SetProc,Value,Index,IValue);
            ptvirtual:
              CallSStringProc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^,Value,Index,IValue);
         end;
end;

Procedure SetStrProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : AnsiString);

begin
      Case Propinfo^.PropType^.Kind of
        tkSString : SetSStrProp(Instance,PropInfo,Value);
        tkAString : SetAStrProp(Instance,Propinfo,Value);
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
Function GetFloatProp(Instance : TObject;PropInfo : PPropInfo) : Extended;

var
  Index,Ivalue : longint;
  Value : Extended;

begin
  SetIndexValues(PropInfo,Index,Ivalue);
  case (PropInfo^.PropProcs) and 3 of
    ptField:
      Case GetTypeData(PropInfo^.PropType)^.FloatType of
       ftSingle:
         Value:=PSingle(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
       ftDouble:
         Value:=PDouble(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
       ftExtended:
         Value:=PExtended(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
{$ifndef m68k}
       ftcomp:
         Value:=PComp(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
{$endif m68k}
       end;

    ptStatic:
      Case GetTypeData(PropInfo^.PropType)^.FloatType of
       ftSingle:
         Value:=CallSingleFunc(Instance,PropInfo^.GetProc,Index,IValue);
       ftDouble:
         Value:=CallDoubleFunc(Instance,PropInfo^.GetProc,Index,IValue);
       ftExtended:
         Value:=CallExtendedFunc(Instance,PropInfo^.GetProc,Index,IValue);
      end;

    ptVirtual:
      Case GetTypeData(PropInfo^.PropType)^.FloatType of
       ftSingle:
         Value:=CallSingleFunc(Instance,
              PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,
              Index,IValue);
       ftDouble:
         Value:=CallDoubleFunc(Instance,
              PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,
              Index,IValue);
       ftExtended:
         Value:=CallExtendedFunc(Instance,
              PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,
              Index,IValue);
      end;
  end;
  Result:=Value;
end;

Procedure SetFloatProp(Instance : TObject;PropInfo : PPropInfo;
  Value : Extended);

type
  TSetExtendedProc = procedure(const AValue: Extended) of object;
  TSetExtendedProcIndex = procedure(Index: integer; const AValue: Extended) of object;
  TSetDoubleProc = procedure(const AValue: Double) of object;
  TSetDoubleProcIndex = procedure(Index: integer; const AValue: Double) of object;
  TSetSingleProc = procedure(const AValue: Single) of object;
  TSetSingleProcIndex = procedure(Index: integer; const AValue: Single) of object;

Var IValue,Index : longint;
  AMethod: TMethod;

begin
  SetIndexValues(PropInfo,Index,Ivalue);
  case (PropInfo^.PropProcs shr 2) and 3 of

    ptfield:
      Case GetTypeData(PropInfo^.PropType)^.FloatType of
        ftSingle:
          PSingle(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
        ftDouble:
          PDouble(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
        ftExtended:
          PExtended(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
{$ifndef m68k}
       ftcomp:
          PComp(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Comp(Value);
{$endif m68k}
        { Uncommenting this code results in a internal error!!
       ftFixed16:
         PFixed16(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
       ftfixed32:
         PFixed32(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
       }
       end;

    ptStatic, ptVirtual:
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=
            PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        Case GetTypeData(PropInfo^.PropType)^.FloatType of
          ftSingle:
            if Index=0 then
              TSetSingleProc(AMethod)(Value)
            else
              TSetSingleProcIndex(AMethod)(IValue,Value);

          ftDouble:
            if Index=0 then
              TSetDoubleProc(AMethod)(Value)
            else
              TSetDoubleProcIndex(AMethod)(IValue,Value);

          ftExtended:
            if Index=0 then
              TSetExtendedProc(AMethod)(Value)
            else
              TSetExtendedProcIndex(AMethod)(IValue,Value);
        end;
      end;
  end;
end;

Function GetFloatProp(Instance: TObject; const PropName: string): Extended;
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
         {!!!!!!!!!!!}
         Result:=nil;
end;

Procedure SetVariantProp(Instance : TObject;PropInfo : PPropInfo;
      const Value: Variant);

begin
         {!!!!!!!!!!!}
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

var
         value: PMethod;
         Index,Ivalue : longint;

begin
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              Value:=PMethod(Pointer(Instance)+Longint(PropInfo^.GetProc));
            ptstatic:
              Value:=PMethod(LongInt(CallIntegerFunc(Instance,PropInfo^.GetProc,Index,IValue)));
            ptvirtual:
              Value:=PMethod(LongInt(CallIntegerFunc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,Index,IValue)));
         end;
         GetMethodProp:=Value^;
end;

Procedure SetMethodProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : TMethod);

var
        Index,IValue : Longint;

begin
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs shr 2) and 3 of
            ptfield:
              PMethod(Pointer(Instance)+Longint(PropInfo^.SetProc))^ := Value;
            ptstatic:
              CallIntegerProc(Instance,PropInfo^.SetProc,Integer(@Value), Index, IValue);
            ptvirtual:
              CallIntegerProc(Instance,
                PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^,
                Integer(@Value), Index, IValue);
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
var
  Index, IValue: LongInt;
begin
      SetIndexValues(PropInfo,Index,Ivalue);
      case PropInfo^.PropProcs and 3 of
        ptfield:
          Result := PInt64(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
        ptstatic:
          Result := CallIntegerFunc(Instance, PropInfo^.GetProc, Index, IValue);
        ptvirtual:
          Result := CallIntegerFunc(Instance,
            PPointer(Pointer(Instance.ClassType) + LongInt(PropInfo^.GetProc))^,
            Index, IValue);
      end;
end;

procedure SetInt64Prop(Instance: TObject; PropInfo: PPropInfo; const Value: Int64);
var
  Index, IValue: LongInt;
begin
      SetIndexValues(PropInfo,Index,Ivalue);
      case PropInfo^.PropProcs and 3 of
        ptfield:
          PInt64(Pointer(Instance)+Longint(PropInfo^.GetProc))^ := Value;
        ptstatic:
          CallIntegerProc(Instance,PropInfo^.SetProc,Value,Index,IValue);
        ptvirtual:
          CallIntegerProc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^,Value,Index,IValue);
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
  Revision 1.15  2003-03-29 16:55:56  michael
  + Patch from Mattias Gaertner for single typeinfo

  Revision 1.14  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed

  Revision 1.13  2002/04/04 18:32:59  peter
    * merged getpropinfo fix

}
