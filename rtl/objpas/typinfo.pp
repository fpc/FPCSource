{
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
{$inline on}
{$h+}

  uses SysUtils;


// temporary types:

    type

{$MINENUMSIZE 1   this saves a lot of memory }
       // if you change one of the following enumeration types
       // you have also to change the compiler in an appropriate way !
       TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,
                   tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord,
                   tkDynArray,tkInterfaceRaw);

       TOrdType  = (otSByte,otUByte,otSWord,otUWord,otSLong,otULong);

       TFloatType = (ftSingle,ftDouble,ftExtended,ftComp,ftCurr);
       TMethodKind = (mkProcedure,mkFunction,mkConstructor,mkDestructor,
                      mkClassProcedure, mkClassFunction);
       TParamFlag     = (pfVar,pfConst,pfArray,pfAddress,pfReference,pfOut);
       TParamFlags    = set of TParamFlag;
       TIntfFlag      = (ifHasGuid,ifDispInterface,ifDispatch,ifHasStrGUID);
       TIntfFlags     = set of TIntfFlag;
       TIntfFlagsBase = set of TIntfFlag;

{$MINENUMSIZE DEFAULT}

   const
      ptField = 0;
      ptStatic = 1;
      ptVirtual = 2;
      ptConst = 3;

      tkString = tkSString;

   type
      TTypeKinds = set of TTypeKind;
		  ShortStringBase = string[255];

{$PACKRECORDS 1}
      TTypeInfo = record
         Kind : TTypeKind;
         Name : ShortString;
         // here the type data follows as TTypeData record
      end;

      PTypeInfo = ^TTypeInfo;
      PPTypeInfo = ^PTypeInfo;

{$PACKRECORDS C}
      PTypeData = ^TTypeData;
      TTypeData =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
         case TTypeKind of
            tkUnKnown,tkLString,tkWString,tkAString,tkVariant:
              ();
            tkInteger,tkChar,tkEnumeration,tkWChar,tkSet:
              (OrdType : TOrdType;
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
            tkInterface:
              (
               IntfParent: PTypeInfo;
               IntfFlags : TIntfFlagsBase;
               GUID: TGUID;
               IntfUnit: ShortString;
              );
            tkInterfaceRaw:
              (
               RawIntfParent: PTypeInfo;
               RawIntfFlags : TIntfFlagsBase;
               IID: TGUID;
               RawIntfUnit: ShortString;
               IIDStr: ShortString;
              );
			      tkDynArray: 
			        (
			        elSize     : PtrUInt;
			        elType2    : PPTypeInfo;
			        varType    : Longint;
			        elType     : PPTypeInfo;
			        DynUnitName: ShortStringBase
			        );
      end;

      // unsed, just for completeness
      TPropData =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
        PropCount : Word;
        PropList : record _alignmentdummy : ptrint; end;
      end;
{$PACKRECORDS 1}

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
Function GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds; PropList : PPropList;Sorted : boolean = true):longint;
Function GetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): SizeInt;
function GetPropList(AObject: TObject; out PropList: PPropList): Integer;



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
Function  GetOrdProp(Instance: TObject; PropInfo : PPropInfo) : Int64;
Function  GetOrdProp(Instance: TObject; const PropName: string): Int64;
Procedure SetOrdProp(Instance: TObject; PropInfo : PPropInfo;  Value : Int64);
Procedure SetOrdProp(Instance: TObject; const PropName: string; Value: Int64);

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
Procedure SetStrProp(Instance: TObject; PropInfo : PPropInfo; const Value : Ansistring);

Function GetWideStrProp(Instance: TObject; PropInfo: PPropInfo): WideString;
Function GetWideStrProp(Instance: TObject; const PropName: string): WideString;
Procedure SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString);
Procedure SetWideStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: WideString);

Function  GetFloatProp(Instance: TObject; PropInfo : PPropInfo) : Extended;
Function  GetFloatProp(Instance: TObject; const PropName: string): Extended;
Procedure SetFloatProp(Instance: TObject; const PropName: string; Value: Extended);
Procedure SetFloatProp(Instance: TObject; PropInfo : PPropInfo;  Value : Extended);

Function  GetObjectProp(Instance: TObject; const PropName: string): TObject;
Function  GetObjectProp(Instance: TObject; const PropName: string; MinClass: TClass): TObject;
Function  GetObjectProp(Instance: TObject; PropInfo: PPropInfo): TObject;
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
Function  GetVariantProp(Instance: TObject; PropInfo : PPropInfo): Variant;
Function  GetVariantProp(Instance: TObject; const PropName: string): Variant;
Procedure SetVariantProp(Instance: TObject; const PropName: string; const Value: Variant);
Procedure SetVariantProp(Instance: TObject; PropInfo : PPropInfo; const Value: Variant);


// Auxiliary routines, which may be useful
Function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;
Function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;
function GetEnumNameCount(enum1: PTypeInfo): SizeInt;

function SetToString(TypeInfo: PTypeInfo; Value: Integer; Brackets: Boolean) : String;
function SetToString(PropInfo: PPropInfo; Value: Integer; Brackets: Boolean) : String;
function SetToString(PropInfo: PPropInfo; Value: Integer) : String;
function StringToSet(PropInfo: PPropInfo; const Value: string): Integer;
function StringToSet(TypeInfo: PTypeInfo; const Value: string): Integer;

const
    BooleanIdents: array[Boolean] of String = ('False', 'True');
    DotSep: String = '.';

Type
  EPropertyError  = Class(Exception);
  TGetPropValue   = Function (Instance: TObject; const PropName: string; PreferStrings: Boolean) : Variant;
  TSetPropValue   = Procedure (Instance: TObject; const PropName: string; const Value: Variant);
  TGetVariantProp = Function (Instance: TObject; PropInfo : PPropInfo): Variant;
  TSetVariantProp = Procedure (Instance: TObject; PropInfo : PPropInfo; const Value: Variant);

Const
  OnGetPropValue   : TGetPropValue = Nil;
  OnSetPropValue   : TSetPropValue = Nil;
  OnGetVariantprop : TGetVariantProp = Nil;
  OnSetVariantprop : TSetVariantProp = Nil;

Implementation

uses rtlconsts;

type
  PMethod = ^TMethod;

{ ---------------------------------------------------------------------
  Auxiliary methods
  ---------------------------------------------------------------------}

function aligntoptr(p : pointer) : pointer;inline;
   begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=align(p,sizeof(p));
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
   end;


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
      sName: shortstring;

begin
  If Length(Name)=0 then
    exit(-1);
  sName := Name;
  PT:=GetTypeData(TypeInfo);
  Count:=0;
  Result:=-1;
  PS:=@PT^.NameList;
  While (Result=-1) and (PByte(PS)^<>0) do
    begin
      If ShortCompareText(PS^, sName) = 0 then
        Result:=Count;
      PS:=PShortString(pointer(PS)+PByte(PS)^+1);
      Inc(Count);
    end;
end;


function GetEnumNameCount(enum1: PTypeInfo): SizeInt;
var
  PS: PShortString;
  PT: PTypeData;
  Count: SizeInt;
begin
  PT:=GetTypeData(enum1);
  Count:=0;
  Result:=0;

  PS:=@PT^.NameList;
  While (PByte(PS)^<>0) do
    begin
      PS:=PShortString(pointer(PS)+PByte(PS)^+1);
      Inc(Count);
    end;

  Result := Count;
end;


Function SetToString(PropInfo: PPropInfo; Value: Integer; Brackets: Boolean) : String;

begin
  Result:=SetToString(PropInfo^.PropType,Value,Brackets);
end;

Function SetToString(TypeInfo: PTypeInfo; Value: Integer; Brackets: Boolean) : String;

{$ifdef FPC_NEW_BIGENDIAN_SETS}
type
  tsetarr = bitpacked array[0..31] of 0..1;
{$endif}
Var
  I : Integer;
  PTI : PTypeInfo;

begin
{$if defined(FPC_NEW_BIGENDIAN_SETS) and defined(FPC_BIG_ENDIAN)}
  case GetTypeData(TypeInfo)^.OrdType of
    otSByte,otUByte: Value:=Value shl 24;
    otSWord,otUWord: Value:=Value shl 16;
  end;
{$endif}

  PTI:=GetTypeData(TypeInfo)^.CompType;
  Result:='';
  For I:=0 to SizeOf(Integer)*8-1 do
    begin
{$ifdef FPC_NEW_BIGENDIAN_SETS}
      if (tsetarr(Value)[i]<>0) then
{$else}
      if ((Value and 1)<>0) then
{$endif}
        begin
          If Result='' then
            Result:=GetEnumName(PTI,i)
          else
            Result:=Result+','+GetEnumName(PTI,I);
        end;
{$ifndef FPC_NEW_BIGENDIAN_SETS}
      Value:=Value shr 1;
{$endif FPC_NEW_BIGENDIAN_SETS}
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

begin
  Result:=StringToSet(PropInfo^.PropType,Value);
end;

Function StringToSet(TypeInfo: PTypeInfo; const Value: string): Integer;
Var
  S,T : String;
  I : Integer;
  PTI : PTypeInfo;

begin
  Result:=0;
  PTI:=GetTypeData(TypeInfo)^.Comptype;
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
  GetTypeData:=PTypeData(aligntoptr(PTypeData(pointer(TypeInfo)+2+PByte(pointer(TypeInfo)+1)^)));
end;


{ ---------------------------------------------------------------------
  Basic Type information functions.
  ---------------------------------------------------------------------}

Function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;
var
  hp : PTypeData;
  i : longint;
  p : shortstring;
  pd : ^TPropData;
begin
  P:=PropName;  // avoid Ansi<->short conversion in a loop
  while Assigned(TypeInfo) do
    begin
      // skip the name
      hp:=GetTypeData(Typeinfo);
      // the class info rtti the property rtti follows immediatly
      pd:=aligntoptr(pointer(pointer(@hp^.UnitName)+Length(hp^.UnitName)+1));
      Result:=PPropInfo(@pd^.PropList);
      for i:=1 to pd^.PropCount do
        begin
          // found a property of that name ?
          if ShortCompareText(Result^.Name, P) = 0 then
            exit;
          // skip to next property
          Result:=PPropInfo(aligntoptr(pointer(@Result^.Name)+byte(Result^.Name[0])+1));
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
  TBooleanIndexFunc=function(Index:integer):boolean of object;
  TBooleanFunc=function:boolean of object;
var
  AMethod : TMethod;
begin
  case (PropInfo^.PropProcs shr 4) and 3 of
    ptfield:
      Result:=PBoolean(Pointer(Instance)+PtrUInt(PropInfo^.StoredProc))^;
    ptconst:
      Result:=LongBool(PropInfo^.StoredProc);
    ptstatic,
    ptvirtual:
      begin
        if (PropInfo^.PropProcs shr 4) and 3=ptstatic then
          AMethod.Code:=PropInfo^.StoredProc
        else
          AMethod.Code:=ppointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.StoredProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
           Result:=TBooleanIndexFunc(AMethod)(PropInfo^.Index)
        else
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
  // Get this objects TOTAL published properties count
  TD:=GetTypeData(TypeInfo);
  // Clear list
  FillChar(PropList^,TD^.PropCount*sizeof(Pointer),0);
  repeat
    TD:=GetTypeData(TypeInfo);
    // published properties count for this object
    TP:=aligntoptr(PPropInfo(aligntoptr((Pointer(@TD^.UnitName)+Length(TD^.UnitName)+1))));
    Count:=PWord(TP)^;
    // Now point TP to first propinfo record.
    Inc(Pointer(TP),SizeOF(Word));
    tp:=aligntoptr(tp);
    While Count>0 do
      begin
        // Don't overwrite properties with the same name
        if PropList^[TP^.NameIndex]=nil then
          PropList^[TP^.NameIndex]:=TP;
        // Point to TP next propinfo record.
        // Located at Name[Length(Name)+1] !
        TP:=aligntoptr(PPropInfo(pointer(@TP^.Name)+PByte(@TP^.Name)^+1));
        Dec(Count);
      end;
    TypeInfo:=TD^.Parentinfo;
  until TypeInfo=nil;
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

Procedure InsertPropnosort (PL : PProplist;PI : PPropInfo; Count : longint);
begin
  PL^[Count]:=PI;
end;

Type TInsertProp = Procedure (PL : PProplist;PI : PPropInfo; Count : longint);

//Const InsertProps : array[false..boolean] of TInsertProp = (InsertPropNoSort,InsertProp);

Function  GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds; PropList : PPropList;Sorted : boolean = true):longint;

{
  Store Pointers to property information OF A CERTAIN KIND in the list pointed
  to by proplist. PRopList must contain enough space to hold ALL
  properties.
}

Var
  TempList : PPropList;
  PropInfo : PPropinfo;
  I,Count : longint;
  DoInsertProp : TInsertProp;
begin
  if sorted then
    DoInsertProp:=@InsertProp
  else
    DoInsertProp:=@InsertPropnosort;
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
                If (PropList<>Nil) then
                  DoInsertProp(PropList,PropInfo,Result);
                Inc(Result);
              end;
          end;
      finally
        FreeMem(TempList,Count*SizeOf(Pointer));
      end;
    end;
end;


Function GetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): SizeInt;
begin
  result:=GetTypeData(TypeInfo)^.Propcount;
  if result>0 then
    begin
      getmem(PropList,result*sizeof(pointer));
      GetPropInfos(TypeInfo,PropList);
    end;
end;


function GetPropList(AObject: TObject; out PropList: PPropList): Integer;
begin
  Result := GetPropList(PTypeInfo(AObject.ClassInfo), PropList);
end;

{ ---------------------------------------------------------------------
  Property access functions
  ---------------------------------------------------------------------}

{ ---------------------------------------------------------------------
  Ordinal properties
  ---------------------------------------------------------------------}

Function GetOrdProp(Instance : TObject;PropInfo : PPropInfo) : Int64;
type
  TGetInt64ProcIndex=function(index:longint):Int64 of object;
  TGetInt64Proc=function():Int64 of object;
  TGetIntegerProcIndex=function(index:longint):longint of object;
  TGetIntegerProc=function:longint of object;
  TGetWordProcIndex=function(index:longint):word of object;
  TGetWordProc=function:word of object;
  TGetByteProcIndex=function(index:longint):Byte of object;
  TGetByteProc=function:Byte of object;
var
  TypeInfo: PTypeInfo;
  AMethod : TMethod;
  DataSize: Integer;
  OrdType: TOrdType;
  Signed: Boolean;
begin
  Result:=0;

  TypeInfo := PropInfo^.PropType;
  Signed := false;
  DataSize := 4;
  case TypeInfo^.Kind of
{$ifdef cpu64}
    tkInterface,
    tkInterfaceRaw,
    tkDynArray,
    tkClass:
      DataSize:=8;
{$endif cpu64}
    tkChar, tkBool:
      DataSize:=1;
    tkWChar:
      DataSize:=2;
    tkSet,
    tkEnumeration,
    tkInteger:
      begin
        OrdType:=GetTypeData(TypeInfo)^.OrdType;
        case OrdType of
          otSByte,otUByte: DataSize := 1;
          otSWord,otUWord: DataSize := 2;
        end;
        Signed := OrdType in [otSByte,otSWord,otSLong];
      end;
    tkInt64 :
      begin
        DataSize:=8;
        Signed:=true;
      end;
    tkQword :
      begin
        DataSize:=8;
        Signed:=false;
      end;
  end;

  case (PropInfo^.PropProcs) and 3 of
    ptfield:
      if Signed then begin
        case DataSize of
          1: Result:=PShortInt(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          2: Result:=PSmallInt(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          4: Result:=PLongint(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          8: Result:=PInt64(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
        end;
      end else begin
        case DataSize of
          1: Result:=PByte(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          2: Result:=PWord(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          4: Result:=PLongint(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          8: Result:=PInt64(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
        end;
      end;
    ptstatic,
    ptvirtual :
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then begin
          case DataSize of
            1: Result:=TGetByteProcIndex(AMethod)(PropInfo^.Index);
            2: Result:=TGetWordProcIndex(AMethod)(PropInfo^.Index);
            4: Result:=TGetIntegerProcIndex(AMethod)(PropInfo^.Index);
            8: result:=TGetInt64ProcIndex(AMethod)(PropInfo^.Index)
          end;
        end else begin
          case DataSize of
            1: Result:=TGetByteProc(AMethod)();
            2: Result:=TGetWordProc(AMethod)();
            4: Result:=TGetIntegerProc(AMethod)();
            8: result:=TGetInt64Proc(AMethod)();
          end;
        end;
        if Signed then begin
          case DataSize of
            1: Result:=ShortInt(Result);
            2: Result:=SmallInt(Result);
          end;
        end;
      end;
  end;
end;

Procedure SetOrdProp(Instance : TObject;PropInfo : PPropInfo;Value : Int64);
type
  TSetInt64ProcIndex=procedure(index:longint;i:Int64) of object;
  TSetInt64Proc=procedure(i:Int64) of object;
  TSetIntegerProcIndex=procedure(index,i:longint) of object;
  TSetIntegerProc=procedure(i:longint) of object;
var
  DataSize: Integer;
  AMethod : TMethod;
begin
  if PropInfo^.PropType^.Kind in [tkInt64,tkQword
  { why do we have to handle classes here, see also below? (FK) }
{$ifdef cpu64}
    ,tkInterface
    ,tkInterfaceRaw
    ,tkDynArray
    ,tkClass
{$endif cpu64}
    ] then
    DataSize := 8
  else
    DataSize := 4;
  if not(PropInfo^.PropType^.Kind in [tkInt64,tkQword,tkClass]) then
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
        1: PByte(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Byte(Value);
        2: PWord(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Word(Value);
        4: PLongint(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Longint(Value);
        8: PInt64(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
      end;
    ptstatic,
    ptvirtual :
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if datasize=8 then
          begin
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              TSetInt64ProcIndex(AMethod)(PropInfo^.Index,Value)
            else
              TSetInt64Proc(AMethod)(Value);
          end
        else
          begin
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              TSetIntegerProcIndex(AMethod)(PropInfo^.Index,Value)
            else
              TSetIntegerProc(AMethod)(Value);
          end;
      end;
  end;
end;


Function GetOrdProp(Instance: TObject; const PropName: string): Int64;
begin
  Result:=GetOrdProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetOrdProp(Instance: TObject; const PropName: string;  Value: Int64);
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
  Int64 wrappers
  ---------------------------------------------------------------------}

Function GetInt64Prop(Instance: TObject; PropInfo: PPropInfo): Int64;
begin
  Result:=GetOrdProp(Instance,PropInfo);
end;


procedure SetInt64Prop(Instance: TObject; PropInfo: PPropInfo; const Value: Int64);
begin
  SetOrdProp(Instance,PropInfo,Value);
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


Function GetObjectProp(Instance: TObject; PropInfo : PPropInfo): TObject;
begin
  Result:=GetObjectProp(Instance,PropInfo,Nil);
end;


Function GetObjectProp(Instance: TObject; PropInfo : PPropInfo; MinClass: TClass): TObject;
begin
{$ifdef cpu64}
  Result:=TObject(GetInt64Prop(Instance,PropInfo));
{$else cpu64}
  Result:=TObject(PtrInt(GetOrdProp(Instance,PropInfo)));
{$endif cpu64}
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
{$ifdef cpu64}
  SetInt64Prop(Instance,PropInfo,Int64(Value));
{$else cpu64}
  SetOrdProp(Instance,PropInfo,Integer(Value));
{$endif cpu64}
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
    tkWString:
      Result:=GetWideStrProp(Instance,PropInfo);
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
                AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
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
                AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
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
    tkWString:
      SetWideStrProp(Instance,PropInfo,Value);
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
                AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
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
                AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
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


Function GetWideStrProp(Instance: TObject; const PropName: string): WideString;
begin
  Result:=GetWideStrProp(Instance, FindPropInfo(Instance, PropName));
end;


procedure SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString);
begin
  SetWideStrProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Function GetWideStrProp(Instance: TObject; PropInfo: PPropInfo): WideString;
type
  TGetWideStrProcIndex=function(index:longint):WideString of object;
  TGetWideStrProc=function():WideString of object;
var
  AMethod : TMethod;
begin
  Result:='';
  case Propinfo^.PropType^.Kind of
    tkSString,tkAString:
      Result:=GetStrProp(Instance,PropInfo);
    tkWString:
      begin
        case (PropInfo^.PropProcs) and 3 of
          ptField:
            Result := PWideString(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          ptstatic,
          ptvirtual :
            begin
              if (PropInfo^.PropProcs and 3)=ptStatic then
                AMethod.Code:=PropInfo^.GetProc
              else
                AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                Result:=TGetWideStrProcIndex(AMethod)(PropInfo^.Index)
              else
                Result:=TGetWideStrProc(AMethod)();
            end;
        end;
      end;
  end;
end;


Procedure SetWideStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: WideString);
type
  TSetWideStrProcIndex=procedure(index:longint;s:WideString) of object;
  TSetWideStrProc=procedure(s:WideString) of object;
var
  AMethod : TMethod;
begin
  case Propinfo^.PropType^.Kind of
    tkSString,tkAString:
       SetStrProp(Instance,PropInfo,Value);
    tkWString:
      begin
        case (PropInfo^.PropProcs shr 2) and 3 of
          ptField:
            PWideString(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
          ptstatic,
          ptvirtual :
            begin
              if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
                AMethod.Code:=PropInfo^.SetProc
              else
                AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                TSetWideStrProcIndex(AMethod)(PropInfo^.Index,Value)
              else
                TSetWideStrProc(AMethod)(Value);
            end;
        end;
      end;
  end;
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
  TGetCurrencyProc = function : Currency of object;
  TGetCurrencyProcIndex = function(Index: integer) : Currency of object;
var
  AMethod : TMethod;
begin
  Result:=0.0;
  case PropInfo^.PropProcs and 3 of
    ptField:
      Case GetTypeData(PropInfo^.PropType)^.FloatType of
       ftSingle:
         Result:=PSingle(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
       ftDouble:
         Result:=PDouble(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
       ftExtended:
         Result:=PExtended(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
       ftcomp:
         Result:=PComp(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
       ftcurr:
         Result:=PCurrency(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
       end;
    ptStatic,
    ptVirtual:
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        Case GetTypeData(PropInfo^.PropType)^.FloatType of
          ftSingle:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              Result:=TGetSingleProc(AMethod)()
            else
              Result:=TGetSingleProcIndex(AMethod)(PropInfo^.Index);
          ftDouble:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              Result:=TGetDoubleProc(AMethod)()
            else
              Result:=TGetDoubleProcIndex(AMethod)(PropInfo^.Index);
          ftExtended:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              Result:=TGetExtendedProc(AMethod)()
            else
              Result:=TGetExtendedProcIndex(AMethod)(PropInfo^.Index);
          ftCurr:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              Result:=TGetCurrencyProc(AMethod)()
            else
              Result:=TGetCurrencyProcIndex(AMethod)(PropInfo^.Index);
        end;
      end;
  end;
end;


Procedure SetFloatProp(Instance : TObject;PropInfo : PPropInfo; Value : Extended);
type
  TSetExtendedProc = procedure(const AValue: Extended) of object;
  TSetExtendedProcIndex = procedure(Index: integer; AValue: Extended) of object;
  TSetDoubleProc = procedure(const AValue: Double) of object;
  TSetDoubleProcIndex = procedure(Index: integer; AValue: Double) of object;
  TSetSingleProc = procedure(const AValue: Single) of object;
  TSetSingleProcIndex = procedure(Index: integer; AValue: Single) of object;
  TSetCurrencyProc = procedure(const AValue: Currency) of object;
  TSetCurrencyProcIndex = procedure(Index: integer;  AValue: Currency) of object;
Var
  AMethod : TMethod;
begin
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptfield:
      Case GetTypeData(PropInfo^.PropType)^.FloatType of
        ftSingle:
          PSingle(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
        ftDouble:
          PDouble(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
        ftExtended:
          PExtended(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
{$ifdef FPC_COMP_IS_INT64}
        ftComp:
          PComp(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=trunc(Value);
{$else FPC_COMP_IS_INT64}
        ftComp:
          PComp(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
{$endif FPC_COMP_IS_INT64}
        ftCurr:
          PCurrency(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
       end;
    ptStatic,
    ptVirtual:
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        Case GetTypeData(PropInfo^.PropType)^.FloatType of
          ftSingle:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              TSetSingleProc(AMethod)(Value)
            else
              TSetSingleProcIndex(AMethod)(PropInfo^.Index,Value);
          ftDouble:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              TSetDoubleProc(AMethod)(Value)
            else
              TSetDoubleProcIndex(AMethod)(PropInfo^.Index,Value);
          ftExtended:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              TSetExtendedProc(AMethod)(Value)
            else
              TSetExtendedProcIndex(AMethod)(PropInfo^.Index,Value);
          ftCurr:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              TSetCurrencyProc(AMethod)(Value)
            else
              TSetCurrencyProcIndex(AMethod)(PropInfo^.Index,Value);
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
  Method properties
  ---------------------------------------------------------------------}


Function GetMethodProp(Instance : TObject;PropInfo : PPropInfo) : TMethod;
type
  TGetMethodProcIndex=function(Index: Longint): TMethod of object;
  TGetMethodProc=function(): TMethod of object;
var
  value: PMethod;
  AMethod : TMethod;
begin
  Result.Code:=nil;
  Result.Data:=nil;
  case (PropInfo^.PropProcs) and 3 of
    ptfield:
      begin
        Value:=PMethod(Pointer(Instance)+PtrUInt(PropInfo^.GetProc));
        if Value<>nil then
          Result:=Value^;
      end;
    ptstatic,
    ptvirtual :
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          Result:=TGetMethodProcIndex(AMethod)(PropInfo^.Index)
        else
          Result:=TGetMethodProc(AMethod)();
      end;
  end;
end;


Procedure SetMethodProp(Instance : TObject;PropInfo : PPropInfo; const Value : TMethod);
type
  TSetMethodProcIndex=procedure(index:longint;p:TMethod) of object;
  TSetMethodProc=procedure(p:TMethod) of object;
var
  AMethod : TMethod;
begin
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptfield:
      PMethod(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^ := Value;
    ptstatic,
    ptvirtual :
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          TSetMethodProcIndex(AMethod)(PropInfo^.Index,Value)
        else
          TSetMethodProc(AMethod)(Value);
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
  Variant properties
  ---------------------------------------------------------------------}

Procedure CheckVariantEvent(P : Pointer);

begin
  If (P=Nil) then
    Raise Exception.Create(SErrNoVariantSupport);
end;

Function GetVariantProp(Instance : TObject;PropInfo : PPropInfo): Variant;
begin
  CheckVariantEvent(Pointer(OnGetVariantProp));
  Result:=OnGetVariantProp(Instance,PropInfo);
end;


Procedure SetVariantProp(Instance : TObject;PropInfo : PPropInfo; const Value: Variant);
begin
   CheckVariantEvent(Pointer(OnSetVariantProp));
   OnSetVariantProp(Instance,PropInfo,Value);
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
  All properties through variant.
  ---------------------------------------------------------------------}

Function GetPropValue(Instance: TObject; const PropName: string): Variant;
begin
  Result:=GetPropValue(Instance,PropName,True);
end;


Function GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;

begin
  CheckVariantEvent(Pointer(OnGetPropValue));
  Result:=OnGetPropValue(Instance,PropName,PreferStrings)
end;

Procedure SetPropValue(Instance: TObject; const PropName: string;  const Value: Variant);

begin
  CheckVariantEvent(Pointer(OnSetPropValue));
  OnSetPropValue(Instance,PropName,Value);
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
