{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements a streaming interface

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_serialization_utils;

interface
uses
  SysUtils, Classes, Contnrs,
  sdo_types, sdo;

type

  TScopeType = ( stObject, stArray );
  TArrayStyle = ( asScoped, asEmbeded, asNone );

  ESDOSerializationException = class(ESDOException) end;

  TSerializationStyle = ( ssNodeSerialization, ssAttibuteSerialization );
  TNameStyle = ( nsUnqualified, nsQualified );

  TStreamBookmark = class(TObject)
  end;

  ISDOSerializerStream = interface
    ['{3C38D6E1-C4BE-4BFC-B16A-BD8A1A09A5E6}']
    function GetFormatName() : string;
    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
    procedure SetNameStyle(const AValue : TNameStyle);
    function GetNameStyle() : TNameStyle;
    function GetCurrentScope():string;
    procedure Clear();
    procedure Initialize();

    procedure BeginObject(
      Const AName      : string;
      Const ATypeInfo  : ISDOType
    );
    procedure BeginArray(
      const AName         : string;
      const AItemTypeInfo : ISDOType;
      const ABounds       : array of Integer
    );
    procedure NilCurrentScope();
    function IsCurrentScopeNil():Boolean;
    procedure EndScope();
    function BeginObjectRead(
      var   AScopeName : string;
      const ATypeInfo  : ISDOType
    ) : Integer;
    function BeginArrayRead(
      var   AScopeName : string;
      const ATypeInfo  : ISDOType;
      const AItemName  : string
    ):Integer;
    function GetScopeItemNames(
      const AItemStyle : TSerializationStyle;
      const AReturnList : TStrings
    ) : Integer;
    procedure EndScopeRead();

    procedure Put(
      const AName     : string;
      const ATypeInfo : ISDOType;
      const AData
    );overload;
    procedure Put(
      const ANameSpace,
            AName     : string;
      const ATypeInfo : ISDOType;
      const AData
    );overload;
    procedure PutScopeInnerValue(
      const ATypeInfo : ISDOType;
      const AData
    );
    function Get(
      const ATypeInfo : ISDOType;
      var   AName     : string;
      var   AData
    ) : Boolean;overload;
    function Get(
      const ANameSpace : string;
      const ATypeInfo  : ISDOType;
      var   AName      : string;
      var   AData
    ) : Boolean;overload;
    function GetScopeInnerValue(
      const ATypeInfo : ISDOType;
      var   AData
    ) : Boolean;
    function ReadBuffer(const AName : string) : string;
    //Please use this method if and _only_ if you do not have another way to achieve your aim!
    procedure WriteBuffer(const AValue : string);

    procedure SaveToStream(AStream : TStream);overload;
    procedure SaveToFile(const AFileName : string);overload;
    procedure LoadFromStream(AStream : TStream);overload;
    procedure LoadFromFile(const AFileName : string);overload;

    function GetBookMark() : TStreamBookmark;
    function GotoBookmark(const AValue : TStreamBookmark) : Boolean;
    // This procedures will raise exceptions!!!
    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  end;

  TObjectStackExCopyFunc = function (const AItem : TObject) : TObject;
  TObjectStackEx = class(TObjectStack)
  public
    function Clone(const ACopyFunc : TObjectStackExCopyFunc) : TObjectStackEx;
  end;

//resourcestring
  //SMSG_InvalidBookmark = 'Invalid bookmark.';

implementation

{ TObjectStackEx }

function TObjectStackEx.Clone(const ACopyFunc : TObjectStackExCopyFunc) : TObjectStackEx;
var
  i, c : PtrInt;
  rls , sls : TList;
begin
  Result := TObjectStackEx.Create();
  try
    sls := List;
    c := sls.Count;
    if ( c > 0 ) then begin
      rls := Result.List;
      rls.Capacity := sls.Capacity;
      for i := 0 to Pred(c) do
        rls.Add(ACopyFunc(TObject(sls[i])));
    end;
  except
    FreeAndNil(Result);
    raise;
  end
end;

end.
