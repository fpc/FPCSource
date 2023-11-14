{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 2023 by the Free Pascal development team

    This file provides the base for a devices registry.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
unit system.devices;

{$WARN 3018 off : Constructor should be public}
interface
{$IFDEF FPC_DOTTEDUNITS}
uses System.Types, System.SysUtils, System.Contnrs;
{$ELSE}
uses types, sysUtils, contnrs;
{$ENDIF}

{$SCOPEDENUMS ON}

const
  sDevAttrDisplayName = 'DisplayName'; 
  sDevAttrOPDefine    = 'OPDefine'; 
  sDevAttrCPPDefine   = 'CPPDefine'; 
  sDevAttrPlatforms   = 'Platforms';

Type
  TDeviceInfo = class;
  TDeviceInfoArray = Array of TDeviceInfo;

  { TDeviceInfoEnumerator }

  TDeviceInfoEnumerator = class
  private
    FPosition: Integer;
  public
    constructor Create;
    function GetCurrent: TDeviceInfo;
    function MoveNext: Boolean;
    property Current: TDeviceInfo read GetCurrent;
  end;

  { TBaseDeviceInfo }

  TBaseDeviceInfo = class
  public 
    type
      TDeviceClass = (Unknown, Desktop, Phone, MediaPlayer, Tablet, Automotive, Industrial, Embedded, Watch, Glasses, Elf, Dwarf, Wizard);
  private
    class var _Devices: TObjectList;
    class var _ThisDevice: TDeviceInfo;
    class constructor Init;
    class destructor Done;
    class procedure SetThisDevice(const Device: TDeviceInfo); static;
    class function GetThisDevice: TDeviceInfo; static;
    class function GetDeviceCount: Integer; static; inline;
    class function GetDevice(aIndex: Integer): TDeviceInfo; static;  inline;
    class function GetDeviceByID(const aID: string): TDeviceInfo; static;
    class function IndexOfDevice(const aID: string): Integer; static;
  public
    class function AddDevice(aDeviceClass: TDeviceClass; const aID: string;
      const aPhysicalScreenSize, aLogicalScreenSize: TSize; aPlatform: TOSVersion.TPlatform; aPixelsPerInch: Integer;
      AExclusive: Boolean = False): TBaseDeviceInfo; overload;
    class function AddDevice(aDeviceClass: TDeviceClass; const aID: string;
      const aMinPhysicalScreenSize, aMinLogicalScreenSize, aMaxPhysicalScreenSize, aMaxLogicalScreenSize: TSize;
      APlatform: TOSVersion.TPlatform; aPixelsPerInch: Integer; aExclusive: Boolean = False): TDeviceInfo; overload;
    class procedure RemoveDevice(const aID: string);
    class procedure ClearDevices;
    class function SelectDevices(aDeviceClass: TDeviceClass; const aPhysicalScreenSize, aLogicalScreenSize: TSize;
      aPlatform: TOSVersion.TPlatform; aPixelsPerInch: Integer; aSetThisDevice: Boolean = True): TDeviceInfoArray;
    class property DeviceCount: Integer read GetDeviceCount;
    class property Devices[Index: Integer]: TDeviceInfo read GetDevice;
    class property DeviceByID[const aID: string]: TDeviceInfo read GetDeviceByID;
    class property ThisDevice: TDeviceInfo read GetThisDevice write SetThisDevice;
  private
    FDeviceClass: TDeviceClass;
    FID: string;
    FPlatform: TOSVersion.TPlatform;
    FMinPhysicalScreenSize: TSize;
    FMinLogicalScreenSize: TSize;
    FMaxPhysicalScreenSize: TSize;
    FMaxLogicalScreenSize: TSize;
    FAspectRatio: Single;
    FLowDelta, FHighDelta: Single;
    FPixelsPerInch: Integer;
    FExclusive: Boolean;
    FAttributes: TFPStringHashTable;
    function GetAttribute(const Key: string): string;
    function GetMaxDiagonal: Single;
    function GetMinDiagonal: Single;
    property LowDelta: Single read FLowDelta write FLowDelta;
    property HighDelta: Single read FHighDelta write FHighDelta;
  Protected
    constructor Create(aDeviceClass: TDeviceClass; const aID: string;
                       const aMinPhysicalScreenSize, aMinLogicalScreenSize, aMaxPhysicalScreenSize, aMaxLogicalScreenSize: TSize;
                       aPlatform: TOSVersion.TPlatform; aPixelsPerInch: Integer; aExclusive: Boolean); overload;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function GetEnumerator: TDeviceInfoEnumerator;
    function Equals(Obj: TObject): Boolean; override;
    procedure AddAttribute(const aKey, aValue: string);
    function HasAttribute(const aKey: string): Boolean;
    function MatchDiagonal(const aDiag : Single) : Boolean;
    property DeviceClass: TDeviceClass read FDeviceClass;
    property Exclusive: Boolean read FExclusive;
    property ID: string read FID;
    property Platform: TOSVersion.TPlatform read FPlatform;
    property MinPhysicalScreenSize: TSize read FMinPhysicalScreenSize;
    property MinLogicalScreenSize: TSize read FMinLogicalScreenSize;
    property MaxPhysicalScreenSize: TSize read FMaxPhysicalScreenSize;
    property MaxLogicalScreenSize: TSize read FMaxLogicalScreenSize;
    property AspectRatio: Single read FAspectRatio;
    property PixelsPerInch: Integer read FPixelsPerInch;
    property MaxDiagonal: Single read GetMaxDiagonal;
    property MinDiagonal: Single read GetMinDiagonal;
    property Attributes[const Key: string]: string read GetAttribute;
  end;

  TDeviceInfo = class sealed (TBaseDeviceInfo);

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.SortBase;
{$ELSE}
uses sortbase;
{$ENDIF}

{ TBaseDeviceInfo }

class constructor TBaseDeviceInfo.Init;
begin
  _Devices:=TObjectList.Create(True);
end;

class destructor TBaseDeviceInfo.Done;
begin
  FreeAndNil(_Devices);
end;

class procedure TBaseDeviceInfo.SetThisDevice(const Device: TDeviceInfo);
begin
  _ThisDevice:=Device;
end;

class function TBaseDeviceInfo.GetThisDevice: TDeviceInfo;
begin
  Result:=_ThisDevice;
end;

class function TBaseDeviceInfo.GetDeviceCount: Integer;
begin
  Result:=_Devices.Count;
end;

class function TBaseDeviceInfo.GetDevice(aIndex: Integer): TDeviceInfo;
begin
  Result:=TDeviceInfo(_Devices[aIndex])
end;

class function TBaseDeviceInfo.GetDeviceByID(const aID: string): TDeviceInfo;

var
  Idx : Integer;

begin
  Idx:=IndexOfDevice(aId);
  if Idx<>-1 then
    Result:=GetDevice(Idx)
  else
    Result:=Nil;
end;

class function TBaseDeviceInfo.IndexOfDevice(const aID: string): Integer;
begin
  Result:=GetDeviceCount-1;
  While (Result>=0) and Not SameText(GetDevice(Result).ID,aID) do
    Dec(Result);
end;

function TBaseDeviceInfo.GetEnumerator: TDeviceInfoEnumerator;
begin
  Result:=TDeviceInfoEnumerator.Create;
end;

class function TBaseDeviceInfo.AddDevice(aDeviceClass: TDeviceClass;
  const aID: string; const aPhysicalScreenSize, aLogicalScreenSize: TSize;
  aPlatform: TOSVersion.TPlatform; aPixelsPerInch: Integer; AExclusive: Boolean
  ): TBaseDeviceInfo;

begin
  Result:=TBaseDeviceInfo.Create(aDeviceClass,aID,
                        aPhysicalScreenSize,aLogicalScreenSize,
                        aPhysicalScreenSize,aLogicalScreenSize,
                        aPlatForm,aPixelsPerInch,
                        aExclusive);
  _Devices.Add(Result);
end;

class function TBaseDeviceInfo.AddDevice(aDeviceClass: TDeviceClass;
  const aID: string; const aMinPhysicalScreenSize, aMinLogicalScreenSize,
  aMaxPhysicalScreenSize, aMaxLogicalScreenSize: TSize;
  APlatform: TOSVersion.TPlatform; aPixelsPerInch: Integer; aExclusive: Boolean
  ): TDeviceInfo;
begin
  Result:=TDeviceInfo.Create(aDeviceClass,aID,
                        aMinPhysicalScreenSize,aMinLogicalScreenSize,
                        aMaxPhysicalScreenSize,amaxLogicalScreenSize,
                        aPlatForm,aPixelsPerInch,
                        aExclusive);
  _Devices.Add(Result);
end;

class procedure TBaseDeviceInfo.RemoveDevice(const aID: string);

var
  Idx : Integer;

begin
  Idx:=IndexOfDevice(aId);
  If Idx<>-1 then
    _Devices.Delete(Idx);
end;

class procedure TBaseDeviceInfo.ClearDevices;
begin
  _Devices.Clear;
end;

function CompareDevices (Item1, Item2, aContext : Pointer): Integer;

var
  aThis:  TBaseDeviceInfo absolute aContext;
  Dev1 : TBaseDeviceInfo absolute Item1;
  Dev2 : TBaseDeviceInfo absolute Item2;
  D1,D2 : Single;

begin
  // Distance to this.Diagonal
  D1:=Abs(Dev1.MaxDiagonal-aThis.MaxDiagonal);
  D2:=Abs(Dev2.MaxDiagonal-aThis.MaxDiagonal);
  Result:=Round(D1-D2); // Closest wins
end;


class function TBaseDeviceInfo.SelectDevices(aDeviceClass: TDeviceClass;
  const aPhysicalScreenSize, aLogicalScreenSize: TSize;
  aPlatform: TOSVersion.TPlatform; aPixelsPerInch: Integer;
  aSetThisDevice: Boolean): TDeviceInfoArray;

var
  I: Integer;
  aFound, aThis,aDev : TDeviceInfo;
  aList: TDeviceInfoArray;
  nCount, ResCount : Integer;
  aDiag : Single;
  isMatch : Boolean;

begin
  Result:=[];
  aList:=[];
  if aPixelsPerInch=0 then
    aDiag:=0
  else
    aDiag:=Sqrt(Sqr(aPhysicalScreenSize.cx) + Sqr(aPhysicalScreenSize.cy))/aPixelsPerInch;
  SetLength(aList,_Devices.Count);
  nCount:=0;
  aFound:=nil;
  aThis:=TDeviceInfo.Create(aDeviceClass,'', aPhysicalScreenSize, aLogicalScreenSize,
                            aPhysicalScreenSize, aLogicalScreenSize, aPlatform, aPixelsPerInch, True);
  try
    // Construct a list of platform matching devices.
    for I:=0 to DeviceCount-1 do
      begin
      aDev:=GetDevice(I);
      if aDev.Equals(aThis) then
        aFound:=aDev;
      isMatch:=(aDev.Platform=aPlatform);
      if IsMatch then
        begin
        aList[nCount]:=aDev;
        Inc(nCount);
        end;
      end;
    // Sort the list on distance to diagonal of aThis
    QuickSort_PtrList_Context(Pointer(aList),nCount,@CompareDevices,aThis);
    // Prepare result
    SetLength(Result,nCount);
    ResCount:=0;
    // Add the ones where device class and diagonal matches.
    For I:=0 to nCount-1 do
      begin
      aDev:=aList[i];
      if (aDev.DeviceClass=aDeviceClass) and aDev.MatchDiagonal(aDiag) then
        begin
        Result[ResCount]:=aDev;
        inc(ResCount);
        aList[i]:=Nil;
        end;
      end;
    // Add the non-exclusive ones with matching diagonal
    For I:=0 to nCount-1 do
      begin
      aDev:=aList[i];
      if (aDev<>Nil) and (Not aDev.Exclusive) and aDev.MatchDiagonal(aDiag) then
        begin
        Result[ResCount]:=aDev;
        inc(ResCount);
        aList[i]:=Nil;
        end;
      end;
    // Add the rest
    For I:=0 to nCount-1 do
      begin
      aDev:=aList[i];
      if (aDev<>Nil) then
        begin
        Result[ResCount]:=aDev;
        inc(ResCount);
        aList[i]:=Nil;
        end;
      end;
    If aSetThisDevice then
      begin
      if Not Assigned(aFound) then
        begin
        aFound:=aThis;
        aThis:=Nil;
        end;
      _ThisDevice:=aFound;
      end;
  Finally
    aThis.Free;
  end;
end;

constructor TBaseDeviceInfo.Create(aDeviceClass: TDeviceClass; const aID: string;
  const aMinPhysicalScreenSize, aMinLogicalScreenSize, aMaxPhysicalScreenSize,
  aMaxLogicalScreenSize: TSize; aPlatform: TOSVersion.TPlatform;
  aPixelsPerInch: Integer; aExclusive: Boolean);

var
  aMaxr,aMinr : Single;

begin
  FID:=aID;
  FDeviceClass:=aDeviceClass;
  FPlatform:=aPlatform;
  FPixelsPerInch:=aPixelsPerInch;
  FExclusive:=aExclusive;
  FMinPhysicalScreenSize:=aMinPhysicalScreenSize;
  FMaxPhysicalScreenSize:=aMaxPhysicalScreenSize;
  FMinLogicalScreenSize:=aMinLogicalScreenSize;
  FMaxLogicalScreenSize:=aMaxLogicalScreenSize;
  FAttributes:=TFPStringHashTable.Create;
  aMaxr:=FMinPhysicalScreenSize.cy;
  if aMaxr<FMinPhysicalScreenSize.cx then
    aMaxr:=FMinPhysicalScreenSize.cx;
  aMinr:=FMinPhysicalScreenSize.cy;
  if aMinr>FMinPhysicalScreenSize.cx then
    aMinr:=FMinPhysicalScreenSize.cx;
  if aMinr=0 then
    FAspectRatio:=0
  else
    FAspectRatio:=aMaxr/aMinr;
end;

constructor TBaseDeviceInfo.Create;
begin
  Raise ENoConstructException.Create('Cannot create manually');
end;

function TBaseDeviceInfo.GetAttribute(const Key: string): string;
begin
  Result:=FAttributes.Items[Key];
end;

function TBaseDeviceInfo.GetMaxDiagonal: Single;
begin
  if FPixelsPerInch=0 then
    Result:=0
  else
    Result:=Sqrt(Sqr(FMaxPhysicalScreenSize.CX)+Sqr(FMaxPhysicalScreenSize.CY))/FPixelsPerInch;
end;

function TBaseDeviceInfo.GetMinDiagonal: Single;
begin
  if FPixelsPerInch=0 then
    Result:=0
  else
    Result:=Sqrt(Sqr(FMinPhysicalScreenSize.CX)+Sqr(FMinPhysicalScreenSize.CY))/FPixelsPerInch;
end;


destructor TBaseDeviceInfo.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited Destroy;
end;

function TBaseDeviceInfo.Equals(Obj: TObject): Boolean;
var
  D : TBaseDeviceInfo absolute Obj;
begin
  Result := (Obj = Self);
  if (Not Result) and (Obj is TBaseDeviceInfo) then
     Result:=(Platform=D.Platform)
             and (DeviceClass=D.DeviceClass)
             and (PixelsPerInch=D.PixelsPerInch)
             and (MaxPhysicalScreenSize=D.MaxPhysicalScreenSize)
             and (MaxLogicalScreenSize=D.MaxLogicalScreenSize)
             and (MinPhysicalScreenSize=D.MinPhysicalScreenSize)
             and (MinLogicalScreenSize=D.MinLogicalScreenSize);
end;

procedure TBaseDeviceInfo.AddAttribute(const aKey, aValue: string);
begin
  if FAttributes.Find(aKey)=Nil then
    FAttributes.Add(aKey,aValue);
end;

function TBaseDeviceInfo.HasAttribute(const aKey: string): Boolean;
begin
  Result:=FAttributes.Find(aKey)<>Nil;
end;

function TBaseDeviceInfo.MatchDiagonal(const aDiag: Single): Boolean;
begin
  Result:=(aDiag>=(MinDiagonal+LowDelta)) and (aDiag<=MaxDiagonal+HighDelta)
end;

{ TDeviceInfoEnumerator }

constructor TDeviceInfoEnumerator.Create;
begin
  FPosition:=-1;
end;

function TDeviceInfoEnumerator.GetCurrent: TDeviceInfo;
begin
  Result:=TDeviceInfo.GetDevice(FPosition);
end;

function TDeviceInfoEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result:=(FPosition<TDeviceInfo.GetDeviceCount);
end;

end.
