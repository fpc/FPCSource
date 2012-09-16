{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit contains some implementation utilities

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_imp_utils;

interface
uses
  SysUtils, Classes,
  sdo_types, sdo;

type

  TBitOrder = 0..7;

  TStringInterfaceItem = class
  private
    FIntf : IInterface;
  public
    constructor Create(const AIntf : IInterface);
    destructor Destroy();override;
    property Intf : IInterface read FIntf write FIntf;
  end;

  TStringInterfaceList = class
  private
    FList : TStringList;
  private
    function GetCount: PtrInt;
    function GetIntf(const AIndex: PtrInt): IInterface;
    function GetString(const AIndex: PtrInt): string;
    procedure SetIntf(const AIndex: PtrInt; const Value: IInterface);
    procedure SetString(const AIndex: PtrInt; const Value: string);
  public
    constructor Create();
    destructor Destroy();override;
    function Add(const AString : string) : PtrInt;overload;
    function Add(const AString : string; const AIntf : IInterface) : PtrInt;overload;
    procedure Delete(const AIndex : PtrInt);
    procedure Clear();
    function IndexOf(const AString : string) : PtrInt;overload;
    function IndexOf(const AIntf : IInterface) : PtrInt;overload;
    property Count : PtrInt read GetCount;
    property Strings[const AIndex : PtrInt] : string read GetString write SetString;
    property Intf[const AIndex : PtrInt] : IInterface read GetIntf write SetIntf;
  end;

  function IsStrEmpty(const AStr : AnsiString) : Boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetNextToken(var AStr : AnsiString; const ASeparator : AnsiChar) : AnsiString;overload;
{$IFDEF HAS_UNICODE}
  function IsStrEmpty(const AStr : UnicodeString) : Boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetNextToken(var AStr : UnicodeString; const ASeparator : AnsiChar) : UnicodeString;overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetNextToken(var AStr : UnicodeString; const ASeparator : UnicodeChar) : UnicodeString;overload;
{$ENDIF HAS_UNICODE}    
  function StringToVarBytes(const AValue : string) : TSDOBytes;
  function StreamToVarBytes(const AValue : TStream) : TSDOBytes; overload;
  function StreamToVarBytes(const AValue : TMemoryStream) : TSDOBytes; overload;
  function ExtractLocalName(const AValue : string; const ASeparator : string = ':') : string ;
  function IsValidName(const AStr : string) : Boolean;

  function IsBitON(const AData : Byte; const ABit : TBitOrder) : Boolean ;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure SetBit(var AData : Byte; const ABit : TBitOrder; const AValue : Boolean);{$IFDEF USE_INLINE}inline;{$ENDIF}

implementation


function IsValidName(const AStr : string) : Boolean;
var
  i, c : PtrInt;
begin
  Result := False;
  c := Length(AStr);
  if ( c > 0 ) then begin
    if ( AStr[1] in ['_', 'a'..'z', 'A'..'Z'] ) then begin
      Result := True;
      i := 2;
      while ( i <= c ) do begin
        if not ( AStr[i] in ['_', 'a'..'z', 'A'..'Z', '0'..'9'] ) then begin
          Result := False;
          Break;
        end;
        Inc(i);
      end;
    end;
  end;
end;

function GetNextToken(var AStr : AnsiString; const ASeparator : AnsiChar) : AnsiString;
var
  j, i, l : PtrInt;
begin
  l := Length(AStr);
  i := 1;
  while ( i <= l ) and ( AStr[i] = ASeparator ) do begin
    Inc(i);
  end;
  j := i;
  while ( i <= l ) and ( AStr[i] <> ASeparator ) do begin
    Inc(i);
  end;
  Result := Copy(AStr, j, ( i - j ));
  if ( i = l ) then
    AStr := ''
  else
    Delete(AStr, 1, i);
end;

function IsStrEmpty(const AStr : AnsiString) : Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 )
end;

{$IFDEF HAS_UNICODE}
function IsStrEmpty(const AStr : UnicodeString) : Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 )
end; 

function GetNextToken(var AStr : UnicodeString; const ASeparator : UnicodeChar) : UnicodeString;
var
  j, i, l : PtrInt;
begin
  l := Length(AStr);
  i := 1;
  while ( i <= l ) and ( AStr[i] = ASeparator ) do begin
    Inc(i);
  end;
  j := i;
  while ( i <= l ) and ( AStr[i] <> ASeparator ) do begin
    Inc(i);
  end;
  Result := Copy(AStr, j, ( i - j ));
  if ( i = l ) then
    AStr := ''
  else
    Delete(AStr, 1, i);
end;           

function GetNextToken(var AStr : UnicodeString; const ASeparator : AnsiChar) : UnicodeString;
begin
  Result := GetNextToken(AStr,UnicodeChar(ASeparator));
end;

{$ENDIF}  

function StringToVarBytes(const AValue : string) : TSDOBytes;
var
  c : Integer;
begin
  c := Length(AValue) * SizeOf(Char);
  SetLength(Result,c);
  if ( c > 0 ) then
    Move(AValue[1],Result[Low(Result)],c);
end;

function StreamToVarBytes(const AValue : TStream) : TSDOBytes;
var
  locOldPos : Int64;
begin
  if (AValue = nil) or (AValue.Size < 1) then begin
    Result := nil;
  end else begin
    SetLength(Result,AValue.Size);
    locOldPos := AValue.Position;
    try
      AValue.Position := 0;
      AValue.Read(Result[0],AValue.Size);
    finally
      AValue.Position := locOldPos;
    end;
  end;
end;

function StreamToVarBytes(const AValue : TMemoryStream) : TSDOBytes;
begin
  if (AValue = nil) or (AValue.Size < 1) then begin
    Result := nil
  end else begin
    SetLength(Result,AValue.Size);
    Move(AValue.Memory^,Result[0],AValue.Size);
  end;
end;

function IsBitON(const AData : Byte; const ABit : TBitOrder) : Boolean ;
begin
  Result := ( ( AData and ( 1 shl ABit ) ) <> 0 );
end;

procedure SetBit(var AData : Byte; const ABit : TBitOrder; const AValue : Boolean);
begin
  if AValue then
    AData := AData or (1 shl (ABit mod 8))
  else
    AData := AData and ( not ( 1 shl ( ABit mod 8 ) ) );
end;

{function IntPower(const ABase, AExponent : Byte) : Byte;
var
  i : Integer;
begin
  if ( AExponent = 0 ) or ( ABase = 1 ) then begin
    Result := 1
  end else begin
    if ( ABase = 0 ) then begin
      Result := 0;
    end else begin
      Result := 1;
      for i := 0 to Pred(AExponent) do begin
        Result := Result * ABase;
      end;
    end;
  end;
end; }

function ExtractLocalName(const AValue : string; const ASeparator : string) : string ;
var
  i : PtrInt;
begin
  i := AnsiPos(ASeparator,AValue);
  if ( i < 0 ) then
    i := 0;
  Inc(i);
  Result := Copy(AValue,i,Length(AValue));
end;

{ TStringInterfaceList }

function TStringInterfaceList.Add(const AString: string): PtrInt;
begin
  Result := Add(AString,nil);
end;

function TStringInterfaceList.Add(const AString: string; const AIntf: IInterface): PtrInt;
begin
  Result := IndexOf(AString);
  if ( Result = -1 ) then
    Result := FList.AddObject(AString,TStringInterfaceItem.Create(AIntf as IInterface))
  else
    TStringInterfaceItem(FList.Objects[Result]).Intf := AIntf as IInterface;
end;

procedure TStringInterfaceList.Clear();
var
  i, c : PtrInt;
  tmp : TObject;
begin
  c := FList.Count;
  if ( c > 0 ) then begin
    FList.BeginUpdate();
    try
      for i := Pred(c) downto 0 do begin
        tmp := FList.Objects[i];
        FList.Delete(i);
        tmp.Free();
      end;
    finally
      FList.EndUpdate();
    end;
  end;
end;

constructor TStringInterfaceList.Create();
begin
  inherited;
  FList := TStringList.Create();
  FList.Sorted := True;
  FList.Duplicates := dupError;
end;

procedure TStringInterfaceList.Delete(const AIndex: PtrInt);
var
  tmp : TObject;
begin
  tmp := FList.Objects[AIndex];
  FList.Delete(AIndex);
  tmp.Free();
end;

destructor TStringInterfaceList.Destroy;
begin
  if ( FList <> nil ) then
    Clear();
  FreeAndNil(FList);
  inherited;
end;

function TStringInterfaceList.GetCount: PtrInt;
begin
  Result := FList.Count;
end;

function TStringInterfaceList.GetIntf(const AIndex: PtrInt): IInterface;
begin
  Result := TStringInterfaceItem(FList[AIndex]).Intf;
end;

function TStringInterfaceList.GetString(const AIndex: PtrInt): string;
begin
  Result := FList[AIndex];
end;

function TStringInterfaceList.IndexOf(const AIntf: IInterface): PtrInt;
var
  c, i : PtrInt;
begin
  Result := -1;
  c := Count;
  if ( c > 0 ) then begin
    for i := 0 to Pred(c) do begin
      if ( Intf[i] = AIntf ) then begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function TStringInterfaceList.IndexOf(const AString: string): PtrInt;
begin
  Result := FList.IndexOf(AString);
end;

procedure TStringInterfaceList.SetIntf(const AIndex: PtrInt; const Value: IInterface);
begin
  TStringInterfaceItem(FList.Objects[AIndex]).Intf := Value as IInterface;
end;

procedure TStringInterfaceList.SetString(const AIndex: PtrInt; const Value: string);
begin
  FList.Strings[AIndex] := Value;
end;

{ TStringInterfaceItem }

constructor TStringInterfaceItem.Create(const AIntf: IInterface);
begin
  FIntf := AIntf;
end;

destructor TStringInterfaceItem.Destroy();
begin
  FIntf := nil;
  inherited;
end;

end.
