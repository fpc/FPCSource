unit wasm.storage.objects;

{$mode objfpc}
{$h+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  wasm.storage.shared,
  wasm.storage.api;

Type
  EWasmStorage = class(Exception);

  { TWasmStorage }

  TWasmStorage = class(TObject)
  private
    FUseExceptions: Boolean;
    function CheckRes(aRes: integer; const aOperation: String): boolean;
  protected
    Class function Kind : Integer; virtual; abstract;
    function GetAndReleaseString(aValue: PAnsiChar; aValueLen: Integer): UTF8String;
    function GetKey(aKey : Integer) : UTF8String;
    function GetItem(const aKey : UTF8String) : UTF8String;
    procedure SetItem(const aKey : UTF8String; const aValue : UTF8String);
  Public
    function Count : Integer;
    procedure Remove(const aKey : UTF8String);
    procedure Clear;
    property Items[aKey : UTF8String] : UTF8String read GetItem write SetItem;
    property Keys[aKey : Integer] : UTF8String Read GetKey;
    property UseExceptions : Boolean Read FUseExceptions Write FUseExceptions;
  end;

  TWasmLocalStorage = class(TWasmStorage)
  protected
    class function kind : integer; override;
  end;

  TWasmSessionStorage = class(TWasmStorage)
  protected
    class function kind : integer; override;
  end;

implementation

function TWasmStorage.CheckRes(aRes : integer; const aOperation : String) : boolean;

begin
  Result:=aRes=ESTORAGE_SUCCESS;
  if not Result and UseExceptions then
    Raise EWasmStorage.CreateFmt('Storage error %d for operation "%s"',[aRes,aOperation]);
end;

function TWasmStorage.GetAndReleaseString(aValue : PAnsiChar; aValueLen : Integer) : UTF8String;

begin
  Result:='';
  if (aValue=Nil) or (aValueLen=0) then
    exit;
  SetLength(Result,aValueLen);
  Move(aValue^,Result[1],aValueLen);
  FreeMem(aValue);
end;

function TWasmStorage.GetKey(aKey : Integer) : UTF8String;

var
  lRes: longint;
  lKeyName : PAnsiChar;
  lKeyNameLen : Longint;

begin
  Result:='';
  lRes:=__storage_key(kind,aKey,@lKeyName,@lKeyNameLen);
  if not CheckRes(lRes,'GetKey') then
    exit;
  Result:=GetAndReleaseString(lKeyName,lKeyNameLen);
end;

function TWasmStorage.GetItem(const aKey: UTF8String): UTF8String;

var
  lRes: longint;
  lValue : PAnsiChar;
  lValueLen : Longint;

begin
  Result:='';
  lRes:=__storage_get_item(kind,PAnsiChar(aKey),Length(aKey),@lValue,@lValueLen);
  if not CheckRes(lRes,'GetItem') then
    exit;
  Result:=GetAndReleaseString(lValue,lValueLen);
end;

procedure TWasmStorage.SetItem(const aKey: UTF8String; const aValue: UTF8String);

var
  lRes: longint;
  lValue : PAnsiChar;
  lValueLen : Longint;

begin
  lRes:=__storage_set_item(kind,PAnsiChar(aKey),Length(aKey),PAnsiChar(aValue),Length(aValue));
  CheckRes(lRes,'SetItem');
end;

function TWasmStorage.Count : Integer;

var
  lRes,lCount : Longint;

begin
  lCount:=0;
  lRes:=__storage_length(kind,@lCount);
  if not CheckRes(lRes,'Count') then
    exit;
  Result:=lCount;
end;

procedure TWasmStorage.Remove(const aKey: UTF8String);
var
  lRes : Longint;
begin
  lRes:=__storage_remove_item(kind,PAnsiChar(aKey),Length(aKey));
  CheckRes(lRes,'Remove');
end;

procedure TWasmStorage.Clear;

var
  lRes : Longint;
begin
  lRes:=__storage_clear(kind);
  CheckRes(lRes,'Remove');
end;

class function TWasmLocalStorage.kind : integer;
begin
  Result:=STORAGE_LOCAL;
end;

class function TWasmSessionStorage.kind : integer;
begin
  Result:=STORAGE_SESSION;
end;

end.

