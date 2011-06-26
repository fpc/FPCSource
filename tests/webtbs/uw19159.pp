Unit uw19159;

{$MODE DELPHI}

interface

type

  IGMStringStorage = interface(IUnknown)
    ['{6C1E6792-ED8D-4c16-A49E-12CB62F61E7E}']
    function ReadString(const ValueName: String; const DefaultValue: String = ''): String; stdcall;
    procedure WriteString(const ValueName, Value: String); stdcall;
  end;

  TGMStorageBase = class(TObject, IGMStringStorage)
   protected
    FRefCount: LongInt;

   public
    function QueryInterface(constref IID: TGUID; out Intf): HResult; virtual; stdcall;
    function _AddRef: LongInt; virtual; stdcall;
    function _Release: LongInt; virtual; stdcall;

    function ReadString(const ValueName: String; const DefaultValue: String { = '' }): String; virtual; stdcall; abstract;
    procedure WriteString(const ValueName, Value: String); virtual; stdcall; abstract;
  end;


  TGMIniFileStorage = class(TGMStorageBase)
   public
    //
    // Error: There is no method in an ancestor class to be overridden: "TGMIniFileStorage.ReadString(const AnsiString,const AnsiString):AnsiString;"
    //
    // function ReadString(const ValueName: String; const DefaultValue: String = ''): String; override;

    //
    // Repeating the stdcall directive and it gets compiled!
    //
    function ReadString(const ValueName: String; const DefaultValue: String = '' ): String; override;

    //
    // But why does this method work without repeating the stdcall directive?
    //
    procedure WriteString(const ValueName, Value: String); override;
  end;


implementation


{ ------------------------ }
{ ---- TGMStorageBase ---- }
{ ------------------------ }

function TGMStorageBase.QueryInterface(constref IID: TGUID; out Intf): HResult;
begin
  if GetInterface(IID, Intf) then Result := S_OK else Result := E_NOINTERFACE;
end;

function TGMStorageBase._AddRef: LongInt;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TGMStorageBase._Release: LongInt;
begin
  Result := InterlockedDecrement(FRefCount);
  //if (Result = 0) and RefLifeTime then OnFinalRelease;
end;


{ --------------------------- }
{ ---- TGMIniFileStorage ---- }
{ --------------------------- }

function TGMIniFileStorage.ReadString(const ValueName: String; const DefaultValue: String = ''): String;
begin
  Result := '';
end;

procedure TGMIniFileStorage.WriteString(const ValueName, Value: String);
begin
end;


end.
