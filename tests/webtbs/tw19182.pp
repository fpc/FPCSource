program MultiIntfDelegation;
{$mode objfpc}{$h+}

type
  IGMGetHandle = interface(IUnknown)
    ['{5BB45961-15A9-11d5-A5E4-00E0987755DD}']
    function GetHandle: THandle; stdcall;
    property Handle: THandle read GetHandle;
  end;

  IGMGetFileName = interface(IUnknown)
    ['{D3ECCB42-A563-4cc4-B375-79931031ECBA}']
    function GetFileName: String; stdcall;
    property FileName: String read GetFileName;
  end;

  IGMGetSetFileName = Interface(IGMGetFileName)
    ['{ECFB879F-86F6-41a3-A685-0C899A2B5BCA}']
    procedure SetFileName(const Value: String); stdcall;
    property FileName: String read GetFileName write SetFileName;
  end;


  { TImplementor }

  TImplementor = class(TObject, IGMGetHandle, IGMGetFileName, IGMGetSetFileName)
   protected
    FController: Tobject;

   public
    constructor Create(const AController: TObject);

    function QueryInterface(constref IID: TGUID; out Intf): HResult; virtual; {$ifdef windows}stdcall{$else}cdecl{$endif};
    function _AddRef: LongInt; virtual; {$ifdef windows}stdcall{$else}cdecl{$endif};
    function _Release: LongInt; virtual; {$ifdef windows}stdcall{$else}cdecl{$endif};

    function GetHandle: THandle; stdcall;
    function GetFileName: String; stdcall;
    procedure SetFileName(const Value: String); stdcall;
  end;


  { TIntfDelegator }

  TIntfDelegator = class(TInterfacedObject, IGMGetFileName, IGMGetSetFileName)
   protected
    FImplementor: TImplementor;
    FGetSetFileName: IGMGetSetFileName;

   public
    constructor Create;
    destructor Destroy; override;

    //
    // This would be nice. NOTE: IGMGetFileName is derived from IGMGetSetFileName!
    //
    property Implementor: IGMGetSetFileName read FGetSetFileName implements IGMGetFileName, IGMGetSetFileName;
  end;


  { TObjDelegator }

  TObjDelegator = class(TInterfacedObject, IGMGetHandle, IGMGetFileName, IGMGetSetFileName)
   protected
    FImplementor: TImplementor;

   public
    constructor Create;
    destructor Destroy; override;

    //
    // This would be really smart!
    //
    property Implementor: TImplementor read FImplementor implements IGMGetHandle, IGMGetFileName, IGMGetSetFileName;
  end;



{ TImplementor }

constructor TImplementor.Create(const AController: TObject);
begin
  FController := AController;
end;

function TImplementor.QueryInterface(constref IID: TGUID; out Intf): HResult; {$ifdef windows}stdcall{$else}cdecl{$endif};
var PIUnkController: IUnknown;
begin
  if GetInterface(IID, Intf) then Result := S_OK else
   if (FController <> nil) and FController.GetInterface(IUnknown, PIUnkController) then
    Result := PIUnkController.QueryInterface(IID, Intf) else Result := E_NOINTERFACE;
end;

function TImplementor._AddRef: LongInt; {$ifdef windows}stdcall{$else}cdecl{$endif};
var PIUnkController: IUnknown;
begin
  if (FController <> nil) and FController.GetInterface(IUnknown, PIUnkController) then
   Result := PIUnkController._AddRef
end;

function TImplementor._Release: LongInt; {$ifdef windows}stdcall{$else}cdecl{$endif};
var PIUnkController: IUnknown;
begin
  if (FController <> nil) and FController.GetInterface(IUnknown, PIUnkController) then
   Result := PIUnkController._Release
end;

function TImplementor.GetHandle: THandle; stdcall;
begin
  writeln('TImplementor.GetHandle');
  Result := 0;
end;

function TImplementor.GetFileName: String; stdcall;
begin
  writeln('TImplementor.GetFileName');
  Result := '';
end;

procedure TImplementor.SetFileName(const Value: String); stdcall;
begin
  writeln('TImplementor.SetFileName');
end;


{ TIntfDelegator }

constructor TIntfDelegator.Create;
begin
  FImplementor := TImplementor.Create(Self);
  FGetSetFileName := FImplementor;
end;

destructor TIntfDelegator.Destroy;
begin
  FImplementor.Free;
  inherited Destroy;
end;


{ TObjDelegator }

constructor TObjDelegator.Create;
begin
  FImplementor := TImplementor.Create(Self);
end;

destructor TObjDelegator.Destroy;
begin
  FImplementor.Free;
  inherited Destroy;
end;


var
  PIUnk: IUnknown;
  PIGetFileNAme: IGMGetFileName;
  PIGetSetFileName: IGMGetSetFileName;
  obj: TObjDelegator;
begin
  PIUnk := TIntfDelegator.Create;
  PIUnk.QueryInterface(IGMGetFileName, PIGetFileName);
  PIGetFileName.GetFileName;
  PIUnk.QueryInterface(IGMGetSetFileName, PIGetSetFileName);
  PIGetSetFileName.SetFileName('');
  
  obj := TObjDelegator.Create;
  (obj as IGMGetFileName).GetFileName;
  (obj as IGMGetSetFileName).SetFileName('');
  (obj as IGMGetHandle).GetHandle;
end.


