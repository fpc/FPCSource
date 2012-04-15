program IntfDelegationCrash;

{$mode objfpc}{$H+}


type

  IGMGetFileName = interface(IUnknown)
    ['{D3ECCB42-A563-4cc4-B375-79931031ECBA}']
    function GetFileName: String; stdcall;
    property FileName: String read GetFileName;
  end;


  IGMGetHandle = interface(IUnknown)
    ['{5BB45961-15A9-11d5-A5E4-00E0987755DD}']
    function GetHandle: THandle; stdcall;
    property Handle: THandle read GetHandle;
  end;


  { TImplementor }

  TImplementor = class(TObject, IGMGetFileName, IGMGetHandle)
   protected
    FController: Tobject;

   public
    constructor Create(const AController: TObject);

    function QueryInterface(constref IID: TGUID; out Intf): HResult; virtual; {$ifdef windows}stdcall{$else}cdecl{$endif};
    function _AddRef: LongInt; virtual; {$ifdef windows}stdcall{$else}cdecl{$endif};
    function _Release: LongInt; virtual; {$ifdef windows}stdcall{$else}cdecl{$endif};

    function GetHandle: THandle; stdcall;
    function GetFileName: String; stdcall;
  end;

  { TDelegator }

  TDelegator = class(TInterfacedObject, IGMGetFileName) // IGMGetHandle
   protected
    FImplementor: TImplementor;
    FGetFileName: IGMGetFileName;

   public
    constructor Create;
    destructor Destroy;

    //
    // This crashes
    //
    property Implementor: TImplementor read FImplementor implements IGMGetFileName;

    //
    // This works
    //
    //property Implementor: IGMGetFileName read FGetFileName implements IGMGetFileName;

    //
    // This is what i really need
    //
    //property Implementor: TImplementor read FImplementor implements IGMGetFileName, IGMGetHandle;
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
  Result := 0;
end;

function TImplementor.GetFileName: String; stdcall;
begin
  Result := '';
end;


{ TDelegator }

constructor TDelegator.Create;
begin
  FImplementor := TImplementor.Create(Self);
  FGetFileName := FImplementor;
end;

destructor TDelegator.Destroy;
begin
  FImplementor.Free;;
end;


var PIUnk: IUnknown; PIGetFileNAme: IGMGetFileName;
begin
  PIUnk := TDelegator.Create;
  PIUnk.QueryInterface(IGMGetFileName, PIGetFileNAme);
end.


