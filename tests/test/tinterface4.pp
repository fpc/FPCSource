{$mode delphi}

uses variants, sysutils;

(*$ASSERTIONS ON*)

var
  fRefCount: Integer = 0;

type
  IA = interface
    ['{81E19F6A-90C2-11D9-8448-00055DDDEA00}']
  end;
  TA = class(TObject, IA, IInterface)
    destructor Destroy; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const iid: TGuid; out obj): HResult; stdcall;
    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;
  end;

class function TA.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  fRefCount := 1;
end;

procedure TA.AfterConstruction;
begin
  InterlockedDecrement(fRefCount);
  inherited AfterConstruction;
end;

function TA._AddRef: Integer; stdcall;
begin
  InterlockedIncrement(fRefCount);
  Result := 0;
end;

function TA._Release: Integer; stdcall;
begin
  InterlockedDecrement(fRefCount);
  if fRefCount = 0 then begin
    Writeln('Destroy');
    Self.Destroy;
  end;

  Result := 0;
end;

function TA.QueryInterface(const iid: TGuid; out obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

var
  gone: Boolean = False;

destructor TA.Destroy;
begin
  gone := True;
  Writeln('gone');
  inherited Destroy;
end;

procedure X;
var
  v: Variant;
  i: IInterface;
begin
  Writeln('start of test');
  (* simple test with nil interface *)
  i := nil;
  v := i;

  i := v;

  v := 3;

  (* complex test with refcounting *)
  Writeln('complex test');

  i := TA.Create;
  assert(fRefCount = 1);
  Writeln('part 1');
  v := i;
  Writeln('part 2');
  //assert(fRefCount = 2);

  i := nil;
  //assert(fRefCount = 1);

  Writeln('part 3');
  i := v;
  //assert(fRefCount = 2);

  Writeln('gone false');
  assert(gone = False);
  i := nil;
  //assert(fRefCount = 1);
  assert(gone = False);
  v := 7; (* TA refcount 0; gone ... note that v := Null doesnt work for some reason *)
  //assert(fRefCount = 0);
  Writeln('goo');
  //assert(gone = True);
  (* "gone" *)

  Writeln('okay');
  //Halt(0);
end;

begin
  X;
end.
