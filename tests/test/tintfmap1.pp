{ a method resolution clause takes effect when the interface is named through
  a type alias: IInterface (an alias of IUnknown), an alias of IInterface, and
  an alias of an ordinary interface, the class listing the alias or the
  interface itself, and with an ancestor that already implements IInterface }
program tintfmap1;

{$mode delphi}
{$macro on}
{$ifdef windows}
  {$define iucc:=stdcall}
{$else}
  {$define iucc:=cdecl}
{$endif}

type
  IAlias = IInterface;

  IFoo = interface
    ['{5B3E9D1A-2C4F-4E6B-8A7D-1F0E3C2B4A59}']
    function Bar: longint;
  end;
  IFooAlias = IFoo;

  TCounted = class(TObject)
  protected
    function QueryInterface(constref IID: TGUID; out Obj): HResult; virtual; iucc;
    function _AddRef: longint; virtual; iucc;
    function _Release: longint; virtual; iucc;
    function MappedAddRef: longint; virtual; iucc;
    function MappedRelease: longint; virtual; iucc;
  end;

  TOnInterface = class(TCounted, IInterface)
  protected
    function IInterface._AddRef = MappedAddRef;
    function IInterface._Release = MappedRelease;
  end;

  TOnAlias = class(TCounted, IAlias)
  protected
    function IAlias._AddRef = MappedAddRef;
    function IAlias._Release = MappedRelease;
  end;

  TStaticBase = class(TObject, IInterface)
  protected
    function QueryInterface(constref IID: TGUID; out Obj): HResult; iucc;
    function _AddRef: longint; iucc;
    function _Release: longint; iucc;
  end;

  TOverBase = class(TStaticBase, IInterface)
  protected
    function _AddRef: longint; virtual; iucc;
    function MappedAddRef: longint; virtual; iucc;
    function IInterface._AddRef = MappedAddRef;
  end;

  TFooListed = class(TInterfacedObject, IFooAlias)
    function Bar: longint;
    function Baz: longint;
    function IFooAlias.Bar = Baz;
  end;

  TFooUnlisted = class(TInterfacedObject, IFoo)
    function Bar: longint;
    function Baz: longint;
    function IFooAlias.Bar = Baz;
  end;

var
  named, mapped: longint;

function TCounted.QueryInterface(constref IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID,Obj) then
    result:=0
  else
    result:=HResult($80004002);
end;

function TCounted._AddRef: longint;
begin
  inc(named);
  result:=1;
end;

function TCounted._Release: longint;
begin
  result:=1;
end;

function TCounted.MappedAddRef: longint;
begin
  inc(mapped);
  result:=1;
end;

function TCounted.MappedRelease: longint;
begin
  result:=1;
end;

function TStaticBase.QueryInterface(constref IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID,Obj) then
    result:=0
  else
    result:=HResult($80004002);
end;

function TStaticBase._AddRef: longint;
begin
  result:=1;
end;

function TStaticBase._Release: longint;
begin
  result:=1;
end;

function TOverBase._AddRef: longint;
begin
  inc(named);
  result:=1;
end;

function TOverBase.MappedAddRef: longint;
begin
  inc(mapped);
  result:=1;
end;

function TFooListed.Bar: longint;
begin
  result:=1;
end;

function TFooListed.Baz: longint;
begin
  result:=2;
end;

function TFooUnlisted.Bar: longint;
begin
  result:=1;
end;

function TFooUnlisted.Baz: longint;
begin
  result:=2;
end;

procedure Check(obj: TObject; code: longint);
var
  i: IInterface;
begin
  named:=0;
  mapped:=0;
  if not obj.GetInterface(IInterface,i) then
    halt(code);
  i._AddRef;
  if (mapped<>2) or (named<>0) then
    begin
      writeln('error ',code,': mapped=',mapped,' named=',named);
      halt(code);
    end;
  pointer(i):=nil;
end;

var
  o: TOnInterface;
  i: IInterface;
  f: IFoo;
begin
  Check(TOnInterface.Create,1);
  Check(TOnAlias.Create,2);
  Check(TOverBase.Create,3);
  { the conversion of a class to an interface takes the mapped method too }
  o:=TOnInterface.Create;
  named:=0;
  mapped:=0;
  i:=o;
  if (mapped<>1) or (named<>0) then
    halt(4);
  pointer(i):=nil;
  f:=TFooListed.Create;
  if f.Bar<>2 then
    halt(5);
  f:=TFooUnlisted.Create;
  if f.Bar<>2 then
    halt(6);
  f:=nil;
  writeln('ok');
end.
