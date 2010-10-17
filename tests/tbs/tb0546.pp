{ test code based on code from ttp://www.geocities.com/svi37/cyber/delphi9/iimplementation.html }
{$ifdef fpc}
{$mode delphi}
{$endif fpc}
uses
  sysutils;

type
   IXInterface = interface(IUnknown)
   ['{713252E5-4636-11D5-B572-00AA00ACFD08}']
      procedure XStaticMethod;
      procedure XVirtualMethod;
   end;

   IYInterface = interface(IUnknown)
   ['{713252E6-4636-11D5-B572-00AA00ACFD08}']
      procedure YMethod;
   end;

   IZInterface = interface(IUnknown)
   ['{713252E4-4636-11D5-B572-00AA00ACFD08}']
   end;


type
   TInnerObject = class(TAggregatedObject,IXInterface,IYInterface)
   public
      procedure XStaticMethod;
      procedure XVirtualMethod; virtual;
      procedure YMethod;
   end;

   TSpecialObject = class(TInnerObject,IXInterface,IYInterface)
   public
      procedure XStaticMethod;
      procedure XVirtualMethod; override;
      procedure YMethod;
   end;

   TFoo = class({!!!! IXInterface, }IYInterface,IZInterface)
   private
      FInnerX: TInnerObject;
   protected
    function QueryInterface(constref IID: TGUID; out Obj): HResult; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function GetX: TInnerObject; virtual;
    function GetY: IYInterface;
   public
      constructor Create;
      destructor  Destroy; override;
//!!!!      property  InnerX: TInnerObject read GetX implements IXInterface;
      property  InnerY: IYInterface  read GetY implements IYInterface;
   end;

   TBar = class(TFoo,{!!!!IXInterface,}IYInterface,IUnknown)
   private
    FX: TSpecialObject;
    FY: IYInterface;
   protected
      function GetX: TInnerObject; override;
   public
      constructor Create;
      destructor  Destroy; override;
      property Y: IYInterface  read FY implements IYInterface;
//!!!!      property  X: TSpecialObject read FX implements IXInterface;
   end;

{ TFoo }

constructor TFoo.Create;
var
   i: IZInterface;
begin
   inherited;
   i := self;
   FInnerX := TInnerObject.Create(i);  //interface inh. to IUnknown
end;

destructor TFoo.Destroy;
begin
  WriteLn('TFoo.Destroy');
  FInnerX.Free;
  inherited;
end;

{ TFoo.IUnknown }

function TFoo._AddRef: Integer;
begin
   result := -1;
end;

function TFoo._Release: Integer;
begin
   result := -1;
end;

function TFoo.QueryInterface(constref IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;function TFoo.GetX: TInnerObject;
begin
   result := FInnerX;
end;

{ TFoo.IUnknown }


function TFoo.GetY: IYInterface;
begin
   result := FInnerX;
end;

{ TBar }

constructor TBar.Create;
begin
   inherited;
   FX := TSpecialObject.Create(Self);  //explicit IUnknown
   FY := FX;
end;

destructor TBar.Destroy;
begin
  WriteLn('TBar.Destroy');
  FY := nil;
  FX.Free;
  inherited;
end;

function TBar.GetX: TInnerObject;
begin
   result := FX;
end;


{ TInnerObject }

procedure TInnerObject.XStaticMethod;
begin
  WriteLn(Format(
  'Calls TInnerObject.XStaticMethod  on a %s',[ClassName]));
end;

procedure TInnerObject.XVirtualMethod;
begin
  WriteLn(Format(
  'Calls TInnerObject.XVirtualMethod on a %s',[ClassName]));
end;


procedure TInnerObject.YMethod;
begin
  WriteLn(Format(
  'Calls TInnerObject.YMethod on a %s',[ClassName]));
end;

{ TSpecialObject }

procedure TSpecialObject.XStaticMethod;
begin
  WriteLn(Format(
  'Calls TSpecialObject.XStaticMethod  on a %s',[ClassName]));
end;

procedure TSpecialObject.XVirtualMethod;
begin
  // inherited;
  WriteLn(Format(
  'Calls TSpecialObject.XVirtualMethod on a %s',[ClassName]));
end;

procedure TSpecialObject.YMethod;
begin
  WriteLn(Format(
  'Calls TSpecialObject.YMethod on a %s',[ClassName]));
end;

procedure TestFoo(AFoo: TFoo);
var
   o: TFoo;
   x: IXInterface;
   y: IYInterface;
   z: IZInterface;
begin

  o := AFoo;

//!!!!  x := o;
//!!!!  x.XStaticMethod;   // if AFoo is TBar TFoo.XStatic hides TBar.XStatic
//!!!!  x.XVirtualMethod;

  y := o;
  y.YMethod;

//!!!!  z := x as IZInterface;
  z := y as IZInterface;

  z := o;
//!!!!  x := z as IXInterface;

end;

procedure TestBar(ABar: TBar);
var
   o: TBar;
   x: IXInterface;
   y: IYInterface;
   z: IZInterface;
begin

  o := ABar;

//!!!!  x := o;
//!!!!  x.XStaticMethod;
//!!!!  x.XVirtualMethod;

  y := o;
  y.YMethod;

//!!!!  z := x as IZInterface;
  z := y as IZInterface;

  z := o;
//!!!!  x := z as IXInterface;

end;

procedure Test;
var
   AFoo: TFoo;
   ABar: TBar;
begin
   AFoo := TFoo.Create;
   ABar := TBar.Create;

   WriteLn('***TestFoo(AFoo)*****************');
   TestFoo(AFoo);

   WriteLn('***TestFoo(ABar)*****************');
   TestFoo(ABar);

   WriteLn('***TestBar(ABar)*****************');
   TestBar(ABar);

   AFoo.Free;
   ABar.Free;
end;


begin
   WriteLn('IntGetter.TInnerObject.InstanceSize: ',TInnerObject.InstanceSize);
   WriteLn('IntGetter.TSpecialObject.InstanceSize: ',TSpecialObject.InstanceSize);
   WriteLn('IntGetter.TFoo.InstanceSize: ',TFoo.InstanceSize);
   WriteLn('IntGetter.TBar.InstanceSize: ',TBar.InstanceSize);
   Test;
end.
