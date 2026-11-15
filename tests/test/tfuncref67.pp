{ override methods converted to function references: overrides of virtual, abstract,
  dynamic and class virtual methods, a reintroduced method, in this program and in
  another unit, and a call through a variable of the base class (issue 42001) }
program tfuncref67;

{$mode delphi}

uses
  ufuncref67;

type
  TProc = reference to procedure;

  TLBase = class
    procedure V; virtual;
    procedure A; virtual; abstract;
    procedure D; dynamic;
    procedure R; virtual;
    class procedure C; virtual;
  end;

  TLChild = class(TLBase)
    procedure V; override;
    procedure A; override;
    procedure D; override;
    procedure R; reintroduce;
    class procedure C; override;
  end;

  TLGrandChild = class(TUChild)
    procedure V; override;
  end;

var
  gFailed: LongInt = 0;

procedure TLBase.V;
begin
  gLog := gLog + 'LBase.V';
end;

procedure TLBase.D;
begin
  gLog := gLog + 'LBase.D';
end;

procedure TLBase.R;
begin
  gLog := gLog + 'LBase.R';
end;

class procedure TLBase.C;
begin
  gLog := gLog + 'LBase.C';
end;

procedure TLChild.V;
begin
  gLog := gLog + 'LChild.V';
end;

procedure TLChild.A;
begin
  gLog := gLog + 'LChild.A';
end;

procedure TLChild.D;
begin
  gLog := gLog + 'LChild.D';
end;

procedure TLChild.R;
begin
  gLog := gLog + 'LChild.R';
end;

class procedure TLChild.C;
begin
  gLog := gLog + 'LChild.C';
end;

procedure TLGrandChild.V;
begin
  gLog := gLog + 'LGrandChild.V';
end;

procedure Check(const aRow: String; aRef: TProc; const aWant: String);
begin
  gLog := '';
  aRef();
  if gLog <> aWant then
    begin
      Writeln(aRow, ': got "', gLog, '" expected "', aWant, '"');
      Inc(gFailed);
    end;
end;

var
  lLC: TLChild;
  lLB: TLBase;
  lUC: TUChild;
  lUB: TUBase;
  lGC: TLGrandChild;
begin
  lLC := TLChild.Create;
  lLB := lLC;
  lUC := TUChild.Create;
  lUB := lUC;
  lGC := TLGrandChild.Create;

  Check('override', lLC.V, 'LChild.V');
  Check('override of an abstract method', lLC.A, 'LChild.A');
  Check('dynamic override', lLC.D, 'LChild.D');
  Check('reintroduced method', lLC.R, 'LChild.R');
  Check('class override through an instance', lLC.C, 'LChild.C');
  Check('class override through the class', TLChild.C, 'LChild.C');
  Check('virtual method through a base class variable', lLB.V, 'LChild.V');

  Check('override in another unit', lUC.V, 'UChild.V');
  Check('override of an abstract method in another unit', lUC.A, 'UChild.A');
  Check('dynamic override in another unit', lUC.D, 'UChild.D');
  Check('class override in another unit through an instance', lUC.C, 'UChild.C');
  Check('class override in another unit through the class', TUChild.C, 'UChild.C');
  Check('virtual method in another unit through a base class variable', lUB.V, 'UChild.V');
  Check('override of a method overridden in another unit', lGC.V, 'LGrandChild.V');

  lGC.Free;
  lUC.Free;
  lLC.Free;
  if gFailed <> 0 then
    Halt(1);
  Writeln('ok');
end.
