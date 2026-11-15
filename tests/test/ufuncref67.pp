{ classes for tfuncref67: overrides of virtual, abstract, dynamic and class virtual methods in another unit }
unit ufuncref67;

{$mode delphi}

interface

type
  TUBase = class
    procedure V; virtual;
    procedure A; virtual; abstract;
    procedure D; dynamic;
    class procedure C; virtual;
  end;

  TUChild = class(TUBase)
    procedure V; override;
    procedure A; override;
    procedure D; override;
    class procedure C; override;
  end;

var
  gLog: String;

implementation

procedure TUBase.V;
begin
  gLog := gLog + 'UBase.V';
end;

procedure TUBase.D;
begin
  gLog := gLog + 'UBase.D';
end;

class procedure TUBase.C;
begin
  gLog := gLog + 'UBase.C';
end;

procedure TUChild.V;
begin
  gLog := gLog + 'UChild.V';
end;

procedure TUChild.A;
begin
  gLog := gLog + 'UChild.A';
end;

procedure TUChild.D;
begin
  gLog := gLog + 'UChild.D';
end;

class procedure TUChild.C;
begin
  gLog := gLog + 'UChild.C';
end;

end.
