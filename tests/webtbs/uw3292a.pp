unit uw3292a;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

interface

type
  TBase = class
  protected
    procedure a(var msg); message 1;
    procedure b; virtual;
  end;

  TMiddle = class(TBase)
  private
    procedure a(var msg); message 1;
    procedure b;override;
  end;

var
  acnt,bcnt : longint;

implementation

{ TBase }

procedure TBase.a(var msg);
begin
  writeln('A: In TBase');
  inc(acnt);
end;

procedure TBase.b;
begin
  writeln('B: In TBase');
  inc(bcnt);
end;

{ TMiddle }

procedure TMiddle.a(var msg);
begin
  writeln('A: In TMiddle');
  inc(acnt);
  inherited a(msg);
end;

procedure TMiddle.b;
begin
  writeln('B: In TMiddle');
  inc(bcnt);
  inherited b;
end;

end.
