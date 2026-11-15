{ the [Ref] parameter attribute is the system unit's RefAttribute: it can also
  be written with its full class name and with the unit name }
program tconstref8;

{$mode delphi}

type
  TSmall = record
    X: Integer;
  end;

var
  gS: TSmall;
  gOk: Boolean;

procedure P1(const [RefAttribute] a: TSmall);
begin
  gOk := @a = @gS;
end;

procedure P2(const [System.Ref] a: TSmall);
begin
  gOk := @a = @gS;
end;

procedure P3(const [System.RefAttribute] a: TSmall);
begin
  gOk := @a = @gS;
end;

procedure Check(aNr: Integer);
begin
  if not gOk then
    begin
      Writeln('P', aNr, ' does not pass by reference');
      Halt(aNr);
    end;
  gOk := False;
end;

begin
  P1(gS);
  Check(1);
  P2(gS);
  Check(2);
  P3(gS);
  Check(3);
  Writeln('ok');
end.
