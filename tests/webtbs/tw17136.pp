{ %opt=-vw -Sew }

{$mode objfpc}

type
  TA = class
  public
    procedure A(X: boolean = false); virtual; abstract;
  end;

  TB = class(TA)
  public
    procedure A(X: boolean = true); override;
  end;

procedure TB.A(X: boolean = true);
begin
  writeln('hi');
end;

var
  B: TB;
begin
  B := TB.Create;
  B.A;
  B.Free;
end.
