program tw17193;

{$mode objfpc}{$H+}

type
  generic G1<T> = class
  public
    value : T;
  end;

  generic G2<T> = class
  public type
    S1 = specialize G1<T>;
    S2 = specialize G1<T>;
  public
    procedure P;
  end;
  
  S = specialize G2<Integer>;

procedure G2.P;
begin
end;

var
  x1 : S.S1;
  x2 : S.S2;
begin
  x1 := S.S1.Create;
  x2 := S.S2.Create;
  x1.value := 111;
  x2.value := x1.value;
  if x2.value <> 111 then
    Halt(1);
end.
