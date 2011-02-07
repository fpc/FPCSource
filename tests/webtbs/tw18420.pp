unit tw18420;

{$mode objfpc}{$H+}

interface

type
  generic TGC<T> = class
    Value: T;
    procedure M;
  end;

  TGI = specialize TGC<Integer>;

implementation

procedure TGC.M;
var
  s: String;
begin
  Str(Value,s);
  Val(s,Value);
end;

end.
