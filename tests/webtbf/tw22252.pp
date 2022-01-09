{ %FAIL }

unit tw22252;

{$mode objfpc}{$H+}

interface

{uses
  Classes, SysUtils;}

type
  TFoo = class
  strict private
    class var FProp: Integer;
  end;

implementation

procedure Test;
var
  f: TFoo;
begin
  f := TFoo.Create;
  f.FProp := 10;
  f.Free;
end;

end.
