{ %FAIL }
{ "type of PT^" in a generic: specializing PT with a non-pointer type fails }
program ttypeinquiry20;

{$mode objfpc}

type
  generic TGiraffe<PT> = class
  public
    type
      T = type of PT^;
  end;

var
  Giraffe: specialize TGiraffe<integer>;
begin
  Giraffe:=nil;
end.
