{ %fail }

{$ifdef fpc}{$mode objfpc}{$endif}
type
  tmyclass = class
    function f2 : ansistring;
    function f : widestring;
    procedure p(var v);
  end;

procedure tmyclass.p(var v);
  begin
  end;

 function tmyclass.f2 : ansistring;
  begin
    p(pchar(result));
  end;

function tmyclass.f : widestring;
  begin
    p(pwidechar(result));
  end;

begin
end.
