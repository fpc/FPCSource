{$mode objfpc}
type
  tmyclass = class
    function f : widestring;
    procedure p(var v);
  end;
    
procedure tmyclass.p(var v);  
  begin
  end;
  
function tmyclass.f : widestring;
  begin
    p(pchar(result));
    p(pwidechar(result));
  end;
  
begin
end.