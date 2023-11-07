{ %NORUN }

program tw40504;

{$scopedenums on}

Type
  TSet = set of (a,b);

var
  S : TSet;

begin
  S:=[];
  Include(S,a);
end.

