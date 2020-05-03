{$inline on}
{$mode objfpc}{$H+}

resourcestring
  rs = 'All files';

function fs: string;inline;
begin
  Result:='*';
end;


function f: String;
begin
  Result:=rs+' ('+fs+')|'+fs;
end;

begin
  writeln(f);
end.
