{$mode objfpc}
{$modeswitch unicodestrings}

unit outpara;

interface

procedure test(out l: string);
procedure main(args: array of string);

implementation

procedure test(out l: string);
begin
  l:='abc';
end;

procedure main(args: array of string);
var
  x: string;
begin
  test(x);
  if x<>'abc' then
    raise jlexception.Create('wrong')
end;

end.

