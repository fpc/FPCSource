{$macro on}

{$define aaa:=1234}
{$define bbb:=4321}

{$define ccc:=aaa} // here aaa is already defined macros

var
  err : boolean;
begin
  err:=true;
{$if aaa=ccc} // condition is equal
  // but compiler not compiling this block, because
  // don't take into account that value of macros ccc is macros also.
  err:=false;
  writeln('success');
{$else}
  writeln('failure');
{$endif}
  if err then
   halt(1);
end.
