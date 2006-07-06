{ Old file: tbs0242b.pp }
{  }


const
  test = 5;

  procedure test_const(const s : string;const x);
    begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      writeln(s,' is ',longint(unaligned(x)));
{$else}
      writeln(s,' is ',longint(x));
{$endif}
    end;

  procedure change(var x);
    begin
      inc(longint(x));
    end;
  const i : longint = 12;
  var
     j : longint;
begin
  j:=34;
  test_const('Const 5',5);
  test_const('Untyped const test',test);
  test_const('Typed_const i',i);
  test_const('Var j',j);
  {test_const('i<>j ',i<>j);}
  change(i);
  change(j);
  { change(test);
  change(longint); }
end.
