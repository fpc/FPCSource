{ %norun }
{ %opt=-Sewn -vwn -O- }

{
  Test for correct emitting of warnings/hints for uninitialized variables of management types
  See also tbf/tb0258.pp
}

// Only hints about uninitialized managed variables must be issued for this code

{$mode objfpc}

type
  TLongArray = array of longint;

procedure p;
var
  a : TLongArray;
  s: ansistring;
begin
  setlength(a,100);  // hint for local var
  setlength(s,100);  // hint for local var
  a[1]:=1;
  writeln(a[1]);
  s[1]:='a';
  writeln(s[1]);
end;

procedure svar(var s: ansistring; len: longint);
begin
  setlength(s,len);
end;

procedure avar(var a: TLongArray; len: longint);
begin
  setlength(a,len);
end;

procedure p2;
var
  a : TLongArray;
  s: ansistring;
begin
  avar(a,100);  // hint for local var
  svar(s,100);  // hint for local var
  a[1]:=1;
  writeln(a[1]);
  s[1]:='a';
  writeln(s[1]);
end;

function f2: ansistring;
begin
  // Hint for the ansistring Result, since all contents of the Result
  // after calling SetLength() is expected to be undefined.
  setlength(Result,1);
  Result[1]:='a';
end;

begin
  p;
  p2;
  f2;
end.
