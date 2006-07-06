{ %VERSION=1.1 }
{ %OPT=-Or }
{ test for full boolean eval and register usage with b+ }

{$b+}

var
  funcscalled: byte;
  ok: boolean;

function function1: boolean;
begin
  writeln('function1 called!');
  inc(funcscalled);
  function1 := false;
end;

function function2: boolean;
begin
  writeln('function2 called!');
  inc(funcscalled);
  function2 := false;
end;

function function3: boolean;
begin
  writeln('function3 called!');
  inc(funcscalled);
  function3 := false;
end;

function function4: boolean;
begin
  writeln('function4 called!');
  inc(funcscalled);
  function4 := false;
end;

function test2: boolean;
var j, k, l, m: longint;
begin
  test2 := true;
  m := 0;
{ get as much regvars occupied as possible }
  for j := 1 to 100 do
    for k := 1 to 100 do
      for l := k downto 0 do
         inc(m,j - k + l);
  if (j = 5) and (k = 0) and (l = 100) and function1 then
    begin
      test2 := false;
      writeln('bug');
    end;
end;

begin
  ok := true;
  funcscalled := 0;
  if function1 and function2 and function3 and function4 then
    begin
      writeln('bug!');
    end;
  ok := funcscalled = 4;
  if ok then
    writeln('all functions called!')
  else
    writeln('not all functions called');
  ok := test2 and (funcscalled = 5);
  if ok then
    writeln('test2 passed')
  else writeln('test2 not passed');
  if not ok then
    begin
      writeln('full boolean evaluation is not working!');
      halt(1);
    end;
end.
