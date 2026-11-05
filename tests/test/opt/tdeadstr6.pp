{ %OPT=-O3 -Oodeadstore }
{ the life information at the condition of a repeat loop (also of while loops
  converted into repeat loops) must contain the variables used by the condition
  and after the loop, also if the body is entered by a goto }
program tdeadstr6;

{$mode objfpc}
{$goto on}

var
  guard : longint;

{ a variable read only after the loop }
function afterloop(n : longint) : longint;
  var
    i, x : longint;
  begin
    i:=0;
    x:=1;
    repeat
      inc(i);
      x:=7;
    until i>=n;
    result:=x;
  end;

{ a flag read only by the condition }
function flag(n : longint) : longint;
  var
    i : longint;
    done : boolean;
  begin
    i:=0;
    done:=false;
    repeat
      inc(i);
      if i>=n then
        begin
          inc(guard);
          done:=true;
        end;
    until done;
    result:=i;
  end;

{ a goto into the body of a while loop }
function gotowhile(n : longint) : longint;
  label
    again;
  var
    i : longint;
    second : boolean;
  begin
    result:=0;
    second:=false;
    i:=1;
    while i<=n do
      begin
      again:
        inc(guard);
        if guard>1000 then
          halt(3);
        inc(result);
        inc(i);
      end;
    if not(second) then
      begin
        second:=true;
        goto again;
      end;
  end;

{ a goto into the body of a repeat loop }
function gotorepeat(n : longint) : longint;
  label
    again;
  var
    i : longint;
    second : boolean;
  begin
    result:=0;
    second:=false;
    i:=1;
    repeat
    again:
      inc(guard);
      if guard>1000 then
        halt(4);
      inc(result);
      inc(i);
    until i>n;
    if not(second) then
      begin
        second:=true;
        goto again;
      end;
  end;

begin
  if afterloop(paramcount+3)<>7 then
    halt(1);
  if flag(paramcount+3)<>3 then
    halt(2);
  guard:=0;
  if gotowhile(paramcount+3)<>4 then
    halt(5);
  guard:=0;
  if gotorepeat(paramcount+3)<>4 then
    halt(6);
  writeln('ok');
end.
