{$mode delphi}

Program test;

procedure outer_proc(var outer: ShortString);

procedure inner_proc;
Begin
  outer := 'abc';
End;

Begin
  inner_proc();
End;

var
 s: shortstring;

Begin
  outer_proc(s);
  if s<>'abc' then
    halt(1);
End.
