program chatserver;

{$mode objfpc}

procedure Sendln(MsgType: Longint; Str: PChar);
begin
  halt(1);
end;

procedure Sendln(MsgType: Longint; Str: array of PChar);
begin
  halt(0);
end;


procedure Sendln(MsgType: Longint; Str: array of char);
begin
  halt(1);
end;



begin
  Sendln(1, ['str1', 'str2'])
end.
