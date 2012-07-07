program trange3;

{$mode objfpc}

{$ifdef cpujvm}
uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define writeln:=jlsystem.fout.println}
{$define write:=jlsystem.fout.println}

{$else}
uses
  SysUtils;
{$endif}


{$r+}

var
  a1: array[-5..6] of byte;
  a2: array[-12..-1] of byte;
  a3: array[0..6] of byte;
  a4: array[1..12] of byte;

  c: cardinal;
  l: longint;
  b: byte;
  finalerror: boolean;

function check_longint(l: longint; res1, res2, res3, res4: boolean): boolean;
var
  caught,
  error: boolean;
begin
  result := false;

  caught := false;
  try
    b := a1[l];
  except
    caught := true;
  end;
  error := caught <> res1;
  if error then writeln('long 1 failed for '+unicodestring(JLInteger.valueOf(l).toString));
  result := result or error;

  caught := false;
  try
    b := a2[l];
  except
    caught := true;
  end;
  error := caught <> res2;
  if error then writeln('long 2 failed for '+unicodestring(JLInteger.valueOf(l).toString));
  result := result or error;

  caught := false;
  try
    b := a3[l];
  except
    caught := true;
  end;
  error := caught <> res3;
  if error then writeln('long 3 failed for '+unicodestring(JLInteger.valueOf(l).toString));
  result := result or error;

  caught := false;
  try
    b := a4[l];
  except
    caught := true;
  end;
  error := caught <> res4;
  if error then writeln('long 4 failed for '+unicodestring(JLInteger.valueOf(l).toString));
  result := result or error;
  writeln;
end;

function check_cardinal(l: cardinal; res1, res2, res3, res4: boolean): boolean;
var
  caught,
  error: boolean;
begin
  result := false;

  caught := false;
  try
    b := a1[l];
  except
    caught := true;
  end;
  error := caught <> res1;
  if error then writeln('card 1 failed for '+unicodestring(JLLong.valueOf(l).toString));
  result := result or error;

  caught := false;
  try
    b := a2[l];
  except
    caught := true;
  end;
  error := caught <> res2;
  if error then writeln('card 2 failed for '+unicodestring(JLLong.valueOf(l).toString));
  result := result or error;

  caught := false;
  try
    b := a3[l];
  except
    caught := true;
  end;
  error := caught <> res3;
  if error then writeln('card 3 failed for '+unicodestring(JLLong.valueOf(l).toString));
  result := result or error;

  caught := false;
  try
    b := a4[l];
  except
    caught := true;
  end;
  error := caught <> res4;
  if error then writeln('card 4 failed for '+unicodestring(JLLong.valueOf(l).toString));
  result := result or error;
  writeln;
end;


begin
  finalerror :=
    check_longint(-1,false,false,true,true);
  finalerror :=
    check_longint(-6,true,false,true,true) or finalerror;
  finalerror :=
    check_longint(0,false,true,false,true) or finalerror;
  finalerror :=
    check_cardinal(0,false,true,false,true);
  finalerror :=
    check_cardinal(cardinal($ffffffff),true,true,true,true) or finalerror;
  finalerror :=
    check_cardinal(5,false,true,false,false) or finalerror;
  if finalerror then
    begin
      writeln('Still errors in range checking for array indexes');
      halt(1);
    end;
end.
