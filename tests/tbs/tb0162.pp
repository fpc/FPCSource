{ Old file: tbs0193.pp }
{ overflow checking for 8 and 16 bit operations wrong }

{$mode objfpc}

uses sysutils;

procedure doerror(l: longint);
begin
  writeln('error near ',l);
  halt(1);
end;

{$R-}
{$Q+}
var i: integer;
    b: byte;
    l: longint;
    c: cardinal;
begin
  i := 32767;
  i := i + 15;
  b := 255;
  b := b + 18;
  b := 255;
  b := b * 8;
  b := 255;
  b := b * 17;

  l := high(longint);
  try
    l := l+1;
    doerror(1);
  except
    on eintoverflow do
      ;
    else
      doerror(2);
  end;

  l := low(longint);
  try
    l := l-1;
    doerror(3);
  except
    on eintoverflow do
      ;
    else
      doerror(4);
  end;

  l := low(longint);
  try
    l := l*2;
    doerror(5);
  except
    on eintoverflow do
      ;
    else
      doerror(6);
  end;

  l := high(longint) div 2;
  try
    l := l*3;
    doerror(7);
  except
    on eintoverflow do
      ;
    else
      doerror(8);
  end;


  c := 0;
  try
    c := c-1;
    doerror(9);
  except
    on eintoverflow do
      ;
    else
      doerror(10);
  end;


  c := high(cardinal);
  try
    c := c+1;
    doerror(11);
  except
    on eintoverflow do
      ;
    else
      doerror(12);
  end;

  c := high(cardinal) div 2;
  try
    c := c*3;
    doerror(13);
  except
    on eintoverflow do
      ;
    else
      doerror(14);
  end;

  c := high(cardinal);
  try
    c := c*high(cardinal);
    doerror(15);
  except
    on eintoverflow do
      ;
    else
      doerror(16);
  end;

End.
