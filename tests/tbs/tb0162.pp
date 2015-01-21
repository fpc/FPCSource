{ Old file: tbs0193.pp }
{ overflow checking for 8 and 16 bit operations wrong }

{$mode objfpc}

uses sysutils;

var
  has_errors: boolean;

procedure doerror(l: longint);
begin
  writeln('error near ',l);
  has_errors:=true;
end;

{$R-}
{$Q+}
var i: integer;
    b: byte;
    l: longint;
    c: cardinal;
    n: int64;
    q: qword;
begin
  has_errors:=false;
  i := 32767;
  i := i + 15;
  b := 255;
  b := b + 18;
  b := 255;
  b := b * 8;
  b := 255;
  b := b * 17;

{ 64 bit cpus do all calculations in 64 bit so longint and cardinal can't overflow }
{$ifndef CPU64}
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

{$endif CPU64}

{$ifdef fpc}

  n := high(int64);
  try
    n := n+1;
    doerror(17);
  except
    on eintoverflow do
      ;
    else
      doerror(18);
  end;

  n := low(int64);
  try
    n := n-1;
    doerror(19);
  except
    on eintoverflow do
      ;
    else
      doerror(20);
  end;

  n := 0;
  try
    n := n-1;
  except
    on eintoverflow do
      doerror(39);
  end;


  n := low(int64);
  try
    n := n*2;
    doerror(21);
  except
    on eintoverflow do
      ;
    else
      doerror(22);
  end;

  n := high(int64) div 2;
  try
    n := n*3;
    doerror(23);
  except
    on eintoverflow do
      ;
    else
      doerror(24);
  end;


  q := 0;
  try
    q := q-1;
    doerror(25);
  except
    on eintoverflow do
      ;
    else
      doerror(26);
  end;


  q := qword(high(qword));
  try
    q := q+1;
    doerror(27);
  except
    on eintoverflow do
      ;
    else
      doerror(28);
  end;

  q := qword(high(qword)) div qword(2);
  try
    q := q*qword(3);
    doerror(29);
  except
    on eintoverflow do
      ;
    else
      doerror(30);
  end;

  q := high(qword);
  try
    q := q*high(qword);
    doerror(31);
  except
    on eintoverflow do
      ;
    else
      doerror(32);
  end;

{$endif fpc}
  if has_errors then
    halt(1);
End.
