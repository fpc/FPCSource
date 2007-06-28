program tb0540;

{$mode objfpc}
{$H-}
{$Q+,R+}

uses sysutils;

var a:string;
    b:string[63];
    c:char;
    w:word;

begin
  a:='';
  b:='';
  w:=257;
  try
    c:=a[w];
    writeln('string[255] failure');
    halt(1);
  except
  end;
  try
    c:=b[w];
    writeln('string[63] failure');
    halt(2);
  except
  end;
end.