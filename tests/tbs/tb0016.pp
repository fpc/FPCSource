{ Old file: tbs0019.pp }
{  }

type
   b = ^x;

   x = byte;

var
   pb : b;

begin
   new(pb);
   pb^:=10;
end.
