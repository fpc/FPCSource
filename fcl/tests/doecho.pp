program doecho;

uses sysutils;

var r : integer;

begin
  for r := 1 to 25 do
    writeln ('Line : ', inttostr (r));
end.
  $Log$
  Revision 1.2  2000-07-13 11:33:04  michael
  + removed logs
 
}
