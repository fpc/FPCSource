program doecho;

uses sysutils;

var r : integer;

begin
  for r := 1 to 25 do
    writeln ('Line : ', inttostr (r));
end.
  $Log: doecho.pp,v $
  Revision 1.4  2005/02/14 17:13:18  peter
    * truncate log

}
