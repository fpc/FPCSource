program testlog;

uses linux,systemlog,sysutils;

procedure dotest;

var i : longint;
    prefix : ansistring;

begin
  i:=getpid;
  prefix:=format('testlog[%d] ',[i]);
  // prefix will be prepended to every message now.
  openlog(pchar(prefix),LOG_NOWAIT,LOG_DEBUG);
  for i:=1 to 10 do
    syslog(log_info,'This is message number %d'#10,[i]);
  prefix:='';
end;

begin
  dotest;
end.
{
  $Log$
  Revision 1.3  2003-01-05 20:26:35  michael
  + fixed log

  Revision 1.2  2002/09/07 15:43:06  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:22  peter
    * splitted to base and extra

}
