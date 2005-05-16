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
  $Log: testlog.pp,v $
  Revision 1.4  2005/02/14 17:13:21  peter
    * truncate log

}
