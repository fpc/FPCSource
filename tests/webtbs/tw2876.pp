{ Source provided for Free Pascal Bug Report 2876 }
{ Submitted by "marco (gory bugs department)" on  2004-01-04 }
{ e-mail:  }
function strtoppchar(const args:array of ansistring):ppchar;

begin
end;

function execl (filename:ansistring;const args:array of ansistring):integer;

var p:ppchar;

begin
 p:=strtoppchar(args);
end;

procedure myexec (filename:ansistring;const args:array of ansistring);

begin
 execl(filename,args);
end;

begin
  myexec('',['','']);
end.
