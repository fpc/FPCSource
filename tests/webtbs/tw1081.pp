uses dos;
var dirinfo:searchrec;

function IntToStr(I: Longint): String;
{ Convert any integer type to a string }
var
  S: string[11];
begin
  Str(I, S);
  IntToStr := S;
end;

procedure write_error(errorstring:string);
var
h,m,s,j,mo,ta,dummy:word;
stri:string;
begin
  gettime(h,m,s,dummy);
  getdate(j,mo,ta,dummy);
  stri:=inttostr(j)+':'+inttostr(mo)+':'+inttostr(ta)+' '+inttostr(h)+':'+inttostr(m)+':'+inttostr(s);
  writeln(stri,' ',errorstring);
end;

procedure readprgfiles;
var i:word;
begin
  FindFirst('*.pp',anyfile, DirInfo);
 while doserror = 0 do
  begin
    inc(i);
    writeln(dirinfo.name);
    write_error(dirinfo.name); {without this function the program works}
    FindNext(DirInfo);
  end;
  write_error('fertig');
end;


BEGIN
readprgfiles;
END.
