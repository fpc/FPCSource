program testenv;
{$mode objfpc}
{$h+}
uses fpodbc,Classes;

Var
  I,J : Integer;
  List,Options : TStringList;
  Env : TODBCEnvironment;
  UseDefault : Boolean;

begin
   useDefault:=(ParamCount>0) and (Paramstr(1)='-d');
   If UseDefault then
     Env:=DefaultEnvironment
   else
     Env:=TODBCEnvironment.Create(Nil);
  try
    Writeln('Handle is : ',Env.Handle);
    List:=TStringlist.Create;
    Options:=TStringList.Create;
    Writeln('List of drivers :');
    Env.GetDriverNames(List);
    Writeln('Count : ',List.Count);
    For I:=0 to List.Count-1 do
       Writeln(i:2,' : ',List[i]);
    Writeln('List of driver options :');
    For I:=0 to List.Count-1 do
      begin
      Env.GetDriverOptions(List[i],Options);
      Writeln('Options for driver ',List[i],' : ');
      For J:=0 to Options.Count-1 do
        Writeln('  ',Options[j]);
      end;
    Env.GetDataSourceNames(List,dtBoth,True);
    Writeln('List of datasource names : ');
    For I:=0 to List.Count-1 do
      writeln(i,' : ',List[i]);
    List.free;
    options.Free;
  finally
    If not UseDefault then
      env.free;
  end;
end.
