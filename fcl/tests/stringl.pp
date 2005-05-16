program teststrings;

Uses classes,sysutils;

Procedure DoRef (P : Pointer);

Type PLongint = ^Longint;

begin
  If P=Nil then
    Writeln ('(Ref : Empty string)')
  else
{$ifdef fpc}
    Writeln (' (Ref: ',Plongint(Longint(P)-4)^,',Len: ',PLongint(Longint(P)-8)^,')');
{$else}
    Writeln (' (Ref: ',Plongint(Longint(P)-8)^,',Len: ',PLongint(Longint(P)-4)^,')');
{$endif}
end;

Procedure test;

var S,TS : ANsiSTring;
    T : TStringList;
    I,J : Longint;
    A : String[255];

begin
  S:='An Ansi string ';
  T:=TStringList.create;
  Writeln ('Count : ',T.Count,' Capacity : ',T.Capacity);
  For I:=1 to 10 do
    begin
    str (I,TS);
    T.Add(S+TS);
    end;
  Writeln ('Count : ',T.Count,' Capacity : ',T.Capacity);
  J:=T.Count-1;
  Writeln ('J : ',J);
  For I:=0 to J do
    Writeln(I,'/',J,' : ',T.Strings[I]);
  T.SaveToFile ('strings.dat');
  T.Clear;
  T.LoadFromFile('strings.dat');
  J:=T.Count-1;
  Writeln ('Count = ',J);
  For I:=0 to J do
    Writeln(I,'/',J,' : ',T.Strings[I]);
  Writeln ('IndexOf(''An Ansi string 6'') = ',T.IndexOf('An Ansi string 6'));
  Writeln ('IndexOf(''An Ansi string 11'') = ',T.IndexOf('An Ansi string 11'));
  T.Clear;
  For I:=1 to 10 do
    T.Values['Var'+IntToStr(I)]:='Val'+IntToSTr(I);
  J:=T.Count-1;
  Writeln ('J = ',J);
  For I:=0 to J do
    Writeln(I,'/',J,' : ',T.Strings[I]);
  Writeln ('Indexof(''Var6'') = ',T.IndexOfName('Var6'));
  Writeln ('Indexof(''Var13'') = ',T.IndexOfName('Var13'));
  Writeln ('Value[''Var6'']  = ',T.Values['Var6']);
  Writeln ('Value[''Var13'']  = ',T.Values['Var13']);
  Try
    Writeln ('String 100 = ');
    S:=T.Strings[100];
  except
    On e: exception do Writeln ('Caught exception : ',e.message);
  end;
  T.Free;
end;

Var Data : longint;
    t    : THeapStatus;
begin
  Data:=getfpcheapstatus.currheapused;
  test;
  Writeln ('Lost ',getfpcheapstatus.currheapused-data);
end.

{
  $Log: stringl.pp,v $
  Revision 1.6  2005/03/20 12:46:55  marco
   * sb removed legacymem. Fixed with getfpcheapstatus. Now 1.9.9 only

  Revision 1.5  2005/03/16 13:30:17  marco
   * fixed with legacymem (I hope)

  Revision 1.4  2005/02/14 17:13:18  peter
    * truncate log
}
