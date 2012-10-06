program loadlibdemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils, Classes, sqldb,sqldblib,
  pqconnection,
  ibconnection,
  mysql55conn,
  mysql51conn,
  mysql50conn,
  mysql41conn,
  mysql40conn;

Procedure List;

Var
  S : TStringList;
  I : Integer;

begin
  S:=TStringList.Create;
  try
    getConnectionList(S);
    Writeln('Available connection types:');
    For I:=0 to S.Count-1 do
      Writeln(S[i],', Default library name: ',GetConnectionDef(S[i]).DefaultLibraryName);
  finally
    S.free;
  end;
end;

Procedure LoadLib(CT,LN : String);

Var
  D : String;

begin
  With TSQLDBLibraryLoader.Create(Nil) do
    try
      ConnectionType:=CT;
      D:=LibraryName;
      if (LN<>'') then
        LibraryName:=LN;
      Writeln('Loading library for connector',ct,' (default: ',D,', actual:', LibraryName,')');
      try
        LoadLibrary;
      except
        On E : Exception do
          begin
          Writeln('Error loading library : ',E.Message);
          Exit;
          end;
      end;
      Writeln('UnLoading library for connector',ct,' (default: ',D,', actual:', LibraryName,')');
      try
        UnLoadLibrary;
      except
        On E : Exception do
          Writeln('Error unloading library : ',E.Message);
      end;
    finally
      Free;
    end;
end;

begin
  if (ParamCount<1) or (paramcount>2) then
    begin
    Writeln('Usage : ');
    Writeln('loadlibdemo list');
    Writeln('  - lists all connection types');
    Writeln('loadlibdemo conntype');
    Writeln('  - Load default library for given connection type');
    Writeln('loadlibdemo conntype libname');
    Writeln('  - Load alternative library for given connection type');
    end
  else if (ParamStr(1)='list') then
    List
  else
    LoadLib(Paramstr(1),ParamStr(2));
end.

