{$mode objfpc}
{$H+}

program testcgi;

uses cgiapp,classes,sysutils;

Type
  TTestCGI = Class(TCGIApplication)
    Procedure DoRun; override;
  end;

Procedure TTestCGI.DoRun;

Var
  L : TStrings;
  I: Integer;

begin
  ContentType:='text/html';
  EmitContentType;
  L:=TStringList.Create;
  Writeln('<HTML><TITLE>',title,'</TITLE><BODY>');
  Try
    Writeln('<H1>List of CGI variables:</H1>');
    GetCGIVarList(L);
    For I:=0 to L.Count-1 do
      Writeln(L[i],'<BR/>');
    Writeln('<H1>List of environment variables:</H1>');
    GetEnvironmentList(L);
    For I:=0 to L.Count-1 do
      Writeln(L[i],'<BR/>');
    If (RequestVariableCount>0) then
      begin
      Writeln('<H1>List of form variables:</H1>');
      GetRequestVarList(L);
      For I:=0 to L.Count-1 do
        Writeln(L[i],'<BR/>');
      Writeln('<H1>List of form variables, tabular format:</H1>');
      Writeln('<table width="100%" border="1">');
      Writeln('<TR><TH>Name</TH><TH>Value</TH></TR>');
      GetRequestVarList(L,True);
      For I:=0 to L.Count-1 do
        Writeln('<TR><TD>',L[i],'</TD><TD>',RequestVariables[L[i]],'</TD></TR>');
      end;
  Finally
    Writeln('</BODY></HTML>');
    Terminate;
  end;
end;

begin
  With TTestCGI.Create(Nil) do
    Try
      Title:='Test CGI application';
      Initialize;
      Run;
    Finally
      Free;
    end;
end.
