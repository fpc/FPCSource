program echo;

uses classes,sysutils,cgiapp;

Type

  { TMyCgiApplication }

  TMyCgiApplication = Class(TCGIApplication)
  Public
    procedure DoRun; override;
  end;

{ TMyCgiApplication }

procedure TMyCgiApplication.DoRun;

  procedure AddRow(const aName,avalue : string);

  begin
   AddResponseLn(Format('<tr><td><b>%s</b></td><td>%s</td></tr>',[aName,aValue]));
  end;

Var
  L : TStrings;
  V,N : String;
  I : Integer;

begin
  Terminate;
  EmitContentType;
  AddResponseLn('<html>');
  AddResponseLn('<body>');
  AddResponseLn('<h1>Simple CGI HTML echo demo</h1>');
  AddResponseLn('<h2>Request variables</h2>');
  AddResponseLn('<table border=1>');
  AddRow('Variable','Value');
  L:=TStringList.Create;
  Try
    GetRequestVarList(L);
    For I:=0 to L.Count-1 do
      begin
      L.GetNameValue(I,N,V);
      AddRow(N,V);
      end;
    AddResponseLn('</table>');
    AddResponseLn('<h2>CGI variables</h2>');
    AddResponseLn('<table border=1>');
    AddRow('Variable','Value');
    L.Clear;
    GetCGIVarList(L);
    For I:=0 to L.Count-1 do
      begin
      L.GetNameValue(I,N,V);
      AddRow(N,V);
      end;
    AddResponseLn('</table>');
    AddResponseLn('</body>');
    AddResponseLn('</html>');
  finally
    L.Free;
  end;
end;

begin
  With TMyCgiApplication.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free
    end;
end.

