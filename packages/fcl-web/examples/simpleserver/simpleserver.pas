program simpleserver;

uses sysutils,custhttpapp, fpwebfile;

Type

  { THTTPApplication }

  THTTPApplication = Class(TCustomHTTPApplication)
  private
    FQuiet: Boolean;
    procedure Usage(Msg: String);
  published
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    Procedure DoRun; override;
    property Quiet : Boolean read FQuiet Write FQuiet;
  end;

Var
  Application : THTTPApplication;

{ THTTPApplication }

procedure THTTPApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  if Quiet then
    exit;
  if IsConsole then
    Writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' [',EventType,'] ',Msg)
  else
    inherited DoLog(EventType, Msg);
end;

procedure THTTPApplication.Usage(Msg : String);

begin
  if (Msg<>'') then
    Writeln('Error: ',Msg);
  Writeln('Usage ',ExtractFileName(ParamStr(0)),' [options] ');
  Writeln('Where options is one or more of : ');
  Writeln('-d --directory=dir  Base directory from which to serve files.');
  Writeln('                    Default is current working directory: ',GetCurrentDir);
  Writeln('-h --help           This help text');
  Writeln('-i --indexpage=name Directory index page to use (default: index.html)');
  Writeln('-n --noindexpage    Do not allow index page.');
  Writeln('-p --port=NNNN      TCP/IP port to listen on (default is 3000)');
  Writeln('-q --quiet          Do not write diagnostic messages');
  Halt(Ord(Msg<>''));
end;

procedure THTTPApplication.DoRun;

Var
  S,IndexPage,D : String;

begin
  S:=Checkoptions('hqd:ni:p:',['help','quiet','noindexpage','directory:','port:','indexpage:']);
  if (S<>'') or HasOption('h','help') then
    usage(S);
  Quiet:=HasOption('q','quiet');
  Port:=StrToIntDef(GetOptionValue('p','port'),3000);
  D:=GetOptionValue('d','directory');
  if D='' then
    D:=GetCurrentDir;
  Log(etInfo,'Listening on port %d, serving files from directory: %s',[Port,D]);
{$ifdef unix}
  MimeTypesFile:='/etc/mime.types';
{$endif}
  TSimpleFileModule.BaseDir:=IncludeTrailingPathDelimiter(D);
  TSimpleFileModule.OnLog:=@Log;
  If not HasOption('n','noindexpage') then
    begin
    IndexPage:=GetOptionValue('i','indexpage');
    if IndexPage='' then
      IndexPage:='index.html';
    Log(etInfo,'Using index page %s',[IndexPage]);
    TSimpleFileModule.IndexPageName:=IndexPage;
    end;
  inherited;
end;

begin
  TSimpleFileModule.RegisterDefaultRoute;
  Application:=THTTPApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

