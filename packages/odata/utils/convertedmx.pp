program convertedmx;

uses
  classes, sysutils, csdl2pas, custapp, restcodegen,
  edmx2pas, odatacodegen, cgs;

Type

  { TConvertEDMXApplication }

  TConvertEDMXApplication = Class(TCustomApplication)
  private
    procedure Usage(Msg: String);
  Protected
    Procedure DoRun; override;
  public
    Procedure DoMyLog(Sender: TObject; LogType: TCodegenLogType; Const Msg: String);
  end;

{ TConvertEDMXApplication }

procedure TConvertEDMXApplication.Usage(Msg : String);

begin
  If (Msg<>'') then
    Writeln('Error: ',Msg);
  Writeln('Usage : ',ExtractFileName(ParamStr(0)),' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-a --aliases=aliases       Schema aliases as comma-separated name=value pairs.');
  Writeln('                           The form @aliases reads from file "aliases", one alias per line.');
  Writeln('-b --basename=classname    Name of class to use as base class.');
  Writeln('-d --odata=version         OData version to use: v2 or v4.');
  Writeln('-e --extraunits=extraunits Comma-separated list of unit names to add.');
  Writeln('-h --help                  This message.');
  Writeln('-i --input=filename        Name of the file to use as input. Mandatory');
  Writeln('-o --output=filename       Name of the file to use as output.');
  Writeln('                           (default: input file with extension changed to .pas)');
  Writeln('-p --prefix=fieldprefix    Text to use as field prefix (default: F)');
  Writeln('-u --enumerations=mode     How to treat enumerations. Possible values: scoped, prefixtypename, plain');
  Writeln('-x --servicesuffix=string  When constructing type names, add this to schema name. Default is _');
  Writeln('-v --verbose               Output some diagnostic messages');
  Halt(Ord(Msg<>''));
end;

procedure TConvertEDMXApplication.DoRun;

Var
  FConverter : TODataCodeGenerator;
  S,FInput,FOutput : String;

begin
  StopOnException:=True;
  S:=CheckOptions('a:hd:i:o:nb:p:u:vx',['aliases','help','odata:','input:','output:','namespace','basename:','prefix:','verbose','enumerations','servicesuffix'],True);
  if (S<>'') then
    Usage(S);
  if HasOption('h','help') then
    Usage('');
  FInput:=GetOptionValue('i','input');
  FOutput:=GetOptionValue('o','output');
  if (FInput='') then
    Usage('Need input filename');
  if (FOutput='') then
    FOutput:=ChangeFileExt(FInput,'.pas');
  Case lowercase(GetOptionValue('d','odata')) of
    'v2' : FConverter:=csdl2pas.TEDMX2PasConverter.Create(Self);
    'v4' : FConverter:=edmx2pas.TEDMX2PasConverter.Create(Self);
  else
    Usage('Unknown OData version :'+GetOptionValue('d','odata'));
  end;
  try
    if HasOption('x','servicesuffix') then
      FConverter.ServiceSuffix:=GetOptionValue('x','servicesuffix');
    if HasOption('a','aliases') then
      begin
      S:=GetOptionValue('a','aliases');
      if S<>'' then
        if S[1]='@' then
          FConverter.Aliases.LoadFromFile(Copy(S,2,Length(S)-1))
        else
          FConverter.Aliases.CommaText:=S;
      end;
    if HasOption('b','basename') then
      FConverter.BaseClassName:=GetOptionValue('b','basename');
    FConverter.ExtraUnits:=GetOptionValue('e','extraunits');
    if HasOption('p','prefix') then
      FConverter.FieldPrefix:=GetOptionValue('p','prefix');
    if HasOption('u','enumerations') then
      Case lowercase(GetOptionValue('u','enumerations')) of
        'plain' : FConverter.EnumerationMode:=emPlain;
        'scoped' : FConverter.EnumerationMode:=emScoped;
        'prefixtypename' : FConverter.EnumerationMode:=emPrefixTypeName;
      else
        Usage('Unknown enumeration mode :'+GetOptionValue('u','enumerations'));
      end;
    if HasOption('v','verbose') then
      FConverter.OnLog:=@DoMyLog;
    // Go ahead
    FConverter.LoadFromFile(FInput);
    FConverter.OutputUnitName:=ChangeFileExt(ExtractFileName(Foutput),'');
    FConverter.Execute;
    FConverter.SaveToFile(FOutput);
  finally
    FConverter.Free;
  end;
  Terminate;
end;

Procedure TConvertEDMXApplication.DoMyLog(Sender: TObject;
  LogType: TCodegenLogType; Const Msg: String);
begin
  Writeln('[',LogType,'] ',Msg);
end;


begin
  With TConvertEDMXApplication.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free;
    end;
end.

