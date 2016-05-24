{$mode objfpc}
{$h+}
{ $define USESYNAPSE}
program googleapiconv;

uses
  custapp, classes, sysutils, fpjson, jsonparser, googleadexchangebuyer,
  googlebase, googleclient, googlediscoverytopas, fpwebclient,
{$IFDEF USESYNAPSE}
  ssl_openssl,
  synapsewebclient,
{$ELSE}
  fphttpwebclient;

Const
  BaseDiscoveryURL = 'https://www.googleapis.com/discovery/v1/apis/';


Type

  { TGoogleAPIConverter }

  { TAPIEntry }

  TAPIEntry = Class(TCollectionItem)
  private
    FAPIIcon: String;
    FAPIName: String;
    FAPIUnitName: String;
    FFileName: String;
  Public
    Property APIName : String Read FAPIName Write FAPIName;
    Property FileName : String Read FFileName Write FFileName;
    Property APIUnitName : String Read FAPIUnitName Write FAPIUnitName;
    Property APIIcon : String Read FAPIIcon Write FAPIIcon;
  end;

  { TAPIEntries }

  TAPIEntries = Class(TCollection)
  private
    function GetE(AIndex : Integer): TAPIEntry;
  Public
    Function AddEntry : TAPIEntry;
    Property Entries [AIndex : Integer] : TAPIEntry Read GetE; default;
  end;

  TGoogleAPIConverter = CLass(TCustomApplication)
  private
    procedure CreateFPMake(FileName: String; L: TAPIEntries);
    procedure DoAll(LocalFile, URL, OFN: String; AllVersions: Boolean);
    Procedure DoConversion(JS: TStream; AEntry : TAPIEntry) ;
    procedure DownloadIcon(APIEntry: TAPIentry);
    function GetList(LocalFile, URL: String; AllVersions: Boolean): TJSONObject;
    Function HttpGetJSON(Const URL : String; Response : TStream) : Boolean;
    procedure RegisterUnit(FileName: String; L: TAPIEntries);
    procedure Usage(Msg: String);
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure DoRun; override;
  end;

{ TAPIEntries }

function TAPIEntries.GetE(AIndex : Integer): TAPIEntry;
begin
  Result:=Items[AIndex] as TAPIEntry;
end;

function TAPIEntries.AddEntry: TAPIEntry;
begin
  Result:=Add as TAPIEntry;
end;

constructor TGoogleAPIConverter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException:=True;
  TDiscoveryJSONToPas.RegisterAllObjects;
end;

destructor TGoogleAPIConverter.Destroy;
begin
  inherited Destroy;
end;

function TGoogleAPIConverter.HttpGetJSON(const URL: String; Response: TStream): Boolean;

Var
  Webclient : TAbstractWebClient;
  Req: TWebClientRequest;
  Resp : TWebClientResponse;

begin
  Result:=True;
  Req:=Nil;
  Resp:=Nil;
{$IFDEF USESYNAPSE}
  WebClient:=TSynapseWebClient.Create(Self);
{$ELSE}
  WebClient:=TFPHTTPWebClient.Create(Self);
{$ENDIF}
  try
    Req:=WebClient.CreateRequest;
    Req.ResponseContent:=Response;
    Resp:=WebClient.ExecuteRequest('GET',URL,Req);
    Result:=(Resp<>Nil);
  finally
    Resp.Free;
    Req.Free;
    WebClient.Free;
  end;
end;

procedure TGoogleAPIConverter.Usage(Msg : String);

begin
  If (Msg<>'') then
    Writeln('Error : ',Msg);
  Writeln('Usage : ',ExeName,' [options] [inputfile] [outputfile]');
  Writeln('Where options is one of: ');
  Writeln('-a --all                   Download and generate code for all preferred services.');
  Writeln('-A --All                   Download and generate code for all services.');
  Writeln('if one of these options is given, then:');
  Writeln('   a) The service name will be appended to the output filename.');
  Writeln('   b) The --input will be used as a json which lists the services.');
  Writeln('-b --baseclass=classname   Class name to use as parent class for all classes.');
  Writeln('-b --baseclass=classname   Class name to use as parent class for all classes.');
  Writeln('-m --fpmake=filename       Generate fpmake program.');
  Writeln('-e --extraunits=units      comma separated list of units to add to uses clause.');
  Writeln('-h --help                  this message');
  Writeln('-i --input=file            input filename (overrides non-option inputfile)');
  Writeln('-I --icons                 Download service icon (size 16)');
  Writeln('-L --license=licensetext   Set license text to be added to the top of the unit.');
  Writeln('                           Use @filename to load license text from filename');
  Writeln('-o --output=file           output filename (overrides non-option outputfile)');
  Writeln('                           Default is to use input filename with extension .pp');
  Writeln('-p --classprefix=prefix    Prefix to use in class names for all classes.');
  Writeln('-r --resourcesuffix=suffix Suffix to use for resource names. Default is Resource.');
  Writeln('-R --register=unit         Register unit for Lazarus.');
  Writeln('-u --url=URL               URL to download the REST description from.');
  Writeln('-v --serviceversion=v      Service version to download the REST description for.');
  Writeln('If the outputfilename is empty and cannot be determined, an error is returned');
  Halt(Ord(Msg<>''));
end;

function TGoogleAPIConverter.GetList(LocalFile, URL: String;
  AllVersions: Boolean): TJSONObject;

Var
  D : TJSONData;
  S : TStream;

begin
  if (LocalFile<>'') then
    S:=TFileStream.Create(LocalFile,fmOpenRead)
  else
    begin
    S:=TMemoryStream.Create;
    if (URL='') then
      URL:=BaseDiscoveryURL;
    If not AllVersions then
      URL:=URL+'?preferred=true';
    HTTPGetJSON(URL,S);
    S.Position:=0;
    end;
  try
    D:=GetJSON(S);
    if Not (D is TJSONObject) then
      begin
      D.Free;
      Raise Exception.CreateFmt('Source is not a valid JSON description',[LocalFile+URL]);
      end;
    Result:=D as TJSONObject;
  finally
    S.Free;
  end;
end;

procedure TGoogleAPIConverter.RegisterUnit(FileName :String; L : TAPIEntries);

Var
  I : Integer;
  UN,N,V : String;

begin
  UN:=ChangeFileext(ExtractFileName(FileName),'');
  With TStringList.Create do
    try
      Add(Format('unit %s;',[un]));
      Add('');
      Add('interface');
      Add('');
      Add('{$mode objfpc}{$h+}');
      Add('');
      Add('uses sysutils,classes;');
      Add('');
      Add('procedure register;');
      Add('');
      Add('implementation');
      Add('');
      Add('uses');
      if Hasoption('I','icon') then
        Add('  lazres,');
      Add('  restbase,');
      Add('  googleservice,');
      Add('  googlebase,');
      Add('  googleclient,');
      For I:=0 to L.Count-1 do
        begin
        N:=L[i].APIUnitName;
        if I<L.Count-1 then
          Add('  '+N+',')
        else
          Add('  '+N+';')
        end;
      Add('');
      Add('');
      Add('procedure register;');
      Add('');
      Add('begin');
      if Hasoption('I','icon') then
        Add('{$i '+un+'.inc}');
      Add('  RegisterComponents(''Google API'',[');
      Add('    TGoogleClient,');
      For I:=0 to L.Count-1 do
        begin
        N:=L[i].APIName;
        if I<L.Count-1 then
          Add('    '+N+',')
        else
          Add('    '+N)
        end;
      Add('   ]);');
      Add('end;');
      Add('');
      Add('end.');
      SaveToFile(FileName);
    finally
      Free;
    end;
end;

procedure TGoogleAPIConverter.CreateFPMake(FileName :String; L : TAPIEntries);

Var
  I : Integer;
  UN,N,V : String;

begin
  UN:=ChangeFileext(ExtractFileName(FileName),'');
  With TStringList.Create do
    try
      Add('program fpmake;');
      Add('');
      Add('{$mode objfpc}{$h+}');
      Add('');
      Add('uses sysutils,classes, fpmkunit;');
      Add('');
      Add('');
      Add('function StdDep(T : TTarget) : TTarget;');
      Add('begin');
      Add('  T.Dependencies.AddUnit(''googlebase'');');
      Add('  T.Dependencies.AddUnit(''googleservice'');');
      Add('  Result:=T;');
      Add('end;');
      Add('');
      Add('Procedure AddGoogle;');
      Add('');
      Add('Var');
      Add('  P : TPackage;');
      Add('  T : TTarget;');
      Add('');
      Add('begin');
      Add('  With Installer do');
      Add('    begin');
      Add('    P:=AddPackage(''googleapis'');');
      Add('    P.ShortName:=''googleap'';');
      Add('    T:=P.Targets.AddUnit(''googlebase.pp'');');
      Add('    T:=P.Targets.AddUnit(''googleclient.pp'');');
      Add('    T:=P.Targets.AddUnit(''googleservice.pp'');');
      Add('    T.Dependencies.AddUnit(''googleclient'');');
      Add('    T.Dependencies.AddUnit(''googlebase'');');
      For I:=0 to L.Count-1 do
        begin
        N:=L[i].APIUnitName;
        Add(Format('    T:=StdDep(P.Targets.AddUnit(''%s''));',[ExtractFileName(L[i].FAPIUnitName)]));
        end;
      Add('    end;');
      Add('end;');
      Add('');
      Add('{$ifndef ALLPACKAGES}');
      Add('begin');
      Add('  AddGoogle;');
      Add('  Installer.Run;');
      Add('end.');
      Add('{$endif ALLPACKAGES}');
      Add('');
      Add('procedure register;');
      Add('');
      Add('begin');
      SaveToFile(FileName);
    finally
      Free;
    end;
end;


procedure TGoogleAPIConverter.DoAll(LocalFile, URL, OFN : String; AllVersions : Boolean);

Var
  D,O : TJSONObject;
  RS : TStringStream;
  A : TJSONArray;
  S : TJSONEnum;
  LFN,RU,E : String;
  UL : TAPIEntries;
  U : TAPIEntry;
  I : Integer;

begin
  E:=ExtractFileExt(OFN);
  if (E='') then
    E:='.pp';
  UL:=Nil;
  D:=GetList(LocalFile,URL,ALlVersions);
  try
    UL:=TAPIEntries.Create(TAPIEntry);
    A:=D.Get('items',TJSONArray(Nil));
    For S in A do
      begin
      O:=S.Value as TJSONObject;
      if AllVersions or O.Get('preferred',false) then
        begin
        RU:=O.get('discoveryRestUrl');
        LFN:=O.get('name');
        if AllVersions then
          LFN:=LFN+'_'+StringReplace(O.get('version'),'.','',[rfReplaceAll]);
        if (OFN='') then
          LFN:=LFN+E
        else
          LFN:=ChangeFileExt(OFN,LFN+E);
        RS:=TStringStream.Create('');
        try
          if not HttpGetJSON(RU,RS) then
            Raise Exception.Create('Could not download rest description from URL: '+RU);
          Writeln('Converting service "',O.get('name'),'" to unit: ',LFN);
          With TFIleStream.Create(ChangeFileExt(LFN,'.json'),fmCreate) do
            try
              CopyFrom(RS,0);
            finally
              Free;
            end;
//          Writeln('Downloaded : ',RS.DataString);
          RS.Position:=0;
          U:=UL.AddEntry;
          U.FileName:=LFN;
          DoConversion(RS,U);
        finally
          RS.Free;
        end;
        end;
      end;
    if HasOption('R','register') then
      RegisterUnit(GetOptionValue('R','register'),UL);
    if HasOption('m','fpmake') then
      CreateFpMake(GetOptionValue('m','fpmake'),UL);
    if HasOption('I','icon') then
      For I:=0 to UL.Count-1 do
        DownloadIcon(UL[i]);

  finally
    UL.Free;
    D.Free;
  end;
end;

procedure TGoogleAPIConverter.DoRun;

Const
  MyO : Array[1..16] of ansistring
      =  ('help','input:','output:','extraunits:','baseclass:','classprefix:',
          'url:','service:','serviceversion:','resourcesuffix:','license:',
          'All','all','register','icon','fpmake:');

Var
  O,NonOpts : TStrings;
  URL, S, IFN, OFN : AnsiString;
  JS : TStream;
  DoAllServices : Boolean;
  APIEntry : TAPIEntry;

begin
  JS:=Nil;
  O:=Nil;
  NonOpts:=TStringList.Create;
  try
    O:=TStringList.Create;
    For S in MyO do O.Add(S);
    S:=Checkoptions('hi:o:e:b:p:u:s:v:r:L:aAR:Im:',O,TStrings(Nil),NonOpts,True);
    if NonOpts.Count>0 then
      IFN:=NonOpts[0];
    if NonOpts.Count>1 then
      OFN:=NonOpts[1];
  finally
    O.Free;
    NonOpts.Free;
  end;
  if (S<>'') or HasOption('h','help') then
    Usage(S);
  DoAllServices:=HasOption('a','all') or HasOption('A','All');
  if HasOption('i','input') then
    IFN:=GetOptionValue('i','input');
  if HasOption('o','output') then
    OFN:=GetOptionValue('o','output');
  if HasOption('u','url') then
    URL:=GetOptionValue('u','url')
  else if hasOption('s','service') then
    begin
    URL:=BaseDiscoveryURL+getOptionValue('s','service');
    if (pos('/',getOptionValue('s','service'))= 0) and
       HasOption('v','serviceversion') then
       URL:=URL+getOptionValue('v','serviceversion');
    if (URL[Length(URL)]<>'/') then
      URL:=URL+'/';
    URL:=URL+'rest';
    end;
  if (not DoAllServices) and (IFN='') and (URL='') then
    Usage('Need an input filename or URL');
  if (OFN='') then
    if (IFN<>'') then
      OFN:=ChangeFileExt(IFN,'.pp')
    else if getOptionValue('s','service')<>'' then
      OFN:='google'+getOptionValue('s','service')+'.pp';
  if (OFN='') and Not DoAllServices then
    Usage('Need an output filename');
  if DoAllServices then
    DoAll(IFN,URL,OFN,HasOption('A','All'))
  else
    begin
    APIEntry:=Nil;
    if (IFN='') and (URL<>'') then
      begin
      JS:=TMemoryStream.Create;
      if not HttpGetJSON(URL,JS) then
        Raise Exception.Create('Could not download from URL: '+URL);
      JS.POsition:=0;
      end
    else
      JS:=TFileStream.Create(IFN,fmOpenRead or fmShareDenyWrite);
    try
      APIEntry:=TAPIEntry.Create(Nil);
      try
        APIEntry.FileName:=OFN;
        DoConversion(JS,APIEntry);
        if HasOption('I','icon') then
          DownloadIcon(APIEntry);
      finally
        APIEntry.Free;
      end;
    finally
      JS.Free;
    end;
    end;
  Terminate;
end;

procedure TGoogleAPIConverter.DownloadIcon(APIEntry : TAPIentry);

Var
  FN : String;
  FS : TFileStream;

begin
  if (APIEntry.APIIcon<>'') then
    begin
    FN:=ExtractFilePath(APIEntry.FileName)+APIEntry.APIName+ExtractFileExt(APIEntry.APIIcon);
    FS:=TFileStream.Create(FN,fmCreate);
    try
      Writeln(Format('Downloading icon %s to %s',[APIEntry.APIIcon,FN]));
      HttpGetJSON(APIEntry.APIIcon,FS);
    finally
      FS.Free;
    end;
    end;
end;

procedure TGoogleAPIConverter.DoConversion(JS: TStream; AEntry: TAPIEntry);

Var
  L: String;
  O : TGoogleIcons;

begin
  With TDiscoveryJSONToPas.Create(Nil) do
    try
      L:=GetOptionValue('L','license');
      if (L<>'') then
        begin
        if (L[1]<>'@') then
          LicenseText.Text:=L
        else
          begin
          Delete(L,1,1);
          LicenseText.LoadFromFile(L);
          end;
        end;
      ExtraUnits:=GetOptionValue('e','extraunits');
      if HasOption('b','baseclass') then
        BaseClassName:=GetOptionValue('b','baseclass');
      if HasOption('p','classprefix') then
        ClassPrefix:=GetOptionValue('p','classprefix');
      if HasOption('r','resourcesuffix') then
        ResourceSuffix:=GetOptionValue('r','resourcesuffix');
      LoadFromStream(JS);
      AEntry.APIUnitName:=ChangeFileExt(ExtractFileName(AEntry.FileName),'');
      AEntry.APIName:=APIClassName;
      O:=Description.icons;
      if Assigned(O) then
        AEntry.APIIcon:=O.x16;
      SaveToFile(AEntry.FileName);
    finally
      Free;
    end;
end;

Var
  Application : TGoogleAPIConverter;
begin
  {$if declared(Heaptrc)}
  printleakedblock:=true;
  printfaultyblock:=true;
  add_tail:=true;
  SetHeapTraceOutput('heaptrc.txt');
  {$endif}
  Application:=TGoogleAPIConverter.Create(Nil);
  Application.Initialize;
  Application.Run;
end.

