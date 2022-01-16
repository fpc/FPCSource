program testv4;

uses classes, sysutils, jsonparser, odatabase, fpjson, fphttpwebclient, custapp, v4sample;

Type

  { TODataV4SampleServiceClientApp }

  TODataV4SampleServiceClientApp = Class(TCustomApplication)
  Private
    FShowExtra : Boolean;
    FDoDelete : Boolean;
    FSavePhoto : Boolean;
    FService:TService;
  Protected
    Procedure RunDemo;
    // Unbound action
    Procedure DoResetDataSource;
    // Entityset
    Procedure DoDumpAirLines;
    // Contained entity, GetStream
    Procedure DoPhoto(APerson: TPerson; DoSave: Boolean);
    // Contained entityset
    Procedure DoListFriends(P: TPerson; DeleteLast: Boolean);
    // Delete
    Procedure DoDeletePerson(P: TPerson);
    // Bound Function
    Procedure ShowFavoriteAirline(P: TPerson);
    // Bound Function
    Procedure ShowFriendsTrips(P: TPerson);
    // Unbound Function
    Procedure ShowNearestAirPort(ALat, ALon: Integer);
  Public
    Constructor Create(AOwner :TComponent); override;
    Destructor Destroy; override;
    procedure DoServiceLog(Sender: TObject; const Msg: String);
    Procedure DumpExtra(A : TODataObject);
    Procedure DoRun; override;
    Procedure Usage(Const Msg : String);
  end;

Constructor TODataV4SampleServiceClientApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FService:=TService.Create(Self);
  FService.WebClient:=TFPHTTPWebClient.Create(Self);
  StopOnException:=True;
end;

Destructor TODataV4SampleServiceClientApp.Destroy;
begin
  FreeAndNil(FService);
  inherited Destroy;
end;

procedure TODataV4SampleServiceClientApp.DoServiceLog(Sender: TObject;
  const Msg: String);
begin
  Writeln(StdErr,'Service log: ',Msg);
end;

Procedure TODataV4SampleServiceClientApp.DumpExtra(A : TODataObject);

Var
  I : Integer;

  Function PJ(J : TJSONData) : String;

  begin
    if J.JSONType in [jtArray,jtObject] then
      Result:=J.FormatJSON
    else
      Result:=J.AsString;
  end;

begin
  if not FShowExtra then
    exit;
  if Assigned(A.additionalProperties) and (A.additionalProperties.Count>0) then
    begin
    Writeln('   Additional properties : ');
    Writeln('   '+PJ(A.additionalProperties));
    end;
  if (A.ODataAnnotationCount>0) then
    begin
    Writeln('   Annotations:');
    For I:=0 to A.ODataAnnotationCount-1 do
      Writeln('   '+A.ODataAnnotations[i].Key,' : ',PJ(A.ODataAnnotations[i].Value));
    end;
end;

Procedure TODataV4SampleServiceClientApp.DoResetDataSource;

begin
  Writeln('Resetting data source');
  FService.DefaultContainer.ResetDataSource;
end;

Procedure TODataV4SampleServiceClientApp.DoDumpAirLines;

Var
  A : TAirline;
  AA : TAirlineArray;
  I : Integer;

begin
  AA:=FService.DefaultContainer.Airlines.ListAll('');
  try
    Writeln('Number of arlines: ',Length(AA));
    For I:=0 to Length(AA)-1 do
      begin
      A:=AA[i];
      // Writeln('Base URL : ',A.BaseURL(FService));
      Writeln('Airline ',I+1,' code : ',A.AirlineCode);
      Writeln('Airline ',I+1,' name : ',A.Name);
      DumpExtra(A);
      end;
  finally
    For I:=0 to Length(AA)-1 do
      FreeAndNil(AA[i]);
  end;
end;

Procedure TODataV4SampleServiceClientApp.DoDeletePerson(P : TPerson);

begin
  Writeln('Attempting to delete person:');
  try
    Writeln(P.Delete(FService));
  except
    On EO : EOData do
      begin
      Writeln('OData error : ',EO.Message);
      Writeln('Status code : ',EO.StatusCode,', text : ',EO.StatusText);
      If Assigned(EO.Error) then
        begin
        Writeln('OData error code : ',EO.Error.Code,', message : ',EO.Error.Message);
        end;
      end;
    On E : Exception do
      begin
      Writeln('General Error : ',E.Message)
      end;
  end;
end;

Procedure TODataV4SampleServiceClientApp.DoListFriends(P : TPerson; DeleteLast : Boolean);

Var
  FES : TPeopleEntitySet;
  PA : TPersonArray;
  FL : TPerson;
  I : integer;
  F : TPerson;

begin
  FES:=P.Friends(FService);
  try
    PA:=FES.ListAll('');
    I:=0;
    for F in PA do
      begin
      Inc(i);
      Writeln('Friend ',I,':  FirstName: ',F.FirstName,', LastName: ',F.LastName);
      DumpExtra(F);
      FL:=F;
      end;
    If DeleteLast and (FL<>Nil) then
      DoDeletePerson(FL);
  finally
    For I:=0 to Length(PA)-1 do
      FreeAndNil(PA[i]);
  end;
end;

Procedure TODataV4SampleServiceClientApp.DoPhoto(APerson : TPerson; DoSave: Boolean);

Var
  P : TPHoto;
  PF : TFileStream;

begin
  PF:=Nil;
  P:=APerson.Photo(FService);
  try
    Writeln('Photo ID : ',P.Id,', name : ', P.Name);
    DumpExtra(p);
    if DoSave then
      begin
      PF:=TFileStream.Create('photo.jpg',fmCreate);
      P.GetStream(FService,'image/jpeg',PF);
      Writeln('Saved profile photo to photo.jpg');
      end;
  finally
    P.Free;
    PF.Free;
  end;
end;

Procedure TODataV4SampleServiceClientApp.ShowFavoriteAirline(P : TPerson);

Var
  A : TAirLine;

begin
  A:=P.GetFavoriteAirline(FService);
  try
    Writeln('Favorite Airline:');
    Writeln('Code: ',A.AirlineCode);
    Writeln('Name: ',A.Name);
    DumpExtra(A);
  finally
    A.Free;
  end;
end;

Procedure TODataV4SampleServiceClientApp.ShowFriendsTrips(P : TPerson);

Var
  TA : TTripArray;
  I : Integer;

begin
  TA:=P.GetFriendsTrips(FService,'russellwhyte');
  try
    For I:=0 to Length(TA)-1 do
      Writeln('Trip [',i,'] : ',TA[i].Name);
  finally
    For I:=0 to Length(TA)-1 do
      FreeAndNil(TA[i]);
  end;
end;

Procedure TODataV4SampleServiceClientApp.ShowNearestAirPort(ALat,ALon : Integer);

Var
  AP : TAirPort;


begin
  Writeln('Nearest airport for (',alat,',',alon,') : ');
  AP:=FService.DefaultContainer.GetNearestAirPort(Alat,ALon);
  try
    Writeln('Name      : ',AP.Name);
    Writeln('IATA code : ',AP.IataCode);
    Writeln('ICAO code : ',AP.IcaoCode);
    if Assigned(AP.Location) then
      begin
      Writeln('Address   : ',AP.Location.Address);
      Writeln('City      : ',AP.Location.City.Name,' (Country: ',AP.Location.City.CountryRegion,', Region: ',AP.Location.City.Region,')');
      if Assigned(AP.Location.Loc) then
        With AP.Location.Loc do
          Writeln('Location  : ',Coordinates[0],',',Coordinates[1]);
      end;
  finally
    AP.Free;
  end;
end;

Procedure TODataV4SampleServiceClientApp.DoRun;

Var
  S : string;

begin
  S:=CheckOptions('hdepl::u:D',['help','log::','delete','extra','photo','url:','debug']);
  if (S<>'') or HasOption('h','help') then
    Usage(S);
  FShowExtra:=HasOption('e','extra');
  FDoDelete:=HasOption('d','delete');
  FSavePhoto:=HasOption('p','photo');
  if HasOption('l','log') then
    begin
    S:=GetOptionValue('l','log');
    if S='' then
      S:='requests.log';
    FService.WebClient.LogFile:=S;
    end;
  S:=GetOptionValue('u','url');
  if S='' then
    S:='http://services.odata.org/V4/TripPinServiceRW/';
  FService.ServiceURL:=S;
  if HasOption('D','debug') then
    FService.OnLog:=@DoServiceLog;
  RunDemo;
  Terminate;
end;

Procedure TODataV4SampleServiceClientApp.RunDemo;

Var
  Me : TPerson;

begin
  Me:=Nil;
  try
    DoResetDataSource;
    DoDumpAirLines;
    Me:=FService.DefaultContainer.Me;
    Writeln('Me.FirstName: ',Me.FirstName);
    Writeln('Me.LastName: ',Me.LastName);
    DumpExtra(Me);
    DoListFriends(Me,FDoDelete);
    DoPhoto(Me,FSavePhoto);
    ShowFavoriteAirLine(Me);
    ShowFriendsTrips(Me);
    ShowNearestAirPort(40,45);
  finally
    FreeAndNil(Me);
  end;
end;

Procedure TODataV4SampleServiceClientApp.Usage(Const Msg: String);
begin
  Writeln('Error : ',Msg);
  Writeln('Usage : ',ExeName,' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help       This help');
  Writeln('-d --delete     Execute delete call on friend');
  Writeln('-e --extra      Show extra OData information');
  Writeln('-l --log[=file] Dump requests and return to file (default is requests.log)');
  Writeln('-p --photo      Save pictore to photo.jpg');
  Writeln('-u --url=URL    Set Service url.');
  Writeln('-D --debug      Debug output');
  Halt(Ord(Msg<>''));
end;

begin
  With TODataV4SampleServiceClientApp.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free;
    end;
end.

