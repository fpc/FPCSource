{
  Issue and merge request statistics of a GitLab project: open, closed, fixed and total issues
  per Category:: label, and per user the issues closed and merge requests merged.
  The GL_PRIVATE_TOKEN environment variable holds the access token.
}
program issuestats;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DateUtils, CustApp, fpjson, jsonparser, gitlabclient, opensslsockets;

const
  DefaultProject = 'freepascal.org/fpc/source';
  CategoryPrefix = 'Category::';
  FixedLabel = 'Resolution::Fixed';
  UnknownUser = '(unknown)';

type

  { TCategoryCounts }

  // Issue counts of a Category:: label
  TCategoryCounts = class(TObject)
  public
    Open : Integer;
    Closed : Integer;
    Fixed : Integer;
  end;

  { TUserCounts }

  // Number of items per user name
  TUserCounts = class(TStringList)
  public
    constructor Create;
    // Add one to the count of aUser
    procedure Increment(const aUser : String);
    // Count of the item at aIndex
    function CountAt(aIndex : Integer) : Integer;
    // Sum of all counts
    function Total : Integer;
    // Sort on count, highest first, then on name
    procedure SortByCount;
  end;

  { TIssueStatsApplication }

  TIssueStatsApplication = class(TCustomApplication)
  private
    FClient : TGitLabClient;
    FProject : String;
    FDays : Integer;
    FSince : TDateTime;
    FCategories : TStringList;
    FClosedBy : TUserCounts;
    FMergedBy : TUserCounts;
    FThreshold : Integer;
    function ProjectResource(const aResource : String) : String;
    function InPeriod(aObject : TJSONObject; const aDateField : String) : Boolean;
    function UserName(aObject : TJSONObject; const aPaths : array of String) : String;
    function PeriodParams(const aParams : array of String) : TStringArray;
    procedure DoLabel(Sender : TObject; aPage, aIndex, aCount : Integer; aObject : TJSONObject; aContinue : Boolean);
    procedure DoClosedIssue(Sender : TObject; aPage, aIndex, aCount : Integer; aObject : TJSONObject; aContinue : Boolean);
    procedure DoMergedRequest(Sender : TObject; aPage, aIndex, aCount : Integer; aObject : TJSONObject; aContinue : Boolean);
    function FixedCount(const aLabel : String) : Integer;
    procedure WriteCategories;
    procedure WriteUserCounts(const aTitle, aCountTitle : String; aCounts : TUserCounts);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
    // Write the usage, preceded by aError when it is not empty
    procedure Usage(const aError : String); virtual;
  end;


{ TUserCounts }

constructor TUserCounts.Create;

begin
  inherited Create;
  CaseSensitive:=True;
end;


procedure TUserCounts.Increment(const aUser : String);

var
  lIndex : Integer;

begin
  lIndex:=IndexOf(aUser);
  if lIndex<0 then
    AddObject(aUser,TObject(PtrInt(1)))
  else
    Objects[lIndex]:=TObject(PtrInt(Objects[lIndex])+1);
end;


function TUserCounts.CountAt(aIndex : Integer) : Integer;

begin
  Result:=PtrInt(Objects[aIndex]);
end;


function TUserCounts.Total : Integer;

var
  I : Integer;

begin
  Result:=0;
  for I:=0 to Count-1 do
    Inc(Result,CountAt(I));
end;


// Order of two entries of a TUserCounts: higher count first, then name
function CompareCounts(aList : TStringList; aIndex1, aIndex2 : Integer) : Integer;

begin
  Result:=TUserCounts(aList).CountAt(aIndex2)-TUserCounts(aList).CountAt(aIndex1);
  if Result=0 then
    Result:=CompareText(aList[aIndex1],aList[aIndex2]);
end;


procedure TUserCounts.SortByCount;

begin
  CustomSort(@CompareCounts);
end;


// aValue without surrounding whitespace, line ends and quotes
function CleanToken(const aValue : String) : String;

begin
  Result:=Trim(aValue);
  if (Length(Result)>=2) and (Result[1] in ['"','''']) and (Result[Length(Result)]=Result[1]) then
    Result:=Trim(Copy(Result,2,Length(Result)-2));
end;


{ TIssueStatsApplication }

constructor TIssueStatsApplication.Create(TheOwner : TComponent);

begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FClient:=TGitLabClient.Create;
  FCategories:=TStringList.Create;
  FCategories.OwnsObjects:=True;
  FClosedBy:=TUserCounts.Create;
  FMergedBy:=TUserCounts.Create;
end;


destructor TIssueStatsApplication.Destroy;

begin
  FreeAndNil(FMergedBy);
  FreeAndNil(FClosedBy);
  FreeAndNil(FCategories);
  FreeAndNil(FClient);
  inherited Destroy;
end;


function TIssueStatsApplication.ProjectResource(const aResource : String) : String;

begin
  Result:='projects/'+StringReplace(FProject,'/','%2F',[rfReplaceAll])+'/'+aResource;
end;


function TIssueStatsApplication.InPeriod(aObject : TJSONObject; const aDateField : String) : Boolean;

var
  lDate : TDateTime;

begin
  if FDays<=0 then
    Exit(True);
  Result:=TryISO8601ToDate(aObject.Get(aDateField,''),lDate,True) and (lDate>=FSince);
end;


function TIssueStatsApplication.UserName(aObject : TJSONObject; const aPaths : array of String) : String;

var
  lPath : String;
  lData : TJSONData;

begin
  for lPath in aPaths do
    begin
    lData:=aObject.FindPath(lPath);
    if Assigned(lData) and (lData.JSONType=jtString) then
      Exit(lData.AsString);
    end;
  Result:=UnknownUser;
end;


function TIssueStatsApplication.PeriodParams(const aParams : array of String) : TStringArray;

var
  I : Integer;

begin
  Result:=[];
  SetLength(Result,Length(aParams));
  for I:=0 to Length(aParams)-1 do
    Result[I]:=aParams[I];
  if FDays>0 then
    Result:=Concat(Result,['updated_after',DateToISO8601(FSince,True)]);
end;


procedure TIssueStatsApplication.DoLabel(Sender : TObject; aPage, aIndex, aCount : Integer; aObject : TJSONObject; aContinue : Boolean);

var
  lName : String;
  lCounts : TCategoryCounts;

begin
  lName:=aObject.Get('name','');
  if Copy(lName,1,Length(CategoryPrefix))<>CategoryPrefix then
    Exit;
  lCounts:=TCategoryCounts.Create;
  lCounts.Open:=aObject.Get('open_issues_count',0);
  lCounts.Closed:=aObject.Get('closed_issues_count',0);
  FCategories.AddObject(lName,lCounts);
end;


procedure TIssueStatsApplication.DoClosedIssue(Sender : TObject; aPage, aIndex, aCount : Integer; aObject : TJSONObject; aContinue : Boolean);

begin
  if InPeriod(aObject,'closed_at') then
    FClosedBy.Increment(UserName(aObject,['closed_by.username']));
end;


procedure TIssueStatsApplication.DoMergedRequest(Sender : TObject; aPage, aIndex, aCount : Integer; aObject : TJSONObject; aContinue : Boolean);

begin
  if InPeriod(aObject,'merged_at') then
    FMergedBy.Increment(UserName(aObject,['merge_user.username','merged_by.username']));
end;


function TIssueStatsApplication.FixedCount(const aLabel : String) : Integer;

var
  lStats : TJSONObject;

begin
  lStats:=FClient.GetSingleResource(ProjectResource('issues_statistics'),['labels',aLabel+','+FixedLabel]);
  try
    Result:=lStats.GetPath('statistics.counts.closed').AsInteger;
  finally
    lStats.Free;
  end;
end;


procedure TIssueStatsApplication.WriteCategories;

const
  RowFmt = '%-35s %8s %8s %8s %8s';
  CountFmt = '%-35s %8d %8d %8d %8d';

var
  I : Integer;
  lCounts : TCategoryCounts;
  lTotalOpen, lTotalClosed, lTotalFixed : Integer;

begin
  FClient.ForEachResource(ProjectResource('labels'),['with_counts','true','search',CategoryPrefix],@DoLabel);
  FCategories.Sort;
  Writeln('Issues per category (fixed: closed with label ',FixedLabel,')');
  Writeln(Format(RowFmt,['Category','Open','Closed','Fixed','Total']));
  lTotalOpen:=0;
  lTotalClosed:=0;
  lTotalFixed:=0;
  for I:=0 to FCategories.Count-1 do
    begin
    lCounts:=TCategoryCounts(FCategories.Objects[I]);
    lCounts.Fixed:=FixedCount(FCategories[I]);
    Writeln(Format(CountFmt,[Copy(FCategories[I],Length(CategoryPrefix)+1,MaxInt),
                             lCounts.Open,lCounts.Closed,lCounts.Fixed,lCounts.Open+lCounts.Closed]));
    Inc(lTotalOpen,lCounts.Open);
    Inc(lTotalClosed,lCounts.Closed);
    Inc(lTotalFixed,lCounts.Fixed);
    end;
  Writeln(Format(CountFmt,['All categories',lTotalOpen,lTotalClosed,lTotalFixed,lTotalOpen+lTotalClosed]));
  Writeln;
end;


procedure TIssueStatsApplication.WriteUserCounts(const aTitle, aCountTitle : String; aCounts : TUserCounts);

var
  I : Integer;
  lCount,lOther : Integer;

begin
  aCounts.SortByCount;
  lOther:=0;
  if FDays>0 then
    Writeln(aTitle,' in the last ',FDays,' days')
  else
    Writeln(aTitle);
  Writeln(Format('%-35s %8s',['User',aCountTitle]));
  for I:=0 to aCounts.Count-1 do
    begin
    lCount:=aCounts.CountAt(I);
    if lCount>=FThreshold then
      Writeln(Format('%-35s %8d',[aCounts[I],lCount]))
    else
      inc(lOther,lCount);  
    end;  
  if lOther>0 then
    Writeln(Format('%-35s %8d',['Other users',lOther]));
  Writeln(Format('%-35s %8d',['All users',aCounts.Total]));
  Writeln;
end;


procedure TIssueStatsApplication.DoRun;

var
  lError : String;
  lConfig : TGitlabConfig;

begin
  Terminate;
  lError:=CheckOptions('hd:p:u:t:',['help','days:','project:','url:','threshold']);
  if (lError<>'') or HasOption('h','help') then
    begin
    Usage(lError);
    Exit;
    end;
  FDays:=0;
  if HasOption('d','days') then
    if not TryStrToInt(GetOptionValue('d','days'),FDays) or (FDays<=0) then
      begin
      Usage('Number of days must be a positive integer');
      Exit;
      end;
  FProject:=GetOptionValue('p','project');
  if FProject='' then
    FProject:=DefaultProject;
  lConfig.Reset;
  if HasOption('u','url') then
    lConfig.BaseURL:=GetOptionValue('u','url');
  lConfig.APIkey:=CleanToken(GetEnvironmentVariable('GL_PRIVATE_TOKEN'));
  if lConfig.APIkey='' then
    begin
    Usage('The GL_PRIVATE_TOKEN environment variable must contain a GitLab access token');
    Exit;
    end;
  FClient.Config:=lConfig;
  FSince:=IncDay(LocalTimeToUniversal(Now),-FDays);
  FThresHold:=StrToIntDef(GetOptionValue('t','threshold'),1);
  Writeln('Project: ',FProject);
  Writeln;
  try
    WriteCategories;
    FClient.ForEachResource(ProjectResource('issues'),PeriodParams(['state','closed','scope','all']),@DoClosedIssue);
    WriteUserCounts('Issues closed per user','Closed',FClosedBy);
    FClient.ForEachResource(ProjectResource('merge_requests'),PeriodParams(['state','merged','scope','all']),@DoMergedRequest);
    WriteUserCounts('Merge requests merged per user','Merged',FMergedBy);
  except
    on E : EGitLab do
      begin
      Writeln('Error: ',E.Message);
      if Pos(': 401 ',E.Message)>0 then
        begin
        Writeln('GitLab rejected the token in GL_PRIVATE_TOKEN (',Length(lConfig.APIkey),' characters).');
        Writeln('Check that it is a valid personal, project or group access token for ',lConfig.BaseURL,',');
        Writeln('that it has not expired or been revoked, and that it has the read_api scope.');
        end;
      ExitCode:=1;
      end;
  end;
end;


procedure TIssueStatsApplication.Usage(const aError : String);

begin
  if aError<>'' then
    Writeln('Error: ',aError);
  Writeln('Usage: ',ExeName,' [options]');
  Writeln('Where [options] is one or more of:');
  Writeln('-d --days=N       Count only issues closed and merge requests merged in the last N days.');
  Writeln('                  Applies to the user statistics; the category statistics are always for all issues.');
  Writeln('-h --help         This help.');
  Writeln('-p --project=ID   Project ID or path, default ',DefaultProject);
  Writeln('-u --url=URL      GitLab API URL, default ',DefaultGitURL);
  Writeln('-t --threshold=N  Do not show users with less than N fixed issues/merged MRs. Default 1');
  Writeln('The GL_PRIVATE_TOKEN environment variable must contain a GitLab access token.');
end;


var
  Application : TIssueStatsApplication;

begin
  Application:=TIssueStatsApplication.Create(nil);
  Application.Title:='GitLab issue statistics';
  Application.Run;
  Application.Free;
end.
