unit tshttp;

{$mode objfpc}
{$h+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
   classes, httpdefs, fphttp, cgiapp, fpcgi, custcgi, inifiles, types,  sysutils,
   sqldb, whtml, db, dbwhtml,
   tsgraph, tsdb, tssql, tshistory, tstypes, tsconsts, tsutils, tshtml;

Type
  { TTestSuite }

  TTestSuite = Class(TCustomHTTPModule)
  Private
    FResponse: TResponse;
    FTitle: String;
    FVars: TQueryData;
    FRunData : TTestRunData;
    FCompareRunData :TTestRunData;
    FPlatFormID : Integer;
    FHTMLWriter : TTestSuiteHtmlWriter;
    FSQL : TTestSQL;
    FConstructSQL : TTestSuiteSQL;
    FRunStats : TRunStats;
    FInfo : TDBInfo;
    FRequest : TRequest;
    FContent : TStream;
    procedure DoDetailURL(aRunID: Int64; aDate: TDateTime; out aURl: String);
    Procedure GetOverviewRowAttr(Sender : TObject; Var BGColor : String;
                                   Var Align : THTMLAlign; Var VAlign : THTMLValign;
                                   Var CustomAttr : String) ;
    Procedure GetRunRowAttr(Sender : TObject; Var BGColor : String;
                            Var Align : THTMLAlign; Var VAlign : THTMLValign;
                            Var CustomAttr : String) ;
    function CreateTestSQL: TTestSQL;
    function GetVersionControlURL: string;
    procedure ShowAllHistoryData(aQuery: TSQLQuery);
    procedure ShowLastLog(aRunID: Int64; aTestID, aPlatformID: Integer);
    procedure ShowSourceFile;
    procedure WriteTestInfo;
  Public
    constructor createnew(aOwner : TComponent; CreateMode: Integer); override;
    destructor destroy; override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    Function InitCGIVars : Integer;
    Procedure DoRun; // override;
    Procedure ShowRunResults;
    Procedure ShowRunComparison;
    Procedure ShowOneTest;
    Procedure ShowHistory;
    Function ConnectToDB : Boolean;
    procedure DisconnectFromDB;
    Procedure ShowRunOverview;
    Procedure CreateRunPie;
    Function  ShowRunData : Boolean;
    Procedure LDump(Const St : String);
    Procedure LDumpLn(Const St : String);
    Property Title : String Read FTitle Write FTitle;
    Property Request : TRequest Read FRequest;
    Property Response : TResponse Read FResponse;
  end;

Procedure HandleTestSuiteRequest(aRequest : TRequest; aResponse : TResponse);

implementation

uses
  wformat,
  dateutils;

Procedure HandleTestSuiteRequest(aRequest : TRequest; aResponse : TResponse);

Var
  Suite : TTestSuite;

begin
  Suite:=TTestSuite.CreateNew(Nil);
  try
    Suite.Title:='Free Pascal Compiler Test Suite Results';
    Suite.HandleRequest(aRequest,aResponse);
    aResponse.SendResponse;
  finally
    Suite.Free;
  end;

end;


procedure TTestSuite.DoRun;

var
  lAction : integer;

begin
//  Terminate;
  Try
    ConnectToDB;
    lAction:=InitCGIVars;
    if (FVars.RunID>0) and not FSQL.GetRunData(FVars.RunID,FRunData) then
      FRunData.RunID:=-1;
    if (FVars.CompareRunID>0) and not FSQL.GetRunData(FVars.CompareRunID,FCompareRunData) then
      FCompareRunData.RunID:=-1;
    Case lAction of
      faction_show_overview :
        begin
        FHTMLWriter.EmitOverviewForm(Title);
        ShowRunOverview;
        end;
      faction_show_run_results :
        if (FVars.CompareRunID<=0) then
          ShowRunResults
        else
          ShowRunComparison;
      faction_show_run_pie : CreateRunPie;
      faction_show_one_test : ShowOneTest;
      faction_show_history : ShowHistory;
      faction_compare_with_previous :
        begin
          FVars.CompareRunID:=FVars.RunID;
          FVars.RunID:=FVars.PreviousRunID;
          ShowRunComparison;
        end;
      faction_compare_with_next :
        begin
          FVars.CompareRunID:=FVars.NextRunID;
          ShowRunComparison;
        end;
      faction_compare2_with_previous :
        begin
          FVars.RunID:=FVars.Previous2RunID;
          ShowRunComparison;
        end;
      faction_compare2_with_next :
        begin
          FVars.RunID:=FVars.CompareRunID;
          FVars.CompareRunID:=FVars.Next2RunID;
          ShowRunComparison;
        end;
      faction_compare_both_with_previous :
        begin
          FVars.RunID:=FVars.PreviousRunID;
          FVars.CompareRunID:=FVars.Previous2RunID;
          ShowRunComparison;
        end;
      faction_compare_both_with_next :
        begin
          FVars.RunID:=FVars.NextRunID;
          FVars.CompareRunID:=FVars.Next2RunID;
          ShowRunComparison;
        end;
      end;
  finally
    FHTMLWriter.EmitEnd;
    DisConnectFromDB;
  end;
end;


function TTestSuite.InitCGIVars: Integer;

var
  L : TStrings;
begin
  TestsuiteCGIURL:=Request.ScriptName;
  DateSeparator:='/';
  L:=TStringList.Create;
  try
    FVars.InitFromVars(FSQL,Request.QueryFields);
  finally
    L.Free;
  end;
  Result:=FVars.Action;
  SDetailsURL := TestsuiteCGIURL + '?action=1&amp;run1id=%s';
end;

procedure TTestSuite.DoDetailURL(aRunID: Int64; aDate: TDateTime; out aURl: String);
var
  lDate : String;
begin
  if aDate=0 then
    lDate:='never'
  else
    lDate:=DateToStr(aDate);
  aURL:=Self.FHTMLWriter.FormatDetailURL(IntToStr(aRunID),lDate);
end;

function TTestSuite.CreateTestSQL : TTestSQL;

var
  aIni : TCustomIniFile;
  lPort : Integer;
  lHostName,lDatabaseName,lUserName,lPassword : String;

begin
  Result:=Nil;
  aIni:=TMemIniFile.Create(DefaultDBConfigFileName);
  try
    With aIni do
      begin
      lHostName:=ReadString(SSection,KeyHost,'localhost');
      lDatabaseName:=ReadString(SSection,KeyName,'testsuite');
      lUserName:=ReadString(SSection,KeyUser,'');
      lPassword:=ReadString(SSection,KeyPassword,'');
      lPort:=ReadInteger(SSection,KeyPort,0);
      end;
  finally
    aIni.Free;
  end;
  if (lHostName='') or (lDatabaseName='') or (lUserName='') or (lPassword='') then
    exit;
  Result:=TTestSQL.create(lDatabaseName,lHostName,lUserName,lPassword,lPort);
end;

constructor TTestSuite.createnew(aOwner: TComponent; CreateMode: Integer);

begin
  inherited createNew(aOwner,CreateMode);

  FSQL:=CreateTestSQL;
  FInfo:=TDBInfo.Create;
  FVars:=TQueryData.Create;
  FConstructSQL:=TTestSuiteSQL.create(FVars,FSQL,FInfo);
  FContent:=TMemoryStream.Create;
  FHtmlWriter:=TTestSuiteHTMLWriter.Create(FContent,FSQL,FVars);
  OnVerbose:=@FHtmlWriter.HandleVerbose;
end;

destructor TTestSuite.destroy;
begin
  OnVerbose:=Nil;
  FreeAndNil(FContent);
  FreeAndNil(FConstructSQL);
  FreeAndNil(FInfo);
  FreeAndNil(FVars);
  FreeAndNil(FSQL);
  inherited destroy;
end;

procedure TTestSuite.HandleRequest(ARequest: TRequest; AResponse: TResponse{; var AHandled: Boolean});

begin
  FRequest:=aRequest;
  FResponse:=aResponse;
  try
    DoRun;
    aResponse.ContentStream:=FContent;
  finally
    FRequest:=Nil;
    FResponse:=Nil;
  end;
end;

function TTestSuite.ConnectToDB: Boolean;

begin
  Result:=False;
  Result:=FSQL.ConnectToDatabase;
  if not Result then
    exit;
  FInfo.AllCategoryID:=FSQL.GetCategoryID('All');
  FInfo.AllOSID:=FSQL.GetOSID('All');
  FInfo.AllCPUID:=FSQL.GetCPUID('All');
  if FVars.OSID <= 0 then
    FVars.OSID:=FInfo.AllOSID;
  if FVars.CPUID<=0 then
    FVars.CPUID:=FInfo.AllCPUID;
end;


procedure TTestSuite.LDump(const St: String);

var
  ShortS : ShortString;
  i,p  : longint;
begin
  i:=length(St);
  p:=1;
  while (i>255) do 
    begin
      ShortS:=copy(St,p,255);
      inc(p,255);
      dec(i,255);
      FHTMLWriter.Dump(ShortS);
    end;
  ShortS:=Copy(St,p,255);
  FHTMLWriter.Dump(ShortS);
end;


procedure TTestSuite.LDumpLn(const St: String);
begin
  LDump(St);
  LDump(LineFeed);
end;


procedure TTestSuite.DisconnectFromDB;

begin
  If Assigned(FSQL) then
    begin
    FSQL.DisconnectDatabase;
    FreeAndNil(FSQL);
    end;
end;


procedure TTestSuite.GetOverviewRowAttr(Sender: TObject; var BGColor: String;
  var Align: THTMLAlign; var VAlign: THTMLValign; var CustomAttr: String);
begin
  If ((Sender as TTAbleProducer).CurrentRow mod 2=0) then
    BGColor:='#EEEEEE'
end;

procedure TTestSuite.ShowRunOverview;

var
  Qry : String;
  Q : TSQLQuery;
  A : String;
  lTable : TTableProducer;

begin
  A:=SDetailsURL;
  If FVars.OnlyFailed then
    A:=A+'&amp;failedonly=1';
  If FVars.NoSkipped then
    A:=A+'&amp;noskipped=1';
  Qry:=FConstructSQL.GetRunOverviewSQL;
  If FVars.Debug then
    Writeln('Query : '+Qry);
  lTable:=Nil;
  Q:=FSQL.CreateQuery(Qry);
  try
    Q.Open;
    lTable:=FHTMLWriter.CreateTableProducer(Q);
    lTable.Border:=True;
    lTable.OnGetRowAttributes:=@GetOverViewRowAttr;
    lTable.CreateColumns(Nil);
    With lTable.TableColumns do
      begin
      ColumnByName('ID').ActionURL:=A;
      ColumnByName('Failed').OnGetCellContents:=@FHTMLWriter.FormatFailedOverview;
      ColumnByName('rev').OnGetCellContents:=@FHTMLWriter.FormatSVN;
      ColumnByName('comprev').OnGetCellContents:=@FHTMLWriter.FormatSVN;
      ColumnByName('rtlrev').OnGetCellContents:=@FHTMLWriter.FormatSVN;
      ColumnByName('packrev').OnGetCellContents:=@FHTMLWriter.FormatSVN;
      ColumnByName('testsrev').OnGetCellContents:=@FHTMLWriter.FormatSVN;
      end;
    lTable.CreateTable(FContent);
    FHTMLWriter.DumpLn(Format('<p>Record count: %d</p>',[Q.RecordCount]));
  finally
    lTable.Free;
    Q.Free;
  end;
end;

function TTestSuite.ShowRunData: Boolean;

  procedure EmitOneRow(RowTitle,FieldLeft,FieldRight : String; is_same : boolean);
    var
      FieldColor : string;
    begin
      if (FieldRight='') then
        FieldColor:=''
      else if is_same then
        FieldColor:='style="color:green;"'
      else
        FieldColor:='style="color:red;"';
      With FHTMLWriter do
        begin
          RowNext;
          if FieldColor<>'' then
            begin
              TagStart('TD',FieldColor);
            end
          else 
            CellStart;
          LDumpLn(RowTitle);
          if FieldColor<>'' then
            begin
              CellEnd;
              TagStart('TD',FieldColor);
            end
          else 
            CellNext;
          LDumpLn(FieldLeft);
          if FieldColor<>'' then
            begin
             CellEnd;
             TagStart('TD',FieldColor);
            end
          else 
            CellNext;
          LDumpLn(FieldRight);
          CellEnd;
        end;
    end;

  procedure EmitOneRow(RowTitle,FieldLeft,FieldRight : String);
    var
      is_same : boolean;
    begin
      is_same:=(FieldLeft=FieldRight);
      EmitOneRow(RowTitle,FieldLeft,FieldRight,is_same);
    end;

var
  aData,aCompData : TTestRunData;
  AddNewPar : Boolean;

  procedure EmitRow(RowTitle,FieldName : String);
    var
      FieldLeft, FieldRight : String;
    begin
      FieldLeft:=aData.GetField(FieldName);
      if aCompData.RunID>0 then
        FieldRight:=aCompData.GetField(FieldName)
      else
        FieldRight:='';
      EmitOneRow(RowTitle,FieldLeft,FieldRight);
    end;

  procedure MaybeEmitButton(const aVar,aValue : String; aCondition : boolean);

  begin
    if not aCondition then exit;
    FHTMLWriter.EmitSubmitButton(aVar,aValue);
    AddNewPar:=True;
  end;

  procedure CheckPar;
  begin
    if not AddNewPar then exit;
    FHTMLWriter.ParagraphEnd;
    FHTMLWriter.ParaGraphStart;
  end;

Var
  isComp : Boolean;
  FLeft,FRight : string;
  Date1, Date2 : String;
  lNextRunID,lNext2RunID : Int64;
  lPreviousRunID,lPrevious2RunID : Int64;
  same_date : boolean;
  CompilerDate1, CompilerDate2 : string;

begin
  lNextRunID:=-1;
  lNext2RunID:=-1;
  lPreviousRunID:=-1;
  lPrevious2RunID:=-1;

  Result:=(FVars.RunID<>-1);
  If not Result then
    exit;
  if Not FSQL.GetRunData(FVars.RunID,aData) then
    exit;
  isComp:=FVars.CompareRunID>0;
  if isComp and Not FSQL.GetRunData(FVars.CompareRunID,aCompData) then
    exit;
  With FHTMLWriter do
    begin
    FormStart(TestsuiteCGIURL,'get');
    TableStart(3,true);
    RowStart;
      CellStart;
        DumpLn('Run ID:');
      CellNext;
        EmitInput('run1id',IntToStr(FVars.RunID));
      CellNext;
        EmitInput('run2id',IntToStr(FVars.CompareRunID));
      CellEnd;

    EmitRow('Operating system:','os');
    EmitRow('Processor:','cpu');
    EmitRow('Version:','VERSION');
    if Not IsComp then
      FRight:=''
    else
      begin
        FRight:=aCompData.GetField('Failed')+
                '/'+aCompData.GetField('Ok')+
                '/'+aCompData.GetField('Total');
      end;
    EmitOneRow('Fails/OK/Total:',
         aData.GetField('Failed')+
         '/'+aData.GetField('Ok')+
         '/'+aData.GetField('Total'),
      FRight);
    EmitRow('Version:','Version');
    EmitRow('Full version:','CompilerFullVersion');
    EmitRow('Config:','Config');
    EmitRow('Machine:','Macgine');
    if (FVars.CategoryID>0) then
      EmitRow('Category:','TU_CATEGORY_FK');
    If (FVars.CategoryID=1) then
      begin
        FLeft:=aData.GetField('rev');
        FormatSVNData(FLeft);
        if isComp then
          begin
            FRight:=aCompData.GetField('rev');
            FormatSVNData(FRight);
          end
        else
          FRight:='';
        EmitOneRow('SVN revisions:',FLeft,FRight);
      end;
    EmitRow('Submitter:','Submitter');
    Date1 := aData.GetField('Date');
    if Not IsComp then
      FRight:=''
    else
      begin
      Date2 := aCompData.GetField('Date');
      FRight:=Date2;
      end;
    same_date:=(date1=Date2);
    EmitOneRow('Date:',Date1,FRight,same_date);
    CompilerDate1 := aData.GetField('CompilerDate');
    if Not IsComp then
      FRight:=''
    else
      begin
      CompilerDate2 := aCompData.GetField('compilerdate');
      FRight:=CompilerDate2;
      end;
    same_date:=(CompilerDate1=CompilerDate2);
    EmitOneRow('CompilerDate:',CompilerDate1,FRight,same_date);
    lPreviousRunID:=FSQL.GetPreviousRunID(aData.RunID);
    EmitHiddenVar('previousrunid',lPreviousRunID);
    FLeft:=IntToStr(lPreviousRunID);
    if IsComp then
      begin
        lPrevious2RunID:=FSQL.GetPreviousRunID(FVars.CompareRunID);
        FRight:=IntToStr(lPrevious2RunID);
        EmitHiddenVar('previous2runid',lPrevious2RunID);
      end
    else
      FRight:='';
    EmitOneRow('Previous run:',FLeft,FRight);
    lNextRunID:=FSQL.GetNextRunID(FVars.RunID);
    EmitHiddenVar('nextrunid',lNextRunID);
    FLeft:=IntToStr(lNextRunID);
    if IsComp then
      begin
        lNext2RunID:=FSQL.GetNextRunID(FVars.CompareRunID);
        FRight:=IntToStr(lNext2RunID);
        EmitHiddenVar('next2runid',lNext2RunID);
      end;
    EmitOneRow('Next run:',FLeft,FRight);
    RowEnd;
    TableEnd;
    ParagraphStart;
    if FVars.Debug then
      EmitHiddenVar('DEBUGCGI', '1');
    EmitCheckBox('noskipped','1',FVars.NoSkipped);
    DumpLn(' Hide skipped tests');
    ParagraphEnd;
    ParagraphStart;
    EmitCheckBox('failedonly','1',FVars.onlyFailed);
    DumpLn(' Hide successful tests');
    ParagraphEnd;
    ParaGraphStart;
    AddNewPar:=false;
    MaybeEmitButton('action', 'Compare_to_previous', lPreviousRunID<>-1);
    MaybeEmitButton('action', 'Compare_to_next', (lNextRunID<>-1) and (lNextRunID <> FVars.CompareRunID));
    MaybeEmitButton('action', 'Compare_right_to_previous', (lPrevious2RunID<>-1) and (lPrevious2RunID <> FVars.RunID));
    MaybeEmitButton('action', 'Compare_right_to_next',lNext2RunID<>-1);
    CheckPar;
    MaybeEmitButton('action', 'Compare_both_to_previous', (lPrevious2RunID<>-1) and (lPreviousRunId<>-1));
    MaybeEmitButton('action', 'Compare_both_to_next', (lNext2RunID<>-1) and (lNextRunId<>-1));
    CheckPar;
    MaybeEmitButton('action','Show/Compare',True);
    MaybeEmitButton('action','View_history',FVars.TestFileID<>-1);
    EmitResetButton('','Reset form');
    ParagraphEnd;
    FormEnd;
    { give warning if dates reversed }
    if IsComp and (aData.Date > aCompData.Date) then
      begin
      ParagraphStart;
      DumpLn('Warning: testruns are not compared in chronological order.');
      ParagraphEnd;
      end;
    end;
end;

procedure TTestSuite.ShowRunResults;

Var
  S : String;
  Qry : String;
  Q : TSQLQuery;
  FL : String;
  lTable : TTableProducer;

begin
  Response.ContentType:='text/html';
  //EmitContentType;
  With FHTMLWriter do
    begin
    EmitDocType;
    EmitTitle(Title+' : Search Results');
    HeaderStart(1);
    DumpLn('Test suite results for run '+IntToStr(FVars.RunID));
    HeaderEnd(1);
    HeaderStart(2);
    DumpLn('Test run data : ');
    HeaderEnd(2);
    If not ShowRunData then
      begin
      DumpLn('No data for test run with ID: '+IntToStr(FVars.RunID));
      Exit;
      end;
    HeaderStart(2);
    DumpLn('ShowRunResults detailed test run results:');
    FL:='';
    If FVars.OnlyFailed or FVars.NoSkipped then
      begin
      FL:='';
      If FVars.OnlyFailed then
        FL:='successful';
      if FVars.NoSkipped then
        begin
        If (FL<>'') then
          FL:=FL+' and ';
        FL:=FL+'skipped';
        end;
      DumpLn(' ('+FL+' tests are hidden)');
      end;
    HeaderEnd(2);
    FPlatFormID:=FSQL.GetPlatformID(FVars.RunID);
    S:=Format(SQLSelectTestResults,[FVars.RunID,FPlatformID]);
    If FVars.OnlyFailed then
      S:=S+' AND (not TR_OK)';
    If FVars.NoSkipped then
      S:=S+' AND (not TR_SKIP)';
    S:=S+' ORDER BY TR_ID ';
    Qry:=S;
    If FVars.Debug then
      begin
      ParaGraphStart;
      Dumpln('Query : '+Qry);
      ParaGraphEnd;
      end;
    end;
  Q:=FSQL.CreateQuery(Qry);
  try
    Q.PacketRecords:=-1;
    Q.Open;
    FHTMLWriter.DumpLn(Format('<p>Record count: %d </p>',[Q.RecordCount]));
    FL:='Id,Filename';
    If Not FVars.NoSkipped then
      FL:=FL+',Skipped';
    If Not FVars.OnlyFailed then
      FL:=FL+',OK';
    FL:=FL+',Result';
    lTable:=FHTMLWriter.CreateTableProducer(Q);
    lTable.Border:=True;
    lTable.CreateColumns(FL);
    lTable.OnGetRowAttributes:=@GetRunRowAttr;
    With lTable.TableColumns do
      begin
      ColumnByName('Id').OnGetCellContents:=@FHTMLWriter.FormatFileIDDetails;
      ColumnByName('Filename').OnGetCellContents:=@FHTMLWriter.FormatFileDetails;
      ColumnByName('Result').OnGetCellContents:=@FHTMLWriter.FormatTestResult;
      end;
    lTable.CreateTable(FContent); //Response);
  finally
    lTable.Free;
    Q.Free;
  end;
  If Not (FRunStats.OKCount=0) and not (FVars.NoSkipped and FVars.OnlyFailed) then
    FHTMLWriter.EmitPieImage(FRunStats.OKCount,FRunStats.FailedCount,FRunStats.SkipCount);
end;

procedure TTestSuite.ShowOneTest;

Var
  Qry : String;
  Q : TSQLQuery;
  Res : Boolean;
  lTable : TTableProducer;

begin
  Response.ContentType:='text/html';
//  EmitContentType;
  With FHTMLWriter do
    begin
    EmitDocType;
    EmitTitle(Title+' : File '+FVars.TestFileName+' Results');
    HeaderStart(1);
    DumpLn('Test suite results for test file '+FVars.TestFileName);
    HeaderEnd(1);
    HeaderStart(2);
    DumpLn('Test run data : ');
    HeaderEnd(2);
    if FVars.RunID<>-1 then
      Res:=ShowRunData
    else
      Res:=True;
    If not Res then
      begin
      DumpLn(Format('No data for test file with ID: %s',[FVars.TestFileID]));
      exit;
      end;
    WriteTestInfo;
    Qry:=FConstructSQL.GetSimpleTestResultsSQL;
    If FVars.Debug then
    begin
      ParaGraphStart;
      Dumpln('Query : '+Qry);
      ParaGraphEnd;
    end;
    FRunStats:=Default(TRunStats);
    lTable:=nil;
    Q:=FSQL.CreateQuery(Qry);
    try
      Q.Open;
      lTable:=CreateTableProducer(Q);
      lTable.Border:=True;
      lTable.CreateColumns(Nil);
      With lTable.TableColumns do
        begin
        Delete(ColumnByName('TR_TEST_FK').Index);
        ColumnByName('RUN').OnGetCellContents:=@FormatTestRunOverview;
        ColumnByName('TR_RESULT').OnGetCellContents:=@FormatTestResult;
        end;
      lTable.CreateTable(FContent); // Response);
      ParaGraphStart;
      DumpLn(Format('Record count: %d',[Q.RecordCount]));
      ParaGraphEnd;
    finally
      lTable.Free;
      Q.Free;
    end;
    if FVars.RunId<>-1 then
      ShowLastLog(FVars.RunId,fvars.testfileid,FRunData.PlatformID);
    if FVars.CompareRunId<>-1 then
      ShowLastLog(FVars.CompareRunId,fvars.testfileid,FRunData.PlatformID);
    if FVars.Debug then
      DumpLn(Format('After Log. Run ID: %d, Testfile ID: %d',[fvars.RunID, fvars.testfileid]));
    ShowSourceFile;
    end;
end;

procedure TTestSuite.ShowLastLog(aRunID : Int64; aTestID,aPlatformID : Integer);
var
  LLog : String;
begin
  LLog:=FSQL.StringQuery(Format('select TR_LOG from TESTLASTRESULTS left join testresults on (TL_TESTRESULTS_FK=TR_ID) where (TR_TEST_FK=%d) and (TL_PLATFORM_FK=%d)',[aTestID,aPlatformID]));
  With FHTMLWriter do
    if LLog='' then
      begin
      HeaderStart(2);
      DumpLn(Format('No log of %s on run %d:',[FVars.TestFileName,aRunId]));
      HeaderEnd(2);
      end
    else
      begin
      HeaderStart(2);
      DumpLn(Format('Log of %s on run %d:',[FVars.TestFileName,aRunID]));
      HeaderEnd(2);
      PreformatStart;
      system.Write(LLog);
      system.flush(output);
      PreformatEnd;
      end;
end;

procedure TTestSuite.WriteTestInfo;

var
  lTestInfo : TTestInfo;

begin
  With FHTMLWriter do
    begin
    HeaderStart(2);
    DumpLn('Test file "'+FVars.TestFileName+'" information:');
    HeaderEnd(2);
    ParaGraphStart;
    if (FVars.TestFileID<>-1) and FSQL.GetTestInfo(FVars.TestFileID,lTestInfo) then
      DumpTestInfo(lTestInfo);
    ParaGraphEnd;
    HeaderStart(2);
    DumpLn('WriteTestInfo detailed test run results:');
    HeaderEnd(2);
    end;
end;


procedure TTestSuite.ShowHistory;

Var
  Res : Boolean;
  Qry : String;
  Q : TSQLQuery;
  TS : TTestStatus;
  lHistory : TTestHistoryInfo;
  lOSMap,lCPUMap,lVersionMap : TIntegerDynArray;
  lTable : TTableProducer;

begin
//  Res:=False;
  Response.ContentType:='text/html';
  // EmitContentType;
  lTable:=nil;
  Q:=Nil;
  try
    With FHTMLWriter do
      begin
      EmitDocType;
      if FVars.TestFileName<>'' then
        EmitTitle(Title+' : File '+FVars.TestFileName+' Results')
      else
        EmitTitle(Title+' : History overview');
      if FVars.TestFileName<>'' then
        begin
          HeaderStart(1);
          DumpLn('Test suite results for test file '+FVars.TestFileName);
          HeaderEnd(1);
          HeaderStart(2);
          DumpLn('Test run data : ');
          HeaderEnd(2);
        end;
      if FVars.RunID<>-1 then
        Res:=ShowRunData
      else
        begin
        EmitHistoryForm(Title);
        Res:=(FVars.TestFileID<>-1);
        if not Res then
          begin
          HeaderStart(2);
          if Trim(FVars.TestFileName) <> '' then
            DumpLn(Format('Error: No test files matching "%s" found.', [FVars.TestFileName]))
          else
            DumpLn('Error: Please specify a test file.');
          HeaderEnd(2);
          end;
        end;
      If not Res then
        exit;
      if (FVars.TestFileName<>'') then
        WriteTestInfo;
      ParaGraphStart;
      If not FInfo.IsAllCPU(FVars.CPUID) then
        lCPUMap:=FSQL.CreateMap(mtCPU);
      If not FInfo.IsAllOS(FVars.OSID) then
        lOSMap:=FSQL.CreateMap(mtOS);
      if not FInfo.IsAllVersion(fVars.VersionID) then
        lVersionMap:=FSQL.CreateMap(mtVersion);
      lHistory:=TTestHistoryInfo.Create(FSQL,lOSMap,lCPUMap,lVersionMap);
      lHistory.OnGetDetailURL:=@DoDetailURL;

      Qry:=FConstructSQL.GetTestResultsSQL;
      If FVars.Debug then
      begin
        Writeln(system.stdout,'Query : '+Qry);
        system.Flush(system.stdout);
      end;
      FRunStats:=Default(TRunStats);
      Q:=FSQL.CreateQuery(Qry);
      Q.PacketRecords:=-1;
      Q.Open;
      lHistory.UpdateFromDataset(Q);
      DumpLn(Format('<p>Total = %d </p>',[lHistory.total_count]));
      if lHistory.Total_count > 0 then
        DumpLn(Format('<p>OK=%d Percentage= %3.2f </p>',[lHistory.OK_count,lHistory.OK_count*100/lHistory.total_count]));
      if lHistory.Skip_count > 0 then
        DumpLn(Format('<p>Skipped=%d Percentage= %3.2f </p>',[lHistory.Skip_count,lHistory.Skip_count*100/lHistory.total_count]));
      if lHistory.total_count>0 then
        begin
          TableStart(5,True);
          RowStart;
          CellStart;
          DumpLn('Result type');
          CellNext;
          DumpLn('Cat.');
          CellNext;
          DumpLn('Count');
          CellNext;
          DumpLn('Percentage');
          CellNext;
          DumpLn('First date');
          CellNext;
          DumpLn('Last Date');
          CellEnd;
        end;
        For TS:=FirstStatus to LastStatus do
          if lHistory.Result_count[TS]>0 then
            begin
              lHistory.WriteCounts(FHTMLWriter,TS);
              lHistory.WriteCPUHistory(FHTMLWriter,TS);
              lHistory.WriteOSHistory(FHTMLWriter,TS);
              lHIstory.WriteVersionHistory(FHTMLWriter,TS);
            end;
        if lHistory.total_count>0 then
          begin
            TableEnd;
          end;
      end; // FHTMLWriter;
    If FVars.Debug or FVars.ListAll then
      ShowAllHistoryData(Q);
    ShowSourceFile;
  Finally
    lTable.Free;
    Q.Free;
  end;
end;

procedure TTestSuite.ShowAllHistoryData(aQuery: TSQLQuery);

var
  FL : String;
  lTable : TTableProducer;
begin
  aQuery.First;
  FL:='RUN,Date,OK,SKIP,Result';
  if FVars.Submitter='' then
    FL:=FL+',Submitter';
  if FVars.Machine='' then
    FL:=FL+',Machine';
  if FVars.Config='' then
    FL:=FL+',Config';
  if (FVars.OSID=-1) or (FVars.OSID=FInfo.AllOSID) then
    FL:=FL+',OS';
  if (FVars.CPUID=-1) or (FVars.CPUID=FInfo.AllCPUID) then
    FL:=FL+',CPU';
  if (FVars.VersionID=-1) or (FVars.VersionID=FInfo.AllVersionID) then
    FL:=FL+',Version';
  FL:=FL+',Fails,CompDate';
  FL:=FL+',Tests_rev,RTL_rev,Compiler_rev,Packages_rev';
  lTable:=FHTMLWriter.CreateTableProducer(aQuery);
  try
    lTable.Border:=True;
    lTable.CreateColumns(FL);
    lTable.TableColumns.ColumnByName('RUN').OnGetCellContents:=@FHTMLWriter.FormatTestRunOverview;
    lTable.TableColumns.ColumnByName('Result').OnGetCellContents:=@FHTMLWriter.FormatTestResult;
    lTable.CreateTable(FContent); //Response);
  finally
    lTable.Free
  end;
end;


function TTestSuite.GetVersionControlURL : string;

var
  Base,lURL : String;
  ver : known_versions;
  Index : Integer;

begin
  Base:='trunk';
  if  FVars.VersionBranch<>'' then
    begin
      // Test all but last version, which is assumed to be trunk
      for ver:=low(known_versions) to pred(high(known_versions)) do
        if ver_string[ver]=FVars.VersionBranch then
          begin
            base:=ver_branch[ver];
            break;
          end;
    end;
  index:=pos('/',Base);
  if index>0 then
    Base:=Copy(Base,index+1,length(Base));
  if Base='trunk' then
    Base:='main';
  lURL:=ViewGitHashURL+Base;
  if FVars.CategoryID<=1 then
    lURL:=lURL+TestsSubDir
  else
    begin
    lURL:=lURL+DataBaseSubDir;
    // This assumes that type TAnyType is
    // defined in anytype.pas source PM
    if pos('/',FVars.TestFileName)>0 then
      FVars.Testfilename:=lowercase(copy(FVars.TestFilename,2,pos('/',FVars.TestFilename)-2)+'.pas');
    end;
  Result:=lURL;
end;

procedure TTestSuite.ShowSourceFile;

var
  lFN,lUrl,Source : String;

begin
  Source:='';
  lFn:=FVars.TestFileName;
  if (fvars.testfileid <> -1) then
    Source:=FSQL.GetTestSource(fvars.testfileid);
  With FHTMLWriter do
    begin
    if Source<>'' then
      begin
      HeaderStart(2);
      DumpLn('Source:');
      HeaderEnd(2);
      PreformatStart;
      Dumpln(Source);
      PreformatEnd;
      end;
    if (Source='') then
      DumpLn('<P>No Source in TestSuite DataBase.</P>');
    lURL:=GetVersionControlURL;
    HeaderStart(3);
    DumpLn('Link to Git view of '+
         '<A HREF="'+lURL+lFn+'?view=markup'+
         '" TARGET="fpc_source"> '+lFN+'</A> source. ');
    HeaderEnd(3);
    end;
end;

procedure TTestSuite.ShowRunComparison;

Var
  Qry : String;
  Q : TSQLQuery;
  FL : String;
  lTable : TTableProducer;

begin
  Response.ContentType:='text/html';
//  EmitContentType;
  With FHTMLWriter do
    begin
    EmitDocType;
    EmitTitle(Title+' : Compare 2 runs');
    HeaderStart(1);
    DumpLn(Format('Test suite results for run %d vs. %d',[FVars.RunID,FVars.CompareRunID]));
    HeaderEnd(1);
    HeaderStart(2);
    DumpLn('Test run data: ');
    HeaderEnd(2);
    If Not ShowRunData then
      begin
      DumpLn(Format('No data for test run with ID: %d',[FVars.RunID]));
      exit;
      end;
    HeaderStart(2);
    DumpLn('ShowRunComparison detailed test run results:');
    FL:='';
    If FVars.OnlyFailed or FVars.NoSkipped then
      begin
      FL:='';
      If FVars.OnlyFailed then
        FL:='successful';
      if FVars.NoSkipped then
        begin
        If (FL<>'') then
          FL:=FL+' and ';
        FL:=FL+'skipped';
        end;
      DumpLn(' ('+FL+' tests are hidden)');
      end;
    HeaderEnd(2);
    ParaGraphStart;
    end;
  Qry:=FConstructSQL.GetCompareRunSQL;
  If FVars.Debug then
    begin
    system.WriteLn('Query: '+Qry);
    system.Flush(stdout);
    end;
  FRunStats:=Default(TRunStats);
  Q:=FSQL.CreateQuery(Qry);
  try
    Q.Open;
    FL:='Id,Filename,Run1_OK,Run2_OK';
    If Not FVars.NoSkipped then
      FL:=FL+',Run1_Skipped,Run2_Skipped';
    FL:=FL+',Run1_Result,Run2_Result';
    lTable:=FHTMLWriter.CreateTableProducer(Q);
    lTable.Border:=True;
    lTable.CreateColumns(FL);
    lTable.OnGetRowAttributes:=@GetRunRowAttr;
    With lTable.TableColumns do
      begin
      ColumnByName('Id').OnGetCellContents:=@FHTMLWriter.FormatFileIDDetails;
      ColumnByName('Run1_Result').OnGetCellContents:=@FHTMLWriter.FormatTestResult;
      ColumnByName('Run2_Result').OnGetCellContents:=@FHTMLWriter.FormatTestResult;
      ColumnByName('Filename').OnGetCellContents:=@FHTMLWriter.FormatFileDetails;
      end;
    //(TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
    lTable.CreateTable(FContent); // Response);
    FHTMLWriter.DumpLn(format('<p>Record count: %d</P>',[Q.RecordCount]));
  finally
    lTable.Free;
    Q.Free;
  end;
  If Not (FRunStats.OKCount=0) and not (FVars.NoSkipped and FVars.OnlyFailed) then
    FHTMLWriter.EmitPieImage(FRunStats.OKCount,FRunStats.FailedCount,FRunStats.SkipCount);
end;

procedure TTestSuite.GetRunRowAttr(Sender: TObject; var BGColor: String;
  var Align: THTMLAlign; var VAlign: THTMLValign; var CustomAttr: String);

Var
  P : TTableProducer;
  Skip1Field, Skip2Field, Run1Field, Run2Field : TField;

begin
  P:=(Sender as TTableProducer);
  Inc(FRunStats.OKCount);
  If (FVars.OnlyFailed and FVars.NoSkipped) then
    begin
    If (P.CurrentRow Mod 2)=0 then
      BGColor:='#EEEEEE'
    end
  else
    begin
    Skip1Field := P.Dataset.FindField('Skipped');
    if Skip1Field = nil then
      begin
      Skip1Field := P.Dataset.FindField('Run1_Skipped');
      Skip2Field := P.Dataset.FindField('Run2_Skipped');
      end
    else
      Skip2Field := nil;
    Run1Field := P.Dataset.FindField('OK');
    if Run1Field = nil then
      Run1Field := P.Dataset.FindField('Run1_OK');
    Run2Field := P.Dataset.FindField('OK');
    if Run2Field = nil then
      Run2Field := P.Dataset.FindField('Run2_OK');
    If (not FVars.NoSkipped) and ((Skip1Field.AsBoolean)
        or ((Skip2Field <> nil) and (Skip2Field.AsBoolean))) then
      begin
      Inc(FRunStats.SkipCount);
      BGColor:='yellow';    // Yellow
      end
    else If Run2Field.AsBoolean then
      begin
      if Run1Field.AsString='' then
        BGColor:='#68DFB8'
      else if Run1Field.AsBoolean then
        BGColor:='#98FB98';    // pale Green
      end
    else if Not Run2Field.AsBoolean then
      begin
      Inc(FRunStats.FailedCount);
      if Run1Field.AsString='' then
        BGColor:='#FF82AB'    // Light red
      else if Not Run1Field.AsBoolean then
        BGColor:='#FF225B';
      end;
    end;
end;

procedure TTestSuite.CreateRunPie;

Var
  lGraph : TTestSuiteGraph;

begin
  lGraph:=TTestSuiteGraph.Create(FVars);
  try
    If FVars.RunCount=0 Then
      Raise Exception.Create('Invalid parameters passed to script: No total count');
    lGraph.DrawPie(FContent,FVars.RunSkipCount,FVars.RunFailedCount,FVars.RunCount);
    Response.ContentType:='image/png';
    FContent.Position:=0;
  Finally
    lGraph.Free;
  end;
end;

begin
  ShortDateFormat:='yyyy/mm/dd';
end.
