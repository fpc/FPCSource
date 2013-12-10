{$mode objfpc}
{$h+}
unit utests;

interface

uses
     cgiapp,
     sysutils,mysql55conn,sqldb,whtml,dbwhtml,db,
     tresults,
     Classes,ftFont,fpimage,fpimgcanv,fpWritePng,fpcanvas;

const
  TestsuiteURLPrefix='http://www.freepascal.org/testsuite/';
  TestsuiteBin='testsuite.cgi';
  ViewURL='http://svn.freepascal.org/cgi-bin/viewvc.cgi/';
  TestsSubDir='/tests/';
  DataBaseSubDir='/packages/fcl-db/tests/';
var
  TestsuiteCGIURL : string;
Type
  TTestSuite = Class(TCgiApplication)
  Private
    FHTMLWriter : THtmlWriter;
    FComboBoxProducer : TComboBoxProducer;
    FDB : TSQLConnection;
    FTrans : TSQLTransaction;
    FRunID,
    FCompareRunID,
    FTestFileID,
    FTestFileName,
    FVersion,
    FVersionBranch,
    FCond,
    FSubmitter,
    FMachine,
    FComment,
    FCPU,
    FCategory,
    FOS  : String;
    FViewVCURL : String;
    FDate : TDateTime;
    FDebug,
    FListAll,
    FNoSkipped,
    FOnlyFailed : Boolean;
    FRunSkipCount,
    FRunFailedCount,
    FRunCount : Integer;
    FAction,
    FLimit : Integer;
    FTestLastDays : Integer;
    FNeedEnd : boolean;
    Procedure GetOverviewRowAttr(Sender : TObject; Var BGColor : String;
                                   Var Align : THTMLAlign; Var VAlign : THTMLValign;
                                   Var CustomAttr : String) ;
    Procedure GetRunRowAttr(Sender : TObject; Var BGColor : String;
                            Var Align : THTMLAlign; Var VAlign : THTMLValign;
                            Var CustomAttr : String) ;
    Procedure FormatFailedOverview(Sender : TObject; Var CellData : String);
    Procedure FormatTestRunOverview(Sender : TObject; Var CellData : String);
    Procedure FormatFileDetails(Sender: TObject; var CellData: String);
    Procedure FormatFileIDDetails(Sender: TObject; var CellData: String);
    Procedure FormatTestResult(Sender: TObject; var CellData: String);
    Function  FormatDetailURL(const RunIdStr, CellData : String) : string;

    Procedure DoDrawPie(Img : TFPCustomImage; Skipped,Failed,Total : Integer);
  Public
    Function CreateDataset(Qry : String) : TSQLQuery;
    Function CreateTableProducer(DS : TDataset) :TTableProducer;
    Procedure DefaultTableFromQuery(Qry,ALink : String; IncludeRecordCount : Boolean);
    Procedure ComboBoxFromQuery(Const ComboName,Qry : String);
    Procedure ComboBoxFromQuery(Const ComboName,Qry,Value : String);
    Function  GetSingleTon(Const Qry : String) : String;
    Function GetOSName(ID : String) : String;
    Function GetCPUName(ID : String) : String;
    Function GetVersionName(ID : String) : String;
    Function GetCategoryName(ID : String) : String;
    Function GetTestFileName(ID : String) : String;
    Function GetFailCount(RunID : longint) : string;
    Function InitCGIVars : Integer;
    Procedure DoRun; override;
    Procedure EmitOverviewForm;
    Procedure EmitHistoryForm;
    Procedure ShowRunResults;
    Procedure ShowRunComparison;
    Procedure ShowOneTest;
    Procedure ShowHistory;
    Function ConnectToDB : Boolean;
    procedure DisconnectFromDB;
    Procedure EmitTitle(ATitle : String);
    Procedure EmitEnd;
    Procedure ShowRunOverview;
    Procedure CreateRunPie;
    Function  ShowRunData : Boolean;

  end;

implementation


  uses
    dateutils;

Const
{$i utests.cfg}

{ if utests.cfg is missed, create one with the following contents:
  DefDatabase = 'TESTSUITE';
  DefHost     = '';
  DefDBUser   = ''; // fill this in when compiling.
  DefPassword = ''; // fill this in, too.
}

Const
  OldTestResultsTableName = 'OLDTESTRESULTS';
  NewTestResultsTableName = 'TESTRESULTS';
  LastOldTestRun = 91178;
  MaxLimit = 1000;

  Function TestResultsTableName(const RunId : String) : string;
  var
    RunIDVal : qword;
    Error : word;
  begin
    system.val (RunId,RunIdVal,error);
    if (error<>0) then
      result:='ErrorTable'
    else if (RunIdVal <= LastOldTestRun) then
      result:=OldTestResultsTableName
    else
      result:=NewTestResultsTableName;
  end;


Var
  SDetailsURL : string;

type
  known_versions = (
    ver_unknown,
    ver_1_0_10,
    ver_2_0_0,
    ver_2_0_1,
    ver_2_0_2,
    ver_2_0_3,
    ver_2_0_4,
    ver_2_0_5,
    ver_2_1_2,
    ver_2_1_4,
    ver_2_2_0,
    ver_2_2_1,
    ver_2_2_2,
    ver_2_2_3,
    ver_2_2_4,
    ver_2_2_5,
    ver_2_3_1,
    ver_2_4_0,
    ver_2_4_1,
    ver_2_4_2,
    ver_2_4_3,
    ver_2_4_4,
    ver_2_4_5,
    ver_2_5_1,
    ver_2_6_0,
    ver_2_6_1,
    ver_2_6_2,
    ver_2_6_3,
    ver_2_7_1);

const
  ver_trunk = high (known_versions);

const
  ver_string : array[known_versions] of string =
  (
   'unknown',
   '1.0.10',
   '2.0.0',
   '2.0.1',
   '2.0.2',
   '2.0.3',
   '2.0.4',
   '2.0.5',
   '2.1.2',
   '2.1.4',
   '2.2.0',
   '2.2.1',
   '2.2.2',
   '2.2.3',
   '2.2.4',
   '2.2.5',
   '2.3.1',
   '2.4.0',
   '2.4.1',
   '2.4.2',
   '2.4.3',
   '2.4.4',
   '2.4.5',
   '2.5.1',
   '2.6.0',
   '2.6.1',
   '2.6.2',
   '2.6.3',
   '2.7.1'
  );

  ver_branch : array [known_versions] of string =
  (
   '',
   '',
   'tags/release_2_0_0',
   'branches/fixes_2_0',
   'tags/release_2_0_2',
   'branches/fixes_2_0',
   'tags/release_2_0_4',
   'branches/fixes_2_0',
   'tags/release_2_1_2',
   'tags/release_2_1_4',
   'tags/release_2_2_0',
   'branches/fixes_2_2',
   'tags/release_2_2_2',
   'branches/fixes_2_2',
   'tags/release_2_2_4',
   'branches/fixes_2_2',
   'tags/release_2_4_0',
   'tags/release_2_4_0',
   'tags/release_2_4_2',
   'tags/release_2_4_2',
   'tags/release_2_4_4',
   'tags/release_2_4_4',
   'branches/fixes_2_4',
   'tags/release_2_6_0',
   'tags/release_2_6_0',
   'tags/release_2_6_2',
   'tags/release_2_6_2',
   'branches/fixes_2_6',
   'trunk'
  );

Procedure TTestSuite.DoRun;

begin
  Try
    Try
      Case InitCGIVars of
        0 : EmitOverviewForm;
        1 :
          if Length(FCompareRunID) = 0 then
            ShowRunResults
          else
            ShowRunComparison;
        2 : CreateRunPie;
        3 : ShowOneTest;
        4 : ShowHistory;
{$ifdef TEST}
        98 :
          begin
            ///EmitOverviewForm;
            system.Writeln(stdout,'<PRE>');
            system.Writeln(stdout,'paramstr(0) is ',paramstr(0));
            system.FreeMem(pointer($ffffffff));
            system.Writeln(stdout,'</PRE>');
            system.Flush(stdout);
          end;
        99 :
          begin
            EmitOverviewForm;
            system.Writeln(stdout,'<PRE>');
            system.Dump_stack(stdout,get_frame);
            system.Writeln(stdout,'</PRE>');
            system.Flush(stdout);
          end;
{$endif TEST}
        end;
    finally
      EmitEnd;
      DisConnectFromDB;
    end;
  Finally
    Terminate;
  end;
end;


Function TTestSuite.InitCGIVars : Integer;

Var
  S : String;

begin
  FHtmlWriter:=THTMLWriter.Create(Response);
  FComboBoxProducer:=TComboBoxProducer.Create(Self);
  DateSeparator:='/';
  Result:=0;
  S:=RequestVariables['action'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTACTION'];
  if S='View history' then
    FAction:=4
  else
    FAction:=StrToIntDef(S,0);
  S:=RequestVariables['limit'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTLIMIT'];
  FLimit:=StrToIntDef(S,50);
  if FLimit > MaxLimit then
    FLimit:=MaxLimit;
  FVersion:=RequestVariables['version'];
  if Length(FVersion) = 0 then
    FVersion:=RequestVariables['TESTVERSION'];

  FOS:=RequestVariables['os'];
  if Length(FOS) = 0 then
    FOS:=RequestVariables['TESTOS'];
  FCPU:=RequestVariables['cpu'];
  if Length(FCPU) = 0 then
    FCPU:=RequestVariables['TESTCPU'];
  FCategory:=RequestVariables['category'];
  if Length(FCategory) = 0 then
    FCategory:=RequestVariables['TESTCATEGORY'];
  FCond:=RequestVariables['cond'];
  if Length(FCond) = 0 then
    FCond:=RequestVariables['TESTCOND'];
  FComment:=RequestVariables['comment'];
  if Length(FComment) = 0 then
    FComment:=RequestVariables['TESTCOMMENT'];
  FSubmitter:=RequestVariables['submitter'];
  if Length(FSubmitter) = 0 then
    FSubmitter:=RequestVariables['TESTSUBMITTER'];
  FMachine:=RequestVariables['machine'];
  if Length(FMachine) = 0 then
    FMachine:=RequestVariables['TESTMACHINE'];

  FRunID:=RequestVariables['run1id'];
  if Length(FRunID) = 0 then
    FRunID:=RequestVariables['TESTRUN'];
  S:=RequestVariables['lastdays'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTLASTDAYS'];
  FTestLastDays:=StrToIntDef(S,31);
  S:=RequestVariables['date'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTDATE'];
  if Length(S) > 0 then
    try
      FDate:=StrToDate(S);
    except
      FDate:=0;
    end;
  S:=RequestVariables['failedonly'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTFAILEDONLY'];
  FOnlyFailed:=(S='1');
  S:=RequestVariables['noskipped'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTNOSKIPPED'];
  FNoSkipped:=(S='1');
  FCompareRunID:=RequestVariables['run2id'];
  FTestFileID:=RequestVariables['testfileid'];
  FTestFileName:=RequestVariables['testfilename'];
  FRunCount:=StrToIntDef(RequestVariables['PIETOTAL'],0);
  FRunSkipCount:=StrToIntDef(RequestVariables['PIESKIPPED'],0);
  FRunFailedCount:=StrToIntDef(RequestVariables['PIEFAILED'],0);
  S:=RequestVariables['DEBUGCGI'];
  FDebug:=(S='1');
  S:=RequestVariables['listall'];
  FListAll:=(S='1');
  Result:=FAction;
end;

Function TTestSuite.ConnectToDB : Boolean;

begin
  Result:=False;
  FDB:=TMySQl55Connection.Create(Self);
  FDB.HostName:=DefHost;
  FDB.DatabaseName:=DefDatabase;
  FDB.UserName:=DefDBUser;
  FDB.Password:=DefPassword;
  FTrans := TSQLTransaction.Create(nil);
  FTrans.DataBase := FDB;
  FDB.Transaction := FTrans;
  FDB.Connected:=True;
  Result:=True;
end;

procedure TTestSuite.DisconnectFromDB;

begin
  If Assigned(FDB) then
    begin
    if (FDB.Connected) then
      FDB.Connected:=False;
    FreeAndNil(FDB);
    FreeAndNil(FTrans);
    end;
end;

Procedure TTestSuite.ComboBoxFromQuery(Const ComboName,Qry: String);

begin
  ComboBoxFromQuery(ComboName,Qry,'')
end;

Procedure TTestSuite.ComboBoxFromQuery(Const ComboName,Qry,Value : String);

Var
  Q : TSQLQuery;

begin
  Q:=TSQLQuery.Create(Self);
  try
    Q.Database:=FDB;
    Q.Transaction:=FTrans;
    Q.SQL.Text:=Qry;
    Q.Open;
    FComboboxProducer.Dataset:=Q;
    FComboBoxProducer.ValueField:=Q.Fields[0].FieldName;
    FComboBoxProducer.DataField:=Q.Fields[1].FieldName;
    FComboBoxProducer.Value:=Value;
    FComboBoxProducer.InputName:=ComboName;
    FComboBoxProducer.CreateComboBox(Response);
  Finally
    Q.Free;
  end;
end;

Function TTestSuite.GetSingleton(Const Qry : String) : String;

Var
  Q : TSQLQuery;

begin
  Result:='';
  if FDEbug then
    begin
      system.Writeln('Query=',Qry);
      system.flush(output);
    end;
  Q:=TSQLQuery.Create(Self);
  try
    Q.Database:=FDB;
    Q.Transaction:=FTrans;
    Q.SQL.Text:=Qry;
    Q.Open;
    Try
      if FDebug and (Q.FieldCount<>1) then
        begin
          system.Writeln('GetSingleton number of fields is not 1, but ',
            Q.FieldCount);
          system.flush(output);
        end;
      If Not (Q.EOF and Q.BOF) then
        Result:=Q.Fields[0].AsString;
    Finally
      Q.Close;
    end;
  finally
    Q.Free;
  end;
end;
Procedure TTestSuite.EmitTitle(ATitle : String);

begin
  AddResponseLn('<HTML>');
  AddResponseLn('<TITLE>'+ATitle+'</TITLE>');
  AddResponseLn('<BODY>');
  FNeedEnd:=true;
end;

Procedure TTestSuite.EmitOverviewForm;

begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  EmitTitle(Title);
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('View Test suite results');
    HeaderEnd(1);
    Write('Please specify search criteria:');
    ParagraphStart;
    FormStart(TestsuiteCGIURL,'');
    if FDebug then
      EmitHiddenVar('DEBUGCGI', '1');

    TableStart(2,true);
    RowStart;
      CellStart;
        Write('Operating system:');
      CellNext;
        ComboBoxFromQuery('os','SELECT TO_ID,TO_NAME FROM TESTOS ORDER BY TO_NAME',FOS);
      CellEnd;
    RowNext;
      CellStart;
        Write('Processor:');
      CellNext;
        ComboBoxFromQuery('cpu','SELECT TC_ID,TC_NAME FROM TESTCPU ORDER BY TC_NAME',FCPU);
      CellEnd;
    RowNext;
      CellStart;
        Write('Version');
      CellNext;
        ComboBoxFromQuery('version','SELECT TV_ID,TV_VERSION FROM TESTVERSION ORDER BY TV_VERSION DESC',FVERSION);
      CellEnd;
    RowNext;
      CellStart;
        Write('Date');
      CellNext;
        If (FDate=0) then
          EmitInput('date','')
        else
          EmitInput('date',DateToStr(FDate));
      CellEnd;
    //if FDebug then
      begin
        RowNext;
        CellStart;
        Write('Submitter');
        CellNext;
        If (FSubmitter='') then
          EmitInput('submitter','')
        else
          EmitInput('submitter',FSubmitter);
        CellEnd;
       RowNext;
        CellStart;
        Write('Machine');
        CellNext;
        If (FMachine='') then
          EmitInput('machine','')
        else
          EmitInput('machine',FMachine);
        CellEnd;
        RowNext;
        CellStart;
        Write('Comment');
        CellNext;
        If (FComment='') then
          EmitInput('comment','')
        else
          EmitInput('comment',FComment);
        CellEnd;

        RowNext;
        CellStart;
        Write('Cond');
        CellNext;
        If (FCond='') then
          EmitInput('cond','')
        else
          EmitInput('cond',FCond);
        CellEnd;
      end;
    RowNext;
      CellStart;
        Write('Category');
      CellNext;
        ComboBoxFromQuery('Category','SELECT TCAT_ID,TCAT_NAME FROM TESTCATEGORY ORDER BY TCAT_NAME',FCategory);
      CellEnd;
    RowNext;
      CellStart;
        Write('Only failed tests');
      CellNext;
        EmitCheckBox('failedonly','1',FonlyFailed);
      CellEnd;
    RowNext;
      CellStart;
        Write('Hide skipped tests');
      CellNext;
        EmitCheckBox('noskipped','1',FNoSkipped);
      CellEnd;
    RowEnd;
    TableEnd;
    ParaGraphStart;
    EmitSubmitButton('','Search');
    EmitSubmitButton('action','View history');

    EmitResetButton('','Reset form');
    FormEnd;
    end;
//  ShowRunOverview;
end;

Procedure TTestSuite.EmitHistoryForm;

begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  EmitTitle(Title);
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('View Test suite results');
    HeaderEnd(1);
    Write('Please specify search criteria:');
    ParagraphStart;
    FormStart(TestsuiteCGIURL,'');
    if FDebug then
      EmitHiddenVar('DEBUGCGI', '1');
    EmitHiddenVar('action','4');
    TableStart(2,true);
    RowStart;
      CellStart;
        Write('File:');
      CellNext;
        EmitInput('testfilename',FTestfilename);
      CellEnd;
    RowNext;
    (*   CellStart;
        Write('FileID:');
      CellNext;
        EmitInput('testfileid',FTestfileid);
      CellEnd;
    RowNext; *)

      CellStart;
        Write('Operating system:');
      CellNext;
        ComboBoxFromQuery('os','SELECT TO_ID,TO_NAME FROM TESTOS ORDER BY TO_NAME',FOS);
      CellEnd;
    RowNext;
      CellStart;
        Write('Processor:');
      CellNext;
        ComboBoxFromQuery('cpu','SELECT TC_ID,TC_NAME FROM TESTCPU ORDER BY TC_NAME',FCPU);
      CellEnd;
    RowNext;
      CellStart;
        Write('Version');
      CellNext;
        ComboBoxFromQuery('version','SELECT TV_ID,TV_VERSION FROM TESTVERSION ORDER BY TV_VERSION DESC',FVERSION);
      CellEnd;
    RowNext;
      CellStart;
        Write('Date');
      CellNext;
        If (FDate=0) then
          EmitInput('date','')
        else
          EmitInput('date',DateToStr(FDate));
      CellEnd;
        RowNext;
        CellStart;
        Write('Submitter');
        CellNext;
        If (FSubmitter='') then
          EmitInput('submitter','')
        else
          EmitInput('submitter',FSubmitter);
        CellEnd;
       RowNext;
        CellStart;
        Write('Machine');
        CellNext;
        If (FMachine='') then
          EmitInput('machine','')
        else
          EmitInput('machine',FMachine);
        CellEnd;
        RowNext;
        CellStart;
        Write('Comment');
        CellNext;
        If (FComment='') then
          EmitInput('comment','')
        else
          EmitInput('comment',FComment);
        CellEnd;
        RowNext;
        CellStart;
        Write('Limit');
        CellNext;
        EmitInput('limit',IntToStr(FLimit));
        CellEnd;

        RowNext;
        CellStart;
        Write('Cond');
        CellNext;
        If (FCond='') then
          EmitInput('cond','')
        else
          EmitInput('cond',FCond);
        CellEnd;
    RowNext;
      CellStart;
        Write('Category');
      CellNext;
        ComboBoxFromQuery('Category','SELECT TCAT_ID,TCAT_NAME FROM TESTCATEGORY ORDER BY TCAT_NAME',FCategory);
      CellEnd;
    RowNext;
      CellStart;
        Write('Only failed tests');
      CellNext;
        EmitCheckBox('failedonly','1',FonlyFailed);
      CellEnd;
    RowNext;
      CellStart;
        Write('Hide skipped tests');
      CellNext;
        EmitCheckBox('noskipped','1',FNoSkipped);
      CellEnd;
    RowNext;
      CellStart;
        Write('List all tests');
      CellNext;
        EmitCheckBox('listall','1',FListAll);
      CellEnd;

    RowEnd;
    TableEnd;
    ParaGraphStart;
    EmitSubmitButton('','Search');
    EmitResetButton('','Reset form');
    FormEnd;
    end;
end;


procedure TTestSuite.EmitEnd;
begin
  if not FNeedEnd then
    exit;
  AddResponseLn('</BODY>');
  AddResponseLn('</HTML>');
end;

procedure TTestSuite.GetOverviewRowAttr(Sender: TObject; var BGColor: String;
  var Align: THTMLAlign; var VAlign: THTMLValign; var CustomAttr: String);
begin
  If ((Sender as TTAbleProducer).CurrentRow mod 2=0) then
    BGColor:='#EEEEEE'
end;


Function TTestSuite.CreateDataset(Qry : String) : TSQLQuery;

begin
  Result:=TSQLQuery.Create(Self);
  With Result do
    begin
    Database:=FDB;
    Transaction := FTrans;
    SQL.Text:=Qry;
    end;
end;

Function TTestSuite.CreateTableProducer(DS : TDataset) :TTableProducer;

begin
  Result:=TTableProducer.Create(Self);
  Result.Dataset:=DS;
end;

Procedure TTestSuite.DefaultTableFromQuery(Qry,Alink : String; IncludeRecordCount : Boolean);

Var
  Q : TSQLQuery;

begin
  If FDebug then
    Writeln('Query : '+Qry);
  Q:=CreateDataset(Qry);
  With Q do
    try
      Open;
      Try
        With CreateTableProducer(Q) do
          Try
            Border:=True;
            If (Alink<>'') then
              begin
              CreateColumns(Nil);
              If TableColumns.Count>0 then
                (TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
              end;
            CreateTable(Response);
          Finally
            Free;
          end;
        If IncludeRecordCount then
          FHTMLWriter.DumpLn(Format('<p>Record count: %d </p>',[Q.RecordCount]));
      Finally
        Close;
      end;
    finally
      Free;
    end;
end;

Procedure TTestSuite.ShowRunOverview;
Const
  SOverview = 'SELECT TU_ID as ID,TU_DATE as Date,TC_NAME as CPU,TO_NAME as OS,'+
               'TV_VERSION as Version,count(*) as Count,'+
               'TU_SVNCOMPILERREVISION as SvnCompRev,'+
               'TU_SVNRTLREVISION as SvnRTLRev,'+
               'TU_SVNPACKAGESREVISION as SvnPackRev,TU_SVNTESTSREVISION as SvnTestsRev,'+
               '(TU_SUCCESSFULLYFAILED+TU_SUCCESFULLYCOMPILED+TU_SUCCESSFULLYRUN) AS OK,'+
               '(TU_FAILEDTOCOMPILE+TU_FAILEDTORUN+TU_FAILEDTOFAIL) as Failed,'+
               '(TU_SUCCESSFULLYFAILED+TU_SUCCESFULLYCOMPILED+TU_SUCCESSFULLYRUN+'+
                'TU_FAILEDTOCOMPILE+TU_FAILEDTORUN+TU_FAILEDTOFAIL) as Total,'+
               'TU_SUBMITTER as Submitter, TU_MACHINE as Machine, TU_COMMENT as Comment %s '+
              'FROM TESTRUN left join TESTCPU on (TC_ID=TU_CPU_FK) left join TESTOS on (TO_ID=TU_OS_FK) '+
              'left join TESTVERSION on (TV_ID=TU_VERSION_FK) left join TESTCATEGORY on (TCAT_ID=TU_CATEGORY_FK) '+
              'left join TESTRESULTS on (TR_TESTRUN_FK=TU_ID) '+
              '%s'+
              'GROUP BY TU_ID '+
              'ORDER BY TU_ID DESC LIMIT %d';


Var
  SC,S,A,Qry : String;
  Q : TSQLQuery;

begin
   S:='';
   If (FCPU<>'') and (GetCPUName(FCPU)<>'All') then
     S:=S+' AND (TU_CPU_FK='+FCPU+')';
   If (FCategory<>'') and (GetCategoryName(FCategory)<>'All') then
     S:=S+' AND (TU_CATEGORY_FK='+FCategory+')';
   If (FVersion<>'') and (GetVersionName(FVersion)<>'All')  then
     S:=S+' AND (TU_VERSION_FK='+FVERSION+')';
   if (FOS<>'') and (GetOSName(FOS)<>'All') then
     S:=S+' AND (TU_OS_FK='+FOS+')';
   If (Round(FDate)<>0) then
     S:=S+' AND (TU_DATE LIKE '''+FormatDateTime('YYYY-MM-DD',FDate)+'%'')';
   If FSubmitter<>'' then
     S:=S+' AND (TU_SUBMITTER='''+FSubmitter+''')';
   If FMachine<>'' then
     S:=S+' AND (TU_MACHINE='''+FMachine+''')';
   If FComment<>'' then
     S:=S+' AND (TU_COMMENT LIKE '''+Fcomment+''')';
   If FCond<>'' then
     S:=S+' AND ('+FCond+')';
   If FOnlyFailed then
     S:=S+' AND (TR_OK="-")';
   If GetCategoryName(FCategory)<>'DB' then
     SC:=', CONCAT(TU_SVNCOMPILERREVISION,"/",TU_SVNRTLREVISION,"/", '+
          'TU_SVNPACKAGESREVISION,"/",TU_SVNTESTSREVISION) as svnrev'
   else
     SC:='';
   If (FCategory='') or (GetCategoryName(FCategory)='All') then
     SC:=SC+', TCAT_NAME as Cat';

   A:=SDetailsURL;
   If FOnlyFailed then
     A:=A+'&failedonly=1';
   If FNoSkipped then
     A:=A+'&noskipped=1';
     
  if S <> '' then
  begin
    Delete(S, 1, 4);
    S:='WHERE '+ S + ' ';
  end;
  Qry:=Format(SOverview,[SC,S,FLimit]);
  If FDebug then
    Writeln('Query : '+Qry);
  Q:=CreateDataset(Qry);
  With Q do
    try
      Open;
      Try
        With CreateTableProducer(Q) do
          Try
            Border:=True;
            OnGetRowAttributes:=@GetOverViewRowAttr;
            CreateColumns(Nil);
            TableColumns.ColumnByName('ID').ActionURL:=A;
            TableColumns.ColumnByNAme('Failed').OnGetCellContents:=@FormatFailedOverview;
            CreateTable(Response);
          Finally
            Free;
          end;
        FHTMLWriter.DumpLn(Format('<p>Record count: %d</p>',[Q.RecordCount]));
      Finally
        Close;
      end;
    finally
      Free;
    end;
end;


Function TTestSuite.GetOSName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleTon('SELECT TO_NAME FROM TESTOS WHERE TO_ID='+ID);
end;

Function TTestSuite.GetTestFileName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleTon('SELECT T_NAME FROM TESTS WHERE T_ID='+ID);
end;

Function TTestsuite.GetFailCount(RunID : longint) : string;
begin
  if RunID<>0 then
    Result:=GetSingleTon('SELECT (TU_FAILEDTOCOMPILE + TU_FAILEDTOFAIL + TU_FAILEDTORUN) FROM TESTRUN WHERE TU_ID='+IntToStr(RunID));
end;

Function TTestSuite.GetCPUName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleTon('SELECT TC_NAME FROM TESTCPU WHERE TC_ID='+ID);
end;

Function TTestSuite.GetVersionName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleton('SELECT TV_VERSION FROM TESTVERSION WHERE TV_ID='+ID);
end;

Function TTestSuite.GetCategoryName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleton('SELECT TCAT_NAME FROM TESTCATEGORY WHERE TCAT_ID='+ID);
end;

Function TTestSuite.ShowRunData : Boolean;

Const
  SGetRunData = 'SELECT TU_ID,TU_DATE,TC_NAME,TO_NAME,' +
                'TU_SUBMITTER,TU_MACHINE,TU_COMMENT,TV_VERSION,'+
                'TU_CATEGORY_FK,TU_SVNCOMPILERREVISION,TU_SVNRTLREVISION,'+
                'TU_COMPILERDATE,'+
                'TU_SVNPACKAGESREVISION,TU_SVNTESTSREVISION,'+
               '(TU_SUCCESSFULLYFAILED+TU_SUCCESFULLYCOMPILED+TU_SUCCESSFULLYRUN) AS OK,'+
               '(TU_FAILEDTOCOMPILE+TU_FAILEDTORUN+TU_FAILEDTOFAIL) as Failed,'+
               '(TU_SUCCESSFULLYFAILED+TU_SUCCESFULLYCOMPILED+TU_SUCCESSFULLYRUN+'+
                'TU_FAILEDTOCOMPILE+TU_FAILEDTORUN+TU_FAILEDTOFAIL) as Total'+

                ' %s FROM TESTRUN,TESTCPU,TESTOS,TESTVERSION '+
                'WHERE '+
                ' (TC_ID=TU_CPU_FK) AND '+
                ' (TO_ID=TU_OS_FK) AND '+
                ' (TV_ID=TU_VERSION_FK) AND '+
                ' (TU_ID=%s)';


Var
  Q1,Q2 : TSQLQuery;
  F : TField;
  SC : string;
  Date1, Date2: TDateTime;
  CompilerDate1, CompilerDate2: TDateTime;
begin
  Result:=(FRunID<>'');
  If Result then
    begin
    If GetCategoryName(FCategory)<>'DB' then
      SC:=', CONCAT(TU_SVNCOMPILERREVISION,"/",TU_SVNRTLREVISION,"/", '+
          'TU_SVNPACKAGESREVISION,"/",TU_SVNTESTSREVISION) as svnrev'
    else
      SC:='';
    If GetCategoryName(FCategory)='All' then
      SC:=SC+', TCAT_NAME as Cat';

    Q1:=CreateDataset(Format(SGetRunData,[SC,FRunID]));
    if Length(FCompareRunID) > 0 then
      Q2:=CreateDataset(Format(SGetRunData,[SC,FCompareRunID]))
    else
      Q2:=nil;
    Try
      Q1.Open;
      if Q2 <> nil then
        Q2.Open;
      Result:=Not (Q1.EOF and Q1.BOF);
      If Result then
        With FHTMLWriter do
          begin
          FormStart(TestsuiteCGIURL,'get');
          EmitHiddenVar('action', '1');
          TableStart(3,true);
          RowStart;
            CellStart;
              Write('Run ID:');
            CellNext;
              EmitInput('run1id',FRunID);
            CellNext;
              EmitInput('run2id',FCompareRunID);
            CellEnd;
          RowNext;
            CellStart;
              Write('Operating system:');
            CellNext;
              Write(Q1.FieldByName('TO_NAME').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TO_NAME').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Processor:');
            CellNext;
              Write(Q1.FieldByName('TC_NAME').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TC_NAME').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Version:');
            CellNext;
              Write(Q1.FieldByNAme('TV_VERSION').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByNAme('TV_VERSION').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Fails/OK/Total:');
            CellNext;
              Write(Q1.FieldByName('Failed').AsString);
              Write('/'+Q1.FieldByName('OK').AsString);
              Write('/'+Q1.FieldByName('Total').AsString);
            CellNext;
              if Q2 <> nil then
                begin
                  Write(Q2.FieldByName('Failed').AsString);
                  Write('/'+Q2.FieldByName('Ok').AsString);
                  Write('/'+Q2.FieldByName('Total').AsString);
               end;
            CellEnd;

          RowNext;
            CellStart;
              Write('Comment:');
            CellNext;
              Write(Q1.FieldByName('TU_COMMENT').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TU_COMMENT').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Machine:');
            CellNext;
              Write(Q1.FieldByName('TU_MACHINE').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TU_MACHINE').AsString);
            CellEnd;
          if GetCategoryName(FCategory)<>'All' then
            begin
              RowNext;
                CellStart;
                Write('Category:');
                CellNext;
                Write(GetCategoryName(Q1.FieldByName('TU_CATEGORY_FK').AsString));
                CellNext;
                if Q2 <> nil then
                  Write(GetCategoryName(Q2.FieldByName('TU_CATEGORY_FK').AsString));
                CellEnd;
            end;
          If GetCategoryName(FCategory)<>'DB' then
            begin
              RowNext;
                CellStart;
                Write('SVN Revisions:');
                CellNext;
                Write(Q1.FieldByName('svnrev').AsString);
                CellNext;
                if Q2 <> nil then
                  Write(Q2.FieldByName('svnrev').AsString);
                CellEnd;
            end;
           RowNext;
            CellStart;
              Write('Submitter:');
            CellNext;
              Write(Q1.FieldByName('TU_SUBMITTER').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TU_SUBMITTER').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Date:');
            CellNext;
              F := Q1.FieldByName('TU_DATE');
              Date1 := F.AsDateTime;
              Write(F.AsString);
              F := Q1.FieldByName('TU_COMPILERDATE');
              Try
                CompilerDate1 := F.AsDateTime;
                if not SameDate(Date1,CompilerDate1) then
                  Write(' <> '+F.AsString);
              Except
                { Not a valid date, do nothing }
              end;
            CellNext;
              if Q2 <> nil then
                begin
                F := Q2.FieldByName('TU_DATE');
                Date2 := F.AsDateTime;
                Write(F.AsString);
                F := Q2.FieldByName('TU_COMPILERDATE');
                Try
                  CompilerDate2 := F.AsDateTime;
                  if not SameDate(Date2,CompilerDate2) then
                    Write(' <> '+F.AsString);
                Except
                  { Not a valid date, do nothing }
                end;
                end;
            CellEnd;
          RowEnd;
          TableEnd;
          ParagraphStart;
          EmitCheckBox('noskipped','1',FNoSkipped);
          Write(' Hide skipped tests');
	  ParagraphEnd;
	  ParagraphStart;
          EmitCheckBox('failedonly','1',FonlyFailed);
          Write(' Hide successful tests');
          ParagraphEnd;
          ParaGraphStart;
          EmitSubmitButton('','Show/Compare');
          EmitResetButton('','Reset form');
          ParagraphEnd;
          FormEnd;
          { give warning if dates reversed }
          if (Q2 <> nil) and (Date1 > Date2) then
            begin
            ParagraphStart;
            Write('Warning: testruns are not compared in chronological order.');
            ParagraphEnd;
            end;
          end;
    Finally
      Q1.Close;
      Q1.Free;
      if Q2 <> nil then
        begin
        Q2.Close;
        Q2.Free;
        end;
    end;
    end;
end;

Procedure TTestSuite.ShowRunResults;

Var
  S : String;
  Qry : String;
  Q : TSQLQuery;
  FL : String;

begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  EmitTitle(Title+' : Search Results');
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('Test suite results for run '+FRunID);
    HeaderEnd(1);
    HeaderStart(2);
    Write('Test run data : ');
    HeaderEnd(2);
    If ShowRunData then
      begin
      HeaderStart(2);
      Write('Detailed test run results:');

      FL:='';
      If FOnlyFailed or FNoSkipped then
        begin
        FL:='';
        If FOnlyFailed then
          FL:='successful';
        if FNoSkipped then
          begin
          If (FL<>'') then
            FL:=FL+' and ';
          FL:=FL+'skipped';
          end;
        Write(' ('+FL+' tests are hidden)');
        end;
      HeaderEnd(2);
      ParaGraphStart;
      S:='SELECT T_ID as Id,T_NAME as Filename,TR_SKIP as Skipped'
        +',TR_OK as OK,TR_RESULT as Result'
        +' FROM '+TESTRESULTSTableName(FRunID)+',TESTS'
        +' WHERE (TR_TEST_FK=T_ID) AND (TR_TESTRUN_FK='+FRunID+') ';

      If FOnlyFailed then
        S:=S+' AND (TR_OK="-")';
      If FNoSkipped then
        S:=S+' AND (TR_SKIP="-")';
      S:=S+' ORDER BY TR_ID ';
      Qry:=S;
      If FDebug then
        begin
        Writeln('Query : '+Qry);
        Flush(stdout);
      end;
      FRunCount:=0;
      FRunSkipCount:=0;
      FRunFailedCount:=0;
      Q:=CreateDataset(Qry);
      With Q do
        try
          Open;
          while not EOF do
            Next;
          RecNo:=0;

          DumpLn(Format('<p>Record count: %d </p>',[Q.RecordCount]));
          Try
            With CreateTableProducer(Q) do
              Try
                Border:=True;
                FL:='Id,Filename';
                If Not FNoSkipped then
                  FL:=FL+',Skipped';
                If Not FOnlyFailed then
                  FL:=FL+',OK';
                FL:=FL+',Result';
                CreateColumns(FL);
                OnGetRowAttributes:=@GetRunRowAttr;
                TableColumns.ColumnByNAme('Id').OnGetCellContents:=
                  @FormatFileIDDetails;

                TableColumns.ColumnByNAme('Filename').OnGetCellContents:=
                  @FormatFileDetails;
                TableColumns.ColumnByNAme('Result').OnGetCellContents:=
                  @FormatTestResult;
                //(TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
                CreateTable(Response);
              Finally
                Free;
              end;
          Finally
            Close;
          end;
        finally
          Free;
        end;
      If Not (FRunCount=0) and not (FNoSkipped or FOnlyFailed) then
        begin
        ParaGraphStart;
        TagStart('IMG',Format('Src="'+TestsuiteCGIURL+'?action=2&pietotal=%d&piefailed=%d&pieskipped=%d"',[FRunCount,FRunFailedCount,FRunSkipCount]));
        end;
      end
    else
      Write('No data for test run with ID: '+FRunID);
    end;
end;

Procedure TTestSuite.ShowOneTest;

Var
  S,S2 : String;
  Qry : String;
  Base, Category : string;
  Q : TSQLQuery;
  i : longint;
  FieldName,FieldValue,
  LLog,Source : String;
  Res : Boolean;
  ver : known_versions;
begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  if FTestFileID='' then
    FTestFileID:=GetSingleton('SELECT T_ID FROM TESTS WHERE T_NAME LIKE ''%'+
     FTestFileName+'%''');
  if FTestFileID<>'' then
    FTestFileName:=GetTestFileName(FTestFileID);
  EmitTitle(Title+' : File '+FTestFileName+' Results');
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('Test suite results for test file '+FTestFileName);
    HeaderEnd(1);
    HeaderStart(2);
    Write('Test run data : ');
    HeaderEnd(2);
    if FRunID<>'' then
      begin
        Res:=ShowRunData;
        Res:=true;
      end
    else
      begin
        // This is useless as it is now
        // It should be integrated into a form probably PM
        Write('Only failed tests');
        EmitCheckBox('failedonly','1',FonlyFailed);
        Write('Hide skipped tests');
        EmitCheckBox('noskipped','1',FNoSkipped);
        Res:=true;
      end;
    If Res then
      begin
      HeaderStart(2);
      Write('Test file "'+FTestFileName+'" information:');
      HeaderEnd(2);
      ParaGraphStart;
      if FTestFileID<>'' then
        S:='SELECT * FROM TESTS WHERE T_ID='+FTestFileID
      else
        S:='SELECT * FROM TESTS WHERE T_NAME='+FTestFileName;
      Q:=CreateDataSet(S);
      With Q do
        Try
          Open;
          Try
            For i:=0 to FieldCount-1 do
              begin
                FieldValue:=Fields[i].AsString;
                FieldName:=Fields[i].DisplayName;

                if (FieldValue<>'') and (FieldValue<>'-') and
                   (FieldName<>'T_NAME') and (FieldName<>'T_SOURCE') then
                  begin
                    if (FieldValue='+') then
                      Write('Flag ');
                    Write(FieldName);
                    Write(' ');
                    if FieldValue='+' then
                      Write(' set')
                    else
                      Write(FieldValue);
                    DumpLn('<BR>');
                  end;
              end;

          Finally
            Close;
          end;
        Finally
          Free;
        end;
      ParaGraphEnd;
      HeaderStart(2);
      Write('Detailed test run results:');

      HeaderEnd(2);
      ParaGraphStart;
      S:='SELECT TR_ID,TR_TESTRUN_FK AS RUN,TR_TEST_FK,TR_OK, TR_SKIP,TR_RESULT '
      //S:='SELECT * '
        +' FROM '+TESTRESULTSTableName(FRunID)
        +' WHERE  (TR_TEST_FK='+FTestFileID+')';
      If FOnlyFailed then
        S:=S+' AND (TR_OK="-")';
      if Fcomparerunid<>'' then
        begin
          if TESTRESULTSTableName(FRunID)<>TESTRESULTSTableName(FCompareRunID) then
            begin
              S2:='SELECT TR_ID,TR_TESTRUN_FK AS RUN,TR_TEST_FK,TR_OK, TR_SKIP,TR_RESULT '
                  +' FROM '+TESTRESULTSTableName(FCompareRunID)
                  +' WHERE  (TR_TEST_FK='+FTestFileID+')';
              If FOnlyFailed then
                S2:=S2+' AND (TR_OK="-")';

              S:=S+' AND (TR_TESTRUN_FK='+Frunid+') UNION '+
                 S2+' AND (TR_TESTRUN_FK='+Fcomparerunid+')'
            end
          else
            S:=S+' AND ((TR_TESTRUN_FK='+Frunid+') OR '+
                 '(TR_TESTRUN_FK='+Fcomparerunid+'))'
        end
      else if Frunid<>'' then
        S:=S+' AND (TR_TESTRUN_FK='+Frunid+')'
      else
         S:=S+' ORDER BY TR_TESTRUN_FK DESC LIMIT '+IntToStr(FLimit);
      Qry:=S;
      If FDebug then
      begin
        Writeln('Query : '+Qry);
        Flush(stdout);
      end;
      FRunCount:=0;
      FRunSkipCount:=0;
      FRunFailedCount:=0;
      Q:=CreateDataset(Qry);
      With Q do
        try
          Open;
          Try
            With CreateTableProducer(Q) do
              Try
                Border:=True;
                //FL:='TR_ID,TR_TESTRUN_FK,T_NAME,T_CPU,T_VERSION';
                CreateColumns(Nil);
                TableColumns.Delete(TableColumns.ColumnByName('TR_TEST_FK').Index);
                TableColumns.ColumnByNAme('RUN').OnGetCellContents:=
                  @FormatTestRunOverview;
                //OnGetRowAttributes:=@GetRunRowAttr;
                TableColumns.ColumnByNAme('TR_RESULT').OnGetCellContents:=
                  @FormatTestResult;
                //(TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
                CreateTable(Response);
              Finally
                Free;
              end;
           DumpLn(Format('<p>Record count: %d </p>',[Q.RecordCount]));
          Finally
            Close;
          end;
        finally
          Free;
        end;
             //If FDebug then
            Category:='1';
            if FRunId<>'' then
              begin
                Category:=getsingleton('select TU_CATEGORY_FK from TESTRUN where TU_ID='+FRunId);
                FVersionBranch:=GetVersionName(getsingleton('select TU_VERSION_FK from TESTRUN where TU_ID='+fRunId));
                LLog:='';
                Try
                LLog:=getsingleton('select TR_LOG from TESTRESULTS where (TR_TEST_FK='+ftestfileid
                     +') and (TR_TESTRUN_FK='+frunid+')');
                if LLog<>'' then
                  begin
                    HeaderStart(2);
                    Write('Log of '+FRunId+':');
                    HeaderEnd(2);
                    PreformatStart;
                    system.Write(LLog);
                    system.flush(output);
                    PreformatEnd;
                  end;
                Finally
                  if LLog='' then
                    begin
                      HeaderStart(2);
                      Write('No log of '+FRunId+'.');
                      HeaderEnd(2);
                    end;
                end;
              end;
            if FCompareRunId<>'' then
              begin
                LLog:='';
                Try
                LLog:=getsingleton('select TR_LOG from TESTRESULTS where (TR_TEST_FK='+ftestfileid
                     +') and (TR_TESTRUN_FK='+fcomparerunid+')');
                if LLog<>'' then
                  begin
                    HeaderStart(2);
                    Write('Log of '+FCompareRunId+':');
                    HeaderEnd(2);
                    PreformatStart;
                    system.Write(LLog);
                    system.flush(output);
                    PreformatEnd;
                  end;
                Finally
                  if LLog='' then
                    begin
                      HeaderStart(2);
                      Write('No log of '+FCompareRunId+'.');
                      HeaderEnd(2);
                    end;
                end;
              end;
            if FDebug then
              Write('After Log.');
            Source:='';
            Try
            Source:=getsingleton('select T_SOURCE from TESTS where T_ID='+ftestfileid);
            if Source<>'' then
              begin
                HeaderStart(2);
                Write('Source:');
                HeaderEnd(2);
                PreformatStart;
                system.Write(Source);
                system.flush(output);
                PreformatEnd;
              end;
            Finally
            Base:='trunk';
            if  FVersionBranch<>'' then
              begin
                // Test all but last version, which is assumed to be trunk
                for ver:=low(known_versions) to pred(high(known_versions)) do
                  if VER_String[ver]=FVersionBranch then
                    begin
                      base:=ver_branch[ver];
                      break;
                    end;
              end;
            FViewVCURL:=ViewURL+Base;
            if Category='1' then
              FViewVCUrl:=FViewVCURL+TestsSubDir
            else
              begin
                FViewVCUrl:=FViewVCURL+DataBaseSubDir;
                // This assumes that type TAnyType is
                // defined in anytype.pas source PM
                if pos('/',FTestFileName)>0 then
                  FTestfilename:=lowercase(copy(FTestFilename,2,pos('/',FTestFilename)-2)+'.pas');
              end;
            if Source='' then
              begin
                HeaderStart(3);
                DumpLn('<P>No Source in TestSuite DataBase.</P>');
                DumpLn('Link to SVN view of '+
                  '<A HREF="'+FViewVCURL+FTestFileName+'?view=markup'+
                  '" TARGET="fpc_source"> '+FTestFileName+'</A> source. ');
                HeaderEnd(3);
              end
            else
              begin
                HeaderStart(3);
                DumpLn('Link to SVN view of '+
                  '<A HREF="'+FViewVCURL+FTestFileName+'?view=markup'+
                  '" TARGET="fpc_source"> '+FTestFileName+'</A> source. ');
                HeaderEnd(3);
              end;
            end;
             if FDebug then
              Write('After Source.');
    end
    else
      Write(Format('No data for test file with ID: %s',[FTestFileID]));

    end;
end;


Procedure TTestSuite.ShowHistory;
Const
  MaxCombo = 50;
Type
  StatusLongintArray = Array [TTestStatus] of longint;
  StatusDateTimeArray = Array [TTestStatus] of TDateTime;
  AStatusLA = Array[1..MaxCombo] of StatusLongintArray;
  AStatusDTA = Array[1..MaxCombo] of StatusDateTimeArray;
  PStatusLA = ^AStatusLA;
  PStatusDTA = ^AStatusDTA;
Var
  S,SS,FL,cpu,version,os : String;
  date : TDateTime;
  Qry : String;
  Base, Category : string;
  Q : TSQLQuery;
  i,run_id,os_id,version_id,cpu_id : longint;
  run_ind,os_ind,version_ind,cpu_ind,
  ok_ind,skip_ind,result_ind,date_ind : longint;
  os_size, cpu_size, version_size : longint;
  os_last, cpu_last, version_last : longint;
  error : word;
  OK_count, not_OK_count,resi,
  total_count, skip_count, not_skip_count : longint;
  TS : TTestStatus;
  result_count : StatusLongintArray;
  os_count,cpu_count,version_count: PStatusLA;
  first_date, last_date : array[TTestStatus] of TDateTime;
  first_date_id, last_date_id : array[TTestStatus] of longint;
  os_first_date, os_last_date,
  cpu_first_date, cpu_last_date,
  version_first_date, version_last_date : PStatusDTA;
  os_first_date_id, os_last_date_id,
  cpu_first_date_id, cpu_last_date_id,
  version_first_date_id, version_last_date_id : PStatusLA;
  FieldName,FieldValue,
  LLog,Source : String;
  Res : Boolean;
  ver : known_versions;
begin
  Res:=False;
  os_count:=nil;
  cpu_count:=nil;
  version_count:=nil;
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  if (FTestFileID='') and (FTestFileName<>'') then
  begin
    FTestFileID:=GetSingleton('SELECT T_ID FROM TESTS WHERE T_NAME LIKE ''%'+
     FTestFileName+'%''');
  end;
  if FTestFileID<>'' then
    FTestFileName:=GetTestFileName(FTestFileID);
  if FTestFileName<>'' then
    EmitTitle(Title+' : File '+FTestFileName+' Results')
  else
    EmitTitle(Title+' : History overview');
  With FHTMLWriter do
    begin
    if FTestFileName<>'' then
      begin
        HeaderStart(1);
        Write('Test suite results for test file '+FTestFileName);
        HeaderEnd(1);
        HeaderStart(2);
        Write('Test run data : ');
        HeaderEnd(2);
      end;
    if FRunID<>'' then
      begin
        Res:=ShowRunData;
        Res:=true;
      end
    else
      begin
        // This is useless as it is now
        // It should be integrated into a form probably PM
        //Write('Only failed tests');
        //EmitCheckBox('failedonly','1',FonlyFailed);
        //Write('Hide skipped tests');
        //EmitCheckBox('noskipped','1',FNoSkipped);
        Res:=true;
        EmitHistoryForm;
        if FTestFileID = '' then
          with FHTMLWriter do begin
            HeaderStart(2);
            if Trim(FTestFileName) <> '' then
              Write(Format('Error: No test files matching "%s" found.', [FTestFileName]))
            else
              Write('Error: Please specify a test file.');
            HeaderEnd(2);
            Res:=False;
          end;
      end;
    If Res then
      begin
        if (FTestFileName<>'') then
          begin
          HeaderStart(2);
          Write('Test file "'+FTestFileName+'" information:');
          HeaderEnd(2);
          ParaGraphStart;
          S:='SELECT * FROM TESTS WHERE T_ID='+FTestFileID;
          Q:=CreateDataSet(S);
          With Q do
            Try
              Open;
              Try
                For i:=0 to FieldCount-1 do
                  begin
                    FieldValue:=Fields[i].AsString;
                    FieldName:=Fields[i].DisplayName;
                    if (FieldValue<>'') and (FieldValue<>'-') and
                       (FieldName<>'T_NAME') and (FieldName<>'T_SOURCE') then
                      begin
                        if (FieldValue='+') then
                          Write('Flag ');
                        Write(FieldName);
                        Write(' ');
                        if FieldValue='+' then
                          Write(' set')
                        else
                          Write(FieldValue);
                        DumpLn('<BR>');
                      end;
                  end;

              Finally
                Close;
              end;
            Finally
              Free;
            end;
          ParaGraphEnd;
          HeaderStart(2);
          Write('Detailed test run results:');
        end;
      HeaderEnd(2);
      ParaGraphStart;
      SS:='SELECT TR_ID,TR_TESTRUN_FK AS Run,TR_TEST_FK,TR_OK AS OK'
        +', TR_SKIP As Skip,TR_RESULT  As Result'
      //S:='SELECT * '
        +',TC_NAME AS CPU, TV_VERSION AS Version, TO_NAME AS OS'
        +',TU_ID,TU_DATE AS Date,TU_SUBMITTER  AS Submitter'
        +',(TU_FAILEDTOCOMPILE + TU_FAILEDTOFAIL + TU_FAILEDTORUN) AS Fails'
        +',TU_MACHINE AS Machine,TU_COMMENT AS Comment'
        +',TU_COMPILERDATE As CompDate'
        +',TU_SVNTESTSREVISION AS Tests_rev'
        +',TU_SVNRTLREVISION AS RTL_rev'
        +',TU_SVNCOMPILERREVISION AS Compiler_rev'
        +',TU_SVNPACKAGESREVISION AS Packages_rev'
        +',TO_ID,TC_ID,TV_ID'
        +' FROM TESTRESULTS '
        +' LEFT JOIN TESTRUN ON  (TR_TESTRUN_FK=TU_ID)'
        +' LEFT JOIN TESTOS ON  (TU_OS_FK=TO_ID)'
        +' LEFT JOIN TESTCPU ON  (TU_CPU_FK=TC_ID)'
        +' LEFT JOIN TESTVERSION ON  (TU_VERSION_FK=TV_ID)';
      S:='';
      if FTestFileID<>'' then
        S:=S+' AND (TR_TEST_FK='+FTestFileID+')';
      if FRunID<>'' then
        S:=S+' AND (TR_TESTRUN_FK='+FRunID+')';
      If FOnlyFailed then
        S:=S+' AND (TR_OK="-")';
      If FNoSkipped then
        S:=S+' AND (TR_SKIP="-")';
      If FCond<>'' then
        S:=S+' AND ('+FCond+')';

      If (FCPU<>'') and (GetCPUName(FCPU)<>'All') then
        begin
          S:=S+' AND (TU_CPU_FK='+FCPU+')';
          cpu_size:=0;
        end
      else
        begin
          cpu_last:=StrToInt(GetSingleton('SELECT COUNT(*) FROM TESTCPU'));
          cpu_size:=Sizeof(StatusLongintArray)*(1+cpu_last);
          cpu_count:=GetMem(cpu_size);
          FillChar(cpu_count^,cpu_size,#0);
          cpu_first_date_id:=GetMem(cpu_size);
          FillChar(cpu_first_date_id^,cpu_size,#0);
          cpu_last_date_id:=GetMem(cpu_size);
          FillChar(cpu_last_date_id^,cpu_size,#0);
          cpu_first_date:=GetMem(cpu_last*SizeOf(StatusDateTimeArray));
          FillChar(cpu_first_date^,cpu_last*Sizeof(StatusDateTimeArray),#0);
          cpu_last_date:=GetMem(cpu_last*SizeOf(StatusDateTimeArray));
          FillChar(cpu_last_date^,cpu_last*Sizeof(StatusDateTimeArray),#0);
        end;
      If (FVersion<>'') and (GetVersionName(FVersion)<>'All')  then
        begin
          S:=S+' AND (TU_VERSION_FK='+FVERSION+')';
          version_size:=0;
        end
      else
        begin
          version_last:=StrToInt(GetSingleton('SELECT COUNT(*) FROM TESTVERSION'));
          version_size:=Sizeof(StatusLongintArray)*(1+version_last);
          version_count:=GetMem(version_size);
          FillChar(version_count^,version_size,#0);
          version_first_date_id:=GetMem(version_size);
          FillChar(version_first_date_id^,version_size,#0);
          version_last_date_id:=GetMem(version_size);
          FillChar(version_last_date_id^,version_size,#0);
          version_first_date:=GetMem(version_last*SizeOf(StatusDateTimeArray));
          FillChar(version_first_date^,version_last*Sizeof(StatusDateTimeArray),#0);
          version_last_date:=GetMem(version_last*SizeOf(StatusDateTimeArray));
          FillChar(version_last_date^,version_last*Sizeof(StatusDateTimeArray),#0);

        end;

      if (FOS<>'') and (GetOSName(FOS)<>'All') then
        begin
          S:=S+' AND (TU_OS_FK='+FOS+')';
          os_size:=0;
        end
      else
        begin
          os_last:=StrToInt(GetSingleton('SELECT COUNT(*) FROM TESTOS'));
          os_size:=Sizeof(StatusLongintArray)*(1+os_last);
          os_count:=GetMem(os_size);
          FillChar(os_count^,os_size,#0);
          os_first_date_id:=GetMem(os_size);
          FillChar(os_first_date_id^,os_size,#0);
          os_last_date_id:=GetMem(os_size);
          FillChar(os_last_date_id^,os_size,#0);
          os_first_date:=GetMem(os_last*SizeOf(StatusDateTimeArray));
          FillChar(os_first_date^,os_last*Sizeof(StatusDateTimeArray),#0);
          os_last_date:=GetMem(os_last*SizeOf(StatusDateTimeArray));
          FillChar(os_last_date^,os_last*Sizeof(StatusDateTimeArray),#0);
        end;

      If FSubmitter<>'' then
        S:=S+' AND (TU_SUBMITTER='''+FSubmitter+''')';
      If FMachine<>'' then
        S:=S+' AND (TU_MACHINE='''+FMachine+''')';
      If FComment<>'' then
        S:=S+' AND (TU_COMMENT LIKE '''+FComment+''')';
      if FDATE<>0 then
        S:=S+' AND (TU_DATE >= '''+FormatDateTime('YYYY-MM-DD',FDate)+''')';

      if S <> '' then
      begin
        Delete(S, 1, 4);
        S:=SS + ' WHERE '+ S;
      end
      else
        S:=SS;

      S:=S+' ORDER BY TR_ID DESC';
      if FDATE=0 then
        S:=S+' LIMIT '+IntToStr(FLimit)
      else
        S:=S+' LIMIT '+IntToStr(MaxLimit);
      Qry:=S;
      If FDebug then
      begin
        Writeln(system.stdout,'Query : '+Qry);
        system.Flush(system.stdout);
      end;
      FRunCount:=0;
      FRunSkipCount:=0;
      FRunFailedCount:=0;
      Q:=CreateDataset(Qry);
      With Q do
        try
          Open;

          while not EOF do
            Next;

          DumpLn(Format('<p>Record count: %d </p>',[Q.RecordCount]));
          if RecordCount>0 then
            RecNo:=0;

          Try
           { if FDebug then
             begin
               Writeln(stdout,'FieldKind=',Fields[0].FieldKind);
               Writeln(stdout,'DataType=',Fields[0].DataType);
               system.flush(stdout);
             end; }

          total_count:=0;
          OK_count:=0;
          not_OK_count:=0;
          skip_count:=0;
          not_skip_count:=0;
          fillchar(Result_Count,Sizeof(Result_count),#0);
          ok_ind:=FieldByName('OK').Index;
          skip_ind:=FieldBYName('SKIP').Index;
          result_ind:=FieldByName('Result').Index;
          cpu_ind:=FieldByName('TC_ID').Index;
          os_ind:=FieldByName('TO_ID').Index;
          version_ind:=FieldByName('TV_ID').Index;
          date_ind:=FieldByName('Date').Index;
          run_ind:=FieldByName('TU_ID').Index;
          For i:=0 to Q.RecordCount-1 do
            begin
              Q.RecNo:=i;
              inc(total_count);
              S:=Fields[ok_ind].AsString;

              if S='+' then
                inc(OK_count)
              else
                inc(not_OK_count);
              S:=Fields[skip_ind].AsString;
              if S='+' then
                inc(skip_count)
              else
                inc(not_skip_count);
              S:=Fields[result_ind].AsString;
              cpu:=Fields[cpu_ind].ASString;
              version:=Fields[version_ind].AsString;
              os:=Fields[os_ind].AsString;
              date:=Fields[date_ind].ASDateTime;
              os_id:=Fields[os_ind].AsLongint;
              cpu_id:=Fields[cpu_ind].AsLongint;
              version_id:=Fields[version_ind].AsLongint;
              system.val(S,resi,error);
              run_id:=Fields[run_ind].ASLongint;
              if (error=0) and (Resi>=longint(FirstStatus)) and
                 (Resi<=longint(LastStatus)) then
                begin
                  TS:=TTestStatus(Resi);
                  if Result_count[TS]=0 then
                    begin
                      first_date[TS]:=date;
                      last_date[TS]:=date;
                      first_date_id[TS]:=run_id;
                      last_date_id[TS]:=run_id;
                    end
                  else
                    begin
                      if (date>last_date[TS]) then
                        begin
                          last_date[TS]:=date;
                          last_date_id[TS]:=run_id;
                        end;
                      if date<first_date[TS] then
                        begin
                          first_date[TS]:=date;
                          first_date_id[TS]:=run_id;
                        end;
                    end;

                  inc(Result_count[TS]);
                  if assigned(cpu_count) and (cpu_id<=cpu_last) then
                    begin
                      if cpu_count^[cpu_id,TS]=0 then
                        begin
                          cpu_first_date^[cpu_id,TS]:=date;
                          cpu_last_date^[cpu_id,TS]:=date;
                          cpu_first_date_id^[cpu_id,TS]:=run_id;
                          cpu_last_date_id^[cpu_id,TS]:=run_id;
                        end
                      else
                        begin
                          if (date>cpu_last_date^[cpu_id,TS]) then
                            begin
                              cpu_last_date^[cpu_id,TS]:=date;
                              cpu_last_date_id^[cpu_id,TS]:=run_id;
                            end;
                          if date<cpu_first_date^[cpu_id,TS] then
                            begin
                              cpu_first_date^[cpu_id,TS]:=date;
                              cpu_first_date_id^[cpu_id,TS]:=run_id;
                            end;
                        end;
                      inc(cpu_count^[cpu_id,TS]);
                    end;
                  if assigned(os_count) and (os_id<=os_last) then
                    begin
                      if os_count^[os_id,TS]=0 then
                        begin
                          os_first_date^[os_id,TS]:=date;
                          os_last_date^[os_id,TS]:=date;
                          os_first_date_id^[os_id,TS]:=run_id;
                          os_last_date_id^[os_id,TS]:=run_id;
                        end
                      else
                        begin
                          if (date>os_last_date^[os_id,TS]) then
                            begin
                              os_last_date^[os_id,TS]:=date;
                              os_last_date_id^[os_id,TS]:=run_id;
                            end;
                          if date<os_first_date^[os_id,TS] then
                            begin
                              os_first_date^[os_id,TS]:=date;
                              os_first_date_id^[os_id,TS]:=run_id;
                            end;
                        end;
                      inc(os_count^[os_id,TS]);
                    end;
                  if assigned(version_count) and (version_id<=version_last) then
                    begin
                      if version_count^[version_id,TS]=0 then
                        begin
                          version_first_date^[version_id,TS]:=date;
                          version_last_date^[version_id,TS]:=date;
                          version_first_date_id^[version_id,TS]:=run_id;
                          version_last_date_id^[version_id,TS]:=run_id;
                        end
                      else
                        begin
                          if (date>version_last_date^[version_id,TS]) then
                            begin
                              version_last_date^[version_id,TS]:=date;
                              version_last_date_id^[version_id,TS]:=run_id;
                            end;
                          if date<version_first_date^[version_id,TS] then
                            begin
                              version_first_date^[version_id,TS]:=date;
                              version_first_date_id^[version_id,TS]:=run_id;
                            end;
                        end;
                      inc(version_count^[version_id,TS]);
                    end;
                end
              else if Fdebug then
                writeln(stdout,'Error for Result, S=',S);
            end;
          DumpLn(Format('<p>Total = %d </p>',[total_count]));
          if Total_count > 0 then
            DumpLn(Format('<p>OK=%d Percentage= %3.2f </p>',[OK_count,OK_count*100/total_count]));
          if Skip_count > 0 then
            DumpLn(Format('<p>Skipped=%d Percentage= %3.2f </p>',[Skip_count,Skip_count*100/total_count]));
          if total_count>0 then
            begin
              TableStart(5,True);
              RowStart;
              CellStart;
              Write('Result type');
              CellNext;
              Write('Cat.');
              CellNext;
              Write('Count');
              CellNext;
              Write('Percentage');
              CellNext;
              Write('First date');
              CellNext;
              Write('Last Date');
              CellEnd;
            end;
          For TS:=FirstStatus to LastStatus do
            if Result_count[TS]>0 then
              begin
                RowNext;
                CellStart;
                Write(StatusText[TS]);
                CellNext;
                CellNext;
                Write(Format('%d',[Result_count[TS]]));
                CellNext;
                Write(Format('%3.1f',[Result_count[TS]*100/total_count]));
                CellNext;

                DumpLn(FormatDetailURL(IntToStr(first_date_id[TS]),
                  DateTimeToStr(first_date[TS])));
                Write(' '+GetFailCount(first_date_id[TS]));
                CellNext;
                DumpLn(FormatDetailURL(IntToStr(last_date_id[TS]),
                  DateTimeToStr(last_date[TS])));
                Write(' '+GetFailCount(last_date_id[TS]));
                CellEnd;
                if assigned(cpu_count) then
                  begin
                    for i:=1 to cpu_last do
                      if cpu_count^[i,TS]>0 then
                        begin
                          RowNext;
                          CellStart;
                          CellNext;
                          Write(GetSingleton('SELECT TC_NAME FROM TESTCPU WHERE TC_ID='+IntToStr(i)));
                          CellNext;
                          Write(Format('%d',[cpu_count^[i,TS]]));
                          CellNext;
                          Write(Format('%3.1f',[cpu_count^[i,TS]*100/result_count[TS]]));
                          CellNext;
                          DumpLn(FormatDetailURL(IntToStr(cpu_first_date_id^[i,TS]),
                            DateTimeToStr(cpu_first_date^[i,TS])));
                          Write(' '+GetFailCount(cpu_first_date_id^[i,TS]));
                          CellNext;
                          DumpLn(FormatDetailURL(IntToStr(cpu_last_date_id^[i,TS]),
                            DateTimeToStr(cpu_last_date^[i,TS])));
                          Write(' '+GetFailCount(cpu_last_date_id^[i,TS]));
                          CellEnd;
                        end;
                  end;
                if assigned(os_count) then
                  begin
                    for i:=1 to os_last do
                      if os_count^[i,TS]>0 then
                        begin
                          RowNext;
                          CellStart;
                          CellNext;
                          Write(GetSingleton('SELECT TO_NAME FROM TESTOS WHERE TO_ID='+IntToStr(i)));
                          CellNext;
                          Write(Format('%d',[os_count^[i,TS]]));
                          CellNext;
                          Write(Format('%3.1f',[os_count^[i,TS]*100/result_count[TS]]));
                          CellNext;
                          DumpLn(FormatDetailURL(IntToStr(os_first_date_id^[i,TS]),
                            DateTimeToStr(os_first_date^[i,TS])));
                          Write(' '+GetFailCount(os_first_date_id^[i,TS]));
                          CellNext;
                          DumpLn(FormatDetailURL(IntToStr(os_last_date_id^[i,TS]),
                            DateTimeToStr(os_last_date^[i,TS])));
                          Write(' '+GetFailCount(os_last_date_id^[i,TS]));
                          CellEnd;

                        end;

                  end;

                if assigned(version_count) then
                  begin
                    for i:=1 to version_last do
                      if version_count^[i,TS]>0 then
                        begin
                          RowNext;
                          CellStart;
                          CellNext;
                          Write(GetSingleton('SELECT TV_VERSION FROM TESTVERSION WHERE TV_ID='+IntToStr(i)));
                          CellNext;
                          Write(Format('%d',[version_count^[i,TS]]));
                          CellNext;
                          Write(Format('%3.1f',[version_count^[i,TS]*100/result_count[TS]]));
                          CellNext;
                          DumpLn(FormatDetailURL(IntToStr(version_first_date_id^[i,TS]),
                            DateTimeToStr(version_first_date^[i,TS])));
                          Write(' '+GetFailCount(version_first_date_id^[i,TS]));
                          CellNext;
                          DumpLn(FormatDetailURL(IntToStr(version_last_date_id^[i,TS]),
                            DateTimeToStr(version_last_date^[i,TS])));
                          Write(' '+GetFailCount(version_last_date_id^[i,TS]));
                          CellEnd;

                        end;

                  end;

              end;
          if total_count>0 then
            begin
              TableEnd;
              RecNo:=0;
            end;
          If FDebug or FListAll then
           begin

            With CreateTableProducer(Q) do
              Try
                Border:=True;
                FL:='RUN,Date,OK,SKIP,Result';
                if FSubmitter='' then
                  FL:=FL+',Submitter';
                if FMachine='' then
                  FL:=FL+',Machine';
                if Fcomment='' then
                  FL:=FL+',Comment';
                if (FOS='') or (GetOSName(FOS)='All') then
                  FL:=FL+',OS';
                if (FCPU='') or (GetCPUName(FCPU)='All') then
                  FL:=FL+',CPU';
                if (FVersion='') or (GetVersionName(FVersion)='All') then
                  FL:=FL+',Version';
                FL:=FL+',Fails,CompDate';
                FL:=FL+',Tests_rev,RTL_rev,Compiler_rev,Packages_rev';
                CreateColumns(FL);
                //TableColumns.Delete(TableColumns.ColumnByName('TR_TEST_FK').Index);
                TableColumns.ColumnByNAme('RUN').OnGetCellContents:=
                  @FormatTestRunOverview;
                //OnGetRowAttributes:=@GetRunRowAttr;
                TableColumns.ColumnByNAme('Result').OnGetCellContents:=
                  @FormatTestResult;
                //(TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
                CreateTable(Response);
              Finally
                Free;
              end;
           end;

          Finally
            Close;
          end;
        finally
          Free;
        end;
             //If FDebug then
            Category:='1';
            if FRunId<>'' then
              begin
                Category:=getsingleton('select TU_CATEGORY_FK from TESTRUN where TU_ID='+FRunId);
                FVersionBranch:=GetVersionName(getsingleton('select TU_VERSION_FK from TESTRUN where TU_ID='+fRunId));
                LLog:='';
                Try
                LLog:=getsingleton('select TR_LOG from TESTRESULTS where (TR_TEST_FK='+ftestfileid
                     +') and (TR_TESTRUN_FK='+frunid+')');
                if LLog<>'' then
                  begin
                    HeaderStart(2);
                    Write('LLog of '+FRunId+':');
                    HeaderEnd(2);
                    PreformatStart;
                    system.Write(LLog);
                    system.flush(output);
                    PreformatEnd;
                  end;
                Finally
                  if LLog='' then
                    begin
                      HeaderStart(2);
                      Write('No log of '+FRunId+'.');
                      HeaderEnd(2);
                    end;
                end;
              end;
            if FCompareRunId<>'' then
              begin
                LLog:='';
                Try
                LLog:=getsingleton('select TR_LOG from TESTRESULTS where (TR_TEST_FK='+ftestfileid
                     +') and (TR_TESTRUN_FK='+fcomparerunid+')');
                if LLog<>'' then
                  begin
                    HeaderStart(2);
                    Write('Log of '+FCompareRunId+':');
                    HeaderEnd(2);
                    PreformatStart;
                    system.Write(LLog);
                    system.flush(output);
                    PreformatEnd;
                  end;
                Finally
                  if LLog='' then
                    begin
                      HeaderStart(2);
                      Write('No log of '+FCompareRunId+'.');
                      HeaderEnd(2);
                    end;
                end;
              end;
            if FDebug then
              Write('After log.');
            Source:='';
            Try
              if ftestfileid <> '' then
              begin
                Source:=getsingleton('select T_SOURCE from TESTS where T_ID='+ftestfileid);
                if Source<>'' then
                  begin
                    HeaderStart(2);
                    Write('Source:');
                    HeaderEnd(2);
                    PreformatStart;
                    system.Write(Source);
                    system.flush(output);
                    PreformatEnd;
                  end;
              end;
            Finally
            Base:='trunk';
            if  FVersionBranch<>'' then
              begin
                // Test all but last version, which is assumed to be trunk
                for ver:=low(known_versions) to pred(high(known_versions)) do
                  if ver_string[ver]=FVersionBranch then
                    begin
                      base:=ver_branch[ver];
                      break;
                    end;
              end;
            FViewVCURL:=ViewURL+Base;
            if Category='1' then
              FViewVCUrl:=FViewVCURL+TestsSubDir
            else
              begin
                FViewVCUrl:=FViewVCURL+DataBaseSubDir;
                // This assumes that type TAnyType is
                // defined in anytype.pas source PM
                if pos('/',FTestFileName)>0 then
                  FTestfilename:=lowercase(copy(FTestFilename,2,pos('/',FTestFilename)-2)+'.pas');
              end;
            if Source='' then
              begin
                HeaderStart(3);
                DumpLn('<P>No Source in TestSuite DataBase.</P>');
                DumpLn('Link to SVN view of '+
                  '<A HREF="'+FViewVCURL+FTestFileName+'?view=markup'+
                  '" TARGET="fpc_source"> '+FTestFileName+'</A> source. ');
                HeaderEnd(3);
              end
            else
              begin
                HeaderStart(3);
                DumpLn('Link to SVN view of '+
                  '<A HREF="'+FViewVCURL+FTestFileName+'?view=markup'+
                  '" TARGET="fpc_source"> '+FTestFileName+'</A> source. ');
                HeaderEnd(3);
              end;
            end;
             if FDebug then
              Write('After Source.');
    end;

    end;
  if assigned(os_count) then
    begin
      FreeMem(os_count);
      FreeMem(os_first_date);
      FreeMem(os_first_date_id);
      FreeMem(os_last_date);
      FreeMem(os_last_date_id);
    end;

  if assigned(cpu_count) then
    begin
      FreeMem(cpu_count);
      FreeMem(cpu_first_date);
      FreeMem(cpu_first_date_id);
      FreeMem(cpu_last_date);
      FreeMem(cpu_last_date_id);
    end;
  if assigned(version_count) then
    begin
      FreeMem(version_count);
      FreeMem(version_first_date);
      FreeMem(version_first_date_id);
      FreeMem(version_last_date);
      FreeMem(version_last_date_id);
    end;
end;

Procedure TTestSuite.ShowRunComparison;

Var
  S : String;
  Qry : String;
  Q : TSQLQuery;
  FL : String;

begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  EmitTitle(Title+' : Compare 2 runs');
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('Test suite results for run '+FRunID+' vs. '+FCompareRunID);
    HeaderEnd(1);
    HeaderStart(2);
    Write('Test run data: ');
    HeaderEnd(2);
    If ShowRunData then
      begin
      HeaderStart(2);
      Write('Detailed test run results:');

      FL:='';
      If FOnlyFailed or FNoSkipped then
        begin
        FL:='';
        If FOnlyFailed then
          FL:='successful';
        if FNoSkipped then
          begin
          If (FL<>'') then
            FL:=FL+' and ';
          FL:=FL+'skipped';
          end;
        Write(' ('+FL+' tests are hidden)');
        end;
      HeaderEnd(2);
      ParaGraphStart;
      Q:=CreateDataset('');
      Q.SQL.Text:='CREATE TEMPORARY TABLE tr1 like TESTRESULTS;';
      Q.ExecSQL;
      Q.SQL.Text:='CREATE TEMPORARY TABLE tr2 like TESTRESULTS;';
      Q.ExecSQL;
      Q.SQL.Text:='INSERT INTO tr1 SELECT * FROM '+TESTRESULTSTableName(FRunId)+

        ' WHERE TR_TESTRUN_FK='+FRunID+';';
      Q.ExecSQL;
      Q.SQL.Text:='INSERT INTO tr2 SELECT * FROM '+TESTRESULTSTableName(FCompareRunId)+
        ' WHERE TR_TESTRUN_FK='+FCompareRunID+';';
      Q.ExecSQL;
      S:='SELECT T_ID as Id,T_NAME as Filename,tr1.TR_SKIP as Run1_Skipped,'
         +'tr2.TR_SKIP as Run2_Skipped,tr1.TR_OK as Run1_OK,'
         +'tr2.TR_OK as Run2_OK, tr1.TR_Result as Run1_Result,'
         +'tr2.TR_RESULT as Run2_Result '
         +'FROM TESTS, tr2 LEFT JOIN tr1 USING (TR_TEST_FK) '
         +'WHERE ((tr1.TR_SKIP IS NULL) or'
         +' (tr2.TR_SKIP IS NULL) or '
         +' (%s (tr1.TR_Result<>tr2.TR_Result)))'
         +'and (T_ID=tr2.TR_TEST_FK)';
      If FNoSkipped then
        begin
        Qry:='(((tr1.TR_SKIP="+") and (tr2.TR_OK="-") and (tr2.TR_SKIP="-")) or '
           +'((tr1.TR_OK="-") and (tr1.TR_SKIP="-") and (tr2.TR_SKIP="+")) or '
           +'((tr1.TR_SKIP="-") and (tr2.TR_SKIP="-"))) and ';
        end
      else
        Qry:='';
      Qry:=Format(S,[Qry]);
      If FDebug then
        begin
        Writeln('Query: '+Qry);
        Flush(stdout);
        end;
      FRunCount:=0;
      FRunSkipCount:=0;
      FRunFailedCount:=0;
      Q.SQL.Text:=Qry;
      With Q do
        try
          Open;
          Try
            With CreateTableProducer(Q) do
              Try
                Border:=True;
                FL:='Filename,Run1_OK,Run2_OK';
                If Not FNoSkipped then
                  FL:=FL+',Run1_Skipped,Run2_Skipped';
                FL:=FL+',Run1_Result,Run2_Result';
                CreateColumns(FL);
                OnGetRowAttributes:=@GetRunRowAttr;
                TableColumns.ColumnByNAme('Run1_Result').OnGetCellContents:=
                  @FormatTestResult;
                TableColumns.ColumnByNAme('Run2_Result').OnGetCellContents:=
                  @FormatTestResult;
                TableColumns.ColumnByNAme('Filename').OnGetCellContents:=
                 @FormatFileDetails;
                //(TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
                CreateTable(Response);
              Finally
                Free;
              end;
            Writeln('<p>Record count: ',Q.RecordCount,'</p>');
          Finally
            Close;
          end;
        finally
          Free;
        end;
      If Not (FRunCount=0) and not (FNoSkipped or FOnlyFailed) then
        begin
        ParaGraphStart;
        TagStart('IMG',Format('Src="'+TestsuiteCGIURL+'?action=2&pietotal=%d&piefailed=%d&pieskipped=%d"',[FRunCount,FRunFailedCount,FRunSkipCount]));
        end;
      end
    else
      Write('No data for test run with ID: '+FRunID);
    end;
end;

procedure TTestSuite.GetRunRowAttr(Sender: TObject; var BGColor: String;
  var Align: THTMLAlign; var VAlign: THTMLValign; var CustomAttr: String);

Var
  P : TTableProducer;
  Skip1Field, Skip2Field, Run1Field, Run2Field : TField;
begin
  P:=(Sender as TTAbleProducer);
  Inc(FRunCount);
  If (FOnlyFailed and FNoSkipped) then
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
    If (not FNoSkipped) and ((Skip1Field.AsString='+')
        or ((Skip2Field <> nil) and (Skip2Field.AsString = '+'))) then
      begin
      Inc(FRunSkipCount);
      BGColor:='yellow';    // Yellow
      end
    else If Run2Field.AsString='+' then
      begin
      if Run1Field.AsString='' then
        BGColor:='#68DFB8'
      else if Run1Field.ASString<>'+' then
        BGColor:='#98FB98';    // pale Green
      end
    else if Run2Field.AsString='-' then
      begin
      Inc(FRunFailedCount);
      if Run1Field.AsString='' then
        BGColor:='#FF82AB'    // Light red
      else if Run1Field.AsString<>'-' then
        BGColor:='#FF225B';
      end;
    end;
end;

procedure TTestSuite.FormatFailedOverview(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  S:=Format(SDetailsURL,[P.DataSet.FieldByName('ID').AsString]);
  S:=S+'&failedonly=1&noskipped=1';
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;


function TTestSuite.FormatDetailURL(const RunIdStr, CellData : String) : string;
Var
  S : String;
begin
  S:=Format(SDetailsURL,[RunIdStr]);
  if FOnlyFailed then
    S:=S+'&failedonly=1';
  if FNoSkipped then
    S:=S+'&noskipped=1';
  FormatDetailURL:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;

procedure TTestSuite.FormatTestRunOverview(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  S:=Format(SDetailsURL,[P.DataSet.FieldByName('RUN').AsString]);
  if FOnlyFailed then
    S:=S+'&failedonly=1';
  if FNoSkipped then
    S:=S+'&noskipped=1';
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;

procedure TTestSuite.FormatFileIDDetails(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  if FVersion<>'' then
    S:=Format(TestSuiteCGIURL + '?action=4&version=%s&testfileid=%s',
       [FVersion,P.DataSet.FieldByName('Id').AsString])
  else
    S:=Format(TestSuiteCGIURL + '?action=4&testfileid=%s',
       [P.DataSet.FieldByName('Id').AsString]);
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;


procedure TTestSuite.FormatFileDetails(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  if FCompareRunID<>'' then
    S:=Format(TestSuiteCGIURL + '?action=3&run1id=%s&run2id=%s&testfileid=%s',
       [FRunID,FCompareRunID,P.DataSet.FieldByName('Id').AsString])
  else
    S:=Format(TestSuiteCGIURL + '?action=3&run1id=%s&testfileid=%s',
       [FRunID,P.DataSet.FieldByName('Id').AsString]);
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;

procedure TTestSuite.FormatTestResult(Sender: TObject; var CellData: String);

Var
  Res : longint;
  Error:word;
  TS : TTestStatus;
begin
  Val(CellData,Res,Error);
  if (Error=0) and (Res>=longint(FirstStatus)) and
     (Res<=longint(LastStatus)) then
    begin
      TS:=TTestStatus(Res);
      CellData:=StatusText[TS];
    end;
end;

Procedure TTestSuite.CreateRunPie;

Var
  I : TFPMemoryImage;
  M : TMemoryStream;

begin
  ftFont.InitEngine;
  FontMgr.SearchPath:='/usr/lib/X11/fonts/truetype';
  I:=TFPMemoryImage.Create(320,320);
  try
    If FRunCount=0 Then
      Raise Exception.Create('Invalid parameters passed to script: No total count');
    DoDrawPie(I,FRunSkipCount,FRunFailedCount,FRunCount);
    M:=TMemoryStream.Create;
    Try
      With TFPWriterPNG.Create do
        try
          UseAlpha:=True;
          ImageWrite(M,I);
        Finally
          Free;
        end;
      ContentType:='image/png';
      EmitContentType;
      M.Position:=0;
      Response.CopyFrom(M,M.Size);
    Finally
      M.Free;
    end;
  Finally
    I.Free;
  end;
end;

Procedure TTestSuite.DoDrawPie(Img : TFPCustomImage; Skipped,Failed,Total : Integer);

Var
  Cnv : TFPImageCanvas;
  W,H,FH,CR,ra : Integer;
  A1,A2,FR,SR,PR : Double;
  R : TRect;
  F : TFreeTypeFont;

  Procedure AddPie(X,Y,R : Integer; AStart,AStop : Double; Col : TFPColor);

  Var
    DX,Dy : Integer;

  begin
    DX:=Round(R*Cos(A1));
    DY:=Round(R*Sin(A1));
    Cnv.Line(X,Y,X+DX,Y-DY);
    DX:=Round(Ra*Cos(A2));
    DY:=Round(Ra*Sin(A2));
    Cnv.Line(X,Y,X+DX,Y-Dy);
    DX:=Round(R/2*Cos((A1+A2)/2));
    DY:=Round(R/2*Sin((A1+A2)/2));
    Cnv.Brush.FpColor:=Col;
    Cnv.FloodFill(X+DX,Y-DY);
  end;

  Function FractionAngle(F,T : Integer): Double;

  begin
    Result:=(2*Pi*(F/T))
  end;



begin
  F:=TFreeTypeFont.Create;
  With F do
    begin
    Name:='arial';
    FontIndex:=0;
    Size:=12;
    FPColor:=colred;
    AntiAliased:=False;
    Resolution:=96;
    end;
  if FDebug then
    Writeln(stdout,'Creating image');
  Cnv:=TFPImageCanvas.Create(Img);
  if FDebug then
    Writeln(stdout,'CNV=0x',hexstr(ptruint(cnv),16));

  if FDebug then
   Writeln(stdout,'Getting width and height');
  W:=Img.Width;
  H:=Img.Height;
  if FDebug then
    begin
      Writeln(stdout,'width=',W,' height=',H);
      //system.flush(stdout);
    end;
  // Writeln('Transparant');
  cnv.Brush.Style:=bsSolid;
  cnv.Brush.FPColor:=colTransparent;
  cnv.Pen.FPColor:=colWhite;
  Cnv.Rectangle(0,0,W,H);
  if FDEbug then
    Writeln(stdout,'Setting font');
  Cnv.Font:=F;
  if FDebug then
    Writeln(stdout,'Getting textwidth ');
  FH:=CNV.GetTextHeight('A');
  If FH=0 then
    FH:=14; // 3 * 14;
  if FDebug then
    writeln(stdout,'FH=',FH);
  Inc(FH,3);
  R.Top:=FH*4;
  R.Left:=0;
  R.Bottom:=H;
  CR:=H-(FH*4);
  If W>CR then
    R.Right:=CR
  else
    R.Right:=W;
  Ra:=CR div 2;
  if FDEbug then
    begin
      Writeln(stdout,'Setting pen color');
      system.flush(stdout);
    end;
  Cnv.Pen.FPColor:=colBlack;
  if FDebug then
    begin
      Writeln(stdout,'Palette size : ',Img.Palette.Count);
      Writeln(stdout,'Setting brush style');
      system.flush(stdout);
    end;
  cnv.brush.FPColor:=colRed;
//  cnv.pen.width:=1;
  // Writeln('Drawing ellipse');
  Cnv.Ellipse(R);
  if FDebug then
    begin
      Writeln(stdout,'Setting text');
      Writeln(stdout,'Palette size : ',Img.Palette.Count);
    end;

  cnv.font.FPColor:=colred;
  Inc(FH,4);
  FR:=Failed/Total;
  SR:=Skipped/Total;
  PR:=1-(FR+SR);
  Cnv.Textout(1,FH,Format('%d Failed (%3.1f%%)',[Failed,Fr*100]));
  // Writeln('Palette size : ',Img.Palette.Count);
  cnv.font.FPColor:=colYellow;
  Cnv.Textout(1,FH*2,Format('%d Skipped (%3.1f%%)',[Skipped,SR*100]));
  A1:=(Pi*2*(failed/total));
  A2:=A1+(Pi*2*(Skipped/Total));
  AddPie(Ra,R.Top+Ra,Ra,A1,A2,ColYellow);
  cnv.font.FPColor:=colGreen;
  // Writeln('Palette size : ',Img.Palette.Count);
  A1:=A2;
  A2:=A1+(Pi*2*((Total-(Skipped+Failed))/Total));
  Cnv.Textout(1,FH*3,Format('%d Passed (%3.1f%%',[Total-Skipped-Failed,PR*100]));
  AddPie(Ra,R.Top+Ra,Ra,A1,A2,ColGreen);
  // Writeln('Palette size : ',Img.Palette.Count);
  // Writeln('All done');
end;

begin
  if paramstr(0)<>'' then
    TestsuiteCGIURL:=TestsuiteURLPrefix+'cgi-bin/'+extractfilename(paramstr(0))
  else
    TestsuiteCGIURL:=TestsuiteURLPrefix+'cgi-bin/'+TestsuiteBin;

  SDetailsURL := TestsuiteCGIURL + '?action=1&run1id=%s';
  ShortDateFormat:='yyyy/mm/dd';
end.
