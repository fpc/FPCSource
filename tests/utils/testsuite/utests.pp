{$mode objfpc}
{$h+}
unit utests;

interface

uses cgiapp,sysutils,mysqlDB4,whtml,dbwhtml,db,
     Classes,ftFont,fpimage,fpimgcanv,fpWritePng,fpcanvas;

Type
  TTestSuite = Class(TCgiApplication)
  Private
    FHTMLWriter : THtmlWriter;
    FComboBoxProducer : TComboBoxProducer;
    FDB : TMySQLDatabase;
    FRunID,
    FCompareRunID,
    FVersion,
    FCPU,
    FOS  : String;
    FDate : TDateTime;
    FDebug,
    FNoSkipped,
    FOnlyFailed : Boolean;
    FRunSkipCount,
    FRunFailedCount,
    FRunCount : Integer;
    FAction : Integer;
    FTestLastDays : Integer;
    Procedure GetOverviewRowAttr(Sender : TObject; Var BGColor : String;
                                   Var Align : THTMLAlign; Var VAlign : THTMLValign;
                                   Var CustomAttr : String) ;
    Procedure GetRunRowAttr(Sender : TObject; Var BGColor : String;
                            Var Align : THTMLAlign; Var VAlign : THTMLValign;
                            Var CustomAttr : String) ;
    Procedure FormatFailedOverview(Sender : TObject; Var CellData : String);
    Procedure DoDrawPie(Img : TFPCustomImage; Skipped,Failed,Total : Integer);
  Public
    Function CreateDataset(Qry : String) : TMySQLDataset;
    Function CreateTableProducer(DS : TDataset) :TTableProducer;
    Procedure DefaultTableFromQuery(Qry,ALink : String; IncludeRecordCount : Boolean);
    Procedure ComboBoxFromQuery(Const ComboName,Qry : String);
    Procedure ComboBoxFromQuery(Const ComboName,Qry,Value : String);
    Function  GetSingleTon(Const Qry : String) : String;
    Function GetOSName(ID : String) : String;
    Function GetCPUName(ID : String) : String;
    Function GetVersionName(ID : String) : String;
    Function InitCGIVars : Integer;
    Procedure DoRun; override;
    Procedure EmitOverviewForm;
    Procedure ShowRunResults;
    Procedure ShowRunComparison;
    Function ConnectToDB : Boolean;
    procedure DisconnectFromDB;
    Procedure EmitTitle(ATitle : String);
    Procedure ShowRunOverview;
    Procedure CreateRunPie;
    Function  ShowRunData : Boolean;
    
  end;

implementation


Const
{$i utests.cfg}

{ if utests.cfg is missed, create one with the following contents:
  DefDatabase = 'TESTSUITE';
  DefHost     = '';
  DefDBUser   = ''; // fill this in when compiling.
  DefPassword = ''; // fill this in, too.
}

Const
  SDetailsURL = 'testsuite.cgi?action=1&run1id=%s';

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
      end;
    finally
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
  FAction:=StrToIntDef(S,0);
  FVersion:=RequestVariables['version'];
  if Length(FVersion) = 0 then
    FVersion:=RequestVariables['TESTVERSION'];
  FOS:=RequestVariables['os'];
  if Length(FOS) = 0 then
    FOS:=RequestVariables['TESTOS'];
  FCPU:=RequestVariables['cpu'];
  if Length(FCPU) = 0 then
    FCPU:=RequestVariables['TESTCPU'];
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
  FRunCount:=StrToIntDef(RequestVariables['PIETOTAL'],0);
  FRunSkipCount:=StrToIntDef(RequestVariables['PIESKIPPED'],0);
  FRunFailedCount:=StrToIntDef(RequestVariables['PIEFAILED'],0);
  S:=RequestVariables['DEBUGCGI'];
  FDebug:=(S='1');
  Result:=FAction;
end;

Function TTestSuite.ConnectToDB : Boolean;

begin
  Result:=False;
  FDB:=TMySQLDatabase.Create(Self);
  FDB.HostName:=DefHost;
  FDB.DatabaseName:=DefDatabase;
  FDB.UserName:=DefDBUser;
  FDB.Password:=DefPassword;
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
    end;
end;

Procedure TTestSuite.ComboBoxFromQuery(Const ComboName,Qry: String);

begin
  ComboBoxFromQuery(ComboName,Qry,'')
end;

Procedure TTestSuite.ComboBoxFromQuery(Const ComboName,Qry,Value : String);

Var
  Q : TMySQLDataset;

begin
  Q:=TMySQLDataset.Create(Self);
  try
    Q.Database:=FDB;
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
  Q : TMySQLDataset;

begin
  Result:='';
  Q:=TMySQLDataset.Create(Self);
  try
    Q.Database:=FDB;
    Q.SQL.Text:=Qry;
    Q.Open;
    Try
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
    FormStart('testsuite.cgi','');
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
    EmitResetButton('','Reset form');
    FormEnd;
    end;
  ShowRunOverview;
  AddResponseLn('</BODY>');
  AddResponseLn('</HTML>');
end;

procedure TTestSuite.GetOverviewRowAttr(Sender: TObject; var BGColor: String;
  var Align: THTMLAlign; var VAlign: THTMLValign; var CustomAttr: String);
begin
  If ((Sender as TTAbleProducer).CurrentRow mod 2=0) then
    BGColor:='#EEEEEE'
end;


Function TTestSuite.CreateDataset(Qry : String) : TMySQLDataset;

begin
  Result:=TMySQLdataset.Create(Self);
  With Result do
    begin
    Database:=FDB;
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
  Q : TMySQLDataset;

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
          Writeln('<p>Record count: ',Q.RecordCount,'</p>');
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
               'TV_VERSION as Version,COUNT(TR_ID) as Count,'+
               '(TU_SUCCESSFULLYFAILED+TU_SUCCESFULLYCOMPILED+TU_SUCCESSFULLYRUN) AS OK,'+
               '(TU_FAILEDTOCOMPILE+TU_FAILEDTORUN+TU_FAILEDTOFAIL) as Failed,'+              
               '(TU_SUCCESSFULLYFAILED+TU_SUCCESFULLYCOMPILED+TU_SUCCESSFULLYRUN+'+
                'TU_FAILEDTOCOMPILE+TU_FAILEDTORUN+TU_FAILEDTOFAIL) as Total,'+
               'TU_SUBMITTER as Submitter, TU_MACHINE as Machine, TU_COMMENT as Comment '+
              'FROM TESTRESULTS,TESTRUN,TESTCPU,TESTOS,TESTVERSION '+
              'WHERE '+
               '(TC_ID=TU_CPU_FK) AND '+
               '(TO_ID=TU_OS_FK) AND '+
               '(TV_ID=TU_VERSION_FK) AND '+
               '(TR_TESTRUN_FK=TU_ID) '+
               '%s '+
              'GROUP BY TU_ID '+
              'ORDER BY TU_ID DESC LIMIT 50';


Var
  S,A,Qry : String;
  Q : TMySQLDataset;

begin
   S:='';
   If (FCPU<>'') and (GetCPUName(FCPU)<>'All') then
     S:=S+' AND (TU_CPU_FK='+FCPU+')';
   If (FVersion<>'') and (GetVersionName(FVersion)<>'All')  then
     S:=S+' AND (TU_VERSION_FK='+FVERSION+')';
   if (FOS<>'') and (GetOSName(FOS)<>'All') then
     S:=S+' AND (TU_OS_FK='+FOS+')';
   If (Round(FDate)<>0) then
     S:=S+' AND (TU_DATE="'+FormatDateTime('YYYY/MM/DD',FDate)+'")';
   If FOnlyFailed then
     S:=S+' AND (TR_OK="-")';
   A:=SDetailsURL;
   If FOnlyFailed then
     A:=A+'&failedonly=1';
   If FNoSkipped then
     A:=A+'&noskipped=1';
  Qry:=Format(SOverview,[S]);
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
        Writeln('<p>Record count: ',Q.RecordCount,'</p>');
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

Function TTestSuite.ShowRunData : Boolean;

Const
  SGetRunData = 'SELECT TU_ID,TU_DATE,TC_NAME,TO_NAME,TU_COMMENT,TV_VERSION '+
                ' FROM TESTRUN,TESTCPU,TESTOS,TESTVERSION '+
                'WHERE '+
                ' (TC_ID=TU_CPU_FK) AND '+
                ' (TO_ID=TU_OS_FK) AND '+
                ' (TV_ID=TU_VERSION_FK) AND '+
                ' (TU_ID=%s)';


Var
  Q1,Q2 : TmYSQLDataset;
  F : TField;
  Date1, Date2: TDateTime;
begin
  Result:=(FRunID<>'');
  If Result then
    begin
    Q1:=CreateDataset(Format(SGetRunData,[FRunID]));
    if Length(FCompareRunID) > 0 then
      Q2:=CreateDataset(Format(SGetRunData,[FCompareRunID]))
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
          FormStart('testsuite.cgi','get');
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
              Write('Comment:');
            CellNext;
              Write(Q1.FieldByName('TU_COMMENT').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TU_COMMENT').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Date:');
            CellNext;
              F := Q1.FieldByName('TU_DATE');
              Date1 := F.AsDateTime;
              Write(F.AsString);
            CellNext;
              if Q2 <> nil then
                begin
                F := Q2.FieldByName('TU_DATE');
                Date2 := F.AsDateTime;
                Write(F.AsString);
                end;
            CellEnd;
          RowEnd;
          TableEnd;
          ParagraphStart;
          EmitCheckBox('noskipped','1',FNoSkipped);
          Write(' Hide skipped tests');
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
  Q : TMySQLDataset;
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
      S:='SELECT T_NAME as Test,T_FULLNAME as Filename,TR_SKIP as Skipped,TR_OK as OK'
        +' FROM TESTRESULTS,TESTS'
        +' WHERE (TR_TEST_FK=T_ID) AND (TR_TESTRUN_FK='+FRunID+') ';
      If FOnlyFailed then
        S:=S+' AND (TR_OK="-")';
      If FNoSkipped then
        S:=S+' AND (TR_SKIP="-")';
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
                FL:='Test,Filename';
                If Not FNoSkipped then
                  FL:=FL+',Skipped';
                If Not FOnlyFailed then
                  FL:=FL+',OK';
                CreateColumns(FL);
                OnGetRowAttributes:=@GetRunRowAttr;
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
        TagStart('IMG',Format('Src="testsuite.cgi?action=2&pietotal=%d&piefailed=%d&pieskipped=%d"',[FRunCount,FRunFailedCount,FRunSkipCount]));
        end;
      end
    else
      Write('No data for test run with ID: '+FRunID);
    end;
end;

Procedure TTestSuite.ShowRunComparison;

Var
  S : String;
  Qry : String;
  Q : TMySQLDataset;
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
      S:='SELECT T_NAME as Test,T_FULLNAME as FileName,tr1.TR_SKIP as Run1_Skipped,'
         +'tr2.TR_SKIP as Run2_Skipped,tr1.TR_OK as Run1_OK,tr2.TR_OK as Run2_OK '
        +'FROM TESTS,(select * from TESTRESULTS where TR_TESTRUN_FK='+FCompareRunID+') as tr2 '
         +'LEFT JOIN (select * from TESTRESULTS where TR_TESTRUN_FK='+FRunID+') as tr1 '
         +'USING (TR_TEST_FK) '
        +'WHERE ((tr1.TR_SKIP IS NULL) or (%s(tr1.TR_OK<>tr2.TR_OK))) and (T_ID=tr2.TR_TEST_FK)';
      If FNoSkipped then
        begin
        S:=S+' and (tr2.TR_SKIP<>"+")';
        Qry:='(tr1.TR_SKIP<>"+") and';
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
      Q:=CreateDataset(Qry);
      With Q do
        try
          Open;
          Try
            With CreateTableProducer(Q) do
              Try
                Border:=True;
                FL:='Test,FileName,Run1_OK,Run2_OK';
                If Not FNoSkipped then
                  FL:=FL+',Run1_Skipped,Run2_Skipped';
                CreateColumns(FL);
                OnGetRowAttributes:=@GetRunRowAttr;
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
        TagStart('IMG',Format('Src="testsuite.cgi?action=2&pietotal=%d&piefailed=%d&pieskipped=%d"',[FRunCount,FRunFailedCount,FRunSkipCount]));
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
      if Run1Field.AsString='' then
        BGColor:='#68DFB8'
      else
        BGColor:='#98FB98'    // pale Green
    else
      begin
      Inc(FRunFailedCount);
      if Run1Field.AsString='' then
        BGColor:='#FF82AB'    // Light red
      else
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
  // Writeln('Creating image');
  Cnv:=TFPImageCanvas.Create(Img);
  // Writeln('Getting width and height');
  W:=Img.Width;
  H:=Img.Height;
  // Writeln('Transparant');
  cnv.Brush.Style:=bsSolid;
  cnv.Brush.FPColor:=colTransparent;
  cnv.Pen.FPColor:=colWhite;
  Cnv.Rectangle(0,0,W,H);
  // Writeln('Setting font');
  Cnv.Font:=F;
  // Writeln('Getting textwidth ');
  FH:=CNV.GetTextHeight('A');
  If FH=0 then 
    FH:=14; // 3 * 14;
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
  // Writeln('Setting pen color');
  Cnv.Pen.FPColor:=colBlack;  
  // Writeln('Palette size : ',Img.Palette.Count);
  // Writeln('Setting brush style');
  cnv.brush.FPColor:=colRed;
//  cnv.pen.width:=1;
  // Writeln('Drawing ellipse');
  Cnv.Ellipse(R);
  // Writeln('Setting text');
  // Writeln('Palette size : ',Img.Palette.Count);

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


end.
