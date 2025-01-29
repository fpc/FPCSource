unit tshtml;

{$mode ObjFPC}
{$h+}
interface

uses
  Classes, SysUtils, wformat, dbwhtml, whtml, sqldb, tsdb, tsconsts, tssql, tsutils, tstypes;

var
  TestsuiteCGIURL : string;

Type

  { TTestSuiteHTMLWriter }

  TTestSuiteHTMLWriter = class(THTMLWriter)
  Private
    FNeedEnd : Boolean;
    FSQL : TTestSQL;
    FComboBoxProducer:TComboBoxProducer;
    FVars : TQueryData;
  Public
    constructor create(aStream : TStream; aSQL: TTestSQL; aVars : TQueryData); reintroduce;
    destructor destroy; override;
    // Create HTML from SQL
    Procedure ComboBoxFromQuery(Const ComboName,Qry : String);
    Procedure ComboBoxFromQuery(Const ComboName,Qry,Value : String);
    Procedure ComboBoxFromQuery(Const ComboName,Qry : String; Value : integer);
    function CreateTableProducer(DS: TSQLQuery): TTableProducer;
    procedure DefaultTableFromQuery(Qry, ALink: String; IncludeRecordCount: Boolean);
    // Formatting things
    function  FormatDetailURL(const RunIdStr, CellData: String): string;
    procedure FormatFailedOverview(Sender: TObject; var CellData: String);
    procedure FormatTestRunOverview(Sender: TObject; var CellData: String);
    procedure FormatSVN(Sender: TObject; var CellData: String);
    procedure FormatSVNData(var CellData: String);
    procedure FormatFileDetails(Sender: TObject; var CellData: String);
    procedure FormatFileIDDetails(Sender: TObject; var CellData: String);
    procedure FormatTestResult(Sender: TObject; var CellData: String);
    // reate Html
    procedure EmitHiddenVar(const Name: String; aValue: Int64); overload;
    procedure EmitDocType;
    procedure EmitTitle(ATitle: String);
    procedure EmitPieImage(aOKCount, aFailedCount, aSkipCount: integer);
    procedure EmitHistoryForm(aTitle: String);
    procedure EmitOverviewForm(aTitle: string);
    procedure DumpTestInfo(aInfo: TTestInfo);
    procedure EmitEnd;
    // In 3.2.2 the htmlwriter uses shortstring for Dumpln.
    // LDump cuts string into 255 char pieces and writes them one by one
    Procedure LDump(Const St : String);
    Procedure LDumpLn(Const St : String);
    procedure HandleVerbose(lvl: TVerboseLevel; const aMsg: String);
 end;

implementation

{ TTestSuiteHTMLWriter }

constructor TTestSuiteHTMLWriter.create(aStream: TStream; aSQL: TTestSQL; aVars: TQueryData);
begin
  Inherited Create(aStream);
  FSQL:=ASQL;
  FComboBoxProducer:=TComboBoxProducer.Create(Nil);
  FVars:=aVars;
end;

destructor TTestSuiteHTMLWriter.destroy;
begin
  FreeAndNil(FComboBoxProducer);
  inherited destroy;
end;

procedure TTestSuiteHTMLWriter.ComboBoxFromQuery(const ComboName, Qry: String);

begin
  ComboBoxFromQuery(ComboName,Qry,'')
end;

procedure TTestSuiteHTMLWriter.ComboBoxFromQuery(const ComboName, Qry, Value: String);

Var
  Q : TSQLQuery;

begin
  Q:=FSQL.CreateQuery(Qry);
  try
    Q.Open;
    FComboboxProducer.Dataset:=Q;
    FComboBoxProducer.ValueField:=Q.Fields[0].FieldName;
    FComboBoxProducer.DataField:=Q.Fields[1].FieldName;
    FComboBoxProducer.Value:=Value;
    FComboBoxProducer.InputName:=ComboName;
    FComboBoxProducer.CreateComboBox(Stream);
  Finally
    Q.Free;
  end;
end;

procedure TTestSuiteHTMLWriter.ComboBoxFromQuery(const ComboName, Qry: String; Value: integer);
begin
  ComboBoxFromQuery(ComboName,Qry,IntToStr(Value))
end;

procedure TTestSuiteHTMLWriter.FormatFailedOverview(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  S:=Format(SDetailsURL,[P.DataSet.FieldByName('ID').AsString]);
  S:=S+'&amp;failedonly=1&amp;noskipped=1';
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;


function TTestSuiteHTMLWriter.FormatDetailURL(const RunIdStr, CellData : String) : string;
Var
  S : String;
begin
  S:=Format(SDetailsURL,[RunIdStr]);
  if FVars.OnlyFailed then
    S:=S+'&amp;failedonly=1';
  if FVars.NoSkipped then
    S:=S+'&amp;noskipped=1';
  Result:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;

procedure TTestSuiteHTMLWriter.FormatTestRunOverview(Sender: TObject; var CellData: String);
Var
  S: String;
  P : TTableProducer;
begin
  P:=(Sender as TTableProducer);
  S:=Format(SDetailsURL,[P.DataSet.FieldByName('RUN').AsString]);
  if FVars.OnlyFailed then
    S:=S+'&amp;failedonly=1';
  if FVars.NoSkipped then
    S:=S+'&amp;noskipped=1';
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;


procedure TTestSuiteHTMLWriter.FormatSVN(Sender: TObject; var CellData: String);
begin
  FormatSVNData(CellData);
end;

procedure TTestSuiteHTMLWriter.FormatSVNData(var CellData: String);
Var
  S, Rev, SubStr, Remaining : String;
  pos_colon, pos_sep : longint;
begin
  if CellData='' then
    exit;
  pos_sep:=pos('/', CellData);
  if pos_sep=0 then
    begin
      pos_colon:=pos(':',CellData);
      S:=ViewGitHashURL+copy(CellData,pos_colon+1,length(CellData));
      CellData:=Format('<A HREF="%s" target="_blank">%s</A>',[S,CellData]);
    end
  else
    begin
      SubStr:=Copy(CellData,1,pos_sep-1);
      Remaining:=Copy(CellData,pos_sep+1,length(CellData));
      CellData:='';
      while SubStr<>'' do
        begin
          pos_colon:=pos(':',SubStr);
          Rev:=copy(SubStr,pos_colon+1,length(SubStr));
          { Remove suffix like M for modified...}
          while (length(Rev)>0) and (not (Rev[length(Rev)] in ['0'..'9','a'..'f','A'..'F'])) do
            Rev:=Copy(Rev,1,length(Rev)-1);
          S:=ViewGitHashURL+Rev;
          CellData:=CellData+Format('<A HREF="%s" target="_blank">%s</A>',[S,SubStr]);
          if Remaining='' then
            SubStr:=''
          else
            begin
              pos_sep:=pos('/',Remaining);
              if pos_sep=0 then
                pos_sep:=length(Remaining)+1;
              CellData:=CellData+':';
              SubStr:=Copy(Remaining,1,pos_sep-1);
              Remaining:=Copy(Remaining,pos_sep+1,length(Remaining));
            end;
        end;
    end;
end;

procedure TTestSuiteHTMLWriter.EmitHiddenVar(const Name: String; aValue: Int64);
begin
  if (aValue<>-1) then
    EmitHiddenVar(Name,IntToStr(aValue));
end;


procedure TTestSuiteHTMLWriter.FormatFileIDDetails(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  if FVars.VersionID<>-1 then
    S:=Format(TestSuiteCGIURL + '?action=%d&amp;version=%d&amp;testfileid=%d',
       [faction_show_history,FVars.VersionID,P.DataSet.FieldByName('Id').AsInteger])
  else
    S:=Format(TestSuiteCGIURL + '?action=%d&amp;testfileid=%s',
       [faction_show_history,P.DataSet.FieldByName('Id').AsString]);
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;


procedure TTestSuiteHTMLWriter.FormatFileDetails(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  if FVars.CompareRunID<>-1 then
    S:=Format(TestSuiteCGIURL + '?action=%d&amp;run1id=%d&amp;run2id=%d&amp;testfileid=%s',
       [faction_show_one_test,FVars.RunID,FVars.CompareRunID,P.DataSet.FieldByName('Id').AsString])
  else
    S:=Format(TestSuiteCGIURL + '?action=%d&amp;run1id=%d&amp;testfileid=%s',
       [faction_show_one_test,FVars.RunID,P.DataSet.FieldByName('Id').AsString]);
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;

procedure TTestSuiteHTMLWriter.FormatTestResult(Sender: TObject; var CellData: String);

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

procedure TTestSuiteHTMLWriter.EmitTitle(ATitle: String);

begin
  if FNeedEnd then
    exit;
  DumpLn('<HTML>');
  DumpLn('<HEAD>');
  DumpLn('<TITLE>'+ATitle+'</TITLE>');
  Dumpln('<STYLE>');
  Dumpln('.logNormal { color: green; }');
  Dumpln('.logAbort { color: red; }');
  Dumpln('.logError { color: red; }');
  Dumpln('.logWarning { color: orange; }');
  Dumpln('.logSQL { color: darkblue; font-size: small; }');
  Dumpln('.logDebug { color: darkblue; font-size: small; }');
  Dumpln('</STYLE>');
  DumpLn('</HEAD>');

  DumpLn('<BODY>');
  FNeedEnd:=true;
end;

procedure TTestSuiteHTMLWriter.EmitDocType;
begin
  if FNeedEnd then
    exit;
  DumpLn('<!DOCTYPE html>');
end;

function TTestSuiteHTMLWriter.CreateTableProducer(DS: TSQLQuery): TTableProducer;

begin
  Result:=TTableProducer.Create(Nil);
  Result.Dataset:=DS;
end;

procedure TTestSuiteHTMLWriter.DefaultTableFromQuery(Qry, ALink: String; IncludeRecordCount: Boolean);

Var
  Q : TSQLQuery;
  lTable : TTableProducer;

begin
  If FVars.Debug then
    Writeln('Query : '+Qry);
  lTable:=Nil;
  Q:=FSQL.CreateQuery(Qry);
  try
    Q.Open;
    lTable:=CreateTableProducer(Q);
    lTable.Border:=True;
    If (Alink<>'') then
      begin
      lTable.CreateColumns(Nil);
      If lTable.TableColumns.Count>0 then
         (lTable.TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
      end;
    lTable.CreateTable(Stream);
    If IncludeRecordCount then
      DumpLn(Format('<p>Record count: %d </p>',[Q.RecordCount]));
  finally
    lTable.Free;
    Q.Free;
  end;
end;

procedure TTestSuiteHTMLWriter.EmitPieImage(aOKCount,aFailedCount,aSkipCount : integer);

const
  sLink = 'Src="%s?action=2&amp;pietotal=%d&amp;piefailed=%d&amp;pieskipped=%d"'+
          ' ALT="total=%d, failed=%d, skipped=%d"';

begin
  ParaGraphStart;
  TagStart('IMG',Format(SLink,[TestsuiteCGIURL,
                               aOKCount,aFailedCount,aSkipCount,
                               aOKCount,aFailedCount,aSkipCount
  ]));
end;

procedure TTestSuiteHTMLWriter.EmitHistoryForm(aTitle : String);

begin
  EmitDocType;
  EmitTitle(aTitle);
  HeaderStart(1);
  DumpLn('View Test suite results');
  HeaderEnd(1);
  DumpLn('Please specify search criteria:');
  FormStart(TestsuiteCGIURL,'');
  if FVars.Debug then
    EmitHiddenVar('DEBUGCGI', '1');
  EmitHiddenVar('action',IntToStr(faction_show_history));
  TableStart(2,true);
  RowStart;
    CellStart;
      DumpLn('File:');
    CellNext;
      EmitInput('testfilename',FVars.Testfilename);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Operating system:');
    CellNext;
      ComboBoxFromQuery('os','SELECT TO_ID,TO_NAME FROM TESTOS ORDER BY TO_NAME',IntToStr(FVars.OSID));
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Processor:');
    CellNext;
      ComboBoxFromQuery('cpu','SELECT TC_ID,TC_NAME FROM TESTCPU ORDER BY TC_NAME',FVars.CPUID);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Version');
    CellNext;
      ComboBoxFromQuery('version','SELECT TV_ID,TV_VERSION FROM TESTVERSION ORDER BY TV_VERSION DESC',FVars.VERSIONID);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Date');
    CellNext;
      If (FVars.Date=0) then
        EmitInput('date','')
      else
        EmitInput('date',DateToStr(FVars.Date));
    CellEnd;
  RowNext;
    CellStart;
    DumpLn('Submitter');
    CellNext;
    If (FVars.Submitter='') then
      EmitInput('submitter','')
    else
      EmitInput('submitter',FVars.Submitter);
    CellEnd;
  RowNext;
    CellStart;
    DumpLn('Machine');
    CellNext;
    If (FVars.Machine='') then
      EmitInput('machine','')
    else
      EmitInput('machine',FVars.Machine);
    CellEnd;
  RowNext;
    CellStart;
    DumpLn('Config');
    CellNext;
    If (FVars.Config='') then
      EmitInput('config','')
    else
      EmitInput('config',FVars.Config);
    CellEnd;
  RowNext;
    CellStart;
    DumpLn('Limit');
    CellNext;
    EmitInput('limit',IntToStr(FVars.Limit));
    CellEnd;
  RowNext;
    CellStart;
    DumpLn('Cond');
    CellNext;
    If (FVars.Cond='') then
      EmitInput('cond','')
    else
      EmitInput('cond',FVars.Cond);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Category');
    CellNext;
      ComboBoxFromQuery('Category','SELECT TA_ID,TA_NAME FROM TESTCATEGORY ORDER BY TA_NAME',FVars.CategoryID);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Only failed tests');
    CellNext;
      EmitCheckBox('failedonly','1',FVars.onlyFailed);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Hide skipped tests');
    CellNext;
      EmitCheckBox('noskipped','1',FVars.NoSkipped);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('List all tests');
    CellNext;
      EmitCheckBox('listall','1',FVars.ListAll);
    CellEnd;

  RowEnd;
  TableEnd;
  ParaGraphStart;
  if FVars.Debug then
    EmitHiddenVar('DEBUGCGI', '1');
  EmitSubmitButton('','Search');
  EmitResetButton('','Reset form');
  FormEnd;
end;

procedure TTestSuiteHTMLWriter.EmitOverviewForm(aTitle : string);

begin
  EmitDocType;
  EmitTitle(aTitle);
  HeaderStart(1);
  DumpLn('View Test suite results');
  HeaderEnd(1);
  DumpLn('Please specify search criteria:');
  FormStart(TestsuiteCGIURL,'');
  if FVars.Debug then
    EmitHiddenVar('DEBUGCGI', '1');
  TableStart(2,true);
  RowStart;
    CellStart;
      DumpLn('Operating system:');
    CellNext;
      ComboBoxFromQuery('os','SELECT TO_ID,TO_NAME FROM TESTOS ORDER BY TO_NAME',FVars.OSID);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Processor:');
    CellNext;
      ComboBoxFromQuery('cpu','SELECT TC_ID,TC_NAME FROM TESTCPU ORDER BY TC_NAME',FVars.CPUID);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Version');
    CellNext;
      ComboBoxFromQuery('version','SELECT TV_ID,TV_VERSION FROM TESTVERSION ORDER BY TV_VERSION DESC',FVars.VERSIONID);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Date');
    CellNext;
      If (FVars.Date=0) then
        EmitInput('date','')
      else
        EmitInput('date',DateToStr(FVars.Date));
    CellEnd;
  //if FDebug then
    begin
      RowNext;
      CellStart;
      DumpLn('Submitter');
      CellNext;
      If (FVars.Submitter='') then
        EmitInput('submitter','')
      else
        EmitInput('submitter',FVars.Submitter);
      CellEnd;
     RowNext;
      CellStart;
      DumpLn('Machine');
      CellNext;
      If (FVars.Machine='') then
        EmitInput('machine','')
      else
        EmitInput('machine',FVars.Machine);
      CellEnd;
      RowNext;
      CellStart;
      DumpLn('Config');
      CellNext;
      If (FVars.Config='') then
        EmitInput('config','')
      else
        EmitInput('config',FVars.Config);
      CellEnd;

      RowNext;
      CellStart;
      DumpLn('Cond');
      CellNext;
      If (FVars.Cond='') then
        EmitInput('cond','')
      else
        EmitInput('cond',FVars.Cond);
      CellEnd;
    end;
  RowNext;
    CellStart;
      DumpLn('Category');
    CellNext;
      ComboBoxFromQuery('Category','SELECT TA_ID,TA_NAME FROM TESTCATEGORY ORDER BY TA_NAME',IntToStr(FVars.CategoryID));
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Only failed tests');
    CellNext;
      EmitCheckBox('failedonly','1',FVars.onlyFailed);
    CellEnd;
  RowNext;
    CellStart;
      DumpLn('Hide skipped tests');
    CellNext;
      EmitCheckBox('noskipped','1',FVars.NoSkipped);
    CellEnd;
  RowEnd;
  TableEnd;
  ParaGraphStart;
  EmitSubmitButton('','Search');
  EmitSubmitButton('action','View history');
  EmitResetButton('','Reset form');
  FormEnd;
end;

procedure TTestSuiteHTMLWriter.DumpTestInfo(aInfo: TTestInfo);

  Procedure MaybeField(const aName,aValue : string);
  begin
    if aValue='' then exit;
    DumpLn(aName+':');
    DumpLn(' ');
    DumpLn(aValue);
    DumpLn('<BR>');
  end;

  Procedure MaybeField(const aName : string; aValue : Boolean);
  begin
    if not aValue then exit;
    DumpLn('Flag ');
    DumpLn('"'+aName+'" :');
    DumpLn(' set');
    DumpLn('<BR>');
  end;

  Procedure MaybeField(const aName : string; aValue : Integer);
  begin
    if aValue<=0 then exit;
    MaybeField(aName,IntToStr(aValue));
  end;

begin
  With aInfo do
    begin
    MaybeField('CPU',CPU);
    MaybeField('OS',OS);
    MaybeField('Version',Version);
    if addDate<>0 then
      MaybeField('Add date',FormatDateTime('yyy-mm-dd',addDate));
    MaybeField('Version',Version);
    MaybeField('Graph',Graph);
    MaybeField('Interactive',Interactive);
    MaybeField('Result',Result);
    MaybeField('Fail',Fail);
    MaybeField('Recompile',Recompile);
    MaybeField('NoRun',NoRun);
    MaybeField('NeedLibrary',NoRun);
    MaybeField('KnownRunError',KnownRunError);
    MaybeField('Note',Note);
    MaybeField('Description',Description);
    MaybeField('Opts',opts);
    end;
end;

procedure TTestSuiteHTMLWriter.EmitEnd;

begin
  if not FNeedEnd then
    exit;
  DumpLn('</BODY>');
  DumpLn('</HTML>');
end;

procedure TTestSuiteHTMLWriter.HandleVerbose(lvl: TVerboseLevel; const aMsg: String);

Const
  StyleNames : Array[TVerboseLevel] of string
              = ('Abort','Error','Warning','Normal','Debug','SQL');
begin
  LDumpln(Format('<span class="log%s" >%s</span><br>',[StyleNames[lvl],aMsg]));
end;

procedure TTestsuiteHTMLWriter.LDump(Const St : String);

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
      Dump(ShortS);
    end;
  ShortS:=Copy(St,p,255);
  Dump(ShortS);
end;

procedure TTestsuiteHTMLWriter.LDumpLn(Const St : String);
begin
  LDump(St);
  LDump(LineFeed);
end;

end.

