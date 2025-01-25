unit tshistory;

{$mode ObjFPC}
{$h+}

interface

uses
  Classes, SysUtils, types, tstypes, db, tsdb, whtml;

Type

  { TDatasetMap }

  TDatasetMap = class(TObject)
    OK,
    Skip,
    Result,
    CPU,
    OS,
    Version,
    Date,
    Run : TField;
    constructor Create(aDataset : TDataset);
  end;

  TGetDetailURLEvent = procedure (aRunID : Int64; aDate : TDateTime; out aURl : String) of object;


  { TTestHistoryInfo }

  TTestHistoryInfo = Class (TObject)
  Const
    { We already have 53 versions }
    MaxCombo = 100;
  Type
    StatusLongintArray = Array [TTestStatus] of Int64;
    StatusDateTimeArray = Array [TTestStatus] of TDateTime;
    TStatusLAArray = Array of StatusLongintArray;
    TStatusDTAArray = Array of StatusDateTimeArray;
  private
    function FormatDetailURL(aRunID: Int64; aDate: TDateTime): string;
    procedure HandleCPU(aRunID: Int64; aCPUID: Integer; aStatus: TTestStatus; aDate: TDateTime);
    procedure HandleDates(aRunID: Int64; aStatus: TTestStatus; aMap: TDatasetMap);
    procedure HandleOS(aRunID: Int64; aOSID: Integer; aStatus: TTestStatus; aDate: TDateTime);
    procedure HandleVersion(aRunID: Int64; aVersionID: Integer; aStatus: TTestStatus; aDate: TDateTime);
    function MapCpu(aID: Integer): integer;
    function MapOS(aID: Integer): integer;
    function MapVersion(aID: Integer): integer;
  Public
    total_count:Integer;
    OK_count:Integer;
    not_OK_count:Integer;
    skip_count:Integer;
    not_skip_count:Integer;
    os_count,cpu_count,version_count: TStatusLAArray;
    os_first_date, os_last_date,
    cpu_first_date, cpu_last_date,
    version_first_date, version_last_date : TStatusDTAArray;
    os_first_date_id, os_last_date_id,
    cpu_first_date_id, cpu_last_date_id,
    version_first_date_id, version_last_date_id : TStatusLAArray;
    first_date, last_date : array[TTestStatus] of TDateTime;
    first_date_id, last_date_id : array[TTestStatus] of Int64;
    result_count : StatusLongintArray;
    FCPUMap,FOSMap,FVersionMap :TIntegerDynArray;
    FSQL : TTestSQL;
    OnGetDetailURL : TGetDetailURLEvent;
    constructor create(aSQL : TTestSQL; aOSMap,aCPUMap,aVersionMap : TIntegerDynArray);
    procedure InitCPU(aCPUCount : Integer);
    procedure InitVersion(aVersionCount : Integer);
    procedure InitOS(aOSCount : Integer);
    procedure UpdateFromDataset(Q: TDataset);
    procedure WriteCounts(aHTMLWriter: THTMLWriter; TS: TTestStatus);
    procedure WriteCPUHistory(aHTMLWriter: THTMLWriter; TS: TTestStatus);
    procedure WriteOSHistory(aHTMLWriter: THTMLWriter; TS: TTestStatus);
    procedure WriteVersionHistory(aHTMLWriter: THTMLWriter; TS: TTestStatus);


  end;

implementation

{ TDatasetMap }

constructor TDatasetMap.Create(aDataset: TDataset);

begin
  OK:=aDataset.FieldByName('OK');
  Skip:=aDataset.FieldByName('SKIP');
  Result:=aDataset.FieldByName('Result');
  CPU:=aDataset.FieldByName('TC_ID');
  OS:=aDataset.FieldByName('TO_ID');
  Version:=aDataset.FieldByName('TV_ID');
  Date:=aDataset.FieldByName('Date');
  Run:=aDataset.FieldByName('TU_ID');
end;

{ TTestHistoryInfo }

constructor TTestHistoryInfo.create(aSQL: TTestSQL; aOSMap, aCPUMap, aVersionMap: TIntegerDynArray);
begin
  FSQL:=aSQL;
  FCPUMap:=aCPUMap;
  FOSMap:=aOSMap;
  FVersionMap:=aVersionMap;
  InitCPU(Length(aCPUMap));
  InitVersion(Length(aVersionMap));
  InitOS(Length(aOSMap));
end;

procedure TTestHistoryInfo.InitCPU(aCPUCount: Integer);

begin
  SetLength(cpu_count,aCPUCount+1);
  SetLength(cpu_first_date_id,aCPUCount+1);
  SetLength(cpu_last_date_id,aCPUCount+1);
  SetLength(cpu_first_date,aCPUCount+1);
  SetLength(cpu_last_date,aCPUCount+1);
end;

procedure TTestHistoryInfo.InitVersion(aVersionCount: Integer);
begin
  SetLength(version_count,aVersionCount+1);
  SetLength(version_first_date_id,aVersionCount+1);
  SetLength(version_last_date_id,aVersionCount+1);
  SetLength(version_first_date,aVersionCount+1);
  SetLength(version_last_date,aVersionCount+1);
end;

procedure TTestHistoryInfo.InitOS(aOSCount: Integer);
begin
  SetLength(os_count,aOSCount+1);
  SetLength(os_first_date_id,aOSCount+1);
  SetLength(os_last_date_id,aOSCount+1);
  SetLength(os_first_date,aOSCount+1);
  SetLength(os_last_date,aOSCount+1);
end;

function TTestHistoryInfo.MapCpu(aID : Integer) : integer;

begin
  Result:=Length(FCPUMap)-1;
  While (Result>=0) and (FCPUMap[Result]<>aID) do
    dec(Result);
end;

function TTestHistoryInfo.MapOS(aID : Integer) : integer;

begin
  Result:=Length(FOSMap)-1;
  While (Result>=0) and (FOSMap[Result]<>aID) do
    dec(Result);
end;

function TTestHistoryInfo.MapVersion(aID: Integer): integer;
begin
  Result:=Length(FVersionMap)-1;
  While (Result>=0) and (FVersionMap[Result]<>aID) do
    dec(Result);
end;


procedure TTestHistoryInfo.HandleCPU(aRunID : Int64; aCPUID : Integer; aStatus : TTestStatus; aDate : TDateTime);

var
  lCPU : Integer;

begin
  if length(cpu_count)=0 then
    exit;
  lCPU:=MapCPU(aCPUID);
  if lCPU=-1 then
    exit;

  if cpu_count[lCPU,aStatus]=0 then
    begin
      cpu_first_date[lCPU,aStatus]:=aDate;
      cpu_last_date[lCPU,aStatus]:=aDate;
      cpu_first_date_id[lCPU,aStatus]:=aRunID;
      cpu_last_date_id[lCPU,aStatus]:=aRunID;
    end
  else
    begin
      if (aDate>cpu_last_date[lCPU,aStatus]) then
        begin
          cpu_last_date[lCPU,aStatus]:=aDate;
          cpu_last_date_id[lCPU,aStatus]:=aRunID;
        end;
      if aDate<cpu_first_date[lCPU,aStatus] then
        begin
          cpu_first_date[lCPU,aStatus]:=aDate;
          cpu_first_date_id[lCPU,aStatus]:=aRunID;
        end;
    end;
  inc(cpu_count[lCPU,aStatus]);
end;

procedure TTestHistoryInfo.HandleOS(aRunID : Int64; aOSID : Integer; aStatus : TTestStatus; aDate : TDateTime);

var
  lOS : Integer;

begin
  if length(os_count)=0 then
    exit;
  lOS:=MapOS(aOSId);
  if lOS=-1 then
    exit;

  if os_count[lOS,aStatus]=0 then
    begin
      os_first_date[lOS,aStatus]:=aDate;
      os_last_date[lOS,aStatus]:=aDate;
      os_first_date_id[lOS,aStatus]:=aRunID;
      os_last_date_id[lOS,aStatus]:=aRunID;
    end
  else
    begin
      if (aDate>os_last_date[lOS,aStatus]) then
        begin
          os_last_date[lOS,aStatus]:=aDate;
          os_last_date_id[lOS,aStatus]:=aRunID;
        end;
      if aDate<os_first_date[lOS,aStatus] then
        begin
          os_first_date[lOS,aStatus]:=aDate;
          os_first_date_id[lOS,aStatus]:=aRunID;
        end;
    end;
  inc(os_count[lOS,aStatus]);
end;


procedure TTestHistoryInfo.HandleDates(aRunID : Int64; aStatus : TTestStatus; aMap : TDatasetMap);

var
  lDate : TDateTime;

begin
  lDate:=aMap.Date.AsDateTime;
  if Result_count[aStatus]=0 then
    begin
      first_date[aStatus]:=lDate;
      last_date[aStatus]:=lDate;
      first_date_id[aStatus]:=aRunID;
      last_date_id[aStatus]:=aRunID;
    end
  else
    begin
      if (date>last_date[aStatus]) then
        begin
          last_date[aStatus]:=lDate;
          last_date_id[aStatus]:=aRunID;
        end;
      if date<first_date[aStatus] then
        begin
          first_date[aStatus]:=lDate;
          first_date_id[aStatus]:=aRunID;
        end;
    end;
end;

procedure TTestHistoryInfo.HandleVersion(aRunID : Int64; aVersionID : Integer; aStatus : TTestStatus; aDate : TDateTime);

var
  lVersion : Integer;

begin
  if length(version_count)=0 then
    exit;
  lVersion:=MapVersion(aVersionId);
  if lVersion=-1 then
    exit;

  if version_count[lVersion,aStatus]=0 then
    begin
      version_first_date[lVersion,aStatus]:=aDate;
      version_last_date[lVersion,aStatus]:=aDate;
      version_first_date_id[lVersion,aStatus]:=aRunID;
      version_last_date_id[lVersion,aStatus]:=aRunID;
    end
  else
    begin
      if (aDate>version_last_date[lVersion,aStatus]) then
        begin
          version_last_date[lVersion,aStatus]:=aDate;
          version_last_date_id[lVersion,aStatus]:=aRunID;
        end;
      if aDate<version_first_date[lVersion,aStatus] then
        begin
          version_first_date[lVersion,aStatus]:=aDate;
          version_first_date_id[lVersion,aStatus]:=aRunID;
        end;
    end;
  inc(version_count[lVersion,aStatus]);

end;

procedure TTestHistoryInfo.UpdateFromDataset(Q: TDataset);

var
  lMap : TDatasetMap;
  resi : Integer;
  TS : TTestStatus;
  lRunID: int64;
  lDate : TDateTime;

begin
  lMap:=TDatasetMap.Create(Q);
  While not Q.EOF do
    begin
    inc(total_count);
    if lMap.OK.AsBoolean then
      inc(OK_count)
    else
      inc(not_OK_count);
    if lMap.Skip.AsBoolean then
      inc(skip_count)
    else
      inc(not_skip_count);
    lRunID:=lMap.Run.AsLongint;
    resi:=lMap.Result.AsInteger;
    if (Resi>=longint(FirstStatus)) and  (Resi<=longint(LastStatus)) then
      begin
        TS:=TTestStatus(Resi);
        inc(Result_count[TS]);
        HandleDates(lRunID,ts,lMap);
        lDate:=lMap.Date.AsDateTime;
        HandleCPU(lRunID,lMap.CPU.asInteger,ts,lDate);
        HandleOS(lRunID,lMap.OS.AsInteger,ts,lDate);
        HandleVersion(lRunID,lMap.Version.AsInteger,ts,lDate);
      end;
    Q.Next;
    end;
end;

procedure TTestHistoryInfo.WriteCounts(aHTMLWriter: THTMLWriter; TS: TTestStatus);
begin
  With aHTMLWriter do
    begin
    RowNext;
    CellStart;
    DumpLn(StatusText[TS]);
    CellNext;
    CellNext;
    DumpLn(Format('%d',[Result_count[TS]]));
    CellNext;
    DumpLn(Format('%3.1f',[Result_count[TS]*100/total_count]));
    CellNext;

    DumpLn(FormatDetailURL(first_date_id[TS], first_date[TS]));
    DumpLn(' : '+IntToStr(FSQL.GetFailCount(first_date_id[TS])));
    CellNext;
    DumpLn(FormatDetailURL(last_date_id[TS], last_date[TS]));
    DumpLn(' : '+IntToStr(FSQL.GetFailCount(last_date_id[TS])));
    CellEnd;
    end;
end;

function TTestHistoryInfo.FormatDetailURL(aRunID: Int64; aDate : TDateTime) : string;

begin
  if assigned(OnGetDetailURL) then
    OnGetDetailURL(aRunID,aDate,Result)
  else
    Result:='';
end;

procedure TTestHistoryInfo.WriteCPUHistory(aHTMLWriter : THTMLWriter; TS : TTestStatus);

var
  i : Integer;

begin
  if not assigned(cpu_count) then
    exit;

  for i:=0 to Length(cpu_count)-1 do
    if cpu_count[i,TS]>0 then
      With aHTMLWriter do
        begin
        RowNext;
        CellStart;
        CellNext;
        DumpLn(FSQL.GetCPUName(FCPUMap[i]));
        CellNext;
        DumpLn(Format('%d',[cpu_count[i,TS]]));
        CellNext;
        DumpLn(Format('%3.1f',[cpu_count[i,TS]*100/result_count[TS]]));
        CellNext;
        DumpLn(FormatDetailURL(cpu_first_date_id[i,TS], cpu_first_date[i,TS]));
        DumpLn(' '+IntToStr(FSQL.GetFailCount(cpu_first_date_id[i,TS])));
        CellNext;
        DumpLn(FormatDetailURL(cpu_last_date_id[i,TS], cpu_last_date[i,TS]));
        DumpLn(' '+IntToStr(FSQL.GetFailCount(cpu_last_date_id[i,TS])));
        CellEnd;
        end;
end;

procedure TTestHistoryInfo.WriteOSHistory(aHTMLWriter: THTMLWriter; TS: TTestStatus);

var
  i : Integer;

begin
  if not assigned(os_count) then
    exit;

  for i:=0 to Length(os_count)-1 do
    if os_count[i,TS]>0 then
      With aHTMLWriter do
        begin
        RowNext;
        CellStart;
        CellNext;
        DumpLn(FSQL.GetOSName(i));
        CellNext;
        DumpLn(Format('%d',[os_count[i,TS]]));
        CellNext;
        DumpLn(Format('%3.1f',[os_count[i,TS]*100/result_count[TS]]));
        CellNext;
        DumpLn(FormatDetailURL(os_first_date_id[i,TS],os_first_date[i,TS]));
        DumpLn(' '+IntToStr(FSQL.GetFailCount(os_first_date_id[i,TS])));
        CellNext;
        DumpLn(FormatDetailURL(os_last_date_id[i,TS],os_last_date[i,TS]));
        DumpLn(' '+IntToStr(FSQL.GetFailCount(os_last_date_id[i,TS])));
        CellEnd;
        end;
end;

procedure TTestHistoryInfo.WriteVersionHistory(aHTMLWriter: THTMLWriter; TS: TTestStatus);

var
  I : integer;

begin
  if not assigned(version_count) then
    exit;
  for i:=0 to length(Version_count)-1 do
    if version_count[i,TS]>0 then
      With aHTMLWriter do
        begin
        RowNext;
        CellStart;
        CellNext;
        DumpLn(FSQL.GetVersionName(i));
        CellNext;
        DumpLn(Format('%d',[version_count[i,TS]]));
        CellNext;
        DumpLn(Format('%3.1f',[version_count[i,TS]*100/result_count[TS]]));
        CellNext;
        DumpLn(FormatDetailURL(version_first_date_id[i,TS],version_first_date[i,TS]));
        DumpLn(' '+IntToStr(FSQL.GetFailCount(version_first_date_id[i,TS])));
        CellNext;
        DumpLn(FormatDetailURL(version_last_date_id[i,TS], version_last_date[i,TS]));
        DumpLn(' '+IntToStr(FSQL.GetFailCount(version_last_date_id[i,TS])));
        CellEnd;
        end;
end;



end.

