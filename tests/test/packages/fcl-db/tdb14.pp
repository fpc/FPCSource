{ %target=aix,beos,haiku,linux,freebsd,darwin,iphonesim,ios,netbsd,openbsd,solaris,win32,win64,wince,android,os2,dragonfly }
{ %delfiles=tdb14.tmp }
program tdb14;

{ A dataset leaves its temporary state when what runs in it raises: a locate
  or a lookup with a key the field cannot take, a filter event that raises, a
  stream that ends early, a resync after applying updates. It must be usable
  afterwards, and leave no record buffer or bookmark behind. The targets are
  those where the dbf unit is built. }

{$mode objfpc}{$H+}

uses sysutils, classes, db, bufdataset, dbf, sdfdata;

type
  EFilterRefused = class(Exception);
  EResyncRefused = class(Exception);

  TFilterEvents = class
    Refuse, TryLocate, LocateRaised: boolean;
    Seen, SeenAfter: longint;
    procedure FilterRecord(DataSet: TDataSet; var Accept: Boolean);
  end;

  { reaches the resync that follows applying updates }
  TResyncDataset = class(TBufDataset)
  public
    Refuse: boolean;
    procedure Resync(Mode: TResyncMode); override;
  end;

var
  errors: longint = 0;
  events: TFilterEvents;

procedure fail(const what: string);
begin
  writeln('FAIL ', what);
  inc(errors);
end;

{ the failure is the one provoked, not another on the way }
function provoked(e: Exception; want: ExceptClass): boolean;
begin
  result:=e is want;
  if not result then
    fail('expected ' + want.ClassName + ', got ' + e.ClassName + ': ' + e.Message);
end;

{ the key's conversion error, which the field reports as a database error }
function failedLocate(d: TDataSet): boolean;
begin
  result:=false;
  try
    d.Locate('N','abc',[]);
  except
    on e: Exception do
      result:=provoked(e,EDatabaseError);
  end;
end;

procedure TFilterEvents.FilterRecord(DataSet: TDataSet; var Accept: Boolean);
begin
  if Refuse then
    raise EFilterRefused.Create('refused by the filter event');
  if TryLocate then
    begin
      TryLocate:=false;
      Seen:=DataSet.FieldByName('N').AsInteger;
      LocateRaised:=failedLocate(DataSet);
      SeenAfter:=DataSet.FieldByName('N').AsInteger;
    end;
end;

procedure TResyncDataset.Resync(Mode: TResyncMode);
begin
  if Refuse then
    begin
      Refuse:=false;
      raise EResyncRefused.Create('refused by the resync');
    end;
  inherited Resync(Mode);
end;

procedure checkAfter(const what: string; raised: boolean; ds: TDataSet; want: TDataSetState);
begin
  if not raised then
    fail(what + ': nothing raised');
  if ds.State<>want then
    fail(what + ': state ' + IntToStr(ord(ds.State)) + ', expected ' + IntToStr(ord(want)));
  { the temporary state also disables the controls }
  if ds.ControlsDisabled then
    fail(what + ': the controls stay disabled');
  writeln('done ', what);
end;

procedure fill(d: TBufDataset);
begin
  d.FieldDefs.Add('N',ftInteger);
  d.CreateDataset;
  d.Append;
  d.FieldByName('N').AsInteger:=1;
  d.Post;
  d.Append;
  d.FieldByName('N').AsInteger:=2;
  d.Post;
  d.First;
end;

function newBufDataset: TBufDataset;
begin
  result:=TBufDataset.Create(nil);
  fill(result);
end;

function failedLookup(d: TDataSet): boolean;
begin
  result:=false;
  try
    d.Lookup('N','abc','N');
  except
    on e: Exception do
      result:=provoked(e,EDatabaseError);
  end;
end;

procedure checkLocate;
var
  d: TBufDataset;
  used, i: ptruint;
begin
  d:=newBufDataset;
  checkAfter('Locate with a key the field cannot take',failedLocate(d),d,dsBrowse);
  if not d.Locate('N',2,[]) then
    fail('Locate after the failed one');
  checkAfter('Lookup with a key the field cannot take',failedLookup(d),d,dsBrowse);
  if d.Lookup('N',1,'N')<>1 then
    fail('Lookup after the failed one');
  { what a failed call leaves behind would add up }
  failedLocate(d);
  failedLookup(d);
  used:=GetHeapStatus.TotalAllocated;
  for i:=1 to 10 do
    begin
      failedLocate(d);
      failedLookup(d);
    end;
  if GetHeapStatus.TotalAllocated<>used then
    fail('failed calls leave ' + IntToStr(int64(GetHeapStatus.TotalAllocated)-int64(used)) + ' bytes behind');
  d.Free;
end;

{ a filter event catching a failed Locate reads its own record afterwards; it
  runs while scrolling, since Locate in an event during First finds the dataset
  empty and does nothing }
procedure checkNestedLocate;
var
  d: TBufDataset;
  i: longint;
begin
  d:=newBufDataset;
  d.Last;
  for i:=3 to 100 do
    begin
      d.Append;
      d.FieldByName('N').AsInteger:=i;
      d.Post;
    end;
  d.First;
  d.OnFilterRecord:=@events.FilterRecord;
  d.Filtered:=true;
  events.TryLocate:=true;
  events.LocateRaised:=false;
  events.Seen:=-1;
  events.SeenAfter:=-2;
  while not d.EOF do
    d.Next;
  if events.TryLocate then
    fail('the filter event did not run while scrolling');
  if not events.LocateRaised then
    fail('Locate in the filter event did not raise');
  if events.Seen<>events.SeenAfter then
    fail('a filter event reads ' + IntToStr(events.SeenAfter) + ' after a failed Locate, ' + IntToStr(events.Seen) + ' before');
  checkAfter('Locate in a filter event',true,d,dsBrowse);
  d.Free;
end;

procedure checkFilter(const what: string; d: TDataSet);
var
  raised: boolean;
begin
  d.OnFilterRecord:=@events.FilterRecord;
  events.Refuse:=false;
  d.Filtered:=true;
  events.Refuse:=true;
  raised:=false;
  try
    d.First;
    d.Next;
  except
    on e: Exception do
      raised:=provoked(e,EFilterRefused);
  end;
  events.Refuse:=false;
  checkAfter(what + ' with a filter event that raises',raised,d,dsBrowse);
  d.Filtered:=false;
  d.First;
  if d.EOF then
    fail(what + ' after the failed filter: no records');
end;

procedure checkBufFilter;
var
  d: TBufDataset;
begin
  d:=newBufDataset;
  checkFilter('TBufDataset',d);
  d.Free;
end;

procedure checkDbfFilter;
var
  d: TDbf;
begin
  d:=TDbf.Create(nil);
  d.Storage:=stoMemory;
  d.TableName:='tdb14';
  d.FieldDefs.Add('N',ftInteger);
  d.CreateTable;
  d.Open;
  d.Append;
  d.FieldByName('N').AsInteger:=1;
  d.Post;
  d.Append;
  d.FieldByName('N').AsInteger:=2;
  d.Post;
  d.First;
  checkFilter('TDbf',d);
  d.Close;
  d.Free;
end;

procedure checkSdfFilter;
const
  fn = 'tdb14.tmp';
var
  d: TSdfDataSet;
  lines: TStringList;
begin
  lines:=TStringList.Create;
  lines.Add('1');
  lines.Add('2');
  lines.SaveToFile(fn);
  lines.Free;
  d:=TSdfDataSet.Create(nil);
  try
    d.Schema.Add('N');
    d.FileName:=fn;
    d.Open;
    checkFilter('TSdfDataSet',d);
    d.Close;
  finally
    d.Free;
    DeleteFile(fn);
  end;
end;

procedure checkStream;
var
  d, e: TBufDataset;
  m: TMemoryStream;
  raised: boolean;
  size: int64;
begin
  d:=newBufDataset;
  m:=TMemoryStream.Create;
  d.SaveToStream(m,dfBinary);
  d.Free;
  size:=m.Size;
  { within the last record }
  m.Size:=size-3;
  m.Position:=0;
  e:=TBufDataset.Create(nil);
  raised:=false;
  try
    e.LoadFromStream(m,dfBinary);
  except
    on x: Exception do
      raised:=provoked(x,EReadError);
  end;
  checkAfter('LoadFromStream of a stream that ends early',raised,e,dsInactive);
  { a stream that does not end early }
  d:=newBufDataset;
  m.Clear;
  d.SaveToStream(m,dfBinary);
  d.Free;
  m.Position:=0;
  e.LoadFromStream(m,dfBinary);
  if e.RecordCount<>2 then
    fail('LoadFromStream after the failed one: ' + IntToStr(e.RecordCount) + ' records');
  e.Free;
  m.Free;
end;

procedure checkApplyUpdates;
var
  d: TResyncDataset;
  raised: boolean;
begin
  d:=TResyncDataset.Create(nil);
  fill(d);
  d.Refuse:=true;
  raised:=false;
  try
    d.ApplyUpdates;
  except
    on e: Exception do
      raised:=provoked(e,EResyncRefused);
  end;
  checkAfter('ApplyUpdates whose resync raises',raised,d,dsBrowse);
  d.Free;
end;

begin
  events:=TFilterEvents.Create;
  checkLocate;
  checkNestedLocate;
  checkBufFilter;
  checkDbfFilter;
  checkSdfFilter;
  checkStream;
  checkApplyUpdates;
  events.Free;
  if errors<>0 then
    begin
      writeln(errors,' failure(s)');
      halt(1);
    end;
  writeln('ok');
end.
