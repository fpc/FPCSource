program TestSearch;

{ $define usefirebird}
{ $define usemem}
{$mode objfpc}{$H+}
{$IFDEF UNIX}
  {$linklib pthread}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads,
  {$ENDIF} {$ENDIF}
  {$ifdef usefirebird}
    SQLDBIndexDB, fbIndexdb,
  {$else}
    {$ifdef usemem}
      memindexdb,
    {$else}
      SQLIteIndexDB,
    {$endif}
  {$endif}
  fpIndexer;

procedure usage;

begin
  Writeln('Usage : ',ExtractFileName(ParamStr(0)),' [-e] databasename word');
  Writeln(' -e  : Exact match only');
  halt(1);
end;

var
  Search: TFPSearch;   //searches phrases
  start: TDateTime;
  endtime: TDateTime;
  i: integer;
  n: int64;

{$ifdef usefirebird}
  Function CreateDB(const dbName : String) : TCustomIndexDB;
  Var
    IB: TFBIndexDB;
  begin
    IB := TFBIndexDB.Create(nil);
    IB.DatabasePath := dbname;
    IB.UserName := 'WISASOFT';
    IB.Password := 'SysteemD';
    if not FileExists(IB.DatabasePath) then
    begin
      writeln('error: could not find index database');
      halt;
    end
    else
      IB.Connect;
    Result:=IB;
  end;
{$else}
  {$ifdef usemem}
    Function CreateDB (const dbName : String) : TCustomIndexDB;
    Var
      FB: TFileIndexDB;
    begin
      FB:=TFileIndexDB.Create(Nil);
      FB.FileName:=dbName;
      FB.Connect;
      Result:=FB;
    end;
  {$else}
    Function CreateDB  (const dbName : String) : TCustomIndexDB;
    Var
      SB: TSQLIteIndexDB;
    begin
      SB := TSQLIteIndexDB.Create(nil);
      SB.FileName := dbname;
      if not FileExists(SB.FileName) then
      begin
        writeln('error: could not find index database');
        halt;
      end
      else
        SB.Connect;
      Result:=SB;
    end;
  {$endif}
{$endif}

Var
  DB : String;

{$R *.res}

begin
  start := Now;
  Search := TFPSearch.Create(nil);
  //setup parameters for indexing
  if (ParamCount<2) or (ParamCount>3) then
    Usage;
  if (ParamCount=2)  then
    begin
    DB:=ParamStr(1);
    Search.SetSearchWord(ParamStr(2));
    Search.Options := [soContains]; //allowed to search with wildcards
    end
  else
    begin
    if (ParamStr(1)<>'-e') then
      Usage;
    DB:=ParamStr(2);
    Search.SetSearchWord(ParamStr(3));
    end;
  Search.Database := CreateDB(DB);
  //execute the search
  N := Search.Execute;
  endtime := Now;
  if N <> 0 then
  begin
    writeln('Searching for word: ', ParamStr(1));
    writeln;

    for i := 0 to Search.RankedCount - 1 do
      with Search.RankedResults[i] do
        writeln(Format('rank:%d word:%s pos:%d lang:%s %s filedate:%s context:%s', [Rank, SearchWord, Position, Language, URL, DateTimeToStr(FileDate), Context]));
  end;

  writeln;
  writeln(Format('done in %.3f sec.', [(endtime - start) * 24 * 3600]));
end.

