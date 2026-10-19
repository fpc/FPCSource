{ %skiptarget=msdos,go32v2 }
program tdb12;

{ The message of an EUpdateError is the native error's text as it is, as in
  Delphi: a '%' in it is neither a format specifier for the context nor an
  error -- also through TDataSet.PSGetUpdateException, which passes on the
  message of the exception it is given. TSQLQuery's, which adds that message to
  a text of its own, is unchanged }

{$mode objfpc}{$H+}

uses sysutils, db, dbconst, memds, sqldb;

type
  TTestDataset = class(TMemDataset)
  end;

  TTestQuery = class(TSQLQuery)
  end;

{ the message of an EUpdateError made from NativeError and Context, or the class
  of what its construction raised }
function UpdateErrorMessage(const NativeError, Context: string): string;
var
  E: EUpdateError;
begin
  E:=nil;
  try
    E:=EUpdateError.Create(NativeError,Context,1,2,nil);
    Result:=E.Message;
    if (E.Context<>Context) or (E.ErrorCode<>1) or (E.PreviousError<>2) then
      Result:='wrong fields';
  except
    on X: Exception do
      Result:='raised '+X.ClassName;
  end;
  E.Free;
end;

{ the message of the EUpdateError PSGetUpdateException makes of an exception
  with message Msg, or the class of what that raised }
function DatasetUpdateMessage(DSet: TTestDataset; const Msg: string; WithPrev: boolean): string;
var
  E, Prev: EUpdateError;
begin
  E:=nil;
  Prev:=nil;
  try
    { the new error's PreviousError is the previous error's ErrorCode }
    if WithPrev then
      Prev:=EUpdateError.Create('previous','',7,0,nil);
    E:=DSet.PSGetUpdateException(Exception.Create(Msg),Prev);
    Result:=E.Message;
    if WithPrev and (E.PreviousError<>7) then
      Result:='wrong previous error';
  except
    on X: Exception do
      Result:='raised '+X.ClassName;
  end;
  E.Free;
  Prev.Free;
end;

var
  DSet: TTestDataset;
  Q: TTestQuery;
  E: EUpdateError;
  S: string;

begin
  S:=UpdateErrorMessage('bad %s value','ctx');
  writeln('native error with %s: ',S);
  if S<>'bad %s value' then
    Halt(1);
  S:=UpdateErrorMessage('100% done %d','');
  writeln('native error with %d: ',S);
  if S<>'100% done %d' then
    Halt(2);

  DSet:=TTestDataset.Create(nil);
  S:=DatasetUpdateMessage(DSet,'50% %s off',false);
  writeln('dataset: ',S);
  if S<>'50% %s off' then
    Halt(3);
  S:=DatasetUpdateMessage(DSet,'25% %d more',true);
  writeln('dataset with a previous error: ',S);
  if S<>'25% %d more' then
    Halt(4);
  DSet.Free;

  Q:=TTestQuery.Create(nil);
  E:=Q.PSGetUpdateException(Exception.Create('x %d y'),nil);
  writeln('query: ',E.Message);
  if (E.Message<>Format(SOnUpdateError,['x %d y'])) or (E.Context<>'x %d y') then
    Halt(5);
  E.Free;
  Q.Free;
  writeln('ok');
end.
