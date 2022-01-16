{ *********************************************************************
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2021 Michael Van Canneyt.

    Javascript & typescript parser demo

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  **********************************************************************}

program parsefiles;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, Math, jsTree, jsScanner, jsParser;

type
  TCounts = Record
    Total,OK,Failed : Integer;
    Stop : Boolean;
  end;

  { TParseTSApplication }

  TParseTSApplication = class(TCustomApplication)
  private
    FTypescript : Boolean;
    FStopOnError : Boolean;
    function ParseFile(const aFileName: string): Boolean;
    function ParseFiles(const aDirectory: string; RecurseLevel : Integer): TCounts;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(Msg : string); virtual;
  end;

{ TParseTSApplication }

procedure TParseTSApplication.DoRun;

var
  ErrorMsg: String;
  Directory : String;
  Counts : TCounts;

begin
  Terminate;
  ErrorMsg:=CheckOptions('hd:srt', ['help','directory','stop-on-error','recurse','typescript']);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  FTypescript:=HasOption('t','typescript');
  Directory:=GetOptionValue('d','directory');
  FStopOnError:=HasOption('s','stop-on-error');
  If Directory='' then
    Directory:=GetCurrentDir;
  Counts:=ParseFiles(Directory,Ord(HasOption('r','recurse')));
  With Counts do
    Writeln('Statistics: ',Total,' Total, ',OK,' OK, ',Failed, ' Failed');
end;

Function TParseTSApplication.ParseFile(const aFileName : string) : Boolean;

Var
  aFile : TStrings;
  P : TJSParser;
  S : TStringStream;
  el : TJSElement;
  I : Integer;
  EP : EJSParser;
  Prefix : String;

begin
  Result:=False;
  Writeln('Parsing: ',aFileName);
  Flush(output);
  el:=nil;
  S:=Nil;
  aFile:=TStringList.Create;
  try
    aFile.LoadFromFile(aFileName);
    S:=TStringStream.Create('');
    S.LoadFromFile(aFileName);
    P:=TJSParser.Create(S,ecma2021,FTypescript,aFileName);
    try
      El:=P.Parse;
      Result:=True;
    except
      On E : Exception do
        begin
        writeln('Error ',E.ClassName,' parsing file ',aFileName,' : ',E.Message);
        if E is EJSParser then
          begin
          Writeln('Offending lines : ');
          EP:=EJSParser(E);
          For I:=Max(1,EP.ErrorRow-1) to Min(aFile.Count,EP.ErrorRow+1) do
            begin
            Prefix:=IntToStr(I);
            Writeln(Prefix,' : ',aFile[I-1]);
            if I=EP.ErrorRow then
              Writeln(StringOfChar(' ',EP.ErrorCol-1),StringOfChar('-',Length(Prefix)+3),'^');
            end;
          end;
        end;
    end;
  finally
    el.Free;
    aFile.Free;
    S.Free;
  end;
end;

Function TParseTSApplication.ParseFiles(Const aDirectory : string; RecurseLevel : Integer) : TCounts;

Var
  Info : TSearchRec;
  Res: TCounts;
  Ext : string;
begin
  Result:=Default(TCounts);
  if FTypeScript then
    Ext:='*.d.ts'
  else
    Ext:='*.js';
  If FindFirst(aDirectory+Ext,0,Info)=0 then
    try
      Repeat
        Inc(Result.Total);
        if ParseFile(aDirectory+Info.Name) then
          Inc(Result.OK)
        else
          begin
          Inc(Result.Failed);
          if FStopOnError then
            Result.Stop:=True;
          end;
      until (FindNext(Info)<>0) or Result.Stop;
    Finally
      FindClose(Info)
    end;
  if RecurseLevel=0 then
    exit;
  If FindFirst(aDirectory+'*',faDirectory,Info)=0 then
    try
      Repeat
        if (Info.Attr and faDirectory)=faDirectory then
          begin
          Res:=ParseFiles(aDirectory+Info.Name+PathDelim,RecurseLevel-1);
          Result.Total:=Result.Total+Res.Total;
          Result.OK:=Result.OK+Res.OK;
          Result.Failed:=Result.Failed+Res.Failed;
          Result.Stop:=Result.Stop or Res.Stop
          end
      until (FindNext(Info)<>0) or Res.Stop;
    Finally
      FindClose(Info)
    end;
end;

constructor TParseTSApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TParseTSApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TParseTSApplication.Usage(Msg: string);
begin
  if Msg<>'' then
    Writeln('Error : ',Msg);
  writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or mote of:');
  Writeln('-h --help           Display this help text');
  Writeln('-d --directory      Parse all files in directory');
  Writeln('-s --stop-on-error  Stop parsing files after an error');
  Writeln('-t --typescript     Assume typscript');

end;

var
  Application: TParseTSApplication;
begin
  Application:=TParseTSApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

