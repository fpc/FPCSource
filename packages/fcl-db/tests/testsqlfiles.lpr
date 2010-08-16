{
    This file is part of the Free Component Library
    Copyright (c) 2010 by the Free Pascal development team

    SQL source syntax parser test program

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program testsqlfiles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils, fpsqltree, fpsqlparser, fpsqlscanner, sqlscript,
  CustApp;
  { you can add units after this }

type

  { TTestSQLFilesApplication }

  TTestSQLFilesApplication = class(TCustomApplication)
  private
    procedure ParseStatement(Sender: TObject; Statement: TStrings;
      var StopExecution: Boolean);
    procedure ProcessFile(const AFileName: String);
  protected
    FStatementCount : integer;
    FFileCount : Integer;
    FErrorCount : Integer;
    FCurrentFile : String;
    FWriteSQL : Boolean; // Set to true to write SQL to screen.
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TTestSQLFilesApplication }
Procedure TTestSQLFilesApplication.ParseStatement(Sender: TObject; Statement: TStrings; var StopExecution: Boolean);

Var
  P : TSQLParser;
  D : TStringStream;
  S : TSQLElement;
  I : Integer;

begin
  Inc(FStatementCount);
  D:=TStringStream.Create(Statement.Text);
  try
    P:=TSQLParser.Create(D);
    try
      try
        S:=P.Parse;
        If FWriteSQL then
          writeln(S.GetasSql([],0));
        S.Free;
      except
        On E : Exception do
          begin
          Inc(FErrorCount);
          Writeln('Error ',FErrorCount,' processing: ',FCurrentFile,' statement after line : ',(Sender as TEventSQLScript).Line);
          for I:=0 to Statement.Count-1 do
            begin
            Writeln(I+1:5,':',Statement[i]);
            end;
          Writeln('Exception message: ',E.Message);
          If (Sender as TEventSQLScript).Terminator<>';' then
            begin
            Statement.Insert(0,'SET TERM ^ ;');
            Statement.Add('^');
            end
          else
            Statement.Add(';');
          Statement.SaveToFile(Format('error-%d.sql',[FErrorCount]));
          end;
      end;
    finally
      P.Free;
    end;
  finally
    D.Free;
  end;
end;




Procedure TTestSQLFilesApplication.ProcessFile(Const AFileName : String);

Var
  I : TEventSQLScript;

begin
  try
    Inc(FFileCount);
    FCurrentFile:=AFileName;
    I:=TEventSQLScript.Create(Nil);
    try
      I.Script.LoadFromFile(AFileName);;
      I.OnSQLStatement:=@ParseStatement;
      I.UseSetTerm:=True;
      I.UseCommit:=True;
      I.Directives.Add('DISPLAY');
      I.Directives.Add('SET SQL DIALECT');
      I.Directives.Add('TRAP');
      I.Execute;
    finally
      I.Free;
    end;
  except
    On E : Exception do
      Writeln('Error processing ',AFIleName,' : ',E.Message);
  end;
end;

procedure TTestSQLFilesApplication.DoRun;
var
  ErrorMsg: String;
  I : Integer;

begin
  For I:=1 to ParamCount do
    ProcessFile(Paramstr(i));
  Writeln('Processed ',FFileCount,' files.');
  Writeln('Processed ',FStatementCount,' statements.');
  Writeln(FErrorCount,' statements had errors');
  Writeln(FStatementCount-FErrorCount,' statements processed correctly');
  // stop program loop
  Terminate;
end;

constructor TTestSQLFilesApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestSQLFilesApplication.Destroy;
begin
  inherited Destroy;
end;
var
  Application: TTestSQLFilesApplication;

begin
  Application:=TTestSQLFilesApplication.Create(nil);
  Application.Title:='Test SQL Files';
  Application.Run;
  Application.Free;
end.

