{
    makestub  -  pas2js stub generator
    Copyright (C) 2017 - 2020 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
program makestub;

{$mode objfpc}
{$H+}

uses SysUtils, Classes, custapp, stubcreator;

Type
  { TStubCreatorApplication }

  TStubCreatorApplication  = Class(TCustomApplication)
  Private
    FCreator : TStubCreator;
    procedure PrintUsage(S: String);
  Protected
    function ParseOptions : Boolean;
    Procedure DoRun; override;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  end;

{ TStubCreatorApplication }

procedure TStubCreatorApplication.PrintUsage(S : String);

begin
  if S<>'' then
    Writeln('Error : ',S);
  writeln('usage: stubcreator options');
  writeln;
  writeln('Where options is one or more of');
  Writeln('-h --help             This text');
  writeln('-i --input=file       Is the file to be read by the parser');
  writeln('-I --include=dir      Add dir to include path');
  writeln('-o --output=file      Output file name. If not specified, standard output is assumed ');
  Writeln('-c --config=filename  Read ini file with configuration');
  Writeln('-H --header=filename  Add file header using contents of file "filename"');
  Writeln('-f --forwardclasses[=list]');
  Writeln('                      Generate forward definitions for list of classes. If empty, for all classes.');
  ExitCode:=Ord(S<>'');
end;

function TStubCreatorApplication.ParseOptions : Boolean;

Var
  S : String;

begin
  Result:=False;
  S:=CheckOptions('d:i:o:c:h:f:H:I:',['help','input:','output:','forwardclasses::',
                                      'config:','linenumberwidth:','define:','header:',
                                      'include:']);
  if (S<>'') or HasOption('h','help') then
     begin
     PrintUsage(S);
     Exit;
     end;
  FCreator.InputFileName:=GetOptionValue('i','input');
  FCreator.OutputFileName:=GetOptionValue('o','output');
  FCreator.HeaderFileName:=GetOptionValue('H','header');
  If HasOption('d','define') then
    for S in GetOptionValues('d','define') do
      FCreator.Defines.Add(S);
  If HasOption('I','include') then
    for S in GetOptionValues('i','include') do
      FCreator.IncludePaths.Add(S);
  if Hasoption('f','forwardclasses') then
    FCreator.ForwardClasses:=GetOptionValue('f','forwardclasses');
  if (FCreator.HeaderFileName<>'') and Not FileExists(FCreator.HeaderFileName) then
    begin
    PrintUsage(Format('Header file "%s"does not exist',[FCreator.HeaderFileName]));
    Exit;
    end;
  Result:=True;
end;

{ TStubCreatorApplication }

procedure TStubCreatorApplication.DoRun;

begin
  Terminate;
  If not ParseOptions then
    exit;
  FCreator.Execute;
end;

constructor TStubCreatorApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCreator:=TStubCreator.Create(Self);
  StopOnException:=True;
end;

destructor TStubCreatorApplication.Destroy;
begin
  FreeAndNil(FCreator);
  inherited Destroy;
end;

Var
  Application : TStubCreatorApplication;

begin
  Application:=TStubCreatorApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
