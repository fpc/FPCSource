{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    JSONSchema fpcunit tester program

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
program testschema;

uses
  {$ifdef unix}
  cwstring,
  {$endif}
  SysUtils, Classes, jsonparser, consoletestrunner, fpjson.schema.schema, fpjson.schema.consts, fpjson.schema.reader,
  fpjson.schema.loader, fpjson.schema.testutils, utOfficialTests, fpjson.schema.types, utSchemaTypes, utSchema,
  fpjson.schema.writer, utSchemaWriter, fpjson.schema.validator, utSchemaValidator, fpjson.schema.pascaltypes,
  fpjson.schema.codegen, utSchemaPascalTypes;

type

  { TMyTestRunner }

  TMyTestRunner = Class(TTestRunner)
    Constructor Create(aOwner : TComponent); override;
  end;
var
  Application: TTestRunner;

{ TMyTestRunner }

constructor TMyTestRunner.Create(aOwner: TComponent);

var
  aDir: String;

begin
  inherited Create(aOwner);
  Longopts.Add('testdir:');
  aDir:=GetEnvironmentVariable('SCHEMATESTDIR');
  if HasOption(#0,'testdir') then
    aDir:=GetOptionValue(#0,'testdir');
  if aDir<>'' then
    RegisterTestFiles(aDir);
end;

begin
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
