{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Prefix units in uses clause of a single program or unit.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program prefixunits;

{$mode objfpc}
{$H+}
 
uses cwstring, SysUtils, Classes, custapp, prefixer;

type

   { TApplication }

   TApplication = class(TCustomApplication)
   Private
     FPrefixer : TPrefixer;
   Public
     Constructor create(aOwner : TComponent); override;
     Destructor Destroy; override;
     Procedure Usage(Err : string);
     Procedure DoRun; override;
   end;

constructor TApplication.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FPrefixer:=TPrefixer.Create(Self);
end;

destructor TApplication.Destroy;
begin
  FreeAndNil(FPrefixer);
  inherited Destroy;
end;

procedure TApplication.Usage(Err: string);

begin
  if Err<>'' then
    Writeln('Error: ',Err);
  Writeln('Usage: ',ExtractFileName(Paramstr(0)),' [OPTIONS] filename');
  Writeln('Where options is exactly one of');
  Writeln('-h or --help                      shows this help');
  Writeln('-f or --filename=FILE             Filename to process, can be specifid simply');
  Writeln('-d or --dest-filename=FILE        Destination filename to produce. Default is namespace.filename');
  Writeln('-k or --known-namespaces=FILE     Name of file with known namespaces.');
  Writeln('-n or --namespace=NS              Namespace to apply to file.');
  Writeln('-o or --option=option             Option to pass to compiler.');
  Writeln('-b or --backup                    Create backups of existing files when overwriting.');
  writeln('-p --program                      Assume the sources are a program. This will not write a dotted file. Namespace is not needed');
  Writeln('-r or --replace                   Default is to create an include and use a define to separate dotted from non-dotted');
  Writeln('                                  Use this to replace the units clause as-is.');
  Writeln('All other options are passed as-is to the parser');
  Halt(Ord(Err<>''));
end;

procedure TApplication.DoRun;

Const
  ShortOpts = 'bhf:k:n:o:d:rp';
  LongOpts : Array of string = ('backup','filename:','known-namespaces:','namespace:','option:','dest-filename:','replace','program');

Var
  S : String;
  Opts : Array of string;
  Prog : Boolean;

begin
  Terminate;
  S:=CheckOptions(Shortopts,LongOpts);
  if (S<>'') or HasOption('h','help') then
    Usage(S);
  FPrefixer.NameSpace:=GetOptionValue('n','namespace');
  if HasOption('r','replace') then
    FPrefixer.UnitFileMode :=fmReplace;
  FPrefixer.FileName:=GetOptionValue('f','filename');
  FPrefixer.DestFileName:=GetOptionValue('d','dest-filename');
  FPrefixer.CreateBackups:=HasOption('b','backup');
  if FPrefixer.FileName='' then
    begin
    Opts:=GetNonOptions(ShortOpts,LongOpts);
    if (Length(Opts)>0) then
      FPrefixer.FileName:=Opts[0];
    end;
  Opts:=GetOptionValues('o','option');
  Prog:=HasOption('p','program');
  For S in Opts do
    FPrefixer.Params.Add(S);
  if Not Prog and (FPrefixer.NameSpace='') then
    Usage('Namespace is required');
  FPrefixer.SkipDestFileName:=Prog;
  if (FPrefixer.FileName='') then
    Usage('Filename is required');
  S:=GetOptionValue('k','known-namespaces');
  if S='' then
    S:=ExtractFilePath(ParamStr(0))+'knownprefixes.txt';
  if FileExists(S) then
    FPrefixer.KnownNameSpaces.LoadFromFile(S);
  FPrefixer.Execute;
end;

begin
  With TApplication.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free;
    end;
end.
