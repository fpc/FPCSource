{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Display unit/program dependencies.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program showdeps;

{$mode objfpc}
{$H+}
 
uses SysUtils, Classes, PParser, PasTree;

 
type
  { We have to override abstract TPasTreeContainer methods.
    See utils/fpdoc/dglobals.pp for an implementation of TFPDocEngine,
    a "real" engine. }
  TSimpleEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;
 
function TSimpleEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
//  Writeln(AName,' : ',AClass.ClassName,' at ',ASourceFilename,':',ASourceLinenumber);
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber; 
end;
 
function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  { dummy implementation, see TFPDocEngine.FindElement for a real example }
  Result := nil;
end;

Procedure PrintUses(aSection : TPasSection; aShowFileName : Boolean = false);

Var
  i : integer;
  aUses : TPasUsesUnit;
  aName : string;
  
begin
  if aSection=Nil then 
    exit;  
  for aUses in aSection.UsesClause do
    begin
    aName:='';
    if aShowFileName and assigned(aUses.InFileName) then
      aName:=AnsiDequotedStr(aUses.InFileName.Value,'''');
    if (aName='') and assigned(aUses.Expr) then
      aName:=aUses.Expr.GetDeclaration(False);  
    if aName='' then
      aName:=aUses.Name;
    Writeln(aName);  
    end;
end;
 
Procedure Usage;

begin
  Writeln('Usage : ',ExtractFileName(Paramstr(0)),' [OPTIONS] options ');
  Writeln('Where options is exactly one of');
  Writeln('-h or --help                 shows this help');
  Writeln('-f or --filename             show actual unit filename, if available'); 
  Writeln('-s or --skip-implementation  Do not show implementation dependencies'); 
  Writeln('All other options are passed as-is to the parser');
  Halt(0);
end;
 
var
  M: TPasModule;
  P : TPasProgram absolute M;
  E: TPasTreeContainer;
  First : String;
  Offset,I: Integer;
  Decls: TFPList;
  cmdline : String;
  SkipImplementation,
  ShowFileName : Boolean;
  
begin
  cmdline:='';
  SkipImplementation:=False;
  ShowFileName:=False;
  First:=ParamStr(1);
  if (ParamCount=0) or (First='-h') or (First='--help') then
    Usage;
  Offset:=1;  
  Case first of
  '-f',
  '--filename':
    ShowFileName:=True;
  '-s',
  '--skip-implementation':
    SkipImplementation:=True;
  else
    Offset:=0;  
  end;
  For I:=1+Offset to ParamCount do
    CmdLine:=CmdLine+' '+Paramstr(i);
  E := TSimpleEngine.Create;
  M := nil;
  try
    M := ParseSource(E, cmdline, 'linux', 'i386');
    PrintUses(M.InterfaceSection);
    if not SkipImplementation then
      PrintUses(M.ImplementationSection);
    if M is TPasProgram then
      PrintUses(P.ProgramSection,ShowFilename);
  finally
    FreeAndNil(M);
    FreeAndNil(E)
  end;
end.
