{ ---------------------------------------------------------------------
  This is a simple program to check whether fcl-passrc

  ---------------------------------------------------------------------}

program parsepp;

{$mode objfpc}{$H+}
 
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
 
Procedure Usage;

begin
  Writeln('Usage : ',ExtractFileName(Paramstr(0)),' [-h|--help] options ');
  Writeln('-h or --help shows this help');
  Writeln('All other options are passed as-is to the parser');
  Halt(0);
end;
 
var
  M: TPasModule;
  E: TPasTreeContainer;
  I: Integer;
  Decls: TFPList;
  cmdline : String;
  
begin
  cmdline:='';
  if (ParamCount=0) or (Paramstr(1)='-h') or (Paramstr(1)='--help') then
    Usage;
  For I:=1 to ParamCount do
    CmdLine:=CmdLine+' '+Paramstr(i);
  E := TSimpleEngine.Create;
  try
    M := ParseSource(E, cmdline, 'linux', 'i386');
 
    { Cool, we successfully parsed the module.
      Now output some info about it. }
    if M.InterfaceSection <> nil then
    begin
      Decls := M.InterfaceSection.Declarations;
      for I := 0 to Decls.Count - 1 do
        Writeln('Interface item ', I, ': ' +
          (TObject(Decls[I]) as TPasElement).Name);
    end else
      Writeln('No interface section --- this is not a unit, this is a ', M.ClassName);
 
    if M.ImplementationSection <> nil then // may be nil in case of a simple program
    begin
      Decls := M.ImplementationSection.Declarations;
      for I := 0 to Decls.Count - 1 do
        Writeln('Implementation item ', I, ': ' +
          (TObject(Decls[I]) as TPasElement).Name);
    end;
 
    FreeAndNil(M);
  finally FreeAndNil(E) end;
end.