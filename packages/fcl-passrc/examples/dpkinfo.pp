{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt (michael@freepascal.org)

    Unit to parse and keep info about a package file.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit dpkinfo;
{$ENDIF}

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Pascal.Tree, Pascal.Parser, Xml.Dom, Xml.Writer;
{$ELSE}
  Classes, SysUtils, pastree, pparser, dom, XMLWrite;
{$ENDIF}

Type
  { TPackageContainer }
  TInfoKind = (ikUnknown,ikRequires,ikFiles,ikPaths);

  TPackageContainer = class(TPasTreeContainer)
  Public
    function FindElement(const AName: String): TPasElement; override;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;overload; override;
  end;

  { TPackageInfo }

  TPackageInfo = class(TComponent)
  private
    FDefines: TStrings;
    FKnownPackages: TStrings;
    FOutput: TStrings;
    FOutputFile: String;
    FUseAbsolute: Boolean;
    FPackageDir : String;
    class function IsAbsoluteWindowsFile(aFile: String): Boolean;
    procedure WriteFiles(Pack: TPasDynamicPackage);
    procedure WritePaths(Pack: TPasDynamicPackage);
    procedure WriteRequires(Pack: TPasDynamicPackage);
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure ShowInfo(const aInputFile: String; aKind : TInfoKind);
    Property KnownPackages : TStrings Read FKnownPackages;
    Property Output : TStrings Read FOutput;
    Property Defines : TStrings Read FDefines;
    Property UseAbsolute : Boolean Read FUseAbsolute Write FUseAbsolute;
  end;

  { TSimpleParser }

  TSimpleParser = Class
    function ParseSource(AEngine: TPasTreeContainer;
                        const FPCCommandLine : Array of String;
                        Defines : TStrings): TPasModule;
  private
    procedure DoIt(Sender: TObject; const aFileName: String; aOptions: TStrings);
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Pascal.Scanner;
{$ELSE}
uses pscanner;
{$ENDIF}

function TSimpleParser.ParseSource(AEngine: TPasTreeContainer;
                     const FPCCommandLine : Array of String;
                     Defines : TStrings): TPasModule;

var
  FileResolver: TBaseFileResolver;
  Parser: TPasParser;
  Filename: String;
  Scanner: TPascalScanner;

  procedure ProcessCmdLinePart(S : String);
  var
    l,Len: Integer;

  begin
    if (S='') then
      exit;
    Len:=Length(S);
    if (s[1] = '-') and (len>1) then
    begin
      case s[2] of
        'd': // -d define
          Scanner.AddDefine(UpperCase(Copy(s, 3, Len)));
        'u': // -u undefine
          Scanner.RemoveDefine(UpperCase(Copy(s, 3, Len)));
        'F': // -F
          if (len>2) and (s[3] = 'i') then // -Fi include path
            FileResolver.AddIncludePath(Copy(s, 4, Len));
        'I': // -I include path
          FileResolver.AddIncludePath(Copy(s, 3, Len));
        'S': // -S mode
          if  (len>2) then
            begin
            l:=3;
            While L<=Len do
              begin
              case S[l] of
                'c' : Scanner.Options:=Scanner.Options+[po_cassignments];
                'd' : Scanner.SetCompilerMode('DELPHI');
                '2' : Scanner.SetCompilerMode('OBJFPC');
                'h' : ; // do nothing
              end;
              inc(l);
              end;
            end;
        'M' :
           begin
           delete(S,1,2);
           Scanner.SetCompilerMode(S);
           end;
      end;
    end else
      if Filename <> '' then
        raise ENotSupportedException.Create(SErrMultipleSourceFiles)
      else
        Filename := s;
  end;

var
  S: String;

begin
  if DefaultFileResolverClass=Nil then
    raise ENotImplemented.Create(SErrFileSystemNotSupported);
  Result := nil;
  FileResolver := nil;
  Scanner := nil;
  Parser := nil;
  try
    FileResolver := DefaultFileResolverClass.Create;
    {$ifdef HasStreams}
    if FileResolver is TFileResolver then
      TFileResolver(FileResolver).UseStreams:=poUseStreams in Options;
    {$endif}
    Scanner := TPascalScanner.Create(FileResolver);
    Scanner.LogEvents:=AEngine.ScannerLogEvents;
    Scanner.OnLog:=AEngine.OnLog;
    Scanner.RegisterResourceHandler(['res'],@DoIt);
    For S in Defines do
      Scanner.AddDefine(S);
    Parser := TPasParser.Create(Scanner, FileResolver, AEngine);
    Parser.ImplicitUses.Clear;
    Filename := '';
    Parser.LogEvents:=AEngine.ParserLogEvents;
    Parser.OnLog:=AEngine.OnLog;

    For S in FPCCommandLine do
      ProcessCmdLinePart(S);
    if Filename = '' then
      raise Exception.Create(SErrNoSourceGiven);
{$IFDEF HASFS}
    FileResolver.AddIncludePath(ExtractFilePath(FileName));
{$ENDIF}
    Scanner.OpenFile(Filename);
    Parser.ParseMain(Result);
  finally
    Parser.Free;
    Scanner.Free;
    FileResolver.Free;
  end;
end;

procedure TSimpleParser.DoIt(Sender: TObject; const aFileName: String; aOptions: TStrings);
begin
  // Do nothing
end;

{ TPackageInfo }

constructor TPackageInfo.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FKnownPackages:=TStringList.Create;
  Foutput:=TStringList.Create;
  FDefines:=TStringList.Create;
end;

destructor TPackageInfo.Destroy;
begin
  FreeAndNil(FKnownPackages);
  FreeAndNil(FOutput);
  FreeAndNil(FDefines);
  inherited Destroy;
end;

procedure TPackageInfo.WriteRequires(Pack : TPasDynamicPackage);

var
  I : Integer;
  aPack : TPasRequiredPackage;

begin
  For I:=0 to Pack.PackageSection.Requires.Count-1 do
    begin
    aPack:=TPasRequiredPackage(Pack.PackageSection.Requires[i]);
    if FKnownPackages.IndexOf(aPack.Name)=-1 then
      FOutput.Add(aPack.Name);
    end;
end;

class function TPackageInfo.IsAbsoluteWindowsFile(aFile: String): Boolean;
begin
  Result:=(aFile<>'') and (aFile[2]=':') and (aFile[3]='\');
end;

procedure TPackageInfo.WriteFiles(Pack : TPasDynamicPackage);


var
  aUsed : TPasUsesUnit;
  aName,aFileName : String;
  isAbsolute : Boolean;

begin
  For aUsed in Pack.PackageSection.UsesClause do
    begin
    aName:=aUsed.Name;
    if (aName='') then
      continue;
    if assigned(aUsed.InFileName) then
      begin
      aFileName:=aUsed.InFilename.Value;
      aFileName:=StringReplace(aFileName,'''','',[rfReplaceAll]);
      if IsAbsoluteWindowsFile(aFileName) then
        isAbsolute:=True
      else
        begin
        aFileName:=StringReplace(aFilename,'\','/',[rfReplaceAll]);
        isAbsolute:=aFileName[1]='/';
        end
      end
    else
      begin
      aFileName:=aName+'.pas'; // Should not happen
      isAbsolute:=False;
      end;
    if (not IsAbsolute) and UseAbsolute then
      aFileName:=FPackageDir+aFileName;
    FOutput.Add(aFileName);
    end;
end;

procedure TPackageInfo.WritePaths(Pack : TPasDynamicPackage);


var
  aUsed : TPasUsesUnit;
  aName,aFileName : String;
  isAbsolute : Boolean;
  Paths : TStrings;

begin
  Paths:=TStringList.Create;
  For aUsed in Pack.PackageSection.UsesClause do
    begin
    aName:=aUsed.Name;
    if (aName='') then
      continue;
    if assigned(aUsed.InFileName) then
      begin
      aFileName:=aUsed.InFilename.Value;
      aFileName:=StringReplace(aFileName,'''','',[rfReplaceAll]);
      if IsAbsoluteWindowsFile(aFileName) then
        isAbsolute:=True
      else
        begin
        aFileName:=ExtractFilePath(StringReplace(aFilename,'\','/',[rfReplaceAll]));
        isAbsolute:=(aFileName<>'') and (aFileName[1]='/');
        end
      end
    else
      begin
      aFileName:=''; // Should not happen
      isAbsolute:=False;
      end;
    if (not IsAbsolute) and UseAbsolute then
      aFileName:=FPackageDir+aFileName;
    if (aFileName<>'') and (Paths.IndexOf(aFileName)=-1) then
      begin
      FOutput.Add(aFileName);
      Paths.Add(aFileName);
      end;
    end;
end;



procedure TPackageInfo.ShowInfo(const aInputFile: String; aKind: TInfoKind);

Var
  El : TPasElement;
  Pack : TPasDynamicPackage absolute El;
  C : TPackageContainer;
  Parser : TSimpleParser;

begin
  Foutput.Clear;
  FPackageDir:=ExtractFilePath(ExpandFileName(aInputFile));
  Parser:=nil;
  C:=TPackageContainer.Create;
  try
    Parser:=TSimpleParser.Create;
    El:=Parser.ParseSource(C,['-Sd',aInputFile],Defines);
    if not (El is TPasDynamicPackage) then
      Raise EPasTree.CreateFmt('%s is not a package source file. Got a %s instead.',[aInputFile,Pack.ClassName]);
    Case aKind of
      ikRequires : WriteRequires(Pack);
      ikPaths : WritePaths(Pack);
      ikFiles : WriteFiles(Pack);
    end;
  finally
    Parser.Free;
    El.Free;
    C.Free;
  end;
end;

{ TPackageContainer }

function TPackageContainer.FindElement(const AName: String): TPasElement;
begin
  Result:=Nil;
end;

function TPackageContainer.CreateElement(AClass: TPTreeElement; const AName: String; AParent: TPasElement;
  AVisibility: TPasMemberVisibility; const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result:=aClass.Create(aName,aParent);
  Result.Visibility:=AVisibility;
  // ASourceFilename, ASourceLinenumber ?
end;


end.

