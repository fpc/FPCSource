{$IFNDEF FPC_DOTTEDUNITS}
unit dpktolpk;
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

  TPackageContainer = class(TPasTreeContainer)
  Public
    function FindElement(const AName: String): TPasElement; override;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;overload; override;
  end;

  { TDPK2LPKConverter }

  TDPK2LPKConverter = class(TComponent)
  private
    FKnownPackages: TStrings;
    FUpdateKnown: Boolean;
    procedure AddRequires(aXML: TXMLDocument; aRequired: TDOMElement; aSection: TPasPackageSection);
    procedure AddUses(aXML: TXMLDocument; aFiles: TDOMElement; aSection: TPasPackageSection);
    procedure CreatePackageXML(aXML: TXMLDocument; aName: string; out aFiles, aRequired: TDOMElement);
    procedure WriteLPK(Pack: TPasDynamicPackage; aOUtputFile: String);
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Convert(const aInputFile,aOutputFile : String);
    Property KnownPackages : TStrings Read FKnownPackages;
    Property UpdateKnown : Boolean Read FUpdateKnown Write FUpdateKnown;
  end;

  { TSimpleParser }

  TSimpleParser = Class
    function ParseSource(AEngine: TPasTreeContainer;
                        const FPCCommandLine : Array of String;
                        OSTarget, CPUTarget: String;
                        Options : TParseSourceOptions): TPasModule;
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
                     OSTarget, CPUTarget: String;
                     Options : TParseSourceOptions): TPasModule;

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
    if not (poSkipDefaultDefs in Options) then
      begin
      Scanner.AddDefine('FPK');
      Scanner.AddDefine('FPC');
      // TargetOS
      s := UpperCase(OSTarget);
      Scanner.AddDefine(s);
      Case s of
        'LINUX' : Scanner.AddDefine('UNIX');
        'FREEBSD' :
          begin
          Scanner.AddDefine('BSD');
          Scanner.AddDefine('UNIX');
          end;
        'NETBSD' :
          begin
          Scanner.AddDefine('BSD');
          Scanner.AddDefine('UNIX');
          end;
        'SUNOS' :
          begin
          Scanner.AddDefine('SOLARIS');
          Scanner.AddDefine('UNIX');
          end;
        'GO32V2' : Scanner.AddDefine('DPMI');
        'BEOS' : Scanner.AddDefine('UNIX');
        'QNX' : Scanner.AddDefine('UNIX');
        'AROS' : Scanner.AddDefine('HASAMIGA');
        'MORPHOS' : Scanner.AddDefine('HASAMIGA');
        'AMIGA' : Scanner.AddDefine('HASAMIGA');
      end;
      // TargetCPU
      s := UpperCase(CPUTarget);
      Scanner.AddDefine('CPU'+s);
      if (s='X86_64') then
        Scanner.AddDefine('CPU64')
      else
        Scanner.AddDefine('CPU32');
      end;
    Parser := TPasParser.Create(Scanner, FileResolver, AEngine);
    if (poSkipDefaultDefs in Options) then
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

{ TDPK2LPKConverter }

procedure TDPK2LPKConverter.CreatePackageXML(aXML: TXMLDocument; aName: string; out aFiles, aRequired: TDOMElement);

  Function AppendEl(aParent : TDOMElement; aTag : String; aValue : String = '') : TDOMelement;
  begin
    Result:=aXML.CreateElement(UTF8Decode(aTag));
    if Assigned(aParent) then
      AParent.AppendChild(Result);
    if aValue<>'' then
      Result.AttribStrings['Value']:=UTF8Decode(aValue);
  end;

var
  Cfg, Pck : TDOMElement;
  El,Sub : TDomElement;


begin
  Cfg:=AppendEl(Nil,'CONFIG');
  aXML.AppendChild(Cfg);
    Pck:=AppendEl(Cfg,'Package');
    Pck.AttribStrings['Version']:='5';
      AppendEl(Pck,'Name',aName);
      AppendEl(Pck,'Type','RunTime');
      AppendEl(Pck,'Author','');
      El:=AppendEl(Pck,'CompilerOptions','');
        AppendEl(El,'Version','11');
        Sub:=AppendEl(El,'SearchPaths','');
          AppendEl(Sub,'UnitOutputDirectory','lib/$(TargetCPU)-$(TargetOS)');
      AppendEl(Pck,'Description','');
      AppendEl(Pck,'Licence','');
      El:=AppendEl(Pck,'Version');
      El.AttribStrings['Major']:='1';
      aFiles:=AppendEl(Pck,'Files','');
      aRequired:=AppendEl(Pck,'RequiredPkgs');
      El:=AppendEl(Pck,'UsageOptions');
        El:=AppendEl(El,'UnitPath');
        EL.AttribStrings['Value']:='$(PkgOutDir)';
      El:=AppendEl(Pck,'PublishOptions');
        Sub:=AppendEl(El,'Version');
        Sub.AttribStrings['Value']:='2';
        Sub:=AppendEl(El,'UseFileFilters');
        Sub.AttribStrings['Value']:='True';
end;


procedure TDPK2LPKConverter.AddUses(aXML : TXMLDocument; aFiles: TDOMElement; aSection : TPasPackageSection);

var
  aUsed : TPasUsesUnit;
  aName,aFileName : String;
  Itm,El : TDomElement;

begin
  For aUsed in aSection.UsesClause do
    begin
    aName:=aUsed.Name;
    if (aName<>'') then
      begin
      if assigned(aUsed.InFileName) then
        begin
        aFileName:=StringReplace(aUsed.InFilename.Value,'''','',[rfReplaceAll]);
        aFileName:=StringReplace(aFilename,'\','/',[rfReplaceAll]);
        end
      else
        aFileName:=aName+'.pas'; // Should not happen
      Itm:=aXML.CreateElement('Item');
      aFiles.AppendChild(Itm);
      El:=aXML.CreateElement('Filename');
      Itm.AppendChild(El);
      El.AttribStrings['Value']:=UTF8Decode(aFileName);
      El:=aXML.CreateElement('UnitName');
      Itm.AppendChild(El);
      El.AttribStrings['Value']:=UTF8Decode(aName);
      end;
    end;
end;

procedure TDPK2LPKConverter.AddRequires(aXML : TXMLDocument; aRequired: TDOMElement; aSection : TPasPackageSection);

var
  I : Integer;
  Itm,El : TDomElement;
  aPack : TPasRequiredPackage;
  aName : String;
  Added : TStringList;

begin
  Added:=TStringList.Create();
  try
    Added.Sorted:=True;
    For I:=0 to aSection.Requires.Count-1 do
      begin
      aPack:=TPasRequiredPackage(aSection.Requires[i]);
      aName:=FKnownPackages.Values[aPack.Name];
      if (aName<>'') and (Added.IndexOf(aName)=-1) then
        begin
        Itm:=aXML.CreateElement('Item');
        aRequired.AppendChild(Itm);
        El:=aXML.CreateElement('PackageName');
        Itm.AppendChild(El);
        El.AttribStrings['Value']:=UTF8Decode(aName);
        Added.Add(aName);
        end;
      end;
  finally
    Added.Free;
  end;
end;

procedure TDPK2LPKConverter.WriteLPK(Pack : TPasDynamicPackage; aOUtputFile : String);

var
  aSection : TPasPackageSection;
  XML : TXMLDocument;
  aFiles,aRequired : TDOMElement;

begin
  XML:=TXMLDocument.Create;
  try
    // Create skeleton package
    CreatePackageXML(XML,Pack.Name,aFiles,aRequired);
    aSection:=Pack.PackageSection;
    // Add Uses
    AddUses(XML,aFiles,aSection);
    // Add Requires
    AddRequires(XML,aRequired,aSection);
    // Write file
    WriteXML(XML,aOUtputFile);
  finally
    XML.Free;
  end;

end;

constructor TDPK2LPKConverter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FKnownPackages:=TStringList.Create;
end;

destructor TDPK2LPKConverter.Destroy;
begin
  FreeAndNil(FKnownPackages);
  inherited Destroy;
end;

procedure TDPK2LPKConverter.Convert(const aInputFile, aOutputFile: String);

Var
  El : TPasElement;
  Pack : TPasDynamicPackage absolute El;
  C : TPackageContainer;
  Parser : TSimpleParser;

begin
  Parser:=nil;
  C:=TPackageContainer.Create;
  try
    Parser:=TSimpleParser.Create;
    El:=Parser.ParseSource(C,['-Sd',aInputFile],'linux','x86_64',[]);
    if not (El is TPasDynamicPackage) then
      Raise EPasTree.CreateFmt('%s is not a package source file. Got a %s instead.',[aInputFile,Pack.ClassName]);
    WriteLPK(Pack,aOutputFile);
    if UpdateKnown then
      FKnownPackages.Values[Pack.Name]:=Pack.Name;
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

