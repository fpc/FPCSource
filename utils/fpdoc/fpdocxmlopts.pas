unit fpdocxmlopts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpdocproj, dom;

Type

  { TXMLFPocOptions }

  { TXMLFPDocOptions }

  TXMLFPDocOptions = Class(TComponent)
  Protected
    Procedure Error(Const Msg : String);
    Procedure Error(Const Fmt : String; Args : Array of Const);
    Procedure LoadPackage(APackage : TFPDocPackage; E : TDOMElement); virtual;
    Procedure LoadPackages(Packages : TFPDocPackages; E : TDOMElement);
    Procedure LoadEngineOptions(Options : TEngineOptions; E : TDOMElement); virtual;
  Public
    Procedure LoadOptionsFromFile(AProject : TFPDocProject; Const AFileName : String);
    Procedure LoadFromXML(AProject : TFPDocProject; XML : TXMLDocument); virtual;
  end;
  EXMLFPdoc = Class(Exception);

implementation

Uses XMLRead;

Resourcestring
  SErrInvalidRootNode = 'Invalid options root node: Got "%s", expected "docproject"';
  SErrNoPackagesNode = 'No "packages" node found in docproject';
  SErrNoInputFile = 'unit tag without file attribute found';
  SErrNoDescrFile = 'description tag without file attribute';

{ TXMLFPDocOptions }

Function IndexOfString(S : String; List : Array of string) : Integer;

begin
  S:=UpperCase(S);
  Result:=High(List);
  While (Result>=0) and (S<>UpperCase(List[Result])) do
    Dec(Result);
end;

procedure TXMLFPDocOptions.Error(Const Msg: String);
begin
  Raise EXMLFPDoc.Create(Msg);
end;

procedure TXMLFPDocOptions.Error(const Fmt: String; Args: array of const);
begin
  Raise EXMLFPDoc.CreateFmt(Fmt,Args);
end;



procedure TXMLFPDocOptions.LoadPackage(APackage: TFPDocPackage; E: TDOMElement);

  Function LoadInput(I : TDOMElement) : String;

  Var
    S : String;

  begin
    Result:=I['file'];
    If (Result='') then
      Error(SErrNoInputFile);
    S:=I['options'];
    if (S<>'') then
      Result:=S+' '+Result;
  end;

  Function LoadDescription(I : TDOMElement) : String;

  Var
    S : String;

  begin
    Result:=I['file'];
    If (Result='') then
      Error(SErrNoDescrFile);
  end;

Const
  OpCount = 0;
  OpNames : Array[0..OpCount] of string
          = ('');
Var
  N,S : TDOMNode;
  O : TDomElement;

begin
  APackage.Name:=E['name'];
  APackage.output:=E['output'];
  APackage.ContentFile:=E['contentfile'];
  N:=E.FirstChild;
  While (N<>Nil) do
    begin
    If (N.NodeType=ELEMENT_NODE) then
      begin
      O:=N as TDOMElement;
      If (O.NodeName='units') then
        begin
        S:=O.FirstChild;
        While (S<>Nil) do
          begin
          If (S.NodeType=Element_Node) and (S.NodeName='unit') then
            APackage.Inputs.add(LoadInput(S as TDomElement));
          S:=S.NextSibling;
          end;
        end
      else If (O.NodeName='descriptions') then
        begin
        S:=O.FirstChild;
        While (S<>Nil) do
          begin
          If (S.NodeType=Element_Node) and (S.NodeName='description') then
            APackage.Descriptions.add(LoadDescription(S as TDomElement));
          S:=S.NextSibling;
          end;
        end
      end;
    N:=N.NextSibling;
    end;
end;

procedure TXMLFPDocOptions.LoadPackages(Packages: TFPDocPackages; E: TDOMElement
  );

Var
  N : TDOMNode;

begin
  N:=E.FirstChild;
  While (N<>Nil) do
    begin
    If (N.NodeName='package') and (N.NodeType=ELEMENT_NODE) then
      LoadPackage(Packages.Add as TFPDocPackage, N as TDOMElement);
    N:=N.NextSibling;
    end;
end;

procedure TXMLFPDocOptions.LoadEngineOptions(Options: TEngineOptions;
  E: TDOMElement);

  Function TrueValue(V : String) : Boolean;

  begin
    V:=LowerCase(V);
    Result:=(v='true') or (v='1') or (v='yes');
  end;

Const
  NCount = 11;
  ONames : Array[0..NCount] of string
         = ('hide-protected','warn-no-node','show-private',
            'stop-on-parser-error', 'ostarget','cputarget',
            'mo-dir','parse-impl','format', 'language',
            'package','dont-trim');

Var
  O : TDOMnode;
  N,V : String;

begin
  O:=E.FirstChild;
  While (O<>Nil) do
    begin
    If (O.NodeType=Element_NODE) and (O.NodeName='option') then
      begin
      N:=LowerCase(TDOMElement(o)['name']);
      V:=TDOMElement(o)['value'];
      Case IndexOfString(N,ONames) of
        0 : Options.HideProtected:=TrueValue(v);
        1 : Options.WarnNoNode:=TrueValue(v);
        2 : Options.ShowPrivate:=TrueValue(v);
        3 : Options.StopOnParseError:=TrueValue(v);
        4 : Options.ostarget:=v;
        5 : Options.cputarget:=v;
        6 : Options.MoDir:=V;
        7 : Options.InterfaceOnly:=Not TrueValue(V);
        8 : Options.Backend:=V;
        9 : Options.Language:=v;
        10 : Options.DefaultPackageName:=V;
        11 : Options.DontTrim:=TrueValue(V);
      else
        Options.BackendOptions.add('--'+n);
        Options.BackendOptions.add(v);
      end;
      end;
    O:=O.NextSibling
    end;
end;

procedure TXMLFPDocOptions.LoadOptionsFromFile(AProject: TFPDocProject;
  const AFileName: String);

Var
  XML : TXMLDocument;

begin
   XMLRead.ReadXMLFile(XML,AFileName);
   try
     LoadFromXML(AProject,XML);
   finally
     FreeAndNil(XML);
   end;
end;

procedure TXMLFPDocOptions.LoadFromXML(AProject: TFPDocProject;
  XML: TXMLDocument);

Var
  E : TDOMElement;
  N : TDomNode;

begin
  E:=XML.DocumentElement;
  if (E.NodeName<>'docproject') then
    Error(SErrInvalidRootNode,[E.NodeName]);
  N:=E.FindNode('packages');
  If (N=Nil) or (N.NodeType<>ELEMENT_NODE) then
    Error(SErrNoPackagesNode);
  LoadPackages(AProject.Packages,N as TDomElement);
  N:=E.FindNode('options');
  If (N<>Nil) and (N.NodeType=ELEMENT_NODE) then
    LoadEngineOptions(AProject.Options,N as TDOMElement);
end;

end.

