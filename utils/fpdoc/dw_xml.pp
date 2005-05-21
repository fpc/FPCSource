{

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * 'XML struct' output generator

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$mode objfpc}
{$H+}


unit dw_XML;

interface

uses DOM, PasTree, dwriter;

Type

  { TXMLWriter }

  TXMLWriter = Class(TFPDocWriter)
    function ModuleToXMLStruct(AModule: TPasModule): TXMLDocument;
    Procedure WriteDoc; override;
  end;




implementation

function TXMLWriter.ModuleToXMLStruct(AModule: TPasModule): TXMLDocument;

var
  ModuleElement: TDOMElement;

  procedure ProcessProcedure(Proc: TPasProcedure; Element: TDOMElement);
  var
    ProcEl: TDOMElement;
  begin
    ProcEl := Result.CreateElement(Proc.TypeName);
    Element.AppendChild(ProcEl);
    ProcEl['name'] := Proc.Name;
  end;

  procedure ProcessVariable(AVar: TPasVariable; Element: TDOMElement);
  var
    VarEl: TDOMElement;
  begin
    VarEl := Result.CreateElement('var');
    Element.AppendChild(VarEl);
    VarEl['name'] := AVar.Name;
  end;

  procedure ProcessSection(ASection: TPasSection; const Name: DOMString);
  var
    Element, UsesElement, UnitElement: TDOMElement;
    i: Integer;
    Decl: TPasElement;
  begin
    Element := Result.CreateElement(Name);
    ModuleElement.AppendChild(Element);
    if ASection.UsesList.Count > 0 then
    begin
      UsesElement := Result.CreateElement('uses');
      Element.AppendChild(UsesElement);
      for i := 0 to ASection.UsesList.Count - 1 do
      begin
        UnitElement := Result.CreateElement('unit-ref');
        UnitElement['name'] := TPasType(ASection.UsesList[i]).Name;
        UsesElement.AppendChild(UnitElement);
      end;
    end;

    for i := 0 to ASection.Declarations.Count - 1 do
    begin
      Decl := TPasElement(ASection.Declarations[i]);
      if Decl.InheritsFrom(TPasProcedure) then
        ProcessProcedure(TPasProcedure(Decl), Element)
      else if Decl.ClassType = TPasVariable then
        ProcessVariable(TPasVariable(Decl), Element);
    end;
  end;


begin
  Result := TXMLDocument.Create;
  Result.AppendChild(Result.CreateComment(' Generated using FPDoc - (c) 2000-2003 Sebastian Guenther, sg@freepascal.org '));
  Result.AppendChild(Result.CreateElement('fp-refdoc'));
  ModuleElement := Result.CreateElement('unit');
  ModuleElement['name'] := AModule.Name;
  Result.DocumentElement.AppendChild(ModuleElement);
  ProcessSection(AModule.InterfaceSection, 'interface');
end;

{ TXMLWriter }

procedure TXMLWriter.WriteDoc;
begin

end;

initialization
  // Do not localize.
  RegisterWriter(TXMLWriter,'xml','fpdoc XML output.');
finalization
  UnRegisterWriter('xml');
end.
