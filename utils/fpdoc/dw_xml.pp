{
    $Id$

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


unit dw_XML;

interface

uses DOM, PasTree;

function ModuleToXMLStruct(AModule: TPasModule): TXMLDocument;


implementation

function ModuleToXMLStruct(AModule: TPasModule): TXMLDocument;
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

end.


{
  $Log$
  Revision 1.1  2003-03-17 23:03:20  michael
  + Initial import in CVS

  Revision 1.5  2003/03/13 22:02:13  sg
  * New version with many bugfixes and our own parser (now independent of the
    compiler source)

  Revision 1.4  2002/05/24 00:13:22  sg
  * much improved new version, including many linking and output fixes

  Revision 1.3  2002/03/12 10:58:36  sg
  * reworked linking engine and internal structure

  Revision 1.2  2001/07/27 10:21:42  sg
  * Just a new, improved version ;)
    (detailed changelogs will be provided again with the next commits)

  Revision 1.1  2000/10/04 09:17:37  sg
  * First public version

}
