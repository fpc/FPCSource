{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements a file locator interface

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_locators;

interface

uses
  Classes, SysUtils,
{$IFDEF DELPHI}
  xmldom, sdo_win_xml,
{$ENDIF DELPHI}
{$IFDEF FPC}
 DOM, XMLRead,
{$ENDIF FPC}
 sdo_xsdparser;

type

  { TFileDocumentLocator }

  TFileDocumentLocator = class(TInterfacedObject,IDocumentLocator)
  private
    FBasePath : string;
  protected
    property BasePath : string read FBasePath;
  protected
    function Find(
      const ADocLocation : string;
      out   ADoc : TXMLDocument
    ) : Boolean;
  public
    constructor Create(const ABasePath : string);
  end;

implementation

{ TFileDocumentLocator }

function TFileDocumentLocator.Find(
  const ADocLocation: string;
  out   ADoc: TXMLDocument
) : Boolean;
var
  locFileName : string;
begin
  locFileName := BasePath + ExtractFileName(ADocLocation);
  locFileName := ExpandFileName(locFileName);
  Result := FileExists(locFileName);
  if Result then
    ReadXMLFile(ADoc,locFileName);
end;

constructor TFileDocumentLocator.Create(const ABasePath: string);
begin
  FBasePath := IncludeTrailingPathDelimiter(ABasePath);
end;

end.

