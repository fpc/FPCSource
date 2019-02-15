{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2019  Mattias Gaertner  mattias@freepascal.org

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Abstract:
    Extends the FCL Pascal use analyzer for the language subset of pas2js.
}
unit Pas2jsUseAnalyzer;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils,
  PasUseAnalyzer, PasTree,
  FPPas2Js;

type

  { TPas2JSAnalyzer }

  TPas2JSAnalyzer = class(TPasAnalyzer)
  public
    function UseModule(aModule: TPasModule; Mode: TPAUseMode): boolean;
      override;
  end;

implementation

{ TPas2JSAnalyzer }

function TPas2JSAnalyzer.UseModule(aModule: TPasModule; Mode: TPAUseMode
  ): boolean;
var
  ModScope: TPas2JSModuleScope;
begin
  Result:=inherited UseModule(aModule, Mode);
  if not Result then exit;
  ModScope:=aModule.CustomData as TPas2JSModuleScope;
  if ModScope.SystemVarRecs<>nil then
    UseProcedure(ModScope.SystemVarRecs);
end;

end.

