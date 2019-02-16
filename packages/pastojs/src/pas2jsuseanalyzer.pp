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
  PasUseAnalyzer, PasTree, PasResolver,
  FPPas2Js;

type

  { TPas2JSAnalyzer }

  TPas2JSAnalyzer = class(TPasAnalyzer)
  public
    procedure UseExpr(El: TPasExpr); override;
  end;

implementation

{ TPas2JSAnalyzer }

procedure TPas2JSAnalyzer.UseExpr(El: TPasExpr);

  procedure CheckArgs(Args: TFPList);
  var
    i: Integer;
    ArgType: TPasType;
    ModScope: TPas2JSModuleScope;
  begin
    if Args=nil then exit;
    for i:=0 to Args.Count-1 do
      begin
      ArgType:=TPasArgument(Args[i]).ArgType;
      if ArgType=nil then continue;
      if (ArgType.ClassType=TPasArrayType)
          and (TPasArrayType(ArgType).ElType=nil) then
        begin
        // array of const
        ModScope:=NoNil(Resolver.RootElement.CustomData) as TPas2JSModuleScope;
        if ModScope.SystemVarRecs=nil then
          RaiseNotSupported(20190216104347,El);
        UseProcedure(ModScope.SystemVarRecs);
        break;
        end;
      end;
  end;

var
  Ref: TResolvedReference;
  Decl: TPasElement;
begin
  if El=nil then exit;
  inherited UseExpr(El);

  Ref:=nil;
  if El.CustomData is TResolvedReference then
    begin
    // this is a reference -> mark target
    Ref:=TResolvedReference(El.CustomData);
    Decl:=Ref.Declaration;
    if Decl is TPasProcedure then
      CheckArgs(TPasProcedure(Decl).ProcType.Args)
    else if Decl.ClassType=TPasProperty then
      CheckArgs(Resolver.GetPasPropertyArgs(TPasProperty(Decl)));
    end;
end;

end.

