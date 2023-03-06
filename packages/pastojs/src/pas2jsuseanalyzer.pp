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

Works:
- Array of Const marks function System.VarRecs()
- TPascalDescendantOfExt.Create marks class method NewInstance

}
{$IFNDEF FPC_DOTTEDUNITS}
unit Pas2jsUseAnalyzer;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}
{$inline on}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes,
  Pascal.UseAnalyzer, Pascal.Tree, Pascal.Resolver,
  Pas2Js.Compiler.Transpiler;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes,
  PasUseAnalyzer, PasTree, PasResolver,
  FPPas2Js;
{$ENDIF FPC_DOTTEDUNITS}

type

  { TPas2JSAnalyzer }

  TPas2JSAnalyzer = class(TPasAnalyzer)
  public
    procedure UseExpr(El: TPasExpr); override;
    procedure UseConstructor(Proc: TPasConstructor; PosEl: TPasElement); virtual;
  end;

implementation

{ TPas2JSAnalyzer }

procedure TPas2JSAnalyzer.UseExpr(El: TPasExpr);

  procedure CheckArgs(Args: TFPList);
  var
    i: Integer;
    ArgType: TPasType;
    ModScope: TPas2JSModuleScope;
    aMod: TPasModule;
    SystemVarRecs: TPasFunction;
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
        aMod:=El.GetModule;
        ModScope:=NoNil(aMod.CustomData) as TPas2JSModuleScope;
        SystemVarRecs:=ModScope.SystemVarRecs;
        if SystemVarRecs=nil then
          RaiseNotSupported(20190216104347,El);
        MarkImplScopeRef(El,SystemVarRecs,psraRead);
        UseProcedure(SystemVarRecs);
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
      begin
      CheckArgs(TPasProcedure(Decl).ProcType.Args);
      if Decl.ClassType=TPasConstructor then
        UseConstructor(TPasConstructor(Decl),El);
      end
    else if Decl.ClassType=TPasProperty then
      CheckArgs(Resolver.GetPasPropertyArgs(TPasProperty(Decl)));
    end;
end;

procedure TPas2JSAnalyzer.UseConstructor(Proc: TPasConstructor;
  PosEl: TPasElement);
var
  ClassScope: TPas2JSClassScope;
begin
  if Proc.Parent.ClassType=TPasClassType then
    begin
    ClassScope:=TPasClassType(Proc.Parent).CustomData as TPas2JSClassScope;
    repeat
      if ClassScope.NewInstanceFunction<>nil then
        begin
        UseProcedure(ClassScope.NewInstanceFunction);
        break;
        end;
      ClassScope:=ClassScope.AncestorScope as TPas2JSClassScope;
    until ClassScope=nil;
    end;
  if PosEl=nil then ;
end;

end.

