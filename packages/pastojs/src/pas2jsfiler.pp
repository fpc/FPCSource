{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

Abstract:
  Write and read a precompiled module.

  Store whole unit, except all
  procedure declarations, proc bodies, finalization/initialization sections are
  replaced by
    -precompiled code
    -lists of references
    -local consts
  The useanalyzer needs the references - TPas2jsUseAnalyzer.

  Due to uses cycles, ability to stop read interface
  ReadContinueImplementation
}
unit Pas2JsFiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPPas2Js, PasResolver, PasTree;

type

  { TPasToJsFiler }

  TPasToJsFiler = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure WriteModule(aResolver: TPasResolver); virtual;
  end;

implementation

{ TPasToJsFiler }

constructor TPasToJsFiler.Create;
begin

end;

destructor TPasToJsFiler.Destroy;
begin
  inherited Destroy;
end;

procedure TPasToJsFiler.WriteModule(aResolver: TPasResolver);
begin

end;

end.

