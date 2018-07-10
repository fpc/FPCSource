{ **********************************************************************
  This file is part of the Free Component Library (FCL)
  Copyright (c) 2015 by the Free Pascal development team
        
  REST classes code generator base.
            
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
                   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  **********************************************************************}

unit restcodegen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pascodegen;

Type
  { TRestCodeGenerator }

  TRestCodeGenerator = Class(TPascalCodeGenerator)
  Private
    FBaseClassName: String;
    FBaseListClassName: String;
    FClassPrefix: String;
  Public
    procedure IncIndent;
    procedure DecIndent;
    Procedure LoadFromFile(Const AFileName : string);
    Procedure LoadFromStream(Const AStream : TStream); virtual; abstract;
    Procedure Execute; virtual; abstract;
  Published
    Property BaseClassName : String Read FBaseClassName Write FBaseClassName;
    Property BaseListClassName : String Read FBaseListClassName Write FBaseListClassName;
    Property ClassPrefix : String Read FClassPrefix Write FClassPrefix;
  end;

implementation

{ TRestCodeGenerator }

procedure TRestCodeGenerator.IncIndent;

begin
  Indent;
end;


procedure TRestCodeGenerator.DecIndent;

begin
  UnDent;
end;


procedure TRestCodeGenerator.LoadFromFile(const AFileName: string);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;


end.

