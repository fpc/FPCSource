{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2018 by Maciej Izak (hnb),
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains tests for the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Acknowledgment

    Thanks to Sphere 10 Software (http://sphere10.com) for sponsoring
    many new types, tests and major refactoring of entire library

 **********************************************************************}

unit tests.generics.bugs;

{$mode delphi}

interface

uses
  fpcunit, testregistry, testutils,
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type

  { TTestBugs }

  TTestBugs = class(TTestCase)
  published
    procedure Test_QuadraticProbing_InfinityLoop;
    procedure Test_GetEqualityComparer;
  end;

implementation

{ TTestBugs }

procedure TTestBugs.Test_QuadraticProbing_InfinityLoop;
// https://github.com/maciej-izak/generics.collections/issues/4
var
  LMap: TOpenAddressingQP<string, pointer, TDelphiHashFactory>;
begin
  LMap := TOpenAddressingQP<string, pointer, TDelphiHashFactory>.Create();
  LMap.Add(#178#178#107#141#143#151#168#39#172#38#83#194#130#90#101, nil);
  LMap.Add(#193#190#172#41#144#231#52#62#45#117#108#45#217#71#77, nil);
  LMap.Add(#49#116#202#160#38#131#41#37#217#171#227#215#122#151#71, nil);
  LMap.Add(#148#159#199#71#198#97#69#201#116#45#195#184#178#129#200, nil);
  CheckEquals(false, LMap.ContainsKey(#$E6'h=fzb'#$E5#$B4#$A0#$C4#$E6'B6r>'));
  LMap.Free;
end;

procedure TTestBugs.Test_GetEqualityComparer;
begin
  TDelphiQuadrupleHashFactory.GetHashService.LookupEqualityComparer(TypeInfo(Integer), SizeOf(Integer));
end;

begin
  RegisterTest(TTestBugs);
end.

