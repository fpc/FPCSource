{
    This file is part of the Free Pascal Utilities
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit reptest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fprepos;

// Num restricted to 1 2 3. 4 will create package with unsatisfied dependency.
Function CreateTestRep(Num : Integer) : TFPRepository;
Procedure FillFirstPackage(P : TFPPackage);
Procedure FillSecondPackage(P : TFPPackage);
Procedure FillThirdPackage(P : TFPPackage);
Procedure FillFourthPackage(P : TFPPackage);

implementation

Procedure FillFirstPackage(P : TFPPackage);

begin
  P.Author:='Michael Van Canneyt';
  P.URL:='http://www.freepascal.org/packages/firstpackage.zip';
  P.Email:='michael@freepascal.org';
  P.Version.AsString:='1.2.3';
  P.Description:='First package in the repository. Provides basic information.';
  P.OSes:=[Win32,linux];
  P.CPUs:=[i386,x86_64];
end;

Procedure FillSecondPackage(P : TFPPackage);

begin
  P.Author:='Peter Vreman';
  P.URL:='http://www.freepascal.org/packages/secondpackage.zip';
  P.Email:='peter@freepascal.org';
  P.Version.AsString:='1.1.0';
  P.Description:='Second package in the repository. Provides extended information. Needs basic information.';
  P.AddDependency('FirstPackage','1.2.3');
  P.OSes:=[linux];
  P.CPUs:=[x86_64];
end;

Procedure FillThirdPackage(P : TFPPackage);

begin
  P.Author:='Florian Klaempfl';
  P.URL:='http://www.freepascal.org/packages/thirdpackage.zip';
  P.Email:='florian@freepascal.org';
  P.Version.AsString:='2.1.0';
  P.Description:='Third package in the repository. Needs basic and extended information.';
  P.AddDependency('FirstPackage','1.0.0');
  P.AddDependency('SecondPackage','1.1.0');
  P.OSes:=[Win32];
  P.CPUs:=[i386];
end;

Procedure FillFourthPackage(P : TFPPackage);

begin
  P.Author:='Bad guy';
  P.URL:='http://www.freepascal.org/packages/bad.zip';
  P.Email:='bad@worse.com';
  P.Version.AsString:='6.6.6';
  P.Description:='Fourth package in the repository. Random dependency.';
  P.AddDependency('FirstPackage','1.0.0');
  P.AddDependency('NonExistingPackage','3.2.1-?');
end;

Function CreateTestRep(Num : integer) : TFPRepository;

begin
  Result:=TFPRepository.Create(Nil);
  FillFirstPackage(Result.AddPackage('FirstPackage'));
  If (Num>1) then
    FillSecondPackage(Result.AddPackage('SecondPackage'));
  If (Num>2) then
    FillThirdPackage(Result.AddPackage('ThirdPackage'));
  If (Num>3) then
    FillFourthPackage(Result.AddPackage('FourthPackage'));
end;

end.

