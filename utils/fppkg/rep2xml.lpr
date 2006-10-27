{
    This file is part of the Free Pascal Utilities
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$H+}
program rep2xml;

uses
  Classes, fpxmlrep, pkgropts, fprepos, streamcoll, reptest;


Procedure TestVersionCompare;

  Procedure CV(V1,V2 : TFPVersion);
  
  begin
    if V1.CompareVersion(V2)>0 then
      Writeln('Error : ',V1.AsString,'>',V2.AsString);
  end;
  

Var
  V1,V2 : TFPVersion;

begin
  Writeln('Testing version comparison');
  V1:=TFPVersion.Create;
  Try
    V1.AsString:='1.2.3';
    V2:=TFPVersion.Create;
    Try
      V2.AsString:='1.2.3-b';
      CV(V1,V2);
      V2.AsString:='1.2.4';
      CV(V1,V2);
      V2.AsString:='1.3.0';
      CV(V1,V2);
      V2.AsString:='2.0.0';
      CV(V1,V2);
      V2.AsString:=V1.AsString;
      If V1.CompareVersion(V2)<>0 then
        Writeln('Error : ',V1.AsString,'<>',V2.AsString);
    Finally
      V2.Free;
    end;
  Finally
    V1.Free;
  end;
end;

Procedure ComparePackages(P1,P2 : TFPPackage);

Var
  i : Integer;


begin
  If P1.Name<>P2.Name then
    Writeln('Names differ: ',P1.Name,'<>',P2.Name);
  If P1.Version.CompareVersion(P2.Version)<>0 then
    Writeln('Versions differ: "',P1.Version.AsString,'"<>"',P2.Version.AsString,'"');
  If P1.Author<>P2.Author then
    Writeln('Authors differ: ',P1.Author,'"<>"',P2.Author,'"');
  If P1.URL<>P2.URL then
    Writeln('URLs differ: "',P1.URL,'"<>"',P2.URL,'"');
  If P1.Email<>P2.Email then
    Writeln('Emails differ: "',P1.Email,'"<>"',P2.Email,'"');
  If P1.Description<>P2.Description then
    Writeln('Descriptions differ: "',P1.Description,'"<>"',P2.Description,'"');
  if P1.OSes<>P2.OSes then
    Writeln('OSes differ');
  if P1.CPUs<>P2.CPUs then
    Writeln('CPUs differ');
  If P1.HasDependencies<>P2.HasDependencies then
    Writeln('HasDependencies differ: "',P1.HasDependencies,'"<>"',P2.HasDependencies,'"');
  If P1.HasDependencies then
    begin
    If P1.Dependencies.Count<>P2.Dependencies.Count then
      Writeln('Dependency counts differ: "',P1.Dependencies.Count,'"<>"',P2.Dependencies.Count,'"')
    else
      For I:=0 to P1.Dependencies.Count-1 do
        begin
        If P1.Dependencies[i].PackageName<>P2.Dependencies[i].PackageName then
          Writeln('Dependency ',I,' name differs :"',P1.Dependencies[i].PackageName,'"<>"',P2.Dependencies[i].PackageName,'"');
        If P1.Dependencies[i].Minversion.CompareVersion(P2.Dependencies[i].MinVersion)<>0 then
          Writeln('Dependency ',I,' minversion differs :"',P1.Dependencies[i].MinVersion.AsString,'"<>"',P2.Dependencies[i].MinVersion.AsString,'"');
        end;
    end;
end;

Procedure TestPackageStream1;

Var
  P1,P2 : TFPPackage;
  S : TMemoryStream;

begin
  Writeln('Testing package streaming (no deps)');
  P1:=TFPPackage.Create(Nil);
  Try
    FillFirstPackage(P1);
    P2:=TFPPackage.Create(Nil);
    Try
      S:=TMemoryStream.Create;
      try
        P1.SaveToStream(S);
        S.Position:=0;
        P2.LoadFromStream(S,StreamVersion);
        ComparePackages(P1,P2);
      finally
        S.Free;
      end;
    Finally
      P2.Free;
    end;
  Finally
    P1.Free;
  end;
end;

Procedure TestPackageStream2;


Var
  P1,P2 : TFPPackage;
  S : TMemoryStream;

begin
  Writeln('Testing package streaming (with deps)');
  P1:=TFPPackage.Create(Nil);
  Try
    FillThirdPackage(P1);
    P2:=TFPPackage.Create(Nil);
    Try
      S:=TMemoryStream.Create;
      try
        P1.SaveToStream(S);
        S.Position:=0;
        P2.LoadFromStream(S,StreamVersion);
        ComparePackages(P1,P2);
      finally
        S.Free;
      end;
    Finally
      P2.Free;
    end;
  Finally
    P1.Free;
  end;
end;

Procedure CompareRepositories(R1,R2 : TFPRepository);

var
  I : Integer;
begin
  If (R1.PackageCount<>R2.PackageCount) then
    Writeln('Package count differs : ',R1.PackageCount,'<>',R2.PackageCount)
  else
    For I:=0 to R1.PackageCount-1 do
      ComparePackages(R1[i],R1[I]);
end;

Procedure TestRepoStream;

Var
  R1,R2 : TFPRepository;
  S : TStream;

begin
  Writeln('Testing Repository streaming');
  R1:=CreateTestRep(4);
  try
    R2:=TFPrepository.Create(Nil);
    try
      S:=TMemoryStream.Create;
      try
        R1.SaveToStream(S);
        S.Position:=0;
        R2.LoadFromStream(S);
        CompareRepositories(R1,R2);
      finally
        S.Free;
      end;
    finally
      R2.Free;
    end;
  finally
    R1.Free;
  end;
end;

Procedure TestXMLWrite;

Var
  R : TFPRepository;
  X : TFPXMLRepositoryHandler;
  
begin
  Writeln('Testing XML writing');
  R:=CreateTestRep(4);
  try
    X:=TFPXMLRepositoryHandler.Create;
    With X do
      try
        SaveToXml(R,'packages.xml');
      finally
        Free;
      end;
  Finally
    R.Free;
  end;
end;

Procedure TestXMLRead;

Var
  R1,R2 : TFPRepository;
  X : TFPXMLRepositoryHandler;

begin
  Writeln('Testing XML reading');
  R1:=TFPrepository.Create(Nil);
  try
    X:=TFPXMLRepositoryHandler.Create;
    With X do
      try
        LoadFromXml(R1,'packages.xml');
        // Save for test purposes;
        SaveToXml(R1,'packages2.xml');
        // Now compare;
        R2:=CreateTestRep(4);
        Try
          CompareRepositories(R1,R2);
        Finally
          R2.Free;
        end;
      finally
        Free;
      end;
  Finally
    R1.Free;
  end;
end;

begin
  TestVersionCompare;
  TestPackageStream1;
  TestPackageStream2;
  TestRepoStream;
  TestXMLWrite;
  TestXMLRead;
end.

