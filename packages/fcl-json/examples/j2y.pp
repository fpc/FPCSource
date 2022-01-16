{
    This file is part of the Free Component Library
    Copyright (c) 2017 by Michael Van Canneyt michael@freepascal.org

    JSON To YAML syntax converter demo

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program j2y;
{$MODE OBJFPC}
{$H+}

uses fpjson,classes, jsonparser,json2yaml,sysutils;

Var
  IFN,OFN : String;
  D : TJSONData;
  IFS,OFS : TStream;
  jtoy : TJSONToYaml;


begin
  If ParamCount=0 then
    writeln('Usage j2y infile [outfile]');
  IFN:=ParamStr(1);
  OFN:=ParamStr(2);
  if OFN='' then
    OFN:=Changefileext(IFN,'yaml');
  D:=Nil;
  OFS:=Nil;
  jtoy:=Nil;
  IFS:=TFileStream.Create(IFN,fmOpenRead or fmShareDenyWrite);
  try
    D:=GetJSON(IFS);
    OFS:=TFileStream.Create(OFN,fmCreate);
    JTOY:=TJSONToYaml.Create;
    JTOY.Convert(D,OFS);
  finally
    D.Free;
    IFS.Free;
    OFS.Free;
    JTOY.Free;
  end;

end.

