{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    Pascal to Javascript converter program.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
program pas2js;

uses
  sysutils, classes, pparser, fppas2js, pastree, jstree, jswriter, pasresolver;

Type

  { TConvertPascal }

  TConvertPascal = Class(TComponent)
     Procedure ConvertSource(ASource, ADest : String);
  end;


{ TConvertPascal }

Procedure TConvertPascal.ConvertSource(ASource, ADest: String);

Var
  C : TPas2JSResolver;
  M : TPasModule;
  CV : TPasToJSConverter;
  JS : TJSElement;
  W : TJSWriter;

begin
  W:=nil;
  M:=Nil;
  CV:=Nil;
  C:=TPas2JSResolver.Create;
  try
    M:=ParseSource(C,ASource,'','',[poUseStreams]);
    CV:=TPasToJSConverter.Create;
    JS:=CV.ConvertPasElement(M,C);
    If JS=nil then
      Writeln('No result')
    else
      begin
      W:=TJSWriter.Create(ADest);
      W.Options:=[woUseUTF8,woCompactArrayLiterals,woCompactObjectLiterals,woCompactArguments];
      W.IndentSize:=2;
      W.WriteJS(JS);
      end
  finally
    W.Free;
    CV.Free;
    M.Free;
    C.Free;
  end;

end;

Var
  Src,Dest : String;

begin
  Src:=Paramstr(1);
  Dest:=ParamStr(2);
  if Dest='' then
    Dest:=ChangeFileExt(Src,'.js');
  With TConvertPascal.Create(Nil) do
    try
      ConvertSource(Src,Dest);
    finally
      Free;
    end;
  With TStringList.Create do
    try
      LoadFromFile(Dest);
      Writeln(Text);
    finally
      Free;
    end;
end.

