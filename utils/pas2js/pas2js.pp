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
  sysutils, classes, pparser, fppas2js, pastree, jstree, jswriter;

Type

  { TContainer }

  TContainer = Class(TPasTreeContainer)

  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      overload; override;
    function FindElement(const AName: String): TPasElement; override;
  end;

  { TConvertPascal }

  TConvertPascal = Class(TComponent)
     Procedure ConvertSource(ASource, ADest : String);
  end;

{ TContainer }

function TContainer.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result:=AClass.Create(AName,AParent);
end;

function TContainer.FindElement(const AName: String): TPasElement;
begin
  Result:=Nil;
end;

{ TConvertPascal }

Procedure TConvertPascal.ConvertSource(ASource, ADest: String);

Var
  p : TPasParser;
  C : TPAsTreeContainer;
  M : TPasModule;
  CV : TPasToJSConverter;
  JS : TJSElement;
  W : TJSWriter;

begin
  C:=TContainer.Create;
  try
    M:=ParseSource(C,ASource,'','',True);
    try
      CV:=TPasToJSConverter.Create;
      try
        JS:=CV.ConvertElement(M);
        If JS=nil then
          Writeln('No result');
      finally
        CV.Free;
      end;
      W:=TJSWriter.Create(ADest);
      try
        W.Options:=[woUseUTF8,woCompactArrayLiterals,woCompactObjectLiterals,woCompactArguments];
        W.IndentSize:=2;
        W.WriteJS(JS);
      finally
        W.Free;
      end;
    finally
      M.Free;
    end;
  finally
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

