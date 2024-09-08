{
    This file is part of the Free Component Library

    Webassembly RegExp API - Demo program
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program wasmregexpdemo;

uses sysutils, wasm.regexp.shared, wasm.regexp.api, wasm.regexp.objects;

Const
   SRegex = 'quick\s(?<color>brown).+?(jumps)';
   STest  = 'The Quick Brown Fox Jumps Over The Lazy Dog';
   SFlags  = 'dgi';

Var
  Regex : TWasmRegExp;
  Res : TRegExpResult;
  I : Integer;
  M : TRegExpMatch;
  G : TRegExpGroup;
  S : String;

begin
  Writeln('Regular expression: ',SRegex);
  Writeln('Flags: ',SFlags);
  Regex:=TWasmRegExp.Create(SRegex,SFlags);
  Writeln('Test string: ',STest);
  Res:=Regex.Exec(STest);
  if Res.Index=0 then
    Writeln('No match')
  else
    With Res do
      begin
      Writeln('Match at : ',Index);
      I:=0;
      For M in Matches do
        begin
        S:=Format('(%d) : "%s"',[I,M.Value]);
        if (rfIndices in Regex.Flags) then
          S:=S+Format(' [From pos %d to %d]',[M.StartIndex,M.StopIndex]);
        Writeln(S);
        Inc(I);
        end;
      Writeln('Named groups : ',Length(Groups));
      For G in Groups do
        begin
        S:=Format('(%d): "%s": "%s"',[I,G.Name,G.Value]);
        if (rfIndices in Regex.Flags) then
          S:=S+Format(' [From pos %d to %d]',[G.StartIndex,G.StopIndex]);
        Writeln(S);
        Inc(I);
        end;
      end;
end.

