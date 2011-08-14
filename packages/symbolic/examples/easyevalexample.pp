Program EasyEvalExample;
{
    Copyright (c) 2011 by Marco van de Voort(marco@freepascal.org)
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL-with-static-linking-exception)

    Rock bottom example of new evaluator helper function.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$mode delphi}
Uses Symbolic,Classes,sysutils;

var s : AnsiString;
    a : extended;
    b : integer;
    
begin
  // quickevaluate('expression',[],[]); evaluates a constant expression to an
  //  extended result

  s:='(5+5+10)*2';
  writeln(s,'=',QuickEvaluate(s,[],[]):10:1);


  // ... but still allows variables:

  a:=2.0;
  b:=3;
  s:='(5+A+10)*B';
  // variable names are case sensitive!
  writeln(s,'=',QuickEvaluate(s,['A','B'],[a,b]):10:1,' with A=',a:0:1,' and B=',b);

 // now let's do that again, but add a symbol (C) that we don't define:

 try
   a:=2.0;
   b:=3;
   s:='(5+A+10)*B+C';
   // variable names are case sensitive!
   writeln(s,'=',QuickEvaluate(s,['A','B'],[a,b]):10:1,' with A=',a:0:1,' and B=',b);
 except
   on E:Exception do
     Writeln('An exception occured: ',e.message);
   end;
end.