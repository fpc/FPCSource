{ %NORUN }

program tw40145;

{$Mode objfpc}{$H+}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}
{$ModeSwitch nestedprocvars}

type
  TVoidFunc = reference to procedure;
  TFuncMaker = reference to function(const thing: string): TVoidFunc;

procedure main;
  var
    cool_bingo: TVoidFunc;
    coolifier: TFuncMaker;
  begin
    cool_bingo := default(TVoidFunc);
    coolifier := default(TFuncMaker);
    coolifier := function (const thing: string): TVoidFunc
    var
      func: TVoidFunc;
    begin
      func := procedure begin writeln('cool ', thing) end;
      result := func; // <-- This is line 24
    end;
    cool_bingo := coolifier('bingo');
    cool_bingo();
  end;

begin
  main;
end.

