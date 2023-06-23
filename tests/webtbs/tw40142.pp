{ %NORUN }

program tw40142;

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
    coolifier := function (const thing: string) : TVoidFunc
    begin
      result := procedure begin writeln('cool ', thing) end;
    end;
    cool_bingo := coolifier('bingo');
    cool_bingo();
  end;

begin
  main;
end.

