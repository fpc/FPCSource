{ %NORUN }

program tw40143;

{$Mode objfpc}{$H+}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}
{$ModeSwitch nestedprocvars}

type
  TVoidFunc = reference to procedure;
  TFuncMaker = reference to function(const thing: string): TVoidFunc;

procedure something;
  begin
    writeln('something...');
  end;

procedure main;
  var
    cool_bingo: TVoidFunc;
    coolifier: TFuncMaker;
  begin
    coolifier := function (const thing: string): TVoidFunc
    begin
      // result := procedure begin writeln('cool ', thing) end;
      result := TVoidFunc(@something);
    end;
    cool_bingo := coolifier('bingo');
    cool_bingo();
  end;

begin
  main;
end.

