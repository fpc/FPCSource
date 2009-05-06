program testwhile;

{$mode objfpc}{$H+}



function lineHtml( sa : string):string;
var
xPoz : integer;
xp,xk  : integer;

function nextToken(var aPocz: integer;var aKon :integer):string;

begin
  result:='';
  aPocz:=xPoz+1;
  aKon:=0;
  try
    while xpoz< length(sa) do begin
      inc(xpoz);
      case sa[xpoz] of

       '|' :begin
               exit;
           end;
      else

      end;
      result:=result+sa[xpoz];
      inc(aKon);

    end;
  finally
    writeln('test ',result);
    aKon:=aPocz+aKon;
//    writeln('test2 ',result);
  end;
end;


begin
 xpoz:=0;
 result:='';
 repeat
    nextToken(xp,xk);
 until xpoz>=length(sa);
end;


begin
 writeln(lineHTML('|  1 | 2 | 3'));
end.

