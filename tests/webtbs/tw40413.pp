program Project1;
{$mode objfpc}{$ModeSwitch arrayoperators}{$ModeSwitch advancedrecords}

uses sysutils;
type IXQValue = record //something like a smart pointer
  x: pinteger;
  class operator initialize(var xx: IXQValue);
  class operator finalize(var xx: IXQValue);
  class operator addref(var xx: IXQValue);
  class operator Copy(constref s: IXQValue; var xx: IXQValue);
  class function create: IXQValue;static;
end;
class operator IXQValue.initialize(var xx: IXQValue);
begin
  xx.x := new(pinteger);
  xx.x^ := 1;
end;

class operator IXQValue.finalize(var xx: IXQValue);
begin
  dec(xx.x^);
  writeln(inttohex(ptruint(@xx),8), ' ',inttohex(ptruint(xx.x),8), ' ', xx.x^);
  if xx.x^ < -1 then
    halt(1);
 // if xx.x^ = 0 then dispose(xx.x);
end;

class operator IXQValue.addref(var xx: IXQValue);
begin
  inc(xx.x^);
end;

class operator IXQValue.Copy(constref s: IXQValue; var xx: IXQValue);
begin
  inc(s.x^);
  write('  copy ');
  //finalize(xx);
//  writeln(inttohex(ptruint(@xx),8), ' ',inttohex(ptruint(xx.x),8));
  dec(xx.x^);
  writeln(inttohex(ptruint(@xx),8), ' ',inttohex(ptruint(xx.x),8), ' ', xx.x^);
  xx.x := s.x;
end;

class function IXQValue.create: IXQValue;
begin
  //result := default(IXQValue);
end;



function test(const previous: IXQValue): IXQValue;


var
  newList: IXQValue;
  {$define doublefree}
  {$ifdef doublefree}
procedure print(const v: IXQValue);
var
  temp: IXQValue;

begin
 writeln(newList.x^);

end;

{$endif}
var
  i: SizeInt;
  resultList: IXQValue;

  tempList: IXQValue;




begin
    resultList:=ixqvalue.create;
    newList := ixqvalue.create;
  newlist := ixqvalue.create;

     tempList := newList;
     resultList := tempList        ;


  writeln(result.x^);
 writeln('newList: ',inttohex(ptruint(@newList), 8));
  writeln('tempList: ',inttohex(ptruint(@tempList), 8));
  result := resultList;
end;
begin
  test(ixqvalue.create);
end.
