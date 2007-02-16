{ %version=1.1 }

{$ifdef fpc}
{$mode objfpc}
{$endif}

type
 TFlowItem = class
  text: string; //replace with "shortstring" and the crash goes away
 end;

 TFlow = array of TFlowItem;

 TFlow2 = array of array of TFlowItem;

var
 flow: tflow;
 flow2 : TFLow2;
begin
 setlength(flow, 10);
 setlength(flow, 1);
 flow[0]:=TFlowItem.Create;

 { release }
 setlength(flow, 0);

 setlength(flow2, 10, 20);
 setlength(flow2, 0);

 writeln('no crash');
end.
