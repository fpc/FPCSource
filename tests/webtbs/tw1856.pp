{ %version=1.1 }

{$mode objfpc}

type
 TFlowItem = class
  text: string; //replace with "shortstring" and the crash goes away
 end;

 TFlow = array of TFlowItem;

var
 flow: tflow;
begin
 setlength(flow, 10);
 setlength(flow, 1);
 { release }
 setlength(flow, 0);
 writeln('no crash');
end.
