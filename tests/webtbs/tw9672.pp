{$mode objfpc} {$H+}
uses fgl,sysutils;

const strs : array[0..16] of integer = (1,2,2,7,0,12,3,4,5,3,6,7,8,9,0,3,4);
    
type
  TInterestingData = integer;
  TMySet = specialize TFPGmap<integer, TInterestingData>;

function mycompare(const a,b : integer) : integer;
begin
  result := a-b;
end;

var
  s : TMySet;
  idx, i,j : Integer;
  
  b : TInterestingData;

begin
  s := TMySet.Create; 
  s.sorted := false;
  s.OnCompare := @mycompare;
  
  for i := low(strs) to high(strs) do begin
    idx := s.IndexOf(strs[i]);
    writeln('count ', s.count, ' used of ', s.capacity, ' available');
    if (idx <> -1) then begin
      b := s[strs[i]];
    end else begin
      b := i;
      s[strs[i]] := b;
    end;
    
    // do something with existing interesting data
    writeln('data: ', b);
  end;
  
  s.Free;
end.

