{ IndexOf and LastIndexOf with a start index on TStringList, issue #41944 }
program tstringsindexof;

{$mode objfpc}{$h+}

uses
  Classes;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


var
  lList: TStringList;

begin
  lList:=TStringList.Create;
  try
    lList.AddStrings(['a','b','c','b','d']);
    Check(1,lList.IndexOf('b')=1,'IndexOf without start');
    Check(2,lList.IndexOf('b',2)=3,'IndexOf with start on TStringList');
    Check(3,lList.IndexOf('a',1)=-1,'IndexOf with start past the only match');
    Check(4,lList.LastIndexOf('b')=3,'LastIndexOf without start');
    Check(5,lList.LastIndexOf('b',2)=1,'LastIndexOf with start on TStringList');
    Check(6,lList.IndexOf('d',-1)=4,'IndexOf with a negative start counts from the end');
    lList.Sorted:=True;
    Check(10,lList.IndexOf('c')=3,'IndexOf on a sorted list');
    Check(11,lList.IndexOf('b',2)=2,'IndexOf with start inside the run of equal strings');
    Check(12,lList.IndexOf('b',0)=1,'IndexOf with start before the run of equal strings');
    Check(13,lList.IndexOf('b',3)=-1,'IndexOf with start after the run of equal strings');
    Check(14,lList.IndexOf('x',0)=-1,'IndexOf with start of a missing string');
    Check(15,lList.IndexOf('d',5)=-1,'IndexOf with start past the end');
    Check(16,lList.IndexOf('d',-1)=4,'IndexOf with a negative start on a sorted list');
    Check(17,lList.IndexOf('a',-10)=0,'IndexOf with a start before the beginning');
    lList.Clear;
    lList.Duplicates:=dupAccept;
    lList.AddStrings(['b','a','b','c','b','a']);
    Check(20,lList.IndexOf('b',1)=2,'IndexOf with start before three equal strings');
    Check(21,lList.IndexOf('b',3)=3,'IndexOf with start on the second of three equal strings');
    Check(22,lList.IndexOf('a',1)=1,'IndexOf with start on the last of two equal strings');
    Check(23,lList.IndexOf('c',2)=5,'IndexOf with start before a single string');
  finally
    lList.Free;
  end;
  writeln('ok');
end.
