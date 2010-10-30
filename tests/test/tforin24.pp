program tforin24;

{$mode objfpc}
{$apptype console}

uses classes;

Type
   TDayObject = Class
     DayOfWeek : Integer;
     Constructor Create(ADayOfWeek : Integer);
   end;

   TObjectEnumerator = Class
     FList : TStrings;
     FIndex : Integer;
     Function GetCurrent : TDayObject;
     Function MoveNext: boolean;
     Property Current : TDayObject Read GetCurrent;
   end;

Constructor TDayObject.Create(ADayOfWeek : Integer);

begin
   DayOfWeek:=ADayOfWeek;
end;


Function TObjectEnumerator.GetCurrent : TDayObject;
begin
   Result:=FList.Objects[Findex] as TDayObject;
end;

Function TObjectEnumerator.MoveNext: boolean;

begin
   Inc(FIndex);
   Result:=(FIndex<FList.Count);
end;


operator enumerator (s : TStrings) : TObjectEnumerator;

begin
   Result:=TObjectEnumerator.Create;
   Result.Flist:=S;
   Result.FIndex:=-1;
end;

Var
   Days : TStrings;
   D : String;
   O : TdayObject;

begin
   Days:=TStringList.Create;
   try
     Days.AddObject('Monday',TDayObject.Create(1));
     Days.AddObject('Tuesday',TDayObject.Create(2));
     Days.AddObject('Wednesday',TDayObject.Create(3));
     Days.AddObject('Thursday',TDayObject.Create(4));
     Days.AddObject('Friday',TDayObject.Create(5));
     Days.AddObject('Saturday',TDayObject.Create(6));
     Days.AddObject('Sunday',TDayObject.Create(7));
     For O in Days do
       Writeln(O.DayOfWeek);
     For D in Days do
        Writeln(D);
   Finally
     Days.Free;
   end;
end.