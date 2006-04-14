{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondtypeconvert() -> second_bool_to_int        }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{                 secondinline()                                 }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS:                                                       }
{****************************************************************}
program tcnvint1;

{$ifdef VER70}
  {$define tp}
{$endif}

var
 tobyte : byte;
 toword : word;
 tolong : longint;
{$ifndef tp}
 toint64 : int64;
{$endif}
 bb1 : bytebool;
 wb1 : wordbool;
 lb1 : longbool;
 bb2 : bytebool;
 wb2 : wordbool;
 lb2 : longbool;
begin
 { left : LOC_REGISTER  }
 { from : LOC_REFERENCE/LOC_REGISTER }
 WriteLn('Testing LOC_REFERENCE...');
 bb1 := TRUE;
 tobyte := byte(bb1);
 WriteLn('boolean->byte : value should be 1...',tobyte);
 if tobyte <> 1 then 
   halt(1);
 bb1 := FALSE;
 tobyte := byte(bb1);
 WriteLn('boolean->byte : value should be 0...',tobyte);
 if tobyte <> 0 then 
   halt(1);
 bb1 := TRUE;
 toword := word(bb1);
 WriteLn('boolean->word : value should be 1...',toword);
 if toword <> 1 then 
   halt(1);
 bb1 := FALSE;
 toword := word(bb1);
 WriteLn('boolean->word : value should be 0...',toword);
 if toword <> 0 then 
   halt(1);
 bb1 := TRUE;
 tolong := longint(bb1);
 WriteLn('boolean->longint : value should be 1...',tolong);
 if tolong <> 1 then 
   halt(1);
 bb1 := FALSE;
 tolong := longint(bb1);
 WriteLn('boolean->longint : value should be 0...',tolong);
 if tolong <> 0 then 
   halt(1);
 wb1 := TRUE;
 tobyte := byte(wb1);
 WriteLn('wordbool->byte : value should be 1...',tobyte);
 if tobyte <> 1 then 
   halt(1);
 wb1 := FALSE;
 tobyte := byte(wb1);
 WriteLn('wordbool->byte : value should be 0...',tobyte);
 if tobyte <> 0 then 
   halt(1);
 wb1 := TRUE;
 toword := word(wb1);
 WriteLn('wordbool->word : value should be 1...',toword);
 if toword <> 1 then 
   halt(1);
 wb1 := FALSE;
 toword := word(wb1);
 WriteLn('wordbool->word : value should be 0...',toword);
 if toword <> 0 then 
   halt(1);
 wb1 := TRUE;
 tolong := longint(wb1);
 WriteLn('wordbool->longint : value should be 1...',tolong);
 if tolong <> 1 then 
   halt(1);
 wb1 := FALSE;
 tolong := longint(wb1);
 WriteLn('wordbool->longint : value should be 0...',tolong);
 if tolong <> 0 then 
   halt(1);
{$ifndef tp}
 bb1 := TRUE;
 toint64 :=int64(bb1);
 WriteLn('boolean->int64 : value should be 1...',toint64);
 if toint64 <> 1 then 
   halt(1);
 bb1 := FALSE;
 toint64 :=int64(bb1);
 WriteLn('boolean->int64 : value should be 0...',toint64);
 if toint64 <> 0 then 
   halt(1);
 wb1 := TRUE;
 toint64 :=int64(wb1);
 WriteLn('wordbool->int64 : value should be 1...',toint64);
 if toint64 <> 1 then 
   halt(1);
 wb1 := FALSE;
 toint64 :=int64(wb1);
 WriteLn('wordbool->int64 : value should be 0...',toint64);
 if toint64 <> 0 then 
   halt(1);
{$endif}
 lb1 := TRUE;
 tobyte := byte(lb1);
 WriteLn('longbool->byte : value should be 1...',tobyte);
 if tobyte <> 1 then 
   halt(1);
 lb1 := FALSE;
 tobyte := byte(lb1);
 WriteLn('longbool->byte : value should be 0...',tobyte);
 if tobyte <> 0 then 
   halt(1);
 lb1 := TRUE;
 toword := word(lb1);
 WriteLn('longbool->word : value should be 1...',toword);
 if toword <> 1 then 
   halt(1);
 lb1 := FALSE;
 toword := word(lb1);
 WriteLn('longbool->word : value should be 0...',toword);
 if toword <> 0 then 
   halt(1);
 lb1 := TRUE;
 tolong := longint(lb1);
 WriteLn('longbool->longint : value should be 1...',tolong);
 if tolong <> 1 then 
   halt(1);
 lb1 := FALSE;
 tolong := longint(lb1);
 WriteLn('longbool->longint : value should be 0...',tolong);
 if tolong <> 0 then 
   halt(1);
 { left : LOC_REGISTER }
 { from : LOC_REFERENCE }
 wb1 := TRUE;
 bb2 := wb1;
 WriteLn('wordbool->boolean : value should be TRUE...',bb2);
 if not bb2 then 
   halt(1);
 wb1 := FALSE;
 bb2 := wb1;
 WriteLn('wordbool->boolean : value should be FALSE...',bb2);
 if bb2 then 
   halt(1);
 lb1 := TRUE;
 bb2 := lb1;
 WriteLn('longbool->boolean : value should be TRUE...',bb2);
 if not bb2 then 
   halt(1);
 lb1 := FALSE;
 bb2 := lb1;
 WriteLn('longbool->boolean : value should be FALSE...',bb2);
 if bb2 then 
   halt(1);
 bb1 := TRUE;
 lb2 := bb1;
 WriteLn('boolean->longbool : value should be TRUE...',lb2);
 if not lb2 then 
   halt(1);
 bb1 := FALSE;
 lb2 := bb1;
 WriteLn('boolean->longbool : value should be FALSE...',lb2);
 if lb2 then 
   halt(1);
 { left : LOC_REGISTER }
 { from : LOC_JUMP     }
 WriteLn('Testing LOC_JUMP...');
 toword := 0;
 tobyte := 1;
 tobyte:=byte(toword > tobyte);
 WriteLn('value should be 0...',tobyte);
 if tobyte <> 0 then 
   halt(1);
 toword := 2;
 tobyte := 1;
 tobyte:=byte(toword > tobyte);
 WriteLn('value should be 1...',tobyte);
 if tobyte <> 1 then 
   halt(1);
 toword := 0;
 tobyte := 1;
 toword:=word(toword > tobyte);
 WriteLn('value should be 0...',toword);
 if toword <> 0 then 
   halt(1);
 toword := 2;
 tobyte := 1;
 toword:=word(toword > tobyte);
 WriteLn('value should be 1...',toword);
 if toword <> 1 then 
   halt(1);
 toword := 0;
 tobyte := 1;
 tolong:=longint(toword > tobyte);
 WriteLn('value should be 0...',tolong);
 if tolong <> 0 then 
   halt(1);
 toword := 2;
 tobyte := 1;
 tolong:=longint(toword > tobyte);
 WriteLn('value should be 1...',tolong);
 if tolong <> 1 then 
   halt(1);
{$ifndef tp}
 toword := 0;
 tobyte := 1;
 toint64:=int64(toword > tobyte);
 WriteLn('value should be 0...',toint64);
 if toint64 <> 0 then 
   halt(1);
 toword := 2;
 tobyte := 1;
 toint64:=int64(toword > tobyte);
 WriteLn('value should be 1...',toint64);
 if toint64 <> 1 then 
   halt(1);
{$endif}
 { left : LOC_REGISTER }
 { from : LOC_FLAGS     }
 WriteLn('Testing LOC_FLAGS...');
 wb1 := TRUE;
 bb1 := FALSE;
 bb1 := (wb1 > bb1);
 WriteLn('Value should be TRUE...',bb1);
 if not bb1 then 
   halt(1);
 wb1 := FALSE;
 bb1 := FALSE;
 bb1 := (wb1 > bb1);
 WriteLn('Value should be FALSE...',bb1);
 if bb1 then 
   halt(1);
 lb1 := TRUE;
 bb1 := FALSE;
 bb1 := (bb1 > lb1);
 WriteLn('Value should be FALSE...',bb1);
 if bb1 then 
   halt(1);
 lb1 := FALSE;
 bb1 := TRUE;
 bb1 := (bb1 > lb1);
 WriteLn('Value should be TRUE...',bb1);
 if not bb1 then 
   halt(1);
 lb1 := TRUE;
 bb1 := FALSE;
 wb1 := (bb1 > lb1);
 WriteLn('Value should be FALSE...',wb1);
 if wb1 then 
   halt(1);
 lb1 := FALSE;
 bb1 := TRUE;
 wb1 := (bb1 > lb1);
 WriteLn('Value should be TRUE...',wb1);
 if not wb1 then 
   halt(1);
 lb1 := TRUE;
 bb1 := FALSE;
 lb1 := (bb1 > lb1);
 WriteLn('Value should be FALSE...',lb1);
 if lb1 then 
   halt(1);
 lb1 := FALSE;
 bb1 := TRUE;
 lb1 := (bb1 > lb1);
 WriteLn('Value should be TRUE...',lb1);
 if not lb1 then 
   halt(1);
 bb1 := TRUE;
 bb2 := FALSE;
 lb1 := (bb1 > bb2);
 WriteLn('Value should be TRUE...',lb1);
 if not lb1 then 
   halt(1);
 bb1 := FALSE;
 bb2 := TRUE;
 lb1 := (bb1 > bb2);
 WriteLn('Value should be FALSE...',lb1);
 if lb1 then 
   halt(1);
end.
