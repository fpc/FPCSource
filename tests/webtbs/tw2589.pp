{ Source provided for Free Pascal Bug Report 2589 }
{ Submitted by "Al Led" on  2003-07-23 }
{ e-mail: alled@volny.cz }

{$mode objfpc}

program Test;
uses SysUtils;

var
  __ReadData, __Calculate : boolean;

begin
 __ReadData := true;
 while __ReadData do
 begin
  // read data from input...
  __readdata:=false;
  __Calculate := false;
  try
// **********************************************
// next construction with Continue statement
// causes linking error
// but only if next code contains another
// while...do loop [!!!]

   if not __Calculate then  // no more calcs ->
    Continue;               // skip rest and read
                            // next data...

// **********************************************

   // another required operations, checks ->
   // maybe  __Calculate := false;

// [!!!]
   while __Calculate do
   begin
    // do something... ->
    // -> save results...
    // checks -> maybe __Calculate := false;
   end;

  except
   on E:exception do
    raise Exception.Create('Err : ' + E.Message);
  end;                  // try..except

 end;     // while __ReadData...

end.
