{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondvecn()                                     }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondfor()                                    }
{                 secondderef()                                  }
{                 Free Pascal compiler                           }
{                 secondnew()                                    }
{                 seconddispose()                                }
{                 secondinline() length()                        }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS:                                                       }
{   Missing tests : LOC_JUMP, LOC_FLAGS                          }
{                   location                                     }
{                   openarray tests                              }
{                                                                }
{                                                                }
{                                                                }
{****************************************************************}
program tvecn;


{ things to test :                                 }
{   array/record offset with index = 0             }
{   array/record offset with index < MAX_CPU_DISP  }
{   non-aligned word/dword access to record field  }
{   ansistring                                     }
{         LOC_REFERENCE, LOC_REGISTER              }
{   string                                         }
{   right (index value)                            }
{     LOC_REGISTER                                 }
{     LOC_FLAGS                                    }
{     LOC_JUMP                                     }
{     LOC_REFERENCE, LOC_MEM                       }
const
 min_small_neg_array = -127;
 max_small_neg_array = 255;

 min_small_array = 0;
 max_small_array = 255;

 min_big_neg_array = -100000;
 max_big_neg_array = 100000;

 min_big_array = 0;
 max_big_array = 524288;

 min_big_odd_array = 0;
 max_big_odd_array = 255;

 alphabet_size = ord('Z')-ord('A')+1;
 alphabet : array[1..alphabet_size] of char =
 (
  'A','B','C','D','E','F','G','H','I',
  'J','K','L','M','N','O','P','Q','R',
  'S','T','U','V','W','X','Y','Z');

type
  { alignment requirements are checked }
  { in tsubscript.pp not here          }
  { so all elements are byte for easy  }
  { testing.                           }
  toddelement   = packed record
   _b0 : array[1..65537] of byte;
   _b1 : byte;
   _b2 : byte;
  end;

  psmallnegarray = ^smallnegarray;
  smallnegarray = array[min_small_neg_array..max_small_neg_array] of word;
  psmallarray   = ^smallarray;
  smallarray    = array[min_small_array..max_small_array] of word;
  pbignegarray  = ^bignegarray;
  bignegarray   = array[min_big_neg_array..max_big_neg_array] of word;
  pbigarray     = ^bigarray;
  bigarray      = array[min_big_array..max_big_array] of word;
  { in the case of odd addresses        }
  { call multiply in calculating offset }
  pbigoddarray = ^bigoddarray;
  bigoddarray  = array[min_big_odd_array..max_big_odd_array] of toddelement;


var
 globalsmallnegarray : smallnegarray;
 globalsmallarray : smallarray;
 globalbignegarray : bignegarray;
 globalbigarray : bigarray;
 globaloddarray : bigoddarray;
 globalindex : longint;
 globalansi : ansistring;


   { this routine clears all arrays     }
   { without calling secondvecn() first }
   procedure clearglobalarrays;
     begin
      FillChar(globalsmallnegarray,sizeof(globalsmallnegarray),0);
      FillChar(globalsmallarray,sizeof(globalsmallarray),0);
      FillChar(globalbignegarray,sizeof(globalbignegarray),0);
      FillChar(globalbignegarray,sizeof(globalbignegarray),0);
      FillChar(globalbigarray,sizeof(globalbigarray),0);
      FillChar(globaloddarray,sizeof(globaloddarray),0);
     end;


  { left: array definition }
  { right : index constant }
  { NOT OPEN ARRAY         }
  { (current): LOC_MEM, LOC_REFERENCE (symbol) }
  { (current): LOC_REFERENCE (with index register) }
  { (current): LOC_REFERENCE (without index register) }
  { (current): LOC_REFERENCE (without base register) }
  procedure testarrayglobal;
   var
    i : longint;
    passed : boolean;
   begin
    passed := true;
    ClearGlobalArrays;
    Write('Testing subscriptn() global variables...');


    { RIGHT : LOC_JUMP ??????      }
    { (current) : LOC_MEM (symbol) }

    { RIGHT : LOC_FLAGS??????      }
    { (current) : LOC_MEM (symbol) }



    { RIGHT : LOC_REFERENCE        }
    { (current) : LOC_MEM (symbol) }
    globalindex := max_big_array;
    globalbigarray[globalindex] := $F0F0;
    if globalbigarray[globalindex] <> $F0F0 then
     passed := false;

    { RIGHT : ordconstn            }
    { (current) : LOC_MEM (symbol) }
    { index 1 : 1                  }
    globalbigarray[max_big_array] := $FF;
    if globalbigarray[max_big_array] <> $FF then
     passed := false;

    { RIGHT : LOC_REGISTER         }
    { (current) : LOC_MEM (symbol) }
    for i:=min_small_neg_array to max_small_neg_array do
      begin
        globalsmallnegarray[i] := word(i);
      end;
    { now compare if the values are correct }
    for i:=min_small_neg_array to max_small_neg_array do
      begin
        if globalsmallnegarray[i] <> word(i) then
           passed := false;
      end;

    for i:=min_small_array to max_small_array do
      begin
        globalsmallarray[i] := i;
      end;
    { now compare if the values are correct }
    for i:=min_small_array to max_small_array do
      begin
        if globalsmallarray[i] <> i then
           passed := false;
      end;

    for i:=min_big_neg_array to max_big_neg_array do
      begin
        globalbignegarray[i] := word(i);
      end;
    { now compare if the values are correct }
    for i:=min_big_neg_array to max_big_neg_array do
      begin
        if globalbignegarray[i] <> word(i) then
           passed := false;
      end;


    for i:=min_big_array to max_big_array do
      begin
        globalbigarray[i] := word(i);
      end;
    { now compare if the values are correct }
    for i:=min_big_array to max_big_array do
      begin
        if globalbigarray[i] <> word(i) then
           passed := false;
      end;


    for i:=min_big_odd_array to max_big_odd_array do
      begin
        globaloddarray[i]._b1 := byte(i);
      end;

    { now compare if the values are correct }
    for i:=min_big_odd_array to max_big_odd_array do
      begin
        if globaloddarray[i]._b1 <> byte(i) then
           passed := false;
      end;


    if passed then
      WriteLn('Success.')
    else
      WriteLn('Failure.');
   end;


  { left: array definition }
  { right : index constant }
  { OPEN ARRAY             }
  { (current): LOC_MEM, LOC_REFERENCE (symbol) }
  { (current): LOC_REFERENCE (with index register) }
  { (current): LOC_REFERENCE (without index register) }
  { (current): LOC_REFERENCE (without base register) }
  procedure testarraylocal;
    var
    localsmallnegarray : psmallnegarray;
    localsmallarray : psmallarray;
    localbignegarray : pbignegarray;
    localbigarray : pbigarray;
    localindex : longint;
    i : longint;
    passed : boolean;
   begin
    Write('Testing subscriptn() local variables...');
    new(localsmallnegarray);
    new(localsmallarray);
    new(localbignegarray);
    new(localbigarray);

    passed := true;
    FillChar(localsmallnegarray^,sizeof(smallnegarray),0);
    FillChar(localsmallarray^,sizeof(smallarray),0);
    FillChar(localbignegarray^,sizeof(bignegarray),0);
    FillChar(localbignegarray^,sizeof(bignegarray),0);
    FillChar(localbigarray^,sizeof(bigarray),0);

    { RIGHT : LOC_JUMP ??????      }
    { (current) : LOC_MEM (symbol) }

    { RIGHT : LOC_FLAGS??????      }
    { (current) : LOC_MEM (symbol) }



    { RIGHT : LOC_REFERENCE        }
    { (current) : LOC_MEM () }
    localindex := max_big_array;
    localbigarray^[localindex] := $F0F0;
    if localbigarray^[localindex] <> $F0F0 then
     passed := false;

    { RIGHT : ordconstn            }
    { (current) : LOC_MEM () }
    { index 1 : 1                  }
    localbigarray^[max_big_array] := $FF;
    if localbigarray^[max_big_array] <> $FF then
     passed := false;

    { RIGHT : LOC_REGISTER         }
    { (current) : LOC_MEM () }
    for i:=min_small_neg_array to max_small_neg_array do
      begin
        localsmallnegarray^[i] := word(i);
      end;
    { now compare if the values are correct }
    for i:=min_small_neg_array to max_small_neg_array do
      begin
        if localsmallnegarray^[i] <> word(i) then
           passed := false;
      end;

    for i:=min_small_array to max_small_array do
      begin
        localsmallarray^[i] := i;
      end;
    { now compare if the values are correct }
    for i:=min_small_array to max_small_array do
      begin
        if localsmallarray^[i] <> i then
           passed := false;
      end;

    for i:=min_big_neg_array to max_big_neg_array do
      begin
        localbignegarray^[i] := word(i);
      end;
    { now compare if the values are correct }
    for i:=min_big_neg_array to max_big_neg_array do
      begin
        if localbignegarray^[i] <> word(i) then
           passed := false;
      end;


    for i:=min_big_array to max_big_array do
      begin
        localbigarray^[i] := word(i);
      end;
    { now compare if the values are correct }
    for i:=min_big_array to max_big_array do
      begin
        if localbigarray^[i] <> word(i) then
           passed := false;
      end;

    if passed then
      WriteLn('Success.')
    else
      WriteLn('Failure.');



    dispose(localbigarray);
    dispose(localbignegarray);
    dispose(localsmallarray);
    dispose(localsmallnegarray);
   end;





  { (current): LOC_MEM, LOC_REFERENCE (symbol) }
  { (current): LOC_REFERENCE (with index register) }
  { (current): LOC_REFERENCE (without index register) }
  { (current): LOC_REFERENCE (without base register) }
  procedure testansistring;

    var
      localansi : ansistring;
      passed : boolean;
      i : longint;
    begin
      Write('Testing subscriptn() ansistring()...');
      passed := true;
      localansi := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
      { RIGHT : LOC_REFERENCE        }
      { (current) : LOC_REFERENCE () }
      for i:=1 to length(localansi) do
        begin
          if localansi[i]<>alphabet[i] then
            passed := false;
        end;

      { RIGHT : LOC_REFERENCE
       (current) : LOC_REGISTER  ()
      for i:=0 to length(localansi) do
        begin
          if ansistring(getansistr)[i]<>alphabet[i] then
            passed := false;
        end;
      }
      if passed then
        WriteLn('Success.')
      else
        WriteLn('Failure.');
    end;


   { left: array definition       }
   { right : + operator           }
   { right right : index constant }
   { With -Or switch only         }
  

   { left: array definition       }
   { right : - operator           }
   { right right : index constant }
   { With -Or switch only         }


begin
  globalansi := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  testarrayglobal;
  testarraylocal;
  testansistring;
end.



{
  $Log$
  Revision 1.1  2001-06-29 02:02:10  carl
  + add array indexing test suite (incomplete)

}

