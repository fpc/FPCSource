{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe
    member of the Free Pascal development team.

    This file implements the helper routines for TObject

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

  procedure TObject.Free;
    begin
      if not DestructorCalled then
        begin
          DestructorCalled:=true;
          Destroy;
        end;
    end;


  destructor TObject.Destroy;
    begin
    end;


  procedure TObject.Finalize;
    begin
      Free;
    end;



  procedure tvarrec.init(l: longint);
    begin
      VType:=vtInteger;
      Value:=JLInteger.valueOf(l);
    end;


  procedure tvarrec.init(b: boolean);
    begin
      VType:=vtBoolean;
      Value:=JLBoolean.valueOf(b);
    end;


  procedure tvarrec.init(c: ansichar);
    begin
      VType:=vtChar;
      Value:=JLByte.valueOf(byte(c));
    end;


  procedure tvarrec.init(w: widechar);
    begin
      VType:=vtWideChar;
      Value:=JLCharacter.valueOf(w);
    end;


  procedure tvarrec.init(d: extended);
    var
      arr: array[0..0] of extended;
    begin
      VType:=vtExtended;
      { VExtended has to return a PExtended -> return address of array (it
        doesn't matter that this is a local variable, all arrays are garbage
        collected pointers underneath!) }
      arr[0]:=d;
      Value:=JLObject(@arr);
    end;


  procedure tvarrec.init(const s: shortstring);
    begin
      VType:=vtString;
      Value:=JLObject(@s);
    end;


  procedure tvarrec.init(constref p: pointer);
    begin
      // pointer = object
      VType:=vtPointer;
      Value:=JLObject(p);
    end;


  procedure tvarrec.init(p: pchar);
    begin
      VType:=vtPChar;
      Value:=JLObject(p);
    end;


  procedure tvarrec.init(p: JLObject);
    begin
      VType:=vtObject;
      Value:=p;
    end;


  procedure tvarrec.init(c: TJClass);
    begin
      VType:=vtClass;
      Value:=JLObject(c);
    end;


  procedure tvarrec.init(p: pwidechar);
    begin
      VType:=vtPWideChar;
      Value:=JLObject(p);
    end;


  procedure tvarrec.init(const a: ansistring);
    begin
      VType:=vtAnsiString;
      Value:=JLObject(a);
    end;


  procedure tvarrec.init(constref c: currency);
    begin
      VType:=vtCurrency;
      { a constref parameter is internally passed as an array -> we can just
        take its address and return it later as a pcurrency (which is also a
        pointer internally) }
      Value:=JLObject(@c);
    end;


  procedure tvarrec.init(const w: widestring);
    begin
      VType:=vtUnicodeString;
      Value:=JLObject(w);
    end;


  procedure tvarrec.init(i: int64);
    var
      arr: array[0..0] of int64;
    begin
      VType:=vtInt64;
      arr[0]:=i;
      Value:=JLObject(@arr);
    end;


  procedure tvarrec.init(q: qword; unsigned: boolean = true);
    var
      arr: array[0..0] of qword;
    begin
      init(int64(q));
      { parameter could be false in case it's called from Java code }
      if unsigned then
        VType:=vtQWord;
    end;


  function tvarrec.VInteger: longint;
    begin
      result:=JLInteger(Value).intValue
    end;


  function tvarrec.VBoolean: boolean;
    begin
      result:=JLBoolean(Value).booleanValue;
    end;


  function tvarrec.VChar: ansichar;
    begin
      result:=char(JLByte(Value).byteValue);
    end;


  function tvarrec.VWideChar: widechar;
    begin
      result:=JLCharacter(Value).charValue;
    end;


  function tvarrec.VExtended: pextended;
    begin
      result:=PExtended(Value);
    end;


  function tvarrec.VDouble: double;
    begin
      result:=JLDouble(Value).doubleValue;
    end;


  function tvarrec.VString: PShortString;
    begin
      result:=PShortString(Value);
    end;


  function tvarrec.VPointer: pointer;
    begin
      result:=pointer(Value);
    end;


  function tvarrec.VPChar: PChar;
    begin
      result:=PChar(Value);
    end;


  function tvarrec.VObject: JLObject;
    begin
      result:=Value;
    end;


  function tvarrec.VClass: TJClass;
    begin
      result:=TJClass(Value);
    end;


  function tvarrec.VPWideChar: PWideChar;
    begin
      result:=PWideChar(Value);
    end;


  function tvarrec.VAnsiString: Pointer;
    begin
      result:=Pointer(Value);
    end;


  function tvarrec.VCurrency: PCurrency;
    begin
      result:=PCurrency(Value);
    end;


  // function tvarrec.VVariant: PVariant;


  function tvarrec.VInterface: JLObject;
    begin
      result:=Value;
    end;


  function tvarrec.VWideString: Pointer;
    begin
      result:=Pointer(Value);
    end;


  function tvarrec.VInt64: PInt64;
    begin
      result:=PInt64(Value);
    end;


  function tvarrec.VUnicodeString: Pointer;
     begin
       result:=Pointer(Value);
     end;


  function tvarrec.VQWord: PQWord;
    begin
      result:=PQword(Value);
    end;

