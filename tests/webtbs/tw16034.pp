program Hello;

{$ifdef fpc}
{$mode delphi}
{$endif}

{$APPTYPE CONSOLE}
{$O-}

const
  val_O0 = 35;
  val_O1 = 74;
  val_O2 = 123;

{$ifdef CPUI8086}
const
  offset_size = 4;
type
  Int = smallint;
  UInt = word;
{$else}
const
  offset_size = 2*sizeof(pointer);
type
  Int = ptrint;
  UInt = ptruint;
{$endif}

type
  ptr = pointer;
{$ifdef fpc}
  codeptr = codepointer;
{$else}
  codeptr = pointer;
{$endif}


  pPtr = ^ptr;
  Bool = Boolean;

  // Object woth VMT at offset 0.
  TObj0 =
    object
      Constructor Init;
      Function Value(p: UInt): UInt; Virtual;
    enD;

  // Object with VMT at offset 0, and size = 5.
  TObj1 =
    object (TObj0)
      f1: Byte; // UInt;

      Constructor Init;
      Function Value(p: UInt): UInt; Virtual;
    enD;

  // Object with VMT at offset 0, but size = 8. (???)
  TObj2 =
    object
      f1{, f2, f3, f4}: Byte; // UInt;

      Constructor Init;
      Function Value(p: UInt): UInt; Virtual;
    enD;

{ Implmentation }

Constructor TObj0.Init;
begin
enD;

Function TObj0.Value(p: UInt): UInt;
begin
  Result := val_O0;
enD;

Constructor TObj1.Init;
begin
enD;

Function TObj1.Value(p: UInt): UInt;
begin
  Result := val_O1;
enD;

Constructor TObj2.Init;
begin
enD;

Function TObj2.Value(p: UInt): UInt;
begin
  Result := val_O2;
enD;

{ Low Level VMT Routines }

type
  pObjVMT = ^TObjVMT;
  ppObjVMT = ^pObjVMT;
  TObjVMT =
    record
      fInstanceSize: UInt;
      fInstanceSize2: Int;
      fParent: ppObjVMT;
    enD;

Function GetInstanceSize(AVMT: pObjVMT): UInt;
begin
  Result := AVMT.fInstanceSize;
enD;

Function GetVMTPtrOffset(AVMT: pObjVMT): UInt;
begin
  writeln('AVMT is ',hexstr(seg(AVMT^),4),':',hexstr(ofs(AVMT^),offset_size));
  writeln('AVMT^.fParent is ',hexstr(seg(AVMT^.fParent^),4),':',hexstr(ofs(AVMT^.fParent^),offset_size));
  if (AVMT.fParent = nil) then
    Result := GetInstanceSize(AVMT) - SizeOf(ptr) else
    Result := GetVMTPtrOffset(AVMT.fParent^);
  writeln('GetVMTPtrOffset=',hexstr(Result,2*sizeof(UInt)));
enD;

Function SetVMT(Obj: ptr; AVMT: ptr): Bool;
var
  p : pptr;
begin
  Result := (AVMT <> nil);

  if (Result) then
    begin
      writeln('Obj is ',hexstr(seg(Obj^),4),':',hexstr(ofs(Obj^),offset_size));
      p:=pPtr(ptr(Obj) + GetVMTPtrOffset(AVMT));
      writeln('Setting p ',hexstr(seg(p^),4),':',hexstr(ofs(p^),offset_size),' to ',hexstr(seg(AVMT^),4),':',hexstr(ofs(AVMT^),offset_size));
      p^ := AVMT;
    end;
enD;


{ Main }

var
  O0: TObj0;
  O1: TObj1;
  O2: TObj2;

  s0, s1, s2: UInt;
  v0, v1, v2: ptr;
  cn0, cn1, cn2: codeptr;
  st : string;
begin
  // VMT Pointers
  v0 := TypeOf(TObj0);
  v1 := TypeOf(TObj1);
  v2 := TypeOf(TObj2);
  writeln('TObj0 VMT ',hexstr(seg(v0^),4),':',hexstr(ofs(v0^),offset_size));
  writeln('TObj1 VMT ',hexstr(seg(v1^),4),':',hexstr(ofs(v1^),offset_size));
  writeln('TObj2 VMT ',hexstr(seg(v2^),4),':',hexstr(ofs(v2^),offset_size));

  // Object sizes
  s0 := SizeOf(TObj0); // = 4
  s1 := SizeOf(TObj1); // = 5
  s2 := SizeOf(TObj2); // = 8 (???)
  writeln(s0);
  writeln(s1);
  writeln(s2);

  // Method pointers
  cn0 := @TObj0.Value;
  cn1 := @TObj1.Value;
  cn2 := @TObj2.Value;

  // VMT offsets (use in watches - need in program!)
// Int(@o0._vptr$) - Int(@o0)     = 0
// Int(@o1._vptr$) - Int(@o1)     = 0
// Int(@o2._vptr$) - Int(@o2)     = 1 (???)

{
  // Constructors - skipping
  O0.Init;
  O1.Init;
  O2.Init;
}

  writeln('@TObj0.Value ',hexstr(seg(cn0^),4),':',hexstr(ofs(cn0^),offset_size));
  writeln('@TObj1.Value ',hexstr(seg(cn1^),4),':',hexstr(ofs(cn1^),offset_size));
  writeln('@TObj2.Value ',hexstr(seg(cn2^),4),':',hexstr(ofs(cn2^),offset_size));
  // Store VMT (emulate constructor)
  SetVMT(@O0, TypeOf(TObj0));
  SetVMT(@O1, TypeOf(TObj1));
  SetVMT(@O2, TypeOf(TObj2));

  // readln(st);
  st:='c';

  if st='c' then
    begin
      writeln('O0 value is ',O0.VAlue(0),' after O0');
      writeln('O1 value is ',O1.VAlue(0),' after O1');
      writeln('O2 value is ',O2.VAlue(0),' after O2');
      // Call Virtual Functions
      O2.f1 := O0.Value(0);
      if O2.f1<>val_O0 then
        halt(1);
      O2.f1 := O1.Value(0);
      if O2.f1<>val_O1 then
        halt(2);
      O2.f1 := O2.Value(0); {CRASHES !!!}
      if O2.f1<>val_O2 then
        halt(3);
      { SizeOf(TObj2) must be 5,
      or ptr(Int(@o2._vptr$) - Int(@o2)) must be 4! }

      // MessageBox will be displayed, if all was successfull
    end;
  writeln(O2.f1, 'Hello, FPC uWorld!', 'Hello', 0);
end.

