(* Test DWARF generated for a unique type alias
   type TFoo = type TBar

   - Both variables point to a different DW_TAG_typedef - this has the correct type name.
   - Those DW_TAG_typedef then point to the actual type DW_TAG_structure_type

   NOTES:
   - The DW_TAG_structure_type for the alias has the name of the original type.
     This may not have been intended?
     But it also is not a mistake according to the DWARF-spec, as the type is defined
     and named by the DW_TAG_typedef.
   - It is unexpected that the same type (with the same name) exists twice.
     (I.e.: Has 2 identical entries in the DWARF info)
     The DW_TAG_typedef is meant to avoid exactly that.
*)

procedure Check;
var
  OffsList: TDwarfTagArray;
  self_name: AnsiString;
  t: TDwarfTag;
  t1: array of TDwarfTag;
  i: integer;
  ExpTag: String;
begin
  (* Start at variables/fields and follow the type
     - DW_AT_type_def
       - optional from a dwarf point of view
       - mandatory by FPC
     - unique alias "type X = type Y"
       - no reference to origin type (it's a new distinct type)
       - DW_TAG_typedef
       - DW_TAG_*_type  // TODO: DW_TAG_pointer_type has no name
       - All fields and All methods included
       - -goDwarfCpp => DW_AT_linkage_name

       - TODO: record-composition "contains" not yet implemented

     * All container/structure types
     * check cross unit
  *)


  CheckVar('UA_iA1', 'ua_TTestInt1', [ttWantAlias, ttAnyType], OffsList, True);
  CheckVar('UA_iA2', 'ua_TTestInt1X', [ttWantAlias, ttAnyType], OffsList, True);
  CheckVar('iA1', 'TUa_TestInt1_X', [ttWantAlias, ttAnyType], OffsList, True);
  CheckVar('iA2', 'TUa_TestInt1X_X', [ttWantAlias, ttAnyType], OffsList, True);

  // record
  ErrInfo := 'RECORD: ';

  SetLength(t1, 5);
  t1[0] := CheckVar('UA_Rec1', 'ua_TTestRec1', [ttWantAlias], OffsList, True);
  t1[1] := CheckVar('UA_Rec1X', 'ua_TTestRec1X', [ttWantAlias], OffsList, True);
  t1[2] := CheckVar('UA_Rec1XX', 'ua_TTestRec1XX', [ttWantAlias], OffsList, True);
  t1[3] := CheckVar('RecXUa1', 'TUa_TestRec1_X', [ttWantAlias], OffsList, True);
  t1[4] := CheckVar('RecXUa1X', 'TUa_TestRec1X_X', [ttWantAlias], OffsList, True);


  t := CheckVar('UA_PRec1', 'ua_PTestRec1', [ttWantAlias, ttAnyTypeNoName], OffsList, True, 'DW_TAG_pointer_type');
       CheckType(GetTypeOffsFromNode(t), 'ua_TTestRec1', [ttWantAlias]);
  t := CheckVar('UA_PRec1X', 'ua_PTestRec1X', [ttWantAlias, ttAnyTypeNoName], OffsList, True, 'DW_TAG_pointer_type');
       CheckType(GetTypeOffsFromNode(t), 'ua_TTestRec1X', [ttWantAlias]);


  for i := 0 to 4 do begin
    t := CheckType(GetTypeOffsFromMember('ua_FldR_C', t1[i]), 'ua_PTestRec1', [ttWantAlias, ttAnyTypeNoName], 'DW_TAG_pointer_type');
         CheckType(GetTypeOffsFromNode(t), 'ua_TTestRec1', [ttWantAlias]);
    t := CheckType(GetTypeOffsFromMember('ua_FldR_CX', t1[i]), 'ua_PTestRec1X', [ttWantAlias, ttAnyTypeNoName], 'DW_TAG_pointer_type');
         CheckType(GetTypeOffsFromNode(t), 'ua_TTestRec1X', [ttWantAlias]);
    CheckType(GetTypeOffsFromMember('ua_FldR_D',  t1[i]), 'ua_TTestRec1Nest1', [ttWantAlias]);
    CheckType(GetTypeOffsFromMember('ua_FldR_DX', t1[i]), 'ua_TTestRec1Nest1X', [ttWantAlias]);
    // FldE is not a unique alias
    //CheckType(GetTypeOffsFromMember('ua_FldR_E' , t1[i]), 'ua_ToTTestRec1Nest1', [ttWantAlias]);
    //CheckType(GetTypeOffsFromMember('ua_FldR_EX', t1[i]), 'ua_ToTTestRec1Nest1X', [ttWantAlias]);
    CheckMember(t1[i], 'ua_FldR_E', 'ua_TTestRec1Nest1', [ttWantAlias]);
    CheckMember(t1[i], 'ua_FldR_EX', 'ua_TTestRec1Nest1X', [ttWantAlias]);
    CheckMember(t1[i], 'ua_FldR_F' , 'ua_XoTTestRec1Nest1', [ttWantAlias]);
    CheckMember(t1[i], 'ua_FldR_FX', 'ua_XoTTestRec1Nest1X', [ttWantAlias]);
    CheckMember(t1[i], 'ua_FldR_G' , 'ua_TTestRec1Nest1', [ttWantAlias]);
    CheckMember(t1[i], 'ua_FldR_GX', 'ua_TTestRec1Nest1X', [ttWantAlias]);
    CheckMember(t1[i], 'ua_FldR_R4', 'ua_XoTTestRec1RX', [ttWantAlias]);
    CheckMember(t1[i], 'ua_FldR_R5', 'ua_XoTTestRec1RY', [ttWantAlias]);

    case i of
      0: self_name := 'ua_TTestRec1';
      1: self_name := 'ua_TTestRec1X';
      2: self_name := 'ua_TTestRec1XX';
      else self_name := '';
    end;

    t := CheckMethod(t1[i], 'ua_PN1',  'ua_TTestRec1Nest1', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name, 'ua_PN1');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias]);
    end;
    t := CheckMethod(t1[i], 'ua_PN1X', 'ua_TTestRec1Nest1X', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name, 'ua_PN1X');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias]);
    end;
    t := CheckMethod(t1[i], 'ua_PN2',  'ua_TTestRec1Nest2', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name, 'ua_PN2');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias]);
    end;
    CheckMethodParam(t, 'a', 'ua_TTestRec1', [ttWantAlias]);
    t := CheckMethod(t1[i], 'ua_PN2X', 'ua_TTestRec1Nest2X', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name, 'ua_PN2X');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias]);
    end;
    CheckMethodParam(t, 'a', 'ua_TTestRec1', [ttWantAlias]);
  end;


  ErrInfo := 'RECORD(2): ';
  SetLength(t1, 3);
  t1[0] := CheckVar('Rec1', 'TTestRec1', [ttWantAlias], OffsList, True);
  t1[1] := CheckVar('Rec1X', 'TTestRec1X', [ttWantAlias], OffsList, True);
  t1[2] := CheckVar('Rec1XX', 'TTestRec1XX', [ttWantAlias], OffsList, True);

  for i := 0 to 2 do begin
    CheckMember(t1[i], 'FldRec1', 'ua_TTestRec1', [ttWantAlias]);
    CheckMember(t1[i], 'FldRec2', 'ua_TTestRec1X', [ttWantAlias]);

    case i of
      0: self_name := 'TTestRec1';
      1: self_name := 'TTestRec1X';
      2: self_name := 'TTestRec1XX';
      else self_name := '';
    end;

    t := CheckMethod(t1[i], 'PN0',  'ua_TTestRec1', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name, 'PN0');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias]);
    end;
    t := CheckMethod(t1[i], 'PN0X',  'ua_TTestRec1X', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name, 'PN0X');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias]);
    end;
    t := CheckMethod(t1[i], 'PN1',  'ua_TTestRec1Nest1', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name, 'PN1');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias]);
    end;
    t := CheckMethod(t1[i], 'PN1X', 'ua_TTestRec1Nest1X', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name, 'PN1X');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias]);
    end;
    t := CheckMethod(t1[i], 'PN2',  'ua_TTestRec1Nest2', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name, 'PN2');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias]);
    end;
    t := CheckMethod(t1[i], 'PN2X', 'ua_TTestRec1Nest2X', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name, 'PN2X');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias]);
    end;
  end;

  // object
  ErrInfo := 'OBJECT: ';
  SetLength(t1, 10);
  t1[0] := CheckVar('UA_Obj1',  'ua_TTestObj1', [ttWantAlias], OffsList, True, 'DW_TAG_class_type');
  t1[1] := CheckVar('UA_Obj1X', 'ua_TTestObj1X', [ttWantAlias], OffsList, True, 'DW_TAG_class_type');
  t1[2] := CheckVar('UA_Obj2',  'ua_TTestObj2', [ttWantAlias], OffsList, True, 'DW_TAG_class_type');
  t1[3] := CheckVar('UA_Obj2X', 'ua_TTestObj2X', [ttWantAlias], OffsList, True, 'DW_TAG_class_type');
  t1[4] := CheckVar('UA_ObjX3', 'ua_TTestXObj3', [ttWantAlias], OffsList, True, 'DW_TAG_class_type');
  t1[5] := CheckVar('UA_ObjX3X','ua_TTestXObj3X', [ttWantAlias], OffsList, True, 'DW_TAG_class_type');

  t1[6] := CheckVar('Obj1',  'TTestObj1', [ttWantAlias], 'DW_TAG_class_type');
  //t1[7] := CheckVar('Obj1X', 'TTestObj1X', [ttWantAlias],  'DW_TAG_class_type');
  t1[8] := CheckVar('ObjX1', 'TTestXObj1', [ttWantAlias], 'DW_TAG_class_type');
  //t1[9] := CheckVar('ObjX1X','TTestXObj1X', [ttWantAlias], 'DW_TAG_class_type');

  t := CheckInheritance(t1[2], 'ua_TTestObj1', [ttWantAlias], 'DW_TAG_class_type');
  if t <> t1[0] then DoError('parent of ua_TTestObj2 is wrong');
// Issue # 42047 incorrectly creates inheritance
  //t := CheckInheritance(t1[3], 'ua_TTestObj1', [ttWantAlias], 'DW_TAG_class_type');
  //if t <> t1[0] then DoError('parent of ua_TTestObj2X is wrong');

  t := CheckInheritance(t1[4], 'ua_TTestObj1X', [ttWantAlias], 'DW_TAG_class_type');
  if t <> t1[1] then DoError('parent of ua_TTestXObj3 is wrong');
// Issue # 42047 incorrectly creates inheritance
  //t := CheckInheritance(t1[5], 'ua_TTestObj1X', [ttWantAlias], 'DW_TAG_class_type');
  //if t <> t1[1] then DoError('parent of ua_TTestXObj3X is wrong');

  t := CheckInheritance(t1[6], 'ua_TTestObj1', [ttWantAlias], 'DW_TAG_class_type');
  t := CheckInheritance(t1[8], 'ua_TTestObj1X', [ttWantAlias], 'DW_TAG_class_type');

  for i := 0 to 1 do begin
    CheckMember(t1[i], 'ua_FldO_A' ,  'LongInt', [ttWantAlias, ttAnyType]);
    CheckMember(t1[i], 'ua_FldO_B' ,  'LongInt', [ttWantAlias, ttAnyType]);
    CheckMember(t1[i], 'ua_FldO_C' ,  'ua_TTestObj1Nest1', [ttWantAlias], 'DW_TAG_class_type');
    CheckMember(t1[i], 'ua_FldO_CX' , 'ua_TTestObj1Nest1X', [ttWantAlias], 'DW_TAG_class_type');
    CheckMember(t1[i], 'ua_FldO_D' ,  'ua_TTestObj1Nest2Rec', [ttWantAlias]);
    CheckMember(t1[i], 'ua_FldO_DX' , 'ua_TTestObj1Nest2RecX', [ttWantAlias]);

    case i of
      0: self_name:= 'ua_TTestObj1' ;
      1: self_name:= 'ua_TTestObj1X';
      else self_name := '';
    end;

    t := CheckMethod(t1[i], 'ua_PO1',  'ua_TTestRec1', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name , 'ua_PO1');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias], 'DW_TAG_class_type');
    end;
    t := CheckMethod(t1[i], 'ua_PO1X',  'ua_TTestRec1X', [ttWantAlias]);
    if self_name <> '' then begin
      if HAS_DWARF_CPP then
        CheckLinkageName(t, self_name , 'ua_PO1X');
      CheckMethodParam(t, 'self;this', self_name, [ttWantAlias], 'DW_TAG_class_type');
    end;
    t := CheckMethod(t1[i], 'ua_PO2',  'ua_TTestRec1', [ttWantAlias]);
    t := CheckMethod(t1[i], 'ua_PO2X', 'ua_TTestRec1X', [ttWantAlias]);
  end;
  for i := 2 to 3 do begin
    CheckMember(t1[i], 'ua_FldO_E' ,  'ua_TTestObj1', [ttWantAlias, ttAnyType]);
    CheckMember(t1[i], 'ua_FldO_EX' , 'ua_TTestObj1X', [ttWantAlias, ttAnyType]);
  end;
  for i := 4 to 5 do begin
    CheckMember(t1[i], 'ua_FldO_F' ,  'ua_TTestObj2', [ttWantAlias, ttAnyType]);
    CheckMember(t1[i], 'ua_FldO_FX' , 'ua_TTestObj2X', [ttWantAlias, ttAnyType]);

    t := CheckMethod(t1[i], 'ua_PO2',  'ua_TTestRec1', [ttWantAlias]);
    case i of
      0: CheckLinkageName(t, 'ua_TTestXObj3' , 'ua_PO2');
      1: CheckLinkageName(t, 'ua_TTestXObj3X', 'ua_PO2');
    end;
    t := CheckMethod(t1[i], 'ua_PO2X', 'ua_TTestRec1X', [ttWantAlias]);
    case i of
      0: CheckLinkageName(t, 'ua_TTestXObj3' , 'ua_PO2X');
      1: CheckLinkageName(t, 'ua_TTestXObj3X', 'ua_PO2X');
    end;
  end;

  // class
  ErrInfo := 'CLASS: ';
  SetLength(t1, 7);
  t1[0] := CheckVar('UA_Class1',  'ua_TTestClass1', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, 'DW_TAG_class_type');
  t1[1] := CheckVar('UA_Class1X', 'ua_TTestClass1X', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, 'DW_TAG_class_type');
  t1[2] := CheckVar('UA_Class2',  'ua_TTestClass2', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, 'DW_TAG_class_type');
  t1[3] := CheckVar('UA_Class2X', 'ua_TTestClass2X', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, 'DW_TAG_class_type');
  t1[4] := CheckVar('UA_ClassX3', 'ua_TTestXClass3', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, 'DW_TAG_class_type');
  t1[5] := CheckVar('UA_ClassX3X','ua_TTestXClass3X', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, 'DW_TAG_class_type');
  t1[6] := CheckVar('Class1','TTestClass1', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, 'DW_TAG_class_type');

  t := CheckInheritance(t1[2], 'ua_TTestClass1', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
  if t <> t1[0] then DoError('parent of ua_TTestClass2 is wrong');
// Issue # 42047 incorrectly creates inheritance
  //t := CheckInheritance(t1[3], 'ua_TTestClass1', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
  //if t <> t1[0] then DoError('parent of ua_TTestClass2X is wrong');

  //t := CheckInheritance(t1[4], 'ua_TTestClass1X', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
  //if t <> t1[1] then DoError('parent of ua_TTestXClass3 is wrong');
// Issue # 42047 incorrectly creates inheritance
  //t := CheckInheritance(t1[5], 'ua_TTestClass1X', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
  //if t <> t1[1] then DoError('parent of ua_TTestXClass3X is wrong');

  t := CheckInheritance(t1[6], 'ua_TTestClass1', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
  // cross unit inheritance => may point to a copy

  for i := 0 to 1 do begin
    CheckMember(t1[i], 'ua_FldC_A' ,  'LongInt', [ttWantAlias, ttAllowRefBYPointer, ttAnyType]);
    CheckMember(t1[i], 'ua_FldC_B' ,  'LongInt', [ttWantAlias, ttAllowRefBYPointer, ttAnyType]);
    CheckMember(t1[i], 'ua_FldC_C' ,  'ua_TTestClass1Nest1', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
    CheckMember(t1[i], 'ua_FldC_CX' , 'ua_TTestClass1Nest1X', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
    CheckMember(t1[i], 'ua_FldC_D' ,  'ua_TTestClass1Nest2Rec', [ttWantAlias]);
    CheckMember(t1[i], 'ua_FldC_DX' , 'ua_TTestClass1Nest2RecX', [ttWantAlias]);

    t := CheckMethod(t1[i], 'ua_PO1',  'ua_TTestRec1', [ttWantAlias]);
    if HAS_DWARF_CPP then
    case i of
      0: CheckLinkageName(t, 'ua_TTestClass1' , 'ua_PO1');
      1: CheckLinkageName(t, 'ua_TTestClass1X', 'ua_PO1');
    end;
    t := CheckMethod(t1[i], 'ua_PO1X',  'ua_TTestRec1X', [ttWantAlias]);
    if HAS_DWARF_CPP then
    case i of
      0: CheckLinkageName(t, 'ua_TTestClass1' , 'ua_PO1X');
      1: CheckLinkageName(t, 'ua_TTestClass1X', 'ua_PO1X');
    end;
    t := CheckMethod(t1[i], 'ua_PO2',  'ua_TTestRec1', [ttWantAlias]);
    t := CheckMethod(t1[i], 'ua_PO2X', 'ua_TTestRec1X', [ttWantAlias]);
  end;
  for i := 2 to 3 do begin
    CheckMember(t1[i], 'ua_FldC2_A' ,  'ua_TTestClass1', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
    CheckMember(t1[i], 'ua_FldC2_AX' , 'ua_TTestClass1X', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
  end;
  for i := 4 to 5 do begin
    CheckMember(t1[i], 'ua_FldC_F' ,  'ua_TTestClass2', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
    CheckMember(t1[i], 'ua_FldC_FX' , 'ua_TTestClass2X', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');

    t := CheckMethod(t1[i], 'ua_PO2',  'ua_TTestRec1', [ttWantAlias]);
    case i of
      0: CheckLinkageName(t, 'ua_TTestXClass3' , 'ua_PO2');
      1: CheckLinkageName(t, 'ua_TTestXClass3X', 'ua_PO2');
    end;
    t := CheckMethod(t1[i], 'ua_PO2X', 'ua_TTestRec1X', [ttWantAlias]);
    case i of
      0: CheckLinkageName(t, 'ua_TTestXClass3' , 'ua_PO2X');
      1: CheckLinkageName(t, 'ua_TTestXClass3X', 'ua_PO2X');
    end;
  end;

  ExpTag  := 'DW_TAG_interface_type';
  if DWARF_VERS = 2 then begin
    ExpTag  := 'DW_TAG_class_type';
  end;

  // interface
  ErrInfo := 'INTERFACE: ';
  SetLength(t1, 6);
  t1[0] := CheckVar('UA_IntfCorba1',  'ua_TIntfCorba1', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);
  t1[1] := CheckVar('UA_IntfCorba1X', 'ua_TIntfCorba1X', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);
  t1[2] := CheckVar('UA_IntfCorba2',  'ua_TIntfCorba2', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);
  t1[3] := CheckVar('UA_IntfCorba2X', 'ua_TIntfCorba2X', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);
  t1[4] := CheckVar('IntfCorba1',  'TIntfCorba1', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);
  t1[5] := CheckVar('IntfCorba1X', 'TIntfCorba1X', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);

  CheckInheritance(t1[2], 'ua_TIntfCorba1', [ttWantAlias, ttAllowRefBYPointer], ExpTag);
  CheckInheritance(t1[4], 'ua_TIntfCorba1', [ttWantAlias, ttAllowRefBYPointer], ExpTag);

  // interface
  SetLength(t1, 6);
  t1[0] := CheckVar('UA_IntfCom1',  'ua_TIntfCom1', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);
  t1[1] := CheckVar('UA_IntfCom1X', 'ua_TIntfCom1X', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);
  t1[2] := CheckVar('UA_IntfCom2',  'ua_TIntfCom2', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);
  t1[3] := CheckVar('UA_IntfCom2X', 'ua_TIntfCom2X', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);
  t1[4] := CheckVar('IntfCom1',  'TIntfCom1', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);
  t1[5] := CheckVar('IntfCom1X', 'TIntfCom1X', [ttWantAlias, ttAllowRefBYPointer], OffsList, True, ExpTag);

  CheckInheritance(t1[2], 'ua_TIntfCom1', [ttWantAlias, ttAllowRefBYPointer], ExpTag);
  CheckInheritance(t1[4], 'ua_TIntfCom1', [ttWantAlias, ttAllowRefBYPointer], ExpTag);

  ErrInfo := '';
end;

procedure CheckProp;
var
  owner, owner2: AnsiString;
  t: TDwarfTag;
  t1: array of TDwarfTag;
  i: Integer;
  f: TCheckPropertyFlags;
begin
  SetLength(t1, 4);
  t1[0] := CheckVar('Rec1', 'TTestRec1', [ttWantAlias]);
  t1[1] := CheckVar('Rec1X', 'TTestRec1X', [ttWantAlias]);
  t1[2] := CheckVar('Rec1Y', 'TTestRec1Y', [ttWantAlias]);
  t1[3] := CheckVar('Rec1XX', 'TTestRec1XX', [ttWantAlias]);

  for i := 0 to 3 do begin
    case i of
      0: owner := 'TTestRec1';
      1: owner := 'TTestRec1X';
      2: owner := 'TTestRec1Y';
      3: owner := 'TTestRec1XX';
    end;
    ErrInfo := 'Property '+owner;

    CheckProperty(t1[i], 'P_ua_FldB',  'LongInt', 'ua_FldR_B', 'ua_TTestRec1' );
    CheckProperty(t1[i], 'P_uaX_FldB', 'LongInt', 'ua_FldR_B', 'ua_TTestRec1X' );

    CheckProperty(t1[i], 'P_P1', 'LongInt', 'P1', owner);
  end;


  SetLength(t1, 12);
  t1[0] := CheckVar('Rec3',  'TTestRec3',  [ttWantAlias]);
  t1[1] := CheckVar('Rec3X', 'TTestRec3X', [ttWantAlias]);
  t1[2] := CheckVar('Obj3',  'TTestObj3',  [ttWantAlias], 'DW_TAG_class_type');
  t1[3] := CheckVar('Obj3X', 'TTestObj3X', [ttWantAlias], 'DW_TAG_class_type');
  t1[4] := CheckVar('Obj5',  'TTestObj5',  [ttWantAlias], 'DW_TAG_class_type');
  t1[5] := CheckVar('Obj5X', 'TTestObj5X', [ttWantAlias], 'DW_TAG_class_type');
  t1[6] := CheckVar('ObjX5',  'TTestObjX5',  [ttWantAlias], 'DW_TAG_class_type');
  t1[7] := CheckVar('ObjX5X', 'TTestObjX5X', [ttWantAlias], 'DW_TAG_class_type');
  t1[8] := CheckVar('Obj6',  'TTestObj6',  [ttWantAlias], 'DW_TAG_class_type');
  t1[9] := CheckVar('Obj6X', 'TTestObj6X', [ttWantAlias], 'DW_TAG_class_type');
  t1[10] := CheckVar('Class1',  'TTestClass1', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
  t1[11] := CheckVar('Class3',  'TTestClass3', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');
  //t1[0] := CheckVar('Class1X', 'TTestClass1X', [ttWantAlias, ttAllowRefBYPointer], 'DW_TAG_class_type');

  for i := 0 to 11 do begin
    case i of
      0:  begin owner := 'TTestRec3';   owner2 := owner;            f:=[cpFieldOwned]; end;
      1:  begin owner := 'TTestRec3X';  owner2 := owner;            f:=[cpFieldOwned]; end;
      2:  begin owner := 'TTestObj3';   owner2 := owner;            f:=[cpFieldOwned]; end;
      3:  begin owner := 'TTestObj3X';  owner2 := owner;            f:=[cpFieldOwned]; end;
      4:  begin owner := 'TTestObj5';   owner2 := 'TTestObj4';      f:=[cpFieldInherited]; end;
      5:  begin owner := 'TTestObj5X';  owner2 := 'TTestObj4';      f:=[cpFieldInherited]; end;
      6:  begin owner := 'TTestObjX5';  owner2 := 'TTestObj4X';     f:=[cpFieldInherited]; end;
      7:  begin owner := 'TTestObjX5X'; owner2 := 'TTestObj4X';     f:=[cpFieldInherited]; end;
      8:  begin owner := 'TTestObj6';   owner2 := 'ua_TTestObj4';   f:=[cpFieldInherited]; end;
      9:  begin owner := 'TTestObj6X';  owner2 := 'ua_TTestObj4';   f:=[cpFieldInherited]; end;
      10: begin owner := 'TTestClass1'; owner2 := 'ua_TTestClass1'; f:=[cpFieldInherited]; end;
      11: begin owner := 'TTestClass3'; owner2 := owner;            f:=[cpFieldOwned]; end;
    end;
    ErrInfo := 'Property '+owner;

    CheckProperty(t1[i], 'P_ua_FldD_A',    'LongInt', 'ua_FldC_A', owner2, f);

    CheckProperty(t1[i], 'P_ua_FldD_E_A',  'LongInt', 'ua_FldR2_A', 'ua_TTestRec2' );

    CheckProperty(t1[i], 'P_ua_FldD_E_BA',   'LongInt', 'ua_FldR_A', 'ua_TTestRec1');
    CheckProperty(t1[i], 'P_ua_FldD_E_BXA',  'LongInt', 'ua_FldR_A', 'ua_TTestRec1X');
    CheckProperty(t1[i], 'P_ua_FldD_E_CA',   'LongInt', 'ua_FldR_A', 'ua_TTestRec1');
    CheckProperty(t1[i], 'P_ua_FldD_E_CXA',  'LongInt', 'ua_FldR_A', 'ua_TTestRec1X');
    CheckProperty(t1[i], 'P_ua_FldD_E_Ci',   'LongInt', 'i', '');

    CheckProperty(t1[i], 'P_ua_FldD_EX_BA',  'LongInt', 'ua_FldR_A', 'ua_TTestRec1');
    CheckProperty(t1[i], 'P_ua_FldD_EX_BXA', 'LongInt', 'ua_FldR_A', 'ua_TTestRec1X');
    CheckProperty(t1[i], 'P_ua_FldD_EX_CA',  'LongInt', 'ua_FldR_A', 'ua_TTestRec1');
    CheckProperty(t1[i], 'P_ua_FldD_EX_CXA', 'LongInt', 'ua_FldR_A', 'ua_TTestRec1X');
    CheckProperty(t1[i], 'P_ua_FldD_EX_Ci',  'LongInt', 'i', '');

    CheckProperty(t1[i], 'P_ua_FldD_G_A',    'longint', 'ua_FldR2_A', 'ua_TTestRec2');
    CheckProperty(t1[i], 'P_ua_FldD_GX_A',   'longint', 'ua_FldR2_A', 'ua_TTestRec2X');
    CheckProperty(t1[i], 'P_ua_FldD_XG_A',   'longint', 'ua_FldR2_A', 'ua_TTestRec2');
    CheckProperty(t1[i], 'P_ua_FldD_XGX_A',  'longint', 'ua_FldR2_A', 'ua_TTestRec2X');
    CheckProperty(t1[i], 'P_ua_FldD_GY_A',   'longint', 'ua_FldR2_A', 'ua_TTestRec2');
    CheckProperty(t1[i], 'P_ua_FldD_GYX_A',  'longint', 'ua_FldR2_A', 'ua_TTestRec2X');

    CheckProperty(t1[i], 'P_ua_Func1',   'ua_TTestInt1',  'ua_Func1',  owner2, f);
    t :=
    CheckProperty(t1[i], 'P_ua_Func1_rw','ua_TTestInt1',  'ua_Func1',  owner2, f);
      CheckPropertySetter(t, 'ua_Proc1',  owner2);
    t :=
    CheckProperty(t1[i], 'P_ua_Func1_w', 'ua_TTestInt1',  '',  '');
      CheckPropertySetter(t, 'ua_Proc1',  owner2);
    CheckProperty(t1[i], 'P_ua_Func1X',  'ua_TTestInt1X', 'ua_Func1X', owner2, f);
    t :=
    CheckProperty(t1[i], 'P_ua_Func1X_rw','ua_TTestInt1X',  'ua_Func1X',  owner2, f);
      CheckPropertySetter(t, 'ua_Proc1X',  owner2);

    CheckProperty(t1[i], 'P_ua_Func2',   'ua_TTestInt1',  'ua_Func2',  owner2, f);
    CheckProperty(t1[i], 'P_ua_Func2_rw','ua_TTestInt1',  'ua_Func2',  owner2, f);
    CheckProperty(t1[i], 'P_ua_Func2X',  'ua_TTestInt1X', 'ua_Func2X', owner2, f);

    CheckProperty(t1[i], 'P_ua_Func2i',   'ua_TTestInt1',  'ua_Func2',  owner2, f+[cpHasFormalParam]);
    CheckProperty(t1[i], 'P_ua_Func2iX',  'ua_TTestInt1X', 'ua_Func2X', owner2, f+[cpHasFormalParam]);

    CheckProperty(t1[i], 'P_ua_Func3',   'ua_TTestInt1',  'ua_Func3',  owner2, f);
    CheckProperty(t1[i], 'P_ua_Func3X',  'ua_TTestInt1X', 'ua_Func3X', owner2, f);

    CheckProperty(t1[i], 'P_ua_Func3i',   'ua_TTestInt1',  'ua_Func3',  owner2, f+[cpHasFormalParam]);
    CheckProperty(t1[i], 'P_ua_Func3iX',  'ua_TTestInt1X', 'ua_Func3X', owner2, f+[cpHasFormalParam]);

  end;

  ErrInfo := '';
end;
var t: TDwarfTag;a:TDwarfAttribute;
begin
  (* Use when running outside of "make test" *)
  for i1 := 1 to argc-1 do if argv[i1] = '-cpp' then HAS_DWARF_CPP := true;
  for i1 := 1 to argc-1 do if argv[i1] = '-nocpp' then HAS_DWARF_CPP := false;
  for i1 := 1 to argc-1 do if argv[i1] = '-d2' then DWARF_VERS := 2;
  for i1 := 1 to argc-1 do if argv[i1] = '-d3' then DWARF_VERS := 3;
  for i1 := 1 to argc-1 do if argv[i1] = '-d4' then DWARF_VERS := 4;
  for i1 := 1 to argc-1 do if argv[i1] = '-d5' then DWARF_VERS := 5;

  init;

  DumpDwarfInfo( @ResProc, '', UNIT_NAME);
  DumpDwarfInfo( @ResProc, '', PROG_NAME);

  //for t in RootTag.Descendants do begin
  //  writeln(StringOfChar(' ', t.Depth), '>>', t.TagName, '<< "',t.OffsetAddr, '" "',t.Offset,'"');
  //  for a in t.Attrs do
  //    writeln(StringOfChar(' ', t.Depth+2), '| >>', a.Name, '<< (', a.Form, ') >>',a.Value, '<<');
  //end;

  Check;
  CheckProp;
  writeln('OK');
end.
