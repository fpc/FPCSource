{
    $Id$
    This file is part of the Free Pascal run time library.

    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This unit provides the same functionality as the TypInfo Unit }
{ of Delphi                                                     }

unit typinfo;

  interface

{$MODE objfpc}

  uses sysutils;


// temporary types:

    type
       PShortString =^ShortString;
       PByte        =^Byte;
       PWord        =^Word;
       PLongint     =^Longint;
       PBoolean     =^Boolean;
       PSingle      =^Single;
       PDouble      =^Double;
       PExtended    =^Extended;
       PComp        =^Comp;
{$ifdef HASFIXED}
       PFixed16     =^Fixed16;
{$endif HASFIXED}
       { Doesn't exist ?
       PFIxed32  = ^Fixed32;
       }
       Variant      = Pointer;

{$MINENUMSIZE 1   this saves a lot of memory }
       // if you change one of the following enumeration types
       // you have also to change the compiler in an appropriate way !
       TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,
                   tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord);

       TTOrdType = (otSByte,otUByte,otSWord,otUWord,otSLong,otULong);

       TFloatType = (ftSingle,ftDouble,ftExtended,ftComp,ftCurr,
                     ftFixed16,ftFixed32);
       TMethodKind = (mkProcedure,mkFunction,mkConstructor,mkDestructor,
                      mkClassProcedure, mkClassFunction);
       TParamFlags = set of (pfVar,pfConst,pfArray,pfAddress,pfReference,pfOut);
       TIntfFlags = set of (ifHasGuid,ifDispInterface,ifDispatch);

{$MINENUMSIZE DEFAULT}

   const
      ptField = 0;
      ptStatic = 1;
      ptVirtual = 2;
      ptConst = 3;

      tkString        = tkSString;

   type
      TTypeKinds = set of TTypeKind;

{$PACKRECORDS 1}
      TTypeInfo = record
         Kind : TTypeKind;
         Name : ShortString;
         // here the type data follows as TTypeData record
      end;

      PTypeInfo = ^TTypeInfo;
      PPTypeInfo = ^PTypeInfo;

      PTypeData = ^TTypeData;
      TTypeData = packed record
         case TTypeKind of
            tkUnKnown,tkLString,tkWString,tkAString,tkVariant:
              ();
            tkInteger,tkChar,tkEnumeration,tkWChar:
              (OrdType : TTOrdType;
               case TTypeKind of
                  tkInteger,tkChar,tkEnumeration,tkBool,tkWChar : (
                    MinValue,MaxValue : Longint;
                    case TTypeKind of
                      tkEnumeration:
                        (
                        BaseType : PTypeInfo;
                        NameList : ShortString)
                    );
                  tkSet:
                    (CompType : PTypeInfo)
              );
            tkFloat:
              (FloatType : TFloatType);
            tkSString:
              (MaxLength : Byte);
            tkClass:
              (ClassType : TClass;
               ParentInfo : PTypeInfo;
               PropCount : SmallInt;
               UnitName : ShortString
               // here the properties follow as array of TPropInfo
              );
            tkMethod:
              (MethodKind : TMethodKind;
               ParamCount : Byte;
               ParamList : array[0..1023] of Char
             {in reality ParamList is a array[1..ParamCount] of:
                  record
                    Flags : TParamFlags;
                    ParamName : ShortString;
                    TypeName : ShortString;
                  end;
              followed by
                  ResultType : ShortString}
              );
            tkInt64:
              (MinInt64Value, MaxInt64Value: Int64);
            tkQWord:
              (MinQWordValue, MaxQWordValue: QWord);
            tkInterface:
              ({!!!!!!!}
              );
      end;

      // unsed, just for completeness
      TPropData = packed record
        PropCount : Word;
        PropList : record end;
      end;

      PPropInfo = ^TPropInfo;
      TPropInfo = packed record
        PropType : PTypeInfo;
        GetProc : Pointer;
        SetProc : Pointer;
        StoredProc : Pointer;
        Index : Integer;
        Default : Longint;
        NameIndex : SmallInt;

        // contains the type of the Get/Set/Storedproc, see also ptxxx
        // bit 0..1 GetProc
        //     2..3 SetProc
        //     4..5 StoredProc
        //     6 : true, constant index property
        PropProcs : Byte;

        Name : ShortString;
      end;

      TProcInfoProc = procedure(PropInfo : PPropInfo) of object;

      PPropList = ^TPropList;
      TPropList = array[0..65535] of PPropInfo;

   const
      tkAny = [Low(TTypeKind)..High(TTypeKind)];
      tkMethods = [tkMethod];
      tkProperties = tkAny-tkMethods-[tkUnknown];

    { general property handling }
    // just skips the id and the name
    function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;

    // searches in the property PropName
    function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;
    procedure GetPropInfos(TypeInfo : PTypeInfo;PropList : PPropList);
    function GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds;
      PropList : PPropList) : Integer;

    // returns true, if PropInfo is a stored property
    function IsStoredProp(Instance : TObject;PropInfo : PPropInfo) : Boolean;

    { subroutines to read/write properties }
    function GetOrdProp(Instance : TObject;PropInfo : PPropInfo) : Longint;
    procedure SetOrdProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Longint);

    function GetStrProp(Instance : TObject;PropInfo : PPropInfo) : Ansistring;
    procedure SetStrProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : Ansistring);

    function GetFloatProp(Instance : TObject;PropInfo : PPropInfo) : Extended;
    procedure SetFloatProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Extended);

    function GetVariantProp(Instance : TObject;PropInfo : PPropInfo): Variant;
    procedure SetVariantProp(Instance : TObject;PropInfo : PPropInfo;
      const Value: Variant);

    function GetMethodProp(Instance : TObject;PropInfo : PPropInfo) : TMethod;
    procedure SetMethodProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : TMethod);

    function GetInt64Prop(Instance: TObject; PropInfo: PPropInfo): Int64;
    procedure SetInt64Prop(Instance: TObject; PropInfo: PPropInfo;
      const Value: Int64);

    { misc. stuff }
    function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;
    function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;


  const
    BooleanIdents: array[Boolean] of String = ('False', 'True');
    DotSep: String = '.';


  implementation

  type

    PMethod = ^TMethod;


{$ASMMODE ATT}

    function CallIntegerFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint) : Integer;assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // ? Indexed function
         movl Index,%eax
         testl %eax,%eax
         je .LINoPush
         movl IValue,%eax
         pushl %eax
      .LINoPush:
         push %esi
         call %edi
         // now the result is in EAX
      end;

    function CallIntegerProc(s : Pointer;Address : Pointer;Value : Integer; INdex,IValue : Longint) : Integer;assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // Push value to set
         movl Value,%eax
         pushl %eax
         // ? Indexed procedure
         movl Index,%eax
         testl %eax,%eax
         je .LIPNoPush
         movl IValue,%eax
         pushl %eax
      .LIPNoPush:
         pushl %esi
         call %edi
      end;

    function CallExtendedFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint) : Extended;assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // ? Indexed function
         movl Index,%eax
         testl %eax,%eax
         je .LINoPush
         movl IValue,%eax
         pushl %eax
      .LINoPush:
         push %esi
         call %edi
         //
      end;

    function CallExtendedProc(s : Pointer;Address : Pointer;Value : Extended; INdex,IVAlue : Longint) : Integer;assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // Push value to set
         leal Value,%eax
         pushl (%eax)
         pushl 4(%eax)
         pushl 8(%eax)
         // ? Indexed procedure
         movl Index,%eax
         testl %eax,%eax
         je .LIPNoPush
         movl IValue,%eax
         pushl %eax
      .LIPNoPush:
         push %esi
         call %edi
      end;

    function CallBooleanFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Boolean;assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // ? Indexed function
         movl Index,%eax
         testl %eax,%eax
         je .LBNoPush
         movl IValue,%eax
         pushl %eax
      .LBNoPush:
         push %esi
         call %edi
      end;

    // Assembler functions can't have short stringreturn values.
    // So we make a procedure with var parameter.
    // That's not true (FK)

    Procedure CallSStringFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint;
                            Var Res: Shortstring);assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // ? Indexed function
         movl Index,%eax
         testl %eax,%eax
         jnz .LSSNoPush
         movl IValue,%eax
         pushl %eax
         // the result is stored in an invisible parameter
         pushl Res
      .LSSNoPush:
         push %esi
         call %edi
      end;

    Procedure CallSStringProc(s : Pointer;Address : Pointer;Const Value : ShortString; INdex,IVAlue : Longint);assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // Push value to set
         movl Value,%eax
         pushl %eax
         // ? Indexed procedure
         movl Index,%eax
         testl %eax,%eax
         jnz .LSSPNoPush
         movl IValue,%eax
         pushl %eax
      .LSSPNoPush:
         push %esi
         call %edi
      end;

    function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;

      begin
         GetTypeData:=PTypeData(pointer(TypeInfo)+2+PByte(pointer(TypeInfo)+1)^);
      end;

    function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;

      var
         hp : PTypeData;
         i : longint;
         p : string;

      begin
         P:=UpCase(PropName);
         while Assigned(TypeInfo) do
           begin
              // skip the name
              hp:=GetTypeData(Typeinfo);

              // the class info rtti the property rtti follows
              // immediatly
              Result:=PPropInfo(pointer(@hp^.UnitName)+Length(hp^.UnitName)+1+SizeOF(Word));
              for i:=1 to hp^.PropCount do
                begin
                   // found a property of that name ?
                   if Upcase(Result^.Name)=P then
                     exit;

                   // skip to next property
                   Result:=PPropInfo(pointer(@Result^.Name)+byte(Result^.Name[0])+1);
                end;
              // parent class
              Typeinfo:=hp^.ParentInfo;
           end;
         Result:=Nil;
      end;

    function IsStoredProp(Instance : TObject;PropInfo : PPropInfo) : Boolean;

      begin
         case (PropInfo^.PropProcs shr 4) and 3 of
            ptfield:
              IsStoredProp:=PBoolean(Pointer(Instance)+Longint(PropInfo^.StoredProc))^;
            ptstatic:
              IsStoredProp:=CallBooleanFunc(Instance,PropInfo^.StoredProc,0,0);
            ptvirtual:
              IsStoredProp:=CallBooleanFunc(Instance,ppointer(Pointer(Instance.ClassType)+Longint(PropInfo^.StoredProc))^,0,0);
            ptconst:
              IsStoredProp:=LongBool(PropInfo^.StoredProc);
         end;
      end;

    procedure GetPropInfos(TypeInfo : PTypeInfo;PropList : PPropList);
      {
        Store Pointers to property information in the list pointed
        to by proplist. PRopList must contain enough space to hold ALL
        properties.
      }
      Type PWord = ^Word;

      Var TD : PTypeData;
          TP : PPropInfo;
          Count : Longint;

      begin
      TD:=GetTypeData(TypeInfo);
      // Get this objects TOTAL published properties count
      TP:=(@TD^.UnitName+Length(TD^.UnitName)+1);
      Count:=PWord(TP)^;
      // Now point TP to first propinfo record.
      Inc(Longint(TP),SizeOF(Word));
      While Count>0 do
        begin
        PropList^[0]:=TP;
        Inc(Longint(PropList),SizeOf(Pointer));
        // Point to TP next propinfo record.
        // Located at Name[Length(Name)+1] !
        TP:=PPropInfo(pointer(@TP^.Name)+PByte(@TP^.Name)^+1);
        Dec(Count);
        end;
      // recursive call for parent info.
      If TD^.Parentinfo<>Nil then
        GetPropInfos (TD^.ParentInfo,PropList);
      end;

    Procedure InsertProp (PL : PProplist;PI : PPropInfo; Count : longint);

    VAr I : Longint;

    begin
     I:=0;
     While (I<Count) and (PI^.Name>PL^[I]^.Name) do Inc(I);
     If I<Count then
       Move(PL^[I], PL^[I+1], (Count - I) * SizeOf(Pointer));
     PL^[I]:=PI;
    end;

    function GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds;
      PropList : PPropList) : Integer;

      {
        Store Pointers to property information OF A CERTAIN KIND in the list pointed
        to by proplist. PRopList must contain enough space to hold ALL
        properties.
      }
      Var TempList : PPropList;
          PropInfo : PPropinfo;
          I,Count : longint;

      begin
        Result:=0;
        Count:=GetTypeData(TypeInfo)^.Propcount;
        If Count>0 then
          begin
          GetMem(TempList,Count*SizeOf(Pointer));
          Try
            GetPropInfos(TypeInfo,TempList);
            For I:=0 to Count-1 do
              begin
              PropInfo:=TempList^[i];
              If PropInfo^.PropType^.Kind in TypeKinds then
                begin
                InsertProp(PropList,PropInfo,Result);
                Inc(Result);
                end;
              end;
          finally
            FreeMem(TempList,Count*SizeOf(Pointer));
          end;
          end;
      end;

    Procedure SetIndexValues (P: PPRopInfo; Var Index,IValue : Longint);

    begin
    Index:=((P^.PropProcs shr 6) and 1);
    If Index<>0 then
      IValue:=P^.Index
    else
      IValue:=0;
    end;

    function GetOrdProp(Instance : TObject;PropInfo : PPropInfo) : Longint;

      var
         value,Index,Ivalue : longint;
         TypeInfo: PTypeInfo;

      begin
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              Value:=PLongint(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
            ptstatic:
              Value:=CallIntegerFunc(Instance,PropInfo^.GetProc,Index,IValue);
            ptvirtual:
              Value:=CallIntegerFunc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,Index,IValue);
         end;
         { cut off unnecessary stuff }
         TypeInfo := PropInfo^.PropType;
         case TypeInfo^.Kind of
           tkChar, tkBool:
             Value:=Value and $ff;
           tkWChar:
             Value:=Value and $ffff;
           tkInteger:
             case GetTypeData(TypeInfo)^.OrdType of
                otSWord,otUWord:
                  Value:=Value and $ffff;
                otSByte,otUByte:
                  Value:=Value and $ff;
             end;
         end;
         GetOrdProp:=Value;
      end;

    procedure SetOrdProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Longint);

      var
        Index,IValue : Longint;
        DataSize: Integer;

      begin
         { cut off unnecessary stuff }
         case GetTypeData(PropInfo^.PropType)^.OrdType of
            otSWord,otUWord: begin
                Value:=Value and $ffff;
                DataSize := 2;
              end;
            otSByte,otUByte: begin
                Value:=Value and $ff;
                DataSize := 1;
            end;
           else DataSize := 4;
         end;
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs shr 2) and 3 of
            ptfield:
              case DataSize of
                1: PByte(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Byte(Value);
                2: PWord(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Word(Value);
                4: PLongint(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
              end;
            ptstatic:
              CallIntegerProc(Instance,PropInfo^.SetProc,Value,Index,IValue);
            ptvirtual:
              CallIntegerProc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^,Value,Index,IValue);
         end;
      end;

    Function GetAStrProp(Instance : TObject;PropInfo : PPropInfo):Pointer;

      {
      Dirty trick based on fact that AnsiString is just a pointer,
      hence can be treated like an integer type.
      }

      var
         value : Pointer;
         Index,Ivalue : Longint;

      begin
         SetIndexValues(PropInfo,Index,IValue);
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              Value:=Pointer(PLongint(Pointer(Instance)+Longint(PropInfo^.GetProc))^);
            ptstatic:
              Value:=Pointer(CallIntegerFunc(Instance,PropInfo^.GetProc,Index,IValue));
            ptvirtual:
              Value:=Pointer(CallIntegerFunc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,Index,IValue));
         end;
         GetAstrProp:=Value;
      end;

    Function GetSStrProp(Instance : TObject;PropInfo : PPropInfo):ShortString;

      var
         value : ShortString;
         Index,IValue : Longint;

      begin
         SetIndexValues(PropInfo,Index,IValue);
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              Value:=PShortString(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
            ptstatic:
             CallSStringFunc(Instance,PropInfo^.GetProc,Index,IValue,Value);
            ptvirtual:
             CallSSTringFunc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,Index,Ivalue,Value);
         end;
         GetSStrProp:=Value;
      end;

    function GetStrProp(Instance : TObject;PropInfo : PPropInfo) : Ansistring;

      begin
      Case Propinfo^.PropType^.Kind of
        tkSString : Result:=GetSStrProp(Instance,PropInfo);
        tkAString : Pointer(Result):=GetAStrProp(Instance,Propinfo);
      else
        Result:='';
      end;
      end;

    procedure SetAStrProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : AnsiString);

      {
      Dirty trick based on fact that AnsiString is just a pointer,
      hence can be treated like an integer type.
      }

      var
         Index,Ivalue : Longint;
      begin
         SetIndexValues(PropInfo,Index,IValue);
         case (PropInfo^.PropProcs shr 2) and 3 of
            ptfield:
              PLongint(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Longint(Pointer(Value)) ;
            ptstatic:
              CallIntegerProc(Instance,PropInfo^.SetProc,Longint(Pointer(Value)),Index,IValue);
            ptvirtual:
              CallIntegerProc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^,Longint(Pointer(Value)),Index,IValue);
         end;
      end;

    procedure SetSStrProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : ShortString);

   Var Index,IValue: longint;

    begin
      SetIndexValues(PRopInfo,Index,IValue);
         case (PropInfo^.PropProcs shr 2) and 3 of
            ptfield:
              PShortString(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
            ptstatic:
              CallSStringProc(Instance,PropInfo^.GetProc,Value,Index,IValue);
            ptvirtual:
              CallSStringProc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,Value,Index,IValue);
         end;
    end;

    procedure SetStrProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : AnsiString);

      begin
      Case Propinfo^.PropType^.Kind of
        tkSString : SetSStrProp(Instance,PropInfo,Value);
        tkAString : SetAStrProp(Instance,Propinfo,Value);
      end;
      end;

    function GetFloatProp(Instance : TObject;PropInfo : PPropInfo) : Extended;

      var
         Index,Ivalue : longint;
         Value : Extended;


      begin
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              Case GetTypeData(PropInfo^.PropType)^.FloatType of
               ftSingle:
                 Value:=PSingle(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
               ftDouble:
                 Value:=PDouble(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
               ftExtended:
                 Value:=PExtended(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
               ftcomp:
                 Value:=PComp(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
               { Uncommenting this code results in a internal error!!
               ftFixed16:
                 Value:=PFixed16(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
               ftfixed32:
                 Value:=PFixed32(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
               }
               end;
            ptstatic:
              Value:=CallExtendedFunc(Instance,PropInfo^.GetProc,Index,IValue);
            ptvirtual:
              Value:=CallExtendedFunc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,Index,IValue);
         end;
         Result:=Value;
      end;

    procedure SetFloatProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Extended);

       Var IValue,Index : longint;

       begin
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs shr 2) and 3 of
            ptfield:
              Case GetTypeData(PropInfo^.PropType)^.FloatType of
               ftSingle:
                 PSingle(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
               ftDouble:
                 PDouble(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
               ftExtended:
                 PExtended(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
               ftcomp:
                 PComp(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Comp(Value);
               { Uncommenting this code results in a internal error!!
               ftFixed16:
                 PFixed16(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
               ftfixed32:
                 PFixed32(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
               }
               end;
            ptstatic:
              CallExtendedProc(Instance,PropInfo^.SetProc,Value,Index,IValue);
            ptvirtual:
              CallExtendedProc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,Value,Index,IValue);
         end;
      end;

    function GetVariantProp(Instance : TObject;PropInfo : PPropInfo): Variant;

      begin
         {!!!!!!!!!!!}
         Result:=nil;
      end;

    procedure SetVariantProp(Instance : TObject;PropInfo : PPropInfo;
      const Value: Variant);

      begin
         {!!!!!!!!!!!}
      end;

    function GetMethodProp(Instance : TObject;PropInfo : PPropInfo) : TMethod;

      var
         value: PMethod;
         Index,Ivalue : longint;

      begin
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              Value:=PMethod(Pointer(Instance)+Longint(PropInfo^.GetProc));
            ptstatic:
              Value:=PMethod(CallIntegerFunc(Instance,PropInfo^.GetProc,Index,IValue));
            ptvirtual:
              Value:=PMethod(CallIntegerFunc(Instance,PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.GetProc))^,Index,IValue));
         end;
         GetMethodProp:=Value^;
      end;

    procedure SetMethodProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : TMethod);

      var
        Index,IValue : Longint;

      begin
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs shr 2) and 3 of
            ptfield:
              PMethod(Pointer(Instance)+Longint(PropInfo^.SetProc))^ := Value;
            ptstatic:
              CallIntegerProc(Instance,PropInfo^.SetProc,Integer(@Value), Index, IValue);
            ptvirtual:
              CallIntegerProc(Instance,
                PPointer(Pointer(Instance.ClassType)+Longint(PropInfo^.SetProc))^,
                Integer(@Value), Index, IValue);
         end;
      end;

    function GetInt64Prop(Instance: TObject; PropInfo: PPropInfo): Int64;
    begin
      // !!!: Implement me!
      Result := 0;
    end;

    procedure SetInt64Prop(Instance: TObject; PropInfo: PPropInfo;
      const Value: Int64);
    begin
      // !!!: Implement me!
    end;

    function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;

      Var PS : PShortString;
          PT : PTypeData;

      begin
       PT:=GetTypeData(TypeInfo);
       // ^.BaseType);
       //      If PT^.MinValue<0 then Value:=Ord(Value<>0); {map to 0/1}
       PS:=@PT^.NameList;
       While Value>0 Do
        begin
          PS:=PShortString(pointer(PS)+PByte(PS)^+1);
          Dec(Value);
        end;
       Result:=PS^;
      end;

    function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;

      Var PS : PShortString;
          PT : PTypeData;
          Count : longint;

      begin
        If Length(Name)=0 then exit(-1);
        PT:=GetTypeData(TypeInfo);
        Count:=0;
        Result:=-1;
        PS:=@PT^.NameList;
        While (Result=-1) and (PByte(PS)^<>0) do
          begin
          If PS^=Name then
            Result:=Count;
          PS:=PShortString(pointer(PS)+PByte(PS)^);
          Inc(Count);
          end;
      end;

end.

{
  $Log$
  Revision 1.42  2000-06-22 20:02:51  peter
    * qword,int64 rtti support basics

  Revision 1.41  2000/06/22 15:31:09  sg
  * Fixed a small typo in my previous update

  Revision 1.40  2000/06/22 15:29:31  sg
  * Added prototypes for GetInt64Prop and SetInt64Prop
  * Added constants "BooleanIdents" and "DotSep"

  Revision 1.39  2000/05/18 09:42:17  michael
  + GetPropInfo now case insensitive

  Revision 1.38  2000/02/15 14:39:56  florian
    * disabled FIXED data type per default

  Revision 1.37  2000/02/09 16:59:33  peter
    * truncated log

  Revision 1.36  2000/01/07 16:41:44  daniel
    * copyright 2000

  Revision 1.35  2000/01/07 16:32:29  daniel
    * copyright 2000 added

  Revision 1.34  2000/01/06 01:08:33  sg
  * _This_ is the real revision 1.32... :-)

  Revision 1.33  2000/01/06 00:23:24  pierre
   * missing declarations for otChar andotWChar added

  Revision 1.32  2000/01/05 18:59:56  sg
  * Fixed missing () in InsertProp which caused memory corruptions
  * GetOrdProp handles Char and WChar now. (there are still some
    property types missing!)

  Revision 1.31  1999/12/28 12:19:36  jonas
    * replaced "movl mem,%eax; xorl %eax,%eax" with "movl mem,%eax;
      testl %eax,%eax"

  Revision 1.30  1999/11/06 14:41:31  peter
    * truncated log

  Revision 1.29  1999/09/16 08:59:48  florian
    * GetPropInfo returns now nil if the property wasn't found

  Revision 1.28  1999/09/15 20:27:24  florian
    + patch of Sebastion Guenther applied: Get/SetMethodProp implementation

  Revision 1.27  1999/09/08 16:14:43  peter
    * pointer fixes

  Revision 1.26  1999/09/03 15:39:23  michael
  * Fixes from Sebastian Guenther

  Revision 1.25  1999/08/29 22:21:27  michael
  * Patch from Sebastian Guenther

  Revision 1.24  1999/08/06 13:21:40  michael
  * Patch from Sebastian Guenther

}