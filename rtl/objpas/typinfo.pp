{
    $Id$
    This file is part of the Free Pascal run time library.

    Copyright (c) 1998 by Florian Klaempfl
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

// temporary types:

    type
       PShortString =^ShortString;
       PByte        =^Byte;
       PLongint     =^Longint;
       PBoolean     =^Boolean;
       PSingle      =^Single;
       PDouble      =^Double;
       PExtended    =^Extended;
       PComp        =^Comp;
       PFixed16     =^Fixed16;
       { Doesn't exist ?
       PFIxed32  = ^Fixed32;
       }
       Variant      = Pointer;
       TMethod      = Pointer;

{$MINENUMSIZE 1   this saves a lot of memory }
       // if you change one of the following enumeration types
       // you have also to change the compiler in an appropriate way !
       TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,
                   tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool);

       TTOrdType = (otSByte,otUByte,otSWord,otUWord,otSLong,otULong);

       TFloatType = (ftSingle,ftDouble,ftExtended,ftComp,ftCurr,
                     ftFixed16,ftFixed32);
       TMethodKind = (mkProcedure,mkFunction,mkSafeProcedure,mkSafeFunction);
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
              ({!!!!!!!}
              );
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

    { misc. stuff }
    function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;
    function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;

  implementation

{$ASMMODE ATT}

    function CallIntegerFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint) : Integer;assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // ? Indexed function
         movl Index,%eax
         xorl %eax,%eax
         jnz .LINoPush
         movl IValue,%eax
         pushl %eax
      .LINoPush:
         call (%edi)
         // now the result should be in EAX, untested yet (FK)
      end;

    function CallIntegerProc(s : Pointer;Address : Pointer;Value : Integer; INdex,IVAlue : Longint) : Integer;assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // Push value to set
         movl Value,%eax
         pushl %eax
         // ? Indexed procedure
         movl Index,%eax
         xorl %eax,%eax
         jnz .LIPNoPush
         movl IValue,%eax
         pushl %eax
      .LIPNoPush:
         call (%edi)
         // now the result should be in EAX, untested yet (FK)
      end;

    function CallExtendedFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint) : Extended;assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // ? Indexed function
         movl Index,%eax
         xorl %eax,%eax
         jnz .LINoPush
         movl IValue,%eax
         pushl %eax
      .LINoPush:
         call (%edi)
         //!! now What ??
      end;

    function CallExtendedProc(s : Pointer;Address : Pointer;Value : Extended; INdex,IVAlue : Longint) : Integer;assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // Push value to set
         //!! MUST BE CHANGED !!
         movl Value,%eax
         pushl %eax
         // ? Indexed procedure
         movl Index,%eax
         xorl %eax,%eax
         jnz .LIPNoPush
         movl IValue,%eax
         pushl %eax
      .LIPNoPush:
         call (%edi)
      end;

    function CallBooleanFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Boolean;assembler;
      asm
         movl S,%edi
         movl Address,%edi
         // ? Indexed function
         movl Index,%eax
         xorl %eax,%eax
         jnz .LBNoPush
         movl IValue,%eax
         pushl %eax
      .LBNoPush:
         call (%edi)
         // now the result should be in EAX, untested yet (FK)
      end;

    //!! Assembler functions can't have short stringreturn values.
    //!! So we make a procedure with var parameter.

    Procedure CallSStringFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint;
                            Var Res: Shortstring);assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // ? Indexed function
         movl Index,%eax
         xorl %eax,%eax
         jnz .LSSNoPush
         movl IValue,%eax
         pushl %eax
      .LSSNoPush:
         call (%edi)
         //!! now what ?? MVC
      end;

    Procedure CallSStringProc(s : Pointer;Address : Pointer;Value : ShortString; INdex,IVAlue : Longint);assembler;
      asm
         movl S,%esi
         movl Address,%edi
         // Push value to set
         //!! Is this correct for short strings ????
         movl Value,%eax
         pushl %eax
         // ? Indexed procedure
         movl Index,%eax
         xorl %eax,%eax
         jnz .LSSPNoPush
         movl IValue,%eax
         pushl %eax
      .LSSPNoPush:
         call (%edi)
         //!! now what ? MVC
      end;

    function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;

      begin
         GetTypeData:=PTypeData(TypeInfo)+2+PByte(TypeInfo+1)^;
      end;

    function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;

      var
         hp : PTypeData;
         i : longint;

      begin
         Result:=Nil;
         while Assigned(TypeInfo) do
           begin
              // skip the name
              hp:=GetTypeData(Typeinfo);

              // the class info rtti the property rtti follows
              // immediatly
              Result:=PPropInfo(@hp^.UnitName)+byte(hp^.UnitName[0])+1;
              for i:=1 to hp^.PropCount do
                begin
                   // found a property of that name ?
                   if Result^.Name=PropName then
                     exit;

                   // skip to next property
                   Result:=PPropInfo(@Result^.Name)+byte(Result^.Name[0])+1;
                end;
              // parent class
              Typeinfo:=hp^.ParentInfo;
           end;
      end;

    function IsStoredProp(Instance : TObject;PropInfo : PPropInfo) : Boolean;

      begin
         case (PropInfo^.PropProcs shr 4) and 3 of
            ptfield:
              IsStoredProp:=PBoolean(Pointer(Instance)+Longint(PropInfo^.StoredProc))^;
            ptstatic:
              IsStoredProp:=CallBooleanFunc(Instance,PropInfo^.StoredProc,0,0);
            ptvirtual:
              IsStoredProp:=CallBooleanFunc(Instance,(PPointer(Instance.ClassType)+Longint(PropInfo^.StoredProc)),0,0);
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
        TP:=PPropInfo((@TP^.Name)+PByte(@TP^.Name)^+1);
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
       Move(PL^[I],PL[I+1],Count-I*SizeOf(Pointer));
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
    If Index=0 then
      IValue:=P^.Index
    else
      IValue:=0;
    end;

    function GetOrdProp(Instance : TObject;PropInfo : PPropInfo) : Longint;

      var
         value,Index,Ivalue : longint;

      begin
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              Value:=PLongint(Pointer(Instance)+Longint(PropInfo^.GetProc))^;
            ptstatic:
              Value:=CallIntegerFunc(Instance,PropInfo^.GetProc,Index,IValue);
            ptvirtual:
              Value:=CallIntegerFunc(Instance,
                                     (PPointer(Instance.ClassType)+Longint(PropInfo^.GetProc)),
                                     Index,IValue);
         end;
         { cut off unnecessary stuff }
         case GetTypeData(PropInfo^.PropType)^.OrdType of
            otSWord,otUWord:
              Value:=Value and $ffff;
            otSByte,otUByte:
              Value:=Value and $ff;
         end;
         GetOrdProp:=Value;
      end;

    procedure SetOrdProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Longint);

      Var Index,IValue : Longint;

      begin
         { cut off unnecessary stuff }
         case GetTypeData(PropInfo^.PropType)^.OrdType of
            otSWord,otUWord:
              Value:=Value and $ffff;
            otSByte,otUByte:
              Value:=Value and $ff;
         end;
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              PLongint(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
            ptstatic:
              CallIntegerProc(Instance,PropInfo^.SetProc,Value,Index,IValue);
            ptvirtual:
              CallIntegerProc(Instance,
                              (PPointer(Instance.ClassType)+Longint(PropInfo^.SetProc)),
                              Value,Index,IValue);
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
              Value:=Pointer(CallIntegerFunc(Instance,
                                     (PPointer(Instance.ClassType)+Longint(PropInfo^.GetProc)),
                                     Index,IValue));
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
             CallSSTringFunc(Instance,
                                     (PPointer(Instance.ClassType)+Longint(PropInfo^.GetProc)),
                                     Index,Ivalue,Value);
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
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              PLongint(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Longint(Pointer(Value)) ;
            ptstatic:
              CallIntegerProc(Instance,PropInfo^.SetProc,Longint(Pointer(Value)),Index,IValue);
            ptvirtual:
              CallIntegerProc(Instance,
                              (PPointer(Instance.ClassType)+Longint(PropInfo^.SetProc)),
                              Longint(Pointer(Value)),Index,IValue);
         end;
      end;

    procedure SetSStrProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : ShortString);

   Var Index,IValue: longint;

    begin
      SetIndexValues(PRopInfo,Index,IValue);
         case (PropInfo^.PropProcs) and 3 of
            ptfield:
              PShortString(Pointer(Instance)+Longint(PropInfo^.SetProc))^:=Value;
            ptstatic:
              CallSStringProc(Instance,PropInfo^.GetProc,Value,Index,IValue);
            ptvirtual:
              CallSStringProc(Instance,
                              (PPointer(Instance.ClassType)+Longint(PropInfo^.GetProc)),
                              Value,Index,IValue);
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
              Value:=CallExtendedFunc(Instance,
                                     (PPointer(Instance.ClassType)+Longint(PropInfo^.GetProc)),
                                     Index,IValue);
         end;
         Result:=Value;
      end;

    procedure SetFloatProp(Instance : TObject;PropInfo : PPropInfo;
      Value : Extended);

       Var IValue,Index : longint;

       begin
         SetIndexValues(PropInfo,Index,Ivalue);
         case (PropInfo^.PropProcs) and 3 of
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
              CallExtendedProc(Instance,
                               (PPointer(Instance.ClassType)+Longint(PropInfo^.GetProc)),
                               Value,Index,IValue);
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

      begin
        {!!!!!!!!!!!!}
         Result:=nil;
      end;

    procedure SetMethodProp(Instance : TObject;PropInfo : PPropInfo;
      const Value : TMethod);

      begin
         {!!!!!!!!!!!}
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
         PS:=PS+PByte(PS)^+1;
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
          PS:=PS+PByte(PS)^;
          Inc(Count);
          end;
      end;

end.

{
  $Log$
  Revision 1.20  1999-05-03 07:30:07  michael
  * Fixes in getenum*

  Revision 1.19  1999/04/08 11:31:04  peter
    * removed warnings

  Revision 1.18  1999/01/19 16:08:12  pierre
   ?? is callSStringProc a function ??

  Revision 1.17  1998/12/15 22:43:13  peter
    * removed temp symbols

  Revision 1.16  1998/12/02 12:35:07  michael
  More changes for type-information

  Revision 1.15  1998/11/26 14:57:47  michael
  + Added packrecords 1

  Revision 1.11  1998/09/24 23:45:28  peter
    * updated for auto objpas loading

  Revision 1.10  1998/09/20 08:25:34  florian
    + description of tpropinfo.propprocs bit 6 added

  Revision 1.9  1998/09/19 15:25:45  florian
    * procedure GetOrdProp added

  Revision 1.8  1998/09/19 08:33:53  florian
    + some procedures added

  Revision 1.7  1998/09/08 09:52:31  florian
    * small problems fixed

  Revision 1.6  1998/09/08 00:08:36  michael
  Made it compilable

  Revision 1.5  1998/09/07 23:11:43  florian
    + more fields to TTypeInfo added

  Revision 1.4  1998/09/07 19:34:47  florian
    * constant value is now supported as stored condition

  Revision 1.3  1998/09/07 08:32:59  florian
    + procedure IsStoredProc added

  Revision 1.2  1998/09/06 21:27:05  florian
    + some methods and declarations added

  Revision 1.1  1998/08/25 22:30:00  florian
    + initial revision:
       o constants
       o basic type data record
}
