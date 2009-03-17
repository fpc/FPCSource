{$M+}
program RTTI132;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$packenum 1}
{$ELSE}
 {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  TypInfo,
  Classes;

type
  TMyObject = class;
  TBatch = Procedure (Var S:String) of Object; stdcall;
  TProcess = function (Var S:String; const A:integer):int64 of Object; stdcall;
  TArray = function (Var Array1:Array of String; const P:Pointer; Out Out1:int64):int64 of Object; stdcall;
  TOnFour = function (A: array of byte; const B: array of byte;
    var C: array of byte; out D: array of byte): TComponent  of object;
  TOnFive = procedure (Component1: TComponent;
    var Component2: TComponent;
    out Component3: TComponent;
    const Component4: TComponent) of object;
  TOnSix = function (const A: string; var Two: integer;
    out Three: TMyObject; Four: PInteger; Five: array of Byte;
    Six: integer): string of object;
  
  TMyObject=Class(TObject)
   private
    FFieldOne : Integer;
    FFieldTwo  : String;
    FOnBatch :TBatch;
    FOnFour: TOnFour;
    FOnFive: TOnFive;
    FOnSix: TOnSix;
  FOnProcess : TProcess;
  FOnArray: TArray;
  
    Procedure ProcNo1(Var S:String); stdcall;
    Procedure ProcNo2(Var S:String); stdcall;
   public
    Function IF_Exist:Boolean;
    Property FP1:Integer read FFieldOne Write FFieldOne;
   published
    Property FP2:String read FFieldTwo  Write FFieldTwo ;
    Property OnTraitement:TBatch read FOnBatch Write FOnBatch;
    Property OnProcess:TProcess read FOnProcess Write FOnProcess;
    Property OnArray:TArray read FOnArray Write FOnArray;
    function FourthPublished(A: array of byte; const B: array of byte;
      var C: array of byte; out D: array of byte): TComponent;
    procedure FifthPublished(Component1: TComponent;
      var Component2: TComponent;
      out Component3: TComponent;
      const Component4: TComponent);
    function SixthPublished(const A: string; var Two: integer;
      out Three: TMyObject; Four: PInteger;
      Five: array of Byte; Six: integer): string;
    property OnFour: TOnFour read FOnFour write FOnFour;
    property OnFive: TOnFive read FOnFive write FOnFive;
    property OnSix: TOnSix read FOnSix write FOnSix;
  end;

  PShortString=^ShortString;

  // This record is the same in typinfo.pas compiler source file TTypeData record
  PParameter= ^Parameter;
  Parameter=Record
    Flags: TParamFlags;
    ParamName: ShortString;
    TypeName: ShortString;
   end;

   ParametersMethod1=Record
    AMat : Array[1..20] of Parameter;
     // for function
    ResultType: ShortString;
   end;

Var OneObject  : TMyObject;
    NumI, I: Integer;
    List_of_Prop: TPropList;
    List_of_Param : ParametersMethod1;


Function TMyObject.IF_Exist:Boolean;
Begin
 result:=True;
end;

Procedure TMyObject.ProcNo1(Var S:String); stdcall;
Begin
 S:='The Batch execute the procedure TMyObject.ProcNo1';
end;

Procedure TMyObject.ProcNo2(Var S:String); stdcall;
Begin
 S:='The Batch execute the procedure TMyObject.ProcNo2';
end;

function TMyObject.FourthPublished(A: array of byte; const B: array of byte;
      var C: array of byte; out D: array of byte): TComponent;
begin
  Result := nil;
end;

procedure TMyObject.FifthPublished(Component1: TComponent;
      var Component2: TComponent;
      out Component3: TComponent;
      const Component4: TComponent);
begin
end;

function TMyObject.SixthPublished(const A: string; var Two: integer;
      out Three: TMyObject; Four: PInteger;
      Five: array of Byte; Six: integer): string;
begin
end;

Function BuildMethodDefinition(Liste: ParametersMethod1;NumI :Integer):String;
//Build the definition of  method
var
  Definition: String;
  i: integer;
begin
 Result:='';
 Definition := '(';
 For i:= 1 to NumI do
 begin
  if pfVar in Liste.AMat[I].Flags
   then Definition := Definition+('var ');
  if pfconst in Liste.AMat[I].Flags
   then Definition := Definition+('const ');
  if pfArray in Liste.AMat[I].Flags
   then Definition := Definition+('array of ');
  if pfAddress in Liste.AMat[I].Flags
   then Definition := Definition+('adresse ?'); // If Self ?
  if pfReference in Liste.AMat[I].Flags
   then Definition := Definition+('reference ?'); // ??
  if pfout in Liste.AMat[I].Flags
   then Definition := Definition+('out ');

  Definition := Format('%s%s: %s', [Definition, Liste.AMat[I].ParamName, Liste.AMat[I].TypeName]);
  If I<NumI
   then Definition := Definition + '; '
 end;
  Definition := Definition + ')';

 if Liste.ResultType<>''
  then Definition := Format('%s: %s', [Definition, Liste.ResultType]);
 Definition := Definition+' of object;';
 Result:=Definition;
end;

procedure SetArrayParameter(ParameterCurrent: PParameter; NumI :Integer; TypeMethode:TMethodKind);
var
  TypeParameter : PShortString;
  i: integer;
begin
 i := 1;
 while i <= NumI do
 begin

  List_of_Param.AMat[I].Flags:=ParameterCurrent^.Flags;
  List_of_Param.AMat[I].ParamName:=ParameterCurrent^.ParamName;

  //  type parameter 
  TypeParameter := Pointer(Integer(@ParameterCurrent^.ParamName) +
                   Length(ParameterCurrent^.ParamName)+1);

  List_of_Param.AMat[I].TypeName:=TypeParameter^;

  // Finding Next parameter using a pointer 

  inc(i);
                  // Address of  current parameter
  ParameterCurrent := PParameter(Integer(ParameterCurrent) +
                  // size of Tparamflags 
                 SizeOf(TParamFlags) +
                  // length of ParamName string 
                 (Length(ParameterCurrent^.ParamName) + 1) +
                  // length of TypeParameter string 
                 (Length(TypeParameter^)+1));
 end;
  // If it is the last parameter and if the method is a  fonction
  // Next Working for the result type function
  if TypeMethode = mkFunction
   then List_of_Param.ResultType:=PShortString(ParameterCurrent)^
   else List_of_Param.ResultType:='';
end;

procedure DisplayDetails(Informations : TPropInfo; const expectedresult: ansistring);
var
 PropTypeZ : String;
 DTypeData: PTypeData;
 Method : TMethod;
 Resultat : String;
 OrdinalValue,
   CurrentParamPosition,
   ParamNameLength,
   i, j         : integer;
   ParamName,
   TypeName     : string;
   TypeData     : PTypeData;
   newTypeInfo  : PTypeInfo;
   EnumerationName : PString;
   ProcessThisProperty : boolean;
   Fu_ResultType: String;
   Flags: TParamFlags;
   Flag:byte;
   Definition: String;
begin
  // Finding property type 
  With Informations do
  begin
   Writeln('Property Type : ',PropType^.Name);
   Write('Getter :');
   if not Assigned(GetProc)
    then Writeln('Nil')
    else Writeln(Format('%p', [GetProc]));

   Write('Setter :');
   if not Assigned(SetProc)
    then Writeln('Nil')
    else Writeln(Format('%p', [SetProc]));

   Write('StoredProc :');
   if not Assigned(StoredProc)
    then Writeln('Nil')
    else Writeln(Format('%p%', [StoredProc]));

   Writeln('Index :',Index);
   Writeln('Default :',Default);
   Writeln('NameIndex :',NameIndex);

   case PropType^.Kind of
    tkInteger : writeln('<tkinteger>');
    tkLString : writeln('<tklstring>');
    //tkString  : writeln('Longueur max ='); string pascal max 255?
    tkMethod  :
      Begin
        writeln('>>> Methode Type >>>');
        //Information for the method type : tkmethod
        //  TPropInfo.PropType= PPTypeInfo;
        //  GetTypeData(TypeInfo: PTypeInfo) send PTypeData
        //  PTypeData is for finding MethodKind 
{$IFDEF FPC}
        DTypeData:= GetTypeData(PTypeInfo(PropType));
{$ELSE}
        DTypeData:= GetTypeData(PTypeInfo(PropType^));
{$ENDIF}                  
         // Détermine le type de la méthode
        Case DTypeData^.MethodKind of
         mkProcedure: PropTypeZ := 'procedure';
         mkFunction: PropTypeZ := 'function';
         mkConstructor: PropTypeZ := 'constructor';
         mkDestructor: PropTypeZ := 'destructor';
         mkClassProcedure: PropTypeZ := 'class procedure';
         mkClassFunction: PropTypeZ := 'class function';
        end;
        Writeln('Number of Parameters : ',DTypeData^.ParamCount);

        Writeln('Parameter List : ');//,DTypeData^.ParamList);

{$IFDEF delphibuiltin}
        With DTypeData^ do
          SetArrayParameter(@DTypeData^.ParamList,ParamCount,MethodKind);
{$ELSE}
       //================================
        Definition:='(';
//              Definition := Definition+'(';
        CurrentParamPosition := 0;
        for i:= 1 to DTypeData^.ParamCount do
          begin
            { First Handle the ParamFlag }
           Flag:=byte(DTypeData^.ParamList[CurrentParamPosition]);
           Flags:=TParamFlags(Flag);
           writeln('ord(Flags):',ord(DTypeData^.ParamList[CurrentParamPosition]));
//         For i:= 1 to NumI do
//         begin
            if pfVar in Flags
             then Definition := Definition+('var ');
            if pfconst in Flags
             then Definition := Definition+('const ');
            if pfArray in Flags
             then Definition := Definition+('array of ');
            if pfAddress in Flags
             then Definition := Definition+('adresse ?'); // si Self ?
            if pfReference in Flags
             then Definition := Definition+('reference ?'); // ??
            if pfout in Flags
             then Definition := Definition+('out ');
           
           { Next char is the length of the ParamName}
           inc(CurrentParamPosition);
           ParamNameLength := ord( DTypeData^.ParamList[CurrentParamPosition]);
           { Next extract the Name of the Parameter }
           ParamName := '';
           for j := CurrentParamPosition + 1 to CurrentParamPosition + ParamNameLength do
             ParamName := ParamName + DTypeData^.ParamList[j];
           CurrentParamPosition := CurrentParamPosition + ParamNameLength;
           { Next extract the Type of the Parameter }
           inc(CurrentParamPosition);
           ParamNameLength := ord( DTypeData^.ParamList[CurrentParamPosition]);
           writeln('Length type:',ParamNameLength);
                 TypeName := '';
           for j := CurrentParamPosition + 1 to CurrentParamPosition + ParamNameLength do
             TypeName  := TypeName + DTypeData^.ParamList[j];
           CurrentParamPosition := CurrentParamPosition +
                                   ParamNameLength + 1;
           writeln('ParamName:',i,':', ParamName);
           writeln('TypeName:',i,':', TypeName);
           Definition := Format('%s%s: %s', [Definition, ParamName, TypeName]);
           If I<DTypeData^.ParamCount  then Definition := Definition + '; '
         end;
        if DTypeData^.MethodKind = mkFunction then
          begin
            ParamNameLength := ord( DTypeData^.ParamList[CurrentParamPosition]);
            Fu_ResultType := '';
            for j := CurrentParamPosition + 1 to CurrentParamPosition + ParamNameLength do
              Fu_ResultType  := Fu_ResultType + DTypeData^.ParamList[j];
          end 
        else
          Fu_ResultType:='';
  //         end;
        Definition := Definition + ')';
        if Fu_ResultType<>''  then 
          Definition := Format('%s: %s', [Definition, Fu_ResultType]);
        Definition := Definition+' of object;';

     //=================================
                   // Build  the definion of  method 
        Writeln(PropTypeZ+' '+Definition);
        if ((PropTypeZ+' '+Definition) <> expectedresult) then
          begin
            writeln(expectedresult);
            halt(1);
          end;
{$ENDIF}
{$IFDEF delphibuiltin}
        Writeln(PropTypeZ+' '+BuildMethodDefinition(List_of_Param,DTypeData^.ParamCount));
{$ENDIF}          
        Method := GetMethodProp(OneObject, Informations.Name);
        if Method.Code <> NIL then
         begin
          Resultat:='';
          TBatch(Method)(Resultat);
          Writeln(Resultat);
         end;
      end;
   end;
  end;
end;

const
  expectedresults: array[0..6] of ansistring = (
    '',
    'function (out Out1: Int64; const P: Pointer; var array of reference ?Array1: AnsiString): Int64 of object;',
    'procedure (adresse ?Component1: TComponent; var adresse ?Component2: TComponent; adresse ?out Component3: TComponent; const adresse ?Component4: TComponent) of object;',
    'function (array of reference ?A: Byte; const array of reference ?B: Byte; var array of reference ?C: Byte; array of reference ?out D: Byte): TComponent of object;',
    'function (const A: LongInt; var S: AnsiString): Int64 of object;',
    'function (const A: AnsiString; var Two: LongInt; adresse ?out Three: TMyObject; Four: PInteger; array of reference ?Five: Byte; Six: LongInt): AnsiString of object;',
    'procedure (var S: AnsiString) of object;'
    );
begin
 OneObject:=TMyObject.Create;
 OneObject.FP1:=3;
  //OneObject.OnTraitement:=Nil; // GetMethodProp => Method.Code=Nil
{$IFDEF FPC}
 OneObject.OnTraitement:=@OneObject.ProcNo1;//(vartrait1);
{$ELSE}
 OneObject.OnTraitement:=OneObject.ProcNo1;//(vartrait1);
{$ENDIF}
 // Get list properties
 NumI := GetPropList(TMyObject.ClassInfo, tkAny, @List_of_Prop);
 writeln('numi: ',numi);
 if (numi<>length(expectedresults)) then
   halt(44);
 for I := 0 to NumI-1 do
  begin
   Writeln('Propriete ',I+1,' = ',List_of_Prop[I]^.Name);
   DisplayDetails(List_of_Prop[I]^,expectedresults[i]);
   Writeln;
  end;
 { Other 
 GetPropInfos(TMyObject.ClassInfo, @List_of_Prop);
 for I := 0 to GetTypeData(TMyObject.ClassInfo).PropCount-1 do
  begin
   Writeln('Property ',I+1,' = ',List_of_Prop[I]^.Name);
   DisplayDetails(List_of_Prop[I]^);
  end;
 }
 OneObject.Free;
end.

