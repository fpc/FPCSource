//This is only for testing the parser, it is not intended to be runable in a real
//program but for checking the contructs to be parsed well.
//All statements are written like testparser would print them out to diff the 
//result with this file again to show differences. 
//Based on /utils/fpdoc/testunit.pp
{$mode objfpc}
{$h+}
unit testunit1;

interface

 uses 
  SysUtils,Classes;

 const
  AnIntegerConst=1;
  AStringConst='Hello, World!';
  AFLoatconst=1.23;
  ABooleanConst=True;
  ATypedConst: Integer=3;
  AnArrayConst: Array[1..3] of Integer=(1,2,3);
  ARecordConst: TMethod=(Code:nil;Data:nil);
  ASetConst=[true,false];
  ADeprecatedConst=1 deprecated;
   
 Type
  TAnEnumType=(one,two,three);
  TASetType=set of TAnEnumType;
  TAnArrayType=Array[1..10] of Integer;
  TASubRangeType=one..two;
  TABooleanArrayType=Array[Boolean] of Integer;  
  TARecordType=record
                   X,Y: Integer;
                   Z: String;
                      end;
  TAVariantRecordType=record
                          A: String;
                          Case Integer of
                        1: (X,Y : Integer);
                        2: (phi,Omega : Real);
                         end; 
  TAVariantRecordType2=record
                          A: String;
                          Case Atype : Integer of
                            1 : (X,Y : Integer);
                            2 : (phi,Omega : Real);
                          end; 
                          
  MyRec = Record  
          X : Longint;  
          Case byte of  
            2 : (Y : Longint;  
                 case byte of  
                 3 : (Z : Longint);  
                 );  
          end;                           
                          
//  TADeprecatedType = Integer deprecated;

  { TMyParentClass }

  TMyParentClass=Class(TComponent)
  Private 
    FI: Integer;
    Function GetA(AIndex: Integer): String;
    Function GetIP(AIndex: integer): String;
    procedure SetA(AIndex: Integer; const AValue: String);
    procedure SetIP(AIndex: integer; const AValue: String);
    Procedure WriteI(AI: Integer);
    Function ReadI: Integer;
  Protected
    Procedure AProtectedMethod;
    Property AProtectedProp: Integer Read FI Write FI;  
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure AVirtualProc; virtual;
    Procedure AnAbstractProc; virtual; abstract;
    Procedure AMessageProc(var Msg);message 123;
    Procedure AStringMessageProc(var Msg);message '123';
    Procedure ADeprecatedProc; deprecated;
    Procedure APlatformProc; Platform;
    Property IntProp: Integer Read FI Write Fi;
    Property IntROProp: Integer Read FI;
    Property GetIntProp: Integer Read ReadI Write WriteI;
    Property AnArrayProp[AIndex: Integer]: String Read GetA Write SetA;
    Property AnIndexProp: String Index 1 Read GetIP Write SetIP;
    Property AnIndexProp2: String Index 2 Read GetIP Write SetIP;
  Published
    Procedure SomePublishedMethod;
  end;
  
  { TMyChildClass }

  TMyChildClass=Class(TMyParentClass)
  Public
    Procedure AVirtualProc; Override;
    Procedure AnAbstractProc; Override;
  Published
    Property AProtectedProp;
  end;
  
 TPasFunctionType=Class(TPasProcedureType)
  public
    destructor Destroy; override;
    Class Function TypeName: string; override;
    Function ElementTypeName: string; override;
    Function GetDeclaration(Full: boolean): string; override;
  public
    ResultEl: TPasResultElement;
  end; 
                        
 var
  ASimpleVar: Integer;  
  ATypedVar: TMethod;
  ARecordVar: Record
                 A,B: Integer;
               end;
  AnArrayVar: Array[1..10] of Integer;
  ATypedArray: Array[TanEnumType] of Integer;
  AInitVar: Integer=1;
  
  ADeprecatedVar: Integer deprecated;
  ACVarVar: Integer ; cvar;
  AnExternalVar: Integer ;external name 'avar';
  AnExternalLibVar: Integer ;external 'library' name 'avar';
      
 Procedure SimpleProc;
 Procedure OverloadedProc(A: Integer);
 Procedure OverloadedProc(B: String);
 Function SimpleFunc: Integer;
 Function OverloadedFunc(A: Integer): Integer;
 Function OverloadedFunc(B: String): Integer;  

 Procedure ConstArgProc(const A: Integer); 
 Procedure VarArgProc(var A: Integer); 
 Procedure OutArgProc(out A: Integer); 
 Procedure UntypedVarArgProc(var A); 
 Procedure UntypedConstArgProc(const A); 
 Procedure UntypedOutArgProc(out A); 

 Procedure ArrayArgProc(A: TAnArrayType);
 Procedure OpenArrayArgProc(A: Array of string);
 Procedure ConstArrayArgProc(A: Array of const);

 Procedure externalproc; external;
 Procedure externalnameProc; external name 'aname';
 Procedure externallibnameProc; external 'alibrary' name 'aname';

  
Implementation


 Procedure SimpleProc;

 procedure  SubProc;
  begin
   s:= s+'a';
  end;

 begin
  a:= 1;
  c:= a+b;
  for i:= 1 to 10 do 
    write(a);
 end;

 Procedure OverloadedProc(A: Integer);
 begin
  if i=1 then ;
 end;

 Procedure OverloadedProc(B: String);
 begin
 end;

 Function SimpleFunc: Integer;
 begin
 end;

 Function OverloadedFunc(A: Integer): Integer; 
 begin
 end;

 Function OverloadedFunc(B: String): Integer;  
 begin
 end;

 Procedure ArrayArgProc(A: TAnArrayType);
 begin
 end;

 Procedure OpenArrayArgProc(A: Array of String);
 begin
 end;

 Procedure ConstArrayArgProc(A: Array of const);
 begin
 end;

 Procedure ConstArgProc(const A: Integer); 
 begin
 end;

 Procedure VarArgProc(var A: Integer); 
 begin
 end;

 Procedure OutArgProc(out A: Integer); 
 begin
 end;

 Procedure UntypedVarArgProc(var A); 
 begin
 end;

 Procedure UntypedConstArgProc(const A); 
 begin
 end;

 Procedure UntypedOutArgProc(out A); 
 begin
 end;

{ TMyChildClass }
 procedure TMyChildClass.AVirtualProc;
 begin
  inherited AVirtualProc;
 end;

 procedure TMyChildClass.AnAbstractProc;
 procedure  SubCProc;
  begin
   sc:= sc+'ac';
  end;

 begin
  // Cannot call ancestor
 end;

{ TMyParentClass }
 procedure TMyParentClass.WriteI(AI: Integer);
 begin
 end;

 Function TMyParentClass.GetA(AIndex: Integer): String;
 begin
 end;

 Function TMyParentClass.GetIP(AIndex: integer): String;
 begin
 end;

 procedure TMyParentClass.SetA(AIndex: Integer; const AValue: String);
 begin
 end;

 procedure TMyParentClass.SetIP(AIndex: integer; const AValue: String);
 begin
 end;

 Function TMyParentClass.ReadI: Integer;
 begin
 end;

 procedure TMyParentClass.AProtectedMethod;
 begin
 end;

 constructor TMyParentClass.Create(AOwner: TComponent);
 begin
  inherited Create(AOwner);
 end;

 destructor TMyParentClass.Destroy;
 begin
  inherited Destroy;
 end;

 procedure TMyParentClass.AVirtualProc;
 begin
 end;

 procedure TMyParentClass.AMessageProc(var Msg);
 begin
 end;

 procedure TMyParentClass.AStringMessageProc(var Msg);
 begin
 end;

 procedure TMyParentClass.ADeprecatedProc;
 begin
 end;

 procedure TMyParentClass.APlatformProc;
 begin
 end;

 procedure TMyParentClass.SomePublishedMethod;
 begin
 end;
 
 Class Function TPasFunctionType.TypeName: String;
 begin
  Result:= 'Function';
 end;

 procedure Statements;
 const
  cint=1;
  cint1=-1;
  creal=3.1415;
  Addi=1+2;
  Subs=2-3;
  Muti=3*3;
  Divi=3/5;
  //Powe=2^3;
  Modu=5 mod 3;
  IDiv=5 div 3;
  fals= not TRUE;
  cand=true and false;
  cor=true or false;
  cxor=true xor false;
  lt=2<3;
  gt=3>2;
  let=2<=3;
  get=3>=2;
  LeftShift=2 shl 3;
  RightShift=2 shr 3;
  ConstString='01'+'ab';

 Type
  Passenger=Record
                Name: String[30];
                Flight: String[10];
  end;

 Type 
  AR=record
      X,Y: LongInt;
     end;
  //PAR = Record;
 var
  TheCustomer: Passenger;
  L: ^LongInt;
  P: PPChar;
  S,T: Ar;
      
 begin
  X:= X+Y;
  //EparserError on C++ style
  //X+=Y;      { Same as X := X+Y, needs -Sc command line switch}
  //x-=y;
  //X/=2;      { Same as X := X/2, needs -Sc command line switch}
  //x*=y;
  Done:= False;
  Weather:= Good;
  //MyPi := 4* Tan(1); warum * ?
  L^:= 3;
  P^^:= 'A';
  Usage;
  WriteLn('Pascal is an easy language !');
  Doit();
  //label jumpto;
  //Jumpto :
  //  Statement;
  //Goto jumpto;

  Case i of
    3: DoSomething;
    1..5: DoSomethingElse;
  end;

  Case C of  
    'a': WriteLn('A pressed');
    'b': WriteLn('B pressed');
    'c': WriteLn('C pressed');
  else  
   WriteLn('unknown letter pressed : ',C);
  end;

  Case C of
    'a','e','i','o','u': WriteLn('vowel pressed');
    'y': WriteLn('This one depends on the language');
  else
   WriteLn('Consonant pressed');
  end;

  Case Number of
    1..10: WriteLn('Small number');
    11..100: WriteLn('Normal, medium number');
  else
   WriteLn('HUGE number');
  end;

  case block of
    1: begin
	writeln('1');
	end;
    2: writeln('2');
  else
    writeln('3');
    writeln('4');
  end;

  If exp1 Then
    If exp2 then
      Stat1
  else
    stat2;

  If exp3 Then
      begin
      If exp4 then
	Stat5
      else
	stat6
      end;

  If exp7 Then
    begin
    If exp8 then
	Stat9
    end
  else
    stat2;

 if i is integer then
  begin
    write('integer');
  end
  else 
    if i is real then 
  begin
    write('real');
  end
  else 
    write('0'); 

  if Today in[Monday..Friday] then
    WriteLn('Must work harder')
  else
    WriteLn('Take a day off.');

  for Day:= Monday to Friday do 
    Work;
  for I:= 100 downto 1 do
    WriteLn('Counting down : ',i);
  for I:= 1 to 7*dwarfs do 
    KissDwarf(i);

  for i:= 0 to 10 do
    begin
    j:= 2+1;
    write(i,j);
    end;

  repeat
    WriteLn('I =',i);
    I:= I+2;
  until I>100;
    
  repeat
    X:= X/2;
  until x<10e-3;

  I:= I+2;
  while i<=100 do
    begin
     WriteLn('I =',i);
     I:= I+2;
    end;
    X:= X/2;
    while x>=10e-3 do 
      dec(x);

    while x>0 do 
    while y>0 do 
      begin
	dec(x);
	dec(y);
      end;

    while x>0 do
    if x>2 then 
     dec(x)
    else 
     dec(x,2);

      X:= 2+3;

    TheCustomer.Name:= 'Michael';
    TheCustomer.Flight:= 'PS901';

    With TheCustomer do
      begin
       Name:= 'Michael';
       Flight:= 'PS901';
      end;

  With A,B,C,D do
   Statement;

    With A do
     With B do
      With C do
       With D do 
        Statement;

    S.X:= 1;S.Y:= 1;
    T.X:= 2;T.Y:= 2;
    With S,T do
      WriteLn(X,' ',Y);

    {asm
      Movl $1,%ebx
      Movl $0,%eax
      addl %eax,%ebx
    end; ['EAX','EBX'];}

    try
	try
	  M:= ParseSource(E,cmdl,'linux','i386');
	except
	  on excep: EParserError do
	    begin
	      writeln(excep.message,' line:',excep.row,' column:',excep.column,' file:',excep.filename);
	      raise ;
	  end;
	end;
	Decls:= M.InterfaceSection.Declarations;
	for I:= 0 to Decls.Count-1 do
	  Writeln('Interface item ',I,': ');

	FreeAndNil(M);
    finally
	FreeAndNil(E)
   end;
   
   raise EParserError.Create(Format(SParserErrorAtToken, [Msg, CurTokenName]) {$ifdef addlocation}+' ('+inttostr(scanner.currow)+' '+inttostr(scanner.curcolumn)+')'{$endif},Scanner.CurFilename, Scanner.CurRow, Scanner.CurColumn);
    
    // try else
 end;

 procedure Expression;
 begin
  A:= a+b *c /(-e+f)*3 div 2 + 4 mod 5 - 2 shl 3 + 3 shr 1 ;
  b:= (a and not b) or c xor d;
  u:= i<=2 or a<>b or j>=3;
  u:= i=1 or a>b or b<a or i<>2;
  u:= i in [1..2];

 If F=@AddOne Then  
  WriteLn('Functions are equal');

 If F()=Addone then  
  WriteLn('Functions return same values ');

 z:= [today,tomorrow];
 z:= [Monday..Friday,Sunday];
 z:= [2,3*2,6*2,9*2];
 z:= ['A'..'Z','a'..'z','0'..'9'];

 x:= Byte('A');
 x:= Char(48);
 x:= boolean(1);
 x:= longint(@Buffer);
 x:= Integer('A');
 x:= Char(4875);
 x:= Word(@Buffer);

 B:= Byte(C);
 Char(B):= C;

 TWordRec(W).L:= $FF;
 TWordRec(W).H:= 0;
 S:= TObject(P).ClassName;

 P:= @MyProc; //warum @ ? fix pparser 769 ?

 Dirname:= Dirname+'\';

 W:= [mon,tue]+[wed,thu,fri]; // equals [mon,tue,wed,thu,fri]
 W:= [mon,tue,wed]-[wed];     // equals [mon,tue]
 W:= [mon,tue,wed]*[wed,thu,fri]; // equals [wed] warum * ?

 (C as TEdit).Text:= 'Some text';
 C:= O as TComponent;

 if A is TComponent then ;
 If A is B then ;

 Inherited ;
 Inherited Test;

  if true then
    Inherited
  else
    DoNothing;

  if true then
    Inherited Test
  else
    DoNothing;

   Inherited P:= 3;  
   Inherited SetP1(3); 
   Result:= Char(P and $FF);  
   Result:= Char((Inherited P) and $FF);  
   Inherited P:= Ord(AValue);
   Result:= Inherited InterPretOption(Cmd,Arg);

  raise Exception.Create(SErrMultipleSourceFiles);

  if Filename<>'' then
	  raise Exception.Create(SErrMultipleSourceFiles);

  if Filename<>'' then
	  raise Exception.Create(SErrMultipleSourceFiles)
	else
	  Filename:= s;

  Self.Write(EscapeText(AText)); 
  TObject.Create(Self);
 end;

 constructor TPasPackage.Create(const AName: String; AParent: TPasElement);
 begin
  if (Length(AName)>0)and(AName[1]<>'#') then
   Inherited Create('#'+AName,AParent)
  else
   Inherited Create(AName,AParent);
  Modules:= TList.Create;
 end;         

 Function TPascalScanner.FetchToken: TToken;
 var
  IncludeStackItem: TIncludeStackItem;

 begin
  while true do
  begin
    Result:= DoFetchToken;
     if FCurToken=tkEOF then
      if FIncludeStack.Count>0 then
      begin
        CurSourceFile.Free;
        IncludeStackItem:= TIncludeStackItem(FIncludeStack[FIncludeStack.Count-1]);
        FIncludeStack.Delete(FIncludeStack.Count-1);
        FCurSourceFile:= IncludeStackItem.SourceFile;
        FCurFilename:= IncludeStackItem.Filename;
        FCurToken:= IncludeStackItem.Token;
        FCurTokenString:= IncludeStackItem.TokenString;
        FCurLine:= IncludeStackItem.Line;
        FCurRow:= IncludeStackItem.Row;
        TokenStr:= IncludeStackItem.TokenStr;
        IncludeStackItem.Free;
        Result:= FCurToken;
      end 
    else
      break
    else
      if not PPIsSkipping then
        break;
  end;
 end;  

 Procedure IFS;
 begin
  if true then
   repeat
   until false
  else
    Noting;
 end;           


 Procedure IFS(x: integer); overload;
 begin
  if true then
    case x of
     1: writeln;
     2: write;
   else 
    writeln('#');
   end
  else
    Noting;
 end;

 Procedure IFS1; 
 begin
  if true then
    while true do
     Something
  else
    Noting;
 end;

 Procedure IFS3;
 begin
  if true then
   if true then 
    write
   else 
    writeln;
 end; 

Initialization
 
  hallo:= valid;
end.
