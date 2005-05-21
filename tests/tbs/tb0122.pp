{ %OPT= -S2 }

{ Old file: tbs0141.pp }
{ Wrong Class sizes when using forwardly defined classes. OK 0.99.6 }

program bug;

{ uses objpas; not with -S2 !! }
type
   //
   TObjectAB = class;
   TObjectABCD = class;
   TObjectABCDEF = class;
   // }
   TObjectAB = class(tobject)
      a, b: integer;
   end ;
   TObjectABCD = class(TObjectAB)
      c, d: integer;
   end ;
   TObjectABCDEF = class(TObjectABCD)
      e, f: integer;
   end ;

var
   a, b, c: TObject;

begin
a := TObjectAB.Create;
WriteLn(a.InstanceSize, '  Should be: 12');
if a.InstanceSize + SizeOf(integer)*2 <> TObjectABCD.InstanceSize then
  Halt(1);
b := TObjectABCD.Create;
if b.InstanceSize + SizeOf(integer)*2 <> TObjectABCDEF.InstanceSize then
  Halt(1);
WriteLn(b.InstanceSize, '  Should be: 20');
c := TObjectABCDEF.Create;
WriteLn(c.InstanceSize, '  Should be: 28');
end.
{
Here are the VMT tables from the assembler file:
.globl VMT_TD$_TOBJECTAB
VMT_TD$_TOBJECTAB:
 .long 12,-12
 .long VMT_OBJPAS$_TOBJECT
 .long _OBJPAS$$_$$_TOBJECT_DESTROY
 .long _OBJPAS$$_$$_TOBJECT_NEWINSTANCE
 .long _OBJPAS$$_$$_TOBJECT_FREEINSTANCE
 .long _OBJPAS$$_$$_TOBJECT_SAFECALLEXCEPTION$TOBJECT$POINTER
 .long _OBJPAS$$_$$_TOBJECT_DEFAULTHANDLER$$$$
.globl VMT_TD$_TOBJECTABCD
VMT_TD$_TOBJECTABCD:
 .long 12,-12
 .long VMT_TD$_TOBJECTAB
 .long _OBJPAS$$_$$_TOBJECT_DESTROY
 .long _OBJPAS$$_$$_TOBJECT_NEWINSTANCE
 .long _OBJPAS$$_$$_TOBJECT_FREEINSTANCE
 .long _OBJPAS$$_$$_TOBJECT_SAFECALLEXCEPTION$TOBJECT$POINTER
 .long _OBJPAS$$_$$_TOBJECT_DEFAULTHANDLER$$$$
.globl VMT_TD$_TOBJECTABCDEF
VMT_TD$_TOBJECTABCDEF:
 .long 12,-12
 .long VMT_TD$_TOBJECTABCD
 .long _OBJPAS$$_$$_TOBJECT_DESTROY
 .long _OBJPAS$$_$$_TOBJECT_NEWINSTANCE
 .long _OBJPAS$$_$$_TOBJECT_FREEINSTANCE
 .long _OBJPAS$$_$$_TOBJECT_SAFECALLEXCEPTION$TOBJECT$POINTER
 .long _OBJPAS$$_$$_TOBJECT_DEFAULTHANDLER$$$$
}
