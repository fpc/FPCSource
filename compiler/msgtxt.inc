{$ifdef Delphi}
const msgtxt : array[0..000108] of string[240]=(
{$else Delphi}
const msgtxt : array[0..000108,1..240] of char=(
{$endif Delphi}
  'T_Compiler: $1'#000+
  'D_Compiler OS: $1'#000+
  'I_Target OS: $1'#000+
  'T_Using executable path: $1'#000+
  'T_Using unit path: $1'#000+
  'T_Using include path: $1'#000+
  'T_Using library path: $1'#000+
  'T_Using object path: $1'#000+
  'I_$1 Lines compiled, $2 sec'#000+
  'F_No memory left'#000+
  'I_Writing Resource Str','ing Table file: $1'#000+
  'E_Writing Resource String Table file: $1'#000+
  'F_Unexpected end of file'#000+
  'F_String exceeds line'#000+
  'F_illegal character'#000+
  'F_Syntax error, $1 expected but $2 found'#000+
  'T_Start reading includefile $1'#000+
  'W_Comment level $1 found'#000+
  'N_$F directive (','FAR) ignored'#000+
  'N_Stack check is global under Linux'#000+
  'N_Ignored compiler switch $1'#000+
  'W_Illegal compiler switch $1'#000+
  'W_This compiler switch has a global effect'#000+
  'E_Illegal char constant'#000+
  'F_Can'#039't open file $1'#000+
  'F_Can'#039't open include file $1'#000+
  'E_Too many $ENDI','Fs or $ELSEs'#000+
  'W_Records fields can be aligned to 1,2,4 or 16 bytes only'#000+
  'W_Enumerated can be saved in 1,2 or 4 bytes only'#000+
  'E_$ENDIF expected for $1 $2 defined in line $3'#000+
  'E_Syntax error while parsing a conditional compiling expression'#000+
  'E_Evaluat','ing a conditional compiling expression'#000+
  'W_Macro contents is cut after char 255 to evalute expression'#000+
  'E_ENDIF without IF(N)DEF'#000+
  'F_User defined: $1'#000+
  'E_User defined: $1'#000+
  'W_User defined: $1'#000+
  'N_User defined: $1'#000+
  'H_User defined: $1'#000+
  'I_User defined: $1'#000+
  'E','_Keyword redefined as macro has no effect'#000+
  'F_Macro buffer overflow while reading or expanding a macro'#000+
  'W_Extension of macros exceeds a deep of 16.'#000+
  'E_compiler switches aren'#039't allowed in (* ... *) styled comments'#000+
  'D_Handling switch "$1"'#000+
  'C_ENDIF ','$1 found'#000+
  'C_IFDEF $1 found, $2'#000+
  'C_IFOPT $1 found, $2'#000+
  'C_IF $1 found, $2'#000+
  'C_IFNDEF $1 found, $2'#000+
  'C_ELSE $1 found, $2'#000+
  'C_Skipping until...'#000+
  'I_Press <return> to continue'#000+
  'W_Unsupported switch $1'#000+
  'W_Illegal compiler directive $1'#000+
  'T_Back in $1'#000+
  'W_Unsupport','ed application type: $1'#000+
  'W_APPTYPE isn'#039't support by the target OS'#000+
  'W_DESCRIPTION is only supported for OS2 and Win32'#000+
  'N_VERSION is not supported by target OS.'#000+
  'N_VERSION only for exes or DLLs'#000+
  'W_Wrong format for VERSION directive $1'#000+
  'W_Unsupporte','d assembler style specified $1'#000+
  'W_ASM reader switch is not possible inside asm statement, $1 will be e'+
  'ffective only for next'#000+
  'E_Wrong switch toggle, use ON/OFF or +/-'#000+
  'E_Resource files are not supported for this target'#000+
  'W_Include environment $1',' not found in environment'#000+
  'E_Illegal value for FPU register limit'#000+
  'E_Parser - Syntax Error'#000+
  'W_Procedure type FAR ignored'#000+
  'W_Procedure type NEAR ignored'#000+
  'W_Procedure type REGISTER ignored'#000+
  'E_No DLL File specified'#000+
  'E_Duplicate exported function name',' $1'#000+
  'E_Duplicate exported function index $1'#000+
  'E_Invalid index for exported function'#000+
  'W_Relocatable DLL or executable $1 debug info does not work, disabled.'+
  #000+
  'W_To allow debugging for win32 code you need to disable relocation wit'+
  'h -WN option'#000+
  'E_Con','structor name must be INIT'#000+
  'E_Destructor name must be DONE'#000+
  'E_Illegal open parameter'#000+
  'E_Procedure type INLINE not supported'#000+
  'W_Private methods shouldn'#039't be VIRTUAL'#000+
  'W_Constructor should be public'#000+
  'W_Destructor should be public'#000+
  'N_Class should have',' one destructor only'#000+
  'E_Local class definitions are not allowed'#000+
  'F_Anonym class definitions are not allowed'#000+
  'E_The object $1 has no VMT'#000+
  'E_Illegal parameter list'#000+
  'E_Wrong parameter type specified for arg no. $1'#000+
  'E_Wrong amount of parameters speci','fied'#000+
  'E_overloaded identifier $1 isn'#039't a function'#000+
  'E_overloaded functions have the same parameter list'#000+
  'E_function header doesn'#039't match the forward declaration $1'#000+
  'E_function header $1 doesn'#039't match forward : var name changes $2 ='+
  '> $3'#000+
  'N_Values ','in enumeration types have to be ascending'#000+
  'N_Interface and implementation names are different $1 => $2'#000+
  'E_With can not be used for variables in a different segment'#000+
  'E_function nesting > 31'#000+
  'E_range check error while evaluating constants'#000+
  'W_range',' check error while evaluating constants'#000+
  'E_duplicate case label'#000+
  'E_Upper bound of case range is less than lower bound'#000+
  'E_typed constants of classes are not allowed'#000+
  'E_functions variables of overloaded functions are not allowed'#000+
  'E_string length m','ust be a value from 1 to 255'#000+
  'W_use extended syntax of NEW and DISPOSE for instances of objects'#000+
  'W_use of NEW or DISPOSE for untyped pointers is meaningless'#000+
  'E_use of NEW or DISPOSE is not possible for untyped pointers'#000+
  'E_class identifier expec','ted'#000+
  'E_type identifier not allowed here'#000+
  'E_method identifier expected'#000+
  'E_function header doesn'#039't match any method of this class'#000+
  'P_procedure/function $1'#000+
  'E_Illegal floating point constant'#000+
  'E_FAIL can be used in constructors only'#000+
  'E_Destructors can',#039't have parameters'#000+
  'E_Only class methods can be referred with class references'#000+
  'E_Only class methods can be accessed in class methods'#000+
  'E_Constant and CASE types do not match'#000+
  'E_The symbol can'#039't be exported from a library'#000+
  'W_An inherited method i','s hidden by $1'#000+
  'E_There is no method in an ancestor class to be overridden: $1'#000+
  'E_No member is provided to access property'#000+
  'W_Stored prorperty directive is not yet implemented'#000+
  'E_Illegal symbol for property access'#000+
  'E_Cannot access a protected fi','eld of an object here'#000+
  'E_Cannot access a private field of an object here'#000+
  'W_overloaded method of virtual method should be virtual: $1'#000+
  'W_overloaded method of non-virtual method should be non-virtual: $1'#000+
  'E_overloaded methods which are virtual m','ust have the same return typ'+
  'e: $1'#000+
  'E_EXPORT declared functions can'#039't be nested'#000+
  'E_methods can'#039't be EXPORTed'#000+
  'E_call by var parameters have to match exactly: Got $1 expected $2'#000+
  'E_Class isn'#039't a parent class of the current class'#000+
  'E_SELF is only al','lowed in methods'#000+
  'E_methods can be only in other methods called direct with type identif'+
  'ier of the class'#000+
  'E_Illegal use of '#039':'#039#000+
  'E_range check error in set constructor or duplicate set element'#000+
  'E_Pointer to object expected'#000+
  'E_Expression must be c','onstructor call'#000+
  'E_Expression must be destructor call'#000+
  'E_Illegal order of record elements'#000+
  'E_Expression type must be class or record type'#000+
  'E_Procedures can'#039't return a value'#000+
  'E_constructors and destructors must be methods'#000+
  'E_Operator is not overlo','aded'#000+
  'E_Re-raise isn'#039't possible there'#000+
  'E_The extended syntax of new or dispose isn'#039't allowed for a class'#000+
  'E_Assembler incompatible with function return type'#000+
  'E_Procedure overloading is switched off'#000+
  'E_It is not possible to overload this operator',' (overload = instead)'#000+
  'E_Comparative operator must return a boolean value'#000+
  'E_Only virtual methods can be abstract'#000+
  'F_Use of unsupported feature!'#000+
  'E_The mix of CLASSES and OBJECTS isn'#039't allowed'#000+
  'W_Unknown procedure directive had to be ignored: $1',#000+
  'E_absolute can only be associated to ONE variable'#000+
  'E_absolute can only be associated a var or const'#000+
  'E_Only ONE variable can be initialized'#000+
  'E_Abstract methods shouldn'#039't have any definition (with function bo'+
  'dy)'#000+
  'E_This overloaded function can'#039,'t be local (must be exported)'#000+
  'W_Virtual methods are used without a constructor in $1'#000+
  'M_Macro defined: $1'#000+
  'M_Macro undefined: $1'#000+
  'M_Macro $1 set to $2'#000+
  'I_Compiling $1'#000+
  'U_Parsing interface of unit $1'#000+
  'U_Parsing implementation of $1'#000+
  'D_Compiling $1 ','for the second time'#000+
  'E_Array properties aren'#039't allowed here'#000+
  'E_No property found to override'#000+
  'E_Only one default property is allowed, found inherited default proper'+
  'ty in class $1'#000+
  'E_The default property must be an array property'#000+
  'E_Virtual const','ructors are only supported in class object model'#000+
  'E_No default property available'#000+
  'E_The class can'#039't have a published section, use the {$M+} switch'#000+
  'E_Forward declaration of class $1 must be resolved here to use the cla'+
  'ss as ancestor'#000+
  'E_Local o','perators not supported'#000+
  'E_Procedure directive $1 not allowed in interface section'#000+
  'E_Procedure directive $1 not allowed in implementation section'#000+
  'E_Procedure directive $1 not allowed in procvar declaration'#000+
  'E_Function is already declared Publi','c/Forward $1'#000+
  'E_Can'#039't use both EXPORT and EXTERNAL'#000+
  'E_NAME keyword expected'#000+
  'W_$1 not yet supported inside inline procedure/function'#000+
  'W_Inlining disabled'#000+
  'I_Writing Browser log $1'#000+
  'H_may be pointer dereference is missing'#000+
  'F_Selected assembler read','er not supported'#000+
  'E_Procedure directive $1 has conflicts with other directives'#000+
  'E_Calling convention doesn'#039't match forward'#000+
  'E_Register calling (fastcall) not supported'#000+
  'E_Property can'#039't have a default value'#000+
  'E_The default value of a property mus','t be constant'#000+
  'E_Symbol can'#039't be published, can be only a class'#000+
  'E_That kind of property can'#039't be published'#000+
  'W_Empty import name specified'#000+
  'W_An import name is required'#000+
  'E_Function internal name changed after use of function'#000+
  'E_Division by zero'#000+
  'E','_Invalid floating point operation'#000+
  'E_Upper bound of range is less than lower bound'#000+
  'E_string length is larger than array of char length'#000+
  'E_Illegal expression after message directive'#000+
  'E_Message handlers can take only one call by ref. parameter'#000+
  'E','_Duplicate message label: $1'#000+
  'E_Self can be only an explicit parameter in message handlers'#000+
  'E_Threadvars can be only static or global'#000+
  'F_Direct assembler not supported for binary output format'#000+
  'W_Don'#039't load OBJPAS unit manual, use {$mode objfpc','} or {$mode de'+
  'lphi} instead'#000+
  'E_OVERRIDE can'#039't be used in objects'#000+
  'E_Data types which requires initialization/finalization can'#039't be u'+
  'sed in variant records'#000+
  'E_Resourcestrings can be only static or global'#000+
  'E_Exit with argument can'#039't be used here'#000,
  'E_The type of the storage symbol must be boolean'#000+
  'E_This symbol isn'#039't allowed as storage symbol'#000+
  'E_Only class which are compiled in $M+ mode can be published'#000+
  'E_Procedure directive expected'#000+
  'E_The value for a property index must be of an ordina','l type'#000+
  'E_Procedure name to short to be exported'#000+
  'E_No DEFFILE entry can be generated for unit global vars'#000+
  'E_Compile without -WD option'#000+
  'E_Type mismatch'#000+
  'E_Incompatible types: got "$1" expected "$2"'#000+
  'E_Type mismatch between $1 and $2'#000+
  'E_Type iden','tifier expected'#000+
  'E_Variable identifier expected'#000+
  'E_Integer expression expected, but got "$1"'#000+
  'E_Boolean expression expected, but got "$1"'#000+
  'E_Ordinal expression expected'#000+
  'E_pointer type expected, but got "$1"'#000+
  'E_class type expected, but got "$1"'#000+
  'E','_Variable or type indentifier expected'#000+
  'E_Can'#039't evaluate constant expression'#000+
  'E_Set elements are not compatible'#000+
  'E_Operation not implemented for sets'#000+
  'W_Automatic type conversion from floating type to COMP which is an int'+
  'eger type'#000+
  'H_use DIV ins','tead to get an integer result'#000+
  'E_string types doesn'#039't match, because of $V+ mode'#000+
  'E_succ or pred on enums with assignments not possible'#000+
  'E_Can'#039't read or write variables of this type'#000+
  'E_Type conflict between set elements'#000+
  'W_lo/hi(dword/qword) ret','urns the upper/lower word/dword'#000+
  'E_Integer or real expression expected'#000+
  'E_Wrong type $1 in array constructor'#000+
  'E_Incompatible type for arg no. $1: Got $2, expected $3'#000+
  'E_Method (variable) and Procedure (variable) are not compatible'#000+
  'E_Illegal con','stant passed to internal math function'#000+
  'E_Can'#039't get the address of constants'#000+
  'E_Argument can'#039't be assigned to'#000+
  'E_Can'#039't assign local procedure/function to procedure variable'#000+
  'E_Can'#039't assign values to an address'#000+
  'E_Can'#039't assign values to const var','iable'#000+
  'E_Identifier not found $1'#000+
  'F_Internal Error in SymTableStack()'#000+
  'E_Duplicate identifier $1'#000+
  'H_Identifier already defined in $1 at line $2'#000+
  'E_Unknown identifier $1'#000+
  'E_Forward declaration not solved $1'#000+
  'F_Identifier type already defined as typ','e'#000+
  'E_Error in type definition'#000+
  'E_Type identifier not defined'#000+
  'E_Forward type not resolved $1'#000+
  'E_Only static variables can be used in static methods or outside metho'+
  'ds'#000+
  'E_Invalid call to tvarsym.mangledname()'#000+
  'F_record or class type expected'#000+
  'E_Ins','tances of classes or objects with an abstract method are not al'+
  'lowed'#000+
  'W_Label not defined $1'#000+
  'E_Label used but not defined $1'#000+
  'E_Illegal label declaration'#000+
  'E_GOTO and LABEL are not supported (use switch -Sg)'#000+
  'E_Label not found'#000+
  'E_identifier isn'#039't',' a label'#000+
  'E_label already defined'#000+
  'E_illegal type declaration of set elements'#000+
  'E_Forward class definition not resolved $1'#000+
  'H_Parameter $1 not used'#000+
  'N_Local variable $1 not used'#000+
  'H_Value parameter $1 is assigned but never used'#000+
  'N_Local variable $1 ','is assigned but never used'#000+
  'H_Local $1 $2 is not used'#000+
  'N_Private field $1.$2 is never used'#000+
  'N_Private field $1.$2 is assigned but never used'#000+
  'N_Private method $1.$2 never used'#000+
  'E_Set type expected'#000+
  'W_Function result does not seem to be set'#000+
  'E_Unkn','own record field identifier $1'#000+
  'W_Local variable $1 does not seem to be initialized'#000+
  'W_Variable $1 does not seem to be initialized'#000+
  'E_identifier idents no member $1'#000+
  'B_Found declaration: $1'#000+
  'E_Data segment too large (max. 2GB)'#000+
  'E_BREAK not allowe','d'#000+
  'E_CONTINUE not allowed'#000+
  'E_Expression too complicated - FPU stack overflow'#000+
  'E_Illegal expression'#000+
  'E_Invalid integer expression'#000+
  'E_Illegal qualifier'#000+
  'E_High range limit < low range limit'#000+
  'E_Illegal counter variable'#000+
  'E_Can'#039't determine which overloa','ded function to call'#000+
  'E_Parameter list size exceeds 65535 bytes'#000+
  'E_Illegal type conversion'#000+
  'D_Conversion between ordinals and pointers is not portable across plat'+
  'forms'#000+
  'E_File types must be var parameters'#000+
  'E_The use of a far pointer isn'#039't allowe','d there'#000+
  'E_illegal call by reference parameters'#000+
  'E_EXPORT declared functions can'#039't be called'#000+
  'W_Possible illegal call of constructor or destructor (doesn'#039't matc'+
  'h to this context)'#000+
  'N_Inefficient code'#000+
  'W_unreachable code'#000+
  'E_procedure call with stac','kframe ESP/SP'#000+
  'E_Abstract methods can'#039't be called directly'#000+
  'F_Internal Error in getfloatreg(), allocation failure'#000+
  'F_Unknown float type'#000+
  'F_SecondVecn() base defined twice'#000+
  'F_Extended cg68k not supported'#000+
  'F_32-bit unsigned not supported in MC68000',' mode'#000+
  'F_Internal Error in secondinline()'#000+
  'D_Register $1 weight $2 $3'#000+
  'E_Stack limit excedeed in local routine'#000+
  'D_Stack frame is omitted'#000+
  'W_Range check for 64 bit integers is not supported on this target'#000+
  'E_Object or class methods can'#039't be inline','.'#000+
  'E_Procvar calls can'#039't be inline.'#000+
  'E_No code for inline procedure stored'#000+
  'E_Direct call of interrupt procedure $1 is not possible'#000+
  'E_Element zero of an ansi/wide- or longstring can'#039't be accessed, u'+
  'se (set)length instead'#000+
  'E_Include and exclude ','not implemented in this case'#000+
  'E_Constructors or destructors can not be called inside a '#039'with'#039+
  ' clause'#000+
  'E_Cannot call message handler method directly'#000+
  'E_Jump in or outside of an exception block'#000+
  'D_Starting $1 styled assembler parsing'#000+
  'D_Finished $','1 styled assembler parsing'#000+
  'E_Non-label pattern contains @'#000+
  'W_Override operator not supported'#000+
  'E_Error building record offset'#000+
  'E_OFFSET used without identifier'#000+
  'E_TYPE used without identifier'#000+
  'E_Cannot use local variable or parameters here'#000+
  'E_need',' to use OFFSET here'#000+
  'E_Cannot use multiple relocatable symbols'#000+
  'E_Relocatable symbol can only be added'#000+
  'E_Invalid constant expression'#000+
  'E_Relocatable symbol is not allowed'#000+
  'E_Invalid reference syntax'#000+
  'E_Local symbols/labels aren'#039't allowed as refer','ences'#000+
  'E_Invalid base and index register usage'#000+
  'E_Wrong scale factor specified'#000+
  'E_Multiple index register usage'#000+
  'E_Invalid operand type'#000+
  'E_Invalid string as opcode operand: $1'#000+
  'W_@CODE and @DATA not supported'#000+
  'E_Null label references are not allow','ed'#000+
  'F_Divide by zero in asm evaluator'#000+
  'F_Evaluator stack overflow'#000+
  'F_Evaluator stack underflow'#000+
  'F_Invalid numeric format in asm evaluator'#000+
  'F_Invalid Operator in asm evaluator'#000+
  'E_escape sequence ignored: $1'#000+
  'E_Invalid symbol reference'#000+
  'W_Fwait can c','ause emulation problems with emu387'#000+
  'W_Calling an overload function in assembler'#000+
  'E_Unsupported symbol type for operand'#000+
  'E_Constant value out of bounds'#000+
  'E_Error converting decimal $1'#000+
  'E_Error converting octal $1'#000+
  'E_Error converting binary $1'#000+
  'E_Er','ror converting hexadecimal $1'#000+
  'H_$1 translated to $2'#000+
  'W_$1 is associated to an overloaded function'#000+
  'E_Cannot use SELF outside a method'#000+
  'E_Cannot use OLDEBP outside a nested procedure'#000+
  'W_Procedures can'#039't return any value in asm code'#000+
  'E_SEG not sup','ported'#000+
  'E_Size suffix and destination or source size do not match'#000+
  'W_Size suffix and destination or source size do not match'#000+
  'E_Assembler syntax error'#000+
  'E_Invalid combination of opcode and operands'#000+
  'E_Assemler syntax error in operand'#000+
  'E_Assemler s','yntax error in constant'#000+
  'E_Invalid String expression'#000+
  '32bit constant created for address'#000+
  'E_Invalid or missing opcode'#000+
  'E_Invalid combination of prefix and opcode: $1'#000+
  'E_Invalid combination of override and opcode: $1'#000+
  'E_Too many operands on line'#000+
  'W','_NEAR ignored'#000+
  'W_FAR ignored'#000+
  'E_Duplicate local symbol $1'#000+
  'E_Undefined local symbol $1'#000+
  'E_Unknown label identifier $1'#000+
  'E_Invalid floating point register name'#000+
  'E_NOR not supported'#000+
  'W_Modulo not supported'#000+
  'E_Invalid floating point constant $1'#000+
  'E_Inval','id floating point expression'#000+
  'E_Wrong symbol type'#000+
  'E_Cannot index a local var or parameter with a register'#000+
  'E_Invalid segment override expression'#000+
  'W_Identifier $1 supposed external'#000+
  'E_Strings not allowed as constants'#000+
  'No type of variable specifie','d'#000+
  'E_assembler code not returned to text section'#000+
  'E_Not a directive or local symbol $1'#000+
  'E_Using a defined name as a local label'#000+
  'E_Dollar token is used without an identifier'#000+
  'W_32bit constant created for address'#000+
  'N_.align is target specific, use ','.balign or .p2align'#000+
  'E_Can'#039't access fields directly for parameters'#000+
  'E_Can'#039't access fields of objects/classes directly'#000+
  'F_Too many assembler files'#000+
  'F_Selected assembler output not supported'#000+
  'F_Comp not supported'#000+
  'F_Direct not support for binary wr','iters'#000+
  'E_Allocating of data is only allowed in bss section'#000+
  'F_No binary writer selected'#000+
  'E_Asm: Opcode $1 not in table'#000+
  'E_Asm: $1 invalid combination of opcode and operands'#000+
  'E_Asm: 16 Bit references not supported'#000+
  'E_Asm: Invalid effective address',#000+
  'E_Asm: Immediate or reference expected'#000+
  'E_Asm: $1 value exceeds bounds $2'#000+
  'E_Asm: Short jump is out of range $1'#000+
  'E_Asm: Undefined label $1'#000+
  'W_Source operating system redefined'#000+
  'I_Assembling (pipe) $1'#000+
  'E_Can'#039't create assember file $1'#000+
  'W_Assembler ','$1 not found, switching to external assembling'#000+
  'T_Using assembler: $1'#000+
  'W_Error while assembling exitcode $1'#000+
  'W_Can'#039't call the assembler, error $1 switching to external assembl'+
  'ing'#000+
  'I_Assembling $1'#000+
  'I_Assembling smartlink $1'#000+
  'W_Object $1 not found,',' Linking may fail !'#000+
  'W_Library $1 not found, Linking may fail !'#000+
  'W_Error while linking'#000+
  'W_Can'#039't call the linker, switching to external linking'#000+
  'I_Linking $1'#000+
  'W_Util $1 not found, switching to external linking'#000+
  'T_Using util $1'#000+
  'E_Creation of Execut','ables not supported'#000+
  'E_Creation of Dynamic/Shared Libraries not supported'#000+
  'I_Closing script $1'#000+
  'W_resource compiler not found, switching to external mode'#000+
  'I_Compiling resource $1'#000+
  'F_Can'#039't post process executable $1'#000+
  'F_Can'#039't open executable $1'#000+
  'X_S','ize of Code: $1 bytes'#000+
  'X_Size of initialized data: $1 bytes'#000+
  'X_Size of uninitialized data: $1 bytes'#000+
  'X_Stack space reserved: $1 bytes'#000+
  'X_Stack space commited: $1 bytes'#000+
  'T_Unitsearch: $1'#000+
  'T_PPU Loading $1'#000+
  'U_PPU Name: $1'#000+
  'U_PPU Flags: $1'#000+
  'U_PPU Crc: ','$1'#000+
  'U_PPU Time: $1'#000+
  'U_PPU File too short'#000+
  'U_PPU Invalid Header (no PPU at the begin)'#000+
  'U_PPU Invalid Version $1'#000+
  'U_PPU is compiled for an other processor'#000+
  'U_PPU is compiled for an other target'#000+
  'U_PPU Source: $1'#000+
  'U_Writing $1'#000+
  'F_Can'#039't Write PPU-File'#000+
  'F','_Error reading PPU-File'#000+
  'F_unexpected end of PPU-File'#000+
  'F_Invalid PPU-File entry: $1'#000+
  'F_PPU Dbx count problem'#000+
  'E_Illegal unit name: $1'#000+
  'F_Too much units'#000+
  'F_Circular unit reference between $1 and $2'#000+
  'F_Can'#039't compile unit $1, no sources available'#000+
  'F_C','an'#039't find unit $1'#000+
  'W_Unit $1 was not found but $2 exists'#000+
  'F_Unit $1 searched but $2 found'#000+
  'W_Compiling the system unit requires the -Us switch'#000+
  'F_There were $1 errors compiling module, stopping'#000+
  'U_Load from $1 ($2) unit $3'#000+
  'U_Recompiling $1, chec','ksum changed for $2'#000+
  'U_Recompiling $1, source found only'#000+
  'U_Recompiling unit, static lib is older than ppufile'#000+
  'U_Recompiling unit, shared lib is older than ppufile'#000+
  'U_Recompiling unit, obj and asm are older than ppufile'#000+
  'U_Recompiling unit, obj',' is older than asm'#000+
  'U_Parsing interface of $1'#000+
  'U_Parsing implementation of $1'#000+
  'U_Second load for unit $1'#000+
  'U_PPU Check file $1 time $2'#000+
  '$1 [options] <inputfile> [options]'#000+
  'W_Only one source file supported'#000+
  'W_DEF file can be created only for OS/2'#000+
  'E_','nested response files are not supported'#000+
  'F_No source file name in command line'#000+
  'N_No option inside $1 config file'#000+
  'E_Illegal parameter: $1'#000+
  'H_-? writes help pages'#000+
  'F_Too many config files nested'#000+
  'F_Unable to open file $1'#000+
  'N_Reading further options',' from $1'#000+
  'W_Target is already set to: $1'#000+
  'W_Shared libs not supported on DOS platform, reverting to static'#000+
  'F_too many IF(N)DEFs'#000+
  'F_too many ENDIFs'#000+
  'F_open conditional at the end of the file'#000+
  'W_Debug information generation is not supported by thi','s executable'#000+
  'H_Try recompiling with -dGDB'#000+
  'E_You are using the obsolete switch $1'#000+
  'E_You are using the obsolete switch $1, please use $2'#000+
  'N_Switching assembler to default source writing assembler'#000+
  'Free Pascal Compiler version $FPCVER [$FPCDATE]',' for $FPCTARGET'#000+
  'Copyright (c) 1993-2000 by Florian Klaempfl'#000+
  'Free Pascal Compiler version $FPCVER'#000+
  #000+
  'Compiler Date  : $FPCDATE'#000+
  'Compiler Target: $FPCTARGET'#000+
  #000+
  'This program comes under the GNU General Public Licence'#000+
  'For more information read COPYI','NG.FPC'#000+
  #000+
  'Report bugs,suggestions etc to:'#000+
  '                 bugrep@freepascal.org'#000+
  '**0*_put + after a boolean switch option to enable it, - to disable it'+
  #000+
  '**1a_the compiler doesn'#039't delete the generated assembler file'#000+
  '**2al_list sourcecode lines ','in assembler file'#000+
  '**2ar_list register allocation/release info in assembler file'#000+
  '**2at_list temp allocation/release info in assembler file'#000+
  '**1b_generate browser info'#000+
  '**2bl_generate local symbol info'#000+
  '**1B_build all modules'#000+
  '**1C<x>_code genera','tion options:'#000+
  '**2CD_create also dynamic library (not supported)'#000+
  '**2Ch<n>_<n> bytes heap (between 1023 and 67107840)'#000+
  '**2Ci_IO-checking'#000+
  '**2Cn_omit linking stage'#000+
  '**2Co_check overflow of integer operations'#000+
  '**2Cr_range checking'#000+
  '**2Cs<n>_set stac','k size to <n>'#000+
  '**2Ct_stack checking'#000+
  '**2CX_create also smartlinked library'#000+
  '**1d<x>_defines the symbol <x>'#000+
  '*O1D_generate a DEF file'#000+
  '*O2Dd<x>_set description to <x>'#000+
  '*O2Dw_PM application'#000+
  '**1e<x>_set path to executable'#000+
  '**1E_same as -Cn'#000+
  '**1F<x>_se','t file names and paths:'#000+
  '**2FD<x>_sets the directory where to search for compiler utilities'#000+
  '**2Fe<x>_redirect error output to <x>'#000+
  '**2FE<x>_set exe/unit output path to <x>'#000+
  '**2Fi<x>_adds <x> to include path'#000+
  '**2Fl<x>_adds <x> to library path'#000+
  '*L','2FL<x>_uses <x> as dynamic linker'#000+
  '**2Fo<x>_adds <x> to object path'#000+
  '**2Fr<x>_load error message file <x>'#000+
  '**2Fu<x>_adds <x> to unit path'#000+
  '**2FU<x>_set unit output path to <x>, overrides -FE'#000+
  '*g1g_generate debugger information:'#000+
  '*g2gg_use gsym'#000+
  '*g','2gd_use dbx'#000+
  '*g2gh_use heap trace unit (for memory leak debugging)'#000+
  '*g2gc_generate checks for pointers'#000+
  '**1i_information'#000+
  '**2iD_return compiler date'#000+
  '**2iV_return compiler version'#000+
  '**2iSO_return compiler OS'#000+
  '**2iSP_return compiler processor'#000+
  '**2iTO','_return target OS'#000+
  '**2iTP_return target processor'#000+
  '**1I<x>_adds <x> to include path'#000+
  '**1k<x>_Pass <x> to the linker'#000+
  '**1l_write logo'#000+
  '**1n_don'#039't read the default config file'#000+
  '**1o<x>_change the name of the executable produced to <x>'#000+
  '**1pg_generat','e profile code for gprof (defines FPC_PROFILE)'#000+
  '*L1P_use pipes instead of creating temporary assembler files'#000+
  '**1S<x>_syntax options:'#000+
  '**2S2_switch some Delphi 2 extensions on'#000+
  '**2Sc_supports operators like C (*=,+=,/= and -=)'#000+
  '**2Sd_tries to be',' Delphi compatible'#000+
  '**2Se<x>_compiler stops after the <x> errors (default is 1)'#000+
  '**2Sg_allow LABEL and GOTO'#000+
  '**2Sh_Use ansistrings'#000+
  '**2Si_support C++ styled INLINE'#000+
  '**2Sm_support macros like C (global)'#000+
  '**2So_tries to be TP/BP 7.0 compatible'#000+
  '**2S','p_tries to be gpc compatible'#000+
  '**2Ss_constructor name must be init (destructor must be done)'#000+
  '**2St_allow static keyword in objects'#000+
  '**1s_don'#039't call assembler and linker (only with -a)'#000+
  '**1u<x>_undefines the symbol <x>'#000+
  '**1U_unit options:'#000+
  '**2Un_d','on'#039't check the unit name'#000+
  '**2Us_compile a system unit'#000+
  '**1v<x>_Be verbose. <x> is a combination of the following letters:'#000+
  '**2*_e : Show errors (default)       d : Show debug info'#000+
  '**2*_w : Show warnings               u : Show unit info'#000+
  '**2*_n ',': Show notes                  t : Show tried/used files'#000+
  '**2*_h : Show hints                  m : Show defined macros'#000+
  '**2*_i : Show general info           p : Show compiled procedures'#000+
  '**2*_l : Show linenumbers            c : Show conditional','s'#000+
  '**2*_a : Show everything             0 : Show nothing (except errors)'#000+
  '**2*_b : Show all procedure          r : Rhide/GCC compatibility mode'#000+
  '**2*_    declarations if an error    x : Executable info (Win32 only)'#000+
  '**2*_    occurs'#000+
  '**1X_executa','ble options:'#000+
  '*L2Xc_link with the c library'#000+
  '**2Xs_strip all symbols from executable'#000+
  '**2XD_try to link dynamic          (defines FPC_LINK_DYNAMIC)'#000+
  '**2XS_try to link static (default) (defines FPC_LINK_STATIC)'#000+
  '**2XX_try to link smart           ',' (defines FPC_LINK_SMART)'#000+
  '**0*_Processor specific options:'#000+
  '3*1A<x>_output format:'#000+
  '3*2Aas_assemble using GNU AS'#000+
  '3*2Aasaout_assemble using GNU AS for aout (Go32v1)'#000+
  '3*2Anasmcoff_coff (Go32v2) file using Nasm'#000+
  '3*2Anasmelf_elf32 (Linux) file usin','g Nasm'#000+
  '3*2Anasmobj_obj file using Nasm'#000+
  '3*2Amasm_obj file using Masm (Microsoft)'#000+
  '3*2Atasm_obj file using Tasm (Borland)'#000+
  '3*2Acoff_coff (Go32v2) using internal writer'#000+
  '3*2Apecoff_pecoff (Win32) using internal writer'#000+
  '3*1R<x>_assembler reading st','yle:'#000+
  '3*2Ratt_read AT&T style assembler'#000+
  '3*2Rintel_read Intel style assembler'#000+
  '3*2Rdirect_copy assembler text directly to assembler file'#000+
  '3*1O<x>_optimizations:'#000+
  '3*2Og_generate smaller code'#000+
  '3*2OG_generate faster code (default)'#000+
  '3*2Or_keep certain',' variables in registers'#000+
  '3*2Ou_enable uncertain optimizations (see docs)'#000+
  '3*2O1_level 1 optimizations (quick optimizations)'#000+
  '3*2O2_level 2 optimizations (-O1 + slower optimizations)'#000+
  '3*2O3_level 3 optimizations (same as -O2u)'#000+
  '3*2Op<x>_target pr','ocessor:'#000+
  '3*3Op1_set target processor to 386/486'#000+
  '3*3Op2_set target processor to Pentium/PentiumMMX (tm)'#000+
  '3*3Op3_set target processor to PPro/PII/c6x86/K6 (tm)'#000+
  '3*1T<x>_Target operating system:'#000+
  '3*2TGO32V1_version 1 of DJ Delorie DOS extender'#000+
  '3*','2TGO32V2_version 2 of DJ Delorie DOS extender'#000+
  '3*2TLINUX_Linux'#000+
  '3*2TOS2_OS/2 2.x'#000+
  '3*2TWin32_Windows 32 Bit'#000+
  '3*1W<x>_Win32 target options'#000+
  '3*2WB<x>_Set Image base to Hexadecimal <x> value'#000+
  '3*2WC_Specify console type application'#000+
  '3*2WD_Use DEFFILE t','o export functions of DLL or EXE'#000+
  '3*2WG_Specify graphic type application'#000+
  '3*2WN_Do not generate relocation code (necessary for debugging)'#000+
  '3*2WR_Generate relocation code'#000+
  '6*1A<x>_output format'#000+
  '6*2Aas_Unix o-file using GNU AS'#000+
  '6*2Agas_GNU Motorol','a assembler'#000+
  '6*2Amit_MIT Syntax (old GAS)'#000+
  '6*2Amot_Standard Motorola assembler'#000+
  '6*1O_optimizations:'#000+
  '6*2Oa_turn on the optimizer'#000+
  '6*2Og_generate smaller code'#000+
  '6*2OG_generate faster code (default)'#000+
  '6*2Ox_optimize maximum (still BUGGY!!!)'#000+
  '6*2O2_set ','target processor to a MC68020+'#000+
  '6*1R<x>_assembler reading style:'#000+
  '6*2RMOT_read motorola style assembler'#000+
  '6*1T<x>_Target operating system:'#000+
  '6*2TAMIGA_Commodore Amiga'#000+
  '6*2TATARI_Atari ST/STe/TT'#000+
  '6*2TMACOS_Macintosh m68k'#000+
  '6*2TLINUX_Linux-68k'#000+
  '**1*_'#000+
  '**','1?_shows this help'#000+
  '**1h_shows this help without waiting'#000
);
