const msgtxt : array[0..00058,1..255] of char=(
  'U_Compiler: $1'#000+
  'D_Source OS: $1'#000+
  'I_Target OS: $1'#000+
  'U_Using executable path: $1'#000+
  'U_Using unit path: $1'#000+
  'U_Using include path: $1'#000+
  'U_Using library path: $1'#000+
  'U_Using object path: $1'#000+
  'I_$1 Lines compiled, $2 sec'#000+
  'F_No memory left'#000+
  'F_Unexpected end of file'#000+
  'F_String excee','ds line'#000+
  'F_illegal character'#000+
  'F_Syntax error $1 expected'#000+
  'U_Start reading includefile $1'#000+
  'W_Comment level $1 found'#000+
  'N_$F directive (FAR) ignored'#000+
  'N_Stack check is global under linux'#000+
  'W_Illegal compiler switch'#000+
  'W_This compiler switch has a global effect'#000+
  'E_Illegal ','char constant'#000+
  'F_Can'#039't open file $1'#000+
  'F_Can'#039't open include file $1'#000+
  'E_Too many $ENDIFs or $ELSEs'#000+
  'W_Records fields can be aligned to 1,2,4 or 16 bytes only'#000+
  'E_$1 expected for $2 defined in line $3'#000+
  'E_Syntax error while parsing a conditional compiling expression'#000,+
  'E_Evaluating a conditional compiling expression'#000+
  'W_Macro contents is cut after char 255 to evalute expression'#000+
  'E_ENDIF without IF(N)DEF'#000+
  'F_User defined: $1'#000+
  'E_User defined: $1'#000+
  'W_User defined: $1'#000+
  'N_User defined: $1'#000+
  'H_User defined: $1'#000+
  'I_User defined: $1'#000+
  'E_Keywo','rd redefined as macro has no effect'#000+
  'F_Macro buffer overflow while reading or expanding a macro'#000+
  'W_Extension of macros exceeds a deep of 16, perhaps there is a recursi'+
  've macro definition (crashes the compiler)'#000+
  'E_compiler switches aren'#039't allowed in (* ... *)',' styled comments'#000+
  'D_Handling switch "$1"'#000+
  'C_ENDIF $1 found'#000+
  'C_IFDEF $1 found, $2'#000+
  'C_IFOPT $1 found, $2'#000+
  'C_IF $1 found, $2'#000+
  'C_IFNDEF $1 found, $2'#000+
  'C_ELSE $1 found, $2'#000+
  'C_Skipping until...'#000+
  'I_Press <return> to continue'#000+
  'W_Unsupported switch $1'#000+
  'W_Illegal compiler dire','ctive $1'#000+
  'D_Back in $1'#000+
  'W_Unsupported assembler style specified $1'#000+
  'E_Parser - Syntax Error'#000+
  'W_Procedure type FAR ignored'#000+
  'W_Procedure type NEAR ignored'#000+
  'E_No DLL File specified'#000+
  'E_Constructor name must be INIT'#000+
  'E_Destructor name must be DONE'#000+
  'E_Illegal open param','eter'#000+
  'E_Procedure type INLINE not supported'#000+
  'W_Private methods shouldn'#039't be VIRTUAL'#000+
  'W_Constructor should be public'#000+
  'W_Destructor should be public'#000+
  'N_Class should have one destructor only'#000+
  'E_Local class definitions are not allowed'#000+
  'E_Anonym class definitions are',' not allowed'#000+
  'E_Ordinal value expected'#000+
  'E_Illegal parameter list'#000+
  'E_overloaded identifier isn'#039't a function identifier'#000+
  'E_overloaded functions have the same parameter list'#000+
  'E_function header doesn'#039't match the forward declaration $1'#000+
  'N_only values can be jumped o','ver in enumeration types'#000+
  'N_Interface and implementation names are different !'#000+
  'E_function nesting > 31'#000+
  'E_range check error while evaluating constants'#000+
  'E_pointer type expected'#000+
  'E_class type expected'#000+
  'E_duplicate case label'#000+
  'E_typed constants of classes are not ','allowed'#000+
  'E_functions variables of overloaded functions are not allowed'#000+
  'E_string length must be a value from 1 to 255'#000+
  'W_use extended syntax of DISPOSE and NEW to generate instances of obje'+
  'cts'#000+
  'E_class identifier expected'#000+
  'E_method identifier expected'#000+
  'E_functi','on header doesn'#039't match any method of this class'#000+
  'P_procedure/function $1'#000+
  'E_Illegal floating point constant'#000+
  'E_FAIL can be used in constructors only'#000+
  'E_Destructors can'#039't have parameters'#000+
  'E_string types doesn'#039't match, because of $V+ mode'#000+
  'E_Only class methods c','an be referred with class references'#000+
  'E_Only class methods can be accessed in class methods'#000+
  'E_Constant and CASE types do not match'#000+
  'E_The symbol can'#039't be exported from a library'#000+
  'W_A inherited method is hidden by $1'#000+
  'E_There is no method in an ancestor class ','to be overridden: $1'#000+
  'E_No member is provided to access property'#000+
  'E_Illegal symbol for property access'#000+
  'E_Cannot access a protected field of an object here'#000+
  'E_Cannot access a private field of an object here'#000+
  'E_all overloaded methods must be virtual if one is v','irtual: $1'#000+
  'E_overloaded methods which are virtual must have the same return type:'+
  ' $1'#000+
  'E_EXPORT declared functions can'#039't be nested'#000+
  'E_methods can'#039't be EXPORTed'#000+
  'E_call by var parameters have to match exactly'#000+
  'E_Class isn'#039't a super class of the current class'#000+
  'E_','SELF is only allowed in methods'#000+
  'E_methods can be only in other methods called direct with type identif'+
  'ier of the class'#000+
  'E_Illegal use of '#039':'#039#000+
  'E_range check error in set constructor or duplicate set element'#000+
  'E_Pointer to class expected'#000+
  'E_Expression must be co','nstructor call'#000+
  'E_Expression must be destructor call'#000+
  'E_Type conflict between set elements'#000+
  'W_Using / will give a floating point result, use DIV instead to get an'+
  ' integer result'#000+
  'E_Illegal order of record elements'#000+
  'E_Expression type must by class or record typ','e'#000+
  'E_Functions with void return value can'#039't return any value'#000+
  'E_constructors and destructors must be methods'#000+
  'W_Automatic type conversion from floating type to COMP which is an int'+
  'eger type'#000+
  'E_Operator is not overloaded'#000+
  'E_Variable or type indentifier expected',#000+
  'E_Re-raise isn'#039't possible there'#000+
  'E_The extended syntax of new or dispose isn'#039't allowed for a class'#000+
  'E_Assembler incompatible with function return value'#000+
  'E_Procedure overloading is switched off'#000+
  'E_It is not possible to overload this operator (overload = inste','ad)'#000+
  'E_Comparative operator must return a boolean value'#000+
  'E_Only virtual methods can be abstract'#000+
  'F_Use of unsupported feature!'#000+
  'E_The mix of CLASSES and OBJECTS isn'#039't allowed'#000+
  'W_Unknown procedure directive had to be ignored: $1'#000+
  'E_absolute can only be associate','d to ONE variable'#000+
  'E_absolute can only be associated a var or const'#000+
  'E_Abtract methods shouldn'#039't have any definition (with function bod'+
  'y)'#000+
  'E_This overloaded function can'#039't be local (must be exported)'#000+
  'E_succ or pred on enums with assignments not possible'#000+
  'W_Vi','rtual methods are used without a constructor in $1'#000+
  'M_Macro defined: $1'#000+
  'M_Macro undefined: $1'#000+
  'M_Macro $1 set to $2'#000+
  'I_Compiling $1'#000+
  'D_Compiling $1 for the second time'#000+
  'E_Array properties aren'#039't allowed at this point'#000+
  'E_No property found to override'#000+
  'E_Only one ','default property is allowed, found inherited default prop'+
  'erty in class %1'#000+
  'E_The default property must be an array property'#000+
  'E_Virtual constructors are only supported in class object model'#000+
  'E_No default property available'#000+
  'E_The class can'#039't have a published s','ection, use the {$M+} switch'+
  #000+
  'E_Forward declaration of class $1 must be resolved here to use the cla'+
  'ss as anchestor'#000+
  'E_Local operators not supported'#000+
  'E_Procedure directive $1 not allowed in interface section'#000+
  'E_Procedure directive $1 not allowed in implementa','tion section'#000+
  'E_Procedure directive $1 not allowed in procvar declaration'#000+
  'E_Function is already declared Public/Forward $1'#000+
  'E_Can'#039't use both EXPORT and EXTERNAL'#000+
  'E_NAME keyword expected'#000+
  'W_$1 not yet supported inside inline procedure/function'#000+
  'W_Inlining disab','led'#000+
  'I_Writing Browser log $1'#000+
  'H_may be pointer deref ^ is missing'#000+
  'F_Selected assembler reader not supported'#000+
  'W_Local variable $1 doesn'#039't seem to be initialized yet !'#000+
  'E_Identifier not found $1'#000+
  'F_Internal Error in SymTableStack()'#000+
  'E_Duplicate identifier $1'#000+
  'E_U','nknown identifier $1'#000+
  'E_Forward declaration not solved: $1'#000+
  'F_Identifier type already defined as type'#000+
  'E_Type identifier expected'#000+
  'E_Type mismatch'#000+
  'E_Error in type defenition'#000+
  'E_Type identifier not defined'#000+
  'E_Only static variables can be used in static methods o','r outside met'+
  'hods'#000+
  'E_Invalid call to tvarsym.mangledname()'#000+
  'F_record or class type expected'#000+
  'E_To generate an instance of a class or an object with an abtract meth'+
  'od isn'#039't allowed'#000+
  'E_Label not defined $1'#000+
  'E_Illegal label declaration'#000+
  'E_GOTO und LABEL are not su','pported (use command line switch -Sg)'#000+
  'E_Label not found'#000+
  'E_identifier isn'#039't a label'#000+
  'E_label already defined'#000+
  'E_illegal type declaration of set elements'#000+
  'E_Forward class definition not resolved $1'#000+
  'H_Parameter not used $1'#000+
  'W_Local variable not used $1'#000+
  'E_Set ele','ments are not compatible'#000+
  'E_Set type expected'#000+
  'W_Function result does not seem to be set'#000+
  'E_Unknown field identifier'#000+
  'W_Local variable $1 does not seem to be initialized'#000+
  'E_identifier idents no member $1'#000+
  'E_BREAK not allowed'#000+
  'E_CONTINUE not allowed'#000+
  'E_Expression ','too complicated - FPU stack overflow'#000+
  'E_Illegal expression'#000+
  'E_Invalid integer'#000+
  'E_Illegal qualifier'#000+
  'E_High range limit < low range limit'#000+
  'E_Illegal counter variable'#000+
  'E_Can'#039't determine which overloaded function to call'#000+
  'E_Parameter list size exceeds 65535 bytes'#000+
  'E','_Illegal type conversion'#000+
  'E_File types must be var parameters'#000+
  'E_The use of a far pointer isn'#039't allowed there'#000+
  'E_illegal call by reference parameters'#000+
  'E_EXPORT declared functions can'#039't be called'#000+
  'W_Possible illegal call of constructor or destructor (doesn'#039't ma','t'+
  'ch to this context)'#000+
  'N_Inefficient code'#000+
  'W_unreachable code'#000+
  'E_procedure call with stackframe ESP/SP'#000+
  'E_Abstract methods can'#039't be called directly'#000+
  'F_Internal Error in getfloatreg(), allocation failure'#000+
  'F_Unknown float type'#000+
  'F_SecondVecn() base defined twice'#000+
  'F_E','xtended cg68k not supported'#000+
  'F_32-bit unsigned not supported in MC68000 mode'#000+
  'F_Internal Error in secondinline()'#000+
  'D_Register $1 weight $2 $3'#000+
  'E_Stack limit excedeed in local routine'#000+
  'D_Stack frame is omited'#000+
  'E_Unable to inline object methods'#000+
  'E_Unable to inline ','procvar calls'#000+
  'E_No code for inline procedure stored'#000+
  'F_Divide by zero in asm evaluator'#000+
  'F_Evaluator stack overflow'#000+
  'F_Evaluator stack underflow'#000+
  'F_Invalid numeric format in asm evaluator'#000+
  'F_Invalid Operator in asm evaluator'#000+
  'F_Unknown error in asm evaluator'#000+
  'W_I','nvalid numeric value'#000+
  'E_escape sequence ignored: $1'#000+
  'E_Asm syntax error - Prefix not found'#000+
  'E_Asm syntax error - Trying to add more than one prefix'#000+
  'E_Asm syntax error - Opcode not found'#000+
  'E_Invalid symbol reference'#000+
  'W_Calling an overload function in an asm'#000+
  'E_Co','nstant value out of bounds'#000+
  'E_Non-label pattern contains @'#000+
  'E_Invalid Operand: $1'#000+
  'W_Override operator not supported'#000+
  'E_Error in binary constant: $1'#000+
  'E_Error in octal constant: $1'#000+
  'E_Error in hexadecimal constant: $1'#000+
  'E_Error in integer constant: $1'#000+
  'E_Invalid la','beled opcode'#000+
  'F_Internal error in Findtype()'#000+
  'E_Invalid size for MOVSX/MOVZX'#000+
  'E_16-bit base in 32-bit segment'#000+
  'E_16-bit index in 32-bit segment'#000+
  'E_Invalid Opcode'#000+
  'E_Constant reference not allowed'#000+
  'W_Fwait can cause emulation problems with emu387'#000+
  'E_Invalid combin','ation of opcode and operands'#000+
  'W_Opcode $1 not in table, operands not checked'#000+
  'F_Internal Error in ConcatOpcode()'#000+
  'E_Invalid size in reference'#000+
  'E_Invalid middle sized operand'#000+
  'E_Invalid three operand opcode'#000+
  'E_Assembler syntax error'#000+
  'E_Invalid operand type'#000+
  'E_Segm','ent overrides not supported'#000+
  'E_Invalid constant symbol $1'#000+
  'F_Internal Errror converting binary'#000+
  'F_Internal Errror converting hexadecimal'#000+
  'F_Internal Errror converting octal'#000+
  'E_Invalid constant expression'#000+
  'E_Unknown identifier: $1'#000+
  'E_Trying to define an index reg','ister more than once'#000+
  'E_Invalid field specifier'#000+
  'F_Internal Error in BuildScaling()'#000+
  'E_Invalid scaling factor'#000+
  'E_Invalid scaling value'#000+
  'E_Scaling value only allowed with index'#000+
  'E_Invalid assembler syntax. No ref with brackets)'#000+
  'E_Expressions of the form [sreg:re','g...] are currently not supported'#000+
  'E_Trying to define a segment register twice'#000+
  'E_Trying to define a base register twice'#000+
  'E_Trying to use a negative index register'#000+
  'E_Asm syntax error - error in reference'#000+
  'E_Local symbols not allowed as references'#000+
  'E_Invalid op','erand in bracket expression'#000+
  'E_Invalid symbol name: $1'#000+
  'E_Invalid Reference syntax'#000+
  'E_Invalid string as opcode operand: $1'#000+
  'W_@CODE and @DATA not supported'#000+
  'E_Null label references are not allowed'#000+
  'E_Cannot use SELF outside a method'#000+
  'E_Asm syntax error - Should ','start with bracket'#000+
  'E_Asm syntax error - register: $1'#000+
  'E_SEG and OFFSET not supported'#000+
  'E_Asm syntax error - in opcode operand'#000+
  'E_Invalid String expression'#000+
  'E_Constant expression out of bounds'#000+
  'F_Internal Error in BuildConstant()'#000+
  'W_A repeat prefix and a segment ','override on <= i386 may result in er'+
  'rors if an interrupt occurs'#000+
  'E_Invalid or missing opcode'#000+
  'E_Invalid combination of prefix and opcode: $1'#000+
  'E_Invalid combination of override and opcode: $1'#000+
  'E_Too many operands on line'#000+
  'E_Duplicate local symbol: $1'#000+
  'E_Unknown ','label identifer: $1'#000+
  'E_Assemble node syntax error'#000+
  'E_Undefined local symbol: $1'#000+
  'D_Starting intel styled assembler parsing...'#000+
  'D_Finished intel styled assembler parsing...'#000+
  'E_Not a directive or local symbol: $1'#000+
  'E_/ at beginning of line not allowed'#000+
  'E_NOR not su','pported'#000+
  'E_Invalid floating point register name'#000+
  'W_Modulo not supported'#000+
  'E_Invalid floating point constant: $1'#000+
  'E_Size suffix and destination register do not match'#000+
  'E_Internal error in ConcatLabeledInstr()'#000+
  'W_Floating point binary representation ignored'#000+
  'W_Float','ing point hexadecimal representation ignored'#000+
  'W_Floating point octal representation ignored'#000+
  'E_Invalid real constant expression'#000+
  'E_Parenthesis are not allowed'#000+
  'E_Invalid Reference'#000+
  'E_Cannot use __SELF outside a method'#000+
  'E_Cannot use __OLDEBP outside a nested pro','cedure'#000+
  'W_Identifier $1 supposed external'#000+
  'E_Invalid segment override expression'#000+
  'E_Strings not allowed as constants'#000+
  'D_Starting AT&T styled assembler parsing...'#000+
  'D_Finished AT&T styled assembler parsing...'#000+
  'E_Switching sections is not allowed in an assembler b','lock'#000+
  'E_Invalid global definition'#000+
  'E_Line separator expected'#000+
  'W_globl not supported'#000+
  'W_align not supported'#000+
  'W_lcomm not supported'#000+
  'W_comm not supported'#000+
  'E_Invalid local common definition'#000+
  'E_Invalid global common definition'#000+
  'E_local symbol: $1 not found inside asm ','statement'#000+
  'E_assembler code not returned to text'#000+
  'F_internal error in BuildReference()'#000+
  'E_invalid opcode size'#000+
  'W_NEAR ignored'#000+
  'W_FAR ignored'#000+
  'D_Creating inline asm lookup tables'#000+
  'W_Using a defined name as a local label'#000+
  'F_internal error in HandleExtend()'#000+
  'E_Invali','d character: <'#000+
  'E_Invalid character: >'#000+
  'E_Unsupported opcode'#000+
  'E_Increment and Decrement mode not allowed together'#000+
  'E_Invalid Register list in movem/fmovem'#000+
  'E_Invalid Register list for opcode'#000+
  'E_68020+ mode required to assemble'#000+
  'D_Starting Motorola styled assembl','er parsing...'#000+
  'D_Finished Motorola styled assembler parsing...'#000+
  'W_XDEF not supported'#000+
  'W_Functions with void return value can'#039't return any value in asm c'+
  'ode'#000+
  'E_Invalid suffix for intel assembler'#000+
  'E_Extended not supported in this mode'#000+
  'E_Comp not supported in thi','s mode'#000+
  'W_You need GNU as version >= 2.81 to compile this MMX code'#000+
  'F_Too many assembler files'#000+
  'F_Selected assembler output not supported'#000+
  'E_Unsupported symbol type for operand'#000+
  'I_Assembling (pipe) $1'#000+
  'E_Can'#039't create assember file $1'#000+
  'W_Assembler $1 not found, s','witching to external assembling'#000+
  'U_Using assembler: $1'#000+
  'W_Error while assembling'#000+
  'W_Can'#039't call the assembler, switching to external assembling'#000+
  'I_Assembling $1'#000+
  'W_Linker $1 not found, switching to external linking'#000+
  'U_Using linker: $1'#000+
  'W_Object $1 not found, Link','ing may fail !'#000+
  'W_Library $1 not found, Linking may fail !'#000+
  'W_Error while linking'#000+
  'W_Can'#039't call the linker, switching to external linking'#000+
  'I_Linking $1'#000+
  'W_binder not found, switching to external binding'#000+
  'W_ar not found, switching to external ar'#000+
  'E_Dynamic Librar','ies not supported'#000+
  'I_Closing script $1'#000+
  'U_PPU Loading $1'#000+
  'D_PPU Time: $1'#000+
  'D_PPU File too short'#000+
  'D_PPU Invalid Header (no PPU at the begin)'#000+
  'D_PPU Invalid Version $1'#000+
  'D_PPU Flags: $1'#000+
  'D_PPU Crc: $1'#000+
  'T_PPU Source: $1'#000+
  'D_objectfile and assemblerfile are older than ppu','file'#000+
  'D_objectfile is older than assemblerfile'#000+
  'T_Unitsearch: $1'#000+
  'U_Writing $1'#000+
  'F_Can'#039't Write PPU-File'#000+
  'F_reading PPU-File'#000+
  'F_Invalid PPU-File entry: $1'#000+
  'F_PPU Dbx count problem'#000+
  'E_Illegal unit name: $1'#000+
  'F_Too much units'#000+
  'F_Circular unit reference'#000+
  'F_Can'#039't compile u','nit $1, no sources available'#000+
  'W_Compiling the system unit requires the -Us switch'#000+
  'F_There were $1 errors compiling module, stopping'#000
);
