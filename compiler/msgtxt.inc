{$ifdef Delphi}
const msgtxt : array[0..000198] of string[240]=(
{$else Delphi}
const msgtxt : array[0..000198,1..240] of char=(
{$endif Delphi}
  '01000_T_Compiler: $1'#000+
  '01001_D_Compiler OS: $1'#000+
  '01002_I_Target OS: $1'#000+
  '01003_T_Using executable path: $1'#000+
  '01004_T_Using unit path: $1'#000+
  '01005_T_Using include path: $1'#000+
  '01006_T_Using library path: $1'#000+
  '01007_T_Using object path: $1'#000+
  '01008_I_$1 lines co','mpiled, $2 sec $3'#000+
  '01009_F_No memory left'#000+
  '01010_I_Writing Resource String Table file: $1'#000+
  '01011_E_Writing Resource String Table file: $1'#000+
  '01012_I_Fatal:'#000+
  '01013_I_Error:'#000+
  '01014_I_Warning:'#000+
  '01015_I_Note:'#000+
  '01016_I_Hint:'#000+
  '01017_E_Path "$1" does not exi','st'#000+
  '01018_F_Compilation aborted'#000+
  '01019_bytes code'#000+
  '01020_bytes data'#000+
  '01021_I_$1 warning(s) issued'#000+
  '01022_I_$1 hint(s) issued'#000+
  '01023_I_$1 note(s) issued'#000+
  '02000_F_Unexpected end of file'#000+
  '02001_F_String exceeds line'#000+
  '02002_F_illegal character "$1" ($2)',#000+
  '02003_F_Syntax error, "$1" expected but "$2" found'#000+
  '02004_TL_Start reading includefile $1'#000+
  '02005_W_Comment level $1 found'#000+
  '02008_N_Ignored compiler switch "$1"'#000+
  '02009_W_Illegal compiler switch "$1"'#000+
  '02010_W_Misplaced global compiler switch'#000+
  '0201','1_E_Illegal char constant'#000+
  '02012_F_Can'#039't open file "$1"'#000+
  '02013_F_Can'#039't open include file "$1"'#000+
  '02015_E_Illegal record alignment specifier "$1"'#000+
  '02016_E_Illegal enum minimum-size specifier "$1"'#000+
  '02017_E_$ENDIF expected for $1 $2 defined in $3 lin','e $4'#000+
  '02018_E_Syntax error while parsing a conditional compiling expression'#000+
  '02019_E_Evaluating a conditional compiling expression'#000+
  '02020_W_Macro contents are limited to 255 characters in length'#000+
  '02021_E_ENDIF without IF(N)DEF'#000+
  '02022_F_User defi','ned: $1'#000+
  '02023_E_User defined: $1'#000+
  '02024_W_User defined: $1'#000+
  '02025_N_User defined: $1'#000+
  '02026_H_User defined: $1'#000+
  '02027_I_User defined: $1'#000+
  '02028_E_Keyword redefined as macro has no effect'#000+
  '02029_F_Macro buffer overflow while reading or expanding a',' macro'#000+
  '02030_W_Expanding of macros exceeds a depth of 16.'#000+
  '02031_W_compiler switches aren'#039't supported in // styled comments'#000+
  '02032_DL_Handling switch "$1"'#000+
  '02033_CL_ENDIF $1 found'#000+
  '02034_CL_IFDEF $1 found, $2'#000+
  '02035_CL_IFOPT $1 found, $2'#000+
  '02036_C','L_IF $1 found, $2'#000+
  '02037_CL_IFNDEF $1 found, $2'#000+
  '02038_CL_ELSE $1 found, $2'#000+
  '02039_CL_Skipping until...'#000+
  '02040_I_Press <return> to continue'#000+
  '02041_W_Unsupported switch "$1"'#000+
  '02042_W_Illegal compiler directive "$1"'#000+
  '02043_TL_Back in $1'#000+
  '02044_W_Unsu','pported application type: "$1"'#000+
  '02045_W_APPTYPE is not supported by the target OS'#000+
  '02046_W_DESCRIPTION is not supported by the target OS'#000+
  '02047_N_VERSION is not supported by target OS'#000+
  '02048_N_VERSION only for exes or DLLs'#000+
  '02049_W_Wrong format ','for VERSION directive "$1"'#000+
  '02050_E_Illegal assembler style specified "$1"'#000+
  '02051_W_ASM reader switch is not possible inside asm statement, "$1" w'+
  'ill be effective only for next'#000+
  '02052_E_Wrong switch toggle, use ON/OFF or +/-'#000+
  '02053_E_Resource f','iles are not supported for this target'#000+
  '02054_W_Include environment "$1" not found in environment'#000+
  '02055_E_Illegal value for FPU register limit'#000+
  '02056_W_Only one resource file is supported for this target'#000+
  '02057_W_Macro support has been turned ','off'#000+
  '02058_E_Illegal interface type specified. Valids are COM, CORBA or DEF'+
  'AULT.'#000+
  '02059_W_APPID is only supported for PalmOS'#000+
  '02060_W_APPNAME is only supported for PalmOS'#000+
  '02061_E_Constant strings can'#039't be longer than 255 chars'#000+
  '02062_F_Includin','g include files exceeds a depth of 16.'#000+
  '02063_F_Too many levels of PUSH'#000+
  '02064_E_A POP without a preceding PUSH'#000+
  '02065_E_Macro or compile time variable "$1" does not have any value'#000+
  '02066_E_Wrong switch toggle, use ON/OFF/DEFAULT or +/-/*'#000+
  '02067','_E_Mode switch "$1" not allowed here'#000+
  '02068_E_Compile time variable or macro "$1" is not defined.'#000+
  '02069_E_UTF-8 code greater than 65535 found'#000+
  '02070_E_Malformed UTF-8 string'#000+
  '02071_C_UTF-8 signature found, using UTF-8 encoding'#000+
  '02072_E_Compile ','time expression: Wanted $1 but got $2 at $3'#000+
  '02073_N_APPTYPE is not supported by the target OS'#000+
  '02074_E_Illegal optimization specified "$1"'#000+
  '02075_W_SETPEFLAGS is not supported by the target OS'#000+
  '02076_W_IMAGEBASE is not supported by the target ','OS'#000+
  '02077_W_MINSTACKSIZE is not supported by the target OS'#000+
  '02078_W_MAXSTACKSIZE is not supported by the target OS'#000+
  '02079_E_Illegal state for $WARN directive'#000+
  '02080_E_Illegal set packing value'#000+
  '02081_W_PIC directive or switch ignored'#000+
  '02082_W_The',' switch "$1" is not supported by the currently selected t'+
  'arget'#000+
  '02084_W_Framework-related options are only supported for Darwin/Mac OS'+
  ' X'#000+
  '02085_E_Illegal minimal floating point constant precision "$1"'#000+
  '02086_W_Overriding name of "main" procedu','re multiple times, was prev'+
  'iously set to "$1"'#000+
  '03000_E_Parser - Syntax Error'#000+
  '03004_E_INTERRUPT procedure can'#039't be nested'#000+
  '03005_W_Procedure type "$1" ignored'#000+
  '03006_E_Not all declarations of "$1" are declared with OVERLOAD'#000+
  '03008_E_Duplicate ex','ported function name "$1"'#000+
  '03009_E_Duplicate exported function index $1'#000+
  '03010_E_Invalid index for exported function'#000+
  '03011_W_Relocatable DLL or executable $1 debug info does not work, dis'+
  'abled.'#000+
  '03012_W_To allow debugging for win32 code you ne','ed to disable reloca'+
  'tion with -WN option'#000+
  '03013_E_Constructor name must be INIT'#000+
  '03014_E_Destructor name must be DONE'#000+
  '03016_E_Procedure type INLINE not supported'#000+
  '03018_W_Constructor should be public'#000+
  '03019_W_Destructor should be public'#000+
  '03020_N','_Class should have one destructor only'#000+
  '03021_E_Local class definitions are not allowed'#000+
  '03022_F_Anonymous class definitions are not allowed'#000+
  '03023_N_The object "$1" has no VMT'#000+
  '03024_E_Illegal parameter list'#000+
  '03026_E_Wrong number of parameters ','specified for call to "$1"'#000+
  '03027_E_overloaded identifier "$1" isn'#039't a function'#000+
  '03028_E_overloaded functions have the same parameter list'#000+
  '03029_E_function header doesn'#039't match the previous declaration "$1'+
  '"'#000+
  '03030_E_function header "$1" doesn'#039,'t match forward : var name cha'+
  'nges $2 => $3'#000+
  '03031_N_Values in enumeration types have to be ascending'#000+
  '03033_E_With can not be used for variables in a different segment'#000+
  '03034_E_function nesting > 31'#000+
  '03035_E_range check error while evaluating ','constants'#000+
  '03036_W_range check error while evaluating constants'#000+
  '03037_E_duplicate case label'#000+
  '03038_E_Upper bound of case range is less than lower bound'#000+
  '03039_E_typed constants of classes or interfaces are not allowed'#000+
  '03040_E_functions variab','les of overloaded functions are not allowed'#000+
  '03041_E_string length must be a value from 1 to 255'#000+
  '03042_W_use extended syntax of NEW and DISPOSE for instances of object'+
  's'#000+
  '03043_W_use of NEW or DISPOSE for untyped pointers is meaningless'#000+
  '03044_','E_use of NEW or DISPOSE is not possible for untyped pointers'#000+
  '03045_E_class identifier expected'#000+
  '03046_E_type identifier not allowed here'#000+
  '03047_E_method identifier expected'#000+
  '03048_E_function header doesn'#039't match any method of this class "$1'+
  '"'#000+
  '0','3049_DL_procedure/function $1'#000+
  '03050_E_Illegal floating point constant'#000+
  '03051_E_FAIL can be used in constructors only'#000+
  '03052_E_Destructors can'#039't have parameters'#000+
  '03053_E_Only class methods can be referred with class references'#000+
  '03054_E_Only clas','s methods can be accessed in class methods'#000+
  '03055_E_Constant and CASE types do not match'#000+
  '03056_E_The symbol can'#039't be exported from a library'#000+
  '03057_W_An inherited method is hidden by "$1"'#000+
  '03058_E_There is no method in an ancestor class to be ','overridden: "$'+
  '1"'#000+
  '03059_E_No member is provided to access property'#000+
  '03060_W_Stored property directive is not yet implemented'#000+
  '03061_E_Illegal symbol for property access'#000+
  '03062_E_Cannot access a protected field of an object here'#000+
  '03063_E_Cannot a','ccess a private field of an object here'#000+
  '03066_E_Overridden methods must have the same return type: "$2" is ove'+
  'rriden by "$1" which has another return type'#000+
  '03067_E_EXPORT declared functions can'#039't be nested'#000+
  '03068_E_Methods can'#039't be EXPORTed'#000+
  '0','3069_E_Call by var for arg no. $1 has to match exactly: Got "$2" ex'+
  'pected "$3"'#000+
  '03070_E_Class isn'#039't a parent class of the current class'#000+
  '03071_E_SELF is only allowed in methods'#000+
  '03072_E_Methods can be only in other methods called direct with t','ype'+
  ' identifier of the class'#000+
  '03073_E_Illegal use of '#039':'#039#000+
  '03074_E_range check error in set constructor or duplicate set element'#000+
  '03075_E_Pointer to object expected'#000+
  '03076_E_Expression must be constructor call'#000+
  '03077_E_Expression must be destructo','r call'#000+
  '03078_E_Illegal order of record elements'#000+
  '03079_E_Expression type must be class or record type'#000+
  '03080_E_Procedures can'#039't return a value'#000+
  '03081_E_constructors and destructors must be methods'#000+
  '03082_E_Operator is not overloaded'#000+
  '03083_E_Imp','ossible to overload assignment for equal types'#000+
  '03084_E_Impossible operator overload'#000+
  '03085_E_Re-raise isn'#039't possible there'#000+
  '03086_E_The extended syntax of new or dispose isn'#039't allowed for a '+
  'class'#000+
  '03088_E_Procedure overloading is switched off'#000,
  '03089_E_It is not possible to overload this operator. Related overload'+
  'able operators (if any) are: $1'#000+
  '03090_E_Comparative operator must return a boolean value'#000+
  '03091_E_Only virtual methods can be abstract'#000+
  '03092_F_Use of unsupported feature!'#000,
  '03093_E_The mix of different kind of objects (class, object, interface'+
  ', etc) isn'#039't allowed'#000+
  '03094_W_Unknown procedure directive had to be ignored: "$1"'#000+
  '03095_E_absolute can only be associated to one variable'#000+
  '03096_E_absolute can only be asso','ciated with a var or const'#000+
  '03097_E_Only one variable can be initialized'#000+
  '03098_E_Abstract methods shouldn'#039't have any definition (with funct'+
  'ion body)'#000+
  '03099_E_This overloaded function can'#039't be local (must be exported)'+
  #000+
  '03100_W_Virtual methods a','re used without a constructor in "$1"'#000+
  '03101_CL_Macro defined: $1'#000+
  '03102_CL_Macro undefined: $1'#000+
  '03103_CL_Macro $1 set to $2'#000+
  '03104_I_Compiling $1'#000+
  '03105_UL_Parsing interface of unit $1'#000+
  '03106_UL_Parsing implementation of $1'#000+
  '03107_DL_Compiling $1',' for the second time'#000+
  '03109_E_No property found to override'#000+
  '03110_E_Only one default property is allowed'#000+
  '03111_E_The default property must be an array property'#000+
  '03112_E_Virtual constructors are only supported in class object model'#000+
  '03113_E_No ','default property available'#000+
  '03114_E_The class can'#039't have a published section, use the {$M+} sw'+
  'itch'#000+
  '03115_E_Forward declaration of class "$1" must be resolved here to use'+
  ' the class as ancestor'#000+
  '03116_E_Local operators not supported'#000+
  '03117_E_Pro','cedure directive "$1" not allowed in interface section'#000+
  '03118_E_Procedure directive "$1" not allowed in implementation section'+
  #000+
  '03119_E_Procedure directive "$1" not allowed in procvar declaration'#000+
  '03120_E_Function is already declared Public/Fo','rward "$1"'#000+
  '03121_E_Can'#039't use both EXPORT and EXTERNAL'#000+
  '03123_W_"$1" not yet supported inside inline procedure/function'#000+
  '03124_W_Inlining disabled'#000+
  '03125_I_Writing Browser log $1'#000+
  '03126_H_may be pointer dereference is missing'#000+
  '03127_F_Selected as','sembler reader not supported'#000+
  '03128_E_Procedure directive "$1" has conflicts with other directives'#000+
  '03129_E_Calling convention doesn'#039't match forward'#000+
  '03131_E_Property can'#039't have a default value'#000+
  '03132_E_The default value of a property must be c','onstant'#000+
  '03133_E_Symbol can'#039't be published, can be only a class'#000+
  '03134_E_This kind of property can'#039't be published'#000+
  '03136_E_An import name is required'#000+
  '03138_E_Division by zero'#000+
  '03139_E_Invalid floating point operation'#000+
  '03140_E_Upper bound of rang','e is less than lower bound'#000+
  '03141_W_string "$1" is longer than "$2"'#000+
  '03142_E_string length is larger than array of char length'#000+
  '03143_E_Illegal expression after message directive'#000+
  '03144_E_Message handlers can take only one call by ref. paramete','r'#000+
  '03145_E_Duplicate message label: "$1"'#000+
  '03146_E_Self can only be an explicit parameter in methods which are me'+
  'ssage handlers'#000+
  '03147_E_Threadvars can be only static or global'#000+
  '03148_F_Direct assembler not supported for binary output format'#000+
  '031','49_W_Don'#039't load OBJPAS unit manually, use \{\$mode objfpc\} o'+
  'r \{\$mode delphi\} instead'#000+
  '03150_E_OVERRIDE can'#039't be used in objects'#000+
  '03151_E_Data types which require initialization/finalization can'#039't'+
  ' be used in variant records'#000+
  '03152_E_Resourc','estrings can be only static or global'#000+
  '03153_E_Exit with argument can'#039't be used here'#000+
  '03154_E_The type of the storage symbol must be boolean'#000+
  '03155_E_This symbol isn'#039't allowed as storage symbol'#000+
  '03156_E_Only classes which are compiled in $M+ mo','de can be published'+
  #000+
  '03157_E_Procedure directive expected'#000+
  '03158_E_The value for a property index must be of an ordinal type'#000+
  '03159_E_Procedure name too short to be exported'#000+
  '03160_E_No DEFFILE entry can be generated for unit global vars'#000+
  '03161_','E_Compile without -WD option'#000+
  '03162_F_You need ObjFpc (-S2) or Delphi (-Sd) mode to compile this mod'+
  'ule'#000+
  '03163_E_Can'#039't export with index under $1'#000+
  '03164_E_Exporting of variables is not supported under $1'#000+
  '03165_E_Improper GUID syntax'#000+
  '03168_W_Pr','ocedure named "$1" not found that is suitable for implemen'+
  'ting the $2.$3'#000+
  '03169_E_interface identifier expected'#000+
  '03170_E_Type "$1" can'#039't be used as array index type'#000+
  '03171_E_Con- and destructors aren'#039't allowed in interfaces'#000+
  '03172_E_Access spec','ifiers can'#039't be used in INTERFACES'#000+
  '03173_E_An interface can'#039't contain fields'#000+
  '03174_E_Can'#039't declare local procedure as EXTERNAL'#000+
  '03175_W_Some fields coming before "$1" weren'#039't initialized'#000+
  '03176_E_Some fields coming before "$1" weren'#039't initial','ized'#000+
  '03177_W_Some fields coming after "$1" weren'#039't initialized'#000+
  '03178_E_VarArgs directive (or '#039'...'#039' in MacPas) without CDecl/C'+
  'PPDecl/MWPascal and External'#000+
  '03179_E_Self must be a normal (call-by-value) parameter'#000+
  '03180_E_Interface "$1" has no ','interface identification'#000+
  '03181_E_Unknown class field or method identifier "$1"'#000+
  '03182_W_Overriding calling convention "$1" with "$2"'#000+
  '03183_E_Typed constants of the type "procedure of object" can only be '+
  'initialized with NIL'#000+
  '03184_E_Default v','alue can only be assigned to one parameter'#000+
  '03185_E_Default parameter required for "$1"'#000+
  '03186_W_Use of unsupported feature!'#000+
  '03187_H_C arrays are passed by reference'#000+
  '03188_E_C array of const must be the last argument'#000+
  '03189_H_Type "$1" redefin','ition'#000+
  '03190_W_cdecl'#039'ared functions have no high parameter'#000+
  '03191_W_cdecl'#039'ared functions do not support open strings'#000+
  '03192_E_Cannot initialize variables declared as threadvar'#000+
  '03193_E_Message directive is only allowed in Classes'#000+
  '03194_E_Proced','ure or Function expected'#000+
  '03195_W_Calling convention directive ignored: "$1"'#000+
  '03196_E_REINTRODUCE can'#039't be used in objects'#000+
  '03197_E_Each argument must have its own location'#000+
  '03198_E_Each argument must have an explicit location'#000+
  '03199_E_Unknown a','rgument location'#000+
  '03200_E_32 Bit-Integer or pointer variable expected'#000+
  '03201_E_Goto statements aren'#039't allowed between different procedure'+
  's'#000+
  '03202_F_Procedure too complex, it requires too many registers'#000+
  '03203_E_Illegal expression'#000+
  '03204_E_Invali','d integer expression'#000+
  '03205_E_Illegal qualifier'#000+
  '03206_E_High range limit < low range limit'#000+
  '03207_E_Exit'#039's parameter must be the name of the procedure it is u'+
  'sed in'#000+
  '03208_E_Illegal assignment to for-loop variable "$1"'#000+
  '03209_E_Can'#039't declare lo','cal variable as EXTERNAL'#000+
  '03210_E_Procedure is already declared EXTERNAL'#000+
  '03211_W_Implicit uses of Variants unit'#000+
  '03212_E_Class and static methods can'#039't be used in INTERFACES'#000+
  '03213_E_Overflow in arithmetic operation'#000+
  '03214_E_Protected or privat','e expected'#000+
  '03215_E_SLICE can'#039't be used outside of parameter list'#000+
  '03216_E_A DISPINTERFACE can'#039't have a parent class'#000+
  '03217_E_A DISPINTERFACE needs a guid'#000+
  '03218_W_Overridden methods must have a related return type. This code '+
  'may crash, it depe','nds on a Delphi parser bug ("$2" is overridden by '+
  '"$1" which has another return type)'#000+
  '03219_E_Dispatch IDs must be ordinal constants'#000+
  '03220_E_The range of the array is too large'#000+
  '03221_E_The address cannot be taken of bit packed array element','s and'+
  ' record fields'#000+
  '03222_E_Dynamic arrays cannot be packed'#000+
  '03223_E_Bit packed array elements and record fields cannot be used as '+
  'loop variables'#000+
  '03224_E_VAR and TYPE are allowed only in generics'#000+
  '03225_E_This type can'#039't be a generic'#000+
  '03226_W_','Don'#039't load LINEINFO unit manually, Use the -gl compiler '+
  'switch instead'#000+
  '03227_E_No function result type specified for function "$1"'#000+
  '03228_E_Specialization is only supported for generic types'#000+
  '03229_E_Generics can'#039't be used as parameters when ','specializing g'+
  'enerics'#000+
  '03230_E_Constants of objects containing a VMT aren'#039't allowed'#000+
  '03231_E_Taking the address of labels defined outside the current scope'+
  ' isn'#039't allowed'#000+
  '03233_E_Cannot initialize variables declared as external'#000+
  '03234_E_Illegal',' function result type'#000+
  '03235_E_No common type possible between "$1" and "$2"'#000+
  '03236_E_Generics without specialization can not be used as a type for '+
  'a variable'#000+
  '03237_W_Register list is ignored for pure assembler routines'#000+
  '03238_E_Implements pro','perty must have class or interface type'#000+
  '03239_E_Implements-property must implement interface of correct type, '+
  'found "$1" expected "$2"'#000+
  '03240_E_Implements-property must have read specifier'#000+
  '03241_E_Implements-property must not have write-spec','ifier'#000+
  '03242_E_Implements-property must not have stored-specifier'#000+
  '03243_E_Implements-property used on unimplemented interface: "$1"'#000+
  '03244_E_Floating point not supported for this target'#000+
  '03245_E_Class "$1" does not implement interface "$2"'#000+
  '032','46_E_Type used by implements must be an interface'#000+
  '03247_E_Variables cannot be exported with a different name on this tar'+
  'get, add the name to the declaration using the "export" directive (var'+
  'iable name: $1, declared export name: $2)'#000+
  '03248_E_','Weak external symbols are not supported for the current targ'+
  'et'#000+
  '03249_E_Forward type definition does not match'#000+
  '04000_E_Type mismatch'#000+
  '04001_E_Incompatible types: got "$1" expected "$2"'#000+
  '04002_E_Type mismatch between "$1" and "$2"'#000+
  '04003_E_Type ','identifier expected'#000+
  '04004_E_Variable identifier expected'#000+
  '04005_E_Integer expression expected, but got "$1"'#000+
  '04006_E_Boolean expression expected, but got "$1"'#000+
  '04007_E_Ordinal expression expected'#000+
  '04008_E_pointer type expected, but got "$1"'#000+
  '040','09_E_class type expected, but got "$1"'#000+
  '04011_E_Can'#039't evaluate constant expression'#000+
  '04012_E_Set elements are not compatible'#000+
  '04013_E_Operation not implemented for sets'#000+
  '04014_W_Automatic type conversion from floating type to COMP which is '+
  'an in','teger type'#000+
  '04015_H_use DIV instead to get an integer result'#000+
  '04016_E_string types doesn'#039't match, because of $V+ mode'#000+
  '04017_E_succ or pred on enums with assignments not possible'#000+
  '04018_E_Can'#039't read or write variables of this type'#000+
  '04019_E_Can'#039't',' use readln or writeln on typed file'#000+
  '04020_E_Can'#039't use read or write on untyped file.'#000+
  '04021_E_Type conflict between set elements'#000+
  '04022_W_lo/hi(dword/qword) returns the upper/lower word/dword'#000+
  '04023_E_Integer or real expression expected'#000+
  '04024','_E_Wrong type "$1" in array constructor'#000+
  '04025_E_Incompatible type for arg no. $1: Got "$2", expected "$3"'#000+
  '04026_E_Method (variable) and Procedure (variable) are not compatible'#000+
  '04027_E_Illegal constant passed to internal math function'#000+
  '04028_','E_Can'#039't take the address of constant expressions'#000+
  '04029_E_Argument can'#039't be assigned to'#000+
  '04030_E_Can'#039't assign local procedure/function to procedure variabl'+
  'e'#000+
  '04031_E_Can'#039't assign values to an address'#000+
  '04032_E_Can'#039't assign values to const variab','le'#000+
  '04033_E_Array type required'#000+
  '04034_E_interface type expected, but got "$1"'#000+
  '04035_H_Mixing signed expressions and longwords gives a 64bit result'#000+
  '04036_W_Mixing signed expressions and cardinals here may cause a range'+
  ' check error'#000+
  '04037_E_Typ','ecast has different size ($1 -> $2) in assignment'#000+
  '04038_E_enums with assignments can'#039't be used as array index'#000+
  '04039_E_Class or Object types "$1" and "$2" are not related'#000+
  '04040_W_Class types "$1" and "$2" are not related'#000+
  '04041_E_Class or int','erface type expected, but got "$1"'#000+
  '04042_E_Type "$1" is not completely defined'#000+
  '04043_W_String literal has more characters than short string length'#000+
  '04044_W_Comparison is always false due to range of values'#000+
  '04045_W_Comparison is always true d','ue to range of values'#000+
  '04046_W_Constructing a class "$1" with abstract method "$2"'#000+
  '04047_H_The left operand of the IN operator should be byte sized'#000+
  '04048_W_Type size mismatch, possible loss of data / range check error'#000+
  '04049_H_Type size misma','tch, possible loss of data / range check erro'+
  'r'#000+
  '04050_E_The address of an abstract method can'#039't be taken'#000+
  '04051_E_Assignments to formal parameters and open arrays are not possi'+
  'ble'#000+
  '04052_E_Constant Expression expected'#000+
  '04053_E_Operation "$1" no','t supported for types "$2" and "$3"'#000+
  '04054_E_Illegal type conversion: "$1" to "$2"'#000+
  '04055_H_Conversion between ordinals and pointers is not portable'#000+
  '04056_W_Conversion between ordinals and pointers is not portable'#000+
  '04057_E_Can'#039't determine whic','h overloaded function to call'#000+
  '04058_E_Illegal counter variable'#000+
  '04059_W_Converting constant real value to double for C variable argume'+
  'nt, add explicit typecast to prevent this.'#000+
  '04060_E_Class or COM interface type expected, but got "$1"'#000+
  '04061','_E_Constant packed arrays are not yet supported'#000+
  '04062_E_Incompatible type for arg no. $1: Got "$2" expected "(Bit)Pack'+
  'ed Array"'#000+
  '04063_E_Incompatible type for arg no. $1: Got "$2" expected "(not pack'+
  'ed) Array"'#000+
  '04064_E_Elements of packed arra','ys cannot be of a type which need to '+
  'be initialised'#000+
  '04065_E_Constant packed records and objects are not yet supported'#000+
  '04066_W_Arithmetic "$1" on untyped pointer is unportable to {$T+}, sug'+
  'gest typecast'#000+
  '04076_E_Can'#039't take address of a subrou','tine marked as local'#000+
  '04077_E_Can'#039't export subroutine marked as local from a unit'#000+
  '04078_E_Type is not automatable: "$1"'#000+
  '04079_H_Converting the operands to "$1" before doing the add could pre'+
  'vent overflow errors.'#000+
  '04080_H_Converting the operan','ds to "$1" before doing the subtract co'+
  'uld prevent overflow errors.'#000+
  '04081_H_Converting the operands to "$1" before doing the multiply coul'+
  'd prevent overflow errors.'#000+
  '04082_W_Converting pointers to signed integers may result in wrong com'+
  'paris','on results and range errors, use an unsigned type instead.'#000+
  '04083_E_Interface type $1 has no valid GUID'#000+
  '05000_E_Identifier not found "$1"'#000+
  '05001_F_Internal Error in SymTableStack()'#000+
  '05002_E_Duplicate identifier "$1"'#000+
  '05003_H_Identifier already ','defined in $1 at line $2'#000+
  '05004_E_Unknown identifier "$1"'#000+
  '05005_E_Forward declaration not solved "$1"'#000+
  '05007_E_Error in type definition'#000+
  '05009_E_Forward type not resolved "$1"'#000+
  '05010_E_Only static variables can be used in static methods or outs','i'+
  'de methods'#000+
  '05012_F_record or class type expected'#000+
  '05013_E_Instances of classes or objects with an abstract method are no'+
  't allowed'#000+
  '05014_W_Label not defined "$1"'#000+
  '05015_E_Label used but not defined "$1"'#000+
  '05016_E_Illegal label declaration'#000+
  '05017','_E_GOTO and LABEL are not supported (use switch -Sg)'#000+
  '05018_E_Label not found'#000+
  '05019_E_identifier isn'#039't a label'#000+
  '05020_E_label already defined'#000+
  '05021_E_illegal type declaration of set elements'#000+
  '05022_E_Forward class definition not resolved "$1"'#000,
  '05023_H_Unit "$1" not used in $2'#000+
  '05024_H_Parameter "$1" not used'#000+
  '05025_N_Local variable "$1" not used'#000+
  '05026_H_Value parameter "$1" is assigned but never used'#000+
  '05027_N_Local variable "$1" is assigned but never used'#000+
  '05028_H_Local $1 "$2" is no','t used'#000+
  '05029_N_Private field "$1.$2" is never used'#000+
  '05030_N_Private field "$1.$2" is assigned but never used'#000+
  '05031_N_Private method "$1.$2" never used'#000+
  '05032_E_Set type expected'#000+
  '05033_W_Function result does not seem to be set'#000+
  '05034_W_Type "$1','" is not aligned correctly in current record for C'#000+
  '05035_E_Unknown record field identifier "$1"'#000+
  '05036_W_Local variable "$1" does not seem to be initialized'#000+
  '05037_W_Variable "$1" does not seem to be initialized'#000+
  '05038_E_identifier idents no m','ember "$1"'#000+
  '05039_H_Found declaration: $1'#000+
  '05040_E_Data element too large'#000+
  '05042_E_No matching implementation for interface method "$1" found'#000+
  '05043_W_Symbol "$1" is deprecated'#000+
  '05044_W_Symbol "$1" is not portable'#000+
  '05055_W_Symbol "$1" is not impl','emented'#000+
  '05056_E_Can'#039't create unique type from this type'#000+
  '05057_H_Local variable "$1" does not seem to be initialized'#000+
  '05058_H_Variable "$1" does not seem to be initialized'#000+
  '05059_W_Function result variable does not seem to initialized'#000+
  '05060_H_','Function result variable does not seem to be initialized'#000+
  '05061_W_Variable "$1" read but nowhere assigned'#000+
  '05062_H_Found abstract method: $1'#000+
  '05063_W_Symbol "$1" is experimental'#000+
  '05064_W_Forward declaration "$1" not resolved, assumed external'#000+
  '0','6009_E_Parameter list size exceeds 65535 bytes'#000+
  '06012_E_File types must be var parameters'#000+
  '06013_E_The use of a far pointer isn'#039't allowed there'#000+
  '06015_E_EXPORT declared functions can'#039't be called'#000+
  '06016_W_Possible illegal call of constructor or ','destructor'#000+
  '06017_N_Inefficient code'#000+
  '06018_W_unreachable code'#000+
  '06020_E_Abstract methods can'#039't be called directly'#000+
  '06027_DL_Register $1 weight $2 $3'#000+
  '06029_DL_Stack frame is omitted'#000+
  '06031_E_Object or class methods can'#039't be inline.'#000+
  '06032_E_Procva','r calls cannot be inline.'#000+
  '06033_E_No code for inline procedure stored'#000+
  '06035_E_Element zero of an ansi/wide- or longstring can'#039't be acces'+
  'sed, use (set)length instead'#000+
  '06037_E_Constructors or destructors can not be called inside a '#039'wi'+
  'th'#039' claus','e'#000+
  '06038_E_Cannot call message handler methods directly'#000+
  '06039_E_Jump in or outside of an exception block'#000+
  '06040_E_Control flow statements aren'#039't allowed in a finally block'#000+
  '06041_W_Parameters size exceeds limit for certain cpu'#039's'#000+
  '06042_W_Local ','variable size exceed limit for certain cpu'#039's'#000+
  '06043_E_Local variables size exceeds supported limit'#000+
  '06044_E_BREAK not allowed'#000+
  '06045_E_CONTINUE not allowed'#000+
  '06046_F_Unknown compilerproc "$1". Check if you use the correct run ti'+
  'me library.'#000+
  '06047','_F_Cannot find system type "$1". Check if you use the correct r'+
  'un time library.'#000+
  '06048_H_Inherited call to abstract method ignored'#000+
  '06049_E_Goto label "$1" not defined or optimized away'#000+
  '07000_DL_Starting $1 styled assembler parsing'#000+
  '07001_DL_F','inished $1 styled assembler parsing'#000+
  '07002_E_Non-label pattern contains @'#000+
  '07004_E_Error building record offset'#000+
  '07005_E_OFFSET used without identifier'#000+
  '07006_E_TYPE used without identifier'#000+
  '07007_E_Cannot use local variable or parameters here'#000+
  '0','7008_E_need to use OFFSET here'#000+
  '07009_E_need to use $ here'#000+
  '07010_E_Cannot use multiple relocatable symbols'#000+
  '07011_E_Relocatable symbol can only be added'#000+
  '07012_E_Invalid constant expression'#000+
  '07013_E_Relocatable symbol is not allowed'#000+
  '07014_E_Inv','alid reference syntax'#000+
  '07015_E_You can not reach $1 from that code'#000+
  '07016_E_Local symbols/labels aren'#039't allowed as references'#000+
  '07017_E_Invalid base and index register usage'#000+
  '07018_W_Possible error in object field handling'#000+
  '07019_E_Wrong scale fa','ctor specified'#000+
  '07020_E_Multiple index register usage'#000+
  '07021_E_Invalid operand type'#000+
  '07022_E_Invalid string as opcode operand: $1'#000+
  '07023_W_@CODE and @DATA not supported'#000+
  '07024_E_Null label references are not allowed'#000+
  '07025_E_Divide by zero in asm',' evaluator'#000+
  '07026_E_Illegal expression'#000+
  '07027_E_escape sequence ignored: $1'#000+
  '07028_E_Invalid symbol reference'#000+
  '07029_W_Fwait can cause emulation problems with emu387'#000+
  '07030_W_$1 without operand translated into $1P'#000+
  '07031_W_ENTER instruction is no','t supported by Linux kernel'#000+
  '07032_W_Calling an overload function in assembler'#000+
  '07033_E_Unsupported symbol type for operand'#000+
  '07034_E_Constant value out of bounds'#000+
  '07035_E_Error converting decimal $1'#000+
  '07036_E_Error converting octal $1'#000+
  '07037_E_Err','or converting binary $1'#000+
  '07038_E_Error converting hexadecimal $1'#000+
  '07039_H_$1 translated to $2'#000+
  '07040_W_$1 is associated to an overloaded function'#000+
  '07041_E_Cannot use SELF outside a method'#000+
  '07042_E_Cannot use OLDEBP outside a nested procedure'#000+
  '070','43_W_Procedures can'#039't return any value in asm code'#000+
  '07044_E_SEG not supported'#000+
  '07045_E_Size suffix and destination or source size do not match'#000+
  '07046_W_Size suffix and destination or source size do not match'#000+
  '07047_E_Assembler syntax error'#000+
  '0704','8_E_Invalid combination of opcode and operands'#000+
  '07049_E_Assembler syntax error in operand'#000+
  '07050_E_Assembler syntax error in constant'#000+
  '07051_E_Invalid String expression'#000+
  '07052_W_constant with symbol $1 for address which is not on a pointer'#000+
  '0705','3_E_Unrecognized opcode $1'#000+
  '07054_E_Invalid or missing opcode'#000+
  '07055_E_Invalid combination of prefix and opcode: $1'#000+
  '07056_E_Invalid combination of override and opcode: $1'#000+
  '07057_E_Too many operands on line'#000+
  '07058_W_NEAR ignored'#000+
  '07059_W_FAR igno','red'#000+
  '07060_E_Duplicate local symbol $1'#000+
  '07061_E_Undefined local symbol $1'#000+
  '07062_E_Unknown label identifier $1'#000+
  '07063_E_Invalid register name'#000+
  '07064_E_Invalid floating point register name'#000+
  '07066_W_Modulo not supported'#000+
  '07067_E_Invalid floating poi','nt constant $1'#000+
  '07068_E_Invalid floating point expression'#000+
  '07069_E_Wrong symbol type'#000+
  '07070_E_Cannot index a local var or parameter with a register'#000+
  '07071_E_Invalid segment override expression'#000+
  '07072_W_Identifier $1 supposed external'#000+
  '07073_E_Str','ings not allowed as constants'#000+
  '07074_No type of variable specified'#000+
  '07075_E_assembler code not returned to text section'#000+
  '07076_E_Not a directive or local symbol $1'#000+
  '07077_E_Using a defined name as a local label'#000+
  '07078_E_Dollar token is used with','out an identifier'#000+
  '07079_W_32bit constant created for address'#000+
  '07080_N_.align is target specific, use .balign or .p2align'#000+
  '07081_E_Can'#039't access fields directly for parameters'#000+
  '07082_E_Can'#039't access fields of objects/classes directly'#000+
  '07083_E_No s','ize specified and unable to determine the size of the op'+
  'erands'#000+
  '07084_E_Cannot use RESULT in this function'#000+
  '07086_W_"$1" without operand translated into "$1 %st,%st(1)"'#000+
  '07087_W_"$1 %st(n)" translated into "$1 %st,%st(n)"'#000+
  '07088_W_"$1 %st(n)" t','ranslated into "$1 %st(n),%st"'#000+
  '07089_E_Char < not allowed here'#000+
  '07090_E_Char > not allowed here'#000+
  '07093_W_ALIGN not supported'#000+
  '07094_E_Inc and Dec cannot be together'#000+
  '07095_E_Invalid reglist for movem'#000+
  '07096_E_Reglist invalid for opcode'#000+
  '07097_E_H','igher cpu mode required ($1)'#000+
  '07098_W_No size specified and unable to determine the size of the oper'+
  'ands, using DWORD as default'#000+
  '07099_E_Syntax error while trying to parse a shifter operand'#000+
  '07100_E_Address of packed component is not at a byt','e boundary'#000+
  '07101_W_No size specified and unable to determine the size of the oper'+
  'ands, using BYTE as default'#000+
  '07102_W_Use of +offset(%ebp) for parameters invalid here'#000+
  '07103_W_Use of +offset(%ebp) is not compatible with regcall convention'+
  #000+
  '071','04_W_Use of -offset(%ebp) is not recommended for local variable a'+
  'ccess'#000+
  '07105_W_Use of -offset(%esp), access may cause a crash or value may be'+
  ' lost'#000+
  '07106_E_VMTOffset must be used in combination with a virtual method, a'+
  'nd "$1" is not virtual'#000,
  '07107_E_Generating PIC, but reference is not PIC-safe'#000+
  '08000_F_Too many assembler files'#000+
  '08001_F_Selected assembler output not supported'#000+
  '08002_F_Comp not supported'#000+
  '08003_F_Direct not support for binary writers'#000+
  '08004_E_Allocating of data is on','ly allowed in bss section'#000+
  '08005_F_No binary writer selected'#000+
  '08006_E_Asm: Opcode $1 not in table'#000+
  '08007_E_Asm: $1 invalid combination of opcode and operands'#000+
  '08008_E_Asm: 16 Bit references not supported'#000+
  '08009_E_Asm: Invalid effective address'#000+
  '0','8010_E_Asm: Immediate or reference expected'#000+
  '08011_E_Asm: $1 value exceeds bounds $2'#000+
  '08012_E_Asm: Short jump is out of range $1'#000+
  '08013_E_Asm: Undefined label $1'#000+
  '08014_E_Asm: Comp type not supported for this target'#000+
  '08015_E_Asm: Extended type n','ot supported for this target'#000+
  '08016_E_Asm: Duplicate label $1'#000+
  '08017_E_Asm: Redefined label $1'#000+
  '08018_E_Asm: First defined here'#000+
  '08019_E_Asm: Invalid register $1'#000+
  '08020_E_Asm: 16 or 32 Bit references not supported'#000+
  '08021_E_Asm: 64 Bit operands no','t supported'#000+
  '09000_W_Source operating system redefined'#000+
  '09001_I_Assembling (pipe) $1'#000+
  '09002_E_Can'#039't create assembler file: $1'#000+
  '09003_E_Can'#039't create object file: $1'#000+
  '09004_E_Can'#039't create archive file: $1'#000+
  '09005_E_Assembler $1 not found, switching ','to external assembling'#000+
  '09006_T_Using assembler: $1'#000+
  '09007_E_Error while assembling exitcode $1'#000+
  '09008_E_Can'#039't call the assembler, error $1 switching to external a'+
  'ssembling'#000+
  '09009_I_Assembling $1'#000+
  '09010_I_Assembling with smartlinking $1'#000+
  '09011_W_','Object $1 not found, Linking may fail !'#000+
  '09012_W_Library $1 not found, Linking may fail !'#000+
  '09013_E_Error while linking'#000+
  '09014_E_Can'#039't call the linker, switching to external linking'#000+
  '09015_I_Linking $1'#000+
  '09016_E_Util $1 not found, switching to ext','ernal linking'#000+
  '09017_T_Using util $1'#000+
  '09018_E_Creation of Executables not supported'#000+
  '09019_E_Creation of Dynamic/Shared Libraries not supported'#000+
  '09020_I_Closing script $1'#000+
  '09021_E_resource compiler "$1" not found, switching to external mode'#000+
  '0902','2_I_Compiling resource $1'#000+
  '09023_T_unit $1 can'#039't be statically linked, switching to smart lin'+
  'king'#000+
  '09024_T_unit $1 can'#039't be smart linked, switching to static linking'+
  #000+
  '09025_T_unit $1 can'#039't be shared linked, switching to static linkin'+
  'g'#000+
  '09026_E_','unit $1 can'#039't be smart or static linked'#000+
  '09027_E_unit $1 can'#039't be shared or static linked'#000+
  '09028_D_Calling resource compiler "$1" with "$2" as command line'#000+
  '09029_E_Error while compiling resources'#000+
  '09030_E_Can'#039't call the resource compiler "$1",',' switching to exte'+
  'rnal mode'#000+
  '09031_E_Can'#039't open resource file "$1"'#000+
  '09032_E_Can'#039't write resource file "$1"'#000+
  '09128_F_Can'#039't post process executable $1'#000+
  '09129_F_Can'#039't open executable $1'#000+
  '09130_X_Size of Code: $1 bytes'#000+
  '09131_X_Size of initialized da','ta: $1 bytes'#000+
  '09132_X_Size of uninitialized data: $1 bytes'#000+
  '09133_X_Stack space reserved: $1 bytes'#000+
  '09134_X_Stack space committed: $1 bytes'#000+
  '09200_F_Executable image size is too big for $1 target.'#000+
  '10000_T_Unitsearch: $1'#000+
  '10001_T_PPU Loading $1'#000+
  '1','0002_U_PPU Name: $1'#000+
  '10003_U_PPU Flags: $1'#000+
  '10004_U_PPU Crc: $1'#000+
  '10005_U_PPU Time: $1'#000+
  '10006_U_PPU File too short'#000+
  '10007_U_PPU Invalid Header (no PPU at the begin)'#000+
  '10008_U_PPU Invalid Version $1'#000+
  '10009_U_PPU is compiled for another processor'#000+
  '1001','0_U_PPU is compiled for an other target'#000+
  '10011_U_PPU Source: $1'#000+
  '10012_U_Writing $1'#000+
  '10013_F_Can'#039't Write PPU-File'#000+
  '10014_F_Error reading PPU-File'#000+
  '10015_F_unexpected end of PPU-File'#000+
  '10016_F_Invalid PPU-File entry: $1'#000+
  '10017_F_PPU Dbx count proble','m'#000+
  '10018_E_Illegal unit name: $1'#000+
  '10019_F_Too much units'#000+
  '10020_F_Circular unit reference between $1 and $2'#000+
  '10021_F_Can'#039't compile unit $1, no sources available'#000+
  '10022_F_Can'#039't find unit $1 used by $2'#000+
  '10023_W_Unit $1 was not found but $2 exists'#000+
  '1','0024_F_Unit $1 searched but $2 found'#000+
  '10025_W_Compiling the system unit requires the -Us switch'#000+
  '10026_F_There were $1 errors compiling module, stopping'#000+
  '10027_U_Load from $1 ($2) unit $3'#000+
  '10028_U_Recompiling $1, checksum changed for $2'#000+
  '10029_U','_Recompiling $1, source found only'#000+
  '10030_U_Recompiling unit, static lib is older than ppufile'#000+
  '10031_U_Recompiling unit, shared lib is older than ppufile'#000+
  '10032_U_Recompiling unit, obj and asm are older than ppufile'#000+
  '10033_U_Recompiling unit, ','obj is older than asm'#000+
  '10034_U_Parsing interface of $1'#000+
  '10035_U_Parsing implementation of $1'#000+
  '10036_U_Second load for unit $1'#000+
  '10037_U_PPU Check file $1 time $2'#000+
  '10040_W_Can'#039't recompile unit $1, but found modifed include files'#000+
  '10041_U_File $1 is',' newer than PPU file $2'#000+
  '10042_U_Trying to use a unit which was compiled with a different FPU m'+
  'ode'#000+
  '10043_U_Loading interface units from $1'#000+
  '10044_U_Loading implementation units from $1'#000+
  '10045_U_Interface CRC changed for unit $1'#000+
  '10046_U_Impleme','ntation CRC changed for unit $1'#000+
  '10047_U_Finished compiling unit $1'#000+
  '10048_U_Add dependency of $1 to $2'#000+
  '10049_U_No reload, is caller: $1'#000+
  '10050_U_No reload, already in second compile: $1'#000+
  '10051_U_Flag for reload: $1'#000+
  '10052_U_Forced reloading'#000+
  '100','53_U_Previous state of $1: $2'#000+
  '10054_U_Already compiling $1, setting second compile'#000+
  '10055_U_Loading unit $1'#000+
  '10056_U_Finished loading unit $1'#000+
  '10057_U_Registering new unit $1'#000+
  '10058_U_Re-resolving unit $1'#000+
  '10059_U_Skipping re-resolving unit $1, ','still loading used units'#000+
  '10060_U_Unloading resource unit $1 (not needed)'#000+
  '11000_O_$1 [options] <inputfile> [options]'#000+
  '11001_W_Only one source file supported'#000+
  '11002_W_DEF file can be created only for OS/2'#000+
  '11003_E_nested response files are not s','upported'#000+
  '11004_F_No source file name in command line'#000+
  '11005_N_No option inside $1 config file'#000+
  '11006_E_Illegal parameter: $1'#000+
  '11007_H_-? writes help pages'#000+
  '11008_F_Too many config files nested'#000+
  '11009_F_Unable to open file $1'#000+
  '11010_D_Reading furt','her options from $1'#000+
  '11011_W_Target is already set to: $1'#000+
  '11012_W_Shared libs not supported on DOS platform, reverting to static'+
  #000+
  '11013_F_In options file $1 at line $2 too many \var{\#IF(N)DEFs} encou'+
  'ntered'#000+
  '11014_F_In options file $1 at line ','$2 unexpected \var{\#ENDIFs} enco'+
  'untered'#000+
  '11015_F_Open conditional at the end of the options file'#000+
  '11016_W_Debug information generation is not supported by this executab'+
  'le'#000+
  '11017_H_Try recompiling with -dGDB'#000+
  '11018_W_You are using the obsolete ','switch $1'#000+
  '11019_W_You are using the obsolete switch $1, please use $2'#000+
  '11020_N_Switching assembler to default source writing assembler'#000+
  '11021_W_Assembler output selected "$1" is not compatible with "$2"'#000+
  '11022_W_"$1" assembler use forced'#000+
  '11026','_T_Reading options from file $1'#000+
  '11027_T_Reading options from environment $1'#000+
  '11028_D_Handling option "$1"'#000+
  '11029_O_*** press enter ***'#000+
  '11030_H_Start of reading config file $1'#000+
  '11031_H_End of reading config file $1'#000+
  '11032_D_interpreting option "','$1"'#000+
  '11036_D_interpreting firstpass option "$1"'#000+
  '11033_D_interpreting file option "$1"'#000+
  '11034_D_Reading config file "$1"'#000+
  '11035_D_found source file name "$1"'#000+
  '11039_E_Unknown code page'#000+
  '11040_F_Config file $1 is a directory'#000+
  '11041_W_Assembler outp','ut selected "$1" cannot generate debug info, d'+
  'ebugging disabled'#000+
  '11042_W_Use of ppc386.cfg is deprecated, please use fpc.cfg instead'#000+
  '11043_F_In options file $1 at line $2 \var{\#ELSE} directive without \'+
  'var{\#IF(N)DEF} found'#000+
  '11023_Free Pasca','l Compiler version $FPCFULLVERSION [$FPCDATE] for $F'+
  'PCCPU'#010+
  'Copyright (c) 1993-2008 by Florian Klaempfl'#000+
  '11024_Free Pascal Compiler version $FPCVERSION'#010+
  #010+
  'Compiler Date      : $FPCDATE'#010+
  'Compiler CPU Target: $FPCCPU'#010+
  #010+
  'Supported targets:'#010+
  '  $OSTARGET','S'#010+
  #010+
  'Supported CPU instruction sets:'#010+
  '  $INSTRUCTIONSETS'#010+
  #010+
  'Supported FPU instruction sets:'#010+
  '  $FPUINSTRUCTIONSETS'#010+
  #010+
  'Supported ABI targets:'#010+
  '  $ABITARGETS'#010+
  #010+
  'Supported Optimizations:'#010+
  '  $OPTIMIZATIONS'#010+
  #010+
  'This program comes under the GNU General Public L','icence'#010+
  'For more information read COPYING.FPC'#010+
  #010+
  'Report bugs,suggestions etc to:'#010+
  '                 bugs@freepascal.org'#000+
  '11025_**0*_Put + after a boolean switch option to enable it, - to disa'+
  'ble it'#010+
  '**1a_The compiler doesn'#039't delete the generated a','ssembler file'#010+
  '**2al_List sourcecode lines in assembler file'#010+
  '**2an_List node info in assembler file'#010+
  '*L2ap_Use pipes instead of creating temporary assembler files'#010+
  '**2ar_List register allocation/release info in assembler file'#010+
  '**2at_List temp a','llocation/release info in assembler file'#010+
  '**1A<x>_Output format:'#010+
  '**2Adefault_Use default assembler'#010+
  '3*2Aas_Assemble using GNU AS'#010+
  '3*2Anasmcoff_COFF (Go32v2) file using Nasm'#010+
  '3*2Anasmelf_ELF32 (Linux) file using Nasm'#010+
  '3*2Anasmwin32_Win32 object f','ile using Nasm'#010+
  '3*2Anasmwdosx_Win32/WDOSX object file using Nasm'#010+
  '3*2Awasm_Obj file using Wasm (Watcom)'#010+
  '3*2Anasmobj_Obj file using Nasm'#010+
  '3*2Amasm_Obj file using Masm (Microsoft)'#010+
  '3*2Atasm_Obj file using Tasm (Borland)'#010+
  '3*2Aelf_ELF (Linux) using ','internal writer'#010+
  '3*2Acoff_COFF (Go32v2) using internal writer'#010+
  '3*2Apecoff_PE-COFF (Win32) using internal writer'#010+
  '4*2Aas_Assemble using GNU AS'#010+
  '6*2Aas_Unix o-file using GNU AS'#010+
  '6*2Agas_GNU Motorola assembler'#010+
  '6*2Amit_MIT Syntax (old GAS)'#010+
  '6*2Amot_S','tandard Motorola assembler'#010+
  'A*2Aas_Assemble using GNU AS'#010+
  'P*2Aas_Assemble using GNU AS'#010+
  'S*2Aas_Assemble using GNU AS'#010+
  '**1b_Generate browser info'#010+
  '**2bl_Generate local symbol info'#010+
  '**1B_Build all modules'#010+
  '**1C<x>_Code generation options:'#010+
  '**2Ca<x>_S','elect ABI, see fpc -i for possible values'#010+
  '**2Cb_Generate big-endian code'#010+
  '**2Cc<x>_Set default calling convention to <x>'#010+
  '**2CD_Create also dynamic library (not supported)'#010+
  '**2Ce_Compilation with emulated floating point opcodes'#010+
  '**2Cf<x>_Select',' fpu instruction set to use, see fpc -i for possible '+
  'values'#010+
  '**2CF<x>_Minimal floating point constant precision (default, 32, 64)'#010+
  '**2Cg_Generate PIC code'#010+
  '**2Ch<n>_<n> bytes heap (between 1023 and 67107840)'#010+
  '**2Ci_IO-checking'#010+
  '**2Cn_Omit linkin','g stage'#010+
  '**2Co_Check overflow of integer operations'#010+
  '**2CO_Check for possible overflow of integer operations'#010+
  '**2Cp<x>_Select instruction set, see fpc -i for possible values'#010+
  '**2CP<x>=<y>_ packing settings'#010+
  '**3CPPACKSET=<y>_ <y> set allocation: ','0, 1 or DEFAULT or NORMAL, 2, '+
  '4 and 8'#010+
  '**2Cr_Range checking'#010+
  '**2CR_Verify object method call validity'#010+
  '**2Cs<n>_Set stack size to <n>'#010+
  '**2Ct_Stack checking'#010+
  '**2CX_Create also smartlinked library'#010+
  '**1d<x>_Defines the symbol <x>'#010+
  '**1D_Generate a DEF',' file'#010+
  '**2Dd<x>_Set description to <x>'#010+
  '**2Dv<x>_Set DLL version to <x>'#010+
  '*O2Dw_PM application'#010+
  '**1e<x>_Set path to executable'#010+
  '**1E_Same as -Cn'#010+
  '**1fPIC_Same as -Cg'#010+
  '**1F<x>_Set file names and paths:'#010+
  '**2Fa<x>[,y]_(for a program) load units <x> and',' [y] before uses is p'+
  'arsed'#010+
  '**2Fc<x>_Set input codepage to <x>'#010+
  '**2FC<x>_Set RC compiler binary name to <x>'#010+
  '**2FD<x>_Set the directory where to search for compiler utilities'#010+
  '**2Fe<x>_Redirect error output to <x>'#010+
  '**2Ff<x>_Add <x> to framework ','path (Darwin only)'#010+
  '**2FE<x>_Set exe/unit output path to <x>'#010+
  '**2Fi<x>_Add <x> to include path'#010+
  '**2Fl<x>_Add <x> to library path'#010+
  '**2FL<x>_Use <x> as dynamic linker'#010+
  '**2Fm<x>_Load unicode conversion table from <x>.txt in the compiler di'+
  'r'#010+
  '**2Fo<x','>_Add <x> to object path'#010+
  '**2Fr<x>_Load error message file <x>'#010+
  '**2FR<x>_Set resource (.res) linker to <x>'#010+
  '**2Fu<x>_Add <x> to unit path'#010+
  '**2FU<x>_Set unit output path to <x>, overrides -FE'#010+
  '*g1g_Generate debug information (default format for t','arget)'#010+
  '*g2gc_Generate checks for pointers'#010+
  '*g2gh_Use heaptrace unit (for memory leak/corruption debugging)'#010+
  '*g2gl_Use line info unit (show more info with backtraces)'#010+
  '*g2go<x>_Set debug information options'#010+
  '*g3godwarfsets_ Enable Dwarf set debu','g information (breaks gdb < 6.5'+
  ')'#010+
  '*g2gp_Preserve case in stabs symbol names'#010+
  '*g2gs_Generate stabs debug information'#010+
  '*g2gt_Trash local variables (to detect uninitialized uses)'#010+
  '*g2gv_Generates programs traceable with valgrind'#010+
  '*g2gw_Generate dwa','rf-2 debug information (same as -gw2)'#010+
  '*g2gw2_Generate dwarf-2 debug information'#010+
  '*g2gw3_Generate dwarf-3 debug information'#010+
  '**1i_Information'#010+
  '**2iD_Return compiler date'#010+
  '**2iV_Return short compiler version'#010+
  '**2iW_Return full compiler version'#010+
  '**2','iSO_Return compiler OS'#010+
  '**2iSP_Return compiler host processor'#010+
  '**2iTO_Return target OS'#010+
  '**2iTP_Return target processor'#010+
  '**1I<x>_Add <x> to include path'#010+
  '**1k<x>_Pass <x> to the linker'#010+
  '**1l_Write logo'#010+
  '**1M<x>_Set language mode to <x>'#010+
  '**2Mfpc_Free',' Pascal dialect (default)'#010+
  '**2Mobjfpc_FPC mode with Object Pascal support'#010+
  '**2Mdelphi_Delphi 7 compatibility mode'#010+
  '**2Mtp_TP/BP 7.0 compatibility mode'#010+
  '**2Mmacpas_Macintosh Pascal dialects compatibility mode'#010+
  '**1n_Do not read the default config ','files'#010+
  '**1N<x>_Node tree optimizations'#010+
  '**2Nu_Unroll loops'#010+
  '**1o<x>_Change the name of the executable produced to <x>'#010+
  '**1O<x>_Optimizations:'#010+
  '**2O-_Disable optimizations'#010+
  '**2O1_Level 1 optimizations (quick and debugger friendly)'#010+
  '**2O2_Level 2 op','timizations (-O1 + quick optimizations)'#010+
  '**2O3_Level 3 optimizations (-O2 + slow optimizations)'#010+
  '**2Oa<x>=<y>_Set alignment'#010+
  '**2Oo[NO]<x>_Enable or disable optimizations, see fpc -i for possible '+
  'values'#010+
  '**2Op<x>_Set target cpu for optimizing, s','ee fpc -i for possible valu'+
  'es'#010+
  '**2Os_Optimize for size rather than speed'#010+
  '**1pg_Generate profile code for gprof (defines FPC_PROFILE)'#010+
  '**1R<x>_Assembler reading style:'#010+
  '**2Rdefault_Use default assembler for target'#010+
  '3*2Ratt_Read AT&T style assemb','ler'#010+
  '3*2Rintel_Read Intel style assembler'#010+
  '6*2RMOT_Read motorola style assembler'#010+
  '**1S<x>_Syntax options:'#010+
  '**2S2_Same as -Mobjfpc'#010+
  '**2Sc_Support operators like C (*=,+=,/= and -=)'#010+
  '**2Sa_Turn on assertions'#010+
  '**2Sd_Same as -Mdelphi'#010+
  '**2Se<x>_Error op','tions. <x> is a combination of the following:'#010+
  '**3*_<n> : Compiler halts after the <n> errors (default is 1)'#010+
  '**3*_w : Compiler also halts after warnings'#010+
  '**3*_n : Compiler also halts after notes'#010+
  '**3*_h : Compiler also halts after hints'#010+
  '**2Sg_','Enable LABEL and GOTO (default in -Mtp and -Mdelphi)'#010+
  '**2Sh_Use ansistrings by default instead of shortstrings'#010+
  '**2Si_Turn on inlining of procedures/functions declared as "inline"'#010+
  '**2Sk_Load fpcylix unit'#010+
  '**2SI<x>_Set interface style to <x>'#010+
  '**','3SIcom_COM compatible interface (default)'#010+
  '**3SIcorba_CORBA compatible interface'#010+
  '**2Sm_Support macros like C (global)'#010+
  '**2So_Same as -Mtp'#010+
  '**2Ss_Constructor name must be init (destructor must be done)'#010+
  '**2St_Allow static keyword in objects'#010+
  '**2S','x_Enable exception keywords (default in Delphi/ObjFPC modes)'#010+
  '**1s_Do not call assembler and linker'#010+
  '**2sh_Generate script to link on host'#010+
  '**2st_Generate script to link on target'#010+
  '**2sr_Skip register allocation phase (use with -alr)'#010+
  '**1T<x>_Ta','rget operating system:'#010+
  '3*2Temx_OS/2 via EMX (including EMX/RSX extender)'#010+
  '3*2Tfreebsd_FreeBSD'#010+
  '3*2Tgo32v2_Version 2 of DJ Delorie DOS extender'#010+
  '3*2Tlinux_Linux'#010+
  '3*2Tnetbsd_NetBSD'#010+
  '3*2Tnetware_Novell Netware Module (clib)'#010+
  '3*2Tnetwlibc_Novell Netw','are Module (libc)'#010+
  '3*2Topenbsd_OpenBSD'#010+
  '3*2Tos2_OS/2 / eComStation'#010+
  '3*2Tsunos_SunOS/Solaris'#010+
  '3*2Tsymbian_Symbian OS'#010+
  '3*2Twatcom_Watcom compatible DOS extender'#010+
  '3*2Twdosx_WDOSX DOS extender'#010+
  '3*2Twin32_Windows 32 Bit'#010+
  '3*2Twince_Windows CE'#010+
  '4*2Tlinux_L','inux'#010+
  '6*2Tamiga_Commodore Amiga'#010+
  '6*2Tatari_Atari ST/STe/TT'#010+
  '6*2Tlinux_Linux/m68k'#010+
  '6*2Tmacos_Macintosh m68k (not supported)'#010+
  '6*2Tpalmos_PalmOS'#010+
  'A*2Tlinux_Linux'#010+
  'A*2Twince_Windows CE'#010+
  'P*2Tamiga_AmigaOS on PowerPC'#010+
  'P*2Tdarwin_Darwin and Mac OS X on Pow','erPC'#010+
  'P*2Tlinux_Linux on PowerPC'#010+
  'P*2Tmacos_Mac OS (classic) on PowerPC'#010+
  'P*2Tmorphos_MorphOS'#010+
  'S*2Tlinux_Linux'#010+
  '**1u<x>_Undefines the symbol <x>'#010+
  '**1U_Unit options:'#010+
  '**2Un_Do not check where the unit name matches the file name'#010+
  '**2Ur_Generate releas','e unit files (never automatically recompiled)'#010+
  '**2Us_Compile a system unit'#010+
  '**1v<x>_Be verbose. <x> is a combination of the following letters:'#010+
  '**2*_e : Show errors (default)       0 : Show nothing (except errors)'#010+
  '**2*_w : Show warnings       ','        u : Show unit info'#010+
  '**2*_n : Show notes                  t : Show tried/used files'#010+
  '**2*_h : Show hints                  c : Show conditionals'#010+
  '**2*_i : Show general info           d : Show debug info'#010+
  '**2*_l : Show linenumbers         ','   r : Rhide/GCC compatibility mod'+
  'e'#010+
  '**2*_a : Show everything             x : Executable info (Win32 only)'#010+
  '**2*_b : Write file names messages with full path'#010+
  '**2*_v : Write fpcdebug.txt with     p : Write tree.log with parse tre'+
  'e'#010+
  '**2*_    lot','s of debugging info      q : Show message numbers'#010+
  '**2*_m<x>,<y> : Don'#039't show messages numbered <x> and <y>'#010+
  '3*1W<x>_Target-specific options (targets)'#010+
  'A*1W<x>_Target-specific options (targets)'#010+
  'P*1W<x>_Target-specific options (targets)'#010+
  '3*2Wb_C','reate a bundle instead of a library (Darwin)'#010+
  'P*2Wb_Create a bundle instead of a library (Darwin)'#010+
  'p*2Wb_Create a bundle instead of a library (Darwin)'#010+
  '3*2WB_Create a relocatable image (Windows)'#010+
  'A*2WB_Create a relocatable image (Windows, Symbi','an)'#010+
  '3*2WC_Specify console type application (EMX, OS/2, Windows)'#010+
  'A*2WC_Specify console type application (Windows)'#010+
  'P*2WC_Specify console type application (Classic Mac OS)'#010+
  '3*2WD_Use DEFFILE to export functions of DLL or EXE (Windows)'#010+
  'A*2WD_Use',' DEFFILE to export functions of DLL or EXE (Windows)'#010+
  '3*2We_Use external resources (Darwin)'#010+
  'P*2We_Use external resources (Darwin)'#010+
  'p*2We_Use external resources (Darwin)'#010+
  '3*2WF_Specify full-screen type application (EMX, OS/2)'#010+
  '3*2WG_Specify grap','hic type application (EMX, OS/2, Windows)'#010+
  'A*2WG_Specify graphic type application (Windows)'#010+
  'P*2WG_Specify graphic type application (Classic Mac OS)'#010+
  '3*2Wi_Use internal resources (Darwin)'#010+
  'P*2Wi_Use internal resources (Darwin)'#010+
  'p*2Wi_Use interna','l resources (Darwin)'#010+
  '3*2WN_Do not generate relocation code, needed for debugging (Windows)'#010+
  'A*2WN_Do not generate relocation code, needed for debugging (Windows)'#010+
  '3*2WR_Generate relocation code (Windows)'#010+
  'A*2WR_Generate relocation code (Window','s)'#010+
  'P*2WT_Specify MPW tool type application (Classic Mac OS)'#010+
  '**1X_Executable options:'#010+
  '**2Xc_Pass --shared/-dynamic to the linker (BeOS, Darwin, FreeBSD, Lin'+
  'ux)'#010+
  '**2Xd_Do not use standard library search path (needed for cross compil'+
  'e)'#010+
  '**2Xe_Us','e external linker'#010+
  '**2Xg_Create debuginfo in a separate file and add a debuglink section '+
  'to executable'#010+
  '**2XD_Try to link units dynamically      (defines FPC_LINK_DYNAMIC)'#010+
  '**2Xi_Use internal linker'#010+
  '**2Xm_Generate link map'#010+
  '**2XM<x>_Set the nam','e of the '#039'main'#039' program routine (default'+
  ' is '#039'main'#039')'#010+
  '**2XP<x>_Prepend the binutils names with the prefix <x>'#010+
  '**2Xr<x>_Set library search path to <x> (needed for cross compile) (Be'+
  'OS, Linux)'#010+
  '**2XR<x>_Prepend <x> to all linker search paths (Be','OS, Darwin, FreeB'+
  'SD, Linux, Mac OS, Solaris)'#010+
  '**2Xs_Strip all symbols from executable'#010+
  '**2XS_Try to link units statically (default, defines FPC_LINK_STATIC)'#010+
  '**2Xt_Link with static libraries (-static is passed to linker)'#010+
  '**2XX_Try to smartlink',' units             (defines FPC_LINK_SMART)'#010+
  '**1*_'#010+
  '**1?_Show this help'#010+
  '**1h_Shows this help without waiting'#000
);
