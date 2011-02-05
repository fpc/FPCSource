//---------------------------------------------------------------------------------
{
//---------------------------------------------------------------------------------
// libgba compatible section macros
//---------------------------------------------------------------------------------
#define ITCM_CODE	__attribute__((section(".itcm"), long_call))

#define DTCM_DATA	__attribute__((section(".dtcm")))
#define DTCM_BSS	__attribute__((section(".sbss")))
#define ALIGN(m)	__attribute__((aligned (m)))

#define PACKED __attribute__ ((packed))
#define packed_struct struct PACKED
}
//---------------------------------------------------------------------------------
// These are linked to the bin2o macro in the Makefile
//---------------------------------------------------------------------------------
{ ### To do
#define GETRAW(name)      (name)
#define GETRAWSIZE(name)  ((int)name##_size)
#define GETRAWEND(name)  ((int)name##_end)
}

{$ifdef NDS_INTERFACE}
type
  (* Unsigned types *)
  uint8  = cuint8;
  uint16 = cuint16;
  uint32 = cuint32;
  uint64 = cuint64;
  puint8 = pcuint8;
  puint16 = pcuint16;
  puint32 = pcuint32;
  puint64 = pcuint64;
  
  (* Signed types *)
  int8  = cint8;
  int16 = cint16;
  int32 = cint32;
  pint8 = pcint8;
  pint16 = pcint16;
  pint32 = pcint32;
  pint64 = pcint64;


  float32 = cfloat;

{
typedef volatile uint8          vuint8;
typedef volatile uint16         vuint16;
typedef volatile uint32         vuint32;
typedef volatile uint64         vuint64;

typedef volatile int8           vint8;
typedef volatile int16          vint16;
typedef volatile int32          vint32;
typedef volatile int64          vint64;

typedef volatile float32        vfloat32;
typedef volatile float64        vfloat64;
}


{
typedef volatile int32          vfixed;
}

  u8  = cuint8;
  u16 = cuint16;
  u32 = cuint32;
  u64 = cuint64;
  pu8 = pcuint8;
  pu16 = pcuint16;
  pu32 = pcuint32;
  pu64 = pcuint64;
  
  s8  = cint8;
  s16 = cint16;
  s32 = cint32;
  s64 = cint64;
  ps8 = pcint8;
  ps16 = pcint16;
  ps32 = pcint32;
  ps64 = pcint64;

  cbool = boolean;
  pcbool = ^cbool;
{
typedef volatile u8          vu8;
typedef volatile u16         vu16;
typedef volatile u32         vu32;
typedef volatile u64         vu64;

typedef volatile s8           vs8;
typedef volatile s16          vs16;
typedef volatile s32          vs32;
typedef volatile s64          vs64;
}


  //IntFn = function: pointer;
  VoidFn = function: pointer; cdecl;
  //fp = function: pointer;

  IntFn = procedure;
  //VoidFunctionPointer = procedure; cdecl;
  fp = procedure;
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION} 
function BIT(n: cint): cint; inline;
begin
  BIT := (1 shl (n));
end;
{$endif NDS_IMPLEMENTATION}


{$ifdef NDS_INTERFACE}
function BIT(n: cint): cint; inline;
{$endif NDS_INTERFACE}
