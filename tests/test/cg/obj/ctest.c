/*
  Program to test linking between C and pascal units.
  Copyright (c) 2002, Carl Eric Codere
*/

/*
   Note : Arrays seem to always be passed by reference
   in the C language. Therefore, no testing is required
   to use them.
*/

unsigned char global_u8bit;
unsigned short global_u16bit;
unsigned long global_u32bit;
unsigned long long global_s64bit;
float global_float;
double global_double;
long double global_long_double;

#define   RESULT_U8BIT    0x55
#define   RESULT_U16BIT   0x500F
#define   RESULT_U32BIT   0x500F0000
#define   RESULT_S64BIT  -12000
#define   RESULT_FLOAT   14.54
#define   RESULT_PCHAR   "Hello world"
#define	  RESULT_LONGDOUBLE  RESULT_FLOAT
#define   RESULT_DOUBLE      RESULT_FLOAT


struct _3BYTE_
{
	unsigned char  u8;
	unsigned short u16;
};

struct _7BYTE_
{
	unsigned char u8;
	long long s64;
	unsigned short u16;
};


struct _7BYTE_ test_struct;


/* simple parameter testing */
void test_param_u8(unsigned char v)
{
  global_u8bit = v;
}


void test_param_u16(unsigned short v)
{
  global_u16bit = v;
}

void test_param_u32(unsigned long v)
{
  global_u32bit = v;
}


void test_param_s64(long long v)
{
  global_s64bit = v;
}

void test_param_float(float v)
{
  global_float = v;
}

void test_param_double(double v)
{
  global_double = v;
}


void test_param_longdouble(long double v)
{
  global_long_double = v;
}

/* if this one works, others should also automatically */
void test_param_var_u8(unsigned char *x)
{
	*x = RESULT_U8BIT;
}


/* mixed parameter testing */
void test_param_mixed_u16(unsigned char z, unsigned short x, unsigned char y)
{
	global_u16bit = x;
	global_u8bit = y;
}

void test_param_mixed_u32(unsigned char z, unsigned long x, unsigned char y)
{
	global_u32bit = x;
	global_u8bit = y;
}

void test_param_mixed_s64(unsigned char z, long long x, unsigned char y)
{
	global_s64bit = x;
	global_u8bit = y;
}

/* simple record testing */
void test_param_struct_small(struct _3BYTE_ buffer)
{
	global_u8bit = buffer.u8;
	global_u16bit = buffer.u16;
}

void test_param_struct_large(struct _7BYTE_ buffer)
{
	global_u8bit = buffer.u8;
	global_u16bit = buffer.u16;
	global_s64bit = buffer.s64;
}


/* function result testing */
unsigned char test_function_u8()
{
	return RESULT_U8BIT;
}

unsigned short test_function_u16()
{
	return RESULT_U16BIT;
}

unsigned long test_function_u32()
{
	return RESULT_U32BIT;
}

unsigned long long test_function_s64()
{
	return RESULT_S64BIT;
}

char* test_function_pchar()
{
	return RESULT_PCHAR;
}

float test_function_float()
{
	return RESULT_FLOAT;
}

double test_function_double()
{
	return RESULT_DOUBLE;
}

long double test_function_longdouble()
{
	return RESULT_LONGDOUBLE;
}

struct _7BYTE_ test_function_struct()
{
	test_struct.u8 = RESULT_U8BIT;
	test_struct.s64 = RESULT_S64BIT;
	test_struct.u16 = RESULT_U16BIT;
	return test_struct;
}

/*
  $Log$
  Revision 1.4  2002-09-07 15:40:56  peter
    * old logs removed and tabs fixed

  Revision 1.3  2002/05/04 16:57:23  carl
  + var parameter testing
  + function result testing
  + floating point testing

  Revision 1.2  2002/04/22 19:09:12  carl
  + added structure testing

  Revision 1.1  2002/04/13 21:06:39  carl
  + c module testing

*/
