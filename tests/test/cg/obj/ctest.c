/*
  Program to test linking between C and pascal units.
  Copyright (c) 2002, Carl Eric Codere
*/

unsigned char global_u8bit;
unsigned short global_u16bit;
unsigned long global_u32bit;
unsigned long long global_s64bit;

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

/*
  $Log$
  Revision 1.2  2002-04-22 19:09:12  carl
  + added structure testing

  Revision 1.1  2002/04/13 21:06:39  carl
  + c module testing

*/