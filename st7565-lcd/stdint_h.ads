with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package stdint_h is

  -- Copyright (C) 1997,1998,1999,2000,2001,2006 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, write to the Free
  --   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
  --   02111-1307 USA.   

  -- *	ISO C99: 7.18 Integer types <stdint.h>
  --  

  -- Exact integral types.   
  -- Signed.   
  -- There is some amount of overlap with <sys/types.h> as known by inet code  
   subtype int8_t is signed_char;  -- /usr/include/stdint.h:37

   subtype int16_t is short;  -- /usr/include/stdint.h:38

   subtype int32_t is int;  -- /usr/include/stdint.h:39

   subtype int64_t is Long_Long_Integer;  -- /usr/include/stdint.h:44

  -- Unsigned.   
   subtype uint8_t is unsigned_char;  -- /usr/include/stdint.h:49

   subtype uint16_t is unsigned_short;  -- /usr/include/stdint.h:50

   subtype uint32_t is unsigned;  -- /usr/include/stdint.h:52

   subtype uint64_t is Extensions.unsigned_long_long;  -- /usr/include/stdint.h:59

  -- Small types.   
  -- Signed.   
   subtype int_least8_t is signed_char;  -- /usr/include/stdint.h:66

   subtype int_least16_t is short;  -- /usr/include/stdint.h:67

   subtype int_least32_t is int;  -- /usr/include/stdint.h:68

   subtype int_least64_t is Long_Long_Integer;  -- /usr/include/stdint.h:73

  -- Unsigned.   
   subtype uint_least8_t is unsigned_char;  -- /usr/include/stdint.h:77

   subtype uint_least16_t is unsigned_short;  -- /usr/include/stdint.h:78

   subtype uint_least32_t is unsigned;  -- /usr/include/stdint.h:79

   subtype uint_least64_t is Extensions.unsigned_long_long;  -- /usr/include/stdint.h:84

  -- Fast types.   
  -- Signed.   
   subtype int_fast8_t is signed_char;  -- /usr/include/stdint.h:91

   subtype int_fast16_t is int;  -- /usr/include/stdint.h:97

   subtype int_fast32_t is int;  -- /usr/include/stdint.h:98

   subtype int_fast64_t is Long_Long_Integer;  -- /usr/include/stdint.h:100

  -- Unsigned.   
   subtype uint_fast8_t is unsigned_char;  -- /usr/include/stdint.h:104

   subtype uint_fast16_t is unsigned;  -- /usr/include/stdint.h:110

   subtype uint_fast32_t is unsigned;  -- /usr/include/stdint.h:111

   subtype uint_fast64_t is Extensions.unsigned_long_long;  -- /usr/include/stdint.h:113

  -- Types for `void *' pointers.   
   subtype intptr_t is int;  -- /usr/include/stdint.h:126

   subtype uintptr_t is unsigned;  -- /usr/include/stdint.h:129

  -- Largest integral types.   
   subtype intmax_t is Long_Long_Integer;  -- /usr/include/stdint.h:139

   subtype uintmax_t is Extensions.unsigned_long_long;  -- /usr/include/stdint.h:141

  -- The ISO C99 standard specifies that in C++ implementations these
  --   macros should only be defined if explicitly requested.   

  -- Limits of integral types.   
  -- Minimum of signed integral types.   
  -- Maximum of signed integral types.   
  -- Maximum of unsigned integral types.   
  -- Minimum of signed integral types having a minimum size.   
  -- Maximum of signed integral types having a minimum size.   
  -- Maximum of unsigned integral types having a minimum size.   
  -- Minimum of fast signed integral types having a minimum size.   
  -- Maximum of fast signed integral types having a minimum size.   
  -- Maximum of fast unsigned integral types having a minimum size.   
  -- Values to test for integral types holding `void *' pointer.   
  -- Minimum for largest signed integral type.   
  -- Maximum for largest signed integral type.   
  -- Maximum for largest unsigned integral type.   
  -- Limits of other integer types.   
  -- Limits of `ptrdiff_t' type.   
  -- Limits of `sig_atomic_t'.   
  -- Limit of `size_t' type.   
  -- Limits of `wchar_t'.   
  -- These constants might also be defined in <wchar.h>.   
  -- Limits of `wint_t'.   
  -- The ISO C99 standard specifies that in C++ implementations these
  --   should only be defined if explicitly requested.   

  -- Signed.   
  -- Unsigned.   
  -- Maximal type.   
end stdint_h;
