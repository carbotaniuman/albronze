#ifndef __STDC_STDDEF_H
#define __STDC_STDDEF_H
typedef long ptrdiff_t;
typedef unsigned long size_t;
typedef long max_align_t;
// Unicode is at most 32-bits per character
typedef int wchar_t;

#define NULL 0
#define offsetof(type, member) (offsetof not_currently_supported)
#endif
