#define A(x, y) x##y
#define B(x, y) A(x, y)

#define FOO 3
#define BAR 6

A(FOO, BAR)
B(FOO, BAR)


#define SPACED1(x, y) x ## y
#define SPACED2(x, y, z) x ## y ## z

SPACED1(1, 2)
SPACED1(A, B)

SPACED2(1, 2, 3)
SPACED2(A, B, C)