#define A(x, y) x##y
#define B(x, y) A(x, y)

#define FOO 3
#define BAR 6

A(FOO, BAR)
B(FOO, BAR)
