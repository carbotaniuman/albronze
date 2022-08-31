#define FOO 5
#define X(x) #x

X(p = "foo\n")
X(FOO)


#define A() 1 B
#define B() 1 A
#define Y(x) X(x)

Y(A()()()())

