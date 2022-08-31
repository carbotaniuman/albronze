#define ID(arg) arg
#define EMPTY
#define NOEXPAND(macro) macro EMPTY
#define F_AGAIN() F
#define F() f NOEXPAND(F_AGAIN)()()

ID(F())
