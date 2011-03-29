#ifdef EXCEPTION_HANDLING
#define TYPE_EXCEPTION type(exception)
#define TYPE_EXCEPTION_ARGUMENT type(exception), intent(out)
#else
#define TYPE_EXCEPTION integer
#define TYPE_EXCEPTION_ARGUMENT integer, intent(in out)
#endif