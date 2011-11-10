#ifdef ERROR_HANDLING
#define TYPE_ERROR type(error)
#define TYPE_ERROR_ARGUMENT type(error), intent(out)
#else
#define TYPE_ERROR integer
#define TYPE_ERROR_ARGUMENT integer, intent(in out)
#endif