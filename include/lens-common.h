#ifndef LENS_COMMON_H
#define LENS_COMMON_H

#if __GLASGOW_HASKELL__ >= 806
# define KVS(kvs) kvs
#else
# define KVS(kvs)
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

#ifndef MIN_VERSION_bytestring
#define MIN_VERSION_bytestring(x,y,z) 1
#endif

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif

#ifndef MIN_VERSION_exceptions
#define MIN_VERSION_exceptions 1
#endif

#ifndef MIN_VERSION_free
#define MIN_VERSION_free(x,y,z) 1
#endif

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

#ifndef MIN_VERSION_parallel
#define MIN_VERSION_parallel(x,y,z) (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL > 700)
#endif

#ifndef MIN_VERSION_reflection
#define MIN_VERSION_reflection(x,y,z) 1
#endif

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 1
#endif

#ifndef MIN_VERSION_vector
#define MIN_VERSION_vector(x,y,z) 1
#endif

#endif
