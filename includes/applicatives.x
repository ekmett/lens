#ifdef USE_RULES

{-# RULES Y(Identity) #-}
{-# RULES Y([]) #-}
{-# RULES Y(Maybe) #-}
{-# RULES Y((Either r)) #-}

#define X(Mon) Y((Accessor Mon))
#include "monoids.x"
#undef X

#define X(Mon) Y((Const Mon))
#include "monoids.x"
#undef X

#define X(Mon) Y(((,) Mon))
#include "monoids.x"
#undef X

#endif
