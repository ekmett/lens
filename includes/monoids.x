#ifdef USE_RULES
{-# RULES X(Any) #-}
{-# RULES X(All) #-}
{-# RULES X([r]) #-}
{-# RULES X((Dual (Endo r))) #-}
{-# RULES X((Endo r)) #-}
{-# RULES X((Last r)) #-}
{-# RULES X((First r)) #-}
{-# RULES X((Leftmost r)) #-}
{-# RULES X((Rightmost r)) #-}
{-# RULES X((Sum Integer)) #-}
{-# RULES X((Sum Int)) #-}
{-# RULES X((Sum Float)) #-}
{-# RULES X((Sum Double)) #-}
{-# RULES X((Product Integer)) #-}
{-# RULES X((Product Int)) #-}
{-# RULES X((Product Float)) #-}
{-# RULES X((Product Double)) #-}
#endif
