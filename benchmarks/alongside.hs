{-# LANGUAGE RankNTypes #-}
import           Control.Lens
import           Criterion.Main

compound :: Lens a b c d
         -> Lens a' b' c' d'
         -> Lens (a,a') (b,b') (c,c') (d,d')
compound l r = lens (\(a, a') -> (view l a, view r a'))
                    (\(a, a') (b, b') -> (set l b a, set r b' a'))
{-# INLINE compound #-}

-- BWAHAAHA
compound5 :: Lens a b c d
          -> Lens a' b' c' d'
          -> Lens a'' b'' c'' d''
          -> Lens a''' b''' c''' d'''
          -> Lens a'''' b'''' c'''' d''''
          -> Lens (a, (a', (a'', (a''', a''''))))
                  (b, (b', (b'', (b''', b''''))))
                  (c, (c', (c'', (c''', c''''))))
                  (d, (d', (d'', (d''', d''''))))
compound5 l l' l'' l''' l''''
  = lens (\(a, (a', (a'', (a''', a''''))))
           -> (view l a, (view l' a', (view l'' a'', (view l''' a''', view l'''' a'''')))) )
         (\(a, (a', (a'', (a''', a'''')))) (b, (b', (b'', (b''', b''''))))
           -> (set l b a, (set l' b' a', (set l'' b'' a'', (set l''' b''' a''', set l'''' b'''' a'''')))) )

main = defaultMain
    [ bench "alongside1" $ nf (view $ alongside _1 _2) (("hi", 1), (2, "there!"))
    , bench "compound1"  $ nf (view $ compound _1 _2) (("hi", 1), (2, "there!"))
    , bench "alongside5"  $ nf (view $ (alongside _1 (alongside _1 (alongside _1 (alongside _1 _1)))))
      ((v,v),((v,v),((v,v),((v, v),(v,v)))))
    , bench "compound5"  $ nf (view $ compound5 _1 _1 _1 _1 _1)
        ((v,v),((v,v),((v,v),((v, v),(v,v)))))
    ]
  where v = 1 :: Int
