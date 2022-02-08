module ErorsT where 

newtype TT h f a = TT (f (h a))

newtype ErorsT e f a = ErorsT {runErorsT :: f (Either e a)}

instance Functor f => Functor (ErorsT e f) where
    fmap g (ErorsT fa) = ErorsT $ fmap (fmap g) fa

instance (Applicative f, Monoid e) => Applicative (ErorsT e f) where
    pure = ErorsT . pure . Right

    (ErorsT fg) <*> (ErorsT fa) =
            let yy = fmap (either (\e -> Left . either (e <>) (const e)) fmap) fg
            in ErorsT $ yy <*> fa

        -- let yy = fmap (\eg -> 
        --             case eg of 
        --                 Left e1 -> \x ->
        --                     case x of
        --                         Left e2 -> Left (e1 <> e2)
        --                         Right a -> Left e1
        --                 Right g -> \x ->
        --                     case x of
        --                         Left e2 -> Left e2
        --                         Right a -> Right (g a) ) fg
        -- in ErorsT $ yy <*> fa


-- instance Applicative (Either e) where
--     pure          = Right
--     Left  e <*> _ = Left e
--     Right f <*> r = fmap f r

data Sum a b = L a | R b

instance Functor (Sum a) where
    fmap f (L a) = L a
    fmap f (R b) = R (f b)

instance Monoid e => Applicative (Sum e) where
    pure a = R a
    L e1 <*> L e2 = L (e1 <> e2)  -- here difference with Applicative (Either e)
    L e  <*> R a  = L e
    R f  <*> L e  = L e
    R f  <*> R a  = R (f a)

    -- identity ::     pure id <*> v = v
    -- Right id <*> v = v ✔
    -- composition ::  pure (.) <*> u <*> v <*> w = u <*> (v <*> w) ✔
    -- homomorphsim :: pure f <*> pure x = pure (f x)
    -- interchange  :: u <*> pure y = pure ($ y) <*> u
    
    

failing :: String -> IO (Either String Int)
failing err = do 
    return (Left err)

success :: Int -> IO (Either String Int)
success n = do
    print "Working... "
    return (Right n)

main = do
    either_ab <- runErorsT $ (,) <$> ErorsT (failing "Ups! ") <*> ErorsT (failing "Not again!")
    either print print either_ab
    either_ab <- runErorsT $ (,) <$> ErorsT (failing "Ups! ") <*> ErorsT (success 1)
    either print print either_ab
    either_ab <- runErorsT $ (,) <$> ErorsT (success 1   )    <*> ErorsT (failing "Close..")
    either print print either_ab
    either_ab <- runErorsT $ (,) <$> ErorsT (success 1   )    <*> ErorsT (success 2)
    either print print either_ab