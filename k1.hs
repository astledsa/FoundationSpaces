data Type
        = Atom String
        | Arrow Type Type
        deriving (Show, Eq)

data Term
        = Var String
        | Lam String Type Term
        | App Term Term
        deriving (Show, Eq)

type Context = [(String, Type)]

search :: Context -> String -> Type
search [] var = error ("unbound variable: " ++ var)
search ((k, v) : ctx) key
        | key == k = v
        | otherwise = search ctx key

extractright :: Type -> Type
extractright (Atom _) = error "Cannot be used on atoms"
extractright (Arrow _ r) = r

infer :: Term -> Context -> Type
infer term ctx = case term of
        Var var -> search ctx var
        Lam var t term -> let
                new_ctx = (var, t) : ctx
                body_type = infer term new_ctx
                in Arrow t body_type
        App fn arg -> let
                fn_type = infer fn ctx
                arg_type = infer arg ctx
                in extractright fn_type
