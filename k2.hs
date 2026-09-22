data Expr =
        Ref String
        | Level Int
        | FuncBn String Expr Expr
        | Lambda String Expr Expr
        | Apply Expr Expr
        deriving(Show, Eq)

type Bindings = [(String, String)]
type BinderEnv = [(String, Expr)]

search :: BinderEnv -> String -> Expr
search [] s = error ("No reference found for " ++ s)
search ((k, v) : benv) key
        | k == key = v
        | otherwise = search benv key

co :: [String] -> String -> Expr -> Bool
co boundvars name expr = case expr of
        Ref s -> (name == s) && (s `notElem` boundvars)
        Level n -> False
        FuncBn s e1 e2 -> co boundvars name e1 || co (s : boundvars) name e2
        Lambda s e1 e2 -> co boundvars name e1 || co (s : boundvars) name e2
        Apply e1 e2 -> co boundvars name e1 || co boundvars name e2

checkoccurence :: String -> Expr -> Bool
checkoccurence = co []

checkLvl :: Expr -> Expr
checkLvl expr = case expr of
        Level n -> Level n
        _ -> error "Must be a level"

maxLvl :: Expr -> Expr -> Expr
maxLvl expr1 expr2 = case expr1 of
        Level i -> case expr2 of
                Level j -> if i > j then Level i else Level j
                _ -> error "Expression must be a level"
        _ -> error "Expression must be a level"

deepreplace :: Expr -> String -> String -> Bool -> Expr
deepreplace expr tochange replacement isbound = case expr of
        Level n -> Level n
        Ref str -> if str == tochange && isbound then Ref replacement else Ref str
        FuncBn name i o ->
                if name == tochange then
                        if not isbound then
                                FuncBn replacement
                                        i
                                        (deepreplace o tochange replacement True)
                        else
                                FuncBn name
                                        (deepreplace i tochange replacement True)
                                        o
                else
                        FuncBn name
                                (deepreplace i tochange replacement isbound)
                                (deepreplace o tochange replacement isbound)
        Lambda name i o ->
                if name == tochange then
                        if not isbound then
                                Lambda replacement
                                        i
                                        (deepreplace o tochange replacement True)
                        else
                                Lambda name
                                        (deepreplace i tochange replacement True)
                                        o
                else
                        Lambda name
                                (deepreplace i tochange replacement isbound)
                                (deepreplace o tochange replacement isbound)
        Apply e1 e2 -> Apply
                        (deepreplace e1 tochange replacement isbound)
                        (deepreplace e2 tochange replacement isbound)

substitute :: Expr -> String -> Expr -> Expr
substitute expr target replacemant = case expr of
        Level n -> Level n
        Ref str -> if str == target then replacemant else Ref str
        FuncBn name expr1 expr2 -> let
                inputexpr = substitute expr1 target replacemant
                in if name == target then
                        FuncBn name inputexpr expr2
                else
                        if checkoccurence name replacemant then
                                substitute
                                        (deepreplace (FuncBn name expr1 expr2) name "y1" False)
                                        target
                                        replacemant
                        else
                                FuncBn name inputexpr (substitute expr2 target replacemant)
        Lambda name expr1 expr2 -> let
                inputexpr = substitute expr1 target replacemant
                in if name == target then
                        Lambda name inputexpr expr2
                else
                        if checkoccurence name replacemant then
                                substitute
                                        (deepreplace (Lambda name expr1 expr2) name "y1" False)
                                        target
                                        replacemant
                        else
                                Lambda name inputexpr (substitute expr2 target replacemant)
        Apply expr1 expr2 -> Apply
                (substitute expr1 target replacemant)
                (substitute expr2 target replacemant)

checkBindings :: Bindings -> Expr -> Expr -> Bool
checkBindings [] s1 s2 = s1 == s2
checkBindings ((r1, r2) : bindings) (Ref s1) (Ref s2)
        | (r1 == s1) && (r2 == s2) = True
        | (r1 == s1) && (r2 /= s2) = False
        | (r1 /= s1) && (r2 == s2) = False
        | otherwise = checkBindings bindings (Ref s1) (Ref s2)

check :: BinderEnv -> Expr -> Expr -> Bool
check benv expr = equal (infer benv expr)

reduce :: Expr -> Expr
reduce expr = case expr of
        Lambda n e1 e2 -> Lambda n (reduce e1) (reduce e2)
        FuncBn n e1 e2 -> FuncBn n (reduce e1) (reduce e2)
        Apply f a -> case reduce f of
                Lambda x _ body -> reduce (substitute body x a)
                f' -> Apply f' (reduce a)
        _ -> expr

eq :: Bindings -> Expr -> Expr -> Bool
eq bindings (Level n) (Level m) = n == m
eq bindings (FuncBn n e1 e2) (FuncBn m e3 e4) =
        eq bindings e1 e3 &&
        eq ((n, m) : bindings) e2 e4
eq bindings (Lambda n e1 e2) (Lambda m e3 e4) =
        eq bindings e1 e3 &&
        eq ((n, m) : bindings) e2 e4
eq bindings (Apply e1 e2) (Apply e3 e4) =
        eq bindings e1 e3 &&
        eq bindings e2 e4
eq bindings (Ref s1) (Ref s2) =
        checkBindings bindings (Ref s1) (Ref s2)
eq _ _ _ = False

equal :: Expr -> Expr -> Bool
equal expr1 expr2 = let
        e1 = reduce expr1
        e2 = reduce expr2
        in eq [] e1 e2

infer :: BinderEnv -> Expr -> Expr
infer benv expr = case expr of
        Ref str -> search benv str
        Level n -> Level (n+1)
        FuncBn name iexpr oexpr -> let
                iexprType = checkLvl (infer benv iexpr)
                newbenv = (name, iexpr) : benv
                oexprType = checkLvl (infer newbenv oexpr)
                in maxLvl iexprType oexprType
        Lambda name iexpr bexpr -> case checkLvl (infer benv iexpr) of
                Level _ -> let
                        newbenv = (name, iexpr) : benv
                        bexprType = infer newbenv bexpr
                        in FuncBn name iexpr bexprType
        Apply expr1 expr2 -> let
                intermediate = infer benv expr1
                f = reduce intermediate
                a = infer benv expr2
                in case f of
                        FuncBn name i o ->
                                if i `equal` a then
                                        substitute o name expr2
                                else
                                        error (
                                                show intermediate ++ " " ++
                                                "Input " ++ show i ++
                                                " Output " ++ show a ++
                                                " types not compatible"
                                        )
                        _ -> error "FunctionBinder not returned"
