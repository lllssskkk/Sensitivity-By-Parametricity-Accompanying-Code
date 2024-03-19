zero = \f -> (\x -> x);
suc = \n -> (\f -> (\x -> f ((n f) x)));
plus = \j -> (\k -> (\f -> (\x -> (j f) ((k f) x))));
-- const = \x -> \f -> x;
-- id = \x -> x;
-- main = print (const id id );
main = print (plus (suc zero) (suc (suc zero)));

-- main = print (const id id);


