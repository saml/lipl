module PPrint where

data ISeq = -- Indented sequence to pretty print
    INil
    | IStr String
    | IAppend ISeq ISeq
    | IIndent ISeq
    | INewline

indent :: ISeq -> ISeq
indent seq = seq

newline :: ISeq -> ISeq
newline = IStr "\n"

flatten :: Int -> [(ISeq, Int)] -> String
flatten i [] = ""
flatten i (INil:xs) = flatten xs
flatten i (IStr s:xs) = s ++ (flatten xs)
flatten i (IAppend s1 s2:xs) = flatten (s1 : s2 : xs)
flatten
display s = flatten 0 [(s, 0)]
