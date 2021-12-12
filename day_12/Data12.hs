module Data12 where

type Cave = String
type Connection = (Cave,Cave)



input :: [Connection]
input = input0 ++ map (\(x,y) -> (y,x)) input0
  where
    input0 = [("qi","UD"),("jt","br"),("wb","TF"),("VO","aa"),("UD","aa"),("br","end"),("end","HA"),("qi","br"),("br","HA"),("UD","start"),("TF","qi"),("br","hf"),("VO","hf"),("start","qi"),("end","aa"),("hf","HA"),("hf","UD"),("aa","hf"),("TF","hf"),("VO","start"),("wb","aa"),("UD","wb"),("KX","wb"),("qi","VO"),("br","TF")]
