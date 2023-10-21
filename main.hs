main :: IO ()
main = do
  putStr "Código a ser analisado: "
  input <- getContents
  case parse programParser "" input of
    Left err -> print err
    Right result -> print result
