import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

-- Definindo os tipos de dados para a AST
data Program = Program FunctionList MainBlock
  deriving (Show)

data FunctionList = FunctionList Function FunctionList | EmptyFunctionList
  deriving (Show)

data Function = Function ReturnType String ParameterList MainBlock
  deriving (Show)

data ReturnType = IntType | StringType | DoubleType | VoidType
  deriving (Show)

data ParameterList = ParameterList Type String ParameterList | EmptyParameterList
  deriving (Show)

data Type = IntType | StringType | DoubleType
  deriving (Show)

data MainBlock = MainBlock DeclarationList CommandList
  deriving (Show)

data DeclarationList = DeclarationList Type [String] DeclarationList | EmptyDeclarationList
  deriving (Show)

data CommandList = CommandList Command CommandList | EmptyCommandList
  deriving (Show)

data Command
  = ReturnStatement MaybeExpression
  | IfStatement Expression MainBlock (Maybe MainBlock)
  | WhileStatement Expression MainBlock
  | Assignment String Expression
  | PrintStatement Expression
  | ReadStatement String
  | FunctionCall String ArgumentList
  deriving (Show)

data MaybeExpression = JustExpression Expression | NoExpression
  deriving (Show)

data Expression
  = RelationalExpression RelationalOperator ArithmeticExpression ArithmeticExpression
  | LogicalExpression LogicalOperator Expression Expression
  | UnaryLogicalExpression LogicalOperator Expression
  | ArithmeticExpression
  deriving (Show)

data RelationalOperator = Less | Greater | LessOrEqual | GreaterOrEqual | Equal | NotEqual
  deriving (Show)

data LogicalOperator = And | Or
  deriving (Show)

data ArgumentList = ArgumentList Expression ArgumentList | EmptyArgumentList
  deriving (Show)

-- Configuração do analisador léxico
lingDef = emptyDef
  { Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = "--"
  , Tok.reservedOpNames = ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "/=", "&&", "||", "!", "=", ",", ";", "{", "}", "(", ")", "return", "if", "else", "while", "print", "read"]
  , Tok.reservedNames = ["int", "string", "double", "void"]
  }

lexico = Tok.makeTokenParser lingDef

natural = Tok.natural lexico
reservedOp = Tok.reservedOp lexico
reserved = Tok.reserved lexico
parens = Tok.parens lexico
identifier = Tok.identifier lexico

-- Parser para tipos (int, string, double)
typeParser :: Parser Type
typeParser = choice
  [ reserved "int" >> return IntType
  , reserved "string" >> return StringType
  , reserved "double" >> return DoubleType
  ]

-- Parser para tipos de retorno (int, string, double, void)
returnTypeParser :: Parser ReturnType
returnTypeParser = choice
  [ reserved "int" >> return IntType
  , reserved "string" >> return StringType
  , reserved "double" >> return DoubleType
  , reserved "void" >> return VoidType
  ]

-- Parser para expressões aritméticas
arithmeticExpressionParser :: Parser Expression
arithmeticExpressionParser = buildExpressionParser arithOperators arithmeticTerm

arithOperators = [ [Infix (reservedOp "*" >> return (BinaryOp Mul)) AssocLeft, Infix (reservedOp "/" >> return (BinaryOp Div)) AssocLeft]
                 , [Infix (reservedOp "+" >> return (BinaryOp Add)) AssocLeft, Infix (reservedOp "-" >> return (BinaryOp Sub)) AssocLeft]
                 ]

arithmeticTerm :: Parser Expression
arithmeticTerm = parens arithmeticExpressionParser <|> try (Const <$> natural)

-- Parser para expressões relacionais
relationalExpressionParser :: Parser Expression
relationalExpressionParser = buildExpressionParser relOperators relationalTerm

relOperators = [ [Infix (reservedOp "<" >> return (BinaryOp Less)) AssocNone, Infix (reservedOp ">" >> return (BinaryOp Greater)) AssocNone
                , Infix (reservedOp "<=" >> return (BinaryOp LessOrEqual)) AssocNone, Infix (reservedOp ">=" >> return (BinaryOp GreaterOrEqual)) AssocNone]
              , [Infix (reservedOp "==" >> return (BinaryOp Equal)) AssocNone, Infix (reservedOp "/=" >> return (BinaryOp NotEqual)) AssocNone]
              ]

relationalTerm :: Parser Expression
relationalTerm = parens relationalExpressionParser <|> try (RelationalExpression <$> arithmeticExpressionParser <*> reservedOp "<" <*> arithmeticExpressionParser)

-- Parser para expressões lógicas
logicalExpressionParser :: Parser Expression
logicalExpressionParser = buildExpressionParser logicalOperators logicalTerm

logicalOperators = [ [Infix (reservedOp "&&" >> return (BinaryOp And)) AssocLeft]
                  , [Infix (reservedOp "||" >> return (BinaryOp Or)) AssocLeft]
                  ]

logicalTerm :: Parser Expression
logicalTerm = parens logicalExpressionParser <|> try (LogicalExpression <$> relationalExpressionParser <*> reservedOp "&&" <*> relationalExpressionParser)

-- Parser para declarações de variáveis
declarationParser :: Parser DeclarationList
declarationParser = do
  t <- typeParser
  ids <- identifier `sepBy1` reservedOp ","
  reservedOp ";"
  rest <- optionMaybe declarationParser
  return (DeclarationList t ids rest)

-- Parser para comandos (return, if, while, atribuição, print, read, chamada de função)
commandParser :: Parser Command
commandParser =
  try (ReturnStatement <$> (reserved "return" >> option NoExpression tvzExpressionParser <* reservedOp ";")) <|>
  try (IfStatement <$> (reserved "if" >> parens logicalExpressionParser) <*> blockParser <*> optionMaybe (reserved "else" >> blockParser)) <|>
  try (WhileStatement <$> (reserved "while" >> parens logicalExpressionParser) <*> blockParser) <|>
  try (Assignment <$> identifier <* reservedOp "=" <*> expressionParser <* reservedOp ";") <|>
  try (PrintStatement <$> (reserved "print" >> parens expressionParser <* reservedOp ";")) <|>
  try (ReadStatement <$> (reserved "read" >> parens identifier <* reservedOp ";")) <|>
  FunctionCall <$> identifier <* parens argumentListParser

-- Parser para a lista de comandos
commandListParser :: Parser CommandList
commandListParser = CommandList <$> commandParser <*> commandListParser <|> return EmptyCommandList

-- Parser para a lista de parâmetros
parameterListParser :: Parser ParameterList
parameterListParser = ParameterList <$> typeParser <*> identifier <* reservedOp "," <*> parameterListParser <|> EmptyParameterList

-- Parser para uma função
functionParser :: Parser Function
functionParser = do
  rt <- returnTypeParser
  name <- identifier
  params <- parens parameterListParser
  block <- blockParser
  return (Function rt name params block)

-- Parser para o programa principal
mainBlockParser :: Parser MainBlock
mainBlockParser = reservedOp "{" >> MainBlock <$> declarationListParser <*> commandListParser <* reservedOp "}"

-- Parser para o programa completo
programParser :: Parser Program
programParser = Program <$> functionListParser <*> mainBlockParser

-- Parser para a lista de funções
functionListParser :: Parser FunctionList
functionListParser = FunctionList <$> functionParser <*> functionListParser <|> return EmptyFunctionList

-- Parser para uma expressão
expressionParser :: Parser Expression
expressionParser = try logicalExpressionParser <|> try relationalExpressionParser <|> arithmeticExpressionParser

-- Parser para uma expressão com a possibilidade de ser vazia
tvzExpressionParser :: Parser MaybeExpression
tvzExpressionParser = JustExpression <$> expressionParser <|> return NoExpression

-- Parser para uma lista de argumentos
argumentListParser :: Parser ArgumentList
argumentListParser = ArgumentList <$> expressionParser <*> (reservedOp "," >> argumentListParser) <|> return EmptyArgumentList

-- Parser para uma lista de declarações
declarationListParser :: Parser DeclarationList
declarationListParser = DeclarationList <$> typeParser <*> (identifier `sepBy1` reservedOp ",") <* reservedOp ";" <*> declarationListParser <|> EmptyDeclarationList

-- Parser para um bloco de comandos
blockParser :: Parser MainBlock
blockParser = reservedOp "{" >> MainBlock <$> declarationListParser <*> commandListParser <* reservedOp "}"

main :: IO ()
main = do
  putStr "Expressão: "
  input <- getLine
  case parse programParser "" input of
    Left err -> print err
    Right result -> print result
