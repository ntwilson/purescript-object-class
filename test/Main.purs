module Test.Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import ObjectClass (NoSubtype, ObjectClass, cast, forgetSubtype, instanceOf, new, subtype)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type ExceptionData r = (message :: String | r)
type Err r = ObjectClass r (ExceptionData ())

databaseException = Proxy :: Proxy "databaseException"
type DatabaseExceptionData r = (server :: String, database :: String | r)
type DatabaseException a b = Err (databaseException :: ObjectClass a (DatabaseExceptionData ()) | b)

newDatabaseException :: ∀ a b. { | ExceptionData + DatabaseExceptionData () } -> DatabaseException (NoSubtype a) b
newDatabaseException = new >>> subtype databaseException 


databaseConnectionException = Proxy :: Proxy "databaseConnectionException"
type DatabaseConnectionException a b c = DatabaseException (databaseConnectionException :: ObjectClass a () | b) c

newDatabaseConnectionException :: ∀ a b c.
  { | ExceptionData + DatabaseExceptionData () } -> DatabaseConnectionException (NoSubtype a) b c
newDatabaseConnectionException raw = new raw # (subtype databaseException <<< subtype databaseConnectionException)

databaseQueryException = Proxy :: Proxy "databaseQueryException"
type DatabaseQueryExceptionData r = (attemptedQuery :: String | r)
type DatabaseQueryException a b c = 
  DatabaseException (databaseQueryException :: ObjectClass a (DatabaseQueryExceptionData ()) | b) c

newDatabaseQueryException :: ∀ a b c.
  { | ExceptionData + DatabaseExceptionData + DatabaseQueryExceptionData () } -> DatabaseQueryException (NoSubtype a) b c
newDatabaseQueryException raw = new raw # (subtype databaseException <<< subtype databaseQueryException)
  
connErr :: ∀ a b c. DatabaseConnectionException (NoSubtype a) b c
connErr = newDatabaseConnectionException { server: "MSSQL03", database: "testing", message: "couldn't connect" }

queryErr :: ∀ a b c. DatabaseQueryException (NoSubtype a) b c
queryErr = newDatabaseQueryException { server: "MSSQL03", database: "testing", attemptedQuery: "SELECT * FROM debug_log", message: "timeout exceeded" }

errs :: Array (Exception _) 
errs = [connErr, queryErr]

spec :: Spec Unit
spec = do
  it "works with nested hierarchies" do
    let qryErr' = queryErr # cast databaseException # cast databaseQueryException

    qryErr'.message `shouldEqual` "timeout exceeded"
    qryErr'.server `shouldEqual` "MSSQL03"
    qryErr'.attemptedQuery `shouldEqual` "SELECT * FROM debug_log"

    Array.length (errs # Array.mapMaybe (instanceOf databaseException >=> instanceOf databaseConnectionException) <#> forgetSubtype)
      `shouldEqual` 1
    Array.length (errs # Array.mapMaybe (instanceOf databaseException >=> instanceOf databaseQueryException) <#> forgetSubtype)
      `shouldEqual` 1
    Array.length (errs # Array.mapMaybe (instanceOf databaseException) <#> forgetSubtype)
      `shouldEqual` 2



-- README


type Exception r = ObjectClass r ( 
  message :: String
)

httpException = Proxy :: Proxy "httpException"
type HTTPException r = Exception 
  ( httpException :: { statusCode :: Int }
  | r 
  ) 

missingKeyException = Proxy :: Proxy "missingKeyException"
type MissingKeyException key r = Exception 
  ( missingKeyException :: { key :: key }
  | r
  )

plainExn :: ∀ r. Exception (NoSubtype r)
plainExn = new { message: "this Exception has no subtype" } 

httpExn :: ∀ r. HTTPException r
httpExn = subtype httpException { message: "this Exception is an HTTPException at runtime", statusCode: 404 } 

keyExn :: ∀ r. MissingKeyException String r
keyExn = subtype missingKeyException { message: "this Exception is a MissingKeyException at runtime", key: "I'm missing" }

exns :: ∀ r. Array (ObjectClass ( httpException :: { statusCode :: Int }, missingKeyException :: { key :: String }, noSubtype :: Unit | r ) ( message :: String ))
exns = [ plainExn, httpExn, keyExn ]

exnMsgs :: Array String
exnMsgs = exns <#> _.message

exnHandler :: ∀ r. Exception (httpException :: { statusCode :: Int }, missingKeyException :: { key :: String } | r ) -> Effect Unit
exnHandler exn 
  | Just http <- instanceOf httpException exn = 
    log ("Got an HTTP Exception: (Status " <> show http.statusCode <> ") '" <> http.message <> "'")
  | Just key <- instanceOf missingKeyException exn = 
    log ("Got a Missing Key Exception while looking for key: '" <> show key.key <> "'.  '" <> key.message <> "'")
  | otherwise = 
    log ("Got some other kind of exception. '" <> exn.message <> "'")

statusCode :: Int
statusCode = 
  let http = cast httpException httpExn
  in http.statusCode

readmeSpec :: Spec Unit
readmeSpec = do
  it "has expected runtime values" do
    exnMsgs `shouldEqual` 
      [ "this Exception has no subtype"
      , "this Exception is an HTTPException at runtime"  
      , "this Exception is a MissingKeyException at runtime"
      ]


main :: Effect Unit
main = launchAff_ $ 
  runSpec [consoleReporter] do
    spec
    readmeSpec

