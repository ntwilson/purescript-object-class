# purescript-object-class
OO-style classes with inherited members and open subclass sum types.

This library allows for "classes" that resemble the classes from an Object Oriented style. 
It specifically aims to create:
1. Inherited members from a parent to a child `ObjectClass`
2. An open union of subtypes. One module can create a subtype of an `ObjectClass` from another module
3. Runtime type checks for the subtypes. 
4. Unified types between objects of different types with the same parent `ObjectClass`. 

You should certainly prefer builtin row polymorphism with records and closed sum types to this library
where possible, though there are times when runtime type checking and open sum types are necessary. 

Consider the following simplified exapmle in some pseudo OO language:
```TypeScript
class Exception { 
  message : String
}

class HTTPException extends Exception { 
  statusCode : Int
}

class MissingKeyException<T> extends Exception { 
  key : T 
}
```

using this library, this would be expressed in PureScript as:
```PureScript
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
```

I can create instances of all these new types:
```PureScript
plainExn :: ∀ r. Exception (NoSubtype r)
plainExn = new { message: "this Exception has no subtype" } 

httpExn :: ∀ r. HTTPException r
httpExn = subtype httpException { message: "this Exception is an HTTPException at runtime", statusCode: 404 } 

keyExn :: ∀ r. MissingKeyException String r
keyExn = subtype missingKeyException { message: "this Exception is a MissingKeyException at runtime", key: "I'm missing" }
```
Note that each is given a row variable `r`.  This lets us unify all three types at runtime:
```PureScript
exns :: ∀ r. Array (ObjectClass ( httpException :: { statusCode :: Int }, missingKeyException :: { key :: String }, noSubtype :: Unit | r ) ( message :: String ))
exns = [ plainExn, httpExn, keyExn ]
```
I wrote out the full subtype row there, though oftentimes you can leave it as just `_` (or just put `exns` in a local 
`let` or `where` binding and leave off the annotation altogether) to avoid having to define all possible cases. 

I can even access the parent `ObjectClass`'s `message` member from all three
```PureScript
exnMsgs :: Array String
exnMsgs = exns <#> _.message
```

If I want to write a handler with pattern matching, I can figure out the type at runtime 
```PureScript
exnHandler :: ∀ r. Exception (httpException :: { statusCode :: Int }, missingKeyException :: { key :: String } | r ) -> Effect Unit
exnHandler exn 
  | Just http <- instanceOf httpException exn = 
    log ("Got an HTTP Exception: (Status " <> show http.statusCode <> ") '" <> http.message <> "'")
  | Just key <- instanceOf missingKeyException exn = 
    log ("Got a Missing Key Exception while looking for key: '" <> show key.key <> "'.  '" <> key.message <> "'")
  | otherwise = 
    log ("Got some other kind of exception. '" <> exn.message <> "'")
```

I can also bypass the pattern matching if I happen to know the exact subtype of a particular `ObjectClass`
```PureScript
httpExn :: ∀ r. HTTPException r
httpExn = subtype httpException { message: "this Exception is an HTTPException at runtime", statusCode: 404 } 

statusCode :: Int
statusCode = 
  let http = cast httpException httpExn
  in http.statusCode
```


As stated above, you should certainly prefer builtin row polymorphism with records and closed sum types 
to this library where possible.  Heavy use of `ObjectClass` is likely to result in some weird compile errors 
that are difficult to diagnose.  Most especially, if you can just define the closed set of cases in one module, 
please prefer to use regular ADTs.

Sometimes you can bypass the need for this library with builtin row polymorphism
```PureScript
f :: ∀ r. { message :: String | r } -> Effect Unit
f {message} = ...
```
which allows you to pass in any record to `f` as long as it has _at least_ a `message` field. 
What's lacking here is that you cannot inspect the contents of `r` at runtime in any way.

Sometimes you can bypass the need for this library with variants
```PureScript
f :: ∀ r key. Variant ( httpException :: { message :: String, statusCode :: Int }, missingKeyException :: { message :: String, key :: Key } | r ) -> String
f var = 
  default ... 
    # on (Proxy :: _ "httpException") ...
    # on (Proxy :: _ "missingKeyException") ... 
```
which does allow you to do a runtime check on whatever you pass to `f` and take different branches 
depending on the runtime representation.  
What's lacking here is the ability to create a hierarchy, and to create a single handler for many cases that
have a similar shape that isn't exactly the same. (Notice that each case must separately define `message`, and we
cannot write a function that can extract a `message` regardless of the case).  
