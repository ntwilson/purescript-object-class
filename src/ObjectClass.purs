module ObjectClass 
  ( NoSubtype, ObjectClass, instanceOf, cast, forgetSubtype
  , new, subtype, class ExtractFields, extractFields, expandSubtype
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (Variant, case_, expand, inj, on, prj)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (RowList)
import Prim.RowList as RL
import Record as Record
import Type.Prelude (class IsSymbol, Proxy(..))


-- | A "class" as one might think of from an Object-Oriented perspective. Specifically, this type
-- | allows for an inheritance hierarchy, with subtypes inheriting members from their parents, 
-- | an open union for subtypes, and runtime type checking. 
-- | Object class takes two rows.  The first is the subtypes of this class.  For an open union (allowing
-- | subtyping from external modules), this would typically be some row variable or an open row.  
-- | For a closed union, you could define a closed row, and this would "seal" the class.
-- | The second row is the members of the class.  These members will be inherited by any subtypes.
type ObjectClass :: Row Type -> Row Type -> Type
type ObjectClass subtypes shared = 
  Record ( subtype :: Variant subtypes | shared )


-- | Represents that an object of some ObjectClass might not have a subtype.  In an OO framework,
-- | having this a part of the subtypes row might be analogous with a class that is not abstract. 
type NoSubtype :: Row Type -> Row Type
type NoSubtype r = (noSubtype :: Unit | r)

noSubtype :: ∀ r. Variant (NoSubtype r)
noSubtype = inj (Proxy :: _ "noSubtype") unit

-- | A runtime check if some ObjectClass has a particular subtype.  If it does, it returns an object
-- | that contains both the parent's members and the subtype's members.  This only checks one inheritance
-- | level at a time, but multiple calls to `instanceOf` can be chained together to "walk down" an inheritance
-- | hierarchy.
instanceOf :: ∀ sym shared subtypes subtype combinedWithDupes combined tail. 
  IsSymbol sym => Union shared subtype combinedWithDupes => Nub combinedWithDupes combined => 
  Lacks "subtype" shared => Cons sym (Record subtype) tail subtypes =>
  Proxy sym -> ObjectClass subtypes shared -> Maybe (Record combined)
instanceOf proxy cls = do
  sub <- prj proxy cls.subtype
  pure $ Record.merge (Record.delete (Proxy :: _ "subtype") cls) sub

-- | Deletes any information about subtypes, returning a plain record that contains only the 
-- | parent class' members.
forgetSubtype :: ∀ shared subtypes. Lacks "subtype" shared => ObjectClass subtypes shared -> Record shared
forgetSubtype = Record.delete (Proxy :: _ "subtype")

-- | A compile-time assertion othat some `ObjectClass` has a particular subtype.  The behavior is the same 
-- | as `instanceOf`, except it is fully solved by the compiler instead of potentially failing at runtime.
cast :: forall sym subtypes shared subtype combinedWithDupes combined.
  Cons sym (Record subtype) () subtypes => IsSymbol sym => 
  Union shared subtype combinedWithDupes => Nub combinedWithDupes combined => Lacks "subtype" shared => 
  Proxy sym -> ObjectClass subtypes shared -> Record combined
cast proxy cls = Record.merge (Record.delete (Proxy :: _ "subtype") cls) subtypeVals
  where
    subtypeVals = (case_ # on proxy identity) cls.subtype

-- | Creates a new `ObjectClass` with no subtype given.
new :: ∀ shared r. Lacks "subtype" shared => Record shared -> ObjectClass (NoSubtype r) shared 
new rec = Record.insert (Proxy :: _ "subtype") noSubtype rec

-- | Creates a new `ObjectClass` from a plain record that contains both the 
-- | parent members and the child members, along with a proxy for the subtype's symbol. 
subtype :: ∀ sym shared subtypes combined tail subtype sharedList subtypeList. 
  IsSymbol sym => Lacks "subtype" shared => Cons sym (Record subtype) tail subtypes => 
  ExtractFields sharedList combined shared => ExtractFields subtypeList combined subtype => 
  Union shared subtype combined => 
  RL.RowToList shared sharedList => RL.RowToList subtype subtypeList => 
  Proxy sym -> Record combined -> ObjectClass subtypes shared 
subtype proxy rec = 
  extractFields (Proxy :: _ sharedList) rec 
  # Record.insert (Proxy :: _ "subtype") (inj proxy $ extractFields (Proxy :: _ subtypeList) rec)

class ExtractFields :: RowList Type -> Row Type -> Row Type -> Constraint
class ExtractFields list combined out | list -> out where
  -- | Given some record, extracts just the fields we care aboout into a new record.  
  extractFields :: Proxy list -> Record combined -> Record out

instance ExtractFields RL.Nil combined () where
  extractFields _ _ = { } 

else instance 
  ( Cons label value rowCombinedTail combined
  , Cons label value rowOutTail out
  , IsSymbol label
  , RL.RowToList rowCombinedTail combinedTail
  , RL.RowToList rowOutTail outTail
  , ExtractFields outTail combined rowOutTail
  , Lacks label rowOutTail
  ) => 
  ExtractFields (RL.Cons label value outTail) combined out where 
  extractFields :: Proxy (RL.Cons label value outTail) -> Record combined -> Record out
  extractFields _ rec = Record.insert label value $ extractFields (Proxy :: Proxy outTail) rec
    where
      label = (Proxy :: _ label)
      value = Record.get label rec

-- | Expand the set of subtypes for an `ObjectClass`.  This is particularly useful for 
-- | converting a closed sum for subtypes (a "sealed" class) into an open sum, allowing 
-- | it to unify types with any other object with the same parent `ObjectClass`.
expandSubtype :: ∀ shared lt a gt. Union lt a gt => ObjectClass lt shared -> ObjectClass gt shared 
expandSubtype cl@{subtype: sub} = cl { subtype = expand sub }
