-- ***LinkedList***
module MathUtil.LinkedList
( 
{- DataTypes -}
  LinkedList(EmptyList, Node)
{- Factory Functions -}
, makeEmptyList
, makeNode
, makeNodeT
, makeLinkedListFiller
, listToLinkedList
, linkedListToList
{- Methods -}
, getData
, setData
, getTail
, setTail
, prependNode
, appendNode
, removeData
, getNodeRec
, isEmptyList
, search
, MathUtil.LinkedList.concat
, len
{- Operators -}
, (.:)
, (.:.)
, (.++)
{- Misc Functions -}
) where


--- **Imports**


--- **DataType Declarations**

---- LinkedList: A singly linked list
----   EmptyList -- An mpty list
----   Node      -- A node in the list
data LinkedList a = EmptyList | Node a (LinkedList a) deriving (Eq, Ord, Show, Read)


--- **DataType Factory Functions**

---- Creates a LinkedList of type `EmptyList'
makeEmptyList:: (LinkedList a)
makeEmptyList = EmptyList

---- Creates a LinkedList of type `Node' with data as param 1, and an `EmptyList' as its tail
makeNode:: a -> (LinkedList a)
makeNode d = Node d EmptyList

---- Creates a LinkedList of type `Node' with data as param 1, and param 2 as its tail
makeNodeT:: a -> (LinkedList a) -> (LinkedList a)
makeNodeT d t = Node d t

---- Creates a LinkedList with chain length `len', and default data `val'
makeLinkedListFiller:: Int -> a -> LinkedList a
makeLinkedListFiller len val
            | len <= 0 = EmptyList
            | otherwise = Node val (makeLinkedListFiller (len-1) val)

---- Converts a List to a LinkedList
listToLinkedList:: [a] -> LinkedList a
listToLinkedList [] = EmptyList
listToLinkedList (h:t) = Node h (listToLinkedList t)

---- Converts a LinkedList to a List
linkedListToList:: (LinkedList a) -> [a]
linkedListToList (EmptyList) = []
linkedListToList (Node d t) = d : (linkedListToList t)


--- **DataType Methods**

---- Gets:
----   `Just Data' from a LinkedList of type `Node'
----   `Nothing' from a LinkedList of type `EmptyList' 
getData:: (LinkedList a) -> Maybe a
getData (Node d _) = Just d
getData llist = Nothing

---- Sets the data in a LinkedList of type `Node'
---- Returns:
----   `Just (Node data tail)' in a LinkedList of type `Node'
----   `Nothing' in a LinkedList of type `EmptyList' 
setData:: a -> (LinkedList a) -> Maybe (LinkedList a)
setData d (Node _ t) = Just (Node d t)
setData _ (EmptyList) = Nothing

---- Gets:
----   the tail from a LinkedList of type `Node'
----   `EmptyList' from a LinkedList of type `EmptyList' 
getTail:: (LinkedList a) -> (LinkedList a)
getTail (Node _ t) = t
getTail (EmptyList) = EmptyList

---- Sets the tail in a LinkedList
---- Returns:
----   `EmptyList' if both params are of type `EmptyList'
----   tail (param 1) if param 2 is of type `EmptyList'
----   `LinkedList' of type `Node data tail', where tail is param 1 and data is the data of param 2, if param 2 is of type `Node'
setTail:: (LinkedList a) -> (LinkedList a) -> (LinkedList a)
setTail (EmptyList) (EmptyList) = EmptyList
setTail tail (EmptyList) = tail
setTail tail (Node d _) = (Node d tail)

---- Prepends a new `Node' with param 1 as the data, and param 2 as a tail
prependNode:: a -> (LinkedList a) -> (LinkedList a)
prependNode d (EmptyList) = Node d EmptyList
prependNode d next@(Node _ _) = Node d next

---- Appends a new `Node' with param 1 as the data, as the tail param 2's last Node
appendNode:: a -> (LinkedList a) -> (LinkedList a)
appendNode d (EmptyList) = Node d EmptyList
appendNode d (Node _ t) = appendNode d t

---- Removes the first `Node', with param 1 as its data, from param 2, if such a `Node' exists
removeData:: (Eq a) => a -> (LinkedList a) -> (LinkedList a)
removeData _ llist@(EmptyList) = llist
removeData d llist = removeDataInternal (search d llist) llist
            where removeDataInternal:: Int -> (LinkedList a) -> (LinkedList a)
                  removeDataInternal (-1) llist = llist
                  removeDataInternal idx llist@(Node a t)
                            | idx == 1 = setTail (getTail t) llist
                            | otherwise = removeDataInternal (idx-1) llist


---- Gets a node in a LinkedList, with link # param 2, recursively
getNodeRec:: (LinkedList a) -> Int -> (LinkedList a)
getNodeRec llist@(EmptyList) _ = llist
getNodeRec llist@(Node _ t) i
        | i <= 0 = llist
        | otherwise = getNodeRec t (i-1)

---- True if param 1 is of type `EmptyList', False otherwise
isEmptyList:: (LinkedList a) -> Bool
isEmptyList (EmptyList) = True
isEmptyList _ = False

---- The first 0-indexed `Node' in param 2, having param 1 as its data, -1 otherwise
search::(Eq a) => a -> (LinkedList a) -> Int
search d llist = searchInternal d llist 0
            where searchInternal::(Eq a) => a -> (LinkedList a) -> Int -> Int
                  searchInternal _ (EmptyList) _ = -1
                  searchInternal val llist@(Node d t) c
                            | val == d = c
                            | otherwise = searchInternal val t (c+1)

---- Sets param 2 as the tail of the last `Node' in param 1
concat:: (LinkedList a) -> (LinkedList a) -> (LinkedList a)
concat (EmptyList) llist = llist
concat llist (EmptyList) = llist
concat ll1@(Node _ (EmptyList)) ll2 = setTail ll1 ll2
concat ll1@(Node _ t) ll2 = Matrix.LinkedList.concat t ll2

len:: (LinkedList a) -> Int
len (EmptyList) = 0
len (Node _ t) = 1 + (len t)


--- **DataType Operators**

---- See prependNode
infixr 5 .:
(.:) :: a -> (LinkedList a) -> (LinkedList a)
d .: llist = prependNode d llist

---- See appendNode
infixr 5 .:.
(.:.) :: (LinkedList a) -> a -> (LinkedList a)
llist .:. d = appendNode d llist

---- See concat
infixr 5 .++
(.++):: (LinkedList a) -> (LinkedList a) -> (LinkedList a)
ll1 .++ ll2 = Matrix.LinkedList.concat ll1 ll2
