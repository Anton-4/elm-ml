module Trees.Tree exposing (..)


type Node
    = Node Int (List Node) -- colIndex children


isLeaf : Node -> Bool
isLeaf (Node _ children) =
    List.length children == 0