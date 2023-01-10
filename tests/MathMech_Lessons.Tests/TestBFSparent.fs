module TestBFSparent

open System
open System.Collections.Generic
open Expecto
open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections
open SparseMatrix
open SparseVector
open BFSparent


let toInt (unsignedInt: uint) =
    try
        Convert.ToInt32(unsignedInt)
    with :? OverflowException ->
        failwith $"is outside the range of the Int32 type."

let rnd = Random()

[<Tests>]
let tests =
    testList
        "samples"
        [ testCase "pBFS with some graph and apexes 1"
          <| fun _ ->
              let list =
                  [ (0u, 1u, Some 4); (1u, 0u, Some 4); (1u, 3u, Some 9); (3u, 1u, Some 9) ]

              let matrix = SparseMatrix(list, 4, 4)
              let actualResult = parentBFS matrix [ 0u; 1u; 2u; 3u ]

              Expect.equal actualResult.Storage
              <| Node(Node(Leaf 0u, Leaf 1u), Node(Leaf 2u, Leaf 3u))
              <| "pBFS should return 'Node(Node(Leaf 0u, Leaf 1u), Node(Leaf 2u, Leaf 3u))'"

          testCase "pBFS with some graph and apexes 2"
          <| fun _ ->
              let list =
                  [ (0u, 1u, Some 4); (1u, 0u, Some 4); (1u, 3u, Some 9); (3u, 1u, Some 9) ]

              let matrix = SparseMatrix(list, 4, 4)
              let actualResult = parentBFS matrix [ 0u ]

              Expect.equal actualResult.Storage
              <| Node(Node(Leaf 0u, Leaf 0u), Node(None, Leaf 1u))
              <| "pBFS should return 'Node(Node(Leaf 0u, Leaf 0u), Node(None, Leaf 1u))' "

          testCase "pBFS with some graph and apexes 3"
          <| fun _ ->
              let list =
                  [ (0u, 4u, Some 7); (1u, 5u, Some 7); (2u, 6u, Some 7); (3u, 7u, Some 7) ]

              let matrix = SparseMatrix(list, 8, 8)
              let actualResult = parentBFS matrix [ 0u; 1u; 2u; 3u ]

              Expect.equal actualResult.Storage
              <| Node(Node(Node(Leaf 0u, Leaf 1u), Node(Leaf 2u, Leaf 3u)), Node(Node(Leaf 0u, Leaf 1u), Node(Leaf 2u, Leaf 3u)))
              <| "pBFS should return 'Node(Node(Node(Leaf 0u, Leaf 1u), Node(Leaf 2u, Leaf 3u)), None)' "

          testCase "pBFS with graph and apexes 3"
          <| fun _ ->
              let list = []
              let matrix = SparseMatrix(list, 0, 0)
              let actualResult = parentBFS matrix []

              Expect.equal actualResult.Storage <| None <| "pBFS should return 'None'"


          testProperty "naive parent bfs"
          <| fun (list: List<uint * uint>) ->
              let size =
                  if list.IsEmpty then
                      0u
                  else
                      let x = List.maxBy fst list
                      let y = List.maxBy snd list
                      (max (fst x) (snd y)) + 1u

              let resultList = List.map (fun (x, y) -> (x, y, Some 10)) list |> List.distinct

              let start =
                  if resultList.Length <> 0 then
                      [ first resultList.Head ]
                  else
                      []

              let matrix = SparseMatrix(resultList, float size, float size)

              let result1 =
                  parentBFS
                      matrix
                      (if resultList.Length <> 0 then
                           [ first resultList.Head ]
                       else
                           [])

              let iSize =
                  try
                      Convert.ToInt32(size)
                  with :? OverflowException ->
                      failwith $"outside the range"

              let arr = Array2D.create iSize iSize Option.None

              for i in resultList do
                  let x = first i
                  let y = second i

                  let iCoord =
                      try
                          Convert.ToInt32(x), Convert.ToInt32(y)
                      with :? OverflowException ->
                          failwith $"outside the range of the Int32 type."

                  arr[fst iCoord, snd iCoord] <- third i


              let naiveParentBFS start (arr: 'a option[,]) =
                  let queue = Queue<uint * uint>()

                  for i in start do
                      queue.Enqueue(i, i)

                  let rec helper result visited =
                      if queue.Count = 0 then
                          List.rev result
                      else
                          let x = queue.Dequeue()

                          if Set.contains (fst x) visited then
                              helper result visited
                          else
                              let iApex =
                                  try
                                      Convert.ToInt32(fst x)
                                  with :? OverflowException ->
                                      failwith $"outside the range"

                              for i in 0 .. Array2D.length2 arr - 1 do
                                  let value = arr[iApex, i]

                                  if value <> Option.None then
                                      queue.Enqueue(uint i, fst x)

                              helper (x :: result) (visited.Add(fst x))

                  helper [] Set.empty

              let list = naiveParentBFS start arr
              let answers = Array.create (int size) Option.None

              for i in 0 .. list.Length - 1 do
                  answers[int (fst list[i])] <- Some(snd list[i])

              let result2 = SparseVector(answers)
              Expect.equal <| result1.Storage <| result2.Storage <| "Something went wrong" ]
