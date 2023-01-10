module Test5

open System
open System.Collections.Generic
open Expecto
open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections
open SparseMatrix
open SparseVector
open BFS

module Bfstest =
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
            [ testCase "Matrix from empty list"
              <| fun _ ->
                  let actualResult = toQTree [] 0 0
                  Expect.equal <| actualResult <| QuadTree.None <| "Matrix from empty list should be 'QuadTree.None'"

              testProperty "toQTree"
              <| fun (tList: list<uint * uint>) ->
                  let tripleList =
                      List.append tList [ (0u, 0u) ] |> List.distinct |> List.map (fun (x, y) -> (x, y, Some 100))

                  let triple = List.maxBy (fun z -> first z + second z) tripleList
                  let length = first triple + second triple + 1u

                  let naiveFormation list length =
                      let new2DArray = Array2D.create (toInt length) (toInt length) Option.None

                      for i in 0 .. List.length list - 1 do
                          new2DArray[first list[i] |> toInt, second list[i] |> toInt] <- third list[i]

                      new2DArray

                  let actualResult = toQTree tripleList (float length) (float length)
                  let expectedResult = naiveFormation tripleList length |> toQuadTree
                  Expect.equal actualResult expectedResult $"Something went wrong"

              testCase "BFS with some graph and apexes 1"
              <| fun _ ->
                  let list =
                      [ (0u, 1u, Some 4); (1u, 0u, Some 4); (1u, 3u, Some 9); (3u, 1u, Some 9) ]

                  let matrix = SparseMatrix(list, 4, 4)
                  let actualResult = BFS matrix [ 0u; 1u; 2u; 3u ]

                  Expect.equal actualResult.Storage
                  <| Node(Node(Leaf 0u, Leaf 0u), Node(Leaf 0u, Leaf 0u))
                  <| "BFS should return 'Node(Node(Leaf 0, Leaf 0),Node(Leaf 0, Leaf 0))' from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)] and start position in [0, 1, 2, 3]"

              testCase "BFS with some graph and apexes 2"
              <| fun _ ->
                  let list =
                      [ (0u, 1u, Some 4); (1u, 0u, Some 4); (1u, 3u, Some 9); (3u, 1u, Some 9) ]

                  let matrix = SparseMatrix(list, 4, 4)
                  let actualResult = BFS matrix [ 0u ]

                  Expect.equal actualResult.Storage
                  <| Node(Node(Leaf 0u, Leaf 1u), Node(None, Leaf 2u))
                  <| "BFS should return 'Node(Node(Leaf 0, Leaf 1), Node(None, Leaf 2))' from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)] and start position in [0]"

              testCase "BFS with graph and apexes 3"
              <| fun _ ->
                  let list = []
                  let matrix = SparseMatrix(list, 0, 0)
                  let actualResult = BFS matrix []

                  Expect.equal actualResult.Storage <| None <| "BFS should return 'None'"

              testProperty "naive bfs"
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
                      BFS
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

                  let naiveBFS start (arr: 'a option[,]) =
                      let queue = Queue<uint * uint>()

                      for i in start do
                          queue.Enqueue(i, 0u)

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
                                          queue.Enqueue(uint i, snd x + 1u)

                                  helper (x :: result) (visited.Add(fst x))

                      helper [] Set.empty

                  let list = naiveBFS start arr
                  let answers = Array.create (int size) Option.None

                  for i in 0 .. list.Length - 1 do
                      answers[int (fst list[i])] <- Some(snd list[i])

                  let result2 = SparseVector(answers)
                  Expect.equal <| result1.Storage <| result2.Storage <| "Something went wrong" ]
