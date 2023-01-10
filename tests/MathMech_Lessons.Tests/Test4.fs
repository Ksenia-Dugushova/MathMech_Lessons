module Tests4

open FsCheck
open Expecto
open System
open Microsoft.FSharp.Core

module SparseVectorTests =
    open SparseVector

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "function square for one element 1"
              <| fun _ ->
                  let actualResult = square [| Some(1) |]
                  Expect.equal actualResult 1u "The result should be 1"

              testCase "function square"
              <| fun _ ->
                  let actualResult =
                      square [| Some(11); Some(5); Some(26); Some(2); Some(9); Some(33); Some(4); Some(17); Some(8) |]

                  Expect.equal actualResult 16u "The result should be 16"

              testCase "function square for empty array"
              <| fun _ ->
                  let actualResult = square [||]
                  Expect.equal actualResult 0u "The result should be 0"

              testProperty "function square property test"
              <| fun (arr: array<Option<int>>) ->
                  Expect.isLessThanOrEqual
                  <| arr.Length
                  <| Convert.ToInt32(square arr)
                  <| "Square expected less than or equal array length result"

              testCase "toBinaryTree for None array"
              <| fun _ ->
                  let actualResult = toBinaryTree [| Option.None |]
                  Expect.equal actualResult BinaryTree.None "The result should be BinaryTree.None"

              testCase "toBinaryTree for random array"
              <| fun _ ->
                  let actualResult = toBinaryTree [| Some(1); Some(1); Option.None; Option.None |]

                  Expect.equal actualResult (Node(Node(Leaf(1), Leaf(1)), BinaryTree.None)) "The result should be (Node(Node(Leaf(1), Leaf(1))"

              testCase "toBinaryTree for string array"
              <| fun _ ->
                  let actualResult = toBinaryTree [| Some("a"); Some("bb") |]

                  Expect.equal actualResult (Node(Leaf("a"), Leaf("bb"))) "The result should be (Node(Leaf('a'), Leaf('bb')))"

              testCase "vectorElement for empty array"
              <| fun _ ->
                  let actualResult =
                      Expect.throws (fun _ -> SparseVector([||])[152u] |> ignore) "Index out of the range"

                  actualResult

              testProperty "vectorElement property test"
              <| fun (arr: array<int option>) (i: uint) ->
                  let actualResult = SparseVector arr
                  let i = Random().Next(0, arr.Length)

                  if arr.Length = 0 then
                      skiptest |> ignore
                  else
                      Expect.equal <| arr[i] <| actualResult[uint i] <| "vectorElement expected same result as Array.get" ]



module SparseMatrixTests =
    open SparseMatrix

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Square for array 2D 1"
              <| fun _ ->
                  let actualResult = square (array2D [ [ Some(1); Some(2) ]; [ Some(3); Some(4) ] ])
                  Expect.equal actualResult 2u "The result should be 2"

              testCase "Square for array 2D 2"
              <| fun _ ->
                  let actualResult =
                      square (array2D [ [ Some(7); Some(22); Some(3) ]; [ Some(4); Some(51); Some(21) ]; [ Some(3); Some(34); Some(35) ] ])

                  Expect.equal actualResult 4u "The result should be 4"

              testCase "Square for array 2D with one element"
              <| fun _ ->
                  let actualResult = square (array2D [ [ Some(10) ] ])
                  Expect.equal actualResult 1u "The result should be 1"

              testCase "toQuadTree for empty array 2D"
              <| fun _ ->
                  let actualResult = toQuadTree (array2D [ [] ])
                  Expect.equal actualResult QuadTree.None "The result should be QuadTree.None"

              testCase "toQuadTree for array2D "
              <| fun _ ->
                  let tree =
                      toQuadTree (array2D [ [ Some(1); Some(1) ]; [ Option.None; Option.None ]; [ Some(1); Some(1) ] ])

                  Expect.equal
                      tree
                      (Node(Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None, Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None))
                      "toQuadTree expected : (Node(Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None, Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None))"

              testProperty "toSquare property test array2D"
              <| fun (arr: int option[,]) ->
                  Expect.isLessThanOrEqual
                  <| max (Array2D.length1 arr) (Array2D.length2 arr)
                  <| Convert.ToInt32(square arr)
                  <| "square expected less than or equal array length result"

              testProperty "Item from the cell of array2D is equal to item from the cell of SparseMatrix"
              <| fun (arr: int option[,]) (i: uint) (j: uint) ->
                  let actualResult = SparseMatrix arr
                  let i = Random().Next(0, Array2D.length1 arr)
                  let j = Random().Next(0, Array2D.length2 arr)

                  if Array2D.length1 arr = 0 || Array2D.length2 arr = 0 then
                      skiptest |> ignore
                  else
                      Expect.equal
                      <| arr[i, j]
                      <| actualResult[uint i, uint j]
                      <| "Item from the cell of array2D should be equal to item from the cell of SparseMatrix" ]

module MultiMatrixTests =
    open MultiMatrix
    open SparseMatrix
    open SparseVector

    let funPlusInt a b =
        match a, b with
        | Some x, Some y -> if x + y <> 0 then Some(x + y) else Option.None
        | Option.None, Some y -> Some y
        | Some x, Option.None -> Some x
        | Option.None, Option.None -> Option.None

    let funMultiInt a b =
        match a, b with
        | Some x, Some y -> if x * y <> 0 then Some(x * y) else Option.None
        | Option.None, _
        | _, Option.None -> Option.None

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Multi vector and matrix"
              <| fun _ ->
                  let vec = SparseVector([| Some(0); Some(1) |])
                  let mat = SparseMatrix(array2D [ [ Some(1); Some(1) ]; [ Some(1); Some(2) ] ])
                  let res = multiplication funPlusInt funMultiInt vec mat
                  Expect.equal res.Storage (BinaryTree.Node(BinaryTree.Leaf(1), BinaryTree.Leaf(2))) "The result should be (BinaryTree.Node(BinaryTree.Leaf(1), BinaryTree.Leaf(2)))"
              testCase "Multi None vector and matrix"
              <| fun _ ->
                  let vec = SparseVector([| Some(1); Some(1); Some(1) |])

                  let mat =
                      SparseMatrix(array2D [ [ Option.None; Option.None; Option.None ]; [ Option.None; Option.None; Option.None ]; [ Option.None; Option.None; Option.None ] ])

                  let res = multiplication funPlusInt funMultiInt vec mat
                  Expect.equal res.Storage BinaryTree.None "The result should be BinaryTree.None"

              testProperty "Multiplication property test"
              <| fun (x: uint) (y: uint) ->
                  if x <> 0u && y <> 0u then
                      let length = abs (Convert.ToInt32(x))
                      let columns = abs (Convert.ToInt32(y))
                      let rows = length

                      let rnd = System.Random()
                      let valueToZero x = if x % 2 = 1 then x else 0

                      let randomVector length =
                          Array.init length (fun _ -> rnd.Next(1, 10))

                      let random2d rows columns =
                          Array2D.init rows columns (fun _ _ -> rnd.Next(1, 10))

                      let zeroToSomeNone x = if x <> 0 then Some x else Option.None

                      let arr = randomVector length |> Array.map valueToZero
                      let arrSome = Array.map zeroToSomeNone arr

                      let arr2d = random2d rows columns |> Array2D.map valueToZero
                      let arr2dSome = arr2d |> Array2D.map zeroToSomeNone

                      let vec = SparseVector arrSome
                      let mtx = SparseMatrix arr2dSome

                      let naiveMulti (arr: array<int>) (arr2d: int[,]) =
                          let rows = arr.Length
                          let columns = Array2D.length2 arr2d
                          let mutable res = Array.zeroCreate columns

                          for j = 0 to columns - 1 do
                              for i = 0 to rows - 1 do
                                  res[j] <- res[j] + arr[i] * arr2d[i, j]

                          res

                      let expectedResult =
                          naiveMulti arr arr2d |> Array.map zeroToSomeNone |> SparseVector

                      let actualResult = multiplication funPlusInt funMultiInt vec mtx

                      Expect.equal actualResult.Storage expectedResult.Storage $"Something went wrong"
                      Expect.equal actualResult.Length mtx.RowCount "Expected actualResult.Length = matrix.RowCount" ]

module PropertyTests =
    open SparseVector
    open SparseMatrix
    open MultiMatrix

    let rnd = System.Random()

    let vectorsMaker (length: int) =
        let arr = Array.init (abs length) (fun _ -> rnd.Next(1, 10))
        let arrOfSome = arr |> Array.map (fun x -> if x > 2 then Option.None else Some(x))
        SparseVector arrOfSome, arrOfSome

    let rec BinaryTreeControl tree =
        match tree with
        | BinaryTree.Node (BinaryTree.None, BinaryTree.None) -> false
        | BinaryTree.Node (left, right) -> BinaryTreeControl left && BinaryTreeControl right
        | _ -> true

    let matrixMaker (length: int) =
        let arr2D = Array2D.init (abs length) (abs length) (fun _ _ -> rnd.Next(1, 10))

        let arrOfSome2D =
            arr2D |> Array2D.map (fun x -> if x > 2 then Option.None else Some(x))

        SparseMatrix arrOfSome2D, arrOfSome2D

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testProperty "Sum vectors"
              <| fun (length: int) ->
                  let vec1, arrOfSome1 = vectorsMaker (abs length)
                  let vec2, arrOfSome2 = vectorsMaker (abs length)
                  let actualResult = addVector MultiMatrixTests.funPlusInt vec1 vec2


                  let naiveSum (arr1: array<int option>) (arr2: array<int option>) =
                      (arr1, arr2) ||> Array.map2 (fun x y -> MultiMatrixTests.funPlusInt x y)

                  naiveSum

                  Expect.equal actualResult.Storage
                  <| SparseVector(naiveSum arrOfSome1 arrOfSome2).Storage
                  <| "Results of FAddTree with two vectors should be the same with naive sum"

                  Expect.equal <| BinaryTreeControl actualResult.Storage <| true <| "Something went wrong"

              ]
