module SparseMatrix

open System
open SparseVector

type QuadTree<'value> =
    | Node of QuadTree<'value> * QuadTree<'value> * QuadTree<'value> * QuadTree<'value>
    | Leaf of 'value
    | None

let square arr =
    let lengthR = Array2D.length1 arr
    let lengthC = Array2D.length2 arr
    let logarithm1 = Math.Log(lengthR, 2)
    let logarithm2 = Math.Log(lengthC, 2)

    if ceil logarithm1 = logarithm1 && ceil logarithm1 = logarithm2 && logarithm1 = logarithm2 then
        uint lengthR
    else
        uint (2.0 ** ceil (max logarithm1 logarithm2))

let toSquareValue length1 length2 =
    let logarithm1 = Math.Log(length1, 2)
    let logarithm2 = Math.Log(length2, 2)

    if ceil logarithm1 = logarithm1 && ceil logarithm2 = logarithm2 && logarithm1 = logarithm2 then
        uint length1
    else
        uint (2.0 ** ceil (max logarithm1 logarithm2))

type SquareArray<'value> =
    struct
        val Memory: option<'value>[,]
        val HeadOfRow: uint
        val HeadOfColumn: uint
        val Length: uint

        new(memory, headOfRow, headOfColumn, length) =
            { Memory = memory
              HeadOfRow = headOfRow
              HeadOfColumn = headOfColumn
              Length = length }
    end

let toQuadTree arr =
    let optionToQuadTree optionValue =
        match optionValue with
        | Option.None -> QuadTree.None
        | Some value -> QuadTree.Leaf value

    let rec quadTreeMaking (arr: SquareArray<'value>) =
        let memory = arr.Memory
        let head1 = arr.HeadOfRow
        let head2 = arr.HeadOfColumn
        let length = arr.Length
        let realLengthR = Array2D.length1 memory
        let realLengthC = Array2D.length2 memory

        if head1 >= uint realLengthR || head2 >= uint realLengthC then
            QuadTree.None
        elif length = 1u then
            optionToQuadTree memory[Convert.ToInt32(head1), Convert.ToInt32(head2)]
        else
            let fst = quadTreeMaking (SquareArray(memory, head1, head2, length / 2u))

            let snd =
                quadTreeMaking (SquareArray(memory, head1, head2 + length / 2u, length / 2u))

            let thd =
                quadTreeMaking (SquareArray(memory, head1 + length / 2u, head2, length / 2u))

            let fth =
                quadTreeMaking (SquareArray(memory, head1 + length / 2u, head2 + length / 2u, length / 2u))

            if fst = QuadTree.None && snd = QuadTree.None && thd = QuadTree.None && fth = QuadTree.None then
                QuadTree.None
            else
                Node(fst, snd, thd, fth)

    quadTreeMaking (SquareArray(arr, 0u, 0u, square arr))


let first (x, _, _) = x
let second (_, x, _) = x
let third (_, _, x) = x

let toQTree (tripleList: list<uint * uint * Option<'value>>) rows columns =
    let partition list length =
        let rec f list one two three four =
            match list with
            | [] -> one, two, three, four
            | head :: tail ->
                if first head < length / 2u then
                    if second head < length / 2u then
                        f tail (head :: one) two three four
                    else
                        f tail one ((first head, second head - length / 2u, third head) :: two) three four
                elif second head < length / 2u then
                    f tail one two ((first head - length / 2u, second head, third head) :: three) four
                else
                    f tail one two three ((first head - length / 2u, second head - length / 2u, third head) :: four)

        f list [] [] [] []

    let rec quadTreeFormation list length =
        if length = 1u then
            if List.length list = 0 then
                QuadTree.None
            else
                match third list[0] with
                | Option.None -> QuadTree.None
                | Some x -> QuadTree.Leaf x
        else
            let one, two, three, four = partition list length

            let o = quadTreeFormation one (length / 2u)
            let t = quadTreeFormation two (length / 2u)
            let th = quadTreeFormation three (length / 2u)
            let ft = quadTreeFormation four (length / 2u)

            if o = QuadTree.None && t = QuadTree.None && th = QuadTree.None && ft = QuadTree.None then
                QuadTree.None
            else
                QuadTree.Node(o, t, th, ft)

    if tripleList.Length = 0 then
        QuadTree.None
    else
        let squareLength = toSquareValue rows columns
        quadTreeFormation tripleList squareLength


type SparseMatrix<'value when 'value: equality> =
    struct
        val Storage: QuadTree<'value>
        val RowCount: uint
        val ColumnCount: uint

        new(arr: option<'value>[,]) =
            { Storage = toQuadTree arr
              RowCount = Array2D.length1 arr |> uint
              ColumnCount = Array2D.length2 arr |> uint }

        new(tripleList, rows, columns) =
            { Storage = toQTree tripleList rows columns
              RowCount = (uint rows)
              ColumnCount = (uint columns) }

        member this.Item
            with get (a, b) =
                let matrixElement a b (matrix: SparseMatrix<'value>) =
                    let rec element a b size tree =
                        match tree with
                        | QuadTree.Leaf value -> Some(value)
                        | QuadTree.None -> Option.None
                        | QuadTree.Node (x, y, z, w) ->
                            let middle = size / 2u

                            if a < middle && b < middle then
                                element a b middle x
                            elif a < middle && b >= middle then
                                element a (b - middle) middle y
                            elif a >= middle && b < middle then
                                element (a - middle) b middle z
                            else
                                element (a - middle) (b - middle) middle w

                    if a < matrix.RowCount && b < matrix.ColumnCount then
                        let powerSize = ceilPowTwo (max matrix.RowCount matrix.ColumnCount)
                        element a b powerSize matrix.Storage
                    else
                        failwith $"Index out of the range"

                matrixElement a b this

        member this.IsEmpty = this.Storage = QuadTree.None
    end

type COOMatrix<'A> =
    struct
        val Data: List<uint * uint * 'A>
        val Rows: uint
        val Columns: uint

        new(triplesList, rows, columns) =
            { Data = triplesList
              Rows = rows
              Columns = columns }
    end
