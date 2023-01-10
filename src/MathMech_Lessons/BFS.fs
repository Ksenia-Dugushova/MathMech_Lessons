module BFS

open SparseVector
open SparseMatrix
open MultiMatrix
open FSharp.Collections

let mult x y =
    match x, y with
    | Some value, Some _ -> Some value
    | _ -> Option.None

let add x y =
    match x, y with
    | Option.None, Option.None -> Option.None
    | _ -> Some()

let mask x y =
    match x, y with
    | Some _, Option.None -> Some()
    | _ -> Option.None

let plusVisited iter x y =
    match x, y with
    | Option.None, Option.None -> Option.None
    | Option.None, Some value -> Some value
    | Some _, Option.None -> Some iter
    | _ -> failwith $"Something went wrong"

let BFS (gMtx: SparseMatrix<'value>) (startV: list<uint>) =
    let apexes = List.map (fun x -> (x, ())) startV
    let front = SparseVector(apexes, gMtx.ColumnCount)

    let visited =
        addVector (plusVisited 0u) front (SparseVector(BinaryTree.None, gMtx.ColumnCount))

    let rec inner (front: SparseVector<'a>) visited iter =
        if front.isEmpty then
            visited
        else
            let newFront = addVector mask (multiplication add mult front gMtx) visited
            let visited = addVector (plusVisited iter) newFront visited
            inner newFront visited (iter + 1u)

    inner front visited 1u
