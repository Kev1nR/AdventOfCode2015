open System
open System.IO

//let sr = new StreamReader(@"C:\Users\Kevin-Roche\Documents\Visual Studio 2013\Projects\AdventOfCode2015\AoC Day2 Parcel dimensions.txt")

type box = {l : int; w : int; h : int}
    with 
        member __.SurfaceArea = 2*(__.l*__.w + __.l*__.h + __.h*__.w)
        member __.SmallestSurface = [|__.l*__.w; __.l*__.h; __.h*__.w|] |> Array.min
        member __.Volume = __.l * __.w * __.h
        member __.SmallestPerimeter = [|__.l + __.w; __.l + __.h; __.h + __.w;|] |> Array.min |> fun p -> p * 2

let parseDimensons (stringDims : string) =
    stringDims.Split('x') 
    |> Array.map (Int32.TryParse >> snd)
    |> function
        | [|l'; w'; h'|] -> {l = l'; w = w'; h = h'}
        | _ -> failwith "Incorrect dimensions"

let totalWrappingPaper () =
    seq {
        use sr = new StreamReader(@"C:\Users\Kevin-Roche\Documents\Visual Studio 2013\Projects\AdventOfCode2015\AoC Day2 Parcel dimensions.txt")
        while not sr.EndOfStream do
            let dimStr = sr.ReadLine()
            yield parseDimensons dimStr}
    |> Seq.fold (fun tot parcel -> (fst tot + (parcel.SurfaceArea + parcel.SmallestSurface)), 
                                    snd tot + (parcel.SmallestPerimeter + parcel.Volume)) (0, 0)

totalWrappingPaper () 
|> fun (wrap, ribbon) ->
        printfn "Dear Sir/Madam, \n\twe would like to order %d sq feet of wrapping paper and %d feet of ribbon. \nYours sincerely\n\t the Elves." wrap ribbon

        
