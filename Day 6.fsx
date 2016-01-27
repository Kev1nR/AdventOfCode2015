﻿(*

--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million 
lights in a 1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. 
The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate 
pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. 
The lights all start turned off. 

To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

For example:
  - turn on 0,0 through 999,999 would turn on (or leave on) every light.
  - toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
  - turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

After following the instructions, how many lights are lit?

To begin, get your puzzle input.


Your puzzle answer was 377891.

The first half of this puzzle is complete! It provides one gold star: *


--- Part Two ---

You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from Ancient Nordic Elvish.

The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. The lights all start at zero.

The phrase turn on actually means that you should increase the brightness of those lights by 1.

The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.

The phrase toggle actually means that you should increase the brightness of those lights by 2.

What is the total brightness of all lights combined after following Santa's instructions?

For example:
  - turn on 0,0 through 0,0 would increase the total brightness by 1.
  - toggle 0,0 through 999,999 would increase the total brightness by 2000000.


*)
open System.Collections
open System.IO

let instructions = ["turn on 887,9 through 959,629";
                    "turn on 454,398 through 844,448";
                    "turn off 539,243 through 559,965";
                    "turn off 370,819 through 676,868";
                    "turn off 145,40 through 370,997";
                    "turn off 301,3 through 808,453";
                    "turn on 351,678 through 951,908";
                    "toggle 720,196 through 897,994";
                    "toggle 831,394 through 904,860";
                    "toggle 753,664 through 970,926";
                    "turn off 150,300 through 213,740"]
                    
let (|TurnOn|TurnOff|Toggle|) (inst : string) =
    let getcoords (coords : string) =
        let crds = coords.Replace(" through ", ",").Split(',')
        
        (
            System.Int32.Parse(crds.[0]),
            System.Int32.Parse(crds.[1]),
            System.Int32.Parse(crds.[2]),
            System.Int32.Parse(crds.[3])
        )
        
    if inst.StartsWith("turn on")
    then
        TurnOn (getcoords (inst.Substring(7)))
    else if inst.StartsWith("turn off")
    then
        TurnOff (getcoords (inst.Substring(8)))
    else
        Toggle (getcoords (inst.Substring(6)))

let turnOnBits (display : BitArray array) coords =
    let x1,y1,x2,y2 = coords
    let orWith = new BitArray([| for n in 0..999 -> n >= x1 && n <= x2 |])

    let rec doRow y =
        //printfn "row %d" y
        display.[y].Or(orWith) |> ignore
        
        if y = y2 then () else doRow (y + 1)

    doRow y1

let turnOffBits (display : BitArray array) coords =
    let x1,y1,x2,y2 = coords
    let andWith = new BitArray([| for n in 0..999 -> n < x1 || n > x2 |])

    let rec doRow y =
       // printfn "row %d" y
        display.[y].And(andWith) |> ignore
        
        if y = y2 then () else doRow (y + 1)

    doRow y1
        
let toggleBits (display : BitArray array) coords =
    let x1,y1,x2,y2 = coords
    let xorWith = new BitArray([| for n in 0..999 -> n >= x1 && n <= x2 |])

    let rec doRow y =
        //printfn "row %d" y
        display.[y].Xor(xorWith) |> ignore
        
        if y = y2 then () else doRow (y + 1)

    doRow y1

let countUp (display : BitArray array) =
    let rec count row col total =
        match (row, col) with
        | 1000, 0   -> total
        | r, 1000   -> count (r + 1) 0 total
        | r, c      -> 
            if display.[r].[c] 
            then
                count r (c + 1) (total + 1)
            else 
                count r (c + 1) total
    count 0 0 0

let letTheShowBegin instructions =
    let display = [| for n in 0..999 -> new BitArray(1000) |]
    let turnOn = turnOnBits display
    let turnOff = turnOffBits display
    let toggle = toggleBits display

    let rec doNext instruction =
        match instruction with
        | [] -> 
            printfn "It's all over"
            display
        | h :: t ->
            match h with
            | TurnOn cs     -> turnOn cs                                              
            | TurnOff cs    -> turnOff cs
            | Toggle cs     -> toggle cs 

            doNext t

    doNext instructions

let readFileIntoList () =
    seq {
        use sr = new StreamReader(@"C:\Users\Kevin-Roche\Documents\GitHub\AdventOfCode2015\Day 6 input.txt")
        while not sr.EndOfStream do
            let dimStr = sr.ReadLine()
            yield dimStr
        }
    |> Seq.toList


