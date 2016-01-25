(*
--- Day 5: Doesn't He Have Intern-Elves For This? ---
--- Part 1 ---
Santa needs help figuring out which strings in his text file are naughty or nice.

A nice string is one with all of the following properties:
  - It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
  - It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
  - It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

For example:
  - ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
  - aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
  - jchzalrnumimnmhp is naughty because it has no double letter.
  - haegwjzuvuyypxyu is naughty because it contains the string xy.
  - dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?

To begin, get your puzzle input.


Answer:   255

--- Part Two ---

Realizing the error of his ways, Santa has switched to a better model of determining whether a string is naughty or nice. None of the old rules apply, as they are all clearly ridiculous.

Now, a nice string is one with all of the following properties:
  - It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
  - It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.

For example:
  - qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz).
  - xxyxx is nice because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap.
  - uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them.
  - ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.

How many strings are nice under these new rules?


*)
open System.IO

let incIfVowel = function
    | 'a' | 'e' | 'i' | 'o' | 'u' -> 1
    | _ -> 0

let ``3 or more vowels`` =
    let vowelCount = ref 0
    fun c -> 
        if c = (char 0) then vowelCount := 0
             
        if !vowelCount < 3
        then
            vowelCount := !vowelCount + (incIfVowel c)
            !vowelCount >= 3
        else
            !vowelCount >= 3

let ``contains a naughty group`` = 
    let prevChar = ref (char 0)
    let groupFound = ref false
    
    fun c -> 
        if c = (char 0) then prevChar := (char 0); groupFound := false

        if not !groupFound
        then
            groupFound := 
                (!prevChar = 'a' && c = 'b')
                ||
                (!prevChar = 'c' && c = 'd')
                ||
                (!prevChar = 'p' && c = 'q')
                ||
                (!prevChar = 'x' && c = 'y')
            prevChar := c
            !groupFound
        else
            !groupFound

let ``doesn't contain a naughty group`` = fun c -> not (``contains a naughty group`` c)

let ``contains double letter`` = 
    let prevChar = ref (char 0)
    let doubleLetterFound = ref false

    fun c -> 
        if c = (char 0) then (prevChar := (char 0);  doubleLetterFound := false)

        if not !doubleLetterFound
        then
            doubleLetterFound := !prevChar = c && c <> (char 0)
            prevChar := c
            !doubleLetterFound
        else
            !doubleLetterFound

let ``contains pair of double letters`` =
    let stringPos = ref 0
    let cache = ref (new System.Collections.Generic.Dictionary<char,int>())
     
    let prevChar = ref (char 0)
    let doubleLetterFound = ref false

    fun c -> 
        if c = (char 0) then (prevChar := (char 0);  doubleLetterFound := false)

        if not !doubleLetterFound
        then
            doubleLetterFound := !prevChar = c && c <> (char 0)
            prevChar := c
            !doubleLetterFound
        else
            !doubleLetterFound

let (|Nice|_|) criteria (testStr : string) =
    let charList = testStr.ToCharArray() |> Array.toList
    let rec isNice state chars =
        match chars with
        | [] -> 
            if (state |> List.forall (fun s -> s))
            then
                Some testStr
            else 
                None
        | h :: t ->
            isNice (criteria |> List.map (fun f -> f h)) t
    
    isNice [] charList
    
let howManyNiceStrings (criteria : (char -> bool) list) =
    seq {
        use sr = new StreamReader(@"C:\Users\Kevin-Roche\Documents\Visual Studio 2013\Projects\AdventOfCode2015\AoC Day5 Naughy and Nice strings.txt")
        while not sr.EndOfStream do
            let dimStr = sr.ReadLine()
            criteria |> List.iter (fun f -> f (char 0) |> ignore)
            yield 
                match dimStr with
                | Nice criteria _ -> 1
                | _ -> 0}
    |> Seq.fold (fun tot nice -> tot + nice) 0