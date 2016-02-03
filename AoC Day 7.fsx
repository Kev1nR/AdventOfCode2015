(*

--- Day 7: Some Assembly Required ---

This year, Santa brought little Bobby Tables a set of wires and bitwise logic gates! Unfortunately, little Bobby is a little under the recommended age range, 
and he needs help assembling the circuit.

Each wire has an identifier (some lowercase letters) and can carry a 16-bit signal (a number from 0 to 65535). A signal is provided to each wire by a gate, 
another wire, or some specific value. Each wire can only get a signal from one source, but can provide its signal to multiple destinations. A gate provides no signal until 
all of its inputs have a signal.

The included instructions booklet describes how to connect the parts together: x AND y -> z means to connect wires x and y to an AND gate, and then connect its output to wire z.

For example:
  - 123 -> x means that the signal 123 is provided to wire x.
  - x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
  - p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q.
  - NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f.

Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for some reason, you'd like to emulate the circuit instead, almost all programming languages 
(for example, C, JavaScript, or Python) provide operators for these gates.

For example, here is a simple circuit:
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i


After it is run, these are the signals on the wires:
d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456


In little Bobby's kit's instructions booklet (provided as your puzzle input), what signal is ultimately provided to wire a?

To begin, get your puzzle input.


Answer:

*)
open System

type Signal = uint16 option
let inline signal n = Some <| uint16 n

type binaryFunc = Signal -> Signal -> Signal
type unaryFunc = Signal -> Signal

type Gate = 
    | AND of string * string * string * binaryFunc
    | OR of string * string * string * binaryFunc
    | NOT of string * string * unaryFunc
    | NOP of string * string * unaryFunc

let andFn : binaryFunc = fun l r ->
    match l, r with
    | Some l', Some r' -> signal (l' &&& r')
    | _ -> None

let orFn : binaryFunc = fun l r ->
    match l, r with
    | Some l', Some r' -> signal (l' ||| r')
    | _ -> None

let notFn : unaryFunc = fun i ->
    match i with
    | Some i' -> signal (~~~ i')
    | _ -> None

let nopFn : unaryFunc = id
        
type Circuit =
    | Empty
    | Circuit of Gate * Circuit * Gate
    with connect circuit gate =
        


// Test data
let gates = [
    "f AND e -> g";
    "d OR b -> f";
    "NOT b -> e";
    "g OR e -> h";
    "a AND b -> c";
    "NOT c -> d";
    "h -> i"]

let gates' = [
    AND ("f", "e", "g", andFn);
    OR ("d", "b", "f", orFn);
    NOT ("b", "e", notFn);
    OR ("g", "e", "h", orFn);
    AND ("a", "b", "c", andFn);
    NOT ("c", "d", notFn);
    NOP ("h", "i", nopFn)]

let testCircuit = Circuit (gates'.[0], Empty, gates'.[6])    
let testCircuit2 = Circuit (gates'.[1], testCircuit, gates'.[5])    
