open System
open System.IO
open System.Collections.Generic

module Seq =

    let foldi folder state source  =
       source 
       |> Seq.fold (fun (prev, i) current -> 
            (folder i prev current, i + 1)) (state, 0)
       |> fst

module Day20 = 

    type Point = { I: int; J: int }

    type Image = 
        { Pixels: Dictionary<Point, char>; 
          DefaultPixel: char; 
          IMin: int; 
          IMax: int; 
          JMin: int; 
          JMax: int }

    let createImage() = 
        { Pixels = new Dictionary<Point, char>();
          DefaultPixel = '0';
          IMin = Int32.MaxValue;
          IMax = Int32.MinValue;
          JMin = Int32.MaxValue;
          JMax = Int32.MinValue }

    let getPixel (i: int) (j: int) (image: Image) =
        let key = { I = i; J = j }
        if image.Pixels.ContainsKey(key) then
            image.Pixels[key]
        else
            image.DefaultPixel

    let setPixel (i: int) (j: int) (value: char) (image: Image) =
        image.Pixels[{ I = i; J = j }] <- value
        { Pixels = image.Pixels;
          DefaultPixel = image.DefaultPixel;
          IMin = min i image.IMin;
          IMax = max i image.IMax;
          JMin = min j image.JMin;
          JMax = max j image.JMax; }

    let readInutFromFile path = 

        let lines = File.ReadAllLines(path)

        let parseChar (c: char) =
            match c with
            | '.' -> '0'
            | '#' -> '1'
            | _ -> failwith $"Invalid character: '{c}'"

        let parseAlgorithm (line: string) =
            line.ToCharArray() 
            |> Seq.map (fun c -> c |> parseChar)
            |> Seq.toList

        let parseImage (lines: seq<string>) =
            lines
            |> Seq.foldi (fun i imageI line -> 
                line.ToCharArray() 
                |> Seq.foldi (fun j imageJ c -> 
                    imageJ |> setPixel i j (c |> parseChar)) imageI) (createImage())

        let algorithm = lines |> Seq.head |> parseAlgorithm
        let image = lines |> Seq.skip 2 |> parseImage
        (algorithm, image)

    let processImage (algorithm: char list) (n: int) (image: Image) =

        let processPixel (i: int) (j: int) (algorithm: char list) (image: Image) =
            let bits = [|
                for ii in [-1 .. 1] do
                    for jj in [-1 .. 1] do
                        image |> getPixel (i + ii) (j + jj)
            |]
            algorithm[int (Convert.ToUInt16(new String(bits), 2))]

        let processImageOnce (image: Image) =
            let defaultPixel = image |> processPixel (image.IMax + 2) (image.JMax + 2) algorithm
            let ret = 
                [image.IMin - 1 .. image.IMax + 1]
                |> Seq.fold (fun imageI i ->
                    [image.JMin - 1 .. image.JMax + 1]
                    |> Seq.fold (fun imageJ j ->
                        imageJ |> setPixel i j (image |> processPixel i j algorithm)) imageI) (createImage())
            { ret with DefaultPixel = defaultPixel }

        [1 .. n] |> Seq.fold (fun image _ -> image |> processImageOnce) image

let (algorithm, image) = Day20.readInutFromFile "input.txt"

let result1 =
    image
    |> Day20.processImage algorithm 2
    |> fun x -> x.Pixels.Values
    |> Seq.where (fun c ->
        match c with 
        | '0' -> false
        | '1' -> true
        | _ -> failwith "Invalid pixel")
    |> Seq.length

printfn "%i" result1

let result2 =
    image
    |> Day20.processImage algorithm 50
    |> fun x -> x.Pixels.Values
    |> Seq.where (fun c ->
        match c with 
        | '0' -> false
        | '1' -> true
        | _ -> failwith "Invalid pixel")
    |> Seq.length

printfn "%i" result2
