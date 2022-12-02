const string input = "Input.txt";

int Part1()
{
    var total = 0;

    foreach (var line in File.ReadLines(input))
    {
        total += line switch
        {
            "A X" => 3 + 1,
            "B X" => 0 + 1,
            "C X" => 6 + 1,
            "A Y" => 6 + 2,
            "B Y" => 3 + 2,
            "C Y" => 0 + 2,
            "A Z" => 0 + 3,
            "B Z" => 6 + 3,
            "C Z" => 3 + 3,
            _ => throw new NotImplementedException()
        };
    }

    return total;
}

int Part2()
{
    var total = 0;

    foreach (var line in File.ReadLines(input))
    {
        total += line switch
        {
            "A X" => 0 + 3,
            "B X" => 0 + 1,
            "C X" => 0 + 2,
            "A Y" => 3 + 1,
            "B Y" => 3 + 2,
            "C Y" => 3 + 3,
            "A Z" => 6 + 2,
            "B Z" => 6 + 3,
            "C Z" => 6 + 1,
            _ => throw new NotImplementedException()
        };
    }

    return total;
}

Console.WriteLine(Part1());
Console.WriteLine(Part2());
