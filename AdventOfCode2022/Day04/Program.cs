const string input = "Input.txt";

int Part1() => 
    File.ReadAllLines(input)
        .Sum(x =>
            Range.ParseTwo(x.AsSpan()) switch
            {
                (Range, Range) r when r.Item1.Contains(r.Item2) => 1,
                (Range, Range) r when r.Item2.Contains(r.Item1) => 1,
                _ => 0,
            }
        );

int Part2() =>
    File.ReadAllLines(input)
        .Sum(x =>
            Range.ParseTwo(x.AsSpan()) switch
            {
                (Range, Range) r when r.Item1.Overlaps(r.Item2) => 1,
                _ => 0,
            }
        );

Console.WriteLine(Part1()); // 513
Console.WriteLine(Part2()); // 878

record Range(int Start, int End)
{
    public static Range Parse(ReadOnlySpan<char> text)
    {
        var index = text.IndexOf('-');
        return new Range(int.Parse(text[..index]), int.Parse(text[(index + 1)..text.Length]));
    }

    public static (Range, Range) ParseTwo(ReadOnlySpan<char> text)
    {
        var index = text.IndexOf(',');
        return (Range.Parse(text[..index]), Range.Parse(text[(index + 1)..text.Length]));
    }

    public bool Contains(int number) => Start <= number && End >= number;

    public bool Contains(Range range) => Contains(range.Start) && Contains(range.End);

    public bool Overlaps(Range range) => Contains(range.Start) || Contains(range.End) || range.Contains(Start) || range.Contains(End);
}