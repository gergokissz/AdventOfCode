using System.Text.RegularExpressions;

const string input = "Input.txt";

string Part1()
{
    var stacks = Initialize();

    foreach ((var count, var from, var to) in ReadSteps())
    {
        for (int i = 0; i < count; i++)
        {
            stacks[to].Push(stacks[from].Pop());
        }
    }

    return GetTop(stacks);
}

string Part2()
{
    var stacks = Initialize();

    foreach ((var count, var from, var to) in ReadSteps())
    {
        var temp = new Stack<char>();

        for (var i = 0; i < count; i++)
        {
            temp.Push(stacks[from].Pop());
        }

        while (temp.Count > 0)
        {
            stacks[to].Push(temp.Pop());
        }
    }

    return GetTop(stacks);
}

Stack<char>[] Initialize()
{
    var lines = new List<string>();

    foreach (var line in File.ReadLines(input))
    {
        if (!line.StartsWith("["))
        {
            break;
        }

        lines.Add(line);
    }

    var ret = new Stack<char>[lines[0].Length / 4 + 1];

    for (var i = lines.Count - 1; i >= 0; i--)
    {
        for (var j = 0; j < ret.Length; j++)
        {
            ret[j] ??= new Stack<char>();

            if (lines[i][4 * j + 1] != ' ')
            {
                ret[j].Push(lines[i][4 * j +1]);
            }
        }
    }

    return ret;
}

IEnumerable<(int from, int to, int count)> ReadSteps()
{
    var regex = new Regex(@"^move\s(?<count>\d+)\sfrom\s(?<from>\d+)\sto\s(?<to>\d+)$", RegexOptions.Compiled);

    foreach (var line in File.ReadLines(input).Where(x => x.StartsWith("move")))
    {
        var match = regex.Match(line);
        var count = int.Parse(match.Groups["count"].Value);
        var from = int.Parse(match.Groups["from"].Value) - 1;
        var to = int.Parse(match.Groups["to"].Value) - 1;
        yield return (count, from, to);
    }
}

string GetTop(Stack<char>[] stacks) => stacks.Aggregate(string.Empty, (ret, stack) => ret + stack.Peek());

Console.WriteLine(Part1()); // QNHWJVJZW
Console.WriteLine(Part2()); // BPCZJLFJW